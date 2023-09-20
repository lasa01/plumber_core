use std::{
    any::{Any, TypeId},
    collections::{BTreeMap, HashMap},
    fmt::{self, Debug, Display, Formatter},
    hash::Hash,
    marker::PhantomData,
    sync::{
        atomic::{AtomicUsize, Ordering},
        RwLock, RwLockReadGuard, RwLockWriteGuard,
    },
};

use crossbeam_utils::sync::WaitGroup;
use plumber_fs::OpenFileSystem;
use rayon::{
    iter::{IntoParallelIterator, ParallelIterator},
    Yield,
};
use tracing::{debug, debug_span};

type CachedResult<C, H> =
    Result<<C as CachedAssetConfig<H>>::CachedOutput, <C as CachedAssetConfig<H>>::Error>;

type CacheMap<C, H> = HashMap<<C as CachedAssetConfig<H>>::Id, CacheState<CachedResult<C, H>>>;

enum CacheState<C> {
    Waiting,
    Processing(WaitGroup),
    Cached(C),
}

#[derive(Default)]
struct CacheManager {
    caches: BTreeMap<TypeId, Box<dyn Any + Send + Sync>>,
}

impl CacheManager {
    fn get_cache<C: CachedAssetConfig<H>, H: Handler<Cached<C>>>(&self) -> Option<&CacheMap<C, H>> {
        let type_id = TypeId::of::<C>();

        let cache = self.caches.get(&type_id)?;

        Some(
            cache
                .downcast_ref()
                .expect("type must be correct here, map is keyed by type id"),
        )
    }

    fn get_cache_state<C: CachedAssetConfig<H>, H: Handler<Cached<C>>>(
        &self,
        id: &C::Id,
    ) -> &CacheState<CachedResult<C, H>> {
        self.get_cache::<C, H>()
            .expect("asset cache should exist at this point")
            .get(id)
            .expect("asset cache entry should exist at this point")
    }

    fn try_get_cache_state<C: CachedAssetConfig<H>, H: Handler<Cached<C>>>(
        &self,
        id: &C::Id,
    ) -> Option<&CacheState<CachedResult<C, H>>> {
        self.get_cache::<C, H>()?.get(id)
    }

    fn get_cache_mut<C: CachedAssetConfig<H>, H: Handler<Cached<C>>>(
        &mut self,
    ) -> &mut CacheMap<C, H> {
        let type_id = TypeId::of::<C>();

        let cache = self
            .caches
            .entry(type_id)
            .or_insert_with(|| Box::<CacheMap<C, H>>::default());

        cache
            .downcast_mut()
            .expect("type must be correct here, map is keyed by type id")
    }
}

struct SharedExecutor {
    asset_cache_manager: RwLock<CacheManager>,
    tp: rayon::ThreadPool,
    fs: OpenFileSystem,
    imported_assets: AtomicUsize,
    in_progress_assets: AtomicUsize,
}

impl SharedExecutor {
    fn new(tp: rayon::ThreadPool, fs: OpenFileSystem) -> Self {
        Self {
            asset_cache_manager: RwLock::default(),
            tp,
            fs,
            imported_assets: AtomicUsize::new(0),
            in_progress_assets: AtomicUsize::new(0),
        }
    }
}

pub struct Executor<H> {
    handler: H,
    shared: SharedExecutor,
}

impl<H: Clone> Executor<H> {
    pub fn new(handler: H, fs: OpenFileSystem) -> Self {
        let tp = rayon::ThreadPoolBuilder::new()
            .thread_name(|index| format!("asset processor {}", index))
            .build()
            .unwrap();

        Self {
            handler,
            shared: SharedExecutor::new(tp, fs),
        }
    }

    pub fn new_with_threads(handler: H, fs: OpenFileSystem, threads: usize) -> Self {
        let tp = rayon::ThreadPoolBuilder::new()
            .thread_name(|index| format!("asset processor {}", index))
            .num_threads(threads)
            .build()
            .unwrap();

        Self {
            handler,
            shared: SharedExecutor::new(tp, fs),
        }
    }

    pub fn fs(&self) -> &OpenFileSystem {
        &self.shared.fs
    }

    pub fn process<A>(
        self,
        config: impl IntoProcessable<A, H>,
        input: A::Input<'_>,
        f: impl FnOnce(),
    ) where
        A: Processable<H>,
        H: Handler<A>,
    {
        let Executor {
            handler,
            ref shared,
        } = self;

        shared.tp.in_place_scope(|scope| {
            scope.spawn(move |scope| {
                let mut context = Context::new(shared, scope, handler);

                context.process(config, input)
            });

            f();
        });
    }

    pub fn process_each<'b, A>(
        self,
        config: impl IntoProcessable<A, H>,
        inputs: impl IntoParallelIterator<Item = A::Input<'b>> + Send,
        f: impl FnOnce(),
    ) where
        A: Processable<H>,
        H: Handler<A>,
    {
        let Executor {
            handler,
            ref shared,
        } = self;

        shared.tp.in_place_scope(|scope| {
            scope.spawn(move |scope| {
                let context = Context::new(shared, scope, handler);

                context.process_each(config, inputs)
            });

            f();
        });
    }

    pub fn depend_on<C: CachedAssetConfig<H>>(
        self,
        config: C,
        input: C::Input<'_>,
        f: impl FnOnce(),
    ) -> CachedResult<C, H>
    where
        H: Handler<Cached<C>>,
    {
        let Executor {
            handler,
            ref shared,
        } = self;

        let mut result = None;
        let result_ref = &mut result;

        shared.tp.in_place_scope(|scope| {
            scope.spawn(move |scope| {
                let mut context = Context::new(shared, scope, handler);

                let result = context.depend_on(config, input);
                *result_ref = Some(result);
            });

            f();
        });

        result.expect("result should have been assigned")
    }

    pub fn in_progress_assets(&self) -> usize {
        self.shared.in_progress_assets.load(Ordering::Relaxed)
    }

    pub fn imported_assets(&self) -> usize {
        self.shared.imported_assets.load(Ordering::Relaxed)
    }
}

#[derive(Clone)]
pub struct Context<'scope, 'scope_ref, H>
where
    'scope: 'scope_ref,
{
    shared: &'scope SharedExecutor,
    scope: &'scope_ref rayon::Scope<'scope>,
    handler: H,
    current_cached: bool,
}

impl<'scope, 'scope_ref, H> Context<'scope, 'scope_ref, H> {
    fn new(
        shared: &'scope SharedExecutor,
        scope: &'scope_ref rayon::Scope<'scope>,
        handler: H,
    ) -> Self {
        Self {
            shared,
            scope,
            handler,
            current_cached: false,
        }
    }

    pub fn fs(&self) -> &'scope OpenFileSystem {
        &self.shared.fs
    }

    /// Process an asset.
    ///
    /// If the asset is cached and already processed, do nothing.
    /// If the asset is cached and currently processed by another thread, wait for it to finish.
    pub fn process<C>(&mut self, config: impl IntoProcessable<C, H>, input: C::Input<'_>)
    where
        C: Processable<H>,
        H: Handler<C>,
    {
        let asset = config.into_processable();
        asset.process(self, input);
    }

    /// Process multiple assets, potentially in parallel.
    ///
    /// If an asset is cached and already processed, do nothing.
    /// If an asset is cached and currently processed by another thread, wait for it to finish.
    pub fn process_each<'b, C>(
        &self,
        config: impl IntoProcessable<C, H>,
        inputs: impl IntoParallelIterator<Item = C::Input<'b>>,
    ) where
        C: Processable<H>,
        H: Handler<C>,
    {
        let asset = config.into_processable();
        let inputs = inputs.into_par_iter();

        inputs.for_each_with(self.clone(), |context, input| asset.process(context, input));
    }

    fn process_internal<C>(&mut self, config: &C, input: C::Input<'_>)
    where
        C: AssetConfig<H>,
        H: Handler<Asset<C>>,
    {
        let _span = debug_span!("process", ?config).entered();

        self.shared
            .in_progress_assets
            .fetch_add(1, Ordering::Relaxed);

        let current_cached = self.current_cached;
        self.current_cached = false;

        let result = config.process(input, self);
        self.handler.handle(result);

        self.current_cached = current_cached;

        self.shared
            .in_progress_assets
            .fetch_sub(1, Ordering::Relaxed);
        self.shared.imported_assets.fetch_add(1, Ordering::Relaxed);
    }

    fn process_cached_internal<C>(&mut self, config: &C, input: C::Input<'_>)
    where
        C: CachedAssetConfig<H>,
        H: Handler<Cached<C>>,
    {
        let id = config.cache_id(&input);

        let mut cache_manager_guard = self.write_cache_manager();
        let cache = cache_manager_guard.get_cache_mut::<C, H>();

        // If the asset is already imported / being currently imported, don't import it again
        if let Some(CacheState::Processing(..) | CacheState::Cached(..)) = cache.get(&id) {
            return;
        }

        let _span = debug_span!("process", ?config, %id).entered();

        self.shared
            .in_progress_assets
            .fetch_add(1, Ordering::Relaxed);

        cache.insert(id.clone(), CacheState::Processing(WaitGroup::new()));

        drop(cache_manager_guard);

        let current_cached = self.current_cached;
        self.current_cached = true;

        let (output, cached) = match config.process(input, self) {
            Ok((output, cached)) => (Ok(output), Ok(cached)),
            Err(err) => (Err(err.clone()), Err(err)),
        };

        self.handler.handle(output);

        self.current_cached = current_cached;

        let mut cache_manager_guard = self.write_cache_manager();
        let cache = cache_manager_guard.get_cache_mut::<C, H>();

        let state = cache
            .get_mut(&id)
            .expect("asset cache entry should exist at this point");

        // This also drops the previous WaitGroup, signaling potential waiting threads
        *state = CacheState::Cached(cached);

        drop(cache_manager_guard);

        self.shared
            .in_progress_assets
            .fetch_sub(1, Ordering::Relaxed);
        self.shared.imported_assets.fetch_add(1, Ordering::Relaxed);
    }

    fn wait_for_asset<C>(&self, config: &C, id: &C::Id) -> CachedResult<C, H>
    where
        C: CachedAssetConfig<H>,
        H: Handler<Cached<C>>,
    {
        let _span = debug_span!("wait", ?config, %id).entered();

        // If the currently imported asset is cached, cannot execute other work while waiting,
        // since the other work might depend on the currently importing asset, leading to a deadlock
        if self.current_cached {
            return self.block_for_asset(config, id);
        }

        // Asset is being imported by another thread, see if there is other work to do in the meanwhile
        loop {
            if let Yield::Idle = self.yield_now() {
                // No work, just block to wait for the import to finish
                return self.block_for_asset(config, id);
            } else {
                // Work was executed, check if the import finished in the meanwhile
                let cache_manager_guard = self.read_cache_manager();
                let cache_state = cache_manager_guard.get_cache_state::<C, H>(id);

                if let CacheState::Cached(c) = cache_state {
                    return c.clone();
                };

                drop(cache_manager_guard);
            }
        }
    }

    fn block_for_asset<C>(&self, config: &C, id: &C::Id) -> CachedResult<C, H>
    where
        C: CachedAssetConfig<H>,
        H: Handler<Cached<C>>,
    {
        let cache_manager_guard = self.read_cache_manager();
        let cache_state = cache_manager_guard.get_cache_state::<C, H>(id);

        match cache_state {
            CacheState::Waiting => panic!("invalid cache state for blocking"),
            CacheState::Processing(wg) => {
                let wg = wg.clone();

                drop(cache_manager_guard);

                let span = debug_span!("block", ?config, %id).entered();
                wg.wait();
                span.exit();

                let cache_manager_guard = self.read_cache_manager();
                let cache_state = cache_manager_guard.get_cache_state::<C, H>(id);

                let CacheState::Cached(cached) = cache_state else {
                    panic!("Asset was not cached after waiting for it");
                };

                cached.clone()
            }
            CacheState::Cached(c) => c.clone(),
        }
    }

    /// Queue a task to process an asset, without immediately waiting for it to finish.
    ///
    /// If the asset is cached and already processed, do nothing.
    /// If the asset is cached and currently processed by another thread, wait for it to finish.
    pub fn queue<C>(&self, config: impl IntoProcessable<C, H>, input: C::Input<'scope>)
    where
        C: Processable<H> + 'scope,
        H: Handler<C> + 'scope,
    {
        let asset = config.into_processable();

        asset.queue(self, input);
    }

    /// Queue a task to process multiple assets, without immediately waiting for it to finish.
    pub fn queue_each<C, I>(&self, config: C, inputs: I)
    where
        C: AssetConfig<H> + 'scope,
        H: Handler<Asset<C>> + 'scope,
        I: IntoParallelIterator<Item = C::Input<'scope>>,
        I::Iter: 'scope,
    {
        let inputs = inputs.into_par_iter();

        let shared = self.shared;
        let handler = self.handler.clone();

        self.scope.spawn(move |scope| {
            let context = Context::new(shared, scope, handler);

            inputs.for_each_with(context, |context, input| {
                context.process_internal(&config, input)
            });
        });
    }

    fn queue_internal<C>(&self, config: C, input: C::Input<'scope>)
    where
        C: AssetConfig<H>,
        H: Handler<Asset<C>> + 'scope,
        C: 'scope,
    {
        let shared = self.shared;
        let handler = self.handler.clone();

        self.scope.spawn(move |scope| {
            let mut context = Context::new(shared, scope, handler);

            context.process_internal(&config, input);
        });
    }

    fn queue_cached_internal<C>(&self, config: C, input: C::Input<'scope>)
    where
        C: CachedAssetConfig<H>,
        H: Handler<Cached<C>> + 'scope,
    {
        let id = config.cache_id(&input);

        let mut cache_manager_guard = self.write_cache_manager();
        let cache = cache_manager_guard.get_cache_mut::<C, H>();

        // If the asset is already imported / being currently imported / already queued, don't queue it again
        if cache.contains_key(&id) {
            return;
        }

        cache.insert(id, CacheState::Waiting);

        drop(cache_manager_guard);

        let shared = self.shared;
        let handler = self.handler.clone();

        self.scope.spawn(move |scope| {
            let mut context = Context::new(shared, scope, handler);

            context.process_cached_internal(&config, input);
        });
    }

    /// Process a cached asset and return the cached output of the asset.
    ///
    /// If the asset is already processed, just return the cached output.
    /// If the asset is cached and currently processed by another thread, wait for it to finish.
    pub fn depend_on<C>(&mut self, config: C, input: C::Input<'_>) -> CachedResult<C, H>
    where
        C: CachedAssetConfig<H>,
        H: Handler<Cached<C>>,
    {
        let cache_manager_guard = self.read_cache_manager();

        let id = config.cache_id(&input);

        if let Some(cache_state @ (CacheState::Processing(..) | CacheState::Cached(..))) =
            cache_manager_guard.try_get_cache_state::<C, H>(&id)
        {
            // The asset is already imported / being currently imported
            if let CacheState::Cached(c) = cache_state {
                return c.clone();
            };

            drop(cache_manager_guard);
            self.wait_for_asset::<C>(&config, &id)
        } else {
            // Otherwise, import the asset.
            drop(cache_manager_guard);

            let mut cache_manager_guard = self.write_cache_manager();
            let cache = cache_manager_guard.get_cache_mut::<C, H>();

            // If the asset is already imported / being currently imported,
            // someone got write access before we did and started importing the asset,
            // so we need to wait for them to finish
            if let Some(CacheState::Processing(..) | CacheState::Cached(..)) = cache.get(&id) {
                drop(cache_manager_guard);
                return self.wait_for_asset::<C>(&config, &id);
            }

            let _span = debug_span!("process", ?config, %id).entered();

            self.shared
                .in_progress_assets
                .fetch_add(1, Ordering::Relaxed);

            cache.insert(id.clone(), CacheState::Processing(WaitGroup::new()));

            drop(cache_manager_guard);

            let current_cached = self.current_cached;
            self.current_cached = true;

            let (output, cached) = match config.process(input, self) {
                Ok((output, cached)) => (Ok(output), Ok(cached)),
                Err(err) => (Err(err.clone()), Err(err)),
            };

            self.current_cached = current_cached;

            self.handler.handle(output);

            let mut cache_manager_guard = self.write_cache_manager();
            let cache = cache_manager_guard.get_cache_mut::<C, H>();

            let state = cache
                .get_mut(&id)
                .expect("asset cache entry should exist at this point");

            // This also drops the previous WaitGroup, signaling potential waiting threads
            *state = CacheState::Cached(cached.clone());

            drop(cache_manager_guard);

            self.shared
                .in_progress_assets
                .fetch_sub(1, Ordering::Relaxed);
            self.shared.imported_assets.fetch_add(1, Ordering::Relaxed);

            cached
        }
    }

    /// Spawn a task that executes a closure without waiting for it to finish.
    pub fn spawn(&self, f: impl FnOnce(Context<'scope, '_, H>) + Send + 'scope)
    where
        H: Clone + Send + 'scope,
    {
        let shared = self.shared;
        let handler = self.handler.clone();

        self.scope.spawn(move |scope| {
            let context = Context::new(shared, scope, handler);

            f(context);
        });
    }

    /// Create a scope to allow borrowing local data for spawns or queues.
    /// The scope will wait until all the spawned tasks complete.
    pub fn scope<'new_scope>(&self, f: impl FnOnce(Context<'new_scope, '_, H>))
    where
        H: Clone,
        'scope: 'new_scope,
    {
        let shared: &SharedExecutor = self.shared;
        let handler = self.handler.clone();

        rayon::in_place_scope(|scope| {
            let context = Context::new(shared, scope, handler);

            f(context);
        });
    }

    fn read_cache_manager(&self) -> RwLockReadGuard<CacheManager> {
        let _span = debug_span!("read_cache").entered();

        self.shared
            .asset_cache_manager
            .read()
            .expect("rwlock shouldn't be poisoned")
    }

    fn write_cache_manager(&self) -> RwLockWriteGuard<CacheManager> {
        let _span = debug_span!("write_cache").entered();

        self.shared
            .asset_cache_manager
            .write()
            .expect("rwlock shouldn't be poisoned")
    }

    fn yield_now(&self) -> Yield {
        self.shared
            .tp
            .yield_now()
            .expect("this method shouldn't be called outside the importer thread pool")
    }
}

pub trait AssetConfig<H>: Debug + Copy + Sized + Send + Sync
where
    H: Handler<Asset<Self>>,
{
    type Input<'a>: Send;
    type Output<'a>;
    type Error<'a>;

    fn process<'a>(
        self,
        input: Self::Input<'a>,
        context: &mut Context<H>,
    ) -> Result<Self::Output<'a>, Self::Error<'a>>;
}

// Blanket impl Handler of assets with no output and no error for all types,
// so that blank handlers don't need to be implemented to import assets with no output
impl<H, A> Handler<Asset<A>> for H
where
    H: Sized + Send + Clone,
    A: for<'a> AssetConfig<H, Output<'a> = (), Error<'a> = NoError>,
{
    fn handle(&self, _output: Result<(), NoError>) {}
}

/// Can be used to pass assets through to handlers if no processing needs to be done on them here.
pub struct Passtrough<T>(PhantomData<fn(T)>);

impl<T> Passtrough<T> {
    pub fn new() -> Self {
        Self(PhantomData)
    }
}

impl<T> Default for Passtrough<T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T> Debug for Passtrough<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str("Passtrough")
    }
}

impl<T> Clone for Passtrough<T> {
    fn clone(&self) -> Self {
        Self(PhantomData)
    }
}

impl<T> Copy for Passtrough<T> {}

impl<H, T> AssetConfig<H> for Passtrough<T>
where
    T: Send,
    H: Handler<Asset<Passtrough<T>>>,
{
    type Input<'a> = T;
    type Output<'a> = T;
    type Error<'a> = NoError;

    fn process<'a>(
        self,
        input: Self::Input<'a>,
        _context: &mut Context<H>,
    ) -> Result<Self::Output<'a>, Self::Error<'a>> {
        Ok(input)
    }
}

pub trait CachedAssetConfig<H>: Debug + Copy + Sized + Send + Sync + 'static
where
    H: Handler<Cached<Self>>,
{
    type Input<'a>: Send;
    type Id: Eq + Hash + Clone + Display + Send + Sync + 'static;
    type Output<'a>;
    type CachedOutput: Clone + Send + Sync + 'static;
    type Error: Clone + Send + Sync + 'static;

    fn cache_id(self, input: &Self::Input<'_>) -> Self::Id;

    fn process<'a>(
        self,
        input: Self::Input<'a>,
        context: &mut Context<H>,
    ) -> Result<(Self::Output<'a>, Self::CachedOutput), Self::Error>;
}

#[derive(Clone, Copy)]
pub struct Asset<C>(pub C);

#[derive(Clone, Copy)]
pub struct Cached<C>(pub C);

mod internal {
    use super::*;

    pub trait Processable<H>: Copy + Sized + Send + Sync
    where
        H: Handler<Self>,
    {
        type Input<'a>: Send;
        type Output<'a>;

        fn process(self, context: &mut Context<H>, input: Self::Input<'_>);

        fn queue<'scope>(self, context: &Context<'scope, '_, H>, input: Self::Input<'scope>)
        where
            H: 'scope,
            Self: 'scope;
    }

    impl<A, H> Processable<H> for Asset<A>
    where
        A: AssetConfig<H>,
        H: Handler<Asset<A>>,
    {
        type Input<'a> = A::Input<'a>;
        type Output<'a> = Result<A::Output<'a>, A::Error<'a>>;

        fn process(self, context: &mut Context<H>, input: Self::Input<'_>) {
            context.process_internal(&self.0, input);
        }

        fn queue<'scope>(self, context: &Context<'scope, '_, H>, input: Self::Input<'scope>)
        where
            H: 'scope,
            Self: 'scope,
        {
            context.queue_internal(self.0, input);
        }
    }

    impl<A, H> Processable<H> for Cached<A>
    where
        A: CachedAssetConfig<H>,
        H: Handler<Cached<A>>,
    {
        type Input<'a> = A::Input<'a>;
        type Output<'a> = Result<A::Output<'a>, A::Error>;

        fn process(self, context: &mut Context<H>, input: Self::Input<'_>) {
            context.process_cached_internal(&self.0, input);
        }

        fn queue<'scope>(self, context: &Context<'scope, '_, H>, input: Self::Input<'scope>)
        where
            H: 'scope,
            Self: 'scope,
        {
            context.queue_cached_internal(self.0, input);
        }
    }
}

pub trait Processable<H>: internal::Processable<H>
where
    H: Handler<Self>,
{
}

impl<A, H> Processable<H> for Asset<A>
where
    A: AssetConfig<H>,
    H: Handler<Asset<A>>,
{
}

impl<A, H> Processable<H> for Cached<A>
where
    A: CachedAssetConfig<H>,
    H: Handler<Cached<A>>,
{
}

pub trait IntoProcessable<P, H>: Send
where
    P: Processable<H>,
    H: Handler<P>,
{
    fn into_processable(self) -> P;
}

impl<C, H> IntoProcessable<Asset<C>, H> for C
where
    C: AssetConfig<H>,
    H: Handler<Asset<C>>,
{
    fn into_processable(self) -> Asset<C> {
        Asset(self)
    }
}

impl<C, H> IntoProcessable<Cached<C>, H> for C
where
    C: CachedAssetConfig<H>,
    H: Handler<Cached<C>>,
{
    fn into_processable(self) -> Cached<C> {
        Cached(self)
    }
}

pub trait Handler<C>: Sized + Send + Clone
where
    C: internal::Processable<Self>,
{
    fn handle(&self, output: C::Output<'_>);
}

#[derive(Debug, Clone, Copy)]
pub enum NoError {}

#[derive(Debug, Clone, Copy)]
pub enum CurrentThread {
    OutsideThreadpool,
    Num(usize),
}

impl Display for CurrentThread {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            CurrentThread::OutsideThreadpool => f.write_str("-"),
            CurrentThread::Num(n) => Display::fmt(&n, f),
        }
    }
}

pub fn current_thread() -> CurrentThread {
    match rayon::current_thread_index() {
        None => CurrentThread::OutsideThreadpool,
        Some(n) => CurrentThread::Num(n),
    }
}
