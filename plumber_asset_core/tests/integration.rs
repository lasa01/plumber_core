use std::{
    collections::BTreeSet,
    sync::{
        mpsc::{channel, Receiver, Sender},
        Mutex,
    },
};

use plumber_asset_core::*;
use plumber_fs::FileSystem;

struct TestAsset {
    id: u32,
    output: u8,
}

#[derive(Debug, Clone, Copy)]
struct TestAssetDefinition;

impl<H: Handler<Cached<Self>>> CachedAssetConfig<H> for TestAssetDefinition {
    type Input<'a> = TestAsset;
    type Id = u32;
    type Output<'a> = i8;
    type CachedOutput = u8;
    type Error = NoError;

    fn process<'a>(
        self,
        input: TestAsset,
        _context: &mut Context<H>,
    ) -> Result<(Self::Output<'a>, u8), NoError> {
        Ok((input.output as i8, input.output))
    }

    fn cache_id(self, input: &TestAsset) -> Self::Id {
        input.id
    }
}

#[derive(Clone, Copy)]
struct TestAsset2 {
    id: u64,
    output: i16,
}

#[derive(Debug, Clone, Copy)]
struct TestAssetDefinition2;

impl<H> CachedAssetConfig<H> for TestAssetDefinition2
where
    H: Handler<Cached<Self>> + Handler<Cached<TestAssetDefinition>>,
{
    type Input<'a> = TestAsset2;
    type Id = u64;
    type Output<'a> = i32;
    type CachedOutput = i16;
    type Error = NoError;

    fn process<'a>(
        self,
        input: TestAsset2,
        context: &mut Context<H>,
    ) -> Result<(Self::Output<'a>, i16), NoError> {
        context
            .depend_on(TestAssetDefinition, TestAsset { id: 1, output: 2 })
            .unwrap();

        Ok((input.output as i32, input.output))
    }

    fn cache_id(self, input: &TestAsset2) -> Self::Id {
        input.id
    }
}

struct TestAsset3(i64);

#[derive(Debug, Clone, Copy)]
struct TestAssetDefinition3;

impl AssetConfig<TestHandler> for TestAssetDefinition3 {
    type Input<'a> = TestAsset3;
    type Output<'a> = i64;
    type Error<'a> = NoError;

    fn process<'a>(
        self,
        input: TestAsset3,
        _context: &mut Context<TestHandler>,
    ) -> Result<Self::Output<'a>, NoError> {
        Ok(input.0)
    }
}

#[derive(Debug, Clone, Copy)]
struct DataAssetDefinition<'a> {
    mutex: &'a Mutex<BTreeSet<i64>>,
}

impl<'a, H: Handler<Asset<Self>>> AssetConfig<H> for DataAssetDefinition<'a> {
    type Input<'b> = TestAsset3;
    type Output<'b> = i64;
    type Error<'b> = NoError;

    fn process<'b>(
        self,
        input: TestAsset3,
        _context: &mut Context<H>,
    ) -> Result<Self::Output<'b>, NoError> {
        let mut guard = self.mutex.lock().unwrap();

        guard.insert(input.0);

        Ok(input.0)
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug)]
enum AssetMessage {
    Asset(i8),
    Asset2(i32),
    Asset3(i64),
}

#[derive(Clone)]
struct TestHandler {
    tx: Sender<AssetMessage>,
}

impl Handler<Cached<TestAssetDefinition>> for TestHandler {
    fn handle(&self, output: Result<i8, NoError>) {
        self.tx.send(AssetMessage::Asset(output.unwrap())).unwrap();
    }
}

impl Handler<Cached<TestAssetDefinition2>> for TestHandler {
    fn handle(&self, output: Result<i32, NoError>) {
        self.tx.send(AssetMessage::Asset2(output.unwrap())).unwrap();
    }
}

impl Handler<Asset<TestAssetDefinition3>> for TestHandler {
    fn handle(&self, output: Result<i64, NoError>) {
        self.tx.send(AssetMessage::Asset3(output.unwrap())).unwrap();
    }
}

impl<'a> Handler<Asset<DataAssetDefinition<'a>>> for TestHandler {
    fn handle(&self, output: Result<i64, NoError>) {
        self.tx.send(AssetMessage::Asset3(output.unwrap())).unwrap();
    }
}

struct TestReceiver {
    rx: Option<Receiver<AssetMessage>>,
    buf: Vec<AssetMessage>,
}

impl TestReceiver {
    fn receive_block(&mut self) {
        for msg in self.rx.take().unwrap() {
            self.buf.push(msg);
        }
    }

    fn assert_received(&self, asset: AssetMessage) {
        assert!(
            self.buf.contains(&asset),
            "should contain: {:?}\ncontains: {:?}",
            asset,
            self.buf.as_slice()
        );
    }

    fn assert_received_n(&self, n: usize) {
        assert_eq!(self.buf.len(), n);
    }

    fn assert_received_order(&self, first: AssetMessage, second: AssetMessage) {
        let first_i = self.buf.iter().position(|m| m == &first).unwrap();
        let second_i = self.buf.iter().position(|m| m == &second).unwrap();

        assert!(first_i < second_i);
    }
}

fn setup() -> (Executor<TestHandler>, TestReceiver) {
    let (tx, rx) = channel();

    let handler = TestHandler { tx };

    let fs = FileSystem {
        name: String::new(),
        search_paths: Vec::new(),
    };
    let fs = fs.open().unwrap();

    let executor = Executor::new_with_threads(handler, fs, 2);

    let receiver = TestReceiver {
        rx: Some(rx),
        buf: Vec::new(),
    };

    (executor, receiver)
}

#[test]
fn simple_asset_import() {
    let (processor, mut receiver) = setup();

    processor.process(TestAssetDefinition3, TestAsset3(1), || {
        receiver.receive_block()
    });

    receiver.assert_received(AssetMessage::Asset3(1));
    receiver.assert_received_n(1);

    let (processor, mut receiver) = setup();

    processor.process_each(
        TestAssetDefinition3,
        [TestAsset3(1), TestAsset3(2), TestAsset3(3), TestAsset3(4)],
        || receiver.receive_block(),
    );

    receiver.assert_received_n(4);
}

#[test]
fn cached_asset_import() {
    let (executor, mut receiver) = setup();

    executor.process_each(
        TestAssetDefinition,
        [
            TestAsset { id: 1, output: 11 },
            TestAsset { id: 2, output: 22 },
            TestAsset { id: 1, output: 11 },
        ],
        || receiver.receive_block(),
    );

    receiver.assert_received(AssetMessage::Asset(11));
    receiver.assert_received(AssetMessage::Asset(22));
    receiver.assert_received_n(2);

    let (executor, mut receiver) = setup();

    executor.process_each(
        TestAssetDefinition2,
        [
            TestAsset2 { id: 1, output: 11 },
            TestAsset2 { id: 2, output: 22 },
            TestAsset2 { id: 1, output: 11 },
        ],
        || receiver.receive_block(),
    );

    receiver.assert_received(AssetMessage::Asset2(11));
    receiver.assert_received(AssetMessage::Asset2(22));
    receiver.assert_received(AssetMessage::Asset(2));
    receiver.assert_received_n(3);
    receiver.assert_received_order(AssetMessage::Asset(2), AssetMessage::Asset2(11));
    receiver.assert_received_order(AssetMessage::Asset(2), AssetMessage::Asset2(22));
}

#[test]
fn asset_definition_data() {
    let (executor, mut receiver) = setup();

    let mutex: Mutex<BTreeSet<i64>> = Mutex::default();

    executor.process_each(
        DataAssetDefinition { mutex: &mutex },
        [TestAsset3(1), TestAsset3(2), TestAsset3(3)],
        || receiver.receive_block(),
    );

    receiver.assert_received_n(3);

    let guard = mutex.lock().unwrap();

    assert!(guard.contains(&1));
    assert!(guard.contains(&2));
    assert!(guard.contains(&3));
}
