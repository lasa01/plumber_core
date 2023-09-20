use plumber_asset_core::{Asset, AssetConfig, Context, Handler, NoError};
use plumber_vmf::entities::TypedEntity;

#[derive(Debug, Clone, Copy)]
pub struct OtherEntityConfig;

impl<H: Handler<Asset<Self>>> AssetConfig<H> for OtherEntityConfig {
    type Input<'a> = TypedEntity<'a>;
    type Output<'a> = TypedEntity<'a>;
    type Error<'a> = NoError;

    fn process<'a>(
        self,
        input: Self::Input<'a>,
        _context: &mut Context<H>,
    ) -> Result<Self::Output<'a>, Self::Error<'a>> {
        Ok(input)
    }
}
