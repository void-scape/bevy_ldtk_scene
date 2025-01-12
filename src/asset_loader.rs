use crate::{
    extract::{entities::LdtkEntityInstance, tiles::TileSetInstance},
    world::{ExtractLdtkWorld, LdtkWorld},
};
use bevy::{
    asset::{io::Reader, AssetLoader, AsyncReadExt, LoadContext},
    prelude::*,
};
use ron::de::SpannedError;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum LdtkWorldAssetLoaderError {
    #[error("Could not read asset: {0}")]
    Io(#[from] std::io::Error),
    #[error("Could not parse asset: {0}")]
    Ron(#[from] SpannedError),
}

#[derive(Default)]
pub struct LdtkWorldAssetLoader;

impl AssetLoader for LdtkWorldAssetLoader {
    type Asset = LdtkWorld;
    type Settings = ();
    type Error = LdtkWorldAssetLoaderError;
    async fn load(
        &self,
        reader: &mut dyn Reader,
        _settings: &(),
        _load_context: &mut LoadContext<'_>,
    ) -> Result<Self::Asset, Self::Error> {
        let mut str = String::new();
        reader.read_to_string(&mut str).await?;
        Ok(ron::from_str(&str)?)
    }

    fn extensions(&self) -> &[&str] {
        &["ron"]
    }
}

#[derive(Debug, Error)]
pub enum TileSetAssetLoaderError {
    #[error("Could not read asset: {0}")]
    Io(#[from] std::io::Error),
    #[error("Could not parse asset: {0}")]
    Ron(#[from] SpannedError),
}

#[derive(Debug, Default, Asset, TypePath, serde::Serialize, serde::Deserialize)]
pub struct LevelTileSetInstances(pub Vec<TileSetInstance>);

#[derive(Default)]
pub struct TileSetAssetLoader;

impl AssetLoader for TileSetAssetLoader {
    type Asset = LevelTileSetInstances;
    type Settings = ();
    type Error = TileSetAssetLoaderError;
    async fn load(
        &self,
        reader: &mut dyn Reader,
        _settings: &(),
        _load_context: &mut LoadContext<'_>,
    ) -> Result<Self::Asset, Self::Error> {
        let mut str = String::new();
        reader.read_to_string(&mut str).await?;
        Ok(ron::from_str(&str)?)
    }

    fn extensions(&self) -> &[&str] {
        &["ron"]
    }
}

#[derive(Debug, Error)]
pub enum EntityAssetLoaderError {
    #[error("Could not read asset: {0}")]
    Io(#[from] std::io::Error),
    #[error("Could not parse asset: {0}")]
    Ron(#[from] SpannedError),
}

#[derive(Debug, Default, Asset, TypePath, serde::Serialize, serde::Deserialize)]
pub struct EntityInstances(pub Vec<LdtkEntityInstance>);

#[derive(Default)]
pub struct EntityAssetLoader;

impl AssetLoader for EntityAssetLoader {
    type Asset = EntityInstances;
    type Settings = ();
    type Error = EntityAssetLoaderError;
    async fn load(
        &self,
        reader: &mut dyn Reader,
        _settings: &(),
        _load_context: &mut LoadContext<'_>,
    ) -> Result<Self::Asset, Self::Error> {
        let mut str = String::new();
        reader.read_to_string(&mut str).await?;
        Ok(ron::from_str(&str)?)
    }

    fn extensions(&self) -> &[&str] {
        &["ron"]
    }
}

#[derive(Debug, Error)]
pub enum HotLdtkWorldAssetLoaderError {
    #[error("Could not read asset: {0}")]
    Io(#[from] std::io::Error),
    #[error("Could not parse asset: {0}")]
    SerdeJson(#[from] ldtk2::serde_json::Error),
}

#[derive(Asset, TypePath)]
pub struct HotLdtkWorld(pub ExtractLdtkWorld);

#[derive(Default)]
pub struct HotAssetLoader;

impl AssetLoader for HotAssetLoader {
    type Asset = HotLdtkWorld;
    type Settings = ();
    type Error = HotLdtkWorldAssetLoaderError;
    async fn load(
        &self,
        reader: &mut dyn Reader,
        _settings: &(),
        load_context: &mut LoadContext<'_>,
    ) -> Result<Self::Asset, Self::Error> {
        let mut str = String::new();
        reader.read_to_string(&mut str).await?;
        Ok(HotLdtkWorld(ExtractLdtkWorld::from_ldtk(
            load_context.path(),
            ldtk2::Ldtk::from_str(&str)?,
        )))
    }

    fn extensions(&self) -> &[&str] {
        &["ldtk"]
    }
}
