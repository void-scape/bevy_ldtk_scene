use crate::{
    asset_loader::EntityInstances,
    extract::{
        composites::ExtractComposites,
        entities::{
            ExtractCompEntities, ExtractEntityInstances, ExtractEntityTypes, LdtkEntityInstance,
        },
        enums::ExtractEnums,
        levels::ExtractLevelUids,
        tiles::{ExtractTileSets, TileSetInstance},
        world::{
            ExtractedComponent, FromLdtkWorld, IntoExtractedComponent, WorldDirPath, WorldPlugin,
        },
    },
    process::composites::CompositeProcessor,
};
use bevy::{platform::collections::HashMap, prelude::*};
use image::{ImageBuffer, ImageFormat, Rgba};
use ldtk2::Ldtk;
use quote::TokenStreamExt;
use std::{
    fs::File,
    io::{self, Write},
    path::{Path, PathBuf},
};
use thiserror::Error;

#[derive(Debug, TypePath, Asset, serde::Serialize, serde::Deserialize)]
pub struct LdtkWorld {
    io: WorldIO,
    levels: Vec<LevelUid>,
}

impl LdtkWorld {
    pub fn save<P: AsRef<Path>>(self, path: P) -> Result<(), io::Error> {
        let ron = ron::ser::to_string_pretty(&self, Default::default()).unwrap();
        let mut file = File::create(path.as_ref())?;
        file.write_all(ron.as_bytes())?;
        Ok(())
    }

    pub fn levels(&self) -> &[LevelUid] {
        &self.levels
    }

    pub fn tiles(&self) -> impl Iterator<Item = (&LevelUid, &Path)> {
        self.io.scene_tiles.iter().map(|(uid, p)| (uid, p.as_ref()))
    }

    pub fn entities(&self) -> impl Iterator<Item = (&LevelUid, &Path)> {
        self.io
            .scene_entities
            .iter()
            .map(|(uid, p)| (uid, p.as_ref()))
    }

    pub fn composites(&self) -> impl Iterator<Item = (&(LevelUid, LayerUid), &Path)> {
        self.io.composites.iter().map(|(uid, p)| (uid, p.as_ref()))
    }
}

#[derive(Debug, Default)]
pub struct ExtractLdtkWorld {
    ldtk: Option<Ldtk>,
    path: PathBuf,
    io: WorldIO,
    data: String,
}

impl ExtractLdtkWorld {
    pub fn new<P: AsRef<Path>>(path: P) -> Result<Self, ldtk2::Error> {
        let ldtk = Ldtk::from_path(&path)?;
        Ok(Self::from_ldtk(path, ldtk))
    }

    pub fn from_ldtk<P: AsRef<Path>>(path: P, ldtk: ldtk2::Ldtk) -> Self {
        Self {
            path: path.as_ref().to_owned(),
            io: WorldIO::new(
                path.as_ref()
                    .parent()
                    .expect("ldtk file has no parent dir")
                    .to_owned(),
            ),
            ldtk: Some(ldtk),
            data: String::new(),
        }
    }

    /// Default extraction of the data held by the LDtk world.
    pub fn extract(&mut self) -> Result<&Self, ExtractError> {
        self.extract_with((
            ExtractComposites,
            ExtractEnums,
            ExtractEntityTypes,
            ExtractTileSets,
            ExtractCompEntities,
            ExtractEntityInstances,
            ExtractLevelUids,
        ))
    }

    pub fn extract_with<C: ExtractedComponent, E: IntoExtractedComponent<C>>(
        &mut self,
        components: E,
    ) -> Result<&Self, ExtractError> {
        let extraction = self.extract_component(components);

        let mut output = proc_macro2::TokenStream::new();
        extraction.io(&mut self.io)?;
        let plugin = WorldPlugin::from_extraction(&extraction)
            .processor(CompositeProcessor)
            .build(self);
        output.append_all(plugin);
        extraction.global(&mut output);

        let parsed: syn::File =
            syn::parse2(output).unwrap_or_else(|e| panic!("Error parsing generated code: {e}"));
        self.data = prettyplease::unparse(&parsed);

        Ok(self)
    }

    /// Default extraction of the data written to disk that is held by the LDtk world.
    pub fn extract_io(&mut self) -> Result<&Self, ExtractError> {
        self.extract_with((ExtractComposites, ExtractTileSets, ExtractEntityInstances))
    }

    pub fn extract_io_with<C: ExtractedComponent, E: IntoExtractedComponent<C>>(
        &mut self,
        components: E,
    ) -> Result<&Self, ExtractError> {
        let extraction = self.extract_component(components);
        extraction.io(&mut self.io)?;

        Ok(self)
    }

    pub fn write<P: AsRef<Path>>(&self, path: P) -> Result<&Self, ExtractError> {
        let mut file = File::create(path.as_ref())?;
        file.write_all(self.data.as_bytes())?;

        Ok(self)
    }

    pub fn world(&self) -> LdtkWorld {
        LdtkWorld {
            io: self.io.clone(),
            levels: self.levels().map(|l| LevelUid(l.uid)).collect(),
        }
    }

    pub fn extract_component<E, I>(&self, component: I) -> E
    where
        E: ExtractedComponent,
        I: IntoExtractedComponent<E>,
    {
        component.extract(self, I::Context::from_world(self))
    }

    pub fn ldtk(&self) -> &ldtk2::Ldtk {
        self.ldtk.as_ref().unwrap()
    }

    pub fn path(&self) -> &Path {
        &self.path
    }

    pub fn levels(&self) -> impl Iterator<Item = &ldtk2::Level> {
        self.ldtk().levels.iter()
    }

    pub fn layers(
        &self,
    ) -> impl Iterator<Item = (&ldtk2::Level, impl Iterator<Item = &ldtk2::LayerInstance>)> {
        self.levels()
            .filter_map(|l| l.layer_instances.as_ref().map(|layers| (l, layers.iter())))
    }

    pub fn tiles(
        &self,
    ) -> impl Iterator<
        Item = (
            &ldtk2::Level,
            &ldtk2::LayerInstance,
            impl Iterator<Item = &ldtk2::TileInstance>,
            f32,
        ),
    > {
        self.layers().flat_map(|(level, layers)| {
            layers
                .enumerate()
                .filter_map(move |(z, l)| match &*l.layer_instance_type {
                    "IntGrid" => Some((
                        level,
                        l,
                        l.auto_layer_tiles.iter(),
                        (level
                            .layer_instances
                            .as_ref()
                            .map(|l| l.len())
                            .unwrap_or_default()
                            - z) as f32,
                    )),
                    "AutoLayer" => Some((
                        level,
                        l,
                        l.auto_layer_tiles.iter(),
                        (level
                            .layer_instances
                            .as_ref()
                            .map(|l| l.len())
                            .unwrap_or_default()
                            - z) as f32,
                    )),
                    "Tiles" => Some((
                        level,
                        l,
                        l.grid_tiles.iter(),
                        (level
                            .layer_instances
                            .as_ref()
                            .map(|l| l.len())
                            .unwrap_or_default()
                            - z) as f32,
                    )),
                    _ => None,
                })
        })
    }

    pub fn entities(
        &self,
    ) -> impl Iterator<
        Item = (
            &ldtk2::Level,
            &ldtk2::LayerInstance,
            impl Iterator<Item = &ldtk2::EntityInstance>,
            f32,
        ),
    > {
        self.layers().flat_map(|(level, layers)| {
            layers
                .enumerate()
                .filter_map(move |(z, l)| match &*l.layer_instance_type {
                    "Entities" => Some((
                        level,
                        l,
                        l.entity_instances.iter(),
                        (level
                            .layer_instances
                            .as_ref()
                            .map(|l| l.len())
                            .unwrap_or_default()
                            - z) as f32,
                    )),
                    _ => None,
                })
        })
    }
}

#[derive(Debug, Error)]
pub enum ExtractError {
    #[error("Could not load asset: {0}")]
    Io(#[from] std::io::Error),
    #[error("Could not create ldtk: {0}")]
    Ldtk2(#[from] ldtk2::Error),
    #[error("Could not parse LDtk file: {0}")]
    SerdeJsonParse(#[from] ldtk2::serde_json::Error),
    #[error("Could not save composite image: {0}")]
    Image(#[from] image::ImageError),
}

#[derive(Debug, Default, Clone, serde::Deserialize, serde::Serialize)]
pub struct WorldIO {
    #[serde(skip)]
    dir: WorldDirPath,
    scene_tiles: HashMap<LevelUid, PathBuf>,
    scene_entities: HashMap<LevelUid, PathBuf>,
    composites: HashMap<(LevelUid, LayerUid), PathBuf>,
}

impl WorldIO {
    pub fn new(dir: impl Into<PathBuf>) -> Self {
        Self {
            dir: WorldDirPath(dir.into()),
            ..Default::default()
        }
    }
}

pub type IOResult = Result<(), ExtractError>;

impl WorldIO {
    pub fn save_entities(&mut self, uid: LevelUid, data: &[LdtkEntityInstance]) -> IOResult {
        let name = &format!("{}.entities.ron", uid.0);

        let mut file = File::create(PathBuf::new().join(&self.dir.0).join(name))?;
        file.write_all(
            ron::ser::to_string_pretty(&EntityInstances(data.to_vec()), Default::default())
                .unwrap()
                .as_bytes(),
        )?;

        self.scene_entities
            .insert(uid, PathBuf::new().join(self.dir.asset_path()).join(name));

        Ok(())
    }

    pub fn save_tileset(&mut self, uid: LevelUid, data: &[TileSetInstance]) -> IOResult {
        let name = &format!("{}.tiles.ron", uid.0);

        let mut file = File::create(PathBuf::new().join(&self.dir.0).join(name))?;
        file.write_all(
            ron::ser::to_string_pretty(
                &crate::asset_loader::LevelTileSetInstances(data.to_vec()),
                Default::default(),
            )
            .unwrap()
            .as_bytes(),
        )?;

        self.scene_tiles
            .insert(uid, PathBuf::new().join(self.dir.asset_path()).join(name));

        Ok(())
    }

    pub fn save_composite(
        &mut self,
        uid: (LevelUid, LayerUid),
        name: &str,
        image: &ImageBuffer<Rgba<u8>, Vec<u8>>,
    ) -> Result<(), ExtractError> {
        let mut file = File::create(PathBuf::new().join(&self.dir.0).join(name))?;
        image.write_to(&mut file, ImageFormat::Png)?;
        self.composites
            .insert(uid, PathBuf::new().join(self.dir.asset_path()).join(name));
        Ok(())
    }
}

#[derive(
    Debug,
    Default,
    Clone,
    Copy,
    PartialEq,
    Eq,
    Hash,
    Component,
    serde::Deserialize,
    serde::Serialize,
)]
pub struct LevelUid(pub i64);

#[derive(
    Debug,
    Default,
    Clone,
    Copy,
    PartialEq,
    Eq,
    Hash,
    Component,
    serde::Deserialize,
    serde::Serialize,
)]
pub struct LayerUid(pub i64);
