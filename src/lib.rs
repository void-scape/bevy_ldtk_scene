#![allow(clippy::type_complexity)]

use asset_loader::HotLdtkWorld;
use bevy::prelude::*;
use levels::{LevelLoader, LevelMetaRegistry, LoadedLevels};
use process::{entities::WorldlyEntities, ProcessSet};
use std::{fmt::Debug, path::PathBuf};
use world::{ExtractLdtkWorld, LdtkWorld};

pub extern crate ldtk2;
pub extern crate serde;
pub extern crate typetag;

pub mod asset_loader;
pub mod extract;
pub mod levels;
pub mod process;
pub mod world;

pub mod prelude {
    pub use crate::extract::composites::*;
    pub use crate::extract::entities::*;
    pub use crate::extract::enums::*;
    pub use crate::extract::levels::*;
    pub use crate::extract::tiles::*;
    pub use crate::levels::LevelLoader;
    pub use crate::levels::*;
    pub use crate::world::*;
    pub use crate::{HotWorld, World};
}

pub struct LdtkScenePlugin;

impl Plugin for LdtkScenePlugin {
    fn build(&self, app: &mut App) {
        app.init_asset::<world::LdtkWorld>()
            .init_asset::<asset_loader::HotLdtkWorld>()
            .init_asset::<asset_loader::LevelTileSetInstances>()
            .init_asset::<asset_loader::EntityInstances>()
            .init_asset_loader::<asset_loader::LdtkWorldAssetLoader>()
            .init_asset_loader::<asset_loader::HotAssetLoader>()
            .init_asset_loader::<asset_loader::TileSetAssetLoader>()
            .init_asset_loader::<asset_loader::EntityAssetLoader>()
            .insert_resource(LevelMetaRegistry::default())
            .add_systems(
                PreUpdate,
                (
                    (
                        levels::sync_level_meta,
                        levels::spawn_levels,
                        levels::despawn_levels,
                        reload_world,
                    )
                        .chain()
                        .before(ProcessSet),
                    (
                        process::tiles::process_tilesets,
                        process::tiles::update_tilesets,
                        process::entities::spawn_entities,
                        process::entities::process_dyn_entities,
                        process::entities::update_dyn_entities,
                    )
                        .in_set(ProcessSet),
                ),
            );
    }
}

/// Root of a collection of [`levels::Level`] entities.
#[derive(Debug, Component)]
#[require(Visibility, Transform, LevelLoader, LoadedLevels, WorldlyEntities)]
pub struct World(pub Handle<LdtkWorld>);

/// When used in conjunction with [`World`], marks this entity for dynamic hot reloading.
#[derive(Debug, Component)]
#[require(Visibility, Transform)]
pub struct HotWorld(pub Handle<HotLdtkWorld>);

pub fn reload_world(mut reader: EventReader<AssetEvent<HotLdtkWorld>>, server: Res<AssetServer>) {
    for event in reader.read() {
        if let AssetEvent::Modified { id } = event {
            if let Some(path) = server.get_path(id.untyped()) {
                ExtractLdtkWorld::new(PathBuf::new().join("assets/").join(path.path()))
                    .unwrap()
                    .extract_io()
                    .unwrap();
            }
        }
    }
}
