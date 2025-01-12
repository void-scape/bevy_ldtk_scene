#![allow(clippy::type_complexity)]

use asset_loader::HotLdtkWorld;
use bevy::prelude::*;
use process::{
    composites::Composite, entities::DynLevelEntities, tiles::LevelTileSets, ProcessSet,
};
use std::{fmt::Debug, path::PathBuf};
use world::{ExtractLdtkWorld, LdtkWorld};

pub extern crate ldtk2;
pub extern crate serde;
pub extern crate typetag;

pub mod asset_loader;
pub mod extract;
pub mod process;
pub mod world;

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
            .add_systems(
                PreUpdate,
                (
                    (spawn_world, update_world).before(ProcessSet),
                    (
                        process::tiles::process_tilesets,
                        process::tiles::update_tilesets,
                        process::entities::spawn_entities,
                        process::entities::process_entities,
                        process::entities::update_entities,
                    )
                        .in_set(ProcessSet),
                ),
            );
    }
}

#[derive(Debug, Component)]
#[require(Visibility, Transform)]
pub struct World(pub Handle<LdtkWorld>);

#[derive(Debug, Component)]
#[require(Visibility, Transform)]
pub struct HotWorld(pub Handle<HotLdtkWorld>);

pub fn spawn_world(
    mut commands: Commands,
    world_query: Query<(Entity, &World), Without<Spawned>>,
    worlds: Res<Assets<LdtkWorld>>,
    server: Res<AssetServer>,
    // registry: Res<LevelEntityRegistry>,
) {
    for (entity, world) in world_query.iter() {
        if let Some(world) = worlds.get(&world.0) {
            commands.entity(entity).insert(Spawned);

            for (_, path) in world.tiles() {
                // if let Some(id) = registry.entities.get(uid) {
                //     commands.run_system(*id);
                // }

                commands
                    .entity(entity)
                    .with_child(LevelTileSets(server.load(path)));
            }

            for (_, path) in world.entities() {
                commands
                    .entity(entity)
                    .with_child(DynLevelEntities(server.load(path)));
            }

            for (_, path) in world.composites() {
                commands
                    .entity(entity)
                    .with_child(Composite(path.to_string_lossy().to_string()));
            }
        }
    }
}

pub fn update_world(mut reader: EventReader<AssetEvent<HotLdtkWorld>>, server: Res<AssetServer>) {
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

#[derive(Component)]
pub struct Spawned;
