use crate::*;
use bevy::utils::hashbrown::HashMap;
use extract::{
    enums::{EnumRegistry, EnumUid},
    world::{ExtractedComponent, FromLdtkWorld, IntoExtractedComponent, WorldDirPath},
};
use ldtk2::TileInstance;
use quote::{quote, TokenStreamExt};
use std::path::PathBuf;
use world::{ExtractLdtkWorld, IOResult, LevelUid, WorldIO};

pub trait LayerTiles {
    fn tiles(&self) -> Option<(TileSetUid, impl Iterator<Item = &TileInstance>)>;
}

/// Filters tileset without a [`TileSetUid`] (definition).
impl LayerTiles for ldtk2::LayerInstance {
    fn tiles(&self) -> Option<(TileSetUid, impl Iterator<Item = &TileInstance>)> {
        self.tileset_def_uid.map(|uid| {
            (
                TileSetUid(uid),
                match &*self.layer_instance_type {
                    "IntGrid" | "AutoLayer" => self.auto_layer_tiles.iter(),
                    "Tiles" => self.grid_tiles.iter(),
                    // Should be emtpy
                    _ => self.grid_tiles.iter(),
                },
            )
        })
    }
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct TileSet {
    pub uid: TileSetUid,
    pub asset_image: String,
    pub rel_image: String,
    pub tile_size: u32,
    pub width: u32,
    pub height: u32,
    pub spacing: u32,
    pub padding: u32,
}

impl TileSet {
    pub fn from_def(def: &ldtk2::TilesetDefinition, dir: WorldDirPath) -> Option<Self> {
        def.rel_path.as_ref().map(|path| Self {
            uid: TileSetUid(def.uid),
            asset_image: PathBuf::new()
                .join(dir.asset_path())
                .join(path.clone())
                .to_string_lossy()
                .to_string(),
            rel_image: path.clone(),
            tile_size: def.tile_grid_size as u32,
            width: def.c_wid as u32,
            height: def.c_hei as u32,
            spacing: def.spacing as u32,
            padding: def.padding as u32,
        })
    }
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct TileSetInstance {
    pub tileset: TileSet,
    pub tiles: Vec<Tile>,
    pub z: f32,
}

#[derive(
    Debug, Default, Clone, Copy, PartialEq, Eq, Hash, serde::Serialize, serde::Deserialize,
)]
pub struct TileSetUid(pub i64);

pub struct TileSetRegistry {
    registry: HashMap<TileSetUid, TileSet>,
}

impl TileSetRegistry {
    pub fn get(&self, uid: TileSetUid) -> Option<&TileSet> {
        self.registry.get(&uid)
    }

    pub fn tileset(&self, uid: TileSetUid) -> &TileSet {
        self.get(uid).expect("unregistered tileset")
    }
}

impl FromLdtkWorld for TileSetRegistry {
    fn from_world(world: &ExtractLdtkWorld) -> Self {
        Self {
            registry: world
                .ldtk()
                .defs
                .tilesets
                .iter()
                .filter_map(|t| {
                    TileSet::from_def(t, <WorldDirPath as FromLdtkWorld>::from_world(world))
                        .map(|t| (t.uid, t))
                })
                .collect(),
        }
    }
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct Tile {
    pub xy: Vec2,
    pub index: u32,
    pub sprite: Option<SpriteMeta>,
}

impl Tile {
    pub fn new(xy: Vec2, index: u32, sprite: SpriteMeta) -> Self {
        Self {
            xy,
            index,
            sprite: Some(sprite),
        }
    }

    pub fn from_xy(xy: Vec2, index: u32) -> Self {
        Self {
            xy,
            index,
            sprite: None,
        }
    }
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct SpriteMeta {
    pub flip_x: bool,
    pub flip_y: bool,
}

pub struct ExtractTileSets;

impl IntoExtractedComponent<ExtractedTileSets> for ExtractTileSets {
    type Context = (TileSetRegistry, EnumRegistry);

    fn extract(
        self,
        world: &ExtractLdtkWorld,
        (tileset_registry, enum_registry): (TileSetRegistry, EnumRegistry),
    ) -> ExtractedTileSets {
        let mut components = Vec::new();
        for tileset in world
            .ldtk()
            .defs
            .tilesets
            .iter()
            .filter(|t| t.tags_source_enum_uid.is_some())
        {
            let tileset_enum =
                enum_registry.definition_from_uid(&EnumUid(tileset.tags_source_enum_uid.unwrap()));
            for enum_tag in tileset.enum_tags.iter() {
                let component = tileset_enum
                    .variant(&enum_tag.enum_value_id)
                    .expect("unregistered enum variant");
                components.push(TileSetComponents {
                    uid: TileSetUid(tileset.uid),
                    component,
                    tiles: enum_tag.tile_ids.iter().map(|id| *id as u32).collect(),
                })
            }
        }

        let mut instances_hash = HashMap::<i64, LevelTileSets>::default();
        for (level, layer, tiles, z) in world
            .tiles()
            .filter(|(_, l, _, _)| !l.identifier.to_lowercase().contains("background"))
            .filter(|(_, l, _, _)| l.tileset_def_uid.is_some())
        {
            let is_composite = layer.identifier.to_lowercase().contains("composite");
            let entry = instances_hash
                .entry(level.uid)
                .or_insert_with(|| LevelTileSets {
                    level_uid: LevelUid(level.uid),
                    level_ident: level.identifier.clone(),
                    instances: Vec::new(),
                });

            let max_x = level.px_wid as f32;
            let max_y = level.px_hei as f32;
            entry.instances.push(TileSetInstance {
                tileset: tileset_registry
                    .tileset(TileSetUid(layer.tileset_def_uid.unwrap()))
                    .clone(),
                tiles: tiles
                    .filter_map(|t| {
                        let xy = Vec2::new(t.px[0] as f32, t.px[1] as f32);
                        if xy.x < max_x && xy.y < max_y {
                            Some(if is_composite {
                                Tile::from_xy(xy, t.t as u32)
                            } else {
                                Tile::new(
                                    xy,
                                    t.t as u32,
                                    SpriteMeta {
                                        flip_x: t.f & 1 == 1,
                                        flip_y: t.f & 2 == 2,
                                    },
                                )
                            })
                        } else {
                            None
                        }
                    })
                    .collect(),
                z,
            });
        }

        ExtractedTileSets {
            tilesets: instances_hash.into_values().collect(),
            components,
        }
    }
}

pub struct LevelTileSets {
    level_uid: LevelUid,
    level_ident: String,
    instances: Vec<TileSetInstance>,
}

pub struct TileSetComponents {
    uid: TileSetUid,
    component: proc_macro2::TokenStream,
    tiles: Vec<u32>,
}

pub struct ExtractedTileSets {
    tilesets: Vec<LevelTileSets>,
    components: Vec<TileSetComponents>,
}

impl ExtractedComponent for ExtractedTileSets {
    fn io(&self, io: &mut WorldIO) -> IOResult {
        for level_tilesets in self.tilesets.iter() {
            io.save_tileset(level_tilesets.level_uid, &level_tilesets.instances)?;
        }

        Ok(())
    }

    fn plugin(&self, output: &mut proc_macro2::TokenStream) {
        for tileset_component in self.components.iter() {
            let uid = tileset_component.uid.0;
            let component = &tileset_component.component;
            let tiles = tileset_component.tiles.iter();

            output.append_all(quote! {
                ::bevy_ldtk_scene::extract::tiles::TileSetAppExt::register_tileset_component(
                    app,
                    ::bevy_ldtk_scene::extract::tiles::TileSetUid(#uid),
                    #component,
                    &[#(#tiles,)*]
                );
            });
        }
    }
}

// fn save_tile_set_instances(instances: Vec<TileSetInstance>) -> String {
//     let mut app = App::default();
//     app.register_type::<TileSetInstance>();
//     let world = app.world_mut();
//
//     for instance in instances.into_iter().filter(|i| !i.tiles.is_empty()) {
//         world.spawn(instance);
//     }
//
//     let scene = DynamicSceneBuilder::from_world(world)
//         .deny_resource::<Time<Real>>()
//         .extract_entities(world.iter_entities().map(|entity| entity.id()))
//         .build();
//     let type_registry = world.resource::<AppTypeRegistry>();
//     let type_registry = type_registry.read();
//     scene.serialize(&type_registry).unwrap()
// }

pub trait TileComponent: 'static + Send + Sync {
    fn insert(&self, entity: &mut EntityCommands<'_>);
}

impl<T> TileComponent for T
where
    T: Clone + Component,
{
    fn insert(&self, entity: &mut EntityCommands<'_>) {
        entity.insert(self.clone());
    }
}

#[derive(Default, Resource)]
pub struct TileSetComponentRegistry(
    pub(crate) HashMap<TileSetUid, Vec<(Box<dyn TileComponent>, &'static [u32])>>,
);

pub trait TileSetAppExt {
    fn register_tileset_component(
        &mut self,
        uid: TileSetUid,
        component: impl TileComponent,
        tiles: &'static [u32],
    ) -> &mut Self;
}

impl TileSetAppExt for App {
    fn register_tileset_component(
        &mut self,
        uid: TileSetUid,
        component: impl TileComponent,
        tiles: &'static [u32],
    ) -> &mut Self {
        let mut registry = self
            .world_mut()
            .get_resource_or_insert_with::<TileSetComponentRegistry>(|| {
                TileSetComponentRegistry::default()
            });
        let entry = registry.0.entry(uid).or_default();
        entry.push((Box::new(component), tiles));
        self
    }
}
