use crate::extract::levels::LevelMeta;
use crate::prelude::LevelMetaExt;
use crate::world::{LdtkWorld, LevelUid};
use crate::{
    process::{
        composites::Composite,
        entities::{DynLevelEntities, LevelEntities},
        tiles::LevelTileSets,
    },
    HotWorld,
};
use bevy::utils::hashbrown::hash_map::Values;
use bevy::{
    ecs::{component::ComponentId, world::DeferredWorld},
    prelude::*,
    utils::HashMap,
};
use std::fmt::Debug;

/// Queued levels for a [`World`] to spawn.
#[derive(Debug, Default, Component)]
pub struct LevelLoader {
    load: Vec<(LevelUid, Vec3)>,
    despawn: Vec<LevelUid>,
    /// Local registry is drained into global registry every frame.
    registry: LevelMetaRegistry,
}

impl LevelLoader {
    pub fn levels(levels: impl LevelSet) -> Self {
        Self::levels_with_offset(levels, Vec2::ZERO)
    }

    pub fn levels_with_offset(levels: impl LevelSet, offset: Vec2) -> Self {
        let mut registry = LevelMetaRegistry::default();

        Self {
            load: levels
                .into_loader(&mut registry)
                .into_iter()
                .map(|(uid, position)| (uid, position + offset.extend(0.)))
                .collect(),
            registry,
            ..Default::default()
        }
    }

    /// Despawn all levels of type [`LevelUid`].
    pub fn despawn(&mut self, uid: LevelUid) -> &mut Self {
        self.despawn.push(uid);
        self
    }
}

/// Contains [`LevelMeta`] information for every level in a [`LevelSet`] spawned with a [`LevelLoader`].
///
/// WARN: If you did not spawn a level with a [`LevelLoader`], you will not find the level's meta
/// here.
#[derive(Debug, Default, Resource)]
pub struct LevelMetaRegistry(HashMap<LevelUid, LevelMeta>);

impl LevelMetaRegistry {
    pub fn register(&mut self, uid: LevelUid, meta: LevelMeta) {
        self.0.insert(uid, meta);
    }

    pub fn meta(&self, uid: LevelUid) -> Option<&LevelMeta> {
        self.0.get(&uid)
    }

    pub fn metas(&self) -> Values<LevelUid, LevelMeta> {
        self.0.values()
    }
}

pub fn sync_level_meta(
    mut registry: ResMut<LevelMetaRegistry>,
    mut loader_query: Query<&mut LevelLoader>,
) {
    for mut loader in loader_query.iter_mut() {
        registry.0.extend(loader.registry.0.drain());
    }
}

/// Levels loaded by [`LoadLevels`] for a [`World`].
#[derive(Default, Component)]
pub struct LoadedLevels(HashMap<LevelUid, Vec<Entity>>);

impl LoadedLevels {
    pub fn push(&mut self, uid: LevelUid, entity: Entity) -> &mut Self {
        self.0.entry(uid).or_default().push(entity);
        self
    }

    pub fn remove(&mut self, uid: LevelUid) -> &mut Self {
        let _ = self.0.remove(&uid);
        self
    }

    pub fn contains(&self, uid: LevelUid) -> bool {
        self.0.contains_key(&uid)
    }
}

/// Root of an LDtk level.
///
/// Level entities, tiles, and composites are stored in [`LevelEntities`], [`LevelTileSets`], and
/// [`Composite`]s, respectively.
///
/// If the [`HotWorld`] is used, then [`LevelEntities`] will instead be [`DynLevelEntities`].
#[derive(Debug, Component)]
#[require(Visibility, Transform)]
#[component(on_remove = remove_level)]
pub struct Level {
    uid: LevelUid,
    meta: LevelMeta,
}

impl Level {
    pub fn new(uid: LevelUid, meta: LevelMeta) -> Self {
        Self { uid, meta }
    }

    pub fn uid(&self) -> LevelUid {
        self.uid
    }

    pub fn meta(&self) -> &LevelMeta {
        &self.meta
    }
}

fn remove_level(mut world: DeferredWorld, entity: Entity, _: ComponentId) {
    let uid = world.get::<Level>(entity).unwrap().uid();
    if let Some(parent) = world.get::<Parent>(entity) {
        if let Some(mut level_cache) = world.get_mut::<LoadedLevels>(parent.get()) {
            level_cache.remove(uid);
        }
    }
}

pub fn spawn_levels(
    mut commands: Commands,
    mut world_query: Query<(
        Entity,
        &super::World,
        &mut LevelLoader,
        &mut LoadedLevels,
        Option<&HotWorld>,
    )>,
    worlds: Res<Assets<LdtkWorld>>,
    server: Res<AssetServer>,
    meta_registry: Res<LevelMetaRegistry>,
) {
    for (world_entity, world, mut load_levels, mut level_cache, hot_reload) in
        world_query.iter_mut()
    {
        if let Some(world) = worlds.get(&world.0) {
            for (level, position) in load_levels.load.drain(..) {
                let level_entity = commands
                    .spawn((
                        Level::new(level, *meta_registry.meta(level).unwrap()),
                        Transform::from_translation(Vec3::new(position.x, -position.y, position.z)),
                    ))
                    .with_children(|root| {
                        if hot_reload.is_some() {
                            if let Some((_, path)) =
                                world.entities().find(|(uid, _)| **uid == level)
                            {
                                root.spawn(DynLevelEntities(server.load(path)));
                            }
                        } else if let Some(uid) = world.levels().iter().find(|uid| **uid == level) {
                            root.spawn(LevelEntities(*uid));
                        }

                        if let Some((_, path)) = world.tiles().find(|(uid, _)| **uid == level) {
                            root.spawn(LevelTileSets(server.load(path)));
                        }

                        for (_, path) in world.composites().filter(|(uid, _)| uid.0 == level) {
                            root.spawn(Composite(path.to_string_lossy().to_string()));
                        }
                    })
                    .id();

                level_cache.push(level, level_entity);
                commands.entity(world_entity).add_child(level_entity);
            }
        }
    }
}

pub fn despawn_levels(
    mut commands: Commands,
    mut loaders: Query<(&mut LevelLoader, &LoadedLevels)>,
) {
    for (mut loader, loaded) in loaders.iter_mut() {
        for uid in loader.despawn.drain(..) {
            if let Some(entities) = loaded.0.get(&uid) {
                for entity in entities.iter() {
                    commands.entity(*entity).despawn_recursive();
                }
            }
        }
    }
}

pub trait LevelSet {
    fn into_loader(self, registry: &mut LevelMetaRegistry) -> Vec<(LevelUid, Vec3)>;
}

impl<T: LevelMetaExt> LevelSet for T {
    fn into_loader(self, registry: &mut LevelMetaRegistry) -> Vec<(LevelUid, Vec3)> {
        registry.0.insert(T::uid(), T::meta());
        vec![(T::uid(), T::world_position().extend(0.))]
    }
}

impl LevelSet for LevelMeta {
    fn into_loader(self, registry: &mut LevelMetaRegistry) -> Vec<(LevelUid, Vec3)> {
        registry.0.insert(self.uid, self);
        vec![(self.uid, self.world_position.extend(0.))]
    }
}

macro_rules! impl_level_set {
    ($($p:ident),*) => {
        impl<$($p),*> LevelSet for ($($p),*)
        where
            $($p: LevelSet),*
        {
            #[allow(non_snake_case)]
            fn into_loader(self, registry: &mut LevelMetaRegistry) -> Vec<(LevelUid, Vec3)> {
                let ($($p),*) = self;
                vec![$($p.into_loader(registry)),*]
                    .into_iter()
                    .flatten()
                    .collect()
            }
        }
    };
}

bevy::utils::all_tuples!(impl_level_set, 2, 15, P);
