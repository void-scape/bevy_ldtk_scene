use crate::{
    asset_loader::{EntityInstances, HotLdtkWorld},
    extract::entities::{Atlas, DynLdtkEntity, EntityUid, ExtractEntityFields},
    levels::Level,
    world::LevelUid,
    HotWorld,
};
use bevy::{
    ecs::system::SystemId,
    platform::collections::{HashMap, HashSet},
    prelude::*,
    sprite::Anchor,
};

/// Root entity of a set of [`LevelEntity`]s.
#[derive(Debug, Clone, Copy, Component)]
#[require(Transform, Visibility)]
pub struct LevelEntities(pub LevelUid);

/// Spawned as a child of a [`LevelEntities`].
#[derive(Component)]
pub struct LevelEntity;

/// Spawned as a child of the [`World`].
#[derive(Component)]
pub struct WorldlyEntity;

#[derive(Debug, Default, Resource)]
pub struct LevelEntityRegistry {
    pub entities: HashMap<LevelUid, SystemId<In<EntitySystemInput>>>,
}

#[derive(Debug)]
pub struct EntitySystemInput {
    level: Entity,
    world: Entity,
}

impl EntitySystemInput {
    pub fn new(level: Entity, world: Entity) -> Self {
        Self { level, world }
    }

    pub fn level(&self) -> Entity {
        self.level
    }

    pub fn world(&self) -> Entity {
        self.world
    }
}

#[derive(Component)]
pub struct Populated;

pub fn spawn_entities(
    mut commands: Commands,
    entity_query: Query<(Entity, &LevelEntities, &ChildOf), Without<Populated>>,
    levels_query: Query<&ChildOf, With<Level>>,
    registry: Res<LevelEntityRegistry>,
) {
    for (entity, level_entities, level_entity) in entity_query.iter() {
        if let Some(id) = registry.entities.get(&level_entities.0) {
            if let Ok(world_entity) = levels_query.get(level_entity.parent()) {
                commands
                    .run_system_with(*id, EntitySystemInput::new(entity, world_entity.parent()));
            }
        }

        commands.entity(entity).insert(Populated);
    }
}

/// Root entity of a set of dynamic [`LevelEntity`]s.
#[derive(Component)]
#[require(Transform, Visibility)]
pub struct DynLevelEntities(pub Handle<EntityInstances>);

#[derive(Default, Resource)]
pub struct LevelDynEntityRegistry {
    pub entities: HashMap<EntityUid, Box<dyn DynLdtkEntity>>,
}

#[derive(Debug, Default, Component)]
pub struct WorldlyEntities(pub HashSet<EntityUid>);

#[allow(clippy::too_many_arguments)]
pub(crate) fn process_dyn_entities(
    mut commands: Commands,
    server: Res<AssetServer>,
    entity_query: Query<(Entity, &DynLevelEntities, &ChildOf), Without<Populated>>,
    entities: Res<Assets<EntityInstances>>,
    mut layouts: ResMut<Assets<TextureAtlasLayout>>,
    registry: Res<LevelDynEntityRegistry>,
    world: Option<Single<(Entity, &HotWorld, &mut WorldlyEntities)>>,
    worlds: Res<Assets<HotLdtkWorld>>,
    levels: Query<&Transform, With<Level>>,
) {
    let Some((world_entity, world, mut worldly)) = world.map(|w| w.into_inner()) else {
        return;
    };

    let Some(world) = worlds.get(&world.0) else {
        return;
    };

    if entity_query.is_empty() {
        return;
    }

    let fields = world.0.extract_component(ExtractEntityFields);

    let mut atlas_registry = HashMap::<(UVec2, Vec<URect>), Handle<TextureAtlasLayout>>::default();
    for (entity, dyn_entities, parent) in entity_query.iter() {
        let Some(entities) = entities.get(&dyn_entities.0) else {
            continue;
        };

        let Ok(level) = levels.get(parent.parent()) else {
            continue;
        };

        for ldtk_entity in entities.0.iter() {
            let new_entity = if let Some(component) = registry.entities.get(&ldtk_entity.uid) {
                if let Some(fields) = fields.0.get(&ldtk_entity.instance) {
                    if ldtk_entity.worldly && !worldly.0.insert(ldtk_entity.uid) {
                        continue;
                    } else {
                        let new_entity = if ldtk_entity.worldly {
                            let id = commands
                                .spawn((
                                    WorldlyEntity,
                                    Transform::from_translation(
                                        ldtk_entity.xyz + level.translation,
                                    ),
                                ))
                                .id();
                            commands.entity(world_entity).add_child(id);

                            info!(
                                "spawning worldly entity {:?} at {:?}",
                                component,
                                ldtk_entity.xyz + level.translation
                            );

                            id
                        } else {
                            let id = commands
                                .spawn((LevelEntity, Transform::from_translation(ldtk_entity.xyz)))
                                .id();
                            commands.entity(entity).add_child(id);

                            info!(
                                "spawning level entity {:?} at {:?}",
                                component,
                                ldtk_entity.xyz + level.translation
                            );

                            id
                        };

                        component.insert(fields, &mut commands.entity(new_entity));
                        new_entity
                    }
                } else {
                    error!("Failed to retrieve entity fields: {:?}", ldtk_entity.uid);
                    continue;
                }
            } else {
                error!(
                    "Entity type is unknown: {:?}\n \
                    This plugin cannot load newly created entity types until after rebuilding!",
                    ldtk_entity.uid
                );
                continue;
            };

            if let Some(entity_sprite) = &ldtk_entity.sprite {
                let anchor = match entity_sprite.pivot.to_array() {
                    [0.0, 0.0] => Anchor::TopLeft,
                    [0.5, 0.5] => Anchor::Center,
                    _ => panic!("unrecognized pivot: {:?}", entity_sprite.pivot),
                };

                let mut sprite = Sprite {
                    image: server.load(&entity_sprite.image),
                    anchor,
                    ..Default::default()
                };

                match &entity_sprite.atlas {
                    Atlas::Rect(rect) => {
                        sprite.rect = Some(*rect);
                    }
                    Atlas::Layout(layout) => {
                        let handle = atlas_registry
                            .entry((layout.size, layout.textures.clone()))
                            .or_insert_with(|| layouts.add(layout.clone()));
                        sprite.texture_atlas = Some(TextureAtlas {
                            index: 0,
                            layout: handle.clone(),
                        });
                    }
                }

                commands.entity(new_entity).insert(sprite);
            }
        }

        commands.entity(entity).insert(Populated);
    }
}

pub fn update_dyn_entities(
    mut commands: Commands,
    mut reader: EventReader<AssetEvent<EntityInstances>>,
    handle_query: Query<Entity, With<DynLevelEntities>>,
) {
    for event in reader.read() {
        // sometimes a dyn entities doesn't update so just flushing all of them in any change.
        if let AssetEvent::Modified { .. } = event {
            for entity in handle_query.iter() {
                commands
                    .entity(entity)
                    .remove::<Populated>()
                    .despawn_related::<Children>();
            }
        }
    }
}
