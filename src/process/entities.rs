use crate::world::LevelUid;
use bevy::{ecs::system::SystemId, prelude::*, utils::hashbrown::HashMap};

#[derive(Default, Resource)]
pub struct LevelEntityRegistry {
    pub entities: HashMap<LevelUid, SystemId>,
}

/// Root entity of a set of [`LevelEntity`]s.
#[derive(Debug, Clone, Copy, Component)]
pub struct LevelEntities(pub LevelUid);

#[derive(Component)]
pub struct LevelEntity;

pub fn spawn_entities(
    mut commands: Commands,
    entity_query: Query<(Entity, &LevelEntities), Without<Children>>,
    registry: Res<LevelEntityRegistry>,
) {
    for (_, level_entities) in entity_query.iter() {
        // println!("hi");
        // if let Some(id) = registry.entities.get(&level_entities.0) {
        //     println!("hello");
        //     commands.run_system(*id);
        // }
    }
}

pub(crate) fn process_entities(
    mut commands: Commands,
    // server: Res<AssetServer>,
    // entity_query: Query<(Entity, &LdtkEntitySprite)>,
    // mut layouts: ResMut<Assets<TextureAtlasLayout>>,
) {
    // let mut atlas_registry = HashMap::<(UVec2, Vec<URect>), Handle<TextureAtlasLayout>>::default();
    // for (entity, ldtk_entity) in entity_query.iter() {
    //     let mut sprite = Sprite {
    //         image: server.load(&ldtk_entity.image),
    //         ..Default::default()
    //     };
    //
    //     if let Some(atlas) = &ldtk_entity.atlas {
    //         match atlas {
    //             Atlas::Rect(rect) => {
    //                 sprite.rect = Some(*rect);
    //             }
    //             Atlas::Layout(layout) => {
    //                 let handle = atlas_registry
    //                     .entry((layout.size, layout.textures.clone()))
    //                     .or_insert_with(|| layouts.add(layout.clone()));
    //                 sprite.texture_atlas = Some(TextureAtlas {
    //                     index: 0,
    //                     layout: handle.clone(),
    //                 });
    //             }
    //         }
    //     }
    //
    //     commands
    //         .entity(entity)
    //         .insert(sprite)
    //         .remove::<LdtkEntitySprite>();
    // }
}
