use crate::*;
use bevy::utils::hashbrown::HashMap;
use convert_case::{Case, Casing};
use extract::{
    enums::{EnumDefinition, EnumRegistry},
    tiles::{TileSetRegistry, TileSetUid},
    world::{ExtractedComponent, FromLdtkWorld, IntoExtractedComponent},
};
use ldtk2::{serde_json::Value, FieldInstance};
use quote::{format_ident, quote, TokenStreamExt};
use world::{ExtractLdtkWorld, LevelUid};

pub trait DynLdtkEntity: 'static + Send + Sync + Debug {
    fn insert(&self, field: &[ldtk2::FieldInstance], entity: &mut EntityCommands);
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
pub struct EntityUid(pub i64);

#[derive(Debug, Clone, Component, serde::Serialize, serde::Deserialize)]
pub struct LdtkEntityInstance {
    pub uid: EntityUid,
    pub xyz: Vec3,
    pub sprite: Option<LdtkEntitySprite>,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct LdtkEntitySprite {
    pub image: String,
    pub tileset: TileSetUid,
    pub atlas: Atlas,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub enum Atlas {
    Rect(Rect),
    Layout(TextureAtlasLayout),
}

impl Atlas {
    pub fn rect(&self) -> Option<&Rect> {
        match self {
            Self::Rect(rect) => Some(rect),
            _ => None,
        }
    }

    pub fn atlas(&self) -> Option<&TextureAtlasLayout> {
        match self {
            Self::Layout(atlas) => Some(atlas),
            _ => None,
        }
    }
}

#[derive(
    Debug, Default, Clone, PartialEq, Eq, Hash, Component, serde::Deserialize, serde::Serialize,
)]
pub struct EntityInstanceUid(pub String);

pub struct DynCompLdtkEntityInstance {
    instance: LdtkEntityInstance,
    entity: proc_macro2::TokenStream,
}

pub struct ExtractEntityInstances;

impl IntoExtractedComponent<ExtractedEntityInstances> for ExtractEntityInstances {
    type Context = TileSetRegistry;

    fn extract(
        self,
        world: &ExtractLdtkWorld,
        tileset_registry: Self::Context,
    ) -> ExtractedEntityInstances {
        let mut instances = HashMap::<LevelUid, Vec<DynCompLdtkEntityInstance>>::default();
        let types = world.extract_component(ExtractEntityTypes);
        let enums = EnumRegistry::from_world(world);

        for (level, _, entities, z) in world.entities() {
            for entity in entities {
                let entry = instances.entry(LevelUid(level.uid)).or_default();
                entry.push(DynCompLdtkEntityInstance {
                    instance: LdtkEntityInstance {
                        uid: EntityUid(entity.def_uid),
                        xyz: Vec3::new(entity.px[0] as f32, -entity.px[1] as f32, z),
                        sprite: entity.tile.as_ref().map(|t| {
                            let tileset = tileset_registry.tileset(TileSetUid(t.tileset_uid));
                            LdtkEntitySprite {
                                tileset: TileSetUid(t.tileset_uid),
                                image: tileset.asset_image.clone(),
                                atlas: entity
                                    .tags
                                    .iter()
                                    .any(|t| t == "atlas")
                                    .then(|| {
                                        Atlas::Layout(TextureAtlasLayout::from_grid(
                                            UVec2::splat(tileset.tile_size),
                                            tileset.width,
                                            tileset.height,
                                            Some(UVec2::splat(tileset.spacing)),
                                            Some(UVec2::splat(tileset.padding)),
                                        ))
                                    })
                                    .unwrap_or(Atlas::Rect(Rect::new(
                                        t.x as f32,
                                        t.y as f32,
                                        (t.x + t.w) as f32,
                                        (t.y + t.h) as f32,
                                    ))),
                            }
                        }),
                    },
                    entity: {
                        let ty = types.uids.get(&EntityUid(entity.def_uid)).unwrap();
                        match ty {
                            EntityType::Marker(name) => {
                                quote! { #name }
                            }
                            EntityType::Struct { name, fields } => {
                                let fields = fields.iter().map(|field| {
                                    let field = entity
                                        .field_instances
                                        .iter()
                                        .find(|i| i.identifier == *field.0)
                                        .expect("invalid entity field");
                                    parse_entity_field_value(field, &enums)
                                        .expect("cannot parse field value")
                                });

                                quote! {
                                    #name {
                                        #(#fields),*
                                    }
                                }
                            }
                        }
                    },
                });
            }
        }

        ExtractedEntityInstances { instances }
    }
}

pub struct ExtractedEntityInstances {
    instances: HashMap<LevelUid, Vec<DynCompLdtkEntityInstance>>,
}

impl ExtractedComponent for ExtractedEntityInstances {
    fn io(&self, io: &mut world::WorldIO) -> world::IOResult {
        for (uid, data) in self.instances.iter() {
            io.save_entities(
                *uid,
                &data.iter().map(|e| e.instance.clone()).collect::<Vec<_>>(),
            )?;
        }

        Ok(())
    }
}

pub struct CompLdtkEntityInstance {
    instance: LdtkEntityInstance,
    entity: proc_macro2::TokenStream,
}

pub struct ExtractCompEntities;

impl IntoExtractedComponent<ExtractedCompEntities> for ExtractCompEntities {
    type Context = TileSetRegistry;

    fn extract(
        self,
        world: &ExtractLdtkWorld,
        tileset_registry: Self::Context,
    ) -> ExtractedCompEntities {
        let mut instances = HashMap::<LevelUid, Vec<CompLdtkEntityInstance>>::default();
        let types = world.extract_component(ExtractEntityTypes);
        let enums = EnumRegistry::from_world(world);

        for (level, _, entities, z) in world.entities() {
            for entity in entities {
                let entry = instances.entry(LevelUid(level.uid)).or_default();
                entry.push(CompLdtkEntityInstance {
                    instance: LdtkEntityInstance {
                        uid: EntityUid(entity.def_uid),
                        xyz: Vec3::new(entity.px[0] as f32, -entity.px[1] as f32, z),
                        sprite: entity.tile.as_ref().map(|t| {
                            let tileset = tileset_registry.tileset(TileSetUid(t.tileset_uid));
                            LdtkEntitySprite {
                                tileset: TileSetUid(t.tileset_uid),
                                image: tileset.asset_image.clone(),
                                atlas: entity
                                    .tags
                                    .iter()
                                    .any(|t| t == "atlas")
                                    .then(|| {
                                        Atlas::Layout(TextureAtlasLayout::from_grid(
                                            UVec2::splat(tileset.tile_size),
                                            tileset.width,
                                            tileset.height,
                                            Some(UVec2::splat(tileset.spacing)),
                                            Some(UVec2::splat(tileset.padding)),
                                        ))
                                    })
                                    .unwrap_or(Atlas::Rect(Rect::new(
                                        t.x as f32,
                                        t.y as f32,
                                        (t.x + t.w) as f32,
                                        (t.y + t.h) as f32,
                                    ))),
                            }
                        }),
                    },
                    entity: {
                        let ty = types.uids.get(&EntityUid(entity.def_uid)).unwrap();
                        match ty {
                            EntityType::Marker(name) => {
                                quote! { #name }
                            }
                            EntityType::Struct { name, fields } => {
                                let fields = fields.iter().map(|field| {
                                    let field = entity
                                        .field_instances
                                        .iter()
                                        .find(|i| i.identifier == *field.0)
                                        .expect("invalid entity field");
                                    parse_entity_field_value(field, &enums)
                                        .expect("cannot parse field value")
                                });

                                quote! {
                                    #name {
                                        #(#fields),*
                                    }
                                }
                            }
                        }
                    },
                });
            }
        }

        ExtractedCompEntities { instances }
    }
}

pub struct ExtractedCompEntities {
    instances: HashMap<LevelUid, Vec<CompLdtkEntityInstance>>,
}

impl ExtractedComponent for ExtractedCompEntities {
    fn global(&self, output: &mut proc_macro2::TokenStream) {
        for (level, level_entities) in self.instances.iter() {
            let mut layouts = HashMap::<String, syn::Ident>::default();
            let mut layout_output = proc_macro2::TokenStream::new();
            let mut entities = Vec::new();

            for entity in level_entities.iter() {
                let entity_tokens = &entity.entity;
                let entity = &entity.instance;

                let x = entity.xyz.x;
                let y = entity.xyz.y;
                let z = entity.xyz.z;

                let sprite = entity
                    .sprite
                    .as_ref()
                    .map(|s| {
                        let path = &s.image;
                        let rect = s
                            .atlas
                            .rect()
                            .map(|rect| {
                                let x0 = rect.min.x;
                                let y0 = rect.min.y;
                                let x1 = rect.max.x;
                                let y1 = rect.max.y;
                                quote! { Some(::bevy::prelude::Rect::new(#x0, #y0, #x1, #y1)) }
                            })
                            .unwrap_or_else(|| quote! { None });
                        let atlas = if let Some(atlas) = s.atlas.atlas() {
                            let size_x = atlas.size.x;
                            let size_y = atlas.size.y;
                            let textures = atlas.textures.iter().map(|v| {
                                let min_x = v.min.x;
                                let min_y = v.min.y;
                                let max_x = v.max.x;
                                let max_y = v.max.y;

                                quote! {
                                    ::bevy::prelude::URect {
                                        min: ::bevy::prelude::UVec2::new(#min_x, #min_y),
                                        max: ::bevy::prelude::UVec2::new(#max_x, #max_y),
                                    }
                                }
                            });

                            let len = layouts.len();
                            let layout = layouts.entry(path.clone()).or_insert_with(|| {
                                let ident = format_ident!("layout_{}", len);
                                layout_output.append_all(quote! {
                                    let #ident = atlases.add(::bevy::prelude::TextureAtlasLayout {
                                        size: bevy::prelude::UVec2::new(#size_x, #size_y),
                                        textures: vec![#(#textures),*],
                                    });
                                });

                                ident
                            });

                            quote! {
                                Some(::bevy::prelude::TextureAtlas {
                                    index: 0,
                                    layout: #layout.clone(),
                                })
                            }
                        } else {
                            quote! {None}
                        };

                        quote! {
                            ::bevy::prelude::Sprite {
                                image: server.load(#path),
                                rect: #rect,
                                texture_atlas: #atlas,
                                anchor: ::bevy::sprite::Anchor::TopLeft,
                                ..Default::default()
                            },
                        }
                    })
                    .unwrap_or_default();

                entities.push(quote! {
                    commands.spawn(
                        (
                            ::bevy_ldtk_scene::process::entities::LevelEntity,
                            ::bevy::prelude::Transform::from_xyz(#x, #y, #z),
                            #sprite
                            #entity_tokens
                        )
                    );
                });
            }

            let func = format_ident!("entities_{}", level.0.to_string());
            output.append_all(quote! {
                fn #func (mut commands: ::bevy::prelude::Commands, server: ::bevy::prelude::Res<::bevy::prelude::AssetServer>, mut atlases: ::bevy::prelude::ResMut<::bevy::prelude::Assets<::bevy::prelude::TextureAtlasLayout>>) {
                    #layout_output
                    #(#entities)*
                }
            });
        }
    }

    fn plugin(&self, output: &mut proc_macro2::TokenStream) {
        for (level, _) in self.instances.iter() {
            let func = format_ident!("entities_{}", level.0.to_string());
            let uid = level.0;

            output.append_all(quote! {
                let system = app.register_system(#func);
                let mut registry = app.world_mut().get_resource_or_insert_with(|| ::bevy_ldtk_scene::process::entities::LevelEntityRegistry::default());
                registry.entities.insert(::bevy_ldtk_scene::world::LevelUid(#uid), system);
            });
        }
    }

    // fn io(&self, io: &mut world::WorldIO) -> world::IOResult {
    //     for (uid, entities) in &self.0 {
    //         io.save_entities(*uid, entities)?;
    //     }
    //
    //     Ok(())
    // }
}

pub struct ExtractEntityTypes;

impl IntoExtractedComponent<ExtractedEntityTypes> for ExtractEntityTypes {
    type Context = EnumRegistry;

    fn extract(self, world: &ExtractLdtkWorld, context: Self::Context) -> ExtractedEntityTypes {
        let mut names = HashMap::default();
        let mut uids = HashMap::default();

        for entity in world.ldtk().defs.entities.iter() {
            let name = format_ident!("{}", entity.identifier.to_case(Case::Pascal));
            let fields: HashMap<String, syn::Ident> = entity
                .field_defs
                .iter()
                .map(|f| (f.identifier.clone(), parse_entity_field_type(f, &context)))
                .collect();

            if fields.is_empty() {
                let ty = EntityType::Marker(name);
                names.insert(entity.identifier.clone(), ty.clone());
                uids.insert(EntityUid(entity.uid), ty);
            } else {
                let ty = EntityType::Struct { name, fields };
                names.insert(entity.identifier.clone(), ty.clone());
                uids.insert(EntityUid(entity.uid), ty);
            }
        }

        ExtractedEntityTypes { names, uids }
    }
}

pub struct ExtractedEntityTypes {
    names: HashMap<String, EntityType>,
    uids: HashMap<EntityUid, EntityType>,
}

impl ExtractedComponent for ExtractedEntityTypes {
    fn global(&self, output: &mut proc_macro2::TokenStream) {
        for entity in self.names.values() {
            let decl = match entity {
                EntityType::Marker(name) => {
                    quote! {
                        pub struct #name;
                    }
                }
                EntityType::Struct { name, fields } => {
                    let fields = fields.iter().map(|(name, ty)| {
                        let name = format_ident!("{}", name.to_case(Case::Snake));
                        quote! { pub #name: #ty }
                    });

                    quote! {
                        pub struct #name {
                            #(#fields,)*
                        }
                    }
                }
            };

            let insert_fields = match entity {
                EntityType::Marker(_) => vec![],
                EntityType::Struct { fields, .. } => fields
                    .iter()
                    .map(|(name, _)| {
                        let name_ident = format_ident!("{}", name.to_case(Case::Snake));
                        quote! { slf.#name_ident = ::bevy_ldtk_scene::extract::entities::FromField::from_field(fields.iter().find(|field| field.identifier == #name).expect("invalid field")); }
                    })
                    .collect(),
            };

            let name = entity.name();
            let entity_mod = format_ident!("{}", name.to_string().to_case(Case::Snake));
            output.append_all(quote! {
                mod #entity_mod {
                    use ::bevy::prelude::ReflectComponent;

                    #[derive(Debug, Default, Clone, PartialEq)]
                    #[derive(::bevy::ecs::component::Component, ::bevy::reflect::Reflect)]
                    #[derive(serde::Serialize, serde::Deserialize)]
                    #[reflect(Component)]
                    #decl
                }
                #[allow(unused)]
                pub use #entity_mod :: #name;
                impl ::bevy_ldtk_scene::extract::entities::DynLdtkEntity for #entity_mod :: #name {
                    fn insert(&self, fields: &[::bevy_ldtk_scene::ldtk2::FieldInstance], entity: &mut ::bevy::prelude::EntityCommands) {
                        let mut slf = self.clone();
                        #(#insert_fields)*
                        entity.insert(slf);
                    }
                }
            });
        }
    }

    fn plugin(&self, output: &mut proc_macro2::TokenStream) {
        for (uid, entity) in self.uids.iter() {
            let uid = uid.0;
            let name = entity.name();
            output.append_all(quote! {
                app.register_type::<#name>();
                let mut registry = app.world_mut().get_resource_or_insert_with(|| ::bevy_ldtk_scene::process::entities::LevelDynEntityRegistry::default());
                registry.entities.insert(::bevy_ldtk_scene::extract::entities::EntityUid(#uid), Box::new(#name::default()));
            });
        }
    }
}

#[derive(Clone)]
pub enum EntityType {
    Marker(syn::Ident),
    Struct {
        name: syn::Ident,
        fields: HashMap<String, syn::Ident>,
    },
}

impl EntityType {
    pub fn name(&self) -> &syn::Ident {
        match self {
            EntityType::Marker(name) => name,
            EntityType::Struct { name, .. } => name,
        }
    }
}

pub struct ExtractEntityFields;

impl IntoExtractedComponent<ExtractedEntityFields> for ExtractEntityFields {
    type Context = ();

    fn extract(self, world: &ExtractLdtkWorld, _: Self::Context) -> ExtractedEntityFields {
        ExtractedEntityFields(
            world
                .entities()
                .flat_map(|(_, _, entities, _)| {
                    entities
                        .map(|entity| (EntityUid(entity.def_uid), entity.field_instances.clone()))
                })
                .collect(),
        )
    }
}

pub struct ExtractedEntityFields(pub HashMap<EntityUid, Vec<FieldInstance>>);

impl ExtractedComponent for ExtractedEntityFields {}

fn parse_entity_field_type(
    field: &ldtk2::FieldDefinition,
    enum_registry: &EnumRegistry,
) -> syn::Ident {
    if field.field_definition_type.contains("LocalEnum") {
        match enum_registry.definition(
            field
                .field_definition_type
                .strip_prefix("LocalEnum.")
                .unwrap(),
        ) {
            EnumDefinition::Enum { ty, .. } => return ty.clone(),
            EnumDefinition::Marker(_) => panic!("an entity cannot have a marker enum"),
        }
    }

    match &*field.field_definition_type {
        "Float" => {
            format_ident!("f32")
        }
        _ => todo!("implement more data types"),
    }
}

fn parse_entity_field_value(
    field: &ldtk2::FieldInstance,
    enum_registry: &EnumRegistry,
) -> Option<proc_macro2::TokenStream> {
    let name = format_ident!("{}", field.identifier.to_case(Case::Snake));

    if field.field_instance_type.contains("LocalEnum") {
        match enum_registry.definition(
            field
                .field_instance_type
                .strip_prefix("LocalEnum.")
                .unwrap(),
        ) {
            EnumDefinition::Enum { ty, .. } => {
                let value = field
                    .value
                    .as_ref()
                    .expect("expected field instance to have a value");
                let variant = match value {
                    Value::String(str) => {
                        format_ident!("{}", str.to_case(Case::Pascal))
                    }
                    _ => {
                        panic!("expected string value for enum");
                    }
                };

                return Some(quote! { #name: #ty :: #variant });
            }
            EnumDefinition::Marker(_) => panic!("an entity cannot have a marker enum"),
        }
    }

    match &*field.field_instance_type {
        "Float" => {
            let Some(Value::Number(number)) = field.value.as_ref() else {
                panic!("Number didn't match expected shape");
            };
            let inner = number
                .as_f64()
                .expect("Numbers should be representable as floats") as f32;

            Some(quote! {
                #name: #inner
            })
        }
        _ => todo!("implement more data types"),
    }
}

pub trait FromField {
    fn from_field(field: &ldtk2::FieldInstance) -> Self;
}

impl FromField for f32 {
    fn from_field(field: &ldtk2::FieldInstance) -> Self {
        field.value.as_ref().expect("f32").as_f64().expect("f32") as f32
    }
}
