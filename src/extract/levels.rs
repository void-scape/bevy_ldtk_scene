use super::entities::{parse_field_type, parse_field_value};
use super::enums::EnumRegistry;
use super::world::{ExtractedComponent, FromLdtkWorld, IntoExtractedComponent};
use crate::world::{ExtractLdtkWorld, LevelUid};
use bevy::platform::collections::HashMap;
use bevy::prelude::*;
use convert_case::{Case, Casing};
use quote::{format_ident, quote, TokenStreamExt};

#[derive(Debug, Clone, Copy)]
pub struct LevelMeta {
    pub uid: LevelUid,
    pub size: Vec2,
    pub world_position: Vec2,
}

impl LevelMeta {
    pub fn new<L: LevelMetaExt>(level: &L) -> Self {
        Self {
            uid: level.uid(),
            size: level.size(),
            world_position: level.world_position(),
        }
    }

    pub fn rect(&self, transform: &GlobalTransform) -> Rect {
        self.rect_with_border(transform, Vec2::ZERO)
    }

    pub fn rect_with_border(&self, transform: &GlobalTransform, border: Vec2) -> Rect {
        let position = transform.translation().xy();
        if transform.scale() != Vec3::ONE {
            todo!("world rect scaled");
        }

        Rect::from_corners(
            position - border,
            position + border + Vec2::new(self.size.x, -self.size.y),
        )
    }
}

pub trait LevelMetaExt {
    fn uid(&self) -> LevelUid;

    /// Pixel dimensions of the level in the LDtk world.
    fn size(&self) -> Vec2;

    /// Pixel position in the LDtk world.
    fn world_position(&self) -> Vec2;

    fn meta(&self) -> LevelMeta
    where
        Self: Sized,
    {
        LevelMeta::new(self)
    }

    /// The [`Rect`] which this level occupies in the bevy world given `transform`.
    fn rect(&self, transform: &GlobalTransform) -> Rect {
        let position = transform.translation().xy();
        if transform.scale() != Vec3::ONE {
            todo!("world rect scaled");
        }

        Rect::from_corners(position, position + self.size())
    }
}

pub struct LevelNames(pub Vec<syn::Ident>);

impl FromLdtkWorld for LevelNames {
    fn from_world(world: &ExtractLdtkWorld) -> Self {
        Self(
            world
                .levels()
                .map(|l| format_ident!("{}", l.identifier.to_case(Case::Pascal)))
                .collect(),
        )
    }
}

pub struct ExtractLevelUids;

impl IntoExtractedComponent<ExtractedLevelUids> for ExtractLevelUids {
    type Context = ();

    fn extract(
        self,
        world: &crate::world::ExtractLdtkWorld,
        _: Self::Context,
    ) -> ExtractedLevelUids {
        let enums = EnumRegistry::from_world(world);
        let field_defs = world
            .ldtk()
            .defs
            .level_fields
            .iter()
            .map(|f| {
                let field = parse_field_type(f, &enums);
                let name = format_ident!("{}", f.identifier.to_case(Case::Snake));
                (f.identifier.clone(), quote! { #name: #field })
            })
            .collect();

        let field_insts = world
            .levels()
            .map(|level| {
                (
                    level.identifier.clone(),
                    level
                        .field_instances
                        .iter()
                        .map(|field| {
                            let can_be_null = world
                                .ldtk()
                                .defs
                                .level_fields
                                .iter()
                                .find(|d| d.uid == field.def_uid)
                                .map(|d| d.can_be_null)
                                .expect("invalid level def");
                            parse_field_value(can_be_null, field, &enums)
                                .expect("cannot parse field value")
                        })
                        .collect(),
                )
            })
            .collect();

        ExtractedLevelUids(
            world
                .levels()
                .map(|l| (LevelUid(l.uid), l.clone()))
                .collect(),
            field_defs,
            field_insts,
        )
    }
}

pub struct ExtractedLevelUids(
    HashMap<LevelUid, ldtk2::Level>,
    HashMap<String, proc_macro2::TokenStream>,
    HashMap<String, Vec<proc_macro2::TokenStream>>,
);

impl ExtractedComponent for ExtractedLevelUids {
    fn global(&self, output: &mut proc_macro2::TokenStream) {
        let field_defs = self.1.values();

        output.append_all(quote! {
            /// User Level data specified in `Custom Fields`.
            #[derive(Debug, Clone)]
            pub struct LevelFields {
                #(#field_defs,)*
            }

            pub trait QueryLevelFields {
                fn fields(&self) -> LevelFields;
            }

            pub trait LevelExt: ::bevy_ldtk_scene::prelude::LevelMetaExt + QueryLevelFields + ::bevy_ldtk_scene::levels::LevelSet + Send + Sync + 'static {}

            impl<T> LevelExt for T where T: ::bevy_ldtk_scene::prelude::LevelMetaExt + QueryLevelFields + ::bevy_ldtk_scene::levels::LevelSet + Send + Sync + 'static {}
        });

        for (uid, level) in self.0.iter() {
            let name = format_ident!("{}", level.identifier.to_case(Case::Pascal));
            let uid = uid.0;

            let size_x = level.px_wid as f32;
            let size_y = level.px_hei as f32;

            let world_x = level.world_x as f32;
            let world_y = level.world_y as f32;

            let fields = self.2.get(&level.identifier).unwrap();
            output.append_all(quote! {
                #[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash)]
                pub struct #name;
                impl ::bevy_ldtk_scene::extract::levels::LevelMetaExt for #name {
                    fn uid(&self) -> ::bevy_ldtk_scene::world::LevelUid {
                        ::bevy_ldtk_scene::world::LevelUid(#uid)
                    }
                    fn size(&self) -> ::bevy::prelude::Vec2 {
                        ::bevy::prelude::Vec2::new(#size_x, #size_y)
                    }
                    fn world_position(&self) -> ::bevy::prelude::Vec2 {
                        ::bevy::prelude::Vec2::new(#world_x, #world_y)
                    }
                }

                impl QueryLevelFields for #name {
                    fn fields(&self) -> LevelFields {
                        LevelFields {
                            #(#fields,)*
                        }
                    }
                }
            });
        }
    }
}
