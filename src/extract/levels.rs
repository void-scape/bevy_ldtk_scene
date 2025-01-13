use super::world::{ExtractedComponent, IntoExtractedComponent};
use crate::world::LevelUid;
use bevy::prelude::*;
use bevy::utils::HashMap;
use convert_case::{Case, Casing};
use quote::{format_ident, quote, TokenStreamExt};

#[derive(Debug, Clone, Copy)]
pub struct LevelMeta {
    pub uid: LevelUid,
    pub size: Vec2,
    pub world_position: Vec2,
}

impl LevelMeta {
    pub fn new<M: LevelMetaExt>() -> Self {
        Self {
            uid: M::uid(),
            size: M::size(),
            world_position: M::world_position(),
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
    fn uid() -> LevelUid;

    /// Pixel dimensions of the level in the LDtk world.
    fn size() -> Vec2;

    /// Pixel position in the LDtk world.
    fn world_position() -> Vec2;

    fn meta() -> LevelMeta
    where
        Self: Sized,
    {
        LevelMeta::new::<Self>()
    }

    /// The [`Rect`] which this level occupies in the bevy world given `transform`.
    fn rect(transform: &GlobalTransform) -> Rect {
        let position = transform.translation().xy();
        if transform.scale() != Vec3::ONE {
            todo!("world rect scaled");
        }

        Rect::from_corners(position, position + Self::size())
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
        ExtractedLevelUids(
            world
                .levels()
                .map(|l| (LevelUid(l.uid), l.clone()))
                .collect(),
        )
    }
}

pub struct ExtractedLevelUids(HashMap<LevelUid, ldtk2::Level>);

impl ExtractedComponent for ExtractedLevelUids {
    fn global(&self, output: &mut proc_macro2::TokenStream) {
        for (uid, level) in self.0.iter() {
            let name = format_ident!("{}Level", level.identifier.to_case(Case::Pascal));
            let uid = uid.0;

            let size_x = level.px_wid as f32;
            let size_y = level.px_hei as f32;

            let world_x = level.world_x as f32;
            let world_y = level.world_y as f32;

            output.append_all(quote! {
                pub struct #name;
                impl ::bevy_ldtk_scene::extract::levels::LevelMetaExt for #name {
                    fn uid() -> ::bevy_ldtk_scene::world::LevelUid {
                        ::bevy_ldtk_scene::world::LevelUid(#uid)
                    }
                    fn size() -> ::bevy::prelude::Vec2 {
                        ::bevy::prelude::Vec2::new(#size_x, #size_y)
                    }
                    fn world_position() -> ::bevy::prelude::Vec2 {
                        ::bevy::prelude::Vec2::new(#world_x, #world_y)
                    }
                }
            });
        }
    }
}
