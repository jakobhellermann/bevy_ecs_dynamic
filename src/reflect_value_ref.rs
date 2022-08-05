//! Types for referencing ecs values (like components and resources) in a way
//! that doesn't deal with raw pointers and doesn't have any lifetime issues.

use bevy_ecs::component::ComponentId;
use bevy_ecs::prelude::*;
use bevy_ecs::ptr::Ptr;
use bevy_ecs::{change_detection::MutUntyped, world::WorldId};
use bevy_reflect::{GetPath, Reflect, ReflectFromPtr, TypeRegistry};

#[derive(Clone, Copy)]
enum WorldValueBase {
    Component(Entity, ComponentId),
    Resource(ComponentId),
}

#[allow(clippy::needless_lifetimes)] // i'd rather be explicit around world references
impl WorldValueBase {
    fn access<'w>(self, world: &'w World) -> Option<Ptr<'w>> {
        match self {
            WorldValueBase::Component(entity, component_id) => {
                world.get_by_id(entity, component_id)
            }
            WorldValueBase::Resource(component_id) => world.get_resource_by_id(component_id),
        }
    }

    fn access_mut<'w>(self, world: &'w mut World) -> Option<MutUntyped<'w>> {
        match self {
            WorldValueBase::Component(entity, component_id) => {
                world.get_mut_by_id(entity, component_id)
            }
            WorldValueBase::Resource(component_id) => world.get_resource_mut_by_id(component_id),
        }
    }
}

pub struct ReflectValueRef {
    base: WorldValueBase,
    world_id: WorldId,
    // must be for the type referenced by the `WorldBase` component id
    reflect_from_ptr: ReflectFromPtr,
    path: String,
}

fn get_reflect_from_ptr(world: &World, component_id: ComponentId) -> Option<ReflectFromPtr> {
    let info = world.components().get_info(component_id)?;
    let type_id = info.type_id()?;

    let type_registry = world.get_resource::<TypeRegistry>()?;
    let reflect_from_ptr = type_registry
        .get_type_data::<ReflectFromPtr>(type_id)?
        .clone();
    assert_eq!(reflect_from_ptr.type_id(), type_id);

    Some(reflect_from_ptr)
}

impl ReflectValueRef {
    pub fn component(
        world: &World,
        entity: Entity,
        component_id: ComponentId,
    ) -> Option<ReflectValueRef> {
        let reflect_from_ptr = get_reflect_from_ptr(world, component_id)?;
        let base = WorldValueBase::Component(entity, component_id);

        Some(ReflectValueRef {
            base,
            world_id: world.id(),
            reflect_from_ptr,
            path: String::new(),
        })
    }

    /// # Safety
    /// The `ComponentId` parameter must resolve to a component in the world with `world_id`
    /// that has the same type as the one `ReflectFromPtr` was created for
    pub unsafe fn component_unchecked(
        entity: Entity,
        component_id: ComponentId,
        reflect_from_ptr: ReflectFromPtr,
        world_id: WorldId,
    ) -> ReflectValueRef {
        ReflectValueRef {
            base: WorldValueBase::Component(entity, component_id),
            world_id,
            reflect_from_ptr,
            path: String::new(),
        }
    }

    pub fn resource(world: &World, component_id: ComponentId) -> Option<ReflectValueRef> {
        let reflect_from_ptr = get_reflect_from_ptr(world, component_id)?;
        let base = WorldValueBase::Resource(component_id);

        Some(ReflectValueRef {
            base,
            world_id: world.id(),
            reflect_from_ptr,
            path: String::new(),
        })
    }

    /// # Safety
    /// The `ComponentId` parameter must resolve to a resource in the world with `world_id`
    /// that has the same type as the one `ReflectFromPtr` was created for
    pub unsafe fn resource_unchecked(
        component_id: ComponentId,
        reflect_from_ptr: ReflectFromPtr,
        world_id: WorldId,
    ) -> ReflectValueRef {
        ReflectValueRef {
            base: WorldValueBase::Resource(component_id),
            world_id,
            reflect_from_ptr,
            path: String::new(),
        }
    }

    pub fn get<'w>(&self, world: &'w World) -> Option<&'w dyn Reflect> {
        assert_eq!(self.world_id, world.id());

        let ptr = self.base.access(world)?;
        // SAFETY:
        // `val` is a pointer to value of the type that the `ReflectFromPtr` was constructed for,
        // because the mapping from `ComponentId -> TypeId` is immutable and `ReflectFromPtr` is checked to be
        // for the type of the `WorldBase`'s type id.
        let reflect = unsafe { self.reflect_from_ptr.as_reflect_ptr(ptr) };

        let reflect = reflect.path(&self.path).ok()?;

        Some(reflect)
    }

    pub fn get_mut<'w>(&self, world: &'w mut World) -> Option<&'w mut dyn Reflect> {
        assert_eq!(self.world_id, world.id());

        let mut ptr = self.base.access_mut(world)?;

        // TODO: don't mark as changed on every access
        ptr.set_changed();

        // SAFETY:
        // `val` is a pointer to value of the type that the `ReflectFromPtr` was constructed for,
        // because the mapping from `ComponentId -> TypeId` is immutable and `ReflectFromPtr` is checked to be
        // for the type of the `WorldBase`'s type id.
        let reflect = unsafe { self.reflect_from_ptr.as_reflect_ptr_mut(ptr.into_inner()) };

        let reflect = reflect.path_mut(&self.path).ok()?;

        Some(reflect)
    }

    /// Appends a new path to the reflect path offset of the [`ReflectValueRef`]
    /// ```rust,no_run
    /// # let value: bevy_ecs_dynamic::reflect_value_ref::ReflectValueRef = unimplemented!();
    /// value.append_path(".foo");
    /// value.append_path("[3]");
    /// ```
    /// This method will *not* check the validity of the path, and further calls to [`get`](Self::get) or [`get_mut`](Self::get_mut) may fail afterwards.
    pub fn append_path(&self, path: &str) -> Self {
        ReflectValueRef {
            base: self.base,
            world_id: self.world_id,
            reflect_from_ptr: self.reflect_from_ptr.clone(), // the base type doesn't change
            path: format!("{}{}", self.path, path),
        }
    }
}
