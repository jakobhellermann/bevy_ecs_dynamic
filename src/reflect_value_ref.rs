//! Types for referencing ecs values (like components and resources) in a way
//! that doesn't deal with raw pointers and doesn't have any lifetime issues.

use bevy_ecs::component::ComponentId;
use bevy_ecs::prelude::*;
use bevy_ecs::ptr::Ptr;
use bevy_ecs::{change_detection::MutUntyped, world::WorldId};
use bevy_reflect::{GetPath, Reflect, ReflectFromPtr, TypeRegistry};

#[derive(Debug, Clone, Copy)]
pub enum WorldValueBase {
    Component(Entity, ComponentId),
    Resource(ComponentId),
}

#[allow(clippy::needless_lifetimes)] // i'd rather be explicit around world references
impl WorldValueBase {
    pub fn access<'w>(self, world: &'w World) -> Option<Ptr<'w>> {
        match self {
            WorldValueBase::Component(entity, component_id) => {
                world.get_by_id(entity, component_id)
            }
            WorldValueBase::Resource(component_id) => world.get_resource_by_id(component_id),
        }
    }

    pub fn access_mut<'w>(self, world: &'w mut World) -> Option<MutUntyped<'w>> {
        match self {
            WorldValueBase::Component(entity, component_id) => {
                world.get_mut_by_id(entity, component_id)
            }
            WorldValueBase::Resource(component_id) => world.get_resource_mut_by_id(component_id),
        }
    }

    pub fn component_id(self) -> ComponentId {
        match self {
            WorldValueBase::Component(_, id) => id,
            WorldValueBase::Resource(id) => id,
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

impl std::fmt::Debug for ReflectValueRef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ReflectValueRef")
            .field("world_id", &self.world_id)
            .field("base", &self.base)
            .field("path", &self.path)
            .finish()
    }
}

#[derive(Debug)]
pub enum ReflectValueRefError {
    InexistentComponent(ComponentId),
    TypeRegistryNotInWorld,
    NoTypeId(ComponentId),
    NoReflectFromPtr(ComponentId),
    InvalidBaseValue(WorldValueBase),
    InvalidPath(String),
}

impl std::error::Error for ReflectValueRefError {}
impl std::fmt::Display for ReflectValueRefError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ReflectValueRefError::InexistentComponent(id) => {
                write!(f, "Component {id:?} does not exist")
            }
            ReflectValueRefError::TypeRegistryNotInWorld => {
                write!(f, "no `TypeRegistry` exists in the world")
            }
            ReflectValueRefError::NoTypeId(id) => write!(
                f,
                "Component {id:?} has no associated `TypeId` and can't be reflected"
            ),
            ReflectValueRefError::NoReflectFromPtr(id) => write!(
                f,
                "Component {id:?} has no registered `ReflectFromPtr` type data"
            ),
            ReflectValueRefError::InvalidBaseValue(WorldValueBase::Component(entity, id)) => {
                write!(
                    f,
                    "The world has no component {id:?} at the entity {entity:?}"
                )
            }
            ReflectValueRefError::InvalidBaseValue(WorldValueBase::Resource(id)) => {
                write!(f, "The world has no resource {id:?}")
            }
            ReflectValueRefError::InvalidPath(msg) => write!(f, "invalid path: {msg}"),
        }
    }
}

fn get_reflect_from_ptr(
    world: &World,
    component_id: ComponentId,
) -> Result<ReflectFromPtr, ReflectValueRefError> {
    let info = world
        .components()
        .get_info(component_id)
        .ok_or_else(|| ReflectValueRefError::InexistentComponent(component_id))?;
    let type_id = info
        .type_id()
        .ok_or_else(|| ReflectValueRefError::NoTypeId(component_id))?;

    let type_registry = world
        .get_resource::<TypeRegistry>()
        .ok_or(ReflectValueRefError::TypeRegistryNotInWorld)?;
    let reflect_from_ptr = type_registry
        .get_type_data::<ReflectFromPtr>(type_id)
        .ok_or_else(|| ReflectValueRefError::NoReflectFromPtr(component_id))?
        .clone();
    assert_eq!(reflect_from_ptr.type_id(), type_id);

    Ok(reflect_from_ptr)
}

impl ReflectValueRef {
    pub fn component(
        world: &World,
        entity: Entity,
        component_id: ComponentId,
    ) -> Result<ReflectValueRef, ReflectValueRefError> {
        let reflect_from_ptr = get_reflect_from_ptr(world, component_id)?;
        let base = WorldValueBase::Component(entity, component_id);

        Ok(ReflectValueRef {
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

    pub fn resource(
        world: &World,
        component_id: ComponentId,
    ) -> Result<ReflectValueRef, ReflectValueRefError> {
        let reflect_from_ptr = get_reflect_from_ptr(world, component_id)?;
        let base = WorldValueBase::Resource(component_id);

        Ok(ReflectValueRef {
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

    pub fn component_id(&self) -> ComponentId {
        self.base.component_id()
    }

    pub fn get<'w>(&self, world: &'w World) -> Result<&'w dyn Reflect, ReflectValueRefError> {
        assert_eq!(self.world_id, world.id());

        let ptr = self
            .base
            .access(world)
            .ok_or_else(|| ReflectValueRefError::InvalidBaseValue(self.base))?;
        // SAFETY:
        // `val` is a pointer to value of the type that the `ReflectFromPtr` was constructed for,
        // because the mapping from `ComponentId -> TypeId` is immutable and `ReflectFromPtr` is checked to be
        // for the type of the `WorldBase`'s type id.
        let reflect = unsafe { self.reflect_from_ptr.as_reflect_ptr(ptr) };

        let reflect = reflect
            .path(&self.path)
            .map_err(|e| ReflectValueRefError::InvalidPath(e.to_string()))?;

        Ok(reflect)
    }

    pub fn get_mut<'w>(
        &self,
        world: &'w mut World,
    ) -> Result<&'w mut dyn Reflect, ReflectValueRefError> {
        assert_eq!(self.world_id, world.id());

        let mut ptr = self
            .base
            .access_mut(world)
            .ok_or_else(|| ReflectValueRefError::InvalidBaseValue(self.base))?;

        // TODO: don't mark as changed on every access
        ptr.set_changed();

        // SAFETY:
        // `val` is a pointer to value of the type that the `ReflectFromPtr` was constructed for,
        // because the mapping from `ComponentId -> TypeId` is immutable and `ReflectFromPtr` is checked to be
        // for the type of the `WorldBase`'s type id.
        let reflect = unsafe { self.reflect_from_ptr.as_reflect_ptr_mut(ptr.into_inner()) };

        let reflect = reflect
            .path_mut(&self.path)
            .map_err(|e| ReflectValueRefError::InvalidPath(e.to_string()))?;

        Ok(reflect)
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

use crate::dynamic_query::{DynamicQuery, DynamicQueryIter, FilterKind};

pub struct ReflectValueRefQuery {
    query: DynamicQuery,
}

impl ReflectValueRefQuery {
    pub fn new(world: &World, components: &[ComponentId]) -> Self {
        Self {
            query: DynamicQuery::new(
                world,
                vec![],
                components.iter().map(|&id| FilterKind::With(id)).collect(),
            )
            .expect("dynamic query construction cannot fail because only filters are supplied"),
        }
    }

    pub fn iter<'w, 's>(&'s mut self, world: &'w World) -> ReflectValueRefQueryIter<'w, 's> {
        let components: Vec<_> = self
            .query
            .filters()
            .iter()
            .map(|filter| match *filter {
                FilterKind::With(id) => id,
                _ => unreachable!(),
            })
            .map(|component_id| {
                // TODO no unwrap
                let reflect_from_ptr = get_reflect_from_ptr(world, component_id).unwrap();
                (component_id, reflect_from_ptr)
            })
            .collect();

        ReflectValueRefQueryIter {
            world_id: world.id(),
            components,
            iter: self.query.iter(world),
        }
    }
}

pub struct ReflectValueRefQueryIter<'w, 's> {
    world_id: WorldId,
    iter: DynamicQueryIter<'w, 's>,
    components: Vec<(ComponentId, ReflectFromPtr)>,
}

pub struct ReflectValueRefQueryItem {
    pub entity: Entity,
    pub items: Vec<ReflectValueRef>,
}

impl<'w, 's> Iterator for ReflectValueRefQueryIter<'w, 's> {
    type Item = ReflectValueRefQueryItem;

    fn next(&mut self) -> Option<Self::Item> {
        let item = self.iter.next()?;

        let items: Vec<_> = self
            .components
            .iter()
            .map(|(component_id, reflect_from_ptr)| unsafe {
                ReflectValueRef::component_unchecked(
                    item.entity,
                    *component_id,
                    reflect_from_ptr.clone(),
                    self.world_id,
                )
            })
            .collect();

        Some(ReflectValueRefQueryItem {
            entity: item.entity,
            items,
        })
    }
}

#[cfg(test)]
mod tests {
    use bevy_ecs::prelude::*;
    use bevy_reflect::{Reflect, TypeRegistry};

    use super::ReflectValueRefQuery;

    #[derive(Component, Reflect)]

    struct TestComponent1 {
        value: String,
    }

    #[derive(Component, Reflect)]

    struct TestComponent2 {
        field: u8,
    }

    #[test]
    fn iter() {
        let mut world = World::new();

        let component_id_1 = world.init_component::<TestComponent1>();
        let component_id_2 = world.init_component::<TestComponent2>();

        let mut type_registry = world.get_resource_or_insert_with(TypeRegistry::new);
        type_registry.register::<TestComponent1>();
        type_registry.register::<TestComponent2>();

        world.spawn().insert(TestComponent1 { value: "no".into() });
        world.spawn().insert_bundle((
            TestComponent1 {
                value: "yes".into(),
            },
            TestComponent2 { field: 5 },
        ));

        let mut query = ReflectValueRefQuery::new(&world, &[component_id_1, component_id_2]);
        let results: Vec<_> = query.iter(&world).collect();
        assert_eq!(results.len(), 1);

        match results[0].items.as_slice() {
            [a, b] => {
                assert_eq!(a.component_id(), component_id_1);
                assert_eq!(b.component_id(), component_id_2);

                assert_eq!(
                    a.append_path(".value")
                        .get(&world)
                        .unwrap()
                        .downcast_ref::<String>()
                        .unwrap(),
                    "yes"
                )
            }
            _ => unreachable!(),
        }
    }
}
