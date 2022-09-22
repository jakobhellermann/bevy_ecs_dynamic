//! Types for referencing ecs values (like components and resources) in a way that doesn't deal with raw pointers.

use std::cell::RefCell;
use std::ops::{Deref, DerefMut};
use std::rc::Rc;

use bevy_app::AppTypeRegistry;
use bevy_ecs::component::ComponentId;
use bevy_ecs::prelude::*;
use bevy_ecs::ptr::Ptr;
use bevy_ecs::{change_detection::MutUntyped, world::WorldId};
use bevy_reflect::{GetPath, Reflect, ReflectFromPtr};

pub mod query;

/// Error type enum for errors dealing with [`ReflectValueRef`]s
#[derive(Debug)]
pub enum ReflectValueRefError {
    InexistentComponent(ComponentId),
    TypeRegistryNotInWorld,
    NoTypeId(ComponentId),
    NoReflectFromPtr(ComponentId),
    InvalidBaseValue(EcsBase),
    InvalidPath { error_message: String, path: String },
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
            ReflectValueRefError::InvalidBaseValue(EcsBase::Component(entity, id)) => {
                write!(
                    f,
                    "The world has no component {id:?} at the entity {entity:?}"
                )
            }
            ReflectValueRefError::InvalidBaseValue(EcsBase::Resource(id)) => {
                write!(f, "The world has no resource {id:?}")
            }
            ReflectValueRefError::InvalidPath {
                error_message: msg,
                path,
            } => {
                write!(f, "invalid path: {msg} (\"{path}\")")
            }
        }
    }
}

/// Either a `(Entity, ComponentId`) pair or a Resource-`ComponentId`
#[derive(Debug, Clone, Copy)]
pub enum EcsBase {
    Component(Entity, ComponentId),
    Resource(ComponentId),
}

impl EcsBase {
    pub fn access(self, world: &World) -> Option<Ptr<'_>> {
        match self {
            EcsBase::Component(entity, component_id) => world.get_by_id(entity, component_id),
            EcsBase::Resource(component_id) => world.get_resource_by_id(component_id),
        }
    }

    pub fn access_mut(self, world: &mut World) -> Option<MutUntyped<'_>> {
        match self {
            EcsBase::Component(entity, component_id) => world.get_mut_by_id(entity, component_id),
            EcsBase::Resource(component_id) => world.get_resource_mut_by_id(component_id),
        }
    }

    pub fn component_id(self) -> ComponentId {
        match self {
            EcsBase::Component(_, id) => id,
            EcsBase::Resource(id) => id,
        }
    }
}

/// [`EcsBase`] together with the necessary data for safely accessing its value as [`Reflect`]
#[derive(Clone)]
pub struct EcsValueRef {
    base: EcsBase,
    world_id: WorldId,
    // must be for the type referenced by the `WorldBase` component id
    reflect_from_ptr: ReflectFromPtr,
}

impl std::fmt::Debug for EcsValueRef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ReflectValueRef")
            .field("world_id", &self.world_id)
            .field("base", &self.base)
            .finish()
    }
}

// guarantees that `ComponentId` matches `ReflectFromPtr`
fn get_reflect_from_ptr(
    world: &World,
    component_id: ComponentId,
) -> Result<ReflectFromPtr, ReflectValueRefError> {
    let info = world
        .components()
        .get_info(component_id)
        .ok_or(ReflectValueRefError::InexistentComponent(component_id))?;
    let type_id = info
        .type_id()
        .ok_or(ReflectValueRefError::NoTypeId(component_id))?;

    let type_registry = world
        .get_resource::<AppTypeRegistry>()
        .ok_or(ReflectValueRefError::TypeRegistryNotInWorld)?;
    let reflect_from_ptr = type_registry
        .read()
        .get_type_data::<ReflectFromPtr>(type_id)
        .ok_or(ReflectValueRefError::NoReflectFromPtr(component_id))?
        .clone();
    assert_eq!(reflect_from_ptr.type_id(), type_id);

    Ok(reflect_from_ptr)
}

impl EcsValueRef {
    pub fn component(
        world: &World,
        entity: Entity,
        component_id: ComponentId,
    ) -> Result<EcsValueRef, ReflectValueRefError> {
        let reflect_from_ptr = get_reflect_from_ptr(world, component_id)?;
        let base = EcsBase::Component(entity, component_id);

        Ok(EcsValueRef {
            base,
            world_id: world.id(),
            reflect_from_ptr,
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
    ) -> EcsValueRef {
        EcsValueRef {
            base: EcsBase::Component(entity, component_id),
            world_id,
            reflect_from_ptr,
        }
    }

    pub fn resource(
        world: &World,
        component_id: ComponentId,
    ) -> Result<EcsValueRef, ReflectValueRefError> {
        let reflect_from_ptr = get_reflect_from_ptr(world, component_id)?;
        let base = EcsBase::Resource(component_id);

        Ok(EcsValueRef {
            base,
            world_id: world.id(),
            reflect_from_ptr,
        })
    }

    /// # Safety
    /// The `ComponentId` parameter must resolve to a resource in the world with `world_id`
    /// that has the same type as the one `ReflectFromPtr` was created for
    pub unsafe fn resource_unchecked(
        component_id: ComponentId,
        reflect_from_ptr: ReflectFromPtr,
        world_id: WorldId,
    ) -> EcsValueRef {
        EcsValueRef {
            base: EcsBase::Resource(component_id),
            world_id,
            reflect_from_ptr,
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
            .ok_or(ReflectValueRefError::InvalidBaseValue(self.base))?;
        // SAFETY:
        // `val` is a pointer to value of the type that the `ReflectFromPtr` was constructed for,
        // because the mapping from `ComponentId -> TypeId` is immutable and `ReflectFromPtr` is checked to be
        // for the type of the `WorldBase`'s type id.
        let reflect = unsafe { self.reflect_from_ptr.as_reflect_ptr(ptr) };

        Ok(reflect)
    }
    pub fn get_mut<'w>(
        &self,
        world: &'w mut World,
    ) -> Result<&'w mut dyn Reflect, ReflectValueRefError> {
        // SAFETY: unique world access
        unsafe { self.get_mut_unchecked(world) }
    }

    /// # Safety
    /// This will allow aliased mutable access to the value. The caller must ensure
    /// that there is either only one mutable access or multiple immutable accesses at a time.
    pub unsafe fn get_mut_unchecked<'w>(
        &self,
        world: &'w mut World,
    ) -> Result<&'w mut dyn Reflect, ReflectValueRefError> {
        assert_eq!(self.world_id, world.id());

        let mut ptr = self
            .base
            .access_mut(world)
            .ok_or(ReflectValueRefError::InvalidBaseValue(self.base))?;

        // TODO: don't mark as changed on every access
        ptr.set_changed();

        // SAFETY:
        // `val` is a pointer to value of the type that the `ReflectFromPtr` was constructed for,
        // because the mapping from `ComponentId -> TypeId` is immutable and `ReflectFromPtr` is checked to be
        // for the type of the `WorldBase`'s type id.
        let reflect = unsafe { self.reflect_from_ptr.as_reflect_ptr_mut(ptr.into_inner()) };

        Ok(reflect)
    }
}

/// A [`ReflectValueRef`] without the path offset
#[derive(Clone, Debug)]
pub enum ReflectValueRefBase {
    EccRef(EcsValueRef),
    Free(Rc<RefCell<Box<dyn Reflect>>>),
}

/// Either a [`EcsValueRef`] or a freestanding [`Reflect`] value, together with a reflect-path offset
#[derive(Clone, Debug)]
pub struct ReflectValueRef {
    base: ReflectValueRefBase,
    path: String,
}
impl From<EcsValueRef> for ReflectValueRef {
    fn from(value: EcsValueRef) -> Self {
        ReflectValueRef {
            base: ReflectValueRefBase::EccRef(value),
            path: String::new(),
        }
    }
}

enum MaybeRef<'a, T: ?Sized> {
    Direct(&'a T),
    Ref(std::cell::Ref<'a, Box<T>>),
}

/// Wraps a borrowed reference to a value in a [`ReflectValueRef`]
pub struct ReflectValueRefBorrow<'a> {
    r: MaybeRef<'a, dyn Reflect>,
    path: &'a str,
}
impl std::fmt::Debug for ReflectValueRefBorrow<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let base = match self.r {
            MaybeRef::Direct(r) => r,
            MaybeRef::Ref(ref r) => &***r,
        };
        let value = base
            .path(self.path)
            .expect("paths are checked in `append_path`");
        value.fmt(f)
    }
}

impl<'a> Deref for ReflectValueRefBorrow<'a> {
    type Target = dyn Reflect;
    fn deref(&self) -> &Self::Target {
        let base = match self.r {
            MaybeRef::Direct(r) => r,
            MaybeRef::Ref(ref r) => &***r,
        };
        let value = base
            .path(self.path)
            .expect("paths are checked in `append_path`");
        value
    }
}

enum MaybeRefMut<'a, T: ?Sized> {
    Direct(&'a mut T),
    Ref(std::cell::RefMut<'a, Box<T>>),
}

/// Wraps a borrowed mutable reference to a value in a [`ReflectValueRef`]
pub struct ReflectValueRefBorrowMut<'a> {
    r: MaybeRefMut<'a, dyn Reflect>,
    path: &'a str,
}
impl std::fmt::Debug for ReflectValueRefBorrowMut<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let base = match &self.r {
            MaybeRefMut::Direct(r) => &**r,
            MaybeRefMut::Ref(ref r) => &***r,
        };
        base.fmt(f)
    }
}

impl<'a> Deref for ReflectValueRefBorrowMut<'a> {
    type Target = dyn Reflect;

    fn deref(&self) -> &Self::Target {
        let base = match self.r {
            MaybeRefMut::Direct(ref r) => &**r,
            MaybeRefMut::Ref(ref r) => &***r,
        };
        let value = base
            .path(self.path)
            .expect("paths are checked in `append_path`");
        value
    }
}
impl<'a> DerefMut for ReflectValueRefBorrowMut<'a> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        let base = match &mut self.r {
            MaybeRefMut::Direct(r) => &mut **r,
            MaybeRefMut::Ref(r) => &mut ***r,
        };
        let value = base
            .path_mut(self.path)
            .expect("paths are checked in `append_path`");
        value
    }
}

impl ReflectValueRef {
    pub fn free(value: Rc<RefCell<Box<dyn Reflect>>>) -> Self {
        ReflectValueRef {
            base: ReflectValueRefBase::Free(value),
            path: String::new(),
        }
    }

    pub fn ecs_ref(value: EcsValueRef) -> Self {
        ReflectValueRef {
            base: ReflectValueRefBase::EccRef(value),
            path: String::new(),
        }
    }
    /// Appends a new path to the reflect path offset of the [`ReflectValueRef`]
    /// ```rust,no_run
    /// # let value: bevy_ecs_dynamic::reflect_value_ref::ReflectValueRef = unimplemented!();
    /// # let world: bevy_ecs::prelude::World = unimplemented!();
    /// value.append_path(".foo", &world);
    /// value.append_path("[3]", &world);
    /// ```
    /// This method will *not* check the validity of the path, and further calls to [`get`](Self::get) or [`get_mut`](Self::get_mut) may fail afterwards.
    // TODO make this check the path, either by taking the world or accessing static info
    pub fn append_path(&self, path: &str, world: &World) -> Result<Self, ReflectValueRefError> {
        let new = ReflectValueRef {
            base: self.base.clone(),
            path: format!("{}{}", self.path, path),
        };

        {
            // TODO: this can surely be less duplicated
            let borrow = new.get(world)?;
            let base = match borrow.r {
                MaybeRef::Direct(r) => r,
                MaybeRef::Ref(ref r) => &***r,
            };
            base.path(&new.path)
                .map_err(|e| ReflectValueRefError::InvalidPath {
                    error_message: e.to_string(),
                    path: new.path.clone(),
                })?;
        }

        Ok(new)
    }

    pub fn get<'s, 'w: 's>(
        &'s self,
        world: &'w World,
    ) -> Result<ReflectValueRefBorrow<'s>, ReflectValueRefError> {
        match &self.base {
            ReflectValueRefBase::EccRef(value) => {
                let value = value.get(world)?;
                Ok(ReflectValueRefBorrow {
                    r: MaybeRef::Direct(value),
                    path: &self.path,
                })
            }
            ReflectValueRefBase::Free(value) => {
                let value = value.borrow();
                Ok(ReflectValueRefBorrow {
                    r: MaybeRef::Ref(value),
                    path: &self.path,
                })
            }
        }
    }
    pub fn get_mut<'s, 'w: 's>(
        &'s mut self,
        world: &'w mut World,
    ) -> Result<ReflectValueRefBorrowMut<'s>, ReflectValueRefError> {
        match &self.base {
            ReflectValueRefBase::EccRef(value) => {
                let value = value.get_mut(world)?;
                Ok(ReflectValueRefBorrowMut {
                    r: MaybeRefMut::Direct(value),
                    path: &self.path,
                })
            }
            ReflectValueRefBase::Free(value) => {
                let value = value.borrow_mut();
                Ok(ReflectValueRefBorrowMut {
                    r: MaybeRefMut::Ref(value),
                    path: &self.path,
                })
            }
        }
    }
}
