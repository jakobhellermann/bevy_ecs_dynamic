use bevy_ecs::{component::ComponentId, prelude::*, query::QueryEntityError, world::WorldId};
use bevy_reflect::ReflectFromPtr;

use crate::dynamic_query::{DynamicQuery, DynamicQueryIter, FilterKind};

use super::{get_reflect_from_ptr, EcsValueRef};

/// Wrapper around [`DynamicQuery`] that yields [`EcsValueRef`]s
pub struct EcsValueRefQuery {
    query: DynamicQuery,
}

impl EcsValueRefQuery {
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

    pub fn get<'w, 's>(
        &'s mut self,
        world: &'w World,
        entity: Entity,
    ) -> Result<Vec<EcsValueRef>, QueryEntityError> {
        let components = self.components(world);
        self.query.get(world, entity)?;

        let results = components
            .iter()
            .map(|(component_id, reflect_from_ptr)| {
                // SAFETY: component_id matches the reflect_from_ptr
                unsafe {
                    EcsValueRef::component_unchecked(
                        entity,
                        *component_id,
                        reflect_from_ptr.clone(),
                        world.id(),
                    )
                }
            })
            .collect();

        Ok(results)
    }

    pub fn iter<'w, 's>(&'s mut self, world: &'w World) -> EcsValueRefQueryIter<'w, 's> {
        let components = self.components(world);

        EcsValueRefQueryIter {
            world_id: world.id(),
            components,
            iter: self.query.iter(world),
        }
    }

    fn components<'s>(&'s mut self, world: &World) -> Vec<(ComponentId, ReflectFromPtr)> {
        self.query
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
            .collect()
    }
}

/// Iterator type of the [`EcsValueRefQuery`]
pub struct EcsValueRefQueryIter<'w, 's> {
    world_id: WorldId,
    iter: DynamicQueryIter<'w, 's>,
    components: Vec<(ComponentId, ReflectFromPtr)>,
}

/// Item type of the [`EcsValueRefQuery`]
pub struct EcsValueRefQueryItem {
    pub entity: Entity,
    pub items: Vec<EcsValueRef>,
}

impl<'w, 's> Iterator for EcsValueRefQueryIter<'w, 's> {
    type Item = EcsValueRefQueryItem;

    fn next(&mut self) -> Option<Self::Item> {
        let item = self.iter.next()?;

        let items: Vec<_> = self
            .components
            .iter()
            .map(|(component_id, reflect_from_ptr)| unsafe {
                EcsValueRef::component_unchecked(
                    item.entity,
                    *component_id,
                    reflect_from_ptr.clone(),
                    self.world_id,
                )
            })
            .collect();

        Some(EcsValueRefQueryItem {
            entity: item.entity,
            items,
        })
    }
}

#[cfg(test)]
mod tests {
    use bevy_ecs::prelude::*;
    use bevy_reflect::{Reflect, TypeRegistryArc};

    use crate::reflect_value_ref::ReflectValueRef;

    use super::EcsValueRefQuery;

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

        let type_registry = world.get_resource_or_insert_with(TypeRegistryArc::default);
        {
            let mut type_registry = type_registry.write();
            type_registry.register::<TestComponent1>();
            type_registry.register::<TestComponent2>();
        }

        world.spawn().insert(TestComponent1 { value: "no".into() });
        world.spawn().insert_bundle((
            TestComponent1 {
                value: "yes".into(),
            },
            TestComponent2 { field: 5 },
        ));

        let mut query = EcsValueRefQuery::new(&world, &[component_id_1, component_id_2]);
        let results: Vec<_> = query.iter(&world).collect();
        assert_eq!(results.len(), 1);

        match results[0].items.as_slice() {
            [a, b] => {
                assert_eq!(a.component_id(), component_id_1);
                assert_eq!(b.component_id(), component_id_2);

                let a = ReflectValueRef::from(a.clone());
                assert_eq!(
                    a.append_path(".value", &world)
                        .unwrap()
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
