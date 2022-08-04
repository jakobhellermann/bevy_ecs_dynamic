# bevy_ecs_dynamic

Utilities for working with `bevy_ecs` in situations where the types you're dealing with might not be known at compile time (e.g. scripting, modding).

# Example Usage

## Dynamic Query

```rust
let component_id_1 = world.init_component::<TestComponent1>();
let component_id_2 = world.init_component::<TestComponent2>();

world.spawn().insert(TestComponent1).insert(TestComponent2);
world.spawn().insert(TestComponent1);

let mut query = DynamicQuery::new(&world, vec![FetchKind::Ref(component_id_1)], vec![FilterKind::Without(component_id_2)]);
assert_eq!(query.iter(&world).count(), 1);
```


# Bevy support table

| bevy | bevy\_ecs\_dynamic |
| ---- | ------------------ |
| 0.8  | _unreleased_       |
