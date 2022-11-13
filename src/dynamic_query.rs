use std::cell::UnsafeCell;

use bevy_ecs::archetype::{
    Archetype, ArchetypeComponentId, ArchetypeEntity, ArchetypeGeneration, ArchetypeId, Archetypes,
};
use bevy_ecs::component::{ComponentId, ComponentTicks, StorageType};
use bevy_ecs::prelude::*;
use bevy_ecs::ptr::{Ptr, PtrMut, ThinSlicePtr, UnsafeCellDeref};
use bevy_ecs::query::{Access, FilteredAccess, QueryEntityError};
use bevy_ecs::storage::{ComponentSparseSet, Table, TableId, Tables};
use bevy_ecs::world::WorldId;
use fixedbitset::FixedBitSet;

#[derive(Clone, Copy, Debug)]
pub enum FetchKind {
    Ref(ComponentId),
    RefMut(ComponentId),
}

impl FetchKind {
    fn is_readonly(self) -> bool {
        match self {
            FetchKind::Ref(_) => true,
            FetchKind::RefMut(_) => false,
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub enum FilterKind {
    With(ComponentId),
    Without(ComponentId),
    Changed(ComponentId),
    Added(ComponentId),
}

pub enum FetchResult<'w> {
    Ref(Ptr<'w>),
    RefMut {
        value: PtrMut<'w>,
        ticks: &'w mut ComponentTicks,
        last_change_tick: u32,
        change_tick: u32,
    },
}

impl FetchKind {
    fn component_id(self) -> ComponentId {
        match self {
            FetchKind::Ref(id) | FetchKind::RefMut(id) => id,
        }
    }

    fn matches_archetype(&self, archetype: &Archetype) -> bool {
        match *self {
            FetchKind::Ref(component_id) | FetchKind::RefMut(component_id) => {
                archetype.contains(component_id)
            }
        }
    }

    fn update_archetype_component_access(
        &self,
        archetype: &Archetype,
        access: &mut Access<ArchetypeComponentId>,
    ) {
        match *self {
            FetchKind::Ref(id) => {
                access.add_read(archetype.get_archetype_component_id(id).unwrap())
            }
            FetchKind::RefMut(id) => {
                access.add_write(archetype.get_archetype_component_id(id).unwrap())
            }
        }
    }

    fn update_component_access(
        &self,
        access: &mut FilteredAccess<ComponentId>,
    ) -> Result<(), QueryError> {
        match *self {
            FetchKind::Ref(id) => {
                if access.access().has_write(id) {
                    return Err(QueryError::ConflicingAccessFetch(*self));
                }
                access.add_read(id);
            }
            FetchKind::RefMut(id) => {
                if access.access().has_read(id) {
                    return Err(QueryError::ConflicingAccessFetch(*self));
                }
                access.add_write(id)
            }
        }

        Ok(())
    }
}

impl FilterKind {
    fn component_id(self) -> ComponentId {
        match self {
            FilterKind::With(id)
            | FilterKind::Without(id)
            | FilterKind::Changed(id)
            | FilterKind::Added(id) => id,
        }
    }

    fn matches_archetype(&self, archetype: &Archetype) -> bool {
        match *self {
            FilterKind::With(id) => archetype.contains(id),
            FilterKind::Without(id) => !archetype.contains(id),
            FilterKind::Changed(id) => archetype.contains(id),
            FilterKind::Added(id) => archetype.contains(id),
        }
    }

    fn update_archetype_component_access(
        &self,
        archetype: &Archetype,
        access: &mut Access<ArchetypeComponentId>,
    ) {
        match *self {
            FilterKind::With(_) => {}
            FilterKind::Without(_) => {}
            FilterKind::Changed(id) | FilterKind::Added(id) => {
                access.add_read(archetype.get_archetype_component_id(id).unwrap())
            }
        }
    }

    fn update_component_access(
        &self,
        access: &mut FilteredAccess<ComponentId>,
    ) -> Result<(), QueryError> {
        match *self {
            FilterKind::With(id) => access.add_with(id),
            FilterKind::Without(id) => access.add_without(id),
            FilterKind::Changed(id) => {
                if access.access().has_write(id) {
                    return Err(QueryError::ConflicingAccessFilter(*self));
                }
                access.add_read(id);
            }
            FilterKind::Added(id) => {
                if access.access().has_write(id) {
                    return Err(QueryError::ConflicingAccessFilter(*self));
                }
                access.add_read(id);
            }
        }

        Ok(())
    }
}

#[derive(Debug)]
pub enum QueryError {
    ConflicingAccessFilter(FilterKind),
    ConflicingAccessFetch(FetchKind),
}

impl std::fmt::Display for QueryError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            QueryError::ConflicingAccessFilter(filter) => {
                write!(f, "{filter:?} conflicts with a previous access in this query. Shared access cannot coincide with exclusive access.")
            }
            QueryError::ConflicingAccessFetch(fetch @ FetchKind::Ref(_)) => {
                write!(f ,"{fetch:?} conflicts with a previous access in this query. Shared access cannot coincide with exclusive access.")
            }
            QueryError::ConflicingAccessFetch(fetch @ FetchKind::RefMut(_)) => {
                write!(f, "{fetch:?} conflicts with a previous access in this query. Mutable component access must be unique.")
            }
        }
    }
}

impl std::error::Error for QueryError {}
struct ComponentFetchState<'w> {
    table_components: Option<Ptr<'w>>,
    entity_table_rows: Option<ThinSlicePtr<'w, usize>>,
    sparse_set: Option<&'w ComponentSparseSet>,

    table_ticks: Option<ThinSlicePtr<'w, UnsafeCell<ComponentTicks>>>,
    last_change_tick: u32,
    change_tick: u32,

    fetch_kind: FetchKind,
    storage_type: StorageType,
    component_size: usize,
}

impl<'w> ComponentFetchState<'w> {
    unsafe fn init(
        world: &'w World,
        last_change_tick: u32,
        change_tick: u32,
        fetch_kind: FetchKind,
    ) -> ComponentFetchState<'w> {
        let component_id = fetch_kind.component_id();
        let component_info = world.components().get_info(component_id).unwrap();

        ComponentFetchState {
            table_components: None,
            entity_table_rows: None,
            sparse_set: None,
            table_ticks: None,
            last_change_tick,
            change_tick,
            fetch_kind,
            storage_type: component_info.storage_type(),
            component_size: component_info.layout().size(),
        }
    }

    #[inline]
    unsafe fn set_archetype(&mut self, archetype: &'w Archetype, tables: &'w Tables) {
        match self.storage_type {
            StorageType::Table => {
                let column = tables[archetype.table_id()]
                    .get_column(self.fetch_kind.component_id())
                    .unwrap();
                self.table_components = Some(column.get_data_ptr());
                self.table_ticks = Some(column.get_ticks_slice().into());
            }
            StorageType::SparseSet => {}
        }
    }

    #[inline]
    unsafe fn set_table(&mut self, table: &'w Table) {
        let column = table.get_column(self.fetch_kind.component_id()).unwrap();
        self.table_components = Some(column.get_data_ptr());
        self.table_ticks = Some(column.get_ticks_slice().into());
    }

    #[inline]
    unsafe fn fetch(&mut self, entity: Entity, table_row: usize) -> FetchResult<'w> {
        match self.storage_type {
            StorageType::Table => {
                let (table_components, table_ticks) =
                    self.table_components.zip(self.table_ticks).unwrap();
                let value = table_components.byte_add(table_row * self.component_size);
                let component_ticks = table_ticks.get(table_row);
                match self.fetch_kind {
                    FetchKind::Ref(_) => FetchResult::Ref(value),
                    FetchKind::RefMut(_) => FetchResult::RefMut {
                        value: value.assert_unique(),
                        ticks: component_ticks.deref_mut(),
                        last_change_tick: self.last_change_tick,
                        change_tick: self.change_tick,
                    },
                }
            }
            StorageType::SparseSet => match self.storage_type {
                StorageType::Table => {
                    let (entity_table_rows, (table_components, table_ticks)) = self
                        .entity_table_rows
                        .zip(self.table_components.zip(self.table_ticks))
                        .unwrap();
                    let table_row = *entity_table_rows.get(table_row);
                    let value = table_components.byte_add(table_row * self.component_size);

                    match self.fetch_kind {
                        FetchKind::Ref(_) => FetchResult::Ref(value),
                        FetchKind::RefMut(_) => {
                            let component_ticks = table_ticks.get(table_row).deref_mut();
                            FetchResult::RefMut {
                                value: value.assert_unique(),
                                ticks: component_ticks,
                                last_change_tick: self.last_change_tick,
                                change_tick: self.change_tick,
                            }
                        }
                    }
                }
                StorageType::SparseSet => {
                    let sparse_set = self.sparse_set.unwrap();
                    let (value, component_ticks) = sparse_set.get_with_ticks(entity).unwrap();
                    match self.fetch_kind {
                        FetchKind::Ref(_) => FetchResult::Ref(value),
                        FetchKind::RefMut(_) => FetchResult::RefMut {
                            value: value.assert_unique(),
                            ticks: component_ticks.deref_mut(),
                            last_change_tick: self.last_change_tick,
                            change_tick: self.change_tick,
                        },
                    }
                }
            },
        }
    }
}

struct ComponentFetchStates<'w> {
    entities: Option<ThinSlicePtr<'w, Entity>>,
    components: Vec<ComponentFetchState<'w>>,
}

impl<'w> ComponentFetchStates<'w> {
    fn is_dense(&self) -> bool {
        self.components
            .iter()
            .all(|component| component.storage_type == StorageType::Table)
    }
}

impl<'w> ComponentFetchStates<'w> {
    unsafe fn init(
        world: &'w World,
        last_change_tick: u32,
        change_tick: u32,
        component_fetches: &[FetchKind],
    ) -> ComponentFetchStates<'w> {
        ComponentFetchStates {
            entities: None,
            components: component_fetches
                .iter()
                .map(|kind| ComponentFetchState::init(world, last_change_tick, change_tick, *kind))
                .collect(),
        }
    }

    #[inline]
    unsafe fn set_archetype(&mut self, archetype: &'w Archetype, tables: &'w Tables) {
        self.components
            .iter_mut()
            .for_each(|component| component.set_archetype(archetype, tables));
    }

    #[inline]
    unsafe fn set_table(&mut self, table: &'w Table) {
        self.entities = Some(table.entities().into());
        self.components
            .iter_mut()
            .for_each(|component| component.set_table(table));
    }

    #[inline]
    unsafe fn fetch(&mut self, entity: Entity, table_row: usize) -> Vec<FetchResult<'w>> {
        self.components
            .iter_mut()
            .map(|component| component.fetch(entity, table_row))
            .collect()
    }

    #[inline]
    unsafe fn entity(&mut self, index: usize) -> Entity {
        *self.entities.unwrap().get(index)
    }
}

struct ComponentFilterChangeDetection<'w> {
    table_ticks: Option<ThinSlicePtr<'w, UnsafeCell<ComponentTicks>>>,
    entities: Option<ThinSlicePtr<'w, ArchetypeEntity>>,
    sparse_set: Option<&'w ComponentSparseSet>,
    last_change_tick: u32,
    change_tick: u32,
}

impl<'w> ComponentFilterChangeDetection<'w> {
    unsafe fn archetype_ticks(
        &self,
        storage_type: StorageType,
        entity: Entity,
        table_row: usize,
    ) -> ComponentTicks {
        match storage_type {
            StorageType::Table => {
                let ticks = *self.table_ticks.unwrap().get(table_row).deref();
                ticks
            }
            StorageType::SparseSet => {
                let ticks = self
                    .sparse_set
                    .unwrap()
                    .get_ticks(entity)
                    .map(|ticks| &*ticks.get())
                    .cloned()
                    .unwrap();
                ticks
            }
        }
    }
}

struct ComponentFilterState<'w> {
    component_id: ComponentId,
    storage_type: StorageType,
    kind: FilterKind,
    change_detection: ComponentFilterChangeDetection<'w>,
}
impl<'w> ComponentFilterState<'w> {
    fn init(
        world: &'w World,
        last_change_tick: u32,
        change_tick: u32,
        kind: FilterKind,
    ) -> ComponentFilterState<'w> {
        let component_id = kind.component_id();
        let component_info = world.components().get_info(component_id).unwrap();
        ComponentFilterState {
            component_id,
            storage_type: component_info.storage_type(),
            kind,
            change_detection: ComponentFilterChangeDetection {
                table_ticks: None,
                entities: None,
                sparse_set: (component_info.storage_type() == StorageType::SparseSet)
                    .then(|| world.storages().sparse_sets.get(component_id).unwrap()),
                last_change_tick,
                change_tick,
            },
        }
    }

    #[inline]
    unsafe fn set_archetype(&mut self, archetype: &'w Archetype, tables: &'w Tables) {
        match self.kind {
            FilterKind::With(_) | FilterKind::Without(_) => {}
            FilterKind::Changed(_) | FilterKind::Added(_) => match self.storage_type {
                StorageType::Table => {
                    let table = &tables[archetype.table_id()];
                    self.change_detection.table_ticks = Some(
                        table
                            .get_column(self.component_id)
                            .unwrap()
                            .get_ticks_slice()
                            .into(),
                    );
                }
                StorageType::SparseSet => {
                    self.change_detection.entities = Some(archetype.entities().into())
                }
            },
        }
    }

    #[inline]
    unsafe fn set_table(&mut self, table: &'w Table) {
        match self.kind {
            FilterKind::With(_) | FilterKind::Without(_) => {}
            FilterKind::Changed(_) | FilterKind::Added(_) => {
                self.change_detection.table_ticks = Some(
                    table
                        .get_column(self.component_id)
                        .unwrap()
                        .get_ticks_slice()
                        .into(),
                );
            }
        }
    }

    #[inline]
    unsafe fn fetch(&mut self, entity: Entity, table_row: usize) -> bool {
        match self.storage_type {
            StorageType::Table => match self.kind {
                FilterKind::With(_) | FilterKind::Without(_) => true,
                FilterKind::Changed(_) => ComponentTicks::is_changed(
                    (self.change_detection.table_ticks.unwrap().get(table_row)).deref(),
                    self.change_detection.last_change_tick,
                    self.change_detection.change_tick,
                ),
                FilterKind::Added(_) => ComponentTicks::is_added(
                    (self.change_detection.table_ticks.unwrap().get(table_row)).deref(),
                    self.change_detection.last_change_tick,
                    self.change_detection.change_tick,
                ),
            },
            StorageType::SparseSet => match self.kind {
                FilterKind::With(_) | FilterKind::Without(_) => true,
                FilterKind::Changed(_) => {
                    let ticks =
                        self.change_detection
                            .archetype_ticks(self.storage_type, entity, table_row);
                    ticks.is_changed(
                        self.change_detection.last_change_tick,
                        self.change_detection.change_tick,
                    )
                }
                FilterKind::Added(_) => {
                    let ticks =
                        self.change_detection
                            .archetype_ticks(self.storage_type, entity, table_row);
                    ticks.is_added(
                        self.change_detection.last_change_tick,
                        self.change_detection.change_tick,
                    )
                }
            },
        }
    }
}

struct ComponentFilterStates<'w> {
    filters: Vec<ComponentFilterState<'w>>,
}

impl<'w> ComponentFilterStates<'w> {
    fn is_dense(&self) -> bool {
        self.filters
            .iter()
            .all(|filter| filter.storage_type == StorageType::Table)
    }
}

impl<'w> ComponentFilterStates<'w> {
    unsafe fn init(
        world: &'w World,
        last_change_tick: u32,
        change_tick: u32,
        filters: &[FilterKind],
    ) -> ComponentFilterStates<'w> {
        ComponentFilterStates {
            filters: filters
                .iter()
                .map(|kind| ComponentFilterState::init(world, last_change_tick, change_tick, *kind))
                .collect(),
        }
    }

    #[inline]
    unsafe fn set_archetype(&mut self, archetype: &'w Archetype, tables: &'w Tables) {
        self.filters
            .iter_mut()
            .for_each(|filter| filter.set_archetype(archetype, tables));
    }

    #[inline]
    unsafe fn set_table(&mut self, table: &'w Table) {
        self.filters
            .iter_mut()
            .for_each(|filter| filter.set_table(table));
    }

    #[inline]
    unsafe fn fetch(&mut self, entity: Entity, table_row: usize) -> bool {
        self.filters
            .iter_mut()
            .all(|filter| filter.fetch(entity, table_row))
    }
}

pub struct DynamicQuery {
    world_id: WorldId,
    component_fetches: Vec<FetchKind>,
    filters: Vec<FilterKind>,

    archetype_generation: ArchetypeGeneration,
    // NOTE: we maintain both a TableId bitset and a vec because iterating the vec is faster
    matched_tables: FixedBitSet,
    matched_table_ids: Vec<TableId>,
    // NOTE: we maintain both a ArchetypeId bitset and a vec because iterating the vec is faster
    matched_archetypes: FixedBitSet,
    matched_archetype_ids: Vec<ArchetypeId>,
    #[allow(unused)]
    component_access: FilteredAccess<ComponentId>,
    archetype_component_access: Access<ArchetypeComponentId>,
}

impl DynamicQuery {
    pub fn new(
        world: &World,
        component_fetches: Vec<FetchKind>,
        filters: Vec<FilterKind>,
    ) -> Result<Self, QueryError> {
        let mut component_access = FilteredAccess::default();
        component_fetches
            .iter()
            .try_for_each(|fetch| fetch.update_component_access(&mut component_access))?;

        // Use a temporary empty FilteredAccess for filters. This prevents them from conflicting with the
        // main Query's `fetch_state` access. Filters are allowed to conflict with the main query fetch
        // because they are evaluated *before* a specific reference is constructed.
        let mut filter_component_access = FilteredAccess::default();
        filters
            .iter()
            .try_for_each(|filter| filter.update_component_access(&mut filter_component_access))?;

        // Merge the temporary filter access with the main access. This ensures that filter access is
        // properly considered in a global "cross-query" context (both within systems and across systems).
        component_access.extend(&filter_component_access);

        let mut query = DynamicQuery {
            world_id: world.id(),
            component_fetches,
            filters,
            component_access,
            archetype_generation: ArchetypeGeneration::initial(),
            matched_tables: Default::default(),
            matched_table_ids: Vec::new(),
            matched_archetypes: Default::default(),
            matched_archetype_ids: Vec::new(),
            archetype_component_access: Default::default(),
        };
        query.update_archetypes(world);

        Ok(query)
    }

    pub fn fetches(&self) -> &[FetchKind] {
        &self.component_fetches
    }
    pub fn filters(&self) -> &[FilterKind] {
        &self.filters
    }

    fn is_readonly(&self) -> bool {
        self.fetches().iter().all(|fetch| fetch.is_readonly())
    }

    fn validate_world(&self, world: &World) {
        assert!(
            world.id() == self.world_id,
            "Attempted to use {} with a mismatched World. QueryStates can only be used with the World they were created from.",
                std::any::type_name::<Self>(),
        );
    }

    pub fn update_archetypes(&mut self, world: &World) {
        self.validate_world(world);
        let archetypes = world.archetypes();
        let new_generation = archetypes.generation();
        let old_generation = std::mem::replace(&mut self.archetype_generation, new_generation);
        let archetype_index_range = old_generation.value()..new_generation.value();

        for archetype_index in archetype_index_range {
            self.new_archetype(&archetypes[ArchetypeId::new(archetype_index)]);
        }
    }

    fn new_archetype(&mut self, archetype: &Archetype) {
        if self
            .component_fetches
            .iter()
            .all(|f| f.matches_archetype(archetype))
            && self.filters.iter().all(|f| f.matches_archetype(archetype))
        {
            self.component_fetches.iter().for_each(|s| {
                s.update_archetype_component_access(archetype, &mut self.archetype_component_access)
            });
            self.filters.iter().for_each(|s| {
                s.update_archetype_component_access(archetype, &mut self.archetype_component_access)
            });
            let archetype_index = archetype.id().index();
            if !self.matched_archetypes.contains(archetype_index) {
                self.matched_archetypes.grow(archetype_index + 1);
                self.matched_archetypes.set(archetype_index, true);
                self.matched_archetype_ids.push(archetype.id());
            }
            let table_index = archetype.table_id().index();
            if !self.matched_tables.contains(table_index) {
                self.matched_tables.grow(table_index + 1);
                self.matched_tables.set(table_index, true);
                self.matched_table_ids.push(archetype.table_id());
            }
        }
    }

    /// Returns an [`Iterator`] over the query results for the given [`World`].
    ///
    /// This can only be called for read-only queries, and will otherwise `panic`.
    /// See [`Self::iter_mut`] for write-queries.
    pub fn iter<'w, 's>(&'s mut self, world: &'w World) -> DynamicQueryIter<'w, 's> {
        assert!(
            self.component_fetches
                .iter()
                .all(|fetch| fetch.is_readonly()),
            "attempted to call `DynamicQuery::iter` for non-read-only query"
        );

        self.update_archetypes(world);

        // SAFETY: all fetches are read only, world is checked by `update_archetypes`
        unsafe {
            self.iter_unchecked_manual(world, world.last_change_tick(), world.read_change_tick())
        }
    }

    /// Returns an [`Iterator`] over the query results for the given [`World`].
    pub fn iter_mut<'w, 's>(&'s mut self, world: &'w mut World) -> DynamicQueryIter<'w, 's> {
        self.update_archetypes(world);

        // SAFETY: query has unique world access, world is checked by `update_archetypes`
        unsafe {
            self.iter_unchecked_manual(world, world.last_change_tick(), world.read_change_tick())
        }
    }

    /// # Safety
    ///
    /// This does not check for mutable query correctness. To be safe, make sure mutable queries
    /// have unique access to the components they query.
    /// This does not validate that `world.id()` matches `self.world_id`. Calling this on a `world`
    /// with a mismatched [`WorldId`] is unsound.
    unsafe fn iter_unchecked_manual<'w, 's>(
        &'s self,
        world: &'w World,
        last_change_tick: u32,
        change_tick: u32,
    ) -> DynamicQueryIter<'w, 's> {
        DynamicQueryIter::new(world, self, last_change_tick, change_tick)
    }

    pub fn get_mut<'w, 's>(
        &'s mut self,
        world: &'w mut World,
        entity: Entity,
    ) -> Result<Vec<FetchResult<'w>>, QueryEntityError> {
        self.update_archetypes(world);

        // SAFETY: query has unique world access
        unsafe { self.get_unchecked(world, entity) }
    }

    pub fn get<'w, 's>(
        &'s mut self,
        world: &'w World,
        entity: Entity,
    ) -> Result<Vec<FetchResult<'w>>, QueryEntityError> {
        self.update_archetypes(world);

        assert!(self.is_readonly());
        // SAFETY: query is readonly and borrows from 'w world
        unsafe { self.get_unchecked(world, entity) }
    }

    /// # Safety
    ///
    /// This does not check for mutable query correctness. To be safe, make sure mutable queries
    /// have unique access to the components they query.
    /// This does not validate that `world.id()` matches `self.world_id`. Calling this on a `world`
    /// with a mismatched [`WorldId`] is unsound.
    unsafe fn get_unchecked<'w, 's>(
        &'s mut self,
        world: &'w World,
        entity: Entity,
    ) -> Result<Vec<FetchResult<'w>>, QueryEntityError> {
        self.update_archetypes(world);

        self.get_unchecked_manual(world, entity)
    }
    /// # Safety
    ///
    /// This does not check for mutable query correctness. To be safe, make sure mutable queries
    /// have unique access to the components they query.
    /// This does not validate that `world.id()` matches `self.world_id`. Calling this on a `world`
    /// with a mismatched [`WorldId`] is unsound.
    unsafe fn get_unchecked_manual<'w, 's>(
        &'s mut self,
        world: &'w World,
        entity: Entity,
    ) -> Result<Vec<FetchResult<'w>>, QueryEntityError> {
        let last_change_tick = world.last_change_tick();
        let change_tick = world.read_change_tick();

        let location = world
            .entities()
            .get(entity)
            .ok_or(QueryEntityError::NoSuchEntity(entity))?;
        if !self
            .matched_archetypes
            .contains(location.archetype_id.index())
        {
            return Err(QueryEntityError::QueryDoesNotMatch(entity));
        }

        let archetype = &world.archetypes()[location.archetype_id];

        let mut fetch = unsafe {
            ComponentFetchStates::init(
                world,
                last_change_tick,
                change_tick,
                &self.component_fetches,
            )
        };
        let mut filter = unsafe {
            ComponentFilterStates::init(world, last_change_tick, change_tick, &self.filters)
        };

        fetch.set_archetype(archetype, &world.storages().tables);
        filter.set_archetype(archetype, &world.storages().tables);

        if filter.fetch(entity, location.index) {
            Ok(fetch.fetch(entity, location.index))
        } else {
            Err(QueryEntityError::QueryDoesNotMatch(entity))
        }
    }
}

pub struct DynamicQueryIter<'w, 's> {
    tables: &'w Tables,
    archetypes: &'w Archetypes,
    is_dense: bool,
    table_entities: &'w [Entity],
    archetype_entities: &'w [ArchetypeEntity],
    fetch: ComponentFetchStates<'w>,
    filter: ComponentFilterStates<'w>,
    table_id_iter: std::slice::Iter<'s, TableId>,
    archetype_id_iter: std::slice::Iter<'s, ArchetypeId>,
    current_len: usize,
    current_index: usize,
}
impl<'w, 's> DynamicQueryIter<'w, 's> {
    fn new(
        world: &'w World,
        query: &'s DynamicQuery,
        last_change_tick: u32,
        change_tick: u32,
    ) -> DynamicQueryIter<'w, 's> {
        let fetch = unsafe {
            ComponentFetchStates::init(
                world,
                last_change_tick,
                change_tick,
                &query.component_fetches,
            )
        };
        let filter = unsafe {
            ComponentFilterStates::init(world, last_change_tick, change_tick, &query.filters)
        };

        let is_dense = fetch.is_dense() && filter.is_dense();

        DynamicQueryIter {
            tables: &world.storages().tables,
            archetypes: world.archetypes(),
            table_entities: &[],
            archetype_entities: &[],
            is_dense,
            fetch,
            filter,
            table_id_iter: query.matched_table_ids.iter(),
            archetype_id_iter: query.matched_archetype_ids.iter(),
            current_len: 0,
            current_index: 0,
        }
    }
}

pub struct DynamicQueryItem<'w> {
    pub entity: Entity,
    pub items: Vec<FetchResult<'w>>,
}

impl<'w, 's> Iterator for DynamicQueryIter<'w, 's> {
    type Item = DynamicQueryItem<'w>;

    fn next(&mut self) -> Option<DynamicQueryItem<'w>> {
        unsafe {
            if self.is_dense {
                loop {
                    if self.current_index == self.current_len {
                        let table_id = self.table_id_iter.next()?;
                        let table = &self.tables[*table_id];
                        self.fetch.set_table(table);
                        self.filter.set_table(table);
                        self.table_entities = table.entities();
                        self.current_len = table.entity_count();
                        self.current_index = 0;
                        continue;
                    }

                    // SAFETY: set_table was called prior.
                    // `current_index` is a table row in range of the current table, because if it was not, then the if above would have been executed.
                    let entity = self.table_entities.get_unchecked(self.current_index);
                    if !self.filter.fetch(*entity, self.current_index) {
                        self.current_index += 1;
                        continue;
                    }

                    let entity = self.fetch.entity(self.current_index);
                    let items = self.fetch.fetch(entity, self.current_index);

                    self.current_index += 1;

                    return Some(DynamicQueryItem { entity, items });
                }
            } else {
                loop {
                    if self.current_index == self.current_len {
                        let archetype_id = self.archetype_id_iter.next()?;
                        let archetype = &self.archetypes[*archetype_id];
                        self.fetch.set_archetype(archetype, self.tables);
                        self.filter.set_archetype(archetype, self.tables);
                        self.archetype_entities = archetype.entities();
                        self.current_len = archetype.len();
                        self.current_index = 0;
                        continue;
                    }

                    // SAFETY: set_archetype was called prior.
                    // `current_index` is an archetype index row in range of the current archetype, because if it was not, then the if above would have been executed.
                    let archetype_entity =
                        self.archetype_entities.get_unchecked(self.current_index);
                    if !self
                        .filter
                        .fetch(archetype_entity.entity(), archetype_entity.table_row())
                    {
                        self.current_index += 1;
                        continue;
                    }

                    let entity = self.fetch.entity(self.current_index);
                    let items = self
                        .fetch
                        .fetch(archetype_entity.entity(), archetype_entity.table_row());

                    self.current_index += 1;

                    return Some(DynamicQueryItem { entity, items });
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{DynamicQuery, FetchKind, FetchResult, FilterKind};
    use bevy_ecs::prelude::*;

    #[derive(Component)]
    struct TestComponent1;

    #[derive(Component)]
    struct TestComponent2;

    #[derive(Component)]
    struct TestComponentWithData(&'static str);

    #[test]
    fn query_empty() {
        let mut world = World::new();
        let component_id = world.init_component::<TestComponent1>();

        let mut query =
            DynamicQuery::new(&world, vec![FetchKind::Ref(component_id)], vec![]).unwrap();
        let count = query.iter_mut(&mut world).count();
        assert_eq!(count, 0);
    }

    #[test]
    fn query_fetch() {
        let mut world = World::new();
        let component_id_1 = world.init_component::<TestComponentWithData>();
        let component_id_2 = world.init_component::<TestComponent2>();

        let entity = world
            .spawn((TestComponentWithData("test"), TestComponent2))
            .id();

        let mut query = DynamicQuery::new(
            &world,
            vec![
                FetchKind::Ref(component_id_1),
                FetchKind::RefMut(component_id_2),
            ],
            vec![],
        )
        .unwrap();

        let results: Vec<_> = query.iter_mut(&mut world).collect();
        assert_eq!(results.len(), 1);

        let item = &results[0];
        assert_eq!(item.entity, entity);

        let ptr = match item.items[0] {
            FetchResult::Ref(ptr) => ptr,
            _ => unreachable!(),
        };
        let value = unsafe { ptr.deref::<TestComponentWithData>() };
        assert_eq!(value.0, "test");
    }

    #[test]
    fn query_fetch_filter_with() {
        let mut world = World::new();
        let component_id_1 = world.init_component::<TestComponent1>();
        let component_id_2 = world.init_component::<TestComponent2>();

        world.spawn((TestComponent1, TestComponent2));
        world.spawn(TestComponent1);

        let mut query =
            DynamicQuery::new(&world, vec![], vec![FilterKind::With(component_id_2)]).unwrap();
        let count = query.iter_mut(&mut world).count();
        assert_eq!(count, 1);

        let mut query =
            DynamicQuery::new(&world, vec![], vec![FilterKind::With(component_id_1)]).unwrap();
        let count = query.iter_mut(&mut world).count();
        assert_eq!(count, 2);
    }

    #[test]
    fn query_fetch_filter_without() {
        let mut world = World::new();
        let component_id_1 = world.init_component::<TestComponent1>();
        let component_id_2 = world.init_component::<TestComponent2>();

        world.spawn((TestComponent1, TestComponent2));
        world.spawn(TestComponent1);

        let mut query =
            DynamicQuery::new(&world, vec![], vec![FilterKind::Without(component_id_2)]).unwrap();
        let count = query.iter_mut(&mut world).count();
        assert_eq!(count, 1);

        let mut query =
            DynamicQuery::new(&world, vec![], vec![FilterKind::Without(component_id_1)]).unwrap();
        let count = query.iter_mut(&mut world).count();
        assert_eq!(count, 0);
    }

    #[test]
    #[should_panic = "Ref(ComponentId(0)) conflicts with a previous access in this query"]
    fn query_component_access_valid() {
        let mut world = World::new();
        let component_id_1 = world.init_component::<TestComponent1>();

        let err = DynamicQuery::new(
            &world,
            vec![
                FetchKind::RefMut(component_id_1),
                FetchKind::Ref(component_id_1),
            ],
            vec![],
        )
        .map(drop)
        .unwrap_err();

        panic!("{err}");
    }

    #[test]
    fn query_filter_added() {
        let mut world = World::new();
        let component_id_1 = world.init_component::<TestComponent1>();

        world.spawn(TestComponent1);

        let mut query =
            DynamicQuery::new(&world, vec![], vec![FilterKind::Added(component_id_1)]).unwrap();
        let count = query.iter_mut(&mut world).count();
        assert_eq!(count, 1);

        world.increment_change_tick();
        world.clear_trackers();

        let mut query =
            DynamicQuery::new(&world, vec![], vec![FilterKind::Added(component_id_1)]).unwrap();
        let count = query.iter_mut(&mut world).count();
        assert_eq!(count, 0);
    }
}
