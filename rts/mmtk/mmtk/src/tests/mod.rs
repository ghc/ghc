// NOTE: Since the dummyvm uses a global MMTK instance,
// it will panic if MMTK initialized more than once per process.
// We run each of the following modules in a separate test process.
//
// One way to avoid re-initialization is to have only one #[test] per module.
// There are also helpers for creating fixtures in `fixture/mod.rs`.
mod issue139;
mod handle_mmap_oom;
mod handle_mmap_conflict;
mod allocate_without_initialize_collection;
mod allocate_with_initialize_collection;
mod allocate_with_disable_collection;
mod allocate_with_re_enable_collection;
mod malloc;
#[cfg(feature = "is_mmtk_object")]
mod conservatism;
mod is_in_mmtk_spaces;
mod fixtures;
