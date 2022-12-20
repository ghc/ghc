use crate::api::*;
use mmtk::util::opaque_pointer::*;
use mmtk::AllocationSemantics;

/// This test allocates without calling initialize_collection(). When we exceed the heap limit, a GC should be triggered by MMTk.
/// But as we haven't enabled collection, GC is not initialized, so MMTk will panic.
#[test]
#[should_panic(expected = "GC is not allowed here")]
pub fn allocate_without_initialize_collection() {
    const MB: usize = 1024 * 1024;
    // 1MB heap
    mmtk_gc_init(MB);
    let handle = mmtk_bind_mutator(VMMutatorThread(VMThread::UNINITIALIZED));
    // Attempt to allocate 2MB memory. This should trigger a GC, but as we never call initialize_collection(), we cannot do GC.
    let addr = mmtk_alloc(handle, 2 * MB, 8, 0, AllocationSemantics::Default);
    assert!(!addr.is_zero());
}
