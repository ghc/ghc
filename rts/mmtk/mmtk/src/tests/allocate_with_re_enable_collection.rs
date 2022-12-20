use crate::api::*;
use mmtk::util::opaque_pointer::*;
use mmtk::AllocationSemantics;

/// This test allocates after calling initialize_collection(). When we exceed the heap limit, MMTk will trigger a GC. And block_for_gc will be called.
/// We havent implemented block_for_gc so it will panic. This test is similar to allocate_with_initialize_collection, except that we once disabled GC in the test.
#[test]
#[should_panic(expected = "block_for_gc is not implemented")]
pub fn allocate_with_re_enable_collection() {
    const MB: usize = 1024 * 1024;
    // 1MB heap
    mmtk_gc_init(MB);
    mmtk_initialize_collection(VMThread::UNINITIALIZED);
    let handle = mmtk_bind_mutator(VMMutatorThread(VMThread::UNINITIALIZED));
    // Allocate 1MB. It should be fine.
    let addr = mmtk_alloc(handle, MB, 8, 0, AllocationSemantics::Default);
    assert!(!addr.is_zero());
    // Disable GC. So we can keep allocate without triggering a GC.
    mmtk_disable_collection();
    let addr = mmtk_alloc(handle, MB, 8, 0, AllocationSemantics::Default);
    assert!(!addr.is_zero());
    // Enable GC again. When we allocate, we should see a GC triggered immediately.
    mmtk_enable_collection();
    let addr = mmtk_alloc(handle, MB, 8, 0, AllocationSemantics::Default);
    assert!(!addr.is_zero());
}
