use crate::api::*;
use mmtk::util::opaque_pointer::*;
use mmtk::AllocationSemantics;

#[test]
pub fn issue139_alloc_non_multiple_of_min_alignment() {
    mmtk_gc_init(200*1024*1024);
    let handle = mmtk_bind_mutator(VMMutatorThread(VMThread::UNINITIALIZED));

    // Allocate 6 bytes with 8 bytes ailgnment required
    let addr = mmtk_alloc(handle, 14, 8, 0, AllocationSemantics::Default);
    assert!(addr.is_aligned_to(8));
    // After the allocation, the cursor is not MIN_ALIGNMENT aligned. If we have the assertion in the next allocation to check if the cursor is aligned to MIN_ALIGNMENT, it fails.
    // We have to remove that assertion.
    let addr2 = mmtk_alloc(handle, 14, 8, 0, AllocationSemantics::Default);
    assert!(addr2.is_aligned_to(8));
}
