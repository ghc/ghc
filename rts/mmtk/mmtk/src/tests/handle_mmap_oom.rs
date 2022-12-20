use mmtk::util::Address;
use mmtk::util::opaque_pointer::*;
use mmtk::util::memory;
use crate::GHCVM;

#[test]
pub fn test_handle_mmap_oom() {
    let panic_res = std::panic::catch_unwind(move || {
        let start = unsafe { Address::from_usize(0x100_0000 )};
        let one_terabyte = 1000000000000;
        // mmap 1 terabyte memory - we expect this will fail due to out of memory.
        // If that's not the case, increase the size we mmap.
        let mmap_res = memory::dzmmap_noreplace(start, one_terabyte);

        memory::handle_mmap_error::<GHCVM>(mmap_res.err().unwrap(), VMThread::UNINITIALIZED);
    });
    assert!(panic_res.is_err());

    // The error should match the default implementation of Collection::out_of_memory()
    let err = panic_res.err().unwrap();
    assert!(err.is::<String>());
    assert_eq!(err.downcast_ref::<String>().unwrap(), &"Out of memory with MmapOutOfMemory!");
}
