use mmtk::util::Address;
use mmtk::util::opaque_pointer::*;
use mmtk::util::memory;
use crate::GHCVM;

#[test]
pub fn test_handle_mmap_conflict() {
    let start = unsafe { Address::from_usize(0x100_0000 )};
    let one_megabyte = 1000000;
    let mmap1_res = memory::dzmmap_noreplace(start, one_megabyte);
    assert!(mmap1_res.is_ok());

    let panic_res = std::panic::catch_unwind(|| {
        let mmap2_res = memory::dzmmap_noreplace(start, one_megabyte);
        assert!(mmap2_res.is_err());
        memory::handle_mmap_error::<GHCVM>(mmap2_res.err().unwrap(), VMThread::UNINITIALIZED);
    });

    // The error should match the error message in memory::handle_mmap_error()
    assert!(panic_res.is_err());
    let err = panic_res.err().unwrap();
    assert!(err.is::<&str>());
    assert_eq!(err.downcast_ref::<&str>().unwrap(), &"Failed to mmap, the address is already mapped. Should MMTk quanrantine the address range first?");
}