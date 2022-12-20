use mmtk::util::malloc;
use crate::GHCVM;

#[test]
fn test_malloc() {
    let (address1, bool1) = malloc::alloc::<GHCVM>(16, 8, 0);
    let (address2, bool2) = malloc::alloc::<GHCVM>(16, 32, 0);
    let (address3, bool3) = malloc::alloc::<GHCVM>(16, 8, 4);
    let (address4, bool4) = malloc::alloc::<GHCVM>(32, 64, 4);

    assert!(address1.is_aligned_to(8));
    assert!(address2.is_aligned_to(32));
    assert!((address3 + 4 as isize).is_aligned_to(8));
    assert!((address4 + 4 as isize).is_aligned_to(64));

    assert!(!bool1);
    #[cfg(feature = "malloc_hoard")]
    assert!(bool2);
    #[cfg(not(feature = "malloc_hoard"))]
    assert!(!bool2);
    assert!(bool3);
    assert!(bool4);

    assert!(malloc::get_malloc_usable_size(address1, bool1) >= 16);
    assert!(malloc::get_malloc_usable_size(address2, bool2) >= 16);
    assert!(malloc::get_malloc_usable_size(address3, bool3) >= 16);
    assert!(malloc::get_malloc_usable_size(address4, bool4) >= 32);

    unsafe { malloc::free(address1.to_mut_ptr()); }
    #[cfg(feature = "malloc_hoard")]
    malloc::offset_free(address2);
    #[cfg(not(feature = "malloc_hoard"))]
    unsafe { malloc::free(address2.to_mut_ptr()); }
    malloc::offset_free(address3);
    malloc::offset_free(address4);
}
