use super::types::*;
use crate::stg_closures::*;

pub unsafe fn offset_bytes<T>(ptr: *mut T, n: isize) -> *mut T {
    ptr.cast::<u8>().offset(n).cast()
}    

pub unsafe fn offset_words<T>(ptr: *mut T, n: isize) -> *mut T {
    ptr.cast::<StgWord>().offset(n).cast()
}

/// Compute a pointer to a structure from an offset relative
/// to the end of another structure.
pub unsafe fn offset_from_end<Src, Target>(ptr: &Src, offset: isize) -> *const Target {
    let end = (ptr as *const Src).offset(1);
    (end as *const u8).offset(offset).cast()
}

#[no_mangle]
#[inline(never)]
pub fn push_node(_ptr: TaggedClosureRef) {
    ()
}