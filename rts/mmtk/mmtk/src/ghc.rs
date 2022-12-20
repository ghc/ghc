use mmtk::util::opaque_pointer::*;
use mmtk::Mutator;
use crate::GHCVM;
use crate::types::{StgPtr, StgWord16};
use crate::stg_closures::{StgTSO, TaggedClosureRef, StgClosure};

pub use binding::Task;

mod binding {
    #![allow(dead_code)]
    #![allow(non_upper_case_globals)]
    #![allow(non_camel_case_types)]
    #![allow(non_snake_case)]

    use crate::stg_closures::{TaggedClosureRef, StgTSO};
    use libc::c_void;

    type StgTSO_ = StgTSO;

    extern "C" {
        pub fn markCapabilities(
            f: unsafe extern "C" fn(*const c_void, *const TaggedClosureRef) -> (),
            ctx: *const c_void
        );
    }

    include!(concat!(env!("OUT_DIR"), "/bindings.rs"));
}

extern "C" {
    pub fn closure_sizeW (p : *const StgClosure) -> u32;
    pub fn upcall_get_mutator(tls: VMMutatorThread) -> *mut Mutator<GHCVM>;
    pub fn upcall_is_task(tls: VMThread) -> bool;
    pub static closure_flags : *const StgWord16;
    pub static all_tasks: *const Task;
    pub static SPT_size: u32;
    pub static stg_END_TSO_QUEUE_closure: StgTSO;
    pub static n_capabilities: u32;
    pub static mut global_TSOs: *mut StgTSO;
    pub static mut stable_ptr_table: *mut spEntry;
}

pub fn markCapabilities<F: Fn(*const TaggedClosureRef)>(f: F) {
    use libc::c_void;
    unsafe extern "C" fn wrapper<F: Fn(*const TaggedClosureRef)>(
        ctx: *const c_void,
        value: *const TaggedClosureRef
    ) {
        (*(ctx as *const F))(value);
    }
    unsafe {
        binding::markCapabilities(wrapper::<F>, &f as *const F as *const c_void);
    }
}


#[repr(C)]
pub struct Capability (*mut binding::Capability);

impl Capability {
    pub fn iter_run_queue(&self) -> TSOIter {
        TSOIter(self.run_queue_hd)
    }
}

impl std::ops::Deref for Capability {
    type Target = binding::Capability;
    fn deref(&self) -> &binding::Capability {
        unsafe { &(*self.0) }
    }
}

impl std::ops::DerefMut for Capability {
    fn deref_mut(&mut self) -> &mut binding::Capability {
        unsafe { &mut (*self.0) }
    }
}

#[repr(C)]
pub struct spEntry {
    pub addr : StgPtr
}

/// An iterator over a linked-list of TSOs (via the `link` field).
#[repr(C)]
pub struct TSOIter (*mut StgTSO);

impl Iterator for TSOIter {
    type Item = &'static mut StgTSO;
    fn next(&mut self) -> Option<&'static mut StgTSO> {
        unsafe {
            if self.0 as *const StgTSO == &stg_END_TSO_QUEUE_closure {
                None
            } else if (*self.0).link as *const StgTSO == &stg_END_TSO_QUEUE_closure {
                None
            } else {
                self.0 = (*self.0).link;
                Some(&mut *self.0)
            }
        }
    }
}

/// This must only be used during a stop-the-world period, when the capability count is known to be
/// fixed.
pub fn iter_capabilities() -> impl Iterator<Item=Capability> {
    unsafe {
        binding::capabilities.iter()
            .take(binding::n_capabilities.try_into().unwrap())
            .map(|x| Capability(*x))
    }
}


// TODO: need to consider when table is enlarged
pub fn iter_stable_ptr_table() -> impl Iterator<Item=&'static spEntry> {
    unsafe {
        let tables: &[spEntry] = std::slice::from_raw_parts(stable_ptr_table, SPT_size as usize);
        tables.iter().map(|x| &*x)
    }
}

pub fn vm_mutator_thread_to_task(mutator: mmtk::util::VMMutatorThread) -> *const Task {
    let optr: mmtk::util::opaque_pointer::OpaquePointer = mutator.0.0;
    // TODO: mmtk should allow unsafe inspection of OpaquePointer's payload
    unsafe { std::mem::transmute(optr) }
}
