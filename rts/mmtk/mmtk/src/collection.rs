use std::sync::atomic::{AtomicBool, Ordering};
use mmtk::vm::Collection;
use mmtk::vm::GCThreadContext;
use mmtk::MutatorContext;
use mmtk::util::opaque_pointer::*;
use mmtk::scheduler::*;
use mmtk::Mutator;
use crate::ghc::{Capability, n_capabilities, Task, vm_mutator_thread_to_task};
use crate::GHCVM;

pub struct VMCollection {}

static mut MMTK_GC_PENDING: AtomicBool = AtomicBool::new(false);

extern "C" {
  fn getMyTask() -> *const Task;
  fn stopAllCapabilitiesForMMTK(task: *const Task);
  fn releaseAllCapabilities(n_capabilities: u32, cap: *const Capability, task: *const Task);
  fn yieldCapabilityForMMTK(task: *const Task, did_gc_last: bool);
  fn upcall_spawn_gc_controller(controller: *mut GCController<GHCVM>);
  fn upcall_spawn_gc_worker(worker: *mut GCWorker<GHCVM>);
}

impl Collection<GHCVM> for VMCollection {
    fn stop_all_mutators<F: FnMut(&'static mut Mutator<GHCVM>)>(_tls: VMWorkerThread, _mutator_visitor: F) {
        unsafe {
            let task = getMyTask();
            stopAllCapabilitiesForMMTK(task);
        }
    }

    fn resume_mutators(_tls: VMWorkerThread) {
        unsafe {
            let task = getMyTask();
            MMTK_GC_PENDING.store(false, Ordering::SeqCst);
            let no_capability = 0 as *const Capability;
            releaseAllCapabilities(n_capabilities, no_capability, task);
        }
    }

    fn block_for_gc(tls: VMMutatorThread) {
        unsafe {
            MMTK_GC_PENDING.store(true, Ordering::SeqCst);
            let task = vm_mutator_thread_to_task(tls);
            while MMTK_GC_PENDING.load(Ordering::SeqCst) {
                yieldCapabilityForMMTK(task, false);
            }
        }
    }

    fn spawn_gc_thread(_tls: VMThread, ctx: GCThreadContext<GHCVM>) {
        unsafe {
            match ctx {
                GCThreadContext::Controller(controller) => upcall_spawn_gc_controller(Box::into_raw(controller)),
                GCThreadContext::Worker(worker) => upcall_spawn_gc_worker(Box::into_raw(worker)),
            }
        }
    }

    fn prepare_mutator<T: MutatorContext<GHCVM>>(_tls_w: VMWorkerThread, _tls_m: VMMutatorThread, _mutator: &T) {
    }

    fn vm_release() {
        crate::active_plan::bump_static_flag();
    }
    
    // TODO: handle schedule_finalization, process_weak_refs
}
