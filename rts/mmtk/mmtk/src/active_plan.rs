use mmtk::Plan;
use mmtk::vm::ActivePlan;
use mmtk::util::{opaque_pointer::*, ObjectReference, Address};
use mmtk::Mutator;
use mmtk::ObjectQueue;
use mmtk::scheduler::GCWorker;
use crate::GHCVM;
use crate::SINGLETON;
use crate::ghc::*;
use crate::stg_closures::*;
use crate::stg_info_table::*;


static mut STATIC_FLAG: bool = false;

pub fn bump_static_flag() {
    unsafe {
        STATIC_FLAG = !STATIC_FLAG;
    }
}

fn get_static_flag() -> bool {
    unsafe {
        STATIC_FLAG
    }
}

static mut ITERATOR: *const Task = std::ptr::null();

pub struct VMActivePlan<> {}

impl ActivePlan<GHCVM> for VMActivePlan {
    fn global() -> &'static dyn Plan<VM=GHCVM> {
        SINGLETON.get_plan()
    }

    fn number_of_mutators() -> usize {
        // todo: number of tasks
        unsafe {n_capabilities as usize}
    }

    fn is_mutator(tls: VMThread) -> bool {
        unsafe { upcall_is_task(tls) }
    }

    fn mutator(tls: VMMutatorThread) -> &'static mut Mutator<GHCVM> {
        unsafe { &mut *upcall_get_mutator(tls) }
    }

    fn reset_mutator_iterator() {
        unsafe {
            ITERATOR = all_tasks;
        }
    }

    fn get_next_mutator() -> Option<&'static mut Mutator<GHCVM>> {
        unsafe {
            // println!("Next Iterator {:?}", ITERATOR);
            // TODO: acquire all_tasks_mutex
            if ! ITERATOR.is_null() {
                let task = ITERATOR;
                ITERATOR = (*task).all_next;
                let result = (*task).mmutator;
                Some(std::mem::transmute(result))
            }
            else {
                None
            }
        }
    }

    fn vm_trace_object<Q: ObjectQueue>(
        queue: &mut Q,
        object: ObjectReference,
        _worker: &mut GCWorker<GHCVM>,
    ) -> ObjectReference {
        // Modelled after evacuate_static_object, returns true if this 
        // is the first time the object has been visited in this GC.
        let mut evacuate_static = |static_link: &mut TaggedClosureRef| -> bool {
            let cur_static_flag = if get_static_flag() { 2 } else { 1 };
            let prev_static_flag = if get_static_flag() { 1 } else { 2 };
            let object_visited: bool = (static_link.get_tag() | prev_static_flag) != 3;
            if !object_visited {
                // N.B. We don't need to maintain a list of static objects, therefore ZERO
                *static_link = TaggedClosureRef::from_address(Address::ZERO).set_tag(cur_static_flag);
                enqueue_roots(queue, object);
            }
            !object_visited
        };

        // Modelled after evacuate() (lines 713 through 760)
        let tagged_ref = TaggedClosureRef::from_object_reference(object);
        let info_table = tagged_ref.get_info_table();

        use crate::stg_closures::Closure::*;
        match tagged_ref.to_closure() {
            ThunkStatic(thunk) => {
                // if srt != 0
                if let Some(_) = StgThunkInfoTable::from_info_table(info_table).get_srt() {
                    let static_link_ref = &mut thunk.static_link;
                    evacuate_static(static_link_ref);
                }
            },
            FunStatic(fun) => {
                let srt =  StgFunInfoTable::from_info_table(info_table).get_srt();
                let ptrs = unsafe { info_table.layout.payload.ptrs };
                // if srt != 0 || ptrs != 0
                if srt.is_some() || (ptrs != 0) {
                    let offset = unsafe { ptrs + info_table.layout.payload.nptrs };
                    let static_link_ref = fun.payload.get_ref(offset as usize);
                    evacuate_static(static_link_ref);
                }
            },
            IndirectStatic(ind) => {
                evacuate_static(&mut ind.static_link);
            },
            Constr(constr) => {
                let offset = unsafe { info_table.layout.payload.ptrs + info_table.layout.payload.nptrs };
                let static_link_ref = constr.payload.get_ref(offset as usize);
                evacuate_static(static_link_ref);
            },
            _ => panic!("invalid static closure"),
        };
        object
    }
}


pub fn enqueue_roots<Q: ObjectQueue>(queue: &mut Q, object: ObjectReference)
{
    queue.enqueue(object);
}