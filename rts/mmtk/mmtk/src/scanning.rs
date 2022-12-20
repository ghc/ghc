use crate::GHCVM;
use mmtk::util::opaque_pointer::*;
use mmtk::vm::{EdgeVisitor, Scanning, RootsWorkFactory};
use mmtk::util::{ObjectReference};
use mmtk::Mutator;
use crate::stg_closures::*;
use crate::stg_info_table::*;
use crate::object_scanning::*;
use crate::ghc::*;
use crate::edges::GHCEdge;

pub struct VMScanning {}

impl Scanning<GHCVM> for VMScanning {

    /// Scan all capabilities' run queues.
    fn scan_thread_roots(_tls: VMWorkerThread, mut factory: impl RootsWorkFactory<GHCEdge>) {
        let mut roots: Vec<GHCEdge> = vec![];
        for mut cap in iter_capabilities() {
            push_root(&mut roots, &mut cap.run_queue_hd);
            push_root(&mut roots, &mut cap.run_queue_tl);
            push_root(&mut roots, &mut (cap.inbox as *mut Message));
            let mut incall = cap.suspended_ccalls;
            while incall != std::ptr::null_mut() {
                push_root(&mut roots, unsafe { &mut (*incall).suspended_tso });
                incall = unsafe{ (*incall).next };
            }
        }
        // TODO: traverseSparkQueue
        factory.create_process_edge_roots_work(roots);
    }

    fn scan_thread_root(
        _tls: VMWorkerThread,
        _mutator: &'static mut Mutator<GHCVM>,
        _factory: impl RootsWorkFactory<GHCEdge>,
    ) {
        unimplemented!()
    }

    /// Treate static objects as GC roots
    fn scan_vm_specific_roots(
        _tls: VMWorkerThread,
        mut factory: impl RootsWorkFactory<GHCEdge>,
    ) {
        unsafe {
            let mut roots = get_stable_ptr_table_roots();

            let edge = IsClosureRef::to_tagged_closure_ref(&mut global_TSOs);
            roots.push(GHCEdge::ClosureRef(edge));

            factory.create_process_edge_roots_work(roots);
        }
        // markWeakPtrList
        // todo: markCAFs (not handling ghci atm)
        // todo: gcStableNameTable
    }

    fn scan_object<EV: EdgeVisitor<GHCEdge>>(
        _tls: VMWorkerThread,
        obj: ObjectReference,
        ev: &mut EV,
    )
    {
        let closure_ref = TaggedClosureRef::from_object_reference(obj);            
        visit_closure(closure_ref, ev);
    }


    fn notify_initial_thread_scan_complete(_partial_scan: bool, _tls: VMWorkerThread) {
    }
    fn supports_return_barrier() -> bool {
        unimplemented!()
    }
    fn prepare_for_roots_re_scanning() {
        unimplemented!()
    }
}

/// Visit the pointers inside a closure, depending on its closure type
/// See rts/sm/Scav.c:scavenge_one()
pub fn visit_closure<EV : EdgeVisitor<GHCEdge>>(closure_ref: TaggedClosureRef, ev: &mut EV) {
    let itbl: &'static StgInfoTable = closure_ref.get_info_table();

    match  closure_ref.to_closure() {
        Closure::MVar(mvar) => {
            visit(ev, &mut mvar.head);
            visit(ev, &mut mvar.tail);
            visit(ev, &mut mvar.value);
        }
        Closure::TVar(tvar) => {
            visit(ev, &mut tvar.current_value);
            visit(ev, &mut tvar.first_watch_queue_entry);
        }
        Closure::Thunk(thunk) => unsafe {
            let n_ptrs : u32 = itbl.layout.payload.ptrs;
            scan_closure_payload(&thunk.payload, n_ptrs, ev);
        }
        Closure::Constr(closure) => unsafe {
            let n_ptrs = itbl.layout.payload.ptrs;
            scan_closure_payload(&closure.payload, n_ptrs, ev);
        }
        Closure::Fun(fun) => unsafe {
            let n_ptrs = itbl.layout.payload.ptrs;
            scan_closure_payload(&fun.payload, n_ptrs, ev);
        }
        Closure::Weak(weak) => {
            visit(ev, &mut weak.value);
            visit(ev, &mut weak.key);
            visit(ev, &mut weak.finalizer);
            visit(ev, &mut weak.cfinalizers);
        }
        Closure::MutVar(mut_var) => {
            visit(ev, &mut mut_var.var);
        }
        Closure::BlockingQueue(bq) => {
            visit(ev, &mut bq.bh);
            visit(ev, &mut bq.owner);
            visit(ev, &mut bq.queue);
            visit(ev, &mut bq.link);
        }
        Closure::ThunkSelector(selector) => {
            visit(ev, &mut selector.selectee);
        }
        Closure::ApStack(ap) => unsafe {
            visit(ev, &mut ap.fun);
            scan_stack(ap.iter(), ev);
        }
        Closure::PartialAP(pap) => {
            visit(ev, &mut pap.fun);
            let size : usize = pap.n_args as usize;
            let fun_info : & StgFunInfoTable = 
                StgFunInfoTable::from_info_table(pap.fun.get_info_table());
            let payload : &ClosurePayload = &pap.payload;
            scan_PAP_payload(fun_info, payload, size, ev);
        }
        Closure::AP(ap) => {
            visit(ev, &mut ap.fun);
            let size : usize = ap.n_args as usize;
            let fun_info : & StgFunInfoTable = 
                StgFunInfoTable::from_info_table(ap.fun.get_info_table());
            let payload : &ClosurePayload = &ap.payload;
            scan_PAP_payload(fun_info, payload, size, ev);
        }
        // ARR_WORDS
        Closure::ArrBytes(_) => { return; }
        Closure::ArrMutPtr(array) => unsafe {
            scan_mut_arr_ptrs(array, ev);
        }
        Closure::ArrMutPtrSmall(array) => {
            scan_closure_payload(&array.payload, array.ptrs as u32, ev)
        }
        Closure::TSO(tso) => {
            scan_TSO(tso, ev);
        }
        Closure::Stack(stack) => {
            scan_stack(stack.iter(), ev);
        }
        Closure::TRecChunk(trec_chunk) => {
            visit(ev, &mut trec_chunk.prev_chunk);
            // visit payload
            let n_ptrs = trec_chunk.next_entry_idx;
            for n in 0..n_ptrs {
                let trec_entry = &mut trec_chunk.entries[n];
                visit(ev, &mut trec_entry.tvar);
                visit(ev, &mut trec_entry.expected_value);
                visit(ev, &mut trec_entry.new_value);
            }
        }
        Closure::Indirect(ind) => {
            visit(ev, &mut ind.indirectee);
        }
        // scan static: see rts/sm/Scav.c:scavenge_static
        Closure::IndirectStatic(ind) => {
            visit(ev, &mut ind.indirectee);
        }
        Closure::ThunkStatic(_) => {
            let thunk_info = StgThunkInfoTable::from_info_table(itbl);
            scan_srt_thunk(thunk_info, ev);
        }
        Closure::FunStatic(fun) => unsafe {
            let fun_info = StgFunInfoTable::from_info_table(itbl);
            scan_srt_fun(fun_info, ev);
            let n_ptrs = itbl.layout.payload.ptrs;
            scan_closure_payload(&fun.payload, n_ptrs, ev);
        }
        // TODO: scavenge_compact for COMPACT_NFDATA
        _ => panic!("scavenge_one: strange object type={:?}, address={:?}", 
                    itbl.type_, itbl)                
    }
}
