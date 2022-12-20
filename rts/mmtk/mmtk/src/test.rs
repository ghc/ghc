use mmtk::vm::{EdgeVisitor};
use crate::stg_closures::*;
use crate::scanning::*;
use crate::types::*;
use crate::edges::GHCEdge;


use std::vec::*;

#[no_mangle]
pub unsafe extern "C" fn print_obj(obj : TaggedClosureRef){  
    let closure = Closure::from_ptr(obj.to_ptr());
    println!("obj in address {:?}:", obj.to_ptr());
    println!("{:?}", closure);

    // TODO: not working
    // match closure {
    //     Closure::Constr(_) => {
    //         println!("closure={:?}, {:?}", closure, 
    //                 StgConInfoTable::from_info_table(obj.get_info_table()).con_desc());
    //     }
    //     _ => {
    //         println!("{:?}", closure);
    //     }
    // }
}

struct CollectPointerVisitor {
    pub pointers : Vec<TaggedClosureRef>,   
}

impl EdgeVisitor<GHCEdge> for CollectPointerVisitor {
    fn visit_edge(&mut self, _edge: GHCEdge) {
        // TODO: redo type match
        // self.pointers.push(TaggedClosureRef::from_address(edge.load()));
    }
}

impl CollectPointerVisitor {
    fn new() -> Self {
        CollectPointerVisitor{pointers : Vec::new()}
    }
}


extern "C" {
    fn heap_view_closureSize(closure: *const StgClosure) -> usize;
    fn collect_pointers(closure: *const StgClosure, pointers: *mut *const StgClosure) -> usize;
}

#[no_mangle]
/*
pub unsafe extern "C" fn rs_collect_pointers(obj : TaggedClosureRef) { 
    // keep a common set to iterate through all closures
    // recursively visit visitor.pointers
    // 1. set of obj to visit
    // 2. set of obj visited

    // Rust version of tracing all the pointers
    // println!("Start tracing pointers using Rust heap model...");
    let mut visited = Vec::new();
    let mut to_visit = Vec::new();
    to_visit.push(obj);

    let mut visitor = CollectPointerVisitor::new();
    while !to_visit.is_empty() {
        let x = to_visit.pop().expect("visitor empty but still poping element...");
        // println!("visiting this object {:?} in Rust", x.to_ptr());
        if !visited.contains(&x) {
            visit_closure(x, &mut visitor); // dereferencing the borrow?
            to_visit.append(&mut visitor.pointers); // this should clear visitor.pointers
            visited.push(x);
        }
    }

    println!();
    // C version of tracing all the pointers
    // println!("Start tracing pointers using C heap model...");
    let mut visited_c = Vec::new();
    let mut to_visit_c = Vec::new();
    to_visit_c.push(obj.to_ptr());
    
    while !to_visit_c.is_empty() {
        let mut x = to_visit_c.pop().expect("visitor empty but still poping element...");
        x = TaggedClosureRef::from_ptr(x as *mut StgClosure).to_ptr();
        if !visited_c.contains(&x) {
            // println!("visiting this object {:?} in C", x);
            let mut _to_visit : Vec<*const StgClosure> = Vec::with_capacity(heap_view_closureSize(x));
            let _n = collect_pointers(x, _to_visit.as_mut_ptr());

            _to_visit.set_len(_n); // update the length of the vector after visiting
            to_visit_c.append(&mut _to_visit); // this should clear _to_visit
            visited_c.push(x);
        }
    }

    // comparing
    // println!("\nFinish visiting all the pointers, comparing the two result...");

    assert_eq!(visited.len(), visited_c.len(), "Two vector not the same length");

    for (i, j) in visited.into_iter().zip(visited_c.into_iter()) {
        // print_obj(i);
        // print_obj(TaggedClosureRef::from_ptr(j as *mut StgClosure));
        // println!();
        assert_eq!(i.to_ptr(), TaggedClosureRef::from_ptr(j as *mut StgClosure).to_ptr(), 
        "Pointers not equal to each other {:?}, {:?}", i, j);
    }
}
*/

pub unsafe extern "C" fn rs_collect_pointers(obj : TaggedClosureRef) { 
    let mut visited : Vec<TaggedClosureRef> = Vec::new();
    let mut to_visit : Vec<TaggedClosureRef> = Vec::new();
    
    // let mut visited_c : Vec<*const StgClosure> = Vec::new();
    // let mut to_visit_c : Vec<*const StgClosure> = Vec::new();

    to_visit.push(obj);
    while !to_visit.is_empty() {
        let x = to_visit.pop().expect("visitor empty but still poping element...");
        if visited.contains(&x) {
            continue;
        }

        // visit with Rust implementation
        let mut visitor = CollectPointerVisitor::new();
        visit_closure(x, &mut visitor);
        let mut rust_ptrs = visitor.pointers;

        println!("{:?}, {:?}", x.to_ptr(), x.get_info_table().type_);

        // visit with C implementation
        // let x_ptr = TaggedClosureRef::from_ptr(x as *mut StgClosure).to_ptr();
        let mut c_ptrs : Vec<*const StgClosure> = Vec::with_capacity(heap_view_closureSize(x.to_ptr()));
        let _n = collect_pointers(x.to_ptr(), c_ptrs.as_mut_ptr());
        c_ptrs.set_len(_n); // update the length of the vector after visiting

        rust_ptrs.sort();
        let rust_ptrs_new : Vec<*const StgClosure> = rust_ptrs.iter().map(|x| x.to_tagged_ptr()).collect();
        c_ptrs.sort();
        let c_ptrs_new : Vec<*const StgClosure> = c_ptrs.iter().map(|x| TaggedClosureRef::from_ptr(*x as *mut StgClosure).to_ptr()).collect();

        // TODO: have a white list to skip some closure types
        // 1. stack
        match x.get_info_table().type_ {
            StgClosureType::STACK | StgClosureType::TVAR => {
                continue;
            }
            _ => ()
        }
        
        // check that results match
        // assert_eq!(rust_ptrs.len(), c_ptrs.len(), "Two vector not the same length");
        assert_eq!(rust_ptrs_new, c_ptrs_new, "Rust pointers and C pointers not matching");

        // for (i, j) in rust_ptrs.iter().zip(c_ptrs.into_iter()) { // into_iter will consume the array; iter gives a reference of elements
        //     // print_obj(i);
        //     // print_obj(TaggedClosureRef::from_ptr(j as *mut StgClosure));
        //     // println!();
        //     assert_eq!(i.to_ptr(), TaggedClosureRef::from_ptr(j as *mut StgClosure).to_ptr(), 
        //     "Pointers not equal to each other {:?}, {:?}", i, j);
        // }
        
        visited.push(x);
        to_visit.append(&mut rust_ptrs);
    }
}