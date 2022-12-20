// use crate::DummyVM;
use crate::types::*;
use crate::stg_info_table::*;
use crate::util::*;
use crate::ghc::Task;
use mmtk::util::{Address, ObjectReference};
use std::mem::size_of;
use std::fmt;


// ------------ Closures.h ------------

// TODO: handle when profiling case
#[repr(C)]
#[derive(Debug)]
pub struct StgProfHeader {}

// ------------ Closure headers ------------
#[repr(C)]
#[derive(Debug)]
pub struct StgSMPThunkHeader {
    pub pad : StgWord
}

#[repr(C)]
#[derive(Debug)]
pub struct StgHeader {
    pub info_table: StgInfoTableRef,
    pub prof_header : StgProfHeader,
}

#[repr(C)]
#[derive(Debug)]
pub struct StgThunkHeader {
    pub info_table : StgInfoTableRef,
    pub prof_header : StgProfHeader,
    pub smp : StgSMPThunkHeader,
}

// ------------ payload ------------
#[repr(C)]
#[derive(Debug)]
pub struct ClosurePayload {}

// TODO: check other instances of indexing in payload
impl ClosurePayload {
    pub fn get(&self, i: usize) -> TaggedClosureRef {
        unsafe {
            let ptr: *const ClosurePayload = &*self;
            let payload: *const TaggedClosureRef = ptr.cast();
            *payload.offset(i as isize)
        }
    }

    pub fn get_ref(&self, i: usize) -> &'static mut TaggedClosureRef {
        unsafe {
            let ptr: *const ClosurePayload = &*self;
            let payload: *mut TaggedClosureRef = ptr as *mut TaggedClosureRef;
            let slot: *mut TaggedClosureRef = payload.offset(i as isize);
            &mut (*slot) as &'static mut TaggedClosureRef
        }
    }
}

// ------------ Closure types ------------
#[derive(Debug)]
pub enum Closure {
    Constr(&'static StgClosure),
    Weak(&'static mut StgWeak),

    Thunk(&'static StgThunk),
    ThunkSelector(&'static mut StgSelector),
    ThunkStatic(&'static mut StgThunkStatic),

    Fun(&'static StgClosure),
    PartialAP(&'static mut StgPAP),
    AP(&'static mut StgAP),
    ApStack(&'static mut StgAP_STACK),
    FunStatic(&'static mut StgClosure),

    Indirect(&'static mut StgInd),
    IndirectStatic(&'static mut StgIndStatic),

    BlockingQueue(&'static mut StgBlockingQueue),
    ArrBytes(&'static StgArrBytes),
    ArrMutPtr(&'static StgMutArrPtrs),
    ArrMutPtrSmall(&'static StgSmallMutArrPtrs),
    MutVar(&'static mut StgMutVar),

    Stack(&'static StgStack),

    ByteCodeObj(&'static StgBCO),
    TSOQueueMVar(&'static StgMVarTSOQueue),

    TSO(&'static mut StgTSO),

    MVar(&'static mut StgMVar),
    TVar(&'static mut StgTVar),

    TRecChunk(&'static mut StgTRecChunk),
    
    // TODO: static pointers?
}

impl Closure{
    pub unsafe fn from_ptr(p: *const StgClosure) -> Closure {
        let info: &'static StgInfoTable = &*(*p).header.info_table;
        use StgClosureType::*;
        match info.type_ {
            // what are PRIM and MUT_PRIM?
            CONSTR | CONSTR_NOCAF | CONSTR_1_0 | CONSTR_0_1 | CONSTR_2_0 | 
            CONSTR_1_1 | CONSTR_0_2 | PRIM | MUT_PRIM => {
                Closure::Constr(&*(p as *const StgClosure))
            }
            THUNK | THUNK_1_0 | THUNK_0_1 | THUNK_2_0 | THUNK_1_1 | THUNK_0_2 => {
                Closure::Thunk(&*(p as *const StgThunk))
            }
            THUNK_SELECTOR => {
                Closure::ThunkSelector(&mut *(p as *mut StgSelector))
            }
            THUNK_STATIC => {
                Closure::ThunkStatic(&mut *(p as *mut StgThunkStatic))
            }
            FUN | FUN_1_0 | FUN_0_1 | FUN_1_1 | FUN_0_2 | FUN_2_0 => {
                Closure::Fun(&*(p as *const StgClosure))
            }
            FUN_STATIC => {
                Closure::FunStatic(&mut *(p as *mut StgClosure))
            }
            PAP => {
                Closure::PartialAP(&mut *(p as *mut StgPAP))
            }
            AP => { 
                Closure::AP(&mut *(p as *mut StgAP))
            }
            AP_STACK => {
                Closure::ApStack(&mut *(p as *mut StgAP_STACK))
            }
            IND | BLACKHOLE => {
                Closure::Indirect(&mut *(p as *mut StgInd))
            }
            IND_STATIC => {
                Closure::IndirectStatic(&mut *(p as *mut StgIndStatic))
            }
            BLOCKING_QUEUE => {
                Closure::BlockingQueue(&mut *(p as *mut StgBlockingQueue))
            }
            ARR_WORDS => {
                Closure::ArrBytes(&*(p as *const StgArrBytes)) 
            }
            MUT_ARR_PTRS_CLEAN | MUT_ARR_PTRS_DIRTY |
            MUT_ARR_PTRS_FROZEN_DIRTY | MUT_ARR_PTRS_FROZEN_CLEAN => {
                Closure::ArrMutPtr(&*(p as *const StgMutArrPtrs))
            }
            SMALL_MUT_ARR_PTRS_CLEAN | SMALL_MUT_ARR_PTRS_DIRTY | 
            SMALL_MUT_ARR_PTRS_FROZEN_DIRTY | SMALL_MUT_ARR_PTRS_FROZEN_CLEAN => {
                Closure::ArrMutPtrSmall(&*(p as *const StgSmallMutArrPtrs))
            }
            MUT_VAR_CLEAN | MUT_VAR_DIRTY => {
                Closure::MutVar(&mut *(p as *mut StgMutVar)) 
            }
            STACK => {
                Closure::Stack(&*(p as *const StgStack)) 
            }
            WEAK => {
                Closure::Weak(&mut *(p as *mut StgWeak))
            }
            // TODO: BCO => {
            //     Closure::ByteCodeObj(&*(p as *const StgBCO))
            // }
            MVAR_CLEAN | MVAR_DIRTY => {
                Closure::MVar(&mut *(p as *mut StgMVar))
            }
            TVAR => {
                Closure::TVar(&mut *(p as *mut StgTVar)) 
            }
            TSO => {
                Closure::TSO(&mut *(p as *mut StgTSO)) 
            }
            TREC_CHUNK => {
                Closure::TRecChunk(&mut *(p as *mut StgTRecChunk))
            }

            _ => panic!("info={:?} address={:?}", info, info as *const StgInfoTable)
        }
    }

    pub fn to_address(&self) -> Address {
        Address::from_ptr(self)
    }
}


#[repr(C)]
#[derive(Debug)]
pub struct StgClosure {
    pub header  : StgHeader,
    pub payload : ClosurePayload,
}

#[repr(C)]
#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd)]
pub struct TaggedClosureRef (*mut StgClosure);

impl TaggedClosureRef {
    const TAG_BITS: usize = 0x7;

    // untagging from a tagged pointer
    pub fn to_ptr(&self) -> *const StgClosure {
        let masked: usize = (self.0 as usize) & !Self::TAG_BITS;
        masked as *const StgClosure
    }

    pub fn to_tagged_ptr(&self) -> *const StgClosure {
        self.0
    }

    pub fn from_object_reference(obj : ObjectReference) -> TaggedClosureRef {
        let closure: *const StgClosure = obj.to_raw_address().to_ptr();
        TaggedClosureRef(closure as *mut StgClosure)
    }

    pub fn to_object_reference(&self) -> ObjectReference {
        ObjectReference::from_raw_address(Address::from_ptr(self))
    }

    pub fn get_info_table(&self) -> &'static StgInfoTable {
        unsafe{
            &*(*self.to_ptr()).header.info_table
        }
    }
    
    pub fn to_closure(&self) -> Closure {
        unsafe { Closure::from_ptr(self.to_ptr()) }
    }

    pub fn to_address(&self) -> Address {
        Address::from_ptr(self.to_ptr())
    }

    pub fn from_address(address : Address) -> Self {
        TaggedClosureRef(address.to_mut_ptr())
    }

    pub fn from_ptr(ptr : *mut StgClosure) -> Self {
        TaggedClosureRef(ptr)
    }

    pub fn set_tag(&self, tag: usize) -> Self {
        assert!(tag & !Self::TAG_BITS == 0);
        TaggedClosureRef((self.0 as usize | tag) as *mut StgClosure)
    }

    pub fn get_tag(&self) -> usize {
        (self.0 as usize) & Self::TAG_BITS
    }
}

// Closure types: THUNK, THUNK_<X>_<Y>
#[repr(C)]
#[derive(Debug)]
pub struct StgThunk {
    pub header  : StgThunkHeader,
    pub payload : ClosurePayload,
}

// Closure types: THUNK_STATIC
#[repr(C)]
#[derive(Debug)]
pub struct StgThunkStatic {
    pub header  : StgThunkHeader,
    pub static_link : TaggedClosureRef,
}

// Closure types: THUNK_SELECTOR
#[repr(C)]
#[derive(Debug)]
pub struct StgSelector {
    pub header : StgThunkHeader,
    pub selectee : TaggedClosureRef,
}

// Closure types: PAP
#[repr(C)]
#[derive(Debug)]
pub struct StgPAP {
    pub header : StgHeader,
    pub arity : StgHalfWord,
    pub n_args : StgHalfWord,
    pub fun : TaggedClosureRef,
    pub payload : ClosurePayload,
}

// Closure types: AP
#[repr(C)]
#[derive(Debug)]
pub struct StgAP {
    pub header : StgThunkHeader,
    pub arity : StgHalfWord,
    pub n_args : StgHalfWord,
    pub fun : TaggedClosureRef,
    pub payload : ClosurePayload,
}

// Closure types: AP_STACK
#[repr(C)]
#[derive(Debug)]
pub struct StgAP_STACK {
    pub header : StgThunkHeader,
    pub size : StgWord, // number of words
    pub fun : TaggedClosureRef,
    pub payload : ClosurePayload,
}

impl StgAP_STACK {
    pub unsafe fn iter(&mut self) -> StackIterator {
        let start : *mut StgStackFrame = (&mut (self.payload) as *mut ClosurePayload).cast();
        StackIterator{
            current : start,
            end : offset_words(start, self.size as isize),
        }
    }
}

// Closure types: IND
#[repr(C)]
#[derive(Debug)]
pub struct StgInd {
    pub header : StgHeader,
    pub indirectee : TaggedClosureRef,
}

// Closure types: IND_STATIC
#[repr(C)]
#[derive(Debug)]
pub struct StgIndStatic {
    pub header : StgHeader,
    pub indirectee : TaggedClosureRef,
    pub static_link : TaggedClosureRef,
    pub saved_info_table : StgInfoTableRef,
}

// Closure types: BLOCKING_QUEUE
#[repr(C)]
#[derive(Debug)]
pub struct StgBlockingQueue {
    pub header : StgHeader,
    pub link : *mut StgBlockingQueue,
    pub bh : TaggedClosureRef,
    pub owner : *mut StgTSO, // TODO: StgTSO
    pub queue : *mut MessageBlackHole,
}

#[repr(C)]
#[derive(Debug)]
pub struct StgTSO {
    pub header : StgHeader,
    pub link : *mut StgTSO,
    pub global_link : *mut StgTSO,
    pub tso_link_prev : *mut StgTSO,
    pub tso_link_next : *mut StgTSO,
    pub stackobj : *mut StgStack,
    pub what_next : StgTSONext, // in types.rs
    pub why_blocked : StgTSOBlocked,  // in types.rs
    pub flags : StgTSOFlag,   // in types.rs
    pub block_info : StgTSOBlockInfo,
    pub id : StgThreadID, 
    pub saved_errno : StgWord32,
    pub dirty : StgWord32,
    pub bound : *mut InCall,
    pub cap : *mut Capability,
    pub trec : *mut StgTRecHeader,
    pub blocked_exceptions : *mut MessageThrowTo,
    pub blocking_queue : *mut StgBlockingQueue,
    pub alloc_limit : StgInt64,
    pub tot_stack_size : StgWord32,

    // TODO: handle TICKY_TICKY, PROFILING, mingw32_HOST_OS
}

#[repr(C)]
#[derive(Debug)]
pub struct StgThreadID(StgWord64);

// TODO: here are some dummy structs to complete fields in TSO
#[repr(C)]
#[derive(Debug)]
pub struct InCall {
    pub tso : *mut StgTSO,
    pub suspended_tso : *mut StgTSO,
    pub suspended_cap : *mut Capability,
    pub rstat : StgInt, // TODO
    pub ret : *mut TaggedClosureRef,
    pub task : *mut Task,
    pub prev_stack : *mut InCall,
    pub prev : *mut InCall,
    pub next : *mut InCall,
}

#[repr(C)]
// #[derive(Debug)]
pub union StgTSOBlockInfo {
    pub closure : TaggedClosureRef,
    pub prev : *mut StgTSO,
    pub black_hole : *mut MessageBlackHole,
    pub throw_to : *mut MessageThrowTo,
    pub wake_up : *mut MessageWakeup,
    pub fd : StgInt,
    pub target : StgWord,
    // TODO: THREADED_RTS
}

impl fmt::Debug for StgTSOBlockInfo {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        unsafe {
            write!(f, "StgTsoBlockInfo({:?})", self.fd)
        }
    }
}

#[repr(C)]
pub struct Capability {}


// Closure types: ARR_WORDS
// an array of bytes -- a buffer of memory
#[repr(C)]
#[derive(Debug)]
pub struct StgArrBytes {
    pub header : StgHeader,
    pub bytes : StgWord, // number of bytes in payload
    // pub payload : *mut StgWord, // Why is it StgWord here not StgClosure?
}

// Closure types: MUT_ARR_PTRS_CLEAN, MUT_ARR_PTRS_DIRTY,
// MUT_ARR_PTRS_FROZEN_DIRTY, MUT_ARR_PTRS_FROZEN_CLEAN, MUT_VAR_CLEAN,
// MUT_VAR_DIRTY
#[repr(C)]
#[derive(Debug)]
pub struct StgMutArrPtrs {
    pub header : StgHeader,
    pub n_ptrs : StgWord,
    pub size : StgWord,
    pub payload : ClosurePayload,
}

// Closure types: SMALL_MUT_ARR_PTRS_CLEAN, SMALL_MUT_ARR_PTRS_DIRTY,
// SMALL_MUT_ARR_PTRS_FROZEN_DIRTY, SMALL_MUT_ARR_PTRS_FROZEN_CLEAN,
#[repr(C)]
#[derive(Debug)]
pub struct StgSmallMutArrPtrs {
    pub header : StgHeader,
    pub ptrs : StgWord,
    pub payload : ClosurePayload,
}

// Closure types: MUT_VAR_CLEAN, MUT_VAR_DIRTY
#[repr(C)]
#[derive(Debug)]
pub struct StgMutVar {
    pub header : StgHeader,
    pub var : TaggedClosureRef,
}

#[repr(C)]
#[derive(Debug)]
pub struct StgStack {
    pub header : StgHeader,
    pub stack_size : StgWord32, // number of words
    pub dirty : StgWord8,
    pub marking : StgWord8,
    pub sp : *mut StgStackFrame,
    pub payload : StgStackPayload, // stack contents - stack frames
}

impl StgStack {
    pub fn iter(&self) -> StackIterator {
        unsafe {
            let start : *const StgWord = (&self.payload as *const StgStackPayload).cast();
            StackIterator{
                current : self.sp,
                end : start.offset(self.stack_size as isize) as *mut StgStackFrame,
            }
        }
    }
}

#[repr(C)]
#[derive(Debug)]
pub struct StgStackPayload {}

pub struct StackIterator {
    current : *mut StgStackFrame, // fst word of the sf
    end : *mut StgStackFrame,
}

impl Iterator for StackIterator {
    type Item = StackFrame;
    fn next(&mut self) -> Option<StackFrame> {
        if self.current < self.end {
            // info table of the stackframe
            let itbl: &'static StgRetInfoTable = unsafe { &(*self.current).header.info_table}; 
            let current = self.current;
            use StgClosureType::*;        

            match itbl.i.type_ {
                UPDATE_FRAME => unsafe {
                    // self.current = self.current.offset(size_of::<StgUpdateFrame>() as isize);
                    self.current = offset_bytes(self.current, size_of::<StgUpdateFrame>() as isize);
                    Some(StackFrame::UPD_FRAME(&mut *current.cast()))
                }
                CATCH_STM_FRAME | CATCH_RETRY_FRAME | ATOMICALLY_FRAME | UNDERFLOW_FRAME |
                STOP_FRAME | CATCH_FRAME | RET_SMALL => unsafe {
                    let bitmap = itbl.i.layout.small_bitmap;
                    let mut size = bitmap.size() * size_of::<StgWord>(); // words
                    size += size_of::<StgStackFrame>(); // bytes
                    self.current = offset_bytes(self.current, size as isize);
                    Some(StackFrame::RET_SMALL(&mut *current.cast(), bitmap))
                }
                RET_BIG => unsafe {
                    let bitmap =  &*(itbl.i.layout.large_bitmap_ref.deref(&itbl.i));
                    let size = bitmap.size * size_of::<StgWord>() + size_of::<StgStackFrame>();
                    self.current = offset_bytes(self.current, size as isize);
                    Some(StackFrame::RET_BIG(&mut *current.cast(), bitmap))
                }
                RET_FUN => unsafe {
                    let ret_fun : &'static StgRetFunFrame = &*current.cast();
                    let fun_info : &'static StgFunInfoTable = 
                        StgFunInfoTable::from_info_table(ret_fun.fun.get_info_table());
                    use StgFunType::*;
                    match fun_info.f.fun_type {
                        ARG_GEN => {
                            // small bitmap
                            let small_bitmap : StgSmallBitmap = fun_info.f.bitmap.small_bitmap;
                            let mut size = small_bitmap.size() * size_of::<StgWord>();
                            size += size_of::<StgRetFunFrame>();
                            self.current = offset_bytes(self.current, size as isize);
                            Some(StackFrame::RET_FUN_SMALL(&mut *current.cast(), small_bitmap))
                        }
                        ARG_GEN_BIG => {
                            // large bitmap
                            let bitmap =  &*(fun_info.f.bitmap.large_bitmap_ref.deref(&itbl.i));
                            let mut size = bitmap.size * size_of::<StgWord>() + size_of::<StgStackFrame>();
                            size += size_of::<StgRetFunFrame>();
                            self.current = offset_bytes(self.current, size as isize);
                            Some(StackFrame::RET_FUN_LARGE(&mut *current.cast(), bitmap))
                        }
                        _ => {
                            // small bitmap indexed by the function type
                            let small_bitmap = StgFunType::get_small_bitmap(&fun_info.f.fun_type);
                            let mut size = small_bitmap.size() * size_of::<StgWord>();
                            size += size_of::<StgRetFunFrame>();
                            self.current = offset_bytes(self.current, size as isize);
                            Some(StackFrame::RET_FUN_SMALL(&mut *current.cast(), small_bitmap))
                        }
                    }
                }
                // TODO: add RET_BCO case
                _ => panic!("Unexpected stackframe type {:?} ", itbl.i.type_)

            }
        }
        else {
            return None;
        }
    }
}



// ------ stack frames -----------
#[repr(C)]
#[derive(Debug)]
pub struct StgStackFrameHeader {
    pub info_table : StgRetInfoTableRef,
    pub prof_header : StgProfHeader,
}

pub struct StgStackFrame {
    pub header : StgStackFrameHeader,
    pub payload : ClosurePayload,
}

#[allow(non_camel_case_types)]
#[derive(Debug)]
pub enum StackFrame {
    RET_BCO(&'static StgRetFrame),
    RET_SMALL(&'static mut StgRetFrame, StgSmallBitmap),
    RET_BIG(&'static mut StgRetFrame, &'static StgLargeBitmap),
    RET_FUN_SMALL(&'static mut StgRetFunFrame, StgSmallBitmap),
    RET_FUN_LARGE(&'static mut StgRetFunFrame,  &'static StgLargeBitmap),
    UPD_FRAME(&'static mut StgUpdateFrame),
    // CATCH_FRAME(&'static StgCatchFrame),
    // UNDERFLOW_FRAME(&'static StgUnderflowFrame),
    // STOP_FRAME(&'static StgStopFrame),
    // ATOMICALLY_FRAME(&'static StgAtomicallyFrame),
    // CATCH_RETRY_FRAME(&'static StgCatchRetryFrame),
    // CATCH_STM_FRAME(&'static StgCatchSTMFrame),
}

#[repr(C)]
#[derive(Debug)]
pub struct StgRetFrame {
    pub header : StgStackFrameHeader,
    pub payload : ClosurePayload,
}

// Closure types: UPDATE_FRAME
#[repr(C)]
#[derive(Debug)]
pub struct StgUpdateFrame {
    pub header : StgStackFrameHeader,
    pub updatee : TaggedClosureRef,
}

// Closure types: CATCH_FRAME
#[repr(C)]
#[derive(Debug)]
pub struct StgCatchFrame {
    pub header : StgStackFrameHeader,
    pub exceptions_blocked : StgWord,
    pub handler : TaggedClosureRef,
}

// impl walk through stack?

// Closure types: UNDERFLOW_FRAME
#[repr(C)]
#[derive(Debug)]
pub struct StgUnderflowFrame {
    pub info_table : StgRetInfoTableRef,
    pub next_chunk : *mut StgStack,
}

// Closure types: STOP_FRAME
#[repr(C)]
#[derive(Debug)]
pub struct StgStopFrame {
    pub header : StgStackFrameHeader,
}

#[repr(C)]
#[derive(Debug)]
pub struct StgAtomicallyFrame {
    pub header : StgStackFrameHeader,
    pub code : TaggedClosureRef,
    pub result : TaggedClosureRef,
}

#[repr(C)]
#[derive(Debug)]
pub struct StgCatchSTMFrame {
    pub header : StgStackFrameHeader,
    pub code : TaggedClosureRef,
    pub handler : TaggedClosureRef,
}

#[repr(C)]
#[derive(Debug)]
pub struct StgCatchRetryFrame {
    pub header : StgStackFrameHeader,
    pub running_alt_code : StgWord,
    pub first_code : TaggedClosureRef,
    pub alt_code : TaggedClosureRef,
}
// Closure types: RET_FUN
#[repr(C)]
#[derive(Debug)]
pub struct StgRetFunFrame{
    pub info_table : StgRetInfoTableRef,
    pub size : StgWord,
    pub fun : TaggedClosureRef,
    pub payload : ClosurePayload,
}

/// end of stack frame types

// Closure type: CONSTR_0_1
#[repr(C)]
pub struct StgIntCharlikeClosure {
    pub header : StgHeader,
    pub data : StgWord,
}

// Stable name, StableName# v
#[repr(C)]
pub struct StgStableName {
    pub header : StgHeader,
    pub sn : StgWord,
}

// Closure types: WEAK
#[repr(C)]
#[derive(Debug)]
pub struct StgWeak {
    pub header : StgHeader,
    pub cfinalizers : TaggedClosureRef,
    pub key : TaggedClosureRef,
    pub value : TaggedClosureRef,
    pub finalizer : TaggedClosureRef,
    pub link : *mut StgWeak,
}


#[repr(C)]
union FinalizerFn {
    pub without_env: *const extern "C" fn(*mut u8),
      // ^ (ptr)
    pub with_env: *const extern "C" fn(*mut u8, *mut u8)
      // ^ (eptr, ptr)
}

// Closure type: CONSTR
#[repr(C)]
pub struct StgCFinalizerList {
    header: StgHeader,
    link: TaggedClosureRef,
    finalize: FinalizerFn,
    ptr: *mut u8,
    eptr: *mut u8,
    flag: StgWord,
}

impl StgCFinalizerList {
    // example of how to use
    pub unsafe fn run(&self) {
        match self.flag {
            0 => (*self.finalize.without_env)(self.ptr),
            1 => (*self.finalize.with_env)(self.eptr, self.ptr),
            _ => panic!("oh no!")
        }
    }
}

// Closure types: BCO
#[repr(C)]
#[derive(Debug)]
pub struct StgBCO {
    pub header : StgHeader,
    pub instrs : *mut StgArrBytes,
    pub literals : *mut StgArrBytes,
    pub ptrs : *mut StgMutArrPtrs,
    pub arity : StgHalfWord,
    pub size : StgHalfWord,
    pub bitmap : StgLargeBitmap, // TODO: large bitmap ? check
}

/*
TODO: have a look at BCO functions later
impl StgBCO {
    // TODO: inline functions of StgBCO
    #[inline(always)]
    pub fn BCO_BITMAP(&self) -> *mut StgLargeBitmap {
        unimplemented!()
    }

    #[inline(always)]
    pub fn BCO_BITMAP_SIZE(&self) -> StgWord {
        unimplemented!()
    }

    #[inline(always)]
    pub fn BCO_BITMAP_SIZE(&self) -> StgLargeBitmap {
        unimplemented!()
    }

    #[inline(always)]
    pub fn BCO_BITMAP_SIZEW(&self) -> StgWord {
        unimplemented!()
    }
}
*/

// which closure type?
#[repr(C)]
#[derive(Debug)]
pub struct StgMVarTSOQueue {
    pub header : StgHeader,
    pub link : *mut StgMVarTSOQueue,
    pub tso : *mut StgTSO, // TODO: define TSO
}

// Closure types: MVAR_CLEAN, MVAR_DIRTY
#[repr(C)]
#[derive(Debug)]
pub struct StgMVar {
    pub header : StgHeader,
    pub head : *mut StgMVarTSOQueue,
    pub tail : *mut StgMVarTSOQueue,
    pub value : TaggedClosureRef,
}

#[repr(C)]
pub struct StgTVarWatchQueue {
    pub header : StgHeader,
    pub closure : *mut StgTSO,
    pub next_queue_entry : *mut StgTVarWatchQueue,
    pub prev_queue_entry : *mut StgTVarWatchQueue,
}

#[repr(C)]
#[derive(Debug)]
pub struct StgTVar {
    pub header : StgHeader,
    pub current_value : TaggedClosureRef,
    pub first_watch_queue_entry : *mut StgTVarWatchQueue,
    pub num_updates : StgInt,
}

#[repr(C)]
#[derive(Debug)]
pub struct TRecEntry {
    pub tvar : *mut StgTVar,
    pub expected_value : TaggedClosureRef,
    pub new_value : TaggedClosureRef,
    // TODO: add num_updates when THREADED_RTS
}


const TREC_CHUNK_NUM_ENTRIES: usize = 16;

// contains many TRec entries and link them together
#[repr(C)]
#[derive(Debug)]
pub struct StgTRecChunk {
    pub header : StgHeader,
    pub prev_chunk : *mut StgTRecChunk,
    pub next_entry_idx : StgWord,
    pub entries : [TRecEntry; TREC_CHUNK_NUM_ENTRIES], 
}

// maybe don't need this
pub enum TRecState {
    TrecActive,        /* Transaction in progress, outcome undecided */
    TrecCondemned,     /* Transaction in progress, inconsistent / out of date reads */
    TrecCommitted,     /* Transaction has committed, now updating tvars */
    TrecAborted,       /* Transaction has aborted, now reverting tvars */
    TrecWaiting,       /* Transaction currently waiting */
}

#[repr(C)]
pub struct StgTRecHeader {
    pub header : StgHeader,
    pub enclosing_trec : *mut StgTRecHeader,
    pub current_chunk : *mut StgTRecChunk,
    pub state : TRecState,
}


/* ----------------------------------------------------------------------------
   Messages
   ------------------------------------------------------------------------- */

#[repr(C)]
pub struct Message {
    pub header : StgHeader,
    pub link : *mut Message,
}

#[repr(C)]
pub struct MessageWakeup {
    pub header : StgHeader,
    pub link : *mut Message,
    pub tso : *mut StgTSO,
}

#[repr(C)]
pub struct MessageThrowTo {
    pub header : StgHeader,
    pub link : *mut MessageThrowTo, // should be just Message ?
    pub source : *mut StgTSO,
    pub target : *mut StgTSO,
    pub exception : TaggedClosureRef,
}

#[repr(C)]
pub struct MessageBlackHole {
    pub header : StgHeader,
    pub link : *mut MessageBlackHole, // should be just Message ?
    pub tso : *mut StgTSO,
    pub bh : TaggedClosureRef,
}

#[repr(C)]
pub struct MessageCloneStack {
    pub header : StgHeader,
    pub link : *mut Message,
    pub result : *mut StgMVar,
    pub tso : *mut StgTSO,
}


/* ----------------------------------------------------------------------------
   Compact Regions
   ------------------------------------------------------------------------- */
#[repr(C)]
pub struct StgCompactNFDataBlock {
    pub self_ : *mut StgCompactNFDataBlock,
    pub owner : *mut StgCompactNFData,
    pub next : *mut StgCompactNFDataBlock,
}

#[repr(C)]
pub struct Hashtable {}

#[repr(C)]
pub struct StgCompactNFData {
    pub header : StgHeader,
    pub total_w : StgWord,
    pub auto_block_w : StgWord,
    pub hp : StgPtr,
    pub hp_lim : StgPtr,
    pub nursery : *mut StgCompactNFDataBlock,
    pub last : *mut StgCompactNFDataBlock,
    pub hash : *mut Hashtable, // TODO: define HashTable
    pub result : TaggedClosureRef,
    pub link : *mut StgCompactNFData, // maybe need to rework compact normal form
}

// TODO: test with some typical haskell objects for object scanning

pub trait IsClosureRef {
    fn to_tagged_closure_ref(ptr: *mut Self) -> *mut TaggedClosureRef {
        ptr.cast()
    }
}

impl IsClosureRef for TaggedClosureRef {
    fn to_tagged_closure_ref(ptr: *mut TaggedClosureRef) -> *mut TaggedClosureRef {
        ptr
    }
}

macro_rules! is_closure {
    ($ty: ident) => {
        impl IsClosureRef for *mut $ty {
            fn to_tagged_closure_ref(ptr: *mut Self) -> *mut TaggedClosureRef {
                ptr.cast()
            }
        }
    }
}


is_closure!(StgTSO);
is_closure!(StgClosure);
is_closure!(StgBlockingQueue);
is_closure!(StgTRecHeader);
is_closure!(StgStack);
is_closure!(StgMVarTSOQueue);
is_closure!(StgTVarWatchQueue);
is_closure!(StgTRecChunk);
is_closure!(StgTVar);
is_closure!(Message);
is_closure!(MessageThrowTo);
is_closure!(MessageBlackHole);
is_closure!(MessageWakeup);
is_closure!(StgRetInfoTable);