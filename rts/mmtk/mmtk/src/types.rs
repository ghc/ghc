// use crate::DummyVM;
// use super::stg_closures::*;
use super::stg_info_table::*;


pub type StgWord = usize;
pub type StgPtr = *mut StgWord;

pub type StgHalfWord = u32; // TODO: change this size later
pub type StgWord64 = u64;
pub type StgInt64 = i64;
pub type StgWord32 = u32;
pub type StgWord16 = u16;
pub type StgWord8 = u8;
pub type StgInt = i64;
pub type StgHalfInt = i32;


// ------------ ClosureTypes.h ------------
#[repr(u32)]
#[derive(PartialEq, Eq, Debug, Copy, Clone)] // comparison traits
#[allow(non_camel_case_types)]
pub enum StgClosureType {
    INVALID_OBJECT                    =  0,
    CONSTR                            =  1,
    CONSTR_1_0                        =  2,
    CONSTR_0_1                        =  3,
    CONSTR_2_0                        =  4,
    CONSTR_1_1                        =  5,
    CONSTR_0_2                        =  6,
    CONSTR_NOCAF                      =  7,
    FUN                               =  8,
    FUN_1_0                           =  9,
    FUN_0_1                           = 10,
    FUN_2_0                           = 11,
    FUN_1_1                           = 12,
    FUN_0_2                           = 13,
    FUN_STATIC                        = 14,
    THUNK                             = 15,
    THUNK_1_0                         = 16,
    THUNK_0_1                         = 17,
    THUNK_2_0                         = 18,
    THUNK_1_1                         = 19,
    THUNK_0_2                         = 20,
    THUNK_STATIC                      = 21,
    THUNK_SELECTOR                    = 22,
    BCO                               = 23,
    AP                                = 24,
    PAP                               = 25,
    AP_STACK                          = 26,
    IND                               = 27,
    IND_STATIC                        = 28,
    // stack frames
    RET_BCO                           = 29,
    RET_SMALL                         = 30,
    RET_BIG                           = 31,
    RET_FUN                           = 32,
    UPDATE_FRAME                      = 33,
    CATCH_FRAME                       = 34,
    UNDERFLOW_FRAME                   = 35,
    STOP_FRAME                        = 36,
    // end of stack frames
    BLOCKING_QUEUE                    = 37,
    BLACKHOLE                         = 38,
    MVAR_CLEAN                        = 39,
    MVAR_DIRTY                        = 40,
    TVAR                              = 41,
    ARR_WORDS                         = 42,
    MUT_ARR_PTRS_CLEAN                = 43,
    MUT_ARR_PTRS_DIRTY                = 44,
    MUT_ARR_PTRS_FROZEN_DIRTY         = 45,
    MUT_ARR_PTRS_FROZEN_CLEAN         = 46,
    MUT_VAR_CLEAN                     = 47,
    MUT_VAR_DIRTY                     = 48,
    WEAK                              = 49,
    PRIM                              = 50,
    MUT_PRIM                          = 51,
    TSO                               = 52,
    STACK                             = 53,
    TREC_CHUNK                        = 54,
    ATOMICALLY_FRAME                  = 55,
    CATCH_RETRY_FRAME                 = 56,
    CATCH_STM_FRAME                   = 57,
    WHITEHOLE                         = 58,
    SMALL_MUT_ARR_PTRS_CLEAN          = 59,
    SMALL_MUT_ARR_PTRS_DIRTY          = 60,
    SMALL_MUT_ARR_PTRS_FROZEN_DIRTY   = 61,
    SMALL_MUT_ARR_PTRS_FROZEN_CLEAN   = 62,
    COMPACT_NFDATA                    = 63,
    N_CLOSURE_TYPES                   = 64,
}

// ------------ FunTypes.h ------------
extern "C" {
    static stg_arg_bitmaps : [usize; 29];
}

// pub struct StgFunType (StgHalfWord);
#[repr(u32)]
#[derive(PartialEq, Eq, Debug, Copy, Clone)]
#[allow(non_camel_case_types)]
pub enum StgFunType {
    ARG_GEN     =  0,
    ARG_GEN_BIG =  1,
    ARG_BCO     =  2,
    ARG_NONE    =  3,
    ARG_N       =  4,
    ARG_P       =  5,
    ARG_F       =  6,
    ARG_D       =  7,
    ARG_L       =  8,
    ARG_V16     =  9,
    ARG_V32      = 10,
    ARG_V64      = 11,
    ARG_NN       = 12,
    ARG_NP       = 13,
    ARG_PN       = 14,
    ARG_PP       = 15,
    ARG_NNN      = 16,
    ARG_NNP      = 17,
    ARG_NPN      = 18,
    ARG_NPP      = 19,
    ARG_PNN      = 20,
    ARG_PNP      = 21,
    ARG_PPN      = 22,
    ARG_PPP      = 23,
    ARG_PPPP     = 24,
    ARG_PPPPP    = 25,
    ARG_PPPPPP   = 26,
    ARG_PPPPPPP  = 27,
    ARG_PPPPPPPP = 28,
}

impl StgFunType {
    pub fn get_small_bitmap(&self) -> StgSmallBitmap {
        unsafe {
            // index: take the value from the ref
            StgSmallBitmap(stg_arg_bitmaps[*self as usize])
        }
    }
}
// ------------ TSO related constants from: rts/include/rts/Constants.h ------------

/*
 * Constants for the what_next field of a TSO, which indicates how it
 * is to be run.
 */

#[derive(Eq, Debug)]
pub struct StgTSONext (StgWord16);
impl StgTSONext {
    pub const THREAD_RUN_GHC : StgTSONext = StgTSONext(1);       /* return to address on top of stack */
    pub const THREAD_INTERPRET : StgTSONext = StgTSONext(2);       /* interpret this thread */
    pub const THREAD_KILLED : StgTSONext = StgTSONext(3);       /* thread has died, don't run it */
    pub const THREAD_COMPLETE : StgTSONext = StgTSONext(4);       /* thread has finished */
}

impl PartialEq for StgTSONext {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

/*
 * Constants for the why_blocked field of a TSO
 */

#[derive(Eq, Debug)] 
pub struct StgTSOBlocked (StgWord16);
impl StgTSOBlocked {
    pub const NOT_BLOCKED : StgTSOBlocked = StgTSOBlocked(0);
    pub const BLOCKED_ON_MVAR : StgTSOBlocked = StgTSOBlocked(1);
    pub const BLOCKED_ON_MVAR_READ : StgTSOBlocked = StgTSOBlocked(14);
    pub const BLOCKED_ON_BLACK_HOLE : StgTSOBlocked = StgTSOBlocked(2);
    pub const BLOCKED_ON_READ : StgTSOBlocked = StgTSOBlocked(3);
    pub const BLOCKED_ON_WRITE : StgTSOBlocked = StgTSOBlocked(4);
    pub const BLOCKED_ON_DELAY : StgTSOBlocked = StgTSOBlocked(5);
    pub const BLOCKED_ON_STM : StgTSOBlocked = StgTSOBlocked(6);

    /* Win32 only: */
    pub const BLOCKED_ON_DO_PROC : StgTSOBlocked = StgTSOBlocked(7);
    /* Only relevant for THREADED_RTS: */
    pub const BLOCKED_ON_CCALL : StgTSOBlocked = StgTSOBlocked(10);
    pub const BLOCKED_ON_CCALL_INTERRUPTIBLE : StgTSOBlocked = StgTSOBlocked(11);

    pub const BLOCKED_ON_MSG_THROW_TO : StgTSOBlocked = StgTSOBlocked(12);
    pub const THREAD_MIGRATING : StgTSOBlocked = StgTSOBlocked(13);
}

impl PartialEq for StgTSOBlocked {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

/*
 * Flags for the tso->flags field.
 */

#[derive(Eq, Debug)] 
pub struct StgTSOFlag (StgWord32);

impl StgTSOFlag {
    pub const TSO_LOCKED : StgTSOFlag = StgTSOFlag(2);
    pub const TSO_BLOCKEX : StgTSOFlag = StgTSOFlag(4);
    pub const TSO_INTERRUPTIBLE : StgTSOFlag = StgTSOFlag(8);
    pub const TSO_STOPPED_ON_BREAKPOINT : StgTSOFlag = StgTSOFlag(16);
    pub const TSO_MARKED : StgTSOFlag = StgTSOFlag(64);
    pub const TSO_SQUEEZED : StgTSOFlag = StgTSOFlag(128);
    pub const TSO_ALLOC_LIMIT : StgTSOFlag = StgTSOFlag(256);
}

impl PartialEq for StgTSOFlag {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}