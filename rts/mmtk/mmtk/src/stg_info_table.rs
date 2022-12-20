use super::types::*;
use super::stg_closures::*;
use super::util::*;
use std::fmt;
use std::ops::Deref;
use crate::ghc::closure_flags;

/**
 * GHC closure info tables in Rust
 * Original C code is at ghc/rts/include/rts/storage/InfoTables.h
 */

/* -----------------------------------------------------------------------------
   Closure flags
   -------------------------------------------------------------------------- */

#[repr(C)]
pub struct ClosureFlag (StgWord16);

impl ClosureFlag {
    const _HNF : ClosureFlag = ClosureFlag(1<<0);  /* head normal form?    */
    const _BTM : ClosureFlag = ClosureFlag(1<<1);  /* uses info->layout.bitmap */
    const _NS  : ClosureFlag = ClosureFlag(1<<2);  /* non-sparkable        */
    const _THU : ClosureFlag = ClosureFlag(1<<3);  /* thunk?               */
    const _MUT : ClosureFlag = ClosureFlag(1<<4);  /* mutable?             */
    const _UPT : ClosureFlag = ClosureFlag(1<<5);  /* unpointed?           */
    const _SRT : ClosureFlag = ClosureFlag(1<<6);  /* has an SRT?          */
    const _IND : ClosureFlag = ClosureFlag(1<<7);  /* is an indirection?   */

    #[inline(always)]
    pub fn is_mutable(&self)     -> bool {(self.0) & (Self::_MUT.0) != 0}

    #[inline(always)]
    pub fn is_bitmap(&self)      -> bool {(self.0) & (Self::_BTM.0) != 0}

    #[inline(always)]
    pub fn is_thunk(&self)       -> bool {(self.0) & (Self::_THU.0) != 0}

    #[inline(always)]
    pub fn is_unpointed(&self)   -> bool {(self.0) & (Self::_UPT.0) != 0}

    #[inline(always)]
    pub fn has_srt(&self)        -> bool {(self.0) & (Self::_SRT.0) != 0}
    

    // TODO: implement closure flags related macros
    #[inline(always)]
    pub fn get_closure_flag(_c : *const StgClosure) -> ClosureFlag {
        unimplemented!()
    }

    pub fn from_closure_type(ty: StgClosureType) -> Self {
        unsafe { ClosureFlag(*(closure_flags).offset(ty as isize)) }
    }    
}


/* -----------------------------------------------------------------------------
   Bitmaps
   -------------------------------------------------------------------------- */
#[repr(C)]
pub union Bitmap {
    pub small_bitmap       : StgSmallBitmap,
    pub large_bitmap_ref   : StgLargeBitmapRef,
}

impl std::fmt::Debug for Bitmap {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> std::fmt::Result {
        unsafe {
            write!(f, "Bitmap({:?})", self.small_bitmap)
        }
    }
}

// -------------------- small bitmap --------------------
#[repr(C)]
#[derive(Debug, Copy, Clone)]
pub struct StgSmallBitmap (pub StgWord);

impl StgSmallBitmap {
    // TODO: handle 32 bits constants
    const BITMAP_BITS_SHIFT : StgWord = 6;
    const BITMAP_SIZE_MASK  : StgWord = 0x3f;

    #[inline(always)]
    pub fn make_small_bitmap(size : StgWord, bits : StgWord) -> Self {
        StgSmallBitmap(((bits) << Self::BITMAP_BITS_SHIFT) | (size))
    }

    #[inline(always)]
    pub fn size(&self) -> StgWord {
        (self.0) & Self::BITMAP_SIZE_MASK 
    }

    #[inline(always)]
    pub fn bits(&self) -> StgWord {
        (self.0) >> Self::BITMAP_BITS_SHIFT
    }
}

// -------------------- large bitmap --------------------

#[repr(C)]
#[derive(Debug)]
pub struct StgLargeBitmap {
    pub size    : StgWord, // number of bits
    pub bitmap  : LargeBitMapPayload // similar to closure payload in stg_closures.rs
}

#[repr(C)]
#[derive(Debug)]
pub struct LargeBitMapPayload {}

impl LargeBitMapPayload {
    pub unsafe fn get_w(&self, i: usize) -> *const StgWord {
        let ptr: *const LargeBitMapPayload = &*self;
        let payload: *const *mut StgWord = ptr.cast();
        *payload.offset(i as isize)
    }
    // TODO: might want to iterate through bits as well
}

#[repr(C)]
#[derive(Debug)]
pub struct StgLargeBitmapRef {
    pub offset : StgInt
    // TODO: handle non TABLES_NEXT_TO_CODE
}

impl StgLargeBitmapRef {
    // relative to the beginning of the infotable of the closure
    pub unsafe fn deref<InfoTable>(&self, itbl: &InfoTable) -> *const StgLargeBitmap {
        // TODO: make sure itbl is an info table
        offset_from_end(itbl, self.offset as isize)
    }
}


/* ----------------------------------------------------------------------------
   Info Tables
   ------------------------------------------------------------------------- */
#[repr(C)]
pub struct StgPointerFirst {
    pub ptrs    : StgHalfWord,  /* number of pointers */
    pub nptrs   : StgHalfWord,  /* number of non-pointers */
}

#[repr(C)]
pub union StgClosureInfo {
    pub payload : StgPointerFirst,

    pub small_bitmap : StgSmallBitmap,
    
    // TODO: check if x64 is still related to OFFSET_FIELD
    // Check if hack in Note [x86-64-relative] is still necessary 
    pub large_bitmap_ref : StgLargeBitmapRef,

    pub selector_offset : StgWord,
}

impl fmt::Debug for StgClosureInfo {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "StgClosureInfo")
    }
}

/* ----------------------------------------------------------------------------
   Function info tables
   ------------------------------------------------------------------------- */

#[repr(C)]
#[derive(Debug)]
pub struct StgSRTField {
    pub srt : StgHalfInt,
    // TODO: handle non USE_INLINE_SRT_FIELD
}

#[cfg(not(profiling))]
#[repr(C)]
#[derive(Debug)]
pub struct StgProfInfo {} // TODO: handle profiling case

/// Tables-next-to-code reference.
///
/// <structure, e.g. an info table> 
/// can be used for both StgInfoTable and StgRetInfoTable
/// 
/// This is a reference to a structure which, when tables-next-to-code is enabled,
/// lives directly before code.
/// In this case &info_table is StgHeader.info - sizeof(StgInfoTable)
#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub struct TntcRef<T> (*const T);

impl<T> TntcRef<T> {
    pub fn get_ptr(&self) -> *const T {
        unsafe {
            if true || cfg!(tables_next_to_code) {
                self.0.offset(-1)
            } else {
                self.0
            }
        }
    }

    pub fn get_mut_ptr(&self) -> *mut T {
        TntcRef::get_ptr(&self) as *mut T
    }
}

impl<T> Deref for TntcRef<T> {
    type Target = T;

    fn deref(&self) -> &T {
        unsafe {
            &*self.get_ptr()
        }
    }
}

pub type StgInfoTableRef = TntcRef<StgInfoTable>;
pub type StgRetInfoTableRef = TntcRef<StgRetInfoTable>;
pub type StgFunInfoTableRef = TntcRef<StgFunInfoTable>;
pub type StgThunkInfoTableRef = TntcRef<StgThunkInfoTable>;

#[repr(C)]
#[derive(Debug)]
pub struct StgInfoTable {
    // TODO: non TABLES_NEXT_TO_CODE
    // #[cfg(not(tables_next_to_code))]
    // pub code    : *const u8, // pointer to entry code
    pub prof    : StgProfInfo,
    pub layout  : StgClosureInfo,
    pub type_   : StgClosureType,
    pub srt     : StgSRTField,
    // pub code    : *mut StgCode, (zero length array)
}

impl StgInfoTable {
    pub fn get_srt(&self) -> Option<*const StgClosure> {
        unsafe {
            if self.srt.srt != 0 {
                Some(offset_from_end(self, self.srt.srt as isize))
            }
            else { None }
        }
    }
}


#[repr(C)]
#[derive(Debug)]
pub struct StgFunInfoExtra {
    pub slow_apply  : StgInt,
    pub bitmap      : Bitmap,

    // TODO: handle offset for USE_INLINE_SRT_FIELD for srtfield

    pub fun_type    : StgFunType, // in types.rs from rts/include/rts/storage/FunTypes.h
    pub arity       : StgHalfWord,
    // TODO: handle non TABLES_NEXT_TO_CODE (StgFunInfoExtraFwd)
}

#[repr(C)]
#[derive(Debug)]
pub struct StgFunInfoTable {
    pub f : StgFunInfoExtra, // 3 words
    pub i : StgInfoTable     // 2 words
    // TODO: handle non TABLES_NEXT_TO_CODE (need to use StgFunInfoExtraFwd)
}

impl StgFunInfoTable {
    // rts/include/rts/storage/ClosureMacros.h
    pub fn from_info_table(itbl : &'static StgInfoTable) -> &'static mut StgFunInfoTable {
        unsafe {
            let itbl = itbl as *const StgInfoTable;
            &mut *(itbl.offset(1) as *mut StgFunInfoTable).offset(-1)
        }
    }

    pub fn to_info_table(itbl : &'static StgFunInfoTable) ->  &'static StgInfoTable {
        unsafe {
            let itbl = itbl as *const StgFunInfoTable;
            &*(itbl.offset(1) as *const StgInfoTable).offset(-1)
        }
    }

    pub fn get_srt(&self) -> Option<*const StgClosure> {
        unsafe {
            if self.i.srt.srt != 0 {
                Some(offset_from_end(self, self.i.srt.srt as isize))
            }
            else {
                None
            }
        }
    }
}

/* -----------------------------------------------------------------------------
   Return info tables
   -------------------------------------------------------------------------- */
#[repr(C)]
#[derive(Debug)]
pub struct StgRetInfoTable {
    // (check line 160 InfoTables.h)
    // TODO: USE_SRT_POINTER is true
    // TODO: USE_SRT_POINTER is false but USE_SRT_OFFSET is true
    pub i : StgInfoTable, // both false case
}

impl StgRetInfoTable {
    pub fn get_srt(&self) -> Option<*const StgClosure> {
        unsafe {
            if self.i.srt.srt != 0 {
                Some(offset_from_end(self, self.i.srt.srt as isize))
            }
            else {
                None
            }
        }
    }
}


/* -----------------------------------------------------------------------------
   Thunk info tables
   -------------------------------------------------------------------------- */
#[repr(C)]
pub struct StgThunkInfoTable {
    // (check line 160 InfoTables.h)
    // TODO: USE_SRT_POINTER is true
    // TODO: USE_SRT_POINTER is false but USE_SRT_OFFSET is true
    pub i : StgInfoTable, // both false case
}

impl StgThunkInfoTable {
    pub fn get_srt(&self) -> Option<*const StgClosure> {
        unsafe {
            if self.i.srt.srt != 0 {
                Some(offset_from_end(self, self.i.srt.srt as isize))
            }
            else {
                None
            }
        }
    }

    pub fn from_info_table(itbl : &'static StgInfoTable) -> &'static mut StgThunkInfoTable {
        unsafe {
            let itbl = itbl as *const StgInfoTable;
            &mut *(itbl.offset(1) as *mut StgThunkInfoTable).offset(-1)
        }
    }

    pub fn to_info_table(itbl : &'static StgThunkInfoTable) ->  &'static StgInfoTable {
        unsafe {
            let itbl = itbl as *const StgThunkInfoTable;
            &*(itbl.offset(1) as *const StgInfoTable).offset(-1)
        }
    }
}


/* -----------------------------------------------------------------------------
   Constructor info tables
   -------------------------------------------------------------------------- */
#[repr(C)]
pub struct StgConInfoTable {
    // TODO: handle non TABLES_NEXT_TO_CODE
    pub con_desc_offset : StgHalfInt,
    pub padding         : StgHalfInt,
    pub i               : StgInfoTable,
}

impl StgConInfoTable {
    pub unsafe fn con_desc(&self) -> &'static std::ffi::CStr {
        std::ffi::CStr::from_ptr(offset_from_end(self, self.con_desc_offset as isize))
    }

    pub fn from_info_table(itbl : &'static StgInfoTable) -> &'static StgConInfoTable {
        unsafe {
            let itbl = itbl as *const StgInfoTable;
            &*(itbl.offset(1) as *const StgConInfoTable).offset(-1)
        }
    }

    pub fn to_info_table(itbl : &'static StgConInfoTable) ->  &'static StgInfoTable {
        unsafe {
            let itbl = itbl as *const StgConInfoTable;
            &*(itbl.offset(1) as *const StgInfoTable).offset(-1)
        }
    }
}

// TODO: implement other macros
