#include "Rts.h"
#include "RtsAPI.h"

void create_and_unpack_tso_and_stack
    // TSO
    ( StgTSO ** outTso
    , const StgInfoTable ** outTsoInfoTablePtr
    , int * outTsoHeapRepSize // Size of outHeapRep (in bytes)
    , StgWord ** outTsoHeapRep   // Array of words
    , int * outTsoPointersSize      // Size of outPointers (in words)
    , StgClosure *** outTsoPointers // Array of all pointers of the TSO
    // Stack
    , StgTSO ** outStack
    , const StgInfoTable ** outStackInfoTablePtr
    , int * outStackHeapRepSize // Size of outHeapRep (in bytes)
    , StgWord ** outStackHeapRep   // Array of words
    , int * outStackPointersSize      // Size of outPointers (in words)
    , StgClosure *** outStackPointers // Array of all pointers of the TSO
    );
