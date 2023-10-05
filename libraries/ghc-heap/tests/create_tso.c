#include "Rts.h"
#include "RtsAPI.h"

// Assumes the rts is paused
void unpack_closure
    ( StgClosure * inClosure
    , const StgInfoTable ** outInfoTablePtr
    , int * outHeapRepSize // Size of outHeapRep (in bytes)
    , StgWord ** outHeapRep   // Array of words
    , int * outPointersSize      // Size of outPointers (in words)
    , StgClosure *** outPointers // Array of all pointers of the TSO
    )
{
    *outInfoTablePtr = get_itbl(inClosure);

    // Copy TSO pointers.
    StgWord closureSizeW = heap_view_closureSize(inClosure);
    int closureSizeB = sizeof(StgWord) * closureSizeW;
    StgClosure ** pointers = malloc(closureSizeB);
    *outPointersSize = collect_pointers(inClosure, pointers);
    *outPointers = pointers;

    // Copy the heap rep.
    StgWord * heapRep = malloc(closureSizeB);
    for (int i = 0; i < closureSizeW; i++)
    {
        heapRep[i] = ((StgWord*)inClosure)[i];
    }

    *outHeapRepSize = closureSizeB;
    *outHeapRep = heapRep;
}

// Must be called from a safe FFI call.
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
    )
{
    // Pause RTS
    PauseToken * token = rts_pause();
    Capability * cap = pauseTokenCapability(token);

    // Create TSO/Stack
    HaskellObj trueClosure = rts_mkBool(cap, 1);
    *outTso = createGenThread(cap, 500U, trueClosure);

    // Unpack TSO
    unpack_closure(
        (StgClosure*)(*outTso),
        outTsoInfoTablePtr,
        outTsoHeapRepSize,
        outTsoHeapRep,
        outTsoPointersSize,
        outTsoPointers);

    // Unpack STACK
    StgClosure * outStackAsClosure = (*outTsoPointers)[2];
    *outStack = (StgTSO *)outStackAsClosure;
    unpack_closure(
        outStackAsClosure,
        outStackInfoTablePtr,
        outStackHeapRepSize,
        outStackHeapRep,
        outStackPointersSize,
        outStackPointers);

    // Resume RTS
    rts_resume(token);
}
