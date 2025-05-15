/* ----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2004
 *
 * Closures
 *
 * -------------------------------------------------------------------------- */

#pragma once

/*
 * The Layout of a closure header depends on which kind of system we're
 * compiling for: profiling, parallel, ticky, etc.
 */

/*
 * Used to mark GC-pointer fields which can be modified by the mutator after
 * an object is made visible on the heap. See Note [Heap memory barriers] in
 * SMP.h for details.
 */
#define MUT_FIELD

/* -----------------------------------------------------------------------------
   The profiling header
   -------------------------------------------------------------------------- */

typedef struct {
  CostCentreStack *ccs;
  union {
    StgWord trav;             /* Heap traversal */
    StgWord ldvw;             /* Lag/Drag/Void Word */
    StgWord era;              /* User-era */
  } hp;
    // Heap profiling header. This field is shared among the various heap
    // profiling modes. Currently it is used by ProfHeap.c for Lag/Drag/Void
    // profiling and by the heap traversal modes using TraverseHeap.c such as
    // the retainer profiler.
} StgProfHeader;

/* -----------------------------------------------------------------------------
   The SMP header

   A thunk has a padding word to take the updated value.  This is so
   that the update doesn't overwrite the payload, so we can avoid
   needing to lock the thunk during entry and update.

   Note: this doesn't apply to THUNK_STATICs, which have no payload.

   Note: we leave this padding word in all ways, rather than just SMP,
   so that we don't have to recompile all our libraries for SMP.
   -------------------------------------------------------------------------- */

typedef struct {
    StgWord pad MUT_FIELD;
} StgSMPThunkHeader;

/* -----------------------------------------------------------------------------
   The full fixed-size closure header

   The size of the fixed header is the sum of the optional parts plus a single
   word for the entry code pointer.
   -------------------------------------------------------------------------- */

typedef struct {
    // If TABLES_NEXT_TO_CODE is defined, then `info` is offset by
    // `sizeof(StgInfoTable)` and so points to the `code` field of the
    // StgInfoTable! You may want to use `get_itbl` to get the pointer to the
    // start of the info table. See
    // https://gitlab.haskell.org/ghc/ghc/-/wikis/commentary/rts/storage/heap-objects#tables_next_to_code.
    const StgInfoTable* info;

#if defined(PROFILING)
    StgProfHeader         prof;
#endif
} StgHeader;

typedef struct {
    const StgInfoTable* info;
#if defined(PROFILING)
    StgProfHeader         prof;
#endif
    StgSMPThunkHeader     smp;
} StgThunkHeader;

#define THUNK_EXTRA_HEADER_W (sizeofW(StgThunkHeader)-sizeofW(StgHeader))

/* -----------------------------------------------------------------------------
   Closure Types

   For any given closure type (defined in ClosureTypes.h), there is a
   corresponding structure defined below.
   -------------------------------------------------------------------------- */


// Generic closure layout, all closures follow this format consisting of a
// header field and some payload
typedef struct StgClosure_ {
    StgHeader   header;
    struct StgClosure_ *payload[];
} *StgClosurePtr; // StgClosure defined in rts/Types.h


// Thunk closure, an unevaluated value
//
// Closure types: THUNK, THUNK_<X>_<Y>
typedef struct StgThunk_ {
    StgThunkHeader  header;
    struct StgClosure_ *payload[];
} StgThunk;


// A selector thunk, this represent an unevaluated applied record field
// selector, ie `fst pair`. The field offset is stored in the header.
//
// Closure types: THUNK_SELECTOR
typedef struct {
    StgThunkHeader   header;
    StgClosure *selectee;
} StgSelector;


// PAP, partially applied function
//
// PAP payload contains pointers and non-pointers interleaved and we only have
// one info table for PAPs (stg_PAP_info). To visit pointers in a PAP payload we
// use the `fun`s bitmap. For a PAP with n_args arguments the first n_args bits
// in the fun's bitmap tell us which payload locations contain pointers.
//
// Closure types: PAP
typedef struct {
    StgHeader   header;
    StgHalfWord arity; // number of arguments left to apply, if zero this is an AP closure
    StgHalfWord n_args; // number of applied arguments
    StgClosure *fun; // guaranteed to point to a FUN closure
    StgClosure *payload[];
} StgPAP;


// AP, applied function
//
// Closure types: AP
typedef struct {
    StgThunkHeader   header;
    StgHalfWord arity; // number of arguments left to apply, 0 for an AP closure
    StgHalfWord n_args; // number of arguments applied
    StgClosure *fun; // guaranteed to point to a FUN closure
    StgClosure *payload[];
} StgAP;


// Paused evaluation, created
// - when async exceptions are thrown
// - when we have to abort thunk evaluation in threadPaused
//
// Closure types: AP_STACK
typedef struct {
    StgThunkHeader header;
    StgWord size; // number of words in payload
    StgClosure *fun;
    StgClosure *payload[]; // contains a chunk of *stack*
} StgAP_STACK;


// Indirection to some other closure on the heap
//
// Closure types: IND
typedef struct {
    StgHeader   header;
    StgClosure *indirectee;
} StgInd;


// Static indirection, indirection to a statically allocated closure? or a the
// statically allocated thing itself?
//
// Closure types: IND_STATIC
typedef struct {
    StgHeader     header;
    StgClosure   *indirectee;
    StgClosure   *static_link; // See Note [CAF lists]
    const StgInfoTable *saved_info;
        // `saved_info` also used for the link field for `debug_caf_list`,
        // see `newCAF` and Note [CAF lists] in rts/sm/Storage.h.
} StgIndStatic;


// A queue for blocking on things
//
// Closure types: BLOCKING_QUEUE
typedef struct StgBlockingQueue_ {
    StgHeader   header;
    struct StgBlockingQueue_ *link;
        // here so it looks like an IND, to be able to skip the queue without
        // deleting it (done in wakeBlockingQueue())
    StgClosure *bh;  // the BLACKHOLE
    StgTSO     *owner;
    struct MessageBlackHole_ *queue;
        // holds TSOs blocked on `bh`
} StgBlockingQueue;


// An array of bytes, ie ByteArray# and MutableByteArray#
//
// Closure types: ARR_WORDS
typedef struct {
    StgHeader  header;
    StgWord    bytes; // number of bytes in payload
    StgWord    payload[];
} StgArrBytes;


// An array of heap objects, ie Array# v and MutableArray# v
//
// Closure types: MUT_ARR_PTRS_CLEAN, MUT_ARR_PTRS_DIRTY,
// MUT_ARR_PTRS_FROZEN_DIRTY, MUT_ARR_PTRS_FROZEN_CLEAN
typedef struct _StgMutArrPtrs {
    StgHeader   header;
    StgWord     ptrs;
    StgWord     size; // ptrs plus card table
    StgClosure *payload[] MUT_FIELD;
    // see also: StgMutArrPtrs macros in ClosureMacros.h
} StgMutArrPtrs;


// A small array of head objects, ie SmallArray# and MutableSmallArray#
//
// Closure types: SMALL_MUT_ARR_PTRS_CLEAN, SMALL_MUT_ARR_PTRS_DIRTY,
// SMALL_MUT_ARR_PTRS_FROZEN_DIRTY, SMALL_MUT_ARR_PTRS_FROZEN_CLEAN,
typedef struct {
    StgHeader   header;
    StgWord     ptrs;
    StgClosure *payload[] MUT_FIELD;
} StgSmallMutArrPtrs;


// A mutable reference, ie MutVar#
//
// Closure types: MUT_VAR_CLEAN, MUT_VAR_DIRTY
typedef struct {
    StgHeader   header;
    StgClosure *var MUT_FIELD;
} StgMutVar;


/* ----------------------------------------------------------------------------
   Stack frames
   ------------------------------------------------------------------------- */


/*
 * See also StgStack in TSO.h
 *
 * These do not appear alone on the heap but always inside an StgStack or a
 * StgAP_STACK.
 */

// Thunk update frame
//
// Closure types: UPDATE_FRAME
typedef struct _StgUpdateFrame {
    StgHeader  header;
    StgClosure *updatee;
} StgUpdateFrame;

// Thunk update frame
//
// Closure types: RET_SMALL
typedef struct _StgOrigThunkInfoFrame {
    StgHeader  header;
    StgInfoTable *info_ptr;
} StgOrigThunkInfoFrame;

// Closure types: RET_SMALL
typedef struct {
    StgHeader  header;
    StgClosure *c;
} StgKeepAliveFrame;

// Stack frame, when we call catch one of these will be put on the stack so we
// know to handle exceptions with the supplied handler
//
// Closure types: CATCH_FRAME
typedef struct {
    StgHeader  header;
    StgClosure *handler;
} StgCatchFrame;


// Stack underflow frame, placed on the bottom of a stack chunk and links to
// the next chunk
//
// Closure types: UNDERFLOW_FRAME
typedef struct {
    const StgInfoTable* info;
    struct StgStack_ *next_chunk;
} StgUnderflowFrame;


// Stack end frame, placed on the bottom of a stack chunk signifying the very
// bottom of the stack
//
// Closure types: STOP_FRAME
typedef struct {
    StgHeader  header;
} StgStopFrame;

// Stack frame indicating that the stack's owning thread has finished.
//
// Closure types: RET_SMALL
typedef struct {
    StgHeader  header;
    StgClosure *result;
} StgDeadThreadFrame;

// A function return stack frame: used when saving the state for a
// garbage collection at a function entry point.  The function
// arguments are on the stack, and we also save the function (its
// info table describes the pointerhood of the arguments).
//
// The stack frame size is also cached in the frame for convenience.
//
// The only RET_FUN is stg_gc_fun, which is created by __stg_gc_fun,
// both in HeapStackCheck.cmm.
//
// Closure types: RET_FUN
typedef struct {
    const StgInfoTable* info;
    StgWord        size;
    StgClosure *   fun;
    StgClosure *   payload[];
} StgRetFun;



/* ----------------------------------------------------------------------------
   Special heap objects
   ------------------------------------------------------------------------- */

// Int or Char-like things, these are statically allocated in StgMiscClosures.h.
// See Note [CHARLIKE and INTLIKE closures] in StgMiscClosures.h.
//
// Closure type: CONSTR_0_1
typedef struct {
  StgHeader header;
  StgWord data;
} StgIntCharlikeClosure;


// Stable name, StableName# v
typedef struct _StgStableName {
  StgHeader      header;
  StgWord        sn;
} StgStableName;


// A weak reference, Weak#
//
// Closure types: WEAK
typedef struct _StgWeak {
  StgHeader header;

  // C finalizers, see StgCFinalizerList below
  //
  // Points to stg_NO_FINALIZER_closure to indicate no c finalizers.
  StgClosure *cfinalizers MUT_FIELD;

  StgClosure *key;
  StgClosure *value; // the actual value references by the weak reference

  // Haskell finalizer (type IO ())
  //
  // Points to stg_NO_FINALIZER_closure to indicate no Haskell finalizer.
  StgClosure *finalizer;

  struct _StgWeak *link;
} StgWeak;


// Linked list of c function pointer finalisers for a weak reference
//
// See the addCFinalizerToWeak# primop where these are constructed and
// runCFinalizers (C) where they are consumed.
//
// Closure type: CONSTR
typedef struct _StgCFinalizerList {
  StgHeader header;
  StgClosure *link MUT_FIELD; // the next finaliser

  // function to call
  //
  // Actual type is `void (*)(void* ptr)` or `void (*)(void* eptr, void* ptr)`
  // depending on the flag field.
  void (*fptr)(void);

  void *ptr; // pointer to data
  void *eptr; // pointer to environment

  StgWord flag; // has environment, 0: no environment, 1: yes environment

} StgCFinalizerList;


// Byte code objects.  These are fixed size objects with pointers to
// four arrays, designed so that a BCO can be easily "re-linked" to
// other BCOs, to facilitate GHC's intelligent recompilation.  The
// array of instructions is static and not re-generated when the BCO
// is re-linked, but the other 3 arrays will be regenerated.
//
// A BCO represents either a function or a stack frame.  In each case,
// it needs a bitmap to describe to the garbage collector the
// pointerhood of its arguments/free variables respectively, and in
// the case of a function it also needs an arity.  These are stored
// directly in the BCO, rather than in the instrs array, for two
// reasons:
// (a) speed: we need to get at the bitmap info quickly when
//     the GC is examining APs and PAPs that point to this BCO
// (b) a subtle interaction with the compacting GC.  In compacting
//     GC, the info that describes the size/layout of a closure
//     cannot be in an object more than one level of indirection
//     away from the current object, because of the order in
//     which pointers are updated to point to their new locations.
//
// Closure types: BCO
typedef struct {
    StgHeader      header;
    StgArrBytes   *instrs; // the code
    StgArrBytes   *literals; // literals used by the instructions
    StgMutArrPtrs *ptrs; // free variables
    StgHalfWord   arity; // arity of this BCO
    StgHalfWord   size;  // size of the closure and bitmap
    StgWord       bitmap[]; // an StgLargeBitmap
} StgBCO;

#define BCO_BITMAP(bco)      ((StgLargeBitmap *)((StgBCO *)(bco))->bitmap)
#define BCO_BITMAP_SIZE(bco) (BCO_BITMAP(bco)->size)
#define BCO_BITMAP_BITS(bco) (BCO_BITMAP(bco)->bitmap)
#define BCO_BITMAP_SIZEW(bco) ((BCO_BITMAP_SIZE(bco) + BITS_IN(StgWord) - 1) \
                                / BITS_IN(StgWord))



/* ----------------------------------------------------------------------------
   Concurrent communication objects
   ------------------------------------------------------------------------- */


// Queue for threads waiting on an MVar
//
// Closure types:
typedef struct StgMVarTSOQueue_ {
    StgHeader                header;
    struct StgMVarTSOQueue_ *link;
    struct StgTSO_          *tso;
} StgMVarTSOQueue;


// An MVar#
//
// Closure types: MVAR_CLEAN, MVAR_DIRTY
typedef struct {
    StgHeader                header;

    // threads that are waiting on this MVar
    struct StgMVarTSOQueue_ *head MUT_FIELD;
    struct StgMVarTSOQueue_ *tail MUT_FIELD;

    // The value in the MVar if filled
    StgClosure*              value MUT_FIELD;
} StgMVar;


/* ----------------------------------------------------------------------------
   STM data structures
   ------------------------------------------------------------------------- */

/*
 *  StgTVar defines the only type that can be updated through the STM
 *  interface.
 *
 *  Note that various optimisations may be possible in order to use less
 *  space for these data structures at the cost of more complexity in the
 *  implementation:
 *
 *   - In StgTVar, current_value and first_watch_queue_entry could be held in
 *     the same field: if any thread is waiting then its expected_value for
 *     the tvar is the current value.
 *
 *   - In StgTRecHeader, it might be worthwhile having separate chunks
 *     of read-only and read-write locations.  This would save a
 *     new_value field in the read-only locations.
 *
 *   - In StgAtomicallyFrame, we could combine the waiting bit into
 *     the header (maybe a different info tbl for a waiting transaction).
 *     This means we can specialise the code for the atomically frame
 *     (it immediately switches on frame->waiting anyway).
 */

typedef struct StgTRecHeader_ StgTRecHeader;

typedef struct StgTVarWatchQueue_ {
  StgHeader                  header;
  StgClosure                *closure; // StgTSO
  struct StgTVarWatchQueue_ *next_queue_entry;
  struct StgTVarWatchQueue_ *prev_queue_entry;
} StgTVarWatchQueue;

typedef struct {
  StgHeader                  header;
  StgClosure                *current_value MUT_FIELD; /* accessed via atomics */
  StgTVarWatchQueue         *first_watch_queue_entry MUT_FIELD; /* accessed via atomics */
  StgInt                     num_updates; /* accessed via atomics */
} StgTVar;

/* new_value == expected_value for read-only accesses */
/* new_value is a StgTVarWatchQueue entry when trec in state TREC_WAITING */
typedef struct {
  StgTVar                   *tvar;
  StgClosure                *expected_value;
  StgClosure                *new_value;
#if defined(THREADED_RTS)
  StgInt                     num_updates;
#endif
} TRecEntry;

#define TREC_CHUNK_NUM_ENTRIES 16

/*
 * A chunk of TVar updates (`TRecEntry`s) belonging to an in-flight STM
 * transaction.
 */
typedef struct StgTRecChunk_ {
  StgHeader                  header;
  struct StgTRecChunk_      *prev_chunk;
  StgWord                    next_entry_idx;
  TRecEntry                  entries[TREC_CHUNK_NUM_ENTRIES];
} StgTRecChunk;

typedef enum {
  TREC_ACTIVE,        /* Transaction in progress, outcome undecided */
  TREC_CONDEMNED,     /* Transaction in progress, inconsistent / out of date reads */
  TREC_ABORTED,       /* Transaction has aborted, now reverting tvars */
  TREC_WAITING,       /* Transaction currently waiting */
} TRecState;

/* A transactional record */
struct StgTRecHeader_ {
  StgHeader                  header;
  struct StgTRecHeader_     *enclosing_trec;
  StgTRecChunk              *current_chunk MUT_FIELD;
  TRecState                  state;
};

/* A stack frame delimiting an STM transaction */
typedef struct {
  StgHeader   header;
  StgClosure *code;
  StgClosure *result;
} StgAtomicallyFrame;

/* A catch# handler introduced within an STM transaction */
typedef struct {
  StgHeader   header;
  StgClosure *code;
  StgClosure *handler;
} StgCatchSTMFrame;

/* A catchRetry# handler */
typedef struct {
  StgHeader      header;
  StgWord        running_alt_code;
  StgClosure    *first_code;
  StgClosure    *alt_code;
} StgCatchRetryFrame;

/* ----------------------------------------------------------------------------
   Messages
   ------------------------------------------------------------------------- */

typedef struct Message_ {
    StgHeader        header;
    struct Message_ *link;
} Message;

typedef struct MessageWakeup_ {
    StgHeader header;
    Message  *link;
    StgTSO   *tso;
} MessageWakeup;

typedef struct MessageThrowTo_ {
    StgHeader   header;
    struct MessageThrowTo_ *link;
    StgTSO     *source;
    StgTSO     *target;
    StgClosure *exception;
} MessageThrowTo;

typedef struct MessageBlackHole_ {
    StgHeader   header;
    struct MessageBlackHole_ *link;
        // here so it looks like an IND, to be able to skip the message without
        // deleting it (done in throwToMsg())
    StgTSO     *tso;
    StgClosure *bh;
} MessageBlackHole;

typedef struct MessageCloneStack_ {
    StgHeader header;
    Message   *link;
    StgMVar   *result;
    StgTSO    *tso;
} MessageCloneStack;


/* ----------------------------------------------------------------------------
   Compact Regions
   ------------------------------------------------------------------------- */

//
// A compact region is a list of blocks.  Each block starts with an
// StgCompactNFDataBlock structure, and the list is chained through the next
// field of these structs.  (the link field of the bdescr is used to chain
// together multiple compact region on the compact_objects field of a
// generation).
//
// See Note [Compact Normal Forms] for details
//
typedef struct StgCompactNFDataBlock_ {
    struct StgCompactNFDataBlock_ *self;
       // the address of this block this is copied over to the
       // receiving end when serializing a compact, so the receiving
       // end can allocate the block at best as it can, and then
       // verify if pointer adjustment is needed or not by comparing
       // self with the actual address; the same data is sent over as
       // SerializedCompact metadata, but having it here simplifies
       // the fixup implementation.
    struct StgCompactNFData_ *owner;
       // the closure who owns this block (used in objectGetCompact)
    struct StgCompactNFDataBlock_ *next MUT_FIELD;
       // chain of blocks used for serialization and freeing
} StgCompactNFDataBlock;

//
// This is the Compact# primitive object.
//
typedef struct StgCompactNFData_ {
    StgHeader header;
      // for sanity and other checks in practice, nothing should ever
      // need the compact info pointer (we don't even need fwding
      // pointers because it's a large object)
    StgWord totalW;
      // Total number of words in all blocks in the compact
    StgWord autoBlockW;
      // size of automatically appended blocks
    StgPtr hp, hpLim;
      // the beginning and end of the free area in the nursery block.  This is
      // just a convenience so that we can avoid multiple indirections through
      // the nursery pointer below during compaction.
    StgCompactNFDataBlock *nursery;
      // where to (try to) allocate from when appending
    StgCompactNFDataBlock *last MUT_FIELD;
      // the last block of the chain (to know where to append new
      // blocks for resize)
    struct hashtable *hash;
      // the hash table for the current compaction, or NULL if
      // there's no (sharing-preserved) compaction in progress.
    StgClosure *result;
      // Used temporarily to store the result of compaction.  Doesn't need to be
      // a GC root.
    struct StgCompactNFData_ *link;
      // Used by compacting GC for linking CNFs with threaded hash tables.
      // See Note [CNFs in compacting GC] in Compact.c for details.
} StgCompactNFData;

/* ----------------------------------------------------------------------------
   Continuations (see Note [Continuations overview] in Continuation.c)
   ------------------------------------------------------------------------- */

typedef StgClosure *StgPromptTag;

typedef struct {
    StgHeader header;
    StgPromptTag tag;
} StgPromptFrame;

// Closure types: CONTINUATION
typedef struct {
    StgHeader header;
    const StgInfoTable *apply_mask_frame;
      // A pointer to a stack frame info table that should be returned to after
      // applying this continuation to update the async exception masking state,
      // or NULL if the masking state of the calling context should be preserved;
      // see Note [Continuations and async exception masking] in Continuation.c
    StgWord mask_frame_offset;
      // Word offset into `stack` for the outermost mask/unmask frame, or 0 if
      // `apply_mask_frame` is NULL;
      // see Note [Continuations and async exception masking] in Continuation.c
    StgWord stack_size;
      // Number of words of captured stack
    StgWord stack[];
} StgContinuation;
