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

/* -----------------------------------------------------------------------------
   The profiling header
   -------------------------------------------------------------------------- */

typedef struct {
  CostCentreStack *ccs;
  union {
    struct _RetainerSet *rs;  /* Retainer Set */
    StgWord ldvw;             /* Lag/Drag/Void Word */
  } hp;
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
    StgWord pad;
} StgSMPThunkHeader;

/* -----------------------------------------------------------------------------
   The full fixed-size closure header

   The size of the fixed header is the sum of the optional parts plus a single
   word for the entry code pointer.
   -------------------------------------------------------------------------- */

typedef struct {
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

   For any given closure type (defined in InfoTables.h), there is a
   corresponding structure defined below.  The name of the structure
   is obtained by concatenating the closure type with '_closure'
   -------------------------------------------------------------------------- */

/* All closures follow the generic format */

typedef struct StgClosure_ {
    StgHeader   header;
    struct StgClosure_ *payload[];
} *StgClosurePtr; // StgClosure defined in rts/Types.h

typedef struct {
    StgThunkHeader  header;
    struct StgClosure_ *payload[];
} StgThunk;

typedef struct {
    StgThunkHeader   header;
    StgClosure *selectee;
} StgSelector;

typedef struct {
    StgHeader   header;
    StgHalfWord arity;          /* zero if it is an AP */
    StgHalfWord n_args;
    StgClosure *fun;            /* really points to a fun */
    StgClosure *payload[];
} StgPAP;

typedef struct {
    StgThunkHeader   header;
    StgHalfWord arity;          /* zero if it is an AP */
    StgHalfWord n_args;
    StgClosure *fun;            /* really points to a fun */
    StgClosure *payload[];
} StgAP;

typedef struct {
    StgThunkHeader   header;
    StgWord     size;                    /* number of words in payload */
    StgClosure *fun;
    StgClosure *payload[]; /* contains a chunk of *stack* */
} StgAP_STACK;

typedef struct {
    StgHeader   header;
    StgClosure *indirectee;
} StgInd;

typedef struct {
    StgHeader     header;
    StgClosure   *indirectee;
    StgClosure   *static_link;
    const StgInfoTable *saved_info;
} StgIndStatic;

typedef struct StgBlockingQueue_ {
    StgHeader   header;
    struct StgBlockingQueue_ *link; // here so it looks like an IND
    StgClosure *bh;  // the BLACKHOLE
    StgTSO     *owner;
    struct MessageBlackHole_ *queue;
} StgBlockingQueue;

typedef struct {
    StgHeader  header;
    StgWord    bytes;
    StgWord    payload[];
} StgArrBytes;

typedef struct {
    StgHeader   header;
    StgWord     ptrs;
    StgWord     size; // ptrs plus card table
    StgClosure *payload[];
    // see also: StgMutArrPtrs macros in ClosureMacros.h
} StgMutArrPtrs;

typedef struct {
    StgHeader   header;
    StgWord     ptrs;
    StgClosure *payload[];
} StgSmallMutArrPtrs;

typedef struct {
    StgHeader   header;
    StgClosure *var;
} StgMutVar;

typedef struct _StgUpdateFrame {
    StgHeader  header;
    StgClosure *updatee;
} StgUpdateFrame;

typedef struct {
    StgHeader  header;
    StgWord    exceptions_blocked;
    StgClosure *handler;
} StgCatchFrame;

typedef struct {
    const StgInfoTable* info;
    struct StgStack_ *next_chunk;
} StgUnderflowFrame;

typedef struct {
    StgHeader  header;
} StgStopFrame;

typedef struct {
  StgHeader header;
  StgWord data;
} StgIntCharlikeClosure;

/* statically allocated */
typedef struct {
  StgHeader  header;
} StgRetry;

typedef struct _StgStableName {
  StgHeader      header;
  StgWord        sn;
} StgStableName;

typedef struct _StgWeak {       /* Weak v */
  StgHeader header;
  StgClosure *cfinalizers;
  StgClosure *key;
  StgClosure *value;            /* v */
  StgClosure *finalizer;
  struct _StgWeak *link;
} StgWeak;

typedef struct _StgCFinalizerList {
  StgHeader header;
  StgClosure *link;
  void (*fptr)(void);
  void *ptr;
  void *eptr;
  StgWord flag; /* has environment (0 or 1) */
} StgCFinalizerList;

/* Byte code objects.  These are fixed size objects with pointers to
 * four arrays, designed so that a BCO can be easily "re-linked" to
 * other BCOs, to facilitate GHC's intelligent recompilation.  The
 * array of instructions is static and not re-generated when the BCO
 * is re-linked, but the other 3 arrays will be regenerated.
 *
 * A BCO represents either a function or a stack frame.  In each case,
 * it needs a bitmap to describe to the garbage collector the
 * pointerhood of its arguments/free variables respectively, and in
 * the case of a function it also needs an arity.  These are stored
 * directly in the BCO, rather than in the instrs array, for two
 * reasons:
 * (a) speed: we need to get at the bitmap info quickly when
 *     the GC is examining APs and PAPs that point to this BCO
 * (b) a subtle interaction with the compacting GC.  In compacting
 *     GC, the info that describes the size/layout of a closure
 *     cannot be in an object more than one level of indirection
 *     away from the current object, because of the order in
 *     which pointers are updated to point to their new locations.
 */

typedef struct {
    StgHeader      header;
    StgArrBytes   *instrs;      /* a pointer to an ArrWords */
    StgArrBytes   *literals;    /* a pointer to an ArrWords */
    StgMutArrPtrs *ptrs;        /* a pointer to a  MutArrPtrs */
    StgHalfWord   arity;        /* arity of this BCO */
    StgHalfWord   size;         /* size of this BCO (in words) */
    StgWord       bitmap[];  /* an StgLargeBitmap */
} StgBCO;

#define BCO_BITMAP(bco)      ((StgLargeBitmap *)((StgBCO *)(bco))->bitmap)
#define BCO_BITMAP_SIZE(bco) (BCO_BITMAP(bco)->size)
#define BCO_BITMAP_BITS(bco) (BCO_BITMAP(bco)->bitmap)
#define BCO_BITMAP_SIZEW(bco) ((BCO_BITMAP_SIZE(bco) + BITS_IN(StgWord) - 1) \
                                / BITS_IN(StgWord))

/* A function return stack frame: used when saving the state for a
 * garbage collection at a function entry point.  The function
 * arguments are on the stack, and we also save the function (its
 * info table describes the pointerhood of the arguments).
 *
 * The stack frame size is also cached in the frame for convenience.
 */
typedef struct {
    const StgInfoTable* info;
    StgWord        size;
    StgClosure *   fun;
    StgClosure *   payload[];
} StgRetFun;

/* Concurrent communication objects */

typedef struct StgMVarTSOQueue_ {
    StgHeader                header;
    struct StgMVarTSOQueue_ *link;
    struct StgTSO_          *tso;
} StgMVarTSOQueue;

typedef struct {
    StgHeader                header;
    struct StgMVarTSOQueue_ *head;
    struct StgMVarTSOQueue_ *tail;
    StgClosure*              value;
} StgMVar;


/* STM data structures
 *
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
  StgClosure                *closure; // StgTSO or StgAtomicInvariant
  struct StgTVarWatchQueue_ *next_queue_entry;
  struct StgTVarWatchQueue_ *prev_queue_entry;
} StgTVarWatchQueue;

typedef struct {
  StgHeader                  header;
  StgClosure                *volatile current_value;
  StgTVarWatchQueue         *volatile first_watch_queue_entry;
  StgInt                     volatile num_updates;
} StgTVar;

typedef struct {
  StgHeader      header;
  StgClosure    *code;
  StgTRecHeader *last_execution;
  StgWord        lock;
} StgAtomicInvariant;

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

typedef struct StgTRecChunk_ {
  StgHeader                  header;
  struct StgTRecChunk_      *prev_chunk;
  StgWord                    next_entry_idx;
  TRecEntry                  entries[TREC_CHUNK_NUM_ENTRIES];
} StgTRecChunk;

typedef enum {
  TREC_ACTIVE,        /* Transaction in progress, outcome undecided */
  TREC_CONDEMNED,     /* Transaction in progress, inconsistent / out of date reads */
  TREC_COMMITTED,     /* Transaction has committed, now updating tvars */
  TREC_ABORTED,       /* Transaction has aborted, now reverting tvars */
  TREC_WAITING,       /* Transaction currently waiting */
} TRecState;

typedef struct StgInvariantCheckQueue_ {
  StgHeader                       header;
  StgAtomicInvariant             *invariant;
  StgTRecHeader                  *my_execution;
  struct StgInvariantCheckQueue_ *next_queue_entry;
} StgInvariantCheckQueue;

struct StgTRecHeader_ {
  StgHeader                  header;
  struct StgTRecHeader_     *enclosing_trec;
  StgTRecChunk              *current_chunk;
  StgInvariantCheckQueue    *invariants_to_check;
  TRecState                  state;
};

typedef struct {
  StgHeader   header;
  StgClosure *code;
  StgTVarWatchQueue *next_invariant_to_check;
  StgClosure *result;
} StgAtomicallyFrame;

typedef struct {
  StgHeader   header;
  StgClosure *code;
  StgClosure *handler;
} StgCatchSTMFrame;

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
    StgTSO     *tso;
    StgClosure *bh;
} MessageBlackHole;

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
    struct StgCompactNFDataBlock_ *next;
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
    StgCompactNFDataBlock *last;
      // the last block of the chain (to know where to append new
      // blocks for resize)
    struct hashtable *hash;
      // the hash table for the current compaction, or NULL if
      // there's no (sharing-preserved) compaction in progress.
    StgClosure *result;
      // Used temporarily to store the result of compaction.  Doesn't need to be
      // a GC root.
} StgCompactNFData;
