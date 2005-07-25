/* ----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2004
 *
 * Closures
 *
 * -------------------------------------------------------------------------- */

#ifndef CLOSURES_H
#define CLOSURES_H

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
   The GranSim header
   -------------------------------------------------------------------------- */

typedef struct {
  StgWord procs; /* bitmask indicating on which PEs this closure resides */
} StgGranHeader;

/* -----------------------------------------------------------------------------
   The SMP header

   In SMP mode, we have an extra word of padding in a thunk's header.
   (Note: thunks only; other closures do not have this padding word).
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
    const struct _StgInfoTable* info;
#ifdef PROFILING
    StgProfHeader         prof;
#endif
#ifdef GRAN
    StgGranHeader         gran;
#endif
} StgHeader;

/*
 * In SMP mode, a thunk has a padding word to take the updated value.
 * This is so that the update doesn't overwrite the payload, so we can
 * avoid needing to lock the thunk during entry and update.
 *
 * Note: this doesn't apply to THUNK_STATICs, which have no payload.
 */
typedef struct {
    const struct _StgInfoTable* info;
#ifdef PROFILING
    StgProfHeader         prof;
#endif
#ifdef GRAN
    StgGranHeader         gran;
#endif
#ifdef SMP
    StgSMPThunkHeader     smp;
#endif
} StgThunkHeader;

/* -----------------------------------------------------------------------------
   Closure Types

   For any given closure type (defined in InfoTables.h), there is a
   corresponding structure defined below.  The name of the structure
   is obtained by concatenating the closure type with '_closure'
   -------------------------------------------------------------------------- */

/* All closures follow the generic format */

struct StgClosure_ {
    StgHeader   header;
    struct StgClosure_ *payload[FLEXIBLE_ARRAY];
};

typedef struct {
    StgThunkHeader  header;
    struct StgClosure_ *payload[FLEXIBLE_ARRAY];
} StgThunk;

typedef struct {
    StgThunkHeader   header;
    StgClosure *selectee;
} StgSelector;

typedef struct {
    StgHeader   header;
    StgHalfWord arity;		/* zero if it is an AP */
    StgHalfWord n_args;
    StgClosure *fun;		/* really points to a fun */
    StgClosure *payload[FLEXIBLE_ARRAY];
} StgPAP;

typedef struct {
    StgThunkHeader   header;
    StgHalfWord arity;		/* zero if it is an AP */
    StgHalfWord n_args;
    StgClosure *fun;		/* really points to a fun */
    StgClosure *payload[FLEXIBLE_ARRAY];
} StgAP;

typedef struct {
    StgThunkHeader   header;
    StgWord     size;                    /* number of words in payload */
    StgClosure *fun;
    StgClosure *payload[FLEXIBLE_ARRAY]; /* contains a chunk of *stack* */
} StgAP_STACK;

typedef struct {
    StgHeader   header;
    StgClosure *indirectee;
} StgInd;

typedef struct {
    StgHeader     header;
    StgClosure   *indirectee;
    StgClosure   *static_link;
    struct _StgInfoTable *saved_info;
} StgIndStatic;

typedef struct {
    StgHeader  header;
    StgWord    words;
    StgWord    payload[FLEXIBLE_ARRAY];
} StgArrWords;

typedef struct {
    StgHeader   header;
    StgWord     ptrs;
    StgClosure *payload[FLEXIBLE_ARRAY];
} StgMutArrPtrs;

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
    StgInt      exceptions_blocked;
    StgClosure *handler;
} StgCatchFrame;

typedef struct {
    StgHeader  header;
} StgStopFrame;  

typedef struct {
    StgHeader   header;
    StgClosure *evacuee;
} StgEvacuated;

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

typedef struct _StgWeak {	/* Weak v */
  StgHeader header;
  StgClosure *key;
  StgClosure *value;		/* v */
  StgClosure *finalizer;
  struct _StgWeak *link;
} StgWeak;

typedef struct _StgDeadWeak {	/* Weak v */
  StgHeader header;
  struct _StgWeak *link;
} StgDeadWeak;

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
    StgArrWords   *instrs;	/* a pointer to an ArrWords */
    StgArrWords   *literals;	/* a pointer to an ArrWords */
    StgMutArrPtrs *ptrs;	/* a pointer to a  MutArrPtrs */
    StgArrWords   *itbls;	/* a pointer to an ArrWords */
    StgHalfWord   arity;        /* arity of this BCO */
    StgHalfWord   size;         /* size of this BCO (in words) */
    StgWord       bitmap[FLEXIBLE_ARRAY];  /* an StgLargeBitmap */
} StgBCO;

#define BCO_BITMAP(bco)      ((StgLargeBitmap *)((StgBCO *)(bco))->bitmap)
#define BCO_BITMAP_SIZE(bco) (BCO_BITMAP(bco)->size)
#define BCO_BITMAP_BITS(bco) (BCO_BITMAP(bco)->bitmap)
#define BCO_BITMAP_SIZEW(bco) ((BCO_BITMAP_SIZE(bco) + BITS_IN(StgWord) - 1) \
			        / BITS_IN(StgWord))

/* -----------------------------------------------------------------------------
   Dynamic stack frames for generic heap checks.

   These generic heap checks are slow, but have the advantage of being
   usable in a variety of situations.

   The one restriction is that any relevant SRTs must already be pointed
   to from the stack.  The return address doesn't need to have an info
   table attached: hence it can be any old code pointer.

   The liveness mask contains a 1 at bit n, if register Rn contains a
   non-pointer.  The contents of all 8 vanilla registers are always saved
   on the stack; the liveness mask tells the GC which ones contain
   pointers.

   Good places to use a generic heap check: 

        - case alternatives (the return address with an SRT is already
	  on the stack).

	- primitives (no SRT required).

   The stack frame layout for a RET_DYN is like this:

          some pointers         |-- RET_DYN_PTRS(liveness) words
          some nonpointers      |-- RET_DYN_NONPTRS(liveness) words
			       
	  L1                    \
          D1-2                  |-- RET_DYN_NONPTR_REGS_SIZE words
	  F1-4                  /
			       
	  R1-8                  |-- RET_DYN_BITMAP_SIZE words
			       
	  return address        \
	  liveness mask         |-- StgRetDyn structure
	  stg_gen_chk_info      /

   we assume that the size of a double is always 2 pointers (wasting a
   word when it is only one pointer, but avoiding lots of #ifdefs).

   See Liveness.h for the macros (RET_DYN_PTRS() etc.).

   NOTE: if you change the layout of RET_DYN stack frames, then you
   might also need to adjust the value of RESERVED_STACK_WORDS in
   Constants.h.
   -------------------------------------------------------------------------- */

typedef struct {
    const struct _StgInfoTable* info;
    StgWord        liveness;
    StgWord        ret_addr;
    StgClosure *   payload[FLEXIBLE_ARRAY];
} StgRetDyn;

/* A function return stack frame: used when saving the state for a
 * garbage collection at a function entry point.  The function
 * arguments are on the stack, and we also save the function (its
 * info table describes the pointerhood of the arguments).
 *
 * The stack frame size is also cached in the frame for convenience.
 */
typedef struct {
    const struct _StgInfoTable* info;
    StgWord        size;
    StgClosure *   fun;
    StgClosure *   payload[FLEXIBLE_ARRAY];
} StgRetFun;

/* Concurrent communication objects */

typedef struct {
  StgHeader       header;
  struct StgTSO_ *head;
  struct StgTSO_ *tail;
  StgClosure*     value;
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
 *   - In StgTVar, current_value and first_wait_queue_entry could be held in
 *     the same field: if any thread is waiting then its expected_value for
 *     the tvar is the current value.  
 *
 *   - In StgTRecHeader, it might be worthwhile having separate chunks
 *     of read-only and read-write locations.  This would save a
 *     new_value field in the read-only locations.
 */

typedef struct StgTVarWaitQueue_ {
  StgHeader                  header;
  struct StgTSO_            *waiting_tso;
  struct StgTVarWaitQueue_  *next_queue_entry;
  struct StgTVarWaitQueue_  *prev_queue_entry;
} StgTVarWaitQueue;

typedef struct {
  StgHeader                  header;
  StgClosure                *volatile current_value;
  StgTVarWaitQueue          *volatile first_wait_queue_entry;
#if defined(SMP)
  struct StgTRecHeader_     *volatile last_update_by;
#endif
} StgTVar;

/* new_value == expected_value for read-only accesses */
/* new_value is a StgTVarWaitQueue entry when trec in state TREC_WAITING */
typedef struct {
  StgTVar                   *tvar;
  StgClosure                *expected_value;
  StgClosure                *new_value; 
#if defined(SMP)
  struct StgTRecHeader_     *saw_update_by;
#endif
} TRecEntry;

#define TREC_CHUNK_NUM_ENTRIES 256

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

typedef struct StgTRecHeader_ {
  StgHeader                  header;
  TRecState                  state;
  struct StgTRecHeader_     *enclosing_trec;
  StgTRecChunk              *current_chunk;
} StgTRecHeader;

typedef struct {
    StgHeader   header;
    StgBool     waiting;
    StgClosure *code;
} StgAtomicallyFrame;

typedef struct {
    StgHeader   header;
    StgClosure *handler;
} StgCatchSTMFrame;

typedef struct {
    StgHeader      header;
    StgBool        running_alt_code;
    StgClosure    *first_code;
    StgClosure    *alt_code;
    StgTRecHeader *first_code_trec;
} StgCatchRetryFrame;

#if defined(PAR) || defined(GRAN)
/*
  StgBlockingQueueElement is a ``collective type'' representing the types
  of closures that can be found on a blocking queue: StgTSO, StgRBHSave,
  StgBlockedFetch.  (StgRBHSave can only appear at the end of a blocking
  queue).  Logically, this is a union type, but defining another struct
  with a common layout is easier to handle in the code.  
  Note that in the standard setup only StgTSOs can be on a blocking queue.
  This is one of the main reasons for slightly different code in files
  such as Schedule.c.
*/
typedef struct StgBlockingQueueElement_ {
  StgHeader                         header;
  struct StgBlockingQueueElement_  *link;      /* next elem in BQ */
  struct StgClosure_               *payload[FLEXIBLE_ARRAY];/* contents of the closure */
} StgBlockingQueueElement;

/* only difference to std code is type of the elem in the BQ */
typedef struct StgBlockingQueue_ {
  StgHeader                 header;
  struct StgBlockingQueueElement_ *blocking_queue; /* start of the BQ */
} StgBlockingQueue;

/* this closure is hanging at the end of a blocking queue in (see RBH.c) */
typedef struct StgRBHSave_ {
  StgHeader    header;
  StgClosure  *payload[FLEXIBLE_ARRAY];     /* 2 words ripped out of the guts of the */
} StgRBHSave;                  /*  closure holding the blocking queue */
 
typedef struct StgRBH_ {
  StgHeader                         header;
  struct StgBlockingQueueElement_  *blocking_queue; /* start of the BQ */
} StgRBH;

#endif

#if defined(PAR)
/* global indirections aka FETCH_ME closures */
typedef struct StgFetchMe_ {
  StgHeader              header;
  globalAddr            *ga;        /* ptr to unique id for a closure */
} StgFetchMe;

/* same contents as an ordinary StgBlockingQueue */
typedef struct StgFetchMeBlockingQueue_ {
  StgHeader                          header;
  struct StgBlockingQueueElement_   *blocking_queue; /* start of the BQ */
} StgFetchMeBlockingQueue;

/* This is an entry in a blocking queue. It indicates a fetch request from a 
   TSO on another PE demanding the value of this closur. Note that a
   StgBlockedFetch can only occur in a BQ. Once the node is evaluated and
   updated with the result, the result will be sent back (the PE is encoded
   in the globalAddr) and the StgBlockedFetch closure will be nuked.
*/
typedef struct StgBlockedFetch_ {
  StgHeader                         header;
  struct StgBlockingQueueElement_  *link;     /* next elem in the BQ */
  StgClosure                       *node;     /* node to fetch */
  globalAddr                        ga;       /* where to send the result to */
} StgBlockedFetch;                            /* NB: not just a ptr to a GA */
#endif

#endif /* CLOSURES_H */
