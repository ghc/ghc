/* ----------------------------------------------------------------------------
 * $Id: Closures.h,v 1.33 2003/03/24 14:46:53 simonmar Exp $
 *
 * (c) The GHC Team, 1998-1999
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
    struct _RetainerSet *rs;  // Retainer Set
    StgWord ldvw;             // Lag/Drag/Void Word
  } hp;
} StgProfHeader;

/* -----------------------------------------------------------------------------
   The GranSim header
   -------------------------------------------------------------------------- */

typedef struct {
  StgWord procs; /* bitmask indicating on which PEs this closure resides */
} StgGranHeader;

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

#define FIXED_HS (sizeof(StgHeader))

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

/* What a stroke of luck - all our mutable closures follow the same
 * basic layout, with the mutable link field as the second field after
 * the header.  This means the following structure is the supertype of
 * mutable closures.
 */

typedef struct StgMutClosure_ {
    StgHeader   header;
    StgWord     padding;
    struct StgMutClosure_ *mut_link;
    struct StgClosure_ *payload[FLEXIBLE_ARRAY];
} StgMutClosure;

typedef struct {
    StgHeader   header;
    StgClosure *selectee;
} StgSelector;

typedef struct {
    StgHeader   header;
    StgHalfWord arity;		/* zero if it is an AP */
    StgHalfWord n_args;
    StgClosure *fun;		/* really points to a fun */
    StgClosure *payload[FLEXIBLE_ARRAY];
} StgPAP;

// AP closures have the same layout, for convenience
typedef StgPAP StgAP;

typedef struct {
    StgHeader   header;
    StgWord     size;                    // number of words in payload
    StgClosure *fun;
    StgClosure *payload[FLEXIBLE_ARRAY]; // contains a chunk of *stack*
} StgAP_STACK;

typedef struct {
    StgHeader   header;
    StgClosure *indirectee;
} StgInd;

typedef struct {
    StgHeader   header;
    StgClosure *indirectee;
    StgMutClosure *mut_link;
} StgIndOldGen;

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
    StgMutClosure *mut_link;	/* mutable list */
    StgClosure *payload[FLEXIBLE_ARRAY];
} StgMutArrPtrs;

typedef struct {
    StgHeader   header;
    StgClosure *var;
    StgMutClosure *mut_link;
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

typedef struct _StgForeignObj {
  StgHeader      header;
  StgAddr        data;		/* pointer to data in non-haskell-land */
} StgForeignObj;
  
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
    StgArrWords   *instrs;	// a pointer to an ArrWords
    StgArrWords   *literals;	// a pointer to an ArrWords
    StgMutArrPtrs *ptrs;	// a pointer to a  MutArrPtrs
    StgArrWords   *itbls;	// a pointer to an ArrWords
    StgHalfWord   arity;        // arity of this BCO
    StgHalfWord   size;         // size of this BCO (in words)
    StgWord       bitmap[FLEXIBLE_ARRAY];  // an StgLargeBitmap
} StgBCO;

#define BCO_BITMAP(bco)      ((StgLargeBitmap *)((StgBCO *)(bco))->bitmap)
#define BCO_BITMAP_SIZE(bco) (BCO_BITMAP(bco)->size)
#define BCO_BITMAP_BITS(bco) (BCO_BITMAP(bco)->bitmap)
#define BCO_BITMAP_SIZEW(bco) ((BCO_BITMAP_SIZE(bco) + BITS_IN(StgWord) - 1) \
			        / BITS_IN(StgWord))

/* Dynamic stack frames - these have a liveness mask in the object
 * itself, rather than in the info table.  Useful for generic heap
 * check code.  See StgMacros.h, HEAP_CHK_GEN().
 */
 
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
  StgMutClosure  *mut_link;
  struct StgTSO_ *tail;
  StgClosure*     value;
} StgMVar;

#if defined(PAR) || defined(GRAN)
/*
  StgBlockingQueueElement is a ``collective type'' representing the types
  of closures that can be found on a blocking queue: StgTSO, StgRBHSave,
  StgBlockedFetch.  (StgRBHSave can only appear at the end of a blocking
  queue).  Logically, this is a union type, but defining another struct
  with a common layout is easier to handle in the code (same as for
  StgMutClosures).  
  Note that in the standard setup only StgTSOs can be on a blocking queue.
  This is one of the main reasons for slightly different code in files
  such as Schedule.c.
*/
typedef struct StgBlockingQueueElement_ {
  StgHeader                         header;
  struct StgBlockingQueueElement_  *link;      /* next elem in BQ */
  StgMutClosure                    *mut_link;  /* next elem in mutable list */
  struct StgClosure_               *payload[FLEXIBLE_ARRAY];/* contents of the closure */
} StgBlockingQueueElement;

/* only difference to std code is type of the elem in the BQ */
typedef struct StgBlockingQueue_ {
  StgHeader                 header;
  struct StgBlockingQueueElement_ *blocking_queue; /* start of the BQ */
  StgMutClosure            *mut_link;              /* next elem in mutable list */
} StgBlockingQueue;

/* this closure is hanging at the end of a blocking queue in (see RBH.c) */
typedef struct StgRBHSave_ {
  StgHeader    header;
  StgClosure  *payload[FLEXIBLE_ARRAY];     /* 2 words ripped out of the guts of the */
} StgRBHSave;                  /*  closure holding the blocking queue */
 
typedef struct StgRBH_ {
  StgHeader                         header;
  struct StgBlockingQueueElement_  *blocking_queue; /* start of the BQ */
  StgMutClosure                    *mut_link;       /* next elem in mutable list */
} StgRBH;

#else

typedef struct StgBlockingQueue_ {
  StgHeader          header;
  struct StgTSO_    *blocking_queue;
  StgMutClosure     *mut_link;
} StgBlockingQueue;

#endif

#if defined(PAR)
/* global indirections aka FETCH_ME closures */
typedef struct StgFetchMe_ {
  StgHeader              header;
  globalAddr            *ga;        /* ptr to unique id for a closure */
  StgMutClosure         *mut_link;  /* next elem in mutable list */
} StgFetchMe;

/* same contents as an ordinary StgBlockingQueue */
typedef struct StgFetchMeBlockingQueue_ {
  StgHeader                          header;
  struct StgBlockingQueueElement_   *blocking_queue; /* start of the BQ */
  StgMutClosure                     *mut_link;       /* next elem in mutable list */
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
  StgMutClosure                    *mut_link; /* next elem in mutable list */
  StgClosure                       *node;     /* node to fetch */
  globalAddr                        ga;       /* where to send the result to */
} StgBlockedFetch;                            /* NB: not just a ptr to a GA */
#endif

#endif /* CLOSURES_H */
