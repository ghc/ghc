/* ----------------------------------------------------------------------------
 * $Id: Closures.h,v 1.31 2002/01/29 16:52:46 simonmar Exp $
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
    StgWord     n_args;
    StgClosure *fun;
    StgClosure *payload[FLEXIBLE_ARRAY];
} StgPAP;

typedef struct {
    StgHeader   header;
    StgWord     n_args;
    StgClosure *fun;
    StgClosure *payload[FLEXIBLE_ARRAY];
} StgAP_UPD;

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

typedef struct {
    StgHeader      header;
    StgArrWords   *instrs;	/* a pointer to an ArrWords */
    StgArrWords   *literals;	/* a pointer to an ArrWords */
    StgMutArrPtrs *ptrs;	/* a pointer to a MutArrPtrs */
    StgArrWords   *itbls;	/* a pointer to an ArrWords */
} StgBCO;

/* 
   A collective typedef for all linkable stack frames i.e.
     StgUpdateFrame, StgSeqFrame, StgCatchFrame
*/
typedef struct _StgFrame {
    StgHeader  header;
    struct _StgFrame *link;
} StgFrame;

typedef struct _StgUpdateFrame {
    StgHeader  header;
    struct _StgUpdateFrame *link;
    StgClosure *updatee;
} StgUpdateFrame;

typedef struct {
    StgHeader  header;
    struct _StgUpdateFrame *link;
} StgSeqFrame;  

typedef struct {
    StgHeader  header;
    struct _StgUpdateFrame *link;
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

/* Dynamic stack frames - these have a liveness mask in the object
 * itself, rather than in the info table.  Useful for generic heap
 * check code.
 */
 
typedef struct {
  const struct _StgInfoTable* info;
  StgWord        liveness;
  StgWord        ret_addr;
  StgWord        payload[FLEXIBLE_ARRAY];
} StgRetDyn;

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
