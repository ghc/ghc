/* ----------------------------------------------------------------------------
 * $Id: Closures.h,v 1.2 1998/12/02 13:20:59 simonm Exp $
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

#ifdef PROFILING

typedef struct {
   CostCentreStack *ccs;
} StgProfHeader;

#else /* !PROFILING */

typedef struct {
	/* empty */
} StgProfHeader;

#endif /* PROFILING */

/* -----------------------------------------------------------------------------
   The parallel header
   -------------------------------------------------------------------------- */

#ifdef GRAN

typedef struct {
  W_ procs;
} StgGranHeader;

#else /* !PAR */

typedef struct {
  /* empty */
} StgGranHeader;

#endif /* PAR */

/* -----------------------------------------------------------------------------
   The ticky-ticky header

   Comment from old Ticky.h:

   This is used to record if a closure has been updated but not yet
   entered. It is set when the closure is updated and cleared when
   subsequently entered.
   
   NB: It is {\em not} an ``entry count'', it is an
   ``entries-after-update count.''
   
   The commoning up of @CONST@, @CHARLIKE@ and @INTLIKE@ closures is
   turned off(?) if this is required. This has only been done for 2s
   collection.  It is done using a nasty hack which defines the
   @_Evacuate@ and @_Scavenge@ code for @CONST@, @CHARLIKE@ and @INTLIKE@
   info tables to be @_Evacuate_1@ and @_Scavenge_1_0@.
   -------------------------------------------------------------------------- */

#ifdef TICKY

typedef struct {
  W_ updated;
} StgTickyHeader;

#else /* !TICKY */

typedef struct {
	/* empty */
} StgTickyHeader;

#endif /* TICKY */

/* -----------------------------------------------------------------------------
   The full fixed-size closure header

   The size of the fixed header is the sum of the optional parts plus a single
   word for the entry code pointer.
   -------------------------------------------------------------------------- */

typedef struct {
	const struct _StgInfoTable* info;
	StgProfHeader         prof;
	StgGranHeader         par;
	StgTickyHeader        ticky;
} StgHeader;

#define FIXED_HS (sizeof(StgHeader))

/* -----------------------------------------------------------------------------
   Closure Types

   For any given closure type (defined in InfoTables.h), there is a
   corresponding structure defined below.  The name of the structure
   is obtained by concatenating the closure type with '_closure'
   -------------------------------------------------------------------------- */

/* All closures follow the generic format */

typedef struct StgClosure_ {
    StgHeader   header;
    struct StgClosure_ *payload[0];
} StgClosure;

typedef struct {
    StgHeader   header;
    StgClosure *selectee;
} StgSelector;

typedef struct {
    StgHeader   header;
    StgWord     n_args;
    StgClosure *fun;
    StgPtr      payload[0];
} StgPAP;

typedef struct {
    StgHeader   header;
    StgWord     n_args;
    StgClosure *fun;
    StgPtr      payload[0];
} StgAP_UPD;

typedef struct {
    StgHeader  header;
    StgWord    n_ptrs;
    StgWord    n_words;
    StgWord    n_instrs;
    StgPtr     payload[0];
} StgBCO;

typedef struct {
    StgHeader   header;
    StgClosure *indirectee;
} StgInd;

typedef struct {
    StgHeader   header;
    StgClosure *mut_link;
    StgClosure *indirectee;
} StgIndOldGen;

typedef struct {
    StgHeader   header;
    StgClosure *indirectee;
    StgClosure *static_link;
} StgIndStatic;

typedef struct StgCAF_ {
    StgHeader   header;
    StgClosure *body;
    StgClosure *value;
    struct StgCAF_ *link;
} StgCAF;

typedef struct {
    StgHeader  header;
    struct StgTSO_ *blocking_queue;
} StgBlackHole;

typedef struct {
    StgHeader  header;
    StgWord    words;
    StgWord    payload[0];
} StgArrWords;

typedef struct {
    StgHeader   header;
    StgWord     ptrs;
    StgClosure *payload[0];
} StgArrPtrs;

typedef struct {
    StgHeader   header;
    StgClosure *var;
} StgMutVar;

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
  
typedef struct _StgWeak {	/* Weak v */
  StgHeader header;
  StgClosure *key;
  StgClosure *value;		/* v */
  StgClosure *finaliser;
  struct _StgWeak *link;
} StgWeak;

/* Dynamic stack frames - these have a liveness mask in the object
 * itself, rather than in the info table.  Useful for generic heap
 * check code.
 */
 
typedef struct {
  StgHeader      header;
  StgWord        liveness;
  StgWord        ret_addr;
  StgWord        payload[0];
} StgRetDyn;

/* Concurrent communication objects */

typedef struct {
  StgHeader       header;
  struct StgTSO_* head;
  struct StgTSO_* tail;
  StgClosure*     value;
} StgMVar;

/* Parallel FETCH_ME closures */
#ifdef PAR
typedef struct {
  StgHeader    header;
  void        *ga;		/* type globalAddr is abstract here */
} StgFetchMe;
#endif

#endif /* CLOSURES_H */
