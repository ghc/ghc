/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2004
 *
 * Macros for profiling operations in STG code
 *
 * ---------------------------------------------------------------------------*/

#ifndef STGPROF_H
#define STGPROF_H

/* -----------------------------------------------------------------------------
 * Data Structures 
 * ---------------------------------------------------------------------------*/  
// NB. be careful to avoid unwanted padding between fields, by
// putting the 8-byte fields on an 8-byte boundary.  Padding can
// vary between C compilers, and we don't take into account any
// possible padding when generating CCS and CC decls in the code
// generator (compiler/codeGen/CgProf.hs).

typedef struct _CostCentre {
  StgInt ccID;

  char * label;
  char * module;
 
  /* used for accumulating costs at the end of the run... */
  StgWord   time_ticks;
  StgWord64 mem_alloc;      // align 8 (see above)

  StgInt    is_caf;

  struct _CostCentre *link;
} CostCentre;

typedef struct _CostCentreStack {
  StgInt ccsID;

  CostCentre *cc;
  struct _CostCentreStack *prevStack;
  struct _IndexTable *indexTable;

  StgWord64  scc_count;       // align 8 (see above)
  StgWord    selected;
  StgWord    time_ticks;
  StgWord64  mem_alloc;       // align 8 (see above)
  StgWord64  inherited_alloc; // align 8 (see above)
  StgWord    inherited_ticks;

  CostCentre *root;
} CostCentreStack;


/* -----------------------------------------------------------------------------
 * The rest is PROFILING only...
 * ---------------------------------------------------------------------------*/

#if defined(PROFILING)
  
/* -----------------------------------------------------------------------------
 * Constants
 * ---------------------------------------------------------------------------*/

#define EMPTY_STACK NULL
#define EMPTY_TABLE NULL

/* Constants used to set sumbsumed flag on CostCentres */

#define CC_IS_CAF      'c'            /* 'c'  => *is* a CAF cc           */
#define CC_IS_BORING   'B'            /* 'B'  => *not* a CAF/sub cc      */


/* -----------------------------------------------------------------------------
 * Data Structures
 * ---------------------------------------------------------------------------*/

typedef struct _IndexTable {
  CostCentre *cc;
  CostCentreStack *ccs;
  struct _IndexTable *next;
  unsigned int back_edge;
} IndexTable;

     
/* -----------------------------------------------------------------------------
   Pre-defined cost centres and cost centre stacks
   -------------------------------------------------------------------------- */

extern CostCentreStack * RTS_VAR(CCCS);	        /* current CCS */
 
#if IN_STG_CODE

extern StgWord CC_MAIN[];	
extern StgWord CCS_MAIN[];      /* Top CCS */

extern StgWord CC_SYSTEM[];	
extern StgWord CCS_SYSTEM[];    /* RTS costs */

extern StgWord CC_GC[];
extern StgWord CCS_GC[];	 /* Garbage collector costs */

extern StgWord CC_SUBSUMED[];	
extern StgWord CCS_SUBSUMED[];   /* Costs are subsumed by caller */

extern StgWord CC_OVERHEAD[];
extern StgWord CCS_OVERHEAD[];   /* Profiling overhead */

extern StgWord CC_DONT_CARE[];
extern StgWord CCS_DONT_CARE[];  /* shouldn't ever get set */

#else

extern CostCentre      CC_MAIN[];	
extern CostCentreStack CCS_MAIN[];      /* Top CCS */

extern CostCentre      CC_SYSTEM[];	
extern CostCentreStack CCS_SYSTEM[];    /* RTS costs */

extern CostCentre      CC_GC[];
extern CostCentreStack CCS_GC[];	 /* Garbage collector costs */

extern CostCentre      CC_SUBSUMED[];	
extern CostCentreStack CCS_SUBSUMED[];   /* Costs are subsumed by caller */

extern CostCentre      CC_OVERHEAD[];
extern CostCentreStack CCS_OVERHEAD[];   /* Profiling overhead */

extern CostCentre      CC_DONT_CARE[];
extern CostCentreStack CCS_DONT_CARE[];  /* shouldn't ever get set */

#endif // IN_STG_CODE

extern unsigned int RTS_VAR(CC_ID);	/* global ids */
extern unsigned int RTS_VAR(CCS_ID);
extern unsigned int RTS_VAR(HP_ID);

extern unsigned int RTS_VAR(era);

/* -----------------------------------------------------------------------------
 * Functions 
 * ---------------------------------------------------------------------------*/

void EnterFunCCS ( CostCentreStack *ccsfn );
CostCentreStack *PushCostCentre ( CostCentreStack *, CostCentre * );
CostCentreStack *AppendCCS ( CostCentreStack *ccs1, CostCentreStack *ccs2 );

extern unsigned int RTS_VAR(entering_PAP);

/* -----------------------------------------------------------------------------
 * Registering CCs
 
 Cost centres are registered at startup by calling a registering
 routine in each module. Each module registers its cost centres and
 calls the registering routine for all imported modules. The RTS calls
 the registering routine for the module Main. This registering must be
 done before initialisation since the evaluation required for
 initialisation may use the cost centres.
 
 As the code for each module uses tail calls we use an auxiliary stack
 (in the heap) to record imported modules still to be registered. At
 the bottom of the stack is NULL which indicates that
 @miniInterpretEnd@ should be resumed.
 
 @START_REGISTER@ and @END_REGISTER@ are special macros used to
 delimit the function. @END_REGISTER@ pops the next registering
 routine off the stack and jumps to it. @REGISTER_CC@ registers a cost
 centre. @REGISTER_IMPORT@ pushes a modules registering routine onto
 the register stack.

 -------------------------------------------------------------------------- */

extern CostCentre * RTS_VAR(CC_LIST);               /* registered CC list */
extern CostCentreStack * RTS_VAR(CCS_LIST);         /* registered CCS list */

#define REGISTER_CC(cc)					\
	do {						\
	extern CostCentre cc[];				\
	if ((cc)->link == (CostCentre *)0) {		\
	    (cc)->link = CC_LIST;			\
	    CC_LIST = (cc);				\
	    (cc)->ccID = CC_ID++; 			\
	}} while(0)

#define REGISTER_CCS(ccs)				\
	do {						\
	extern CostCentreStack ccs[];			\
        if ((ccs)->prevStack == (CostCentreStack *)0) {	\
	  (ccs)->prevStack = CCS_LIST;			\
	  CCS_LIST = (ccs);				\
	  (ccs)->ccsID = CCS_ID++;			\
	}} while(0)

/* -----------------------------------------------------------------------------
 * Declaring Cost Centres & Cost Centre Stacks.
 * -------------------------------------------------------------------------- */

# define CC_DECLARE(cc_ident,name,module,caf,is_local)		\
     is_local CostCentre cc_ident[1]				\
	= {{ 0,							\
	     name,						\
	     module,						\
             0,							\
	     0,							\
	     caf,						\
	     0 }};

# define CCS_DECLARE(ccs_ident,cc_ident,is_local)		\
     is_local CostCentreStack ccs_ident[1]			\
       = {{ ccsID 		: 0,				\
	    cc 			: cc_ident,			\
	    prevStack 		: NULL,				\
	    indexTable 		: NULL,				\
            selected            : 0,				\
	    scc_count 		: 0,				\
	    time_ticks 		: 0,				\
	    mem_alloc 		: 0,				\
	    inherited_ticks 	: 0,				\
	    inherited_alloc	: 0,				\
	    root 		: 0,				\
       }};

/* -----------------------------------------------------------------------------
 * Time / Allocation Macros
 * ---------------------------------------------------------------------------*/

/* eliminate profiling overhead from allocation costs */
#define CCS_ALLOC(ccs, size) (ccs)->mem_alloc += ((size)-sizeofW(StgProfHeader))

#else /* !PROFILING */

#define CCS_ALLOC(ccs, amount) doNothing()
 
#endif /* PROFILING */

#endif /* STGPROF_H */

