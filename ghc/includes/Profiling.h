/* -----------------------------------------------------------------------------
 * $Id: Profiling.h,v 1.7 2000/02/29 16:58:08 simonmar Exp $
 *
 * (c) The GHC Team, 1998-1999
 *
 * Cost-Centre Stack Profiling Include
 *
 * ---------------------------------------------------------------------------*/


#ifndef PROFILING_H
#define PROFILING_H

#if !defined(PROFILING)
  
#define CCS_ALLOC(ccs, amount) doNothing()
#define ENTER_CC_PAP_CL(r)     doNothing()
#define ENTER_CCS_PAP_CL(r)    doNothing()
 
#else /* PROFILING... */

/* -----------------------------------------------------------------------------
 * Constants
 * ---------------------------------------------------------------------------*/

#define EMPTY_STACK NULL
#define EMPTY_TABLE NULL

/* Constants used to set sumbsumed flag on CostCentres */

#define CC_IS_CAF      'c'            /* 'c'  => *is* a CAF cc           */
#define CC_IS_SUBSUMED 's'            /* 's'  => *is* a subsumed cc      */
#define CC_IS_BORING   'B'            /* 'B'  => *not* a CAF/sub cc      */

/* Constants used for abreviated output of data in binary format.  The order
 * is important and corresponds to the "item" elementType in the XML log 
 * description.   */

#define END_TAG 0 
#define CC_TAG 1
#define CCS_TAG 2
#define TYPE_CON_TAG 3
#define HEAP_OBJ_TAG 4
#define TIME_UPDATE_TAG 5
#define HEAP_UPDATE_TAG 6


/* -----------------------------------------------------------------------------
 * Data Structures 
 * ---------------------------------------------------------------------------*/  
/* 
 * CostCentre 
 */

typedef struct _CostCentre {
  int ccID;

  char *label;
  char *module;
  char *group;
 
  /* used for accumulating costs at the end of the run... */
  unsigned long time_ticks;
  unsigned long mem_alloc;

  char is_subsumed;

  struct _CostCentre *link;
} CostCentre;


	
/* 
 * CostCentreStack 
 */

typedef struct _CostCentreStack {
  int ccsID;

  CostCentre *cc;
  struct _CostCentreStack *prevStack;
  struct _IndexTable *indexTable;
  
  unsigned long scc_count;
  unsigned long sub_scc_count;
  unsigned long sub_cafcc_count;
    
  unsigned long time_ticks;
  unsigned long mem_alloc;
  unsigned long mem_resid;

  CostCentre *root;
} CostCentreStack;



/* 
 * IndexTable 
 */

typedef struct _IndexTable {
  CostCentre *cc;
  CostCentreStack *ccs;
  struct _IndexTable *next;
  unsigned int back_edge;
} IndexTable;

     
/* -----------------------------------------------------------------------------
   Pre-defined cost centres and cost centre stacks
   -------------------------------------------------------------------------- */

extern CostCentreStack *CCCS;	        /* current CCS */
 
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

extern CostCentre      CC_DONTZuCARE[];
extern CostCentreStack CCS_DONTZuCARE[]; /* shouldn't ever get set */

extern unsigned int CC_ID;	/* global id's */
extern unsigned int CCS_ID;
extern unsigned int HP_ID;

extern unsigned int interval_ticks;
extern unsigned int earlier_ticks;

typedef unsigned int hash_t;
extern hash_t time_intervals;

/* In RtsFlags.c, these are used to specify how to hash the data for 
 * output.  None of this is necessary now since the viewer will be in 
 * charge of ordering and displaying output.  */
extern hash_t max_cc_no;                        /* Hash on CC ptr */
extern hash_t max_mod_no;                       /* Hash on CC module */
extern hash_t max_grp_no;                       /* Hash on CC group */
extern hash_t max_descr_no;                     /* Hash on closure description */
extern hash_t max_type_no;                      /* Hash on type description */

/* -----------------------------------------------------------------------------
 * Functions 
 * ---------------------------------------------------------------------------*/

CostCentreStack *EnterFunCCS ( CostCentreStack *cccs, CostCentreStack *ccsfn );
CostCentreStack *PushCostCentre ( CostCentreStack *, CostCentre * );
CostCentreStack *AppendCCS ( CostCentreStack *ccs1, CostCentreStack *ccs2 );

extern unsigned int entering_PAP;

#endif /* PROFILING */

#endif PROFILING_H
