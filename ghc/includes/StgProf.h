/* -----------------------------------------------------------------------------
 * $Id: StgProf.h,v 1.7 1999/09/15 13:45:14 simonmar Exp $
 *
 * (c) The GHC Team, 1998
 *
 * Macros for profiling operations in STG code
 * ---------------------------------------------------------------------------*/

#ifndef STGPROF_H
#define STGPROF_H

#if defined(PROFILING)
  
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

extern F_ *register_stack;

extern CostCentre *CC_LIST;               /* registered CC list */
extern CostCentreStack *CCS_LIST;         /* registered CCS list */

# define PUSH_REGISTER_STACK(reg_function)				\
	*(register_stack++) = (F_)reg_function

# define POP_REGISTER_STACK()						\
	*(--register_stack)

# define START_REGISTER_CCS(reg_mod_name)				\
	static int _module_registered = 0;				\
	FN_(reg_mod_name) {						\
	    FB_;							\
	    if (! _module_registered) { 				\
		_module_registered = 1

# define REGISTER_IMPORT(reg_mod_name)					\
	do { EF_(reg_mod_name);						\
	  PUSH_REGISTER_STACK(reg_mod_name) ;				\
	} while (0)
	
# define END_REGISTER_CCS()						\
        };								\
	JMP_(POP_REGISTER_STACK());					\
	FE_ }

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

# define CC_DECLARE(cc_ident,name,module,group,subsumed,is_local)	\
     is_local CostCentre cc_ident[1]					\
	= {{ 0,								\
	     name,							\
	     module,							\
	     group,							\
             0,								\
	     0,								\
	     subsumed,							\
	     0 }};

# define CCS_DECLARE(ccs_ident,cc_ident,subsumed,is_local)	\
     is_local CostCentreStack ccs_ident[1]			\
       = {{ ccsID 		: 0,				\
	    cc 			: cc_ident,			\
	    prevStack 		: NULL,				\
	    indexTable 		: NULL,				\
	    scc_count 		: 0,				\
	    sub_scc_count 	: 0,				\
	    sub_cafcc_count 	: 0,				\
	    time_ticks 		: 0,				\
	    mem_alloc 		: 0,				\
	    mem_resid 		: 0,				\
	    root 		: 0,				\
       }};

# define CC_EXTERN(cc_ident) \
    extern CostCentre cc_ident[];

/* -----------------------------------------------------------------------------
 * Time / Allocation Macros
 * ---------------------------------------------------------------------------*/

#define CCS_TICK(ccs)  (ccs)->time_ticks++

/* eliminate profiling overhead from allocation costs */
#define CCS_ALLOC(ccs, size) (ccs)->mem_alloc += ((size)-sizeofW(StgProfHeader))

/* For grabbing the cost centre from a closure */
#define CCS_HDR(closure)   ((StgClosure *)(closure))->header.prof.ccs

/* Restore the CCCS from a stack frame.
 * (addr should always be Sp->header.prof.ccs) 
 */
#define RESTORE_CCCS(addr)   (CCCS = (CostCentreStack *)(addr))

/* -----------------------------------------------------------------------------
 * Pushing a new cost centre (i.e. for scc annotations)
 * -------------------------------------------------------------------------- */

# define SET_CCC_X(cc,do_subcc_count,do_scc_count)		\
	do {							\
	if (do_subcc_count)   { CCCS->sub_scc_count++; }	\
	CCCS = PushCostCentre(CCCS,cc);				\
	if (do_scc_count)     { CCCS->scc_count++; }		\
	} while(0)

/* We sometimes don't increment the scc_count field, for example when
 * this scc has been placed by the compiler on an expression it
 * floated outside the main scc annotation.
 */

# define SET_CCC(cc_ident,do_scc_count) \
	 SET_CCC_X(cc_ident,do_scc_count,do_scc_count)

# define SET_CCS_TOP(cc_ident) \
	 SET_CCC_X(cc_ident,0,1)

/* -----------------------------------------------------------------------------
 * Allocating new cost centres / cost centre stacks.
 * -------------------------------------------------------------------------- */

#define ASSIGN_CC_ID(ccID)                \
        do {                              \
        ccID = CC_ID;                     \
        CC_ID++;                          \
        } while(0)

#define ASSIGN_CCS_ID(ccsID)              \
        do {                              \
        ccsID = CCS_ID;                   \
        CCS_ID++;                         \
        } while(0)

#define ASSIGN_HP_ID(hpID)                \
        do {                              \
        hpID = HP_ID;                     \
        HP_ID++;                          \
        } while(0)

#define SET_STATS_TO_ZERO(stack)          \
        do {                              \
        (stack)->scc_count = 0;           \
        (stack)->time_ticks = 0;          \
        (stack)->sub_cafcc_count = 0;     \
        (stack)->mem_alloc = 0;           \
        } while(0)

/* -----------------------------------------------------------------------------
 * Setting the cost centre when we enter a closure
 * -------------------------------------------------------------------------- */

#if defined(PROFILING_DETAIL_COUNTS)
#define CCCS_DETAIL_COUNT(inc_this) ((inc_this)++)
#else
#define CCCS_DETAIL_COUNT(inc_this) /*nothing*/
#endif

#define IS_CAF_OR_SUB_CCS(ccs)			\
        /* tests for lower case character */	\
        ((ccs)->is_subsumed & ' ')
	

/* On entry to top level CAFs we count the scc ...*/

#define ENTER_CCS_CAF_X(ccs)                                \
        do {                                                \
        /* inc subcaf count of CCCS */                      \
        CCCS->sub_cafcc_count++;                            \
        /* set CCCS to ident ccs */                         \
        CCCS = (CostCentreStack *)(ccs);                    \
        /* inc scc count of CAF ccs */                      \
        CCCS->scc_count++;                                  \
        } while(0)
 
#define ENTER_CCS_CAF(ccs_ident)   ENTER_CCS_CAF_X(ccs_ident)
#define ENTER_CCS_CAF_CL(closure)  ENTER_CCS_CAF_X(CCS_HDR(closure))

/* ----------------------------------------------------------------------------
 * Entering a Thunk
 *
 * On entering a closure we only count the enter to thunks ...
 * ------------------------------------------------------------------------- */

#define ENTER_CCS_T(ccs)				\
        do {						\
        CCCS = (CostCentreStack *)(ccs);		\
        CCCS_DETAIL_COUNT(CCCS->thunk_count);		\
        } while(0)      
 
#define ENTER_CCS_TCL(closure)  ENTER_CCS_T(CCS_HDR(closure))
 
/* -----------------------------------------------------------------------------
 * Entering a function
 *
 * Here is our special "hybrid" case when we do *not* set the CCCS.
 *  (a) The closure is a function, not a thunk;
 *  (b) The CCS is CAF-ish.
 * -------------------------------------------------------------------------- */

#define ENTER_CCS_F(stack)						\
        do {								\
        CostCentreStack *ccs = (CostCentreStack *) (stack);		\
        CCCS_DETAIL_COUNT(CCCS->function_count);			\
        CCCS = EnterFunCCS(CCCS,ccs);				  	\
        } while(0)
 
#define ENTER_CCS_FCL(closure)  ENTER_CCS_F(CCS_HDR(closure))

/* Entering a top-level function: costs are subsumed by the caller 
 */
#define ENTER_CCS_FSUB()				\
        do {						\
        CCCS_DETAIL_COUNT(CCCS->subsumed_fun_count);	\
        CCCS_DETAIL_COUNT(CCCS->function_count);	\
	entering_PAP = 0;				\
        } while(0)
 
#define ENTER_CCS_FCAF(stack)					\
        do {							\
        CostCentreStack *ccs = (CostCentreStack *) (stack);	\
        CCCS_DETAIL_COUNT(ccs->caffun_subsumed);		\
        CCCS_DETAIL_COUNT(CCCS->subsumed_caf_count);		\
        CCCS_DETAIL_COUNT(CCCS->function_count);		\
	entering_PAP = 0;					\
        } while(0)
 
#define ENTER_CCS_FLOAD(ccs)                                \
        do {                                                \
        CCCS = (CostCentreStack *)(ccs);                    \
        CCCS_DETAIL_COUNT(CCCS->function_count);            \
        } while(0)
 
/* These ENTER_CC_PAP things are only used in the RTS */
 
#define ENTER_CCS_PAP(stack) 			\
        do {					\
	ENTER_CCS_F(stack);			\
	entering_PAP = rtsTrue;			\
	} while(0)

#define ENTER_CCS_PAP_CL(closure)  \
        ENTER_CCS_PAP((closure)->header.prof.ccs)

 /* temp EW */
#define STATIC_CCS_REF(ccs) (ccs)

#endif /* PROFILING */

#endif /* STGPROF_H */
