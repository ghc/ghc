/* -----------------------------------------------------------------------------
 *
 * (c) The University of Glasgow 2002-2004
 *
 * Declarations for things defined in AutoApply.cmm
 *
 * -------------------------------------------------------------------------- */

#ifndef APPLY_H
#define APPLY_H

// canned slow entry points, indexed by arg type (ARG_P, ARG_PP, etc.)
#ifdef IN_STG_CODE
extern RTS_PRIVATE StgWord stg_ap_stack_entries[];
#else
extern RTS_PRIVATE StgFun *stg_ap_stack_entries[];
#endif

// canned register save code for heap check failure in a function
#ifdef IN_STG_CODE
extern RTS_PRIVATE StgWord stg_stack_save_entries[];
#else
extern RTS_PRIVATE StgFun *stg_stack_save_entries[];
#endif

#endif /* APPLY_H */
