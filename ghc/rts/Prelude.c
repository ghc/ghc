/* -----------------------------------------------------------------------------
 * $Id: Prelude.c,v 1.1 2000/03/14 11:11:40 simonmar Exp $
 *
 * (c) The GHC Team, 1998-2000
 *
 * Prelude identifiers that we sometimes need to refer to in the RTS.
 *
 * ---------------------------------------------------------------------------*/

#include "Rts.h"
#include "Prelude.h"

#if defined(INTERPRETER)

const StgClosure *ind_True_static_closure;
const StgClosure *ind_False_static_closure;
const StgClosure *ind_unpackCString_closure;
const StgClosure *ind_stackOverflow_closure;
const StgClosure *ind_heapOverflow_closure;
const StgClosure *ind_PutFullMVar_static_closure;
const StgClosure *ind_NonTermination_static_closure;
const StgClosure *ind_mainIO_closure;

const StgInfoTable *ind_Czh_static_info;
const StgInfoTable *ind_Izh_static_info;
const StgInfoTable *ind_Fzh_static_info;
const StgInfoTable *ind_Dzh_static_info;
const StgInfoTable *ind_Azh_static_info;
const StgInfoTable *ind_Wzh_static_info;
const StgInfoTable *ind_Czh_con_info;
const StgInfoTable *ind_Izh_con_info;
const StgInfoTable *ind_Fzh_con_info;
const StgInfoTable *ind_Dzh_con_info;
const StgInfoTable *ind_Azh_con_info;
const StgInfoTable *ind_Wzh_con_info;
const StgInfoTable *ind_I64zh_con_info;
const StgInfoTable *ind_W64zh_con_info;
const StgInfoTable *ind_StablePtr_static_info;
const StgInfoTable *ind_StablePtr_con_info;

#endif

void
fixupPreludeRefs(void)
{
#ifdef INTERPRETER
  ind_True_static_closure           = True_static_closure;
  ind_False_static_closure          = False_static_closure;
  ind_unpackCString_closure         = ind_unpackCString_closure;
  ind_stackOverflow_closure         = stackOverflow_closure;
  ind_heapOverflow_closure          = heapOverflow_closure;
  ind_PutFullMVar_static_closure    = PutFullMVar_static_closure;
  ind_NonTermination_static_closure = NonTermination_static_closure;
  ind_mainIO_closure                = mainIO_closure;

  ind_Czh_static_info               = Czh_static_info;
  ind_Izh_static_info               = Izh_static_info;
  ind_Fzh_static_info               = Fzh_static_info;
  ind_Dzh_static_info               = Dzh_static_info;
  ind_Azh_static_info               = Azh_static_info;
  ind_Wzh_static_info               = Wzh_static_info;
  ind_Czh_con_info                  = Czh_con_info;
  ind_Izh_con_info                  = Izh_con_info;
  ind_Fzh_con_info                  = Fzh_con_info;
  ind_Dzh_con_info                  = Dzh_con_info;
  ind_Azh_con_info                  = Azh_con_info;
  ind_Wzh_con_info                  = Wzh_con_info;
  ind_I64zh_con_info                = I64zh_con_info;
  ind_W64zh_con_info                = W64zh_con_info;
  ind_StablePtr_static_info         = StablePtr_static_info;
  ind_StablePtr_con_info            = StablePtr_con_info;
#endif

  /* When the RTS and Prelude live in separate DLLs,
     we need to patch up the char- and int-like tables
     that the RTS keep after both DLLs have been loaded,
     filling in the tables with references to where the
     static info tables have been loaded inside the running
     process.
  */
#if defined(INTERPRETER) || defined(ENABLE_WIN32_DLL_SUPPORT)
  {
    int i;
  
    for(i=0;i<=255;i++)
      (CHARLIKE_closure[i]).header.info = Czh_static_info;
    
    for(i=0;i<=32;i++)
      (INTLIKE_closure[i]).header.info = Izh_static_info;
  }
#endif
}
