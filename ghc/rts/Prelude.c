
/* -----------------------------------------------------------------------------
 * $Id: Prelude.c,v 1.7 2000/05/22 13:09:29 simonmar Exp $
 *
 * (c) The GHC Team, 1998-2000
 *
 * Prelude identifiers that we sometimes need to refer to in the RTS.
 *
 * ---------------------------------------------------------------------------*/

#include "Rts.h"
#include "Prelude.h"

#if defined(INTERPRETER)
const StgClosure *ind_True_closure;
const StgClosure *ind_False_closure;
const StgClosure *ind_unpackCString_closure;
const StgClosure *ind_runFinalizerBatch_closure;

const StgClosure *ind_stackOverflow_closure;
const StgClosure *ind_heapOverflow_closure;
const StgClosure *ind_PutFullMVar_closure;
const StgClosure *ind_BlockedOnDeadMVar_closure;
const StgClosure *ind_NonTermination_closure;

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

INFO_TABLE_CONSTR(hugs_standalone_Czh_con_info,Hugs_CONSTR_entry,
                  0,sizeofW(StgChar),0,CONSTR,,EF_,0,0);
INFO_TABLE_CONSTR(hugs_standalone_Izh_con_info,Hugs_CONSTR_entry,
                  0,sizeofW(StgInt),0,CONSTR,,EF_,0,0);
INFO_TABLE_CONSTR(hugs_standalone_I64zh_con_info,Hugs_CONSTR_entry,
                  0,sizeofW(StgInt64),0,CONSTR,,EF_,0,0);
INFO_TABLE_CONSTR(hugs_standalone_W64zh_con_info,Hugs_CONSTR_entry,
                  0,sizeofW(StgWord64),0,CONSTR,,EF_,0,0);
INFO_TABLE_CONSTR(hugs_standalone_Fzh_con_info,Hugs_CONSTR_entry,
                  0,sizeofW(StgFloat),0,CONSTR,,EF_,0,0);
INFO_TABLE_CONSTR(hugs_standalone_Dzh_con_info,Hugs_CONSTR_entry,
                  0,sizeofW(StgDouble),0,CONSTR,,EF_,0,0);
INFO_TABLE_CONSTR(hugs_standalone_Azh_con_info,Hugs_CONSTR_entry,
                  0,sizeofW(StgAddr),0,CONSTR,,EF_,0,0);
INFO_TABLE_CONSTR(hugs_standalone_Wzh_con_info,Hugs_CONSTR_entry,
                  0,sizeofW(StgWord),0,CONSTR,,EF_,0,0);
INFO_TABLE_CONSTR(hugs_standalone_StablePtr_con_info,Hugs_CONSTR_entry,
                  0,sizeofW(StgStablePtr),0,CONSTR,,EF_,0,0);

INFO_TABLE_CONSTR(hugs_standalone_Czh_static_info,Hugs_CONSTR_entry,
                  0,sizeofW(StgChar),0,CONSTR_NOCAF_STATIC,,EF_,0,0);
INFO_TABLE_CONSTR(hugs_standalone_Izh_static_info,Hugs_CONSTR_entry,
                  0,sizeofW(StgInt),0,CONSTR_NOCAF_STATIC,,EF_,0,0);
INFO_TABLE_CONSTR(hugs_standalone_I64zh_static_info,Hugs_CONSTR_entry,
                  0,sizeofW(StgInt64),0,CONSTR_NOCAF_STATIC,,EF_,0,0);
INFO_TABLE_CONSTR(hugs_standalone_Fzh_static_info,Hugs_CONSTR_entry,
                  0,sizeofW(StgFloat),0,CONSTR_NOCAF_STATIC,,EF_,0,0);
INFO_TABLE_CONSTR(hugs_standalone_Dzh_static_info,Hugs_CONSTR_entry,
                  0,sizeofW(StgDouble),0,CONSTR_NOCAF_STATIC,,EF_,0,0);
INFO_TABLE_CONSTR(hugs_standalone_Azh_static_info,Hugs_CONSTR_entry,
                  0,sizeofW(StgAddr),0,CONSTR_NOCAF_STATIC,,EF_,0,0);
INFO_TABLE_CONSTR(hugs_standalone_Wzh_static_info,Hugs_CONSTR_entry,
                  0,sizeofW(StgWord),0,CONSTR_NOCAF_STATIC,,EF_,0,0);
INFO_TABLE_CONSTR(hugs_standalone_StablePtr_static_info,Hugs_CONSTR_entry,
                  0,sizeofW(StgStablePtr),0,CONSTR_NOCAF_STATIC,,EF_,0,0);
#endif


/* Fix up references to various Prelude symbols.  For Hugs, we
   pass either NULL, to denote standalone mode, or the address of
   a lookup function which finds the specified symbol in the 
   compiled Prelude which Hugs has just loaded.
  
   In combined mode, call here when POSTPREL is signalled in link.c
   (since before that point, there are no symbols to link to).
   In standalone mode, call here at any time, preferably as early
   as possible -- when PREPREL is signalled.

   At the moment, standalone mode does not link True, False,
   PutFullMVar or NonTermination.  That might change (if we
   implement them in the Hugs standalone Prelude), but then
   we (1) need a way to ask hugs the address of the BCOs, and
   (2) this can only be done at POSTPREL time.
*/
void fixupRTStoPreludeRefs ( void*(*ask_hugs_dynamic_linker)(char*) )
{
  (void)ask_hugs_dynamic_linker;   /* keep gcc -Wall happy */
#if defined(INTERPRETER)
  if (ask_hugs_dynamic_linker == NULL) {

    /* Hugs standalone mode. */
    ind_True_closure               = NULL; /* True__closure; */
    ind_False_closure              = NULL; /* False_closure; */
    ind_runFinalizerBatch_closure  = NULL; /* runFinalizerBatch_closure; */
    ind_PutFullMVar_closure        = NULL; /* PutFullMVar_closure; */
    ind_BlockedOnDeadMVar_closure  = NULL; /* BlockedOnDeadMVar_closure; */
    ind_NonTermination_closure     = NULL; /* NonTermination_closure; */
    ind_unpackCString_closure      = NULL; /* unpackCString_closure; */

    ind_stackOverflow_closure = stackOverflow_closure;
    ind_heapOverflow_closure  = heapOverflow_closure;

    ind_Czh_static_info       = &hugs_standalone_Czh_static_info;
    ind_Izh_static_info       = &hugs_standalone_Izh_static_info;
    ind_Fzh_static_info       = &hugs_standalone_Fzh_static_info;
    ind_Dzh_static_info       = &hugs_standalone_Dzh_static_info;
    ind_Azh_static_info       = &hugs_standalone_Azh_static_info;
    ind_Wzh_static_info       = &hugs_standalone_Wzh_static_info;
    ind_Czh_con_info          = &hugs_standalone_Czh_con_info;
    ind_Izh_con_info          = &hugs_standalone_Izh_con_info;
    ind_Fzh_con_info          = &hugs_standalone_Fzh_con_info;
    ind_Dzh_con_info          = &hugs_standalone_Dzh_con_info;
    ind_Azh_con_info          = &hugs_standalone_Azh_con_info;
    ind_Wzh_con_info          = &hugs_standalone_Wzh_con_info;
    ind_I64zh_con_info        = &hugs_standalone_I64zh_con_info;
    ind_W64zh_con_info        = &hugs_standalone_W64zh_con_info;
    ind_StablePtr_static_info = &hugs_standalone_StablePtr_static_info;
    ind_StablePtr_con_info    = &hugs_standalone_StablePtr_con_info;

  } else {

    /* Hugs combined mode. */
    void*(*ask)(char*) = ask_hugs_dynamic_linker;

    ind_True_closure           
       = ask("PrelBase_True_closure");
    ind_False_closure          
       = ask("PrelBase_False_closure");
    ind_runFinalizerBatch_closure    
       = ask("PrelWeak_runFinalizzerBatch_closure");
    ind_PutFullMVar_closure    
       = ask("PrelException_PutFullMVar_closure");
    ind_BlockedOnDeadMVar_closure    
       = ask("PrelException_BlockedOnDeadMVar_closure");
    ind_NonTermination_closure 
       = ask("PrelException_NonTermination_closure");

    ind_unpackCString_closure = ask("PrelPack_unpackCString_closure");
    ind_stackOverflow_closure = ask("PrelException_stackOverflow_closure");
    ind_heapOverflow_closure  = ask("PrelException_heapOverflow_closure");

    ind_Czh_static_info       = ask("PrelBase_Czh_static_info");
    ind_Izh_static_info       = ask("PrelBase_Izh_static_info");
    ind_Fzh_static_info       = ask("PrelFloat_Fzh_static_info");
    ind_Dzh_static_info       = ask("PrelFloat_Dzh_static_info");
    ind_Azh_static_info       = ask("PrelAddr_Azh_static_info");
    ind_Wzh_static_info       = ask("PrelAddr_Wzh_static_info");
    ind_Czh_con_info          = ask("PrelBase_Czh_con_info");
    ind_Izh_con_info          = ask("PrelBase_Izh_con_info");
    ind_Fzh_con_info          = ask("PrelFloat_Fzh_con_info");
    ind_Dzh_con_info          = ask("PrelFloat_Dzh_con_info");
    ind_Azh_con_info          = ask("PrelAddr_Azh_con_info");
    ind_Wzh_con_info          = ask("PrelAddr_Wzh_con_info");
    ind_I64zh_con_info        = ask("PrelAddr_I64zh_con_info");
    ind_W64zh_con_info        = ask("PrelAddr_W64zh_con_info");
    ind_StablePtr_static_info = ask("PrelStable_StablePtr_static_info");
    ind_StablePtr_con_info    = ask("PrelStable_StablePtr_con_info");

  }
#endif

  /* When the RTS and Prelude live in separate DLLs,
     we need to patch up the char- and int-like tables
     that the RTS keeps after both DLLs have been loaded,
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
