/* -----------------------------------------------------------------------------
 * $Id: Prelude.h,v 1.7 2000/03/28 10:10:17 andy Exp $
 *
 * (c) The GHC Team, 1998-2000
 *
 * Prelude identifiers that we sometimes need to refer to in the RTS.
 *
 * ---------------------------------------------------------------------------*/

#ifndef PRELUDE_H
#define PRELUDE_H

/* Define canonical names so we can abstract away from the actual
 * module these names are defined in.
 */

#ifndef INTERPRETER
extern DLL_IMPORT const StgClosure PrelBase_True_closure;
extern DLL_IMPORT const StgClosure PrelBase_False_closure;
extern DLL_IMPORT const StgClosure PrelPack_unpackCString_closure;
extern DLL_IMPORT const StgClosure PrelException_stackOverflow_closure;
extern DLL_IMPORT const StgClosure PrelException_heapOverflow_closure;
extern const StgClosure PrelMain_mainIO_closure;

extern DLL_IMPORT const StgClosure PrelException_PutFullMVar_closure;
extern DLL_IMPORT const StgClosure PrelException_BlockedOnDeadMVar_closure;
extern DLL_IMPORT const StgClosure PrelException_NonTermination_closure;

extern DLL_IMPORT const StgInfoTable PrelBase_Czh_static_info;
extern DLL_IMPORT const StgInfoTable PrelBase_Izh_static_info;
extern DLL_IMPORT const StgInfoTable PrelFloat_Fzh_static_info;
extern DLL_IMPORT const StgInfoTable PrelFloat_Dzh_static_info;
extern DLL_IMPORT const StgInfoTable PrelAddr_Azh_static_info;
extern DLL_IMPORT const StgInfoTable PrelAddr_Wzh_static_info;
extern DLL_IMPORT const StgInfoTable PrelBase_Czh_con_info;
extern DLL_IMPORT const StgInfoTable PrelBase_Izh_con_info;
extern DLL_IMPORT const StgInfoTable PrelFloat_Fzh_con_info;
extern DLL_IMPORT const StgInfoTable PrelFloat_Dzh_con_info;
extern DLL_IMPORT const StgInfoTable PrelAddr_Azh_con_info;
extern DLL_IMPORT const StgInfoTable PrelAddr_Wzh_con_info;
extern DLL_IMPORT const StgInfoTable PrelAddr_I64zh_con_info;
extern DLL_IMPORT const StgInfoTable PrelAddr_W64zh_con_info;
extern DLL_IMPORT const StgInfoTable PrelStable_StablePtr_static_info;
extern DLL_IMPORT const StgInfoTable PrelStable_StablePtr_con_info;

#define True_closure           (&PrelBase_True_closure)
#define False_closure          (&PrelBase_False_closure)
#define stackOverflow_closure  (&PrelException_stackOverflow_closure)
#define heapOverflow_closure   (&PrelException_heapOverflow_closure)
#define PutFullMVar_closure    (&PrelException_PutFullMVar_closure)
#define BlockedOnDeadMVar_closure (&PrelException_BlockedOnDeadMVar_closure)
#define NonTermination_closure (&PrelException_NonTermination_closure)
#define Czh_static_info        (&PrelBase_Czh_static_info)
#define Izh_static_info        (&PrelBase_Izh_static_info)
#define Fzh_static_info        (&PrelFloat_Fzh_static_info)
#define Dzh_static_info        (&PrelFloat_Dzh_static_info)
#define Azh_static_info        (&PrelAddr_Azh_static_info)
#define Wzh_static_info        (&PrelAddr_Wzh_static_info)
#define Czh_con_info           (&PrelBase_Czh_con_info)
#define Izh_con_info           (&PrelBase_Izh_con_info)
#define Fzh_con_info           (&PrelFloat_Fzh_con_info)
#define Dzh_con_info           (&PrelFloat_Dzh_con_info)
#define Azh_con_info           (&PrelAddr_Azh_con_info)
#define Wzh_con_info           (&PrelAddr_Wzh_con_info)
#define W64zh_con_info         (&PrelAddr_W64zh_con_info)
#define I64zh_con_info         (&PrelAddr_I64zh_con_info)
#define StablePtr_static_info  (&PrelStable_StablePtr_static_info)
#define StablePtr_con_info     (&PrelStable_StablePtr_con_info)
#define mainIO_closure         (&PrelMain_mainIO_closure)
#define unpackCString_closure  (&PrelPack_unpackCString_closure)

#else /* INTERPRETER */

/* We need indirections to the Prelude stuff, because we can't link
 * these symbols statically.
 */
extern const StgClosure *ind_True_static_closure;
extern const StgClosure *ind_False_static_closure;
extern const StgClosure *ind_unpackCString_closure;
extern const StgClosure *ind_stackOverflow_closure;
extern const StgClosure *ind_heapOverflow_closure;
extern const StgClosure *ind_PutFullMVar_static_closure;
extern const StgClosure *ind_BlockedOnDeadMVar_closure;
extern const StgClosure *ind_NonTermination_closure;

extern const StgInfoTable *ind_Czh_static_info;
extern const StgInfoTable *ind_Izh_static_info;
extern const StgInfoTable *ind_Fzh_static_info;
extern const StgInfoTable *ind_Dzh_static_info;
extern const StgInfoTable *ind_Azh_static_info;
extern const StgInfoTable *ind_Wzh_static_info;
extern const StgInfoTable *ind_Czh_con_info;
extern const StgInfoTable *ind_Izh_con_info;
extern const StgInfoTable *ind_Fzh_con_info;
extern const StgInfoTable *ind_Dzh_con_info;
extern const StgInfoTable *ind_Azh_con_info;
extern const StgInfoTable *ind_Wzh_con_info;
extern const StgInfoTable *ind_I64zh_con_info;
extern const StgInfoTable *ind_W64zh_con_info;
extern const StgInfoTable *ind_StablePtr_static_info;
extern const StgInfoTable *ind_StablePtr_con_info;

#define True_closure           ind_True_static_closure
#define False_closure          ind_False_static_closure
#define stackOverflow_closure  ind_stackOverflow_closure
#define heapOverflow_closure   ind_heapOverflow_closure
#define PutFullMVar_closure    ind_PutFullMVar_static_closure
#define BlockedOnDeadMVar_closure ind_BlockedOnDeadMVar_closure
#define NonTermination_closure ind_NonTermination_closure
#define Czh_static_info        ind_Czh_static_info
#define Izh_static_info        ind_Izh_static_info
#define Fzh_static_info        ind_Fzh_static_info
#define Dzh_static_info        ind_Dzh_static_info
#define Azh_static_info        ind_Azh_static_info
#define Wzh_static_info        ind_Wzh_static_info
#define Czh_con_info           ind_Czh_con_info
#define Izh_con_info           ind_Izh_con_info
#define Fzh_con_info           ind_Fzh_con_info
#define Dzh_con_info           ind_Dzh_con_info
#define Azh_con_info           ind_Azh_con_info
#define Wzh_con_info           ind_Wzh_con_info
#define W64zh_con_info         ind_W64zh_con_info
#define I64zh_con_info         ind_I64zh_con_info
#define StablePtr_static_info  ind_StablePtr_static_info
#define StablePtr_con_info     ind_StablePtr_con_info
#define unpackCString_closure  ind_unpackCString_closure

#endif

void fixupRTStoPreludeRefs( void*(*)(char*) );

#endif /* PRELUDE_H */
