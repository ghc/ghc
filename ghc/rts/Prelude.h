/* -----------------------------------------------------------------------------
 * $Id: Prelude.h,v 1.1 2000/03/13 10:53:56 simonmar Exp $
 *
 * (c) The GHC Team, 1998-1999
 *
 * Prelude identifiers that we sometimes need to refer to in the RTS.
 *
 * ---------------------------------------------------------------------------*/

#ifndef PRELUDE_H
#define PRELUDE_H

#ifdef COMPILING_RTS

#ifdef COMPILER
extern DLL_IMPORT const StgClosure PrelBase_Z91Z93_static_closure;
extern DLL_IMPORT const StgClosure PrelBase_Z40Z41_static_closure;
extern DLL_IMPORT const StgClosure PrelBase_True_static_closure;
extern DLL_IMPORT const StgClosure PrelBase_False_static_closure;
extern DLL_IMPORT const StgClosure PrelPack_unpackCString_closure;
extern DLL_IMPORT const StgClosure PrelException_stackOverflow_closure;
extern DLL_IMPORT const StgClosure PrelException_heapOverflow_closure;
extern DLL_IMPORT const StgClosure PrelException_PutFullMVar_static_closure;
extern DLL_IMPORT const StgClosure PrelException_NonTermination_static_closure;
extern const StgClosure PrelMain_mainIO_closure;

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

/* Define canonical names so we can abstract away from the actual
 * module these names are defined in.
 */

#define Nil_closure            PrelBase_ZMZN_static_closure
#define Unit_closure           PrelBase_Z0T_static_closure
#define True_closure           PrelBase_True_static_closure
#define False_closure          PrelBase_False_static_closure
#define stackOverflow_closure  PrelException_stackOverflow_closure
#define heapOverflow_closure   PrelException_heapOverflow_closure
#define PutFullMVar_closure    PrelException_PutFullMVar_static_closure
#define NonTermination_closure PrelException_NonTermination_static_closure
#define Czh_static_info        PrelBase_Czh_static_info
#define Izh_static_info        PrelBase_Izh_static_info
#define Fzh_static_info        PrelFloat_Fzh_static_info
#define Dzh_static_info        PrelFloat_Dzh_static_info
#define Azh_static_info        PrelAddr_Azh_static_info
#define Wzh_static_info        PrelAddr_Wzh_static_info
#define Czh_con_info           PrelBase_Czh_con_info
#define Izh_con_info           PrelBase_Izh_con_info
#define Fzh_con_info           PrelFloat_Fzh_con_info
#define Dzh_con_info           PrelFloat_Dzh_con_info
#define Azh_con_info           PrelAddr_Azh_con_info
#define Wzh_con_info           PrelAddr_Wzh_con_info
#define W64zh_con_info         PrelAddr_W64zh_con_info
#define I64zh_con_info         PrelAddr_I64zh_con_info
#define StablePtr_static_info  PrelStable_StablePtr_static_info
#define StablePtr_con_info     PrelStable_StablePtr_con_info

#define mainIO_closure         PrelMain_mainIO_closure
#define unpackCString_closure  PrelPack_unpackCString_closure

#else /* INTERPRETER, I guess */

extern const StgInfoTable Czh_con_info;
extern const StgInfoTable Izh_con_info;
extern const StgInfoTable I64zh_con_info;
extern const StgInfoTable Fzh_con_info;
extern const StgInfoTable Dzh_con_info;
extern const StgInfoTable Azh_con_info;
extern const StgInfoTable Wzh_con_info;
extern const StgInfoTable StablePtr_con_info;

extern const StgInfoTable Czh_static_info;
extern const StgInfoTable Izh_static_info;
extern const StgInfoTable I64zh_static_info;
extern const StgInfoTable Fzh_static_info;
extern const StgInfoTable Dzh_static_info;
extern const StgInfoTable Azh_static_info;
extern const StgInfoTable Wzh_static_info;
extern const StgInfoTable StablePtr_static_info;

#define W64zh_con_info        I64zh_con_info
#define W64zh_static_info     I64zh_con_info

#endif

#endif /* COMPILING_RTS */

#endif /* PRELUDE_H */
