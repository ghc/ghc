/* -----------------------------------------------------------------------------
 * $Id: Prelude.h,v 1.16 2001/03/19 10:24:03 simonmar Exp $
 *
 * (c) The GHC Team, 1998-2001
 *
 * Prelude identifiers that we sometimes need to refer to in the RTS.
 *
 * ---------------------------------------------------------------------------*/

#ifndef PRELUDE_H
#define PRELUDE_H

/* Define canonical names so we can abstract away from the actual
 * modules these names are defined in.
 */

extern DLL_IMPORT const StgClosure PrelBase_True_closure;
extern DLL_IMPORT const StgClosure PrelBase_False_closure;
extern DLL_IMPORT const StgClosure PrelPack_unpackCString_closure;
extern DLL_IMPORT const StgClosure PrelWeak_runFinalizzerBatch_closure;
extern const StgClosure PrelMain_mainIO_closure;

extern DLL_IMPORT const StgClosure PrelIOBase_stackOverflow_closure;
extern DLL_IMPORT const StgClosure PrelIOBase_heapOverflow_closure;
extern DLL_IMPORT const StgClosure PrelIOBase_BlockedOnDeadMVar_closure;
extern DLL_IMPORT const StgClosure PrelIOBase_NonTermination_closure;

extern DLL_IMPORT const StgInfoTable PrelBase_Czh_static_info;
extern DLL_IMPORT const StgInfoTable PrelBase_Izh_static_info;
extern DLL_IMPORT const StgInfoTable PrelFloat_Fzh_static_info;
extern DLL_IMPORT const StgInfoTable PrelFloat_Dzh_static_info;
extern DLL_IMPORT const StgInfoTable Addr_Azh_static_info;
extern DLL_IMPORT const StgInfoTable PrelPtr_Ptr_static_info;
extern DLL_IMPORT const StgInfoTable PrelInt_I8zh_static_info;
extern DLL_IMPORT const StgInfoTable PrelInt_I16zh_static_info;
extern DLL_IMPORT const StgInfoTable PrelInt_I32zh_static_info;
extern DLL_IMPORT const StgInfoTable PrelInt_I64zh_static_info;
extern DLL_IMPORT const StgInfoTable PrelWord_Wzh_static_info;
extern DLL_IMPORT const StgInfoTable PrelWord_W8zh_static_info;
extern DLL_IMPORT const StgInfoTable PrelWord_W16zh_static_info;
extern DLL_IMPORT const StgInfoTable PrelWord_W32zh_static_info;
extern DLL_IMPORT const StgInfoTable PrelWord_W64zh_static_info;
extern DLL_IMPORT const StgInfoTable PrelBase_Czh_con_info;
extern DLL_IMPORT const StgInfoTable PrelBase_Izh_con_info;
extern DLL_IMPORT const StgInfoTable PrelFloat_Fzh_con_info;
extern DLL_IMPORT const StgInfoTable PrelFloat_Dzh_con_info;
extern DLL_IMPORT const StgInfoTable PrelPtr_Ptr_con_info;
extern DLL_IMPORT const StgInfoTable Addr_Azh_con_info;
extern DLL_IMPORT const StgInfoTable PrelWord_Wzh_con_info;
extern DLL_IMPORT const StgInfoTable PrelInt_I8zh_con_info;
extern DLL_IMPORT const StgInfoTable PrelInt_I16zh_con_info;
extern DLL_IMPORT const StgInfoTable PrelInt_I32zh_con_info;
extern DLL_IMPORT const StgInfoTable PrelInt_I64zh_con_info;
extern DLL_IMPORT const StgInfoTable PrelWord_W8zh_con_info;
extern DLL_IMPORT const StgInfoTable PrelWord_W16zh_con_info;
extern DLL_IMPORT const StgInfoTable PrelWord_W32zh_con_info;
extern DLL_IMPORT const StgInfoTable PrelWord_W64zh_con_info;
extern DLL_IMPORT const StgInfoTable PrelStable_StablePtr_static_info;
extern DLL_IMPORT const StgInfoTable PrelStable_StablePtr_con_info;

#define True_closure              (&PrelBase_True_closure)
#define False_closure             (&PrelBase_False_closure)
#define unpackCString_closure     (&PrelPack_unpackCString_closure)
#define runFinalizerBatch_closure (&PrelWeak_runFinalizzerBatch_closure)
#define mainIO_closure            (&PrelMain_mainIO_closure)

#define stackOverflow_closure     (&PrelIOBase_stackOverflow_closure)
#define heapOverflow_closure      (&PrelIOBase_heapOverflow_closure)
#define BlockedOnDeadMVar_closure (&PrelIOBase_BlockedOnDeadMVar_closure)
#define NonTermination_closure    (&PrelIOBase_NonTermination_closure)

#define Czh_static_info           (&PrelBase_Czh_static_info)
#define Fzh_static_info           (&PrelFloat_Fzh_static_info)
#define Dzh_static_info           (&PrelFloat_Dzh_static_info)
#define Azh_static_info           (&Addr_Azh_static_info)
#define Izh_static_info           (&PrelBase_Izh_static_info)
#define I8zh_static_info          (&PrelInt_I8zh_static_info)
#define I16zh_static_info         (&PrelInt_I16zh_static_info)
#define I32zh_static_info         (&PrelInt_I32zh_static_info)
#define I64zh_static_info         (&PrelInt_I64zh_static_info)
#define Wzh_static_info           (&PrelWord_Wzh_static_info)
#define W8zh_static_info          (&PrelWord_W8zh_static_info)
#define W16zh_static_info         (&PrelWord_W16zh_static_info)
#define W32zh_static_info         (&PrelWord_W32zh_static_info)
#define W64zh_static_info         (&PrelWord_W64zh_static_info)
#define Ptr_static_info           (&PrelPtr_Ptr_static_info)
#define Czh_con_info              (&PrelBase_Czh_con_info)
#define Izh_con_info              (&PrelBase_Izh_con_info)
#define Fzh_con_info              (&PrelFloat_Fzh_con_info)
#define Dzh_con_info              (&PrelFloat_Dzh_con_info)
#define Azh_con_info              (&Addr_Azh_con_info)
#define Wzh_con_info              (&PrelWord_Wzh_con_info)
#define W8zh_con_info             (&PrelWord_W8zh_con_info)
#define W16zh_con_info            (&PrelWord_W16zh_con_info)
#define W32zh_con_info            (&PrelWord_W32zh_con_info)
#define W64zh_con_info            (&PrelWord_W64zh_con_info)
#define I8zh_con_info             (&PrelInt_I8zh_con_info)
#define I16zh_con_info            (&PrelInt_I16zh_con_info)
#define I32zh_con_info            (&PrelInt_I32zh_con_info)
#define I64zh_con_info            (&PrelInt_I64zh_con_info)
#define I64zh_con_info            (&PrelInt_I64zh_con_info)
#define Ptr_con_info              (&PrelPtr_Ptr_con_info)
#define StablePtr_static_info     (&PrelStable_StablePtr_static_info)
#define StablePtr_con_info        (&PrelStable_StablePtr_con_info)

#endif /* PRELUDE_H */
