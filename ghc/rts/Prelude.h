/* -----------------------------------------------------------------------------
 * $Id: Prelude.h,v 1.19 2002/02/12 15:17:22 simonmar Exp $
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

extern DLL_IMPORT const StgClosure GHCziBase_True_closure;
extern DLL_IMPORT const StgClosure GHCziBase_False_closure;
extern DLL_IMPORT const StgClosure GHCziPack_unpackCString_closure;
extern DLL_IMPORT const StgClosure GHCziWeak_runFinalizzerBatch_closure;
extern const StgClosure Main_zdmain_closure;

extern DLL_IMPORT const StgClosure GHCziIOBase_stackOverflow_closure;
extern DLL_IMPORT const StgClosure GHCziIOBase_heapOverflow_closure;
extern DLL_IMPORT const StgClosure GHCziIOBase_BlockedOnDeadMVar_closure;
extern DLL_IMPORT const StgClosure GHCziIOBase_NonTermination_closure;
extern DLL_IMPORT const StgClosure GHCziIOBase_Deadlock_closure;

extern DLL_IMPORT const StgInfoTable GHCziBase_Czh_static_info;
extern DLL_IMPORT const StgInfoTable GHCziBase_Izh_static_info;
extern DLL_IMPORT const StgInfoTable GHCziFloat_Fzh_static_info;
extern DLL_IMPORT const StgInfoTable GHCziFloat_Dzh_static_info;
extern DLL_IMPORT const StgInfoTable Addr_Azh_static_info;
extern DLL_IMPORT const StgInfoTable GHCziPtr_Ptr_static_info;
extern DLL_IMPORT const StgInfoTable GHCziInt_I8zh_static_info;
extern DLL_IMPORT const StgInfoTable GHCziInt_I16zh_static_info;
extern DLL_IMPORT const StgInfoTable GHCziInt_I32zh_static_info;
extern DLL_IMPORT const StgInfoTable GHCziInt_I64zh_static_info;
extern DLL_IMPORT const StgInfoTable GHCziWord_Wzh_static_info;
extern DLL_IMPORT const StgInfoTable GHCziWord_W8zh_static_info;
extern DLL_IMPORT const StgInfoTable GHCziWord_W16zh_static_info;
extern DLL_IMPORT const StgInfoTable GHCziWord_W32zh_static_info;
extern DLL_IMPORT const StgInfoTable GHCziWord_W64zh_static_info;
extern DLL_IMPORT const StgInfoTable GHCziBase_Czh_con_info;
extern DLL_IMPORT const StgInfoTable GHCziBase_Izh_con_info;
extern DLL_IMPORT const StgInfoTable GHCziFloat_Fzh_con_info;
extern DLL_IMPORT const StgInfoTable GHCziFloat_Dzh_con_info;
extern DLL_IMPORT const StgInfoTable GHCziPtr_Ptr_con_info;
extern DLL_IMPORT const StgInfoTable Addr_Azh_con_info;
extern DLL_IMPORT const StgInfoTable GHCziWord_Wzh_con_info;
extern DLL_IMPORT const StgInfoTable GHCziInt_I8zh_con_info;
extern DLL_IMPORT const StgInfoTable GHCziInt_I16zh_con_info;
extern DLL_IMPORT const StgInfoTable GHCziInt_I32zh_con_info;
extern DLL_IMPORT const StgInfoTable GHCziInt_I64zh_con_info;
extern DLL_IMPORT const StgInfoTable GHCziWord_W8zh_con_info;
extern DLL_IMPORT const StgInfoTable GHCziWord_W16zh_con_info;
extern DLL_IMPORT const StgInfoTable GHCziWord_W32zh_con_info;
extern DLL_IMPORT const StgInfoTable GHCziWord_W64zh_con_info;
extern DLL_IMPORT const StgInfoTable GHCziStable_StablePtr_static_info;
extern DLL_IMPORT const StgInfoTable GHCziStable_StablePtr_con_info;

#define True_closure              (&GHCziBase_True_closure)
#define False_closure             (&GHCziBase_False_closure)
#define unpackCString_closure     (&GHCziPack_unpackCString_closure)
#define runFinalizerBatch_closure (&GHCziWeak_runFinalizzerBatch_closure)
#define mainIO_closure            (&Main_zdmain_closure)

#define stackOverflow_closure     (&GHCziIOBase_stackOverflow_closure)
#define heapOverflow_closure      (&GHCziIOBase_heapOverflow_closure)
#define BlockedOnDeadMVar_closure (&GHCziIOBase_BlockedOnDeadMVar_closure)
#define NonTermination_closure    (&GHCziIOBase_NonTermination_closure)
#define Deadlock_closure         (&GHCziIOBase_NonTermination_closure)

#define Czh_static_info           (&GHCziBase_Czh_static_info)
#define Fzh_static_info           (&GHCziFloat_Fzh_static_info)
#define Dzh_static_info           (&GHCziFloat_Dzh_static_info)
#define Azh_static_info           (&Addr_Azh_static_info)
#define Izh_static_info           (&GHCziBase_Izh_static_info)
#define I8zh_static_info          (&GHCziInt_I8zh_static_info)
#define I16zh_static_info         (&GHCziInt_I16zh_static_info)
#define I32zh_static_info         (&GHCziInt_I32zh_static_info)
#define I64zh_static_info         (&GHCziInt_I64zh_static_info)
#define Wzh_static_info           (&GHCziWord_Wzh_static_info)
#define W8zh_static_info          (&GHCziWord_W8zh_static_info)
#define W16zh_static_info         (&GHCziWord_W16zh_static_info)
#define W32zh_static_info         (&GHCziWord_W32zh_static_info)
#define W64zh_static_info         (&GHCziWord_W64zh_static_info)
#define Ptr_static_info           (&GHCziPtr_Ptr_static_info)
#define Czh_con_info              (&GHCziBase_Czh_con_info)
#define Izh_con_info              (&GHCziBase_Izh_con_info)
#define Fzh_con_info              (&GHCziFloat_Fzh_con_info)
#define Dzh_con_info              (&GHCziFloat_Dzh_con_info)
#define Azh_con_info              (&Addr_Azh_con_info)
#define Wzh_con_info              (&GHCziWord_Wzh_con_info)
#define W8zh_con_info             (&GHCziWord_W8zh_con_info)
#define W16zh_con_info            (&GHCziWord_W16zh_con_info)
#define W32zh_con_info            (&GHCziWord_W32zh_con_info)
#define W64zh_con_info            (&GHCziWord_W64zh_con_info)
#define I8zh_con_info             (&GHCziInt_I8zh_con_info)
#define I16zh_con_info            (&GHCziInt_I16zh_con_info)
#define I32zh_con_info            (&GHCziInt_I32zh_con_info)
#define I64zh_con_info            (&GHCziInt_I64zh_con_info)
#define I64zh_con_info            (&GHCziInt_I64zh_con_info)
#define Ptr_con_info              (&GHCziPtr_Ptr_con_info)
#define StablePtr_static_info     (&GHCziStable_StablePtr_static_info)
#define StablePtr_con_info        (&GHCziStable_StablePtr_con_info)

#endif /* PRELUDE_H */
