/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2005
 *
 * Prelude identifiers that we sometimes need to refer to in the RTS.
 *
 * ---------------------------------------------------------------------------*/

#ifndef PRELUDE_H
#define PRELUDE_H

/* These definitions are required by the RTS .cmm files too, so we
 * need declarations that we can #include into the generated .hc files.
 */
#if IN_STG_CODE
#define PRELUDE_INFO(i)       extern W_(i)[]
#define PRELUDE_CLOSURE(i)    extern W_(i)[]
#else
#define PRELUDE_INFO(i)       extern const StgInfoTable DLL_IMPORT_DATA_VARNAME(i)
#define PRELUDE_CLOSURE(i)    extern StgClosure DLL_IMPORT_DATA_VARNAME(i)
#endif

/* Define canonical names so we can abstract away from the actual
 * modules these names are defined in.
 */

PRELUDE_CLOSURE(ghczmprim_GHCziTypes_True_closure);
PRELUDE_CLOSURE(ghczmprim_GHCziTypes_False_closure);
PRELUDE_CLOSURE(base_GHCziPack_unpackCString_closure);
PRELUDE_CLOSURE(base_GHCziWeak_runFinalizzerBatch_closure);

#ifdef IN_STG_CODE
extern W_ ZCMain_main_closure[];
#else
extern StgClosure ZCMain_main_closure;
#endif

PRELUDE_CLOSURE(base_GHCziIOziException_stackOverflow_closure);
PRELUDE_CLOSURE(base_GHCziIOziException_heapOverflow_closure);
PRELUDE_CLOSURE(base_GHCziIOziException_blockedIndefinitelyOnThrowTo_closure);
PRELUDE_CLOSURE(base_GHCziIOziException_blockedIndefinitelyOnMVar_closure);
PRELUDE_CLOSURE(base_GHCziIOziException_blockedIndefinitelyOnSTM_closure);
PRELUDE_CLOSURE(base_ControlziExceptionziBase_nonTermination_closure);
PRELUDE_CLOSURE(base_ControlziExceptionziBase_nestedAtomically_closure);

PRELUDE_CLOSURE(base_GHCziConcziSync_runSparks_closure);
PRELUDE_CLOSURE(base_GHCziConcziIO_ensureIOManagerIsRunning_closure);
PRELUDE_CLOSURE(base_GHCziConcziIO_ioManagerCapabilitiesChanged_closure);
PRELUDE_CLOSURE(base_GHCziConcziSignal_runHandlers_closure);

PRELUDE_CLOSURE(base_GHCziTopHandler_flushStdHandles_closure);

PRELUDE_INFO(ghczmprim_GHCziTypes_Czh_static_info);
PRELUDE_INFO(ghczmprim_GHCziTypes_Izh_static_info);
PRELUDE_INFO(ghczmprim_GHCziTypes_Fzh_static_info);
PRELUDE_INFO(ghczmprim_GHCziTypes_Dzh_static_info);
PRELUDE_INFO(base_Addr_Azh_static_info);
PRELUDE_INFO(base_GHCziPtr_Ptr_static_info);
PRELUDE_INFO(base_GHCziPtr_FunPtr_static_info);
PRELUDE_INFO(base_GHCziInt_I8zh_static_info);
PRELUDE_INFO(base_GHCziInt_I16zh_static_info);
PRELUDE_INFO(base_GHCziInt_I32zh_static_info);
PRELUDE_INFO(base_GHCziInt_I64zh_static_info);
PRELUDE_INFO(ghczmprim_GHCziTypes_Wzh_static_info);
PRELUDE_INFO(base_GHCziWord_W8zh_static_info);
PRELUDE_INFO(base_GHCziWord_W16zh_static_info);
PRELUDE_INFO(base_GHCziWord_W32zh_static_info);
PRELUDE_INFO(base_GHCziWord_W64zh_static_info);
PRELUDE_INFO(ghczmprim_GHCziTypes_Czh_con_info);
PRELUDE_INFO(ghczmprim_GHCziTypes_Izh_con_info);
PRELUDE_INFO(ghczmprim_GHCziTypes_Fzh_con_info);
PRELUDE_INFO(ghczmprim_GHCziTypes_Dzh_con_info);
PRELUDE_INFO(base_GHCziPtr_Ptr_con_info);
PRELUDE_INFO(base_GHCziPtr_FunPtr_con_info);
PRELUDE_INFO(base_Addr_Azh_con_info);
PRELUDE_INFO(ghczmprim_GHCziTypes_Wzh_con_info);
PRELUDE_INFO(base_GHCziInt_I8zh_con_info);
PRELUDE_INFO(base_GHCziInt_I16zh_con_info);
PRELUDE_INFO(base_GHCziInt_I32zh_con_info);
PRELUDE_INFO(base_GHCziInt_I64zh_con_info);
PRELUDE_INFO(base_GHCziWord_W8zh_con_info);
PRELUDE_INFO(base_GHCziWord_W16zh_con_info);
PRELUDE_INFO(base_GHCziWord_W32zh_con_info);
PRELUDE_INFO(base_GHCziWord_W64zh_con_info);
PRELUDE_INFO(base_GHCziStable_StablePtr_static_info);
PRELUDE_INFO(base_GHCziStable_StablePtr_con_info);

#define True_closure              DLL_IMPORT_DATA_REF(ghczmprim_GHCziTypes_True_closure)
#define False_closure             DLL_IMPORT_DATA_REF(ghczmprim_GHCziTypes_False_closure)
#define unpackCString_closure     DLL_IMPORT_DATA_REF(base_GHCziPack_unpackCString_closure)
#define runFinalizerBatch_closure DLL_IMPORT_DATA_REF(base_GHCziWeak_runFinalizzerBatch_closure)
#define mainIO_closure            (&ZCMain_main_closure)

#define runSparks_closure         DLL_IMPORT_DATA_REF(base_GHCziConcziSync_runSparks_closure)
#define ensureIOManagerIsRunning_closure DLL_IMPORT_DATA_REF(base_GHCziConcziIO_ensureIOManagerIsRunning_closure)
#define runHandlers_closure       DLL_IMPORT_DATA_REF(base_GHCziConcziSignal_runHandlers_closure)

#define flushStdHandles_closure   DLL_IMPORT_DATA_REF(base_GHCziTopHandler_flushStdHandles_closure)

#define stackOverflow_closure     DLL_IMPORT_DATA_REF(base_GHCziIOziException_stackOverflow_closure)
#define heapOverflow_closure      DLL_IMPORT_DATA_REF(base_GHCziIOziException_heapOverflow_closure)
#define blockedIndefinitelyOnMVar_closure DLL_IMPORT_DATA_REF(base_GHCziIOziException_blockedIndefinitelyOnMVar_closure)
#define blockedIndefinitelyOnSTM_closure DLL_IMPORT_DATA_REF(base_GHCziIOziException_blockedIndefinitelyOnSTM_closure)
#define nonTermination_closure    DLL_IMPORT_DATA_REF(base_ControlziExceptionziBase_nonTermination_closure)
#define nestedAtomically_closure  DLL_IMPORT_DATA_REF(base_ControlziExceptionziBase_nestedAtomically_closure)

#define Czh_static_info           DLL_IMPORT_DATA_REF(ghczmprim_GHCziTypes_Czh_static_info)
#define Fzh_static_info           DLL_IMPORT_DATA_REF(ghczmprim_GHCziTypes_Fzh_static_info)
#define Dzh_static_info           DLL_IMPORT_DATA_REF(ghczmprim_GHCziTypes_Dzh_static_info)
#define Azh_static_info           DLL_IMPORT_DATA_REF(base_Addr_Azh_static_info)
#define Izh_static_info           DLL_IMPORT_DATA_REF(ghczmprim_GHCziTypes_Izh_static_info)
#define I8zh_static_info          DLL_IMPORT_DATA_REF(base_GHCziInt_I8zh_static_info)
#define I16zh_static_info         DLL_IMPORT_DATA_REF(base_GHCziInt_I16zh_static_info)
#define I32zh_static_info         DLL_IMPORT_DATA_REF(base_GHCziInt_I32zh_static_info)
#define I64zh_static_info         DLL_IMPORT_DATA_REF(base_GHCziInt_I64zh_static_info)
#define Wzh_static_info           DLL_IMPORT_DATA_REF(ghczmprim_GHCziTypes_Wzh_static_info)
#define W8zh_static_info          DLL_IMPORT_DATA_REF(base_GHCziWord_W8zh_static_info)
#define W16zh_static_info         DLL_IMPORT_DATA_REF(base_GHCziWord_W16zh_static_info)
#define W32zh_static_info         DLL_IMPORT_DATA_REF(base_GHCziWord_W32zh_static_info)
#define W64zh_static_info         DLL_IMPORT_DATA_REF(base_GHCziWord_W64zh_static_info)
#define Ptr_static_info           DLL_IMPORT_DATA_REF(base_GHCziPtr_Ptr_static_info)
#define FunPtr_static_info        DLL_IMPORT_DATA_REF(base_GHCziPtr_FunPtr_static_info)
#define Czh_con_info              DLL_IMPORT_DATA_REF(ghczmprim_GHCziTypes_Czh_con_info)
#define Izh_con_info              DLL_IMPORT_DATA_REF(ghczmprim_GHCziTypes_Izh_con_info)
#define Fzh_con_info              DLL_IMPORT_DATA_REF(ghczmprim_GHCziTypes_Fzh_con_info)
#define Dzh_con_info              DLL_IMPORT_DATA_REF(ghczmprim_GHCziTypes_Dzh_con_info)
#define Azh_con_info              DLL_IMPORT_DATA_REF(base_Addr_Azh_con_info)
#define Wzh_con_info              DLL_IMPORT_DATA_REF(ghczmprim_GHCziTypes_Wzh_con_info)
#define W8zh_con_info             DLL_IMPORT_DATA_REF(base_GHCziWord_W8zh_con_info)
#define W16zh_con_info            DLL_IMPORT_DATA_REF(base_GHCziWord_W16zh_con_info)
#define W32zh_con_info            DLL_IMPORT_DATA_REF(base_GHCziWord_W32zh_con_info)
#define W64zh_con_info            DLL_IMPORT_DATA_REF(base_GHCziWord_W64zh_con_info)
#define I8zh_con_info             DLL_IMPORT_DATA_REF(base_GHCziInt_I8zh_con_info)
#define I16zh_con_info            DLL_IMPORT_DATA_REF(base_GHCziInt_I16zh_con_info)
#define I32zh_con_info            DLL_IMPORT_DATA_REF(base_GHCziInt_I32zh_con_info)
#define I64zh_con_info            DLL_IMPORT_DATA_REF(base_GHCziInt_I64zh_con_info)
#define I64zh_con_info            DLL_IMPORT_DATA_REF(base_GHCziInt_I64zh_con_info)
#define Ptr_con_info              DLL_IMPORT_DATA_REF(base_GHCziPtr_Ptr_con_info)
#define FunPtr_con_info           DLL_IMPORT_DATA_REF(base_GHCziPtr_FunPtr_con_info)
#define StablePtr_static_info     DLL_IMPORT_DATA_REF(base_GHCziStable_StablePtr_static_info)
#define StablePtr_con_info        DLL_IMPORT_DATA_REF(base_GHCziStable_StablePtr_con_info)

#endif /* PRELUDE_H */
