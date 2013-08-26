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
#define PRELUDE_CLOSURE(i)    extern W_(i ## _static_closure)[]
#else
#define PRELUDE_INFO(i)       extern const StgInfoTable DLL_IMPORT_DATA_VARNAME(i)
#define PRELUDE_CLOSURE(i)    extern StgClosure DLL_IMPORT_DATA_VARNAME(i ## _static_closure)
#endif

/* Define canonical names so we can abstract away from the actual
 * modules these names are defined in.
 */

PRELUDE_CLOSURE(ghczmprim_GHCziTypes_True);
PRELUDE_CLOSURE(ghczmprim_GHCziTypes_False);
PRELUDE_CLOSURE(base_GHCziPack_unpackCString);
PRELUDE_CLOSURE(base_GHCziWeak_runFinalizzerBatch);

#ifdef IN_STG_CODE
extern W_ ZCMain_main_static_closure[];
#else
extern StgClosure ZCMain_main_static_closure;
#endif

PRELUDE_CLOSURE(base_GHCziIOziException_stackOverflow);
PRELUDE_CLOSURE(base_GHCziIOziException_heapOverflow);
PRELUDE_CLOSURE(base_GHCziIOziException_blockedIndefinitelyOnThrowTo);
PRELUDE_CLOSURE(base_GHCziIOziException_blockedIndefinitelyOnMVar);
PRELUDE_CLOSURE(base_GHCziIOziException_blockedIndefinitelyOnSTM);
PRELUDE_CLOSURE(base_ControlziExceptionziBase_nonTermination);
PRELUDE_CLOSURE(base_ControlziExceptionziBase_nestedAtomically);
PRELUDE_CLOSURE(base_GHCziEventziThread_blockedOnBadFD);

PRELUDE_CLOSURE(base_GHCziConcziSync_runSparks);
PRELUDE_CLOSURE(base_GHCziConcziIO_ensureIOManagerIsRunning);
PRELUDE_CLOSURE(base_GHCziConcziIO_ioManagerCapabilitiesChanged);
PRELUDE_CLOSURE(base_GHCziConcziSignal_runHandlers);

PRELUDE_CLOSURE(base_GHCziTopHandler_flushStdHandles);

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

#define mainIO_closure            (STATIC_CLOSURE(ZCMain_main))

// XXX update me
#define IMPORT_CLOSURE(name) DLL_IMPORT_DATA_REF(name ## _static_closure)

#define True_closure              IMPORT_CLOSURE(ghczmprim_GHCziTypes_True)
#define False_closure             IMPORT_CLOSURE(ghczmprim_GHCziTypes_False)
#define unpackCString_closure     IMPORT_CLOSURE(base_GHCziPack_unpackCString)
#define runFinalizerBatch_closure IMPORT_CLOSURE(base_GHCziWeak_runFinalizzerBatch)

#define runSparks_closure         IMPORT_CLOSURE(base_GHCziConcziSync_runSparks)
#define ensureIOManagerIsRunning_closure IMPORT_CLOSURE(base_GHCziConcziIO_ensureIOManagerIsRunning)
#define ioManagerCapabilitiesChanged_closure IMPORT_CLOSURE(base_GHCziConcziIO_ioManagerCapabilitiesChanged)
#define runHandlers_closure       IMPORT_CLOSURE(base_GHCziConcziSignal_runHandlers)

#define flushStdHandles_closure   IMPORT_CLOSURE(base_GHCziTopHandler_flushStdHandles)

#define stackOverflow_closure     IMPORT_CLOSURE(base_GHCziIOziException_stackOverflow)
#define heapOverflow_closure      IMPORT_CLOSURE(base_GHCziIOziException_heapOverflow)
#define blockedIndefinitelyOnMVar_closure IMPORT_CLOSURE(base_GHCziIOziException_blockedIndefinitelyOnMVar)
#define blockedIndefinitelyOnSTM_closure IMPORT_CLOSURE(base_GHCziIOziException_blockedIndefinitelyOnSTM)
#define nonTermination_closure    IMPORT_CLOSURE(base_ControlziExceptionziBase_nonTermination)
#define nestedAtomically_closure  IMPORT_CLOSURE(base_ControlziExceptionziBase_nestedAtomically)
#define blockedOnBadFD_closure    IMPORT_CLOSURE(base_GHCziEventziThread_blockedOnBadFD)

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
