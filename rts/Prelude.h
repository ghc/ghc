/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2005
 *
 * Prelude identifiers that we sometimes need to refer to in the RTS.
 *
 * ---------------------------------------------------------------------------*/

#pragma once

/* These definitions are required by the RTS .cmm files too, so we
 * need declarations that we can #include into the generated .hc files.
 */
#if IN_STG_CODE
#define PRELUDE_INFO(i)       extern W_(i)[]
#define PRELUDE_CLOSURE(i)    extern W_(i)[]
#else
#define PRELUDE_INFO(i)       extern const StgInfoTable (i)
#define PRELUDE_CLOSURE(i)    extern StgClosure (i)
#endif


/* Define canonical names so we can abstract away from the actual
 * modules these names are defined in.
 */

#if defined(IN_STG_CODE)
extern W_ ZCMain_main_closure[];
#else
extern StgClosure ZCMain_main_closure;
#endif

#define Unit_closure              ghc_hs_iface->Z0T_closure
#define True_closure              ghc_hs_iface->True_closure
#define False_closure             ghc_hs_iface->False_closure
#define unpackCString_closure     ghc_hs_iface->unpackCString_closure
#define runFinalizerBatch_closure ghc_hs_iface->runFinalizzerBatch_closure
#define mainIO_closure            (&ZCMain_main_closure)

#define runSparks_closure         ghc_hs_iface->runSparks_closure
#define ensureIOManagerIsRunning_closure ghc_hs_iface->ensureIOManagerIsRunning_closure
#define interruptIOManager_closure ghc_hs_iface->interruptIOManager_closure
#define ioManagerCapabilitiesChanged_closure ghc_hs_iface->ioManagerCapabilitiesChanged_closure
#define runHandlersPtr_closure       ghc_hs_iface->runHandlersPtr_closure
#if defined(mingw32_HOST_OS)
#define processRemoteCompletion_closure ghc_hs_iface->processRemoteCompletion_closure
#endif
#define runAllocationLimitHandler_closure ghc_hs_iface->runAllocationLimitHandler_closure

#define flushStdHandles_closure   ghc_hs_iface->flushStdHandles_closure
#define runMainIO_closure   ghc_hs_iface->runMainIO_closure

#define stackOverflow_closure     ghc_hs_iface->stackOverflow_closure
#define heapOverflow_closure      ghc_hs_iface->heapOverflow_closure
#define allocationLimitExceeded_closure ghc_hs_iface->allocationLimitExceeded_closure
#define blockedIndefinitelyOnMVar_closure ghc_hs_iface->blockedIndefinitelyOnMVar_closure
#define blockedIndefinitelyOnSTM_closure ghc_hs_iface->blockedIndefinitelyOnSTM_closure
#define cannotCompactFunction_closure ghc_hs_iface->cannotCompactFunction_closure
#define cannotCompactPinned_closure ghc_hs_iface->cannotCompactPinned_closure
#define cannotCompactMutable_closure ghc_hs_iface->cannotCompactMutable_closure
#define nonTermination_closure    ghc_hs_iface->nonTermination_closure
#define nestedAtomically_closure  ghc_hs_iface->nestedAtomically_closure
#define absentSumFieldError_closure ghc_hs_iface->absentSumFieldError_closure
#define underflowException_closure ghc_hs_iface->underflowException_closure
#define overflowException_closure ghc_hs_iface->overflowException_closure
#define divZeroException_closure  ghc_hs_iface->divZZeroException_closure

#define blockedOnBadFD_closure    ghc_hs_iface->blockedOnBadFD_closure

#define Czh_con_info              ghc_hs_iface->Czh_con_info
#define Izh_con_info              ghc_hs_iface->Izh_con_info
#define Fzh_con_info              ghc_hs_iface->Fzh_con_info
#define Dzh_con_info              ghc_hs_iface->Dzh_con_info
#define Wzh_con_info              ghc_hs_iface->Wzh_con_info
#define W8zh_con_info             ghc_hs_iface->W8zh_con_info
#define W16zh_con_info            ghc_hs_iface->W16zh_con_info
#define W32zh_con_info            ghc_hs_iface->W32zh_con_info
#define W64zh_con_info            ghc_hs_iface->W64zh_con_info
#define I8zh_con_info             ghc_hs_iface->I8zh_con_info
#define I16zh_con_info            ghc_hs_iface->I16zh_con_info
#define I32zh_con_info            ghc_hs_iface->I32zh_con_info
#define I64zh_con_info            ghc_hs_iface->I64zh_con_info
#define Ptr_con_info              ghc_hs_iface->Ptr_con_info
#define FunPtr_con_info           ghc_hs_iface->FunPtr_con_info
#define StablePtr_static_info     ghc_hs_iface->StablePtr_static_info
#define StablePtr_con_info        ghc_hs_iface->StablePtr_con_info
