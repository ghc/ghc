/*
 * (c) The GHC Team, 2025-2026
 *
 * RTS/ghc-internal interface
 *
 * See Note [RTS/ghc-internal interface].
 */

#pragma once

typedef struct {
    StgClosure *processRemoteCompletion_closure;  // GHC.Internal.Event.Windows.processRemoteCompletion_closure
    StgClosure *runIO_closure;  // GHC.Internal.TopHandler.runIO_closure
    StgClosure *runNonIO_closure;  // GHC.Internal.TopHandler.runNonIO_closure
    StgClosure *Z0T_closure;  // GHC.Internal.Tuple.Z0T_closure
    StgClosure *True_closure;  // GHC.Internal.Types.True_closure
    StgClosure *False_closure;  // GHC.Internal.Types.False_closure
    StgClosure *unpackCString_closure;  // GHC.Internal.Pack.unpackCString_closure
    StgClosure *runFinalizzerBatch_closure;  // GHC.Internal.Weak.Finalizze.runFinalizzerBatch_closure
    StgClosure *stackOverflow_closure;  // GHC.Internal.IO.Exception.stackOverflow_closure
    StgClosure *heapOverflow_closure;  // GHC.Internal.IO.Exception.heapOverflow_closure
    StgClosure *allocationLimitExceeded_closure;  // GHC.Internal.IO.Exception.allocationLimitExceeded_closure
    StgClosure *blockedIndefinitelyOnMVar_closure;  // GHC.Internal.IO.Exception.blockedIndefinitelyOnMVar_closure
    StgClosure *blockedIndefinitelyOnSTM_closure;  // GHC.Internal.IO.Exception.blockedIndefinitelyOnSTM_closure
    StgClosure *cannotCompactFunction_closure;  // GHC.Internal.IO.Exception.cannotCompactFunction_closure
    StgClosure *cannotCompactPinned_closure;  // GHC.Internal.IO.Exception.cannotCompactPinned_closure
    StgClosure *cannotCompactMutable_closure;  // GHC.Internal.IO.Exception.cannotCompactMutable_closure
    StgClosure *nonTermination_closure;  // GHC.Internal.Control.Exception.Base.nonTermination_closure
    StgClosure *nestedAtomically_closure;  // GHC.Internal.Control.Exception.Base.nestedAtomically_closure
    StgClosure *noMatchingContinuationPrompt_closure;  // GHC.Internal.Control.Exception.Base.noMatchingContinuationPrompt_closure
    StgClosure *blockedOnBadFD_closure;  // GHC.Internal.Event.Thread.blockedOnBadFD_closure
    StgClosure *runSparks_closure;  // GHC.Internal.Conc.Sync.runSparks_closure
    StgClosure *ensureIOManagerIsRunning_closure;  // GHC.Internal.Conc.IO.ensureIOManagerIsRunning_closure
    StgClosure *interruptIOManager_closure;  // GHC.Internal.Conc.IO.interruptIOManager_closure
    StgClosure *ioManagerCapabilitiesChanged_closure;  // GHC.Internal.Conc.IO.ioManagerCapabilitiesChanged_closure
    StgClosure *runHandlersPtr_closure;  // GHC.Internal.Conc.Signal.runHandlersPtr_closure
    StgClosure *flushStdHandles_closure;  // GHC.Internal.TopHandler.flushStdHandles_closure
    StgClosure *runMainIO_closure;  // GHC.Internal.TopHandler.runMainIO_closure
    const StgInfoTable *Czh_con_info;  // GHC.Internal.Types.Czh_con_info
    const StgInfoTable *Izh_con_info;  // GHC.Internal.Types.Izh_con_info
    const StgInfoTable *Fzh_con_info;  // GHC.Internal.Types.Fzh_con_info
    const StgInfoTable *Dzh_con_info;  // GHC.Internal.Types.Dzh_con_info
    const StgInfoTable *Wzh_con_info;  // GHC.Internal.Types.Wzh_con_info
    StgClosure *absentSumFieldError_closure;  // GHC.Internal.Prim.Panic.absentSumFieldError_closure
    StgClosure *runAllocationLimitHandler_closure;  // GHC.Internal.AllocationLimitHandler.runAllocationLimitHandler_closure
    const StgInfoTable *Ptr_con_info;  // GHC.Internal.Ptr.Ptr_con_info
    const StgInfoTable *FunPtr_con_info;  // GHC.Internal.Ptr.FunPtr_con_info
    const StgInfoTable *I8zh_con_info;  // GHC.Internal.Int.I8zh_con_info
    const StgInfoTable *I16zh_con_info;  // GHC.Internal.Int.I16zh_con_info
    const StgInfoTable *I32zh_con_info;  // GHC.Internal.Int.I32zh_con_info
    const StgInfoTable *I64zh_con_info;  // GHC.Internal.Int.I64zh_con_info
    const StgInfoTable *W8zh_con_info;  // GHC.Internal.Word.W8zh_con_info
    const StgInfoTable *W16zh_con_info;  // GHC.Internal.Word.W16zh_con_info
    const StgInfoTable *W32zh_con_info;  // GHC.Internal.Word.W32zh_con_info
    const StgInfoTable *W64zh_con_info;  // GHC.Internal.Word.W64zh_con_info
    const StgInfoTable *StablePtr_con_info;  // GHC.Internal.Stable.StablePtr_con_info
    StgClosure *StackSnapshot_closure;  // GHC.Internal.Stack.CloneStack.StackSnapshot_closure
    StgClosure *divZZeroException_closure;  // GHC.Internal.Exception.Type.divZeroException_closure
    StgClosure *underflowException_closure;  // GHC.Internal.Exception.Type.underflowException_closure
    StgClosure *overflowException_closure;  // GHC.Internal.Exception.Type.overflowException_closure
    const StgInfoTable *unpackCStringzh_info;  // GHC.Internal.CString.unpackCStringzh_info
    const StgInfoTable *unpackCStringUtf8zh_info;  // GHC.Internal.CString.unpackCStringUtf8zh_info
#if defined(wasm32_HOST_ARCH)
    StgClosure *raiseJSException_closure;  // GHC.Internal.Wasm.Prim.Imports.raiseJSException_closure
    const StgInfoTable *JSVal_con_info;  // GHC.Internal.Wasm.Prim.Types.JSVal_con_info
    StgClosure *threadDelay_closure;  // GHC.Internal.Wasm.Prim.Conc.Internal.threadDelay_closure
#endif
} HsIface;

extern HsIface *ghc_hs_iface;
