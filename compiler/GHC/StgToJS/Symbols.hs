{-# LANGUAGE OverloadedStrings #-}

-- | JS symbol generation
module GHC.StgToJS.Symbols where

import GHC.Prelude

import GHC.JS.JStg.Syntax
import GHC.JS.Ident

import GHC.Data.FastString
import GHC.Unit.Module
import GHC.Utils.Word64 (intToWord64)
import Data.ByteString (ByteString)
import Data.Word (Word64)
import qualified Data.ByteString.Char8   as BSC
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Lazy    as BSL

import Data.Array
import Data.Semigroup ((<>))

-- | Hexadecimal representation of an int
--
-- Used for the sub indices.
intBS :: Int -> ByteString
intBS = word64BS . intToWord64

-- | Hexadecimal representation of a 64-bit word
--
-- Used for uniques. We could use base-62 as GHC usually does but this is likely
-- faster.
word64BS :: Word64 -> ByteString
word64BS = BSL.toStrict . BSB.toLazyByteString . BSB.word64Hex

-- | Return z-encoded unit:module
unitModuleStringZ :: Module -> ByteString
unitModuleStringZ mod = mconcat
  [ fastZStringToByteString (zEncodeFS (unitIdFS (moduleUnitId mod)))
  , BSC.pack "ZC" -- z-encoding for ":"
  , fastZStringToByteString (zEncodeFS (moduleNameFS (moduleName mod)))
  ]

-- | the global linkable unit of a module exports this symbol, depend on it to
--   include that unit (used for cost centres)
moduleGlobalSymbol :: Module -> FastString
moduleGlobalSymbol m = mkFastStringByteString $ mconcat
  [ hdB
  , unitModuleStringZ m
  , BSC.pack "_<global>"
  ]

moduleExportsSymbol :: Module -> FastString
moduleExportsSymbol m = mkFastStringByteString $ mconcat
  [ hdB
  , unitModuleStringZ m
  , BSC.pack "_<exports>"
  ]

-- | Make JS symbol corresponding to the given Haskell symbol in the given
-- module
mkJsSymbolBS :: Bool -> Module -> FastString -> ByteString
mkJsSymbolBS exported mod s = mconcat
  [ if exported then hdB else hddB
  , unitModuleStringZ mod
  , BSC.pack "zi" -- z-encoding of "."
  , fastZStringToByteString (zEncodeFS s)
  ]

-- | Make JS symbol corresponding to the given Haskell symbol in the given
-- module
mkJsSymbol :: Bool -> Module -> FastString -> FastString
mkJsSymbol exported mod s = mkFastStringByteString (mkJsSymbolBS exported mod s)

-- | Make JS symbol for given module and unique.
mkFreshJsSymbol :: Module -> Int -> FastString
mkFreshJsSymbol mod i = mkFastStringByteString $ mconcat
  [ hddB
  , unitModuleStringZ mod
  , BSC.pack "_"
  , intBS i
  ]

-- | Make symbol "h$XYZ" or "h$$XYZ"
mkRawSymbol :: Bool -> FastString -> FastString
mkRawSymbol exported fs
  | exported  = mkFastStringByteString $ mconcat [ hdB,  bytesFS fs ]
  | otherwise = mkFastStringByteString $ mconcat [ hddB, bytesFS fs ]

-- | "h$$" constant string
hddB :: ByteString
hddB = BSC.pack "h$$"

-- | "h$" constant string
hdB :: ByteString
hdB = BSC.take 2 hddB

hd :: JStgExpr
hd = global hdStr

hdStr :: FastString
hdStr = mkFastStringByteString hdB

hdlB :: ByteString
hdlB = BSC.pack "h$l"

----------------------------------------- Runtime -------------------------------
hdApply :: JStgExpr
hdApply = global hdApplyStr

hdApplyStr :: FastString
hdApplyStr = fsLit "h$apply"

hdMoveRegs2 :: FastString
hdMoveRegs2 = fsLit "h$moveRegs2"

hdPapGen :: JStgExpr
hdPapGen = global hdPapGenStr

hdPapGenStr :: FastString
hdPapGenStr = fsLit "h$pap_gen"

hdSetReg :: JStgExpr
hdSetReg = global hdSetRegStr

hdSetRegStr :: FastString
hdSetRegStr = fsLit "h$setReg"

hdGetReg :: JStgExpr
hdGetReg = global hdGetRegStr

hdGetRegStr :: FastString
hdGetRegStr = fsLit "h$getReg"

hdResetRegisters :: Ident
hdResetRegisters = name "h$resetRegisters"

hdResetResultVars :: Ident
hdResetResultVars = name "h$resetResultVars"

hdInitClosure :: FastString
hdInitClosure = fsLit "h$init_closure"

hdRegs :: JStgExpr
hdRegs = global (identFS hdRegsStr)

hdRegsStr :: Ident
hdRegsStr = name "h$regs"

hdReturn :: JStgExpr
hdReturn = global (identFS hdReturnStr)

hdReturnStr :: Ident
hdReturnStr = name "h$return"

hdStack :: JStgExpr
hdStack = global (identFS hdStackStr)

hdStackStr :: Ident
hdStackStr = name "h$stack"

hdStackPtr :: JStgExpr
hdStackPtr = global (identFS hdStackPtrStr)

hdStackPtrStr :: Ident
hdStackPtrStr = name "h$sp"

hdBlackHoleTrap :: JStgExpr
hdBlackHoleTrap = global (identFS hdBlackHoleTrapStr)

hdBlackHoleTrapStr :: Ident
hdBlackHoleTrapStr = name "h$blackholeTrap"

hdBlockOnBlackHoleStr :: FastString
hdBlockOnBlackHoleStr = "h$blockOnBlackhole"

hdBlackHoleLNE :: JStgExpr
hdBlackHoleLNE = global (identFS hdBlackHoleLNEStr)

hdBlackHoleLNEStr :: Ident
hdBlackHoleLNEStr = name "h$bh_lne"

hdClosureTypeName :: JStgExpr
hdClosureTypeName = global (identFS hdClosureTypeNameStr)

hdClosureTypeNameStr :: Ident
hdClosureTypeNameStr = name "h$closureTypeName"

hdBh :: JStgExpr
hdBh = global hdBhStr

hdBhStr :: FastString
hdBhStr = fsLit "h$bh"

hdBlackHole :: JStgExpr
hdBlackHole = global (identFS hdBlackHoleStr)

hdBlackHoleStr :: Ident
hdBlackHoleStr = name "h$blackhole"

hdUpdFrame :: JStgExpr
hdUpdFrame = global (identFS hdUpdFrameStr)

hdUpdFrameStr :: Ident
hdUpdFrameStr = name $ fsLit "h$upd_frame"

hdCSel :: JStgExpr
hdCSel = global hdCSelStr

hdCSelStr :: FastString
hdCSelStr = "h$c_sel_"

hdEntry :: Ident
hdEntry = name hdEntryStr

hdEntryStr :: FastString
hdEntryStr = fsLit "h$e"

hdApGen :: JStgExpr
hdApGen = global (identFS hdApGenStr)

hdApGenStr :: Ident
hdApGenStr = name "h$ap_gen"

hdApGenFastStr :: Ident
hdApGenFastStr = name $ fsLit $ unpackFS (identFS hdApGenStr) ++ "_fast"

hdLog :: JStgExpr
hdLog = global hdLogStr

hdLogStr :: FastString
hdLogStr = fsLit "h$log"

hdMkFunctionPtr :: JStgExpr
hdMkFunctionPtr = global "h$mkFunctionPtr"

hdInitStatic :: JStgExpr
hdInitStatic = global (identFS hdInitStaticStr)

hdInitStaticStr :: Ident
hdInitStaticStr = name "h$initStatic"

hdHsSptInsert :: JStgExpr
hdHsSptInsert = global "h$hs_spt_insert"

hdCurrentThread :: JStgExpr
hdCurrentThread = global (identFS hdCurrentThreadStr)

hdCurrentThreadStr :: Ident
hdCurrentThreadStr = name "h$currentThread"

hdWakeupThread :: FastString
hdWakeupThread = fsLit "h$wakeupThread"

hdPaps :: JStgExpr
hdPaps = global hdPapsStr

hdPapsStr :: FastString
hdPapsStr = fsLit "h$paps"

hdPapStr_ :: FastString
hdPapStr_ = fsLit "h$pap_"

hdLazyEntryStr :: Ident
hdLazyEntryStr = name "h$lazy_e"

hdUnboxEntry :: JStgExpr
hdUnboxEntry = global (identFS hdUnboxEntryStr)

hdUnboxEntryStr :: Ident
hdUnboxEntryStr = name "h$unbox_e"

hdMaskFrame :: JStgExpr
hdMaskFrame = global (identFS hdMaskFrameStr)

hdMaskFrameStr :: Ident
hdMaskFrameStr = name "h$maskFrame"

hdUnMaskFrameStr :: Ident
hdUnMaskFrameStr = name "h$unmaskFrame"

hdReturnF :: JStgExpr
hdReturnF = global (identFS hdReturnFStr)

hdReturnFStr :: Ident
hdReturnFStr = name "h$returnf"

hdResumeEntryStr :: Ident
hdResumeEntryStr = name "h$resume_e"

hdFlushStdout :: JStgExpr
hdFlushStdout = global (identFS hdFlushStdoutStr)

hdFlushStdoutStr :: Ident
hdFlushStdoutStr = name "h$flushStdout"

hdFlushStdoutEntry :: JStgExpr
hdFlushStdoutEntry = global (identFS hdFlushStdoutEntryStr)

hdFlushStdoutEntryStr :: Ident
hdFlushStdoutEntryStr = name "h$flushStdout_e"

hdRunIOEntry :: JStgExpr
hdRunIOEntry = global (identFS hdRunIOEntryStr)

hdRunIOEntryStr :: Ident
hdRunIOEntryStr = name "h$runio_e"

hdReduce :: JStgExpr
hdReduce = global (identFS hdReduceStr)

hdReduceStr :: Ident
hdReduceStr = name "h$reduce"

hdThrowStr :: FastString
hdThrowStr = fsLit "h$throw"

hdRaiseAsyncFrame :: JStgExpr
hdRaiseAsyncFrame = global (identFS hdRaiseAsyncFrameStr)

hdRaiseAsyncFrameStr :: Ident
hdRaiseAsyncFrameStr = name "h$raiseAsync_frame"

hdRaiseAsyncEntry :: JStgExpr
hdRaiseAsyncEntry = global (identFS hdRaiseAsyncEntryStr)

hdRaiseAsyncEntryStr :: Ident
hdRaiseAsyncEntryStr = name "h$raiseAsync_e"

hdRaiseEntry :: JStgExpr
hdRaiseEntry = global (identFS hdRaiseEntryStr)

hdRaiseEntryStr :: Ident
hdRaiseEntryStr = name "h$raise_e"

hdKeepAliveEntry :: JStgExpr
hdKeepAliveEntry = global (identFS hdKeepAliveEntryStr)

hdKeepAliveEntryStr :: Ident
hdKeepAliveEntryStr = name "h$keepAlive_e"

hdSelect2Return :: JStgExpr
hdSelect2Return = global (identFS hdSelect2ReturnStr)

hdSelect2ReturnStr :: Ident
hdSelect2ReturnStr = name "h$select2_ret"

hdSelect2Entry :: JStgExpr
hdSelect2Entry = global (identFS hdSelect2EntryStr)

hdSelect2EntryStr :: Ident
hdSelect2EntryStr = name "h$select2_e"

hdSelect1Ret :: JStgExpr
hdSelect1Ret = global (identFS hdSelect1RetStr)

hdSelect1RetStr :: Ident
hdSelect1RetStr = name "h$select1_ret"

hdSelect1EntryStr :: Ident
hdSelect1EntryStr = name "h$select1_e"

hdStaticThunkStr :: FastString
hdStaticThunkStr = fsLit "h$static_thunk"

hdStaticThunksStr
  , hdStaticThunksArrStr
  , hdCAFsStr
  , hdCAFsResetStr :: Ident
hdStaticThunksStr    = name "h$staticThunks"
hdStaticThunksArrStr = name "h$staticThunksArr"
hdCAFsStr            = name "h$CAFs"
hdCAFsResetStr       = name "h$CAFsReset"

hdUpdThunkEntryStr :: Ident
hdUpdThunkEntryStr = name "h$upd_thunk_e"

hdAp3EntryStr :: Ident
hdAp3EntryStr = name "h$ap3_e"

hdAp2EntryStr :: Ident
hdAp2EntryStr = name "h$ap2_e"

hdAp1EntryStr :: Ident
hdAp1EntryStr = name "h$ap1_e"

hdDataToTagEntryStr :: Ident
hdDataToTagEntryStr = name "h$dataToTag_e"

hdTagToEnum :: FastString
hdTagToEnum = fsLit "h$tagToEnum"

hdCatchEntryStr :: Ident
hdCatchEntryStr = name "h$catch_e"

hdNoopStr :: Ident
hdNoopStr = name "h$noop"

hdNoopEntry :: JStgExpr
hdNoopEntry = global (identFS hdNoopEntryStr)

hdNoopEntryStr :: Ident
hdNoopEntryStr = name "h$noop_e"

hdC0 :: JStgExpr
hdC0 = global (identFS hdC0Str)

hdC :: JStgExpr
hdC = global (identFS hdCStr)

hdC0Str :: Ident
hdC0Str = name "h$c0"

hdCStr :: Ident
hdCStr = name "h$c"

hdData2Entry :: Ident
hdData2Entry = name "h$data2_e"

hdData1Entry :: Ident
hdData1Entry = name "h$data1_e"

hdTrueEntry :: Ident
hdTrueEntry = name "h$true_e"

hdFalseEntry :: Ident
hdFalseEntry = name "h$false_e"

hdDoneMainEntry :: JStgExpr
hdDoneMainEntry = global (identFS hdDoneMainEntryStr)

hdDoneMainEntryStr :: Ident
hdDoneMainEntryStr = name "h$doneMain_e"

hdDoneMain :: JStgExpr
hdDoneMain = global "h$doneMain"

hdDone :: Ident
hdDone = name "h$done"

hdExitProcess :: FastString
hdExitProcess = "h$exitProcess"

hdTraceAlloc :: FastString
hdTraceAlloc = fsLit "h$traceAlloc"

hdDebugAllocNotifyAlloc :: FastString
hdDebugAllocNotifyAlloc = fsLit "h$debugAlloc_notifyAlloc"

hdRtsTraceForeign
  , hdRtsProfiling
  , hdCtFun
  , hdCtCon
  , hdCtThunk
  , hdCtPap
  , hdCtBlackhole
  , hdCtStackFrame
  , hdCtVtPtr
  , hdVtVoid
  , hdVtInt
  , hdVtDouble
  , hdVtLong
  , hdVtAddr
  , hdVtObj
  , hdVtArr :: Ident
hdRtsTraceForeign = name "h$rts_traceForeign"
hdRtsProfiling    = name "h$rts_profiling"
hdCtFun           = name "h$ct_fun"
hdCtCon           = name "h$ct_con"
hdCtThunk         = name "h$ct_thunk"
hdCtPap           = name "h$ct_pap"
hdCtBlackhole     = name "h$ct_blackhole"
hdCtStackFrame    = name "h$ct_stackframe"
hdCtVtPtr         = name "h$vt_ptr"
hdVtVoid          = name "h$vt_void"
hdVtInt           = name "h$vt_int"
hdVtDouble        = name "h$vt_double"
hdVtLong          = name "h$vt_long"
hdVtAddr          = name "h$vt_addr"
hdVtObj           = name "h$vt_obj"
hdVtArr           = name "h$vt_arr"


hdLoads :: Array Int Ident
hdLoads = listArray (1,32) [ name . mkFastStringByteString $ hdlB <> BSC.pack (show n)
                           | n <- [1..32::Int]
                           ]

----------------------------------------- Precompiled Aps ----------------------
hdAp00 :: JStgExpr
hdAp00 = global (identFS hdAp00Str)

hdAp00Str :: Ident
hdAp00Str = name "h$ap_0_0"

hdAp00FastStr :: FastString
hdAp00FastStr = fsLit "h$ap_0_0_fast"

hdAp11Fast :: FastString
hdAp11Fast = fsLit "h$ap_1_1_fast"

hdAp10 :: JStgExpr
hdAp10 = global "h$ap_1_0"

hdAp33FastStr :: FastString
hdAp33FastStr = fsLit "h$ap_3_3_fast"

hdAp22FastStr :: FastString
hdAp22FastStr = fsLit "h$ap_2_2_fast"

----------------------------------------- ByteArray -----------------------------
hdNewByteArrayStr :: FastString
hdNewByteArrayStr = "h$newByteArray"

hdCopyMutableByteArrayStr :: FastString
hdCopyMutableByteArrayStr = "h$copyMutableByteArray"

hdCheckOverlapByteArrayStr :: FastString
hdCheckOverlapByteArrayStr = "h$checkOverlapByteArray"

hdShrinkMutableCharArrayStr :: FastString
hdShrinkMutableCharArrayStr = "h$shrinkMutableCharArray"

----------------------------------------- EventLog -----------------------------
hdTraceEventStr :: FastString
hdTraceEventStr = "h$traceEvent"

hdTraceEventBinaryStr :: FastString
hdTraceEventBinaryStr = "h$traceEventBinary"

hdTraceMarkerStr :: FastString
hdTraceMarkerStr = "h$traceMarker"

----------------------------------------- FFI ----------------------------------
hdThrowJSException :: JStgExpr
hdThrowJSException = global $ fsLit "h$throwJSException"

hdUnboxFFIResult :: JStgExpr
hdUnboxFFIResult = global (identFS hdUnboxFFIResultStr)

hdUnboxFFIResultStr :: Ident
hdUnboxFFIResultStr = name "h$unboxFFIResult"

hdMkForeignCallback :: JStgExpr
hdMkForeignCallback = global $ fsLit "h$mkForeignCallback"

hdTraceForeign :: JStgExpr
hdTraceForeign = global $ fsLit "h$traceForeign"

hdBuildObject :: JStgExpr
hdBuildObject = global hdBuildObjectStr

hdBuildObjectStr :: FastString
hdBuildObjectStr = fsLit "h$buildObject"

hdCallDynamicStr :: FastString
hdCallDynamicStr = fsLit "h$callDynamic"

except :: JStgExpr
except = global $ identFS exceptStr

exceptStr :: Ident
exceptStr = name $ fsLit "except"

excepStr :: FastString
excepStr = fsLit "excep"

----------------------------------------- Accessors -----------------------------

-- for almost all other symbols that are faststrings we turn 'foo' into 'fooStr'
-- because these are overloaded with JStgExpr's. But for accessors we leave
-- these as FastStrings because they will become Idents after the refactor.
mv :: FastString
mv = fsLit "mv"

lngth :: FastString
lngth = fsLit "length"

-- | only for byte arrays. This is a JS byte array method
len :: FastString
len = fsLit "len"

slice :: FastString
slice = fsLit "slice"

this :: JStgExpr
this = global "this"

arr :: FastString
arr = fsLit "arr"

dv :: FastString
dv = fsLit "dv"

d1, d2, d3 :: JStgExpr
d1 = global d1Str
d2 = global d2Str
d3 = global d3Str

d1Str, d2Str, d3Str :: FastString
d1Str = fsLit "d1"
d2Str = fsLit "d2"
d3Str = fsLit "d3"

getInt16 :: FastString
getInt16 = "getInt16"

getUint16 :: FastString
getUint16 = "getUint16"

getInt32 :: FastString
getInt32 = "getInt32"

getUint32 :: FastString
getUint32 = "getUint32"

getFloat32 :: FastString
getFloat32 = "getFloat32"

getFloat64 :: FastString
getFloat64 = "getFloat64"

setInt16 :: FastString
setInt16 = "setInt16"

setUint16 :: FastString
setUint16 = "setUint16"

setInt32 :: FastString
setInt32 = "setInt32"

setUint32 :: FastString
setUint32 = "setUint32"

setFloat32 :: FastString
setFloat32 = "setFloat32"

setFloat64 :: FastString
setFloat64 = "setFloat64"

i3, u8, u1, f6, f3 :: FastString
i3 = "i3"
u8 = "u8"
u1 = "u1"
f6 = "f6"
f3 = "f3"

val :: FastString
val = fsLit "val"

label :: FastString
label = fsLit "label"

mask :: FastString
mask = fsLit "mask"

unMask :: FastString
unMask = fsLit "unmask"

resume :: FastString
resume = "resume"

f :: FastString
f = fsLit "f"

n :: FastString
n = fsLit "n"

hasOwnProperty :: FastString
hasOwnProperty = fsLit "hasOwnProperty"

hdCollectProps :: FastString
hdCollectProps = fsLit "h$collectProps"

replace :: FastString
replace = fsLit "replace"

substring :: FastString
substring = fsLit "substring"

trace :: FastString
trace = fsLit "trace"

apply :: FastString
apply = fsLit "apply"

----------------------------------------- STM ----------------------------------
hdMVar :: JStgExpr
hdMVar = global hdMVarStr

hdMVarStr :: FastString
hdMVarStr = fsLit "h$MVar"

hdTakeMVar :: JStgExpr
hdTakeMVar = global hdTakeMVarStr

hdTakeMVarStr :: FastString
hdTakeMVarStr = fsLit "h$takeMVar"

hdTryTakeMVarStr :: FastString
hdTryTakeMVarStr = fsLit "h$tryTakeMVar"

hdPutMVarStr :: FastString
hdPutMVarStr = fsLit "h$putMVar"

hdTryPutMVarStr :: FastString
hdTryPutMVarStr = fsLit "h$tryPutMVar"

hdNewTVar :: FastString
hdNewTVar = fsLit "h$newTVar"

hdReadTVar :: FastString
hdReadTVar = fsLit "h$readTVar"

hdReadTVarIO :: FastString
hdReadTVarIO = fsLit "h$readTVarIO"

hdWriteTVar :: FastString
hdWriteTVar = fsLit "h$writeTVar"

hdReadMVarStr :: FastString
hdReadMVarStr = fsLit "h$readMVar"

hdStmRemoveBlockedThreadStr :: FastString
hdStmRemoveBlockedThreadStr = fsLit "h$stmRemoveBlockedThread"

hdStmStartTransactionStr :: FastString
hdStmStartTransactionStr = fsLit "h$stmStartTransaction"

hdAtomicallyEntry :: JStgExpr
hdAtomicallyEntry = global (identFS hdAtomicallyEntryStr)

hdAtomicallyEntryStr :: Ident
hdAtomicallyEntryStr = name $ fsLit "h$atomically_e"

hdAtomicallyStr :: FastString
hdAtomicallyStr = "h$atomically"

hdStgResumeRetryEntry :: JStgExpr
hdStgResumeRetryEntry = global (identFS hdStgResumeRetryEntryStr)

hdStgResumeRetryEntryStr :: Ident
hdStgResumeRetryEntryStr = name $ fsLit "h$stmResumeRetry_e"

hdStmCommitTransactionStr :: FastString
hdStmCommitTransactionStr = fsLit "h$stmCommitTransaction"

hdStmValidateTransactionStr :: FastString
hdStmValidateTransactionStr = "h$stmValidateTransaction"

hdStmCatchRetryEntry :: JStgExpr
hdStmCatchRetryEntry = global (identFS hdStmCatchRetryEntryStr)

hdStmCatchRetryEntryStr :: Ident
hdStmCatchRetryEntryStr = name $ fsLit "h$stmCatchRetry_e"

hdStmRetryStr :: FastString
hdStmRetryStr = fsLit "h$stmRetry"

hdStmCatchRetryStr :: FastString
hdStmCatchRetryStr = fsLit "h$stmCatchRetry"

hdStmCatchEntry :: JStgExpr
hdStmCatchEntry = global (identFS hdStmCatchEntryStr)

hdCatchStmStr :: FastString
hdCatchStmStr = fsLit "h$catchStm"

hdStmCatchEntryStr :: Ident
hdStmCatchEntryStr = name $ fsLit "h$catchStm_e"

hdRetryInterrupted :: JStgExpr
hdRetryInterrupted = global (identFS hdRetryInterruptedStr)

hdRetryInterruptedStr :: Ident
hdRetryInterruptedStr = name $ fsLit "h$retryInterrupted"

hdMaskUnintFrame :: JStgExpr
hdMaskUnintFrame = global (identFS hdMaskUnintFrameStr)

hdMaskUnintFrameStr :: Ident
hdMaskUnintFrameStr = name $ fsLit "h$maskUnintFrame"

hdReschedule :: JStgExpr
hdReschedule = global (identFS hdRescheduleStr)

hdRescheduleStr :: Ident
hdRescheduleStr = name $ fsLit "h$reschedule"

hdRestoreThread :: JStgExpr
hdRestoreThread = global (identFS hdRestoreThreadStr)

hdRestoreThreadStr :: Ident
hdRestoreThreadStr = name $ fsLit "h$restoreThread"

hdFinishedThread :: FastString
hdFinishedThread = fsLit "h$finishThread"

----------------------------------------- Z-Encodings ---------------------------
hdPrimOpStr :: FastString
hdPrimOpStr = fsLit "h$primop_"

wrapperColonStr :: FastString
wrapperColonStr = fsLit "ghczuwrapperZC" -- equivalent non-z-encoding => ghc_wrapper:

hdInternalExceptionTypeDivZero :: JStgExpr
hdInternalExceptionTypeDivZero = global "h$ghczminternalZCGHCziInternalziExceptionziTypezidivZZeroException"

hdInternalExceptionTypeOverflow :: JStgExpr
hdInternalExceptionTypeOverflow = global "h$ghczminternalZCGHCziInternalziExceptionziTypezioverflowException"

hdInternalExceptionTypeUnderflow :: JStgExpr
hdInternalExceptionTypeUnderflow = global "h$ghczminternalZCGHCziInternalziExceptionziTypeziunderflowException"

hdInternalExceptionControlExceptionBaseNonTermination :: JStgExpr
hdInternalExceptionControlExceptionBaseNonTermination = global "h$ghczminternalZCGHCziInternalziControlziExceptionziBasezinonTermination"

hdGhcInternalIOHandleFlush :: JStgExpr
hdGhcInternalIOHandleFlush = global "h$ghczminternalZCGHCziInternalziIOziHandlezihFlush"

hdGhcInternalIOHandleFDStdout :: JStgExpr
hdGhcInternalIOHandleFDStdout = global "h$ghczminternalZCGHCziInternalziIOziHandleziFDzistdout"

hdGhcInternalJSPrimValConEntryStr :: FastString
hdGhcInternalJSPrimValConEntryStr = fsLit "h$ghczminternalZCGHCziInternalziJSziPrimziJSVal_con_e"

----------------------------------------- Profiling -----------------------------
hdBuildCCSPtrStr :: FastString
hdBuildCCSPtrStr = "h$buildCCSPtr"

hdClearCCSStr :: FastString
hdClearCCSStr = "h$clearCCS"

hdRestoreCCSStr :: FastString
hdRestoreCCSStr = fsLit "h$restoreCCS"

hdSetCcsEntry :: JStgExpr
hdSetCcsEntry = global (identFS hdSetCcsEntryStr)

hdSetCcsEntryStr :: Ident
hdSetCcsEntryStr = name $ fsLit "h$setCcs_e"

ccStr :: FastString
ccStr = fsLit "cc"
----------------------------------------- Others -------------------------------
unknown :: FastString
unknown = fsLit "<unknown>"

typeof :: FastString
typeof = fsLit "typeof"

hdRawStr :: FastString
hdRawStr = fsLit "h$rstr"

throwStr :: FastString
throwStr = fsLit "throw"

hdCheckObj :: JStgExpr
hdCheckObj = global $ fsLit "h$checkObj"

console :: JStgExpr
console = global consoleStr

consoleStr :: FastString
consoleStr = fsLit "console"

arguments :: JStgExpr
arguments = global argumentsStr

argumentsStr :: FastString
argumentsStr = fsLit "arguments"

hdReportHeapOverflow :: JStgExpr
hdReportHeapOverflow = global (identFS hdReportHeapOverflowStr)

hdReportHeapOverflowStr :: Ident
hdReportHeapOverflowStr = name $ fsLit "h$reportHeapOverflow"

hdReportStackOverflow :: JStgExpr
hdReportStackOverflow = global (identFS hdReportStackOverflowStr)

hdReportStackOverflowStr :: Ident
hdReportStackOverflowStr = name $ fsLit "h$reportStackOverflow"

hdDumpRes :: JStgExpr
hdDumpRes = global (identFS hdDumpResStr)

hdDumpResStr :: Ident
hdDumpResStr = name $ fsLit "h$dumpRes"

ghcjsArray :: FastString
ghcjsArray = fsLit "__ghcjsArray"

----------------------------------------- Compact -------------------------------

hdCompactSize :: FastString
hdCompactSize = fsLit "h$compactSize"

hdCompactAddWithSharing :: FastString
hdCompactAddWithSharing = fsLit "h$compactAddWithSharing"

hdCompactAdd :: FastString
hdCompactAdd = fsLit "h$compactAdd"

hdCompactFixupPointers :: FastString
hdCompactFixupPointers = fsLit "h$compactFixupPointers"

hdCompactAllocateBlock :: FastString
hdCompactAllocateBlock = fsLit "h$compactAllocateBlock"

hdCompactGetNextBlock :: FastString
hdCompactGetNextBlock = fsLit "h$compactGetNextBlock"

hdCompactGetFirstBlock :: FastString
hdCompactGetFirstBlock = fsLit "h$compactGetFirstBlock"

hdCompactContainsAny :: FastString
hdCompactContainsAny = fsLit "h$compactContainsAny"

hdCompactContains :: FastString
hdCompactContains = fsLit "h$compactContains"

hdCompactResize :: FastString
hdCompactResize = fsLit "h$compactResize"

hdCompactNew :: FastString
hdCompactNew = fsLit "h$compactNew"

----------------------------------------- Stable Pointers -----------------------

hdStableNameInt :: FastString
hdStableNameInt = fsLit "h$stableNameInt"

hdMakeStableName :: FastString
hdMakeStableName = fsLit "h$makeStableName"

hdDeRefStablePtr :: FastString
hdDeRefStablePtr = fsLit "h$deRefStablePtr"

hdStablePtrBuf :: JStgExpr
hdStablePtrBuf = global "h$stablePtrBuf"

hdMakeStablePtrStr :: FastString
hdMakeStablePtrStr = fsLit "h$makeStablePtr"

------------------------------- Weak Pointers -----------------------------------

hdKeepAlive :: FastString
hdKeepAlive = fsLit "h$keepAlive"

hdFinalizeWeak :: FastString
hdFinalizeWeak = fsLit "h$finalizeWeak"

hdMakeWeakNoFinalizer :: FastString
hdMakeWeakNoFinalizer = fsLit "h$makeWeakNoFinalizer"

hdMakeWeak :: FastString
hdMakeWeak = fsLit "h$makeWeak"

------------------------------- Concurrency Primitives -------------------------

hdGetThreadLabel :: FastString
hdGetThreadLabel = fsLit "h$getThreadLabel"

hdListThreads :: FastString
hdListThreads = fsLit "h$listThreads"

hdThreadStatus :: FastString
hdThreadStatus = fsLit "h$threadStatus"

hdYield :: FastString
hdYield = fsLit "h$yield"

hdKillThread :: FastString
hdKillThread = fsLit "h$killThread"

hdFork :: FastString
hdFork = fsLit "h$fork"

------------------------------- Delay/Wait Ops ---------------------------------

hdWaitWrite :: FastString
hdWaitWrite = fsLit "h$waitWrite"

hdWaitRead :: FastString
hdWaitRead = fsLit "h$waitRead"

hdDelayThread :: FastString
hdDelayThread = fsLit "h$delayThread"

------------------------------- Exceptions --------------------------------------

hdCatchStr :: FastString
hdCatchStr = fsLit "h$catch"

hdMaskAsyncStr :: FastString
hdMaskAsyncStr = fsLit "h$maskAsync"

hdMaskUnintAsyncStr :: FastString
hdMaskUnintAsyncStr = fsLit "h$maskUnintAsync"

hdUnmaskAsyncStr :: FastString
hdUnmaskAsyncStr = fsLit "h$unmaskAsync"

------------------------------- Mutable variables --------------------------------------

hdMutVarStr :: FastString
hdMutVarStr = fsLit "h$MutVar"

hdAtomicModifyMutVar2Str :: FastString
hdAtomicModifyMutVar2Str = fsLit "h$atomicModifyMutVar2"

hdAtomicModifyMutVarStr :: FastString
hdAtomicModifyMutVarStr = fsLit "h$atomicModifyMutVar"

------------------------------- Addr# ------------------------------------------

hdComparePointerStr :: FastString
hdComparePointerStr = fsLit "h$comparePointer"

------------------------------- Byte Arrays -------------------------------------

hdCompareByteArraysStr :: FastString
hdCompareByteArraysStr = fsLit "h$compareByteArrays"

hdResizeMutableByteArrayStr :: FastString
hdResizeMutableByteArrayStr = fsLit "h$resizeMutableByteArray"

hdShrinkMutableByteArrayStr :: FastString
hdShrinkMutableByteArrayStr = fsLit "h$shrinkMutableByteArray"

------------------------------- Arrays ------------------------------------------

hdCopyMutableArrayStr :: FastString
hdCopyMutableArrayStr = fsLit "h$copyMutableArray"

hdNewArrayStr :: FastString
hdNewArrayStr = fsLit "h$newArray"

hdSliceArrayStr :: FastString
hdSliceArrayStr = fsLit "h$sliceArray"

------------------------------ Float --------------------------------------------

hdDecodeFloatIntStr :: FastString
hdDecodeFloatIntStr = fsLit "h$decodeFloatInt"

hdCastFloatToWord32Str :: FastString
hdCastFloatToWord32Str = fsLit "h$castFloatToWord32"

hdCastWord32ToFloatStr :: FastString
hdCastWord32ToFloatStr = fsLit "h$castWord32ToFloat"

------------------------------ Double -------------------------------------------

hdDecodeDouble2IntStr :: FastString
hdDecodeDouble2IntStr = fsLit "h$decodeDouble2Int"

hdDecodeDoubleInt64Str :: FastString
hdDecodeDoubleInt64Str = fsLit "h$decodeDoubleInt64"

hdCastDoubleToWord64Str :: FastString
hdCastDoubleToWord64Str = fsLit "h$castDoubleToWord64"

hdCastWord64ToDoubleStr :: FastString
hdCastWord64ToDoubleStr = fsLit "h$castWord64ToDouble"

------------------------------ Word -------------------------------------------

hdReverseWordStr :: FastString
hdReverseWordStr = fsLit "h$reverseWord"

hdClz8Str
  , hdClz16Str
  , hdClz32Str
  , hdClz64Str
  , hdCtz8Str
  , hdCtz16Str
  , hdCtz32Str
  , hdCtz64Str :: FastString

hdClz8Str  = fsLit "h$clz8"
hdClz16Str = fsLit "h$clz16"
hdClz32Str = fsLit "h$clz32"
hdClz64Str = fsLit "h$clz64"
hdCtz8Str  = fsLit "h$ctz8"
hdCtz16Str = fsLit "h$ctz16"
hdCtz32Str = fsLit "h$ctz32"
hdCtz64Str = fsLit "h$ctz64"

hdBSwap64Str :: FastString
hdBSwap64Str = "h$bswap64"

hdPExit8Str
  , hdPExit16Str
  , hdPExit32Str
  , hdPExit64Str
  , hdPDep8Str
  , hdPDep16Str
  , hdPDep32Str
  , hdPDep64Str :: FastString

hdPExit8Str  = fsLit "h$pext8"
hdPExit16Str = fsLit "h$pext16"
hdPExit32Str = fsLit "h$pext32"
hdPExit64Str = fsLit "h$pext64"
hdPDep8Str   = fsLit "h$pdep8"
hdPDep16Str  = fsLit "h$pdep16"
hdPDep32Str  = fsLit "h$pdep32"
hdPDep64Str  = fsLit "h$pdep64"

hdPopCntTab :: JStgExpr
hdPopCntTab = global "h$popCntTab"

hdPopCnt32Str :: FastString
hdPopCnt32Str = fsLit "h$popCnt32"

hdPopCnt64Str :: FastString
hdPopCnt64Str = fsLit "h$popCnt64"

hdQuotRem2Word32Str :: FastString
hdQuotRem2Word32Str = fsLit "h$quotRem2Word32"

hdQuotRemWord32Str :: FastString
hdQuotRemWord32Str = fsLit "h$quotRemWord32"

hdRemWord32Str :: FastString
hdRemWord32Str = fsLit "h$remWord32"

hdQuotWord32Str :: FastString
hdQuotWord32Str = fsLit "h$quotWord32"

hdMul2Word32Str :: FastString
hdMul2Word32Str = fsLit "h$mul2Word32"

hdMulImulStr :: FastString
hdMulImulStr = fsLit "Math.imul"

hdWordAdd2 :: FastString
hdWordAdd2 = fsLit "h$wordAdd2"

hdHsPlusWord64Str :: FastString
hdHsPlusWord64Str = fsLit "h$hs_plusWord64"

hdHsMinusWord64Str :: FastString
hdHsMinusWord64Str = fsLit "h$hs_minusWord64"

hdHsTimesWord64Str :: FastString
hdHsTimesWord64Str = fsLit "h$hs_timesWord64"

hdHsQuotWord64Str :: FastString
hdHsQuotWord64Str = fsLit "h$hs_quotWord64"

hdHsRemWord64Str :: FastString
hdHsRemWord64Str = fsLit "h$hs_remWord64"

hdHsUncheckedShiftRWord64Str :: FastString
hdHsUncheckedShiftRWord64Str = fsLit "h$hs_uncheckedShiftRWord64"

hdHsUncheckedShiftLWord64Str :: FastString
hdHsUncheckedShiftLWord64Str = fsLit "h$hs_uncheckedShiftLWord64"

hdHsPlusInt64Str :: FastString
hdHsPlusInt64Str = fsLit "h$hs_plusInt64"

hdHsMinusInt64Str :: FastString
hdHsMinusInt64Str = fsLit "h$hs_minusInt64"

hdHsTimesInt64Str :: FastString
hdHsTimesInt64Str = fsLit "h$hs_timesInt64"

hdHsQuotInt64Str :: FastString
hdHsQuotInt64Str = fsLit "h$hs_quotInt64"

hdHsRemInt64Str :: FastString
hdHsRemInt64Str = fsLit "h$hs_remInt64"

hdHsUncheckedShiftLLInt64Str :: FastString
hdHsUncheckedShiftLLInt64Str = fsLit "h$hs_uncheckedShiftLLInt64"

hdHsUncheckedShiftRAInt64Str :: FastString
hdHsUncheckedShiftRAInt64Str = fsLit "h$hs_uncheckedShiftRAInt64"

hdHsUncheckedShiftRLInt64Str :: FastString
hdHsUncheckedShiftRLInt64Str = fsLit "h$hs_uncheckedShiftRLInt64"

hdHsTimesInt2Str :: FastString
hdHsTimesInt2Str = fsLit "h$hs_timesInt2"

------------------------------ Linker -------------------------------------------

hdEncodeModifiedUtf8Str :: FastString
hdEncodeModifiedUtf8Str = fsLit "h$encodeModifiedUtf8"

hdRawStringDataStr :: FastString
hdRawStringDataStr = fsLit "h$rawStringData"

hdPStr :: FastString
hdPStr = fsLit "h$p"

hdDStr :: FastString
hdDStr = fsLit "h$d"

hdDiStr :: FastString
hdDiStr = fsLit "h$di"

hdStcStr :: FastString
hdStcStr = fsLit "h$stc"

hdStlStr :: FastString
hdStlStr = fsLit "h$stl"

hdStiStr :: FastString
hdStiStr = fsLit "h$sti"

hdStrStr :: FastString
hdStrStr = fsLit "h$str"
------------------------------ Pack/Unpack --------------------------------------------

hdDecodeUtf8Z :: FastString
hdDecodeUtf8Z = fsLit "h$decodeUtf8z"
