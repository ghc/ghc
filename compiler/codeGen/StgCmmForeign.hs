-----------------------------------------------------------------------------
--
-- Code generation for foreign calls.
--
-- (c) The University of Glasgow 2004-2006
--
-----------------------------------------------------------------------------

module StgCmmForeign (
  cgForeignCall, loadThreadState, saveThreadState,
  emitPrimCall, emitCCall,
  emitSaveThreadState, -- will be needed by the Cmm parser
  emitLoadThreadState, -- ditto
  emitOpenNursery,
 ) where

#include "HsVersions.h"

import StgSyn
import StgCmmProf
import StgCmmEnv
import StgCmmMonad
import StgCmmUtils
import StgCmmClosure

import BlockId
import Cmm
import CmmUtils
import OldCmm ( CmmReturnInfo(..) )
import MkGraph
import Type
import TysPrim
import CLabel
import SMRep
import ForeignCall
import Constants
import StaticFlags
import Maybes
import Outputable
import BasicTypes

import Control.Monad

-----------------------------------------------------------------------------
-- Code generation for Foreign Calls
-----------------------------------------------------------------------------

cgForeignCall :: [LocalReg]             -- r1,r2  where to put the results
              -> [ForeignHint]
              -> ForeignCall            -- the op
              -> [StgArg]               -- x,y    arguments
              -> FCode ()
-- Emits code for an unsafe foreign call:      r1, r2 = foo( x, y, z )

cgForeignCall results result_hints (CCall (CCallSpec target cconv safety)) stg_args
  = do  { cmm_args <- getFCallArgs stg_args
        ; let ((call_args, arg_hints), cmm_target)
                = case target of
                   StaticTarget _   _      False ->
                       panic "cgForeignCall: unexpected FFI value import"
                   StaticTarget lbl mPkgId True
                     -> let labelSource
                                = case mPkgId of
                                        Nothing         -> ForeignLabelInThisPackage
                                        Just pkgId      -> ForeignLabelInPackage pkgId
                            size        = call_size cmm_args
                        in  ( unzip cmm_args
                            , CmmLit (CmmLabel
                                        (mkForeignLabel lbl size labelSource IsFunction)))

                   DynamicTarget    ->  case cmm_args of
                                           (fn,_):rest -> (unzip rest, fn)
                                           [] -> panic "cgForeignCall []"
              fc = ForeignConvention cconv arg_hints result_hints
              call_target = ForeignTarget cmm_target fc

        ; srt <- getSRTInfo NoSRT        -- SLPJ: Not sure what SRT
                                        -- is right here
                                        -- JD: Does it matter in the new codegen?
        ; emitForeignCall safety results call_target call_args srt CmmMayReturn }
  where
        -- in the stdcall calling convention, the symbol needs @size appended
        -- to it, where size is the total number of bytes of arguments.  We
        -- attach this info to the CLabel here, and the CLabel pretty printer
        -- will generate the suffix when the label is printed.
      call_size args
        | StdCallConv <- cconv = Just (sum (map arg_size args))
        | otherwise            = Nothing

        -- ToDo: this might not be correct for 64-bit API
      arg_size (arg, _) = max (widthInBytes $ typeWidth $ cmmExprType arg) wORD_SIZE

emitCCall :: [(CmmFormal,ForeignHint)]
          -> CmmExpr
          -> [(CmmActual,ForeignHint)]
          -> FCode ()
emitCCall hinted_results fn hinted_args
  = emitForeignCall PlayRisky results target args
                    NoC_SRT -- No SRT b/c we PlayRisky
                    CmmMayReturn
  where
    (args, arg_hints) = unzip hinted_args
    (results, result_hints) = unzip hinted_results
    target = ForeignTarget fn fc
    fc = ForeignConvention CCallConv arg_hints result_hints


emitPrimCall :: [CmmFormal] -> CallishMachOp -> [CmmActual] -> FCode ()
emitPrimCall res op args
  = emitForeignCall PlayRisky res (PrimTarget op) args NoC_SRT CmmMayReturn

-- alternative entry point, used by CmmParse
emitForeignCall
        :: Safety
        -> [CmmFormal]          -- where to put the results
        -> ForeignTarget        -- the op
        -> [CmmActual]          -- arguments
        -> C_SRT                -- the SRT of the calls continuation
        -> CmmReturnInfo        -- This can say "never returns"
                                --   only RTS procedures do this
        -> FCode ()
emitForeignCall safety results target args _srt _ret
  | not (playSafe safety) = do
    let (caller_save, caller_load) = callerSaveVolatileRegs
    emit caller_save
    emit $ mkUnsafeCall target results args
    emit caller_load

  | otherwise = do
    updfr_off <- getUpdFrameOff
    temp_target <- load_target_into_temp target
    emit $ mkSafeCall temp_target results args updfr_off (playInterruptible safety)


{-
--      THINK ABOUT THIS (used to happen)
-- we might need to load arguments into temporaries before
-- making the call, because certain global registers might
-- overlap with registers that the C calling convention uses
-- for passing arguments.
--
-- This is a HACK; really it should be done in the back end, but
-- it's easier to generate the temporaries here.
load_args_into_temps = mapM arg_assign_temp
  where arg_assign_temp (e,hint) = do
           tmp <- maybe_assign_temp e
           return (tmp,hint)
-}

load_target_into_temp :: ForeignTarget -> FCode ForeignTarget
load_target_into_temp (ForeignTarget expr conv) = do
  tmp <- maybe_assign_temp expr
  return (ForeignTarget tmp conv)
load_target_into_temp other_target@(PrimTarget _) =
  return other_target

maybe_assign_temp :: CmmExpr -> FCode CmmExpr
maybe_assign_temp e
  | hasNoGlobalRegs e = return e
  | otherwise         = do
        -- don't use assignTemp, it uses its own notion of "trivial"
        -- expressions, which are wrong here.
        -- this is a NonPtr because it only duplicates an existing
        reg <- newTemp (cmmExprType e) --TODO FIXME NOW
        emit (mkAssign (CmmLocal reg) e)
        return (CmmReg (CmmLocal reg))

-- -----------------------------------------------------------------------------
-- Save/restore the thread state in the TSO

-- This stuff can't be done in suspendThread/resumeThread, because it
-- refers to global registers which aren't available in the C world.

saveThreadState :: CmmAGraph
saveThreadState =
  -- CurrentTSO->stackobj->sp = Sp;
  mkStore (cmmOffset (CmmLoad (cmmOffset stgCurrentTSO tso_stackobj) bWord) stack_SP) stgSp
  <*> closeNursery
  -- and save the current cost centre stack in the TSO when profiling:
  <*> if opt_SccProfilingOn then
        mkStore (cmmOffset stgCurrentTSO tso_CCCS) curCCS
      else mkNop

emitSaveThreadState :: BlockId -> FCode ()
emitSaveThreadState bid = do
  -- CurrentTSO->stackobj->sp = Sp;
  emit $ mkStore (cmmOffset (CmmLoad (cmmOffset stgCurrentTSO tso_stackobj) bWord) stack_SP)
                 (CmmStackSlot (CallArea (Young bid)) (widthInBytes (typeWidth gcWord)))
  emit closeNursery
  -- and save the current cost centre stack in the TSO when profiling:
  when opt_SccProfilingOn $
        emit (mkStore (cmmOffset stgCurrentTSO tso_CCCS) curCCS)

   -- CurrentNursery->free = Hp+1;
closeNursery :: CmmAGraph
closeNursery = mkStore nursery_bdescr_free (cmmOffsetW stgHp 1)

loadThreadState :: LocalReg -> LocalReg -> CmmAGraph
loadThreadState tso stack = do
  -- tso <- newTemp gcWord -- TODO FIXME NOW
  -- stack <- newTemp gcWord -- TODO FIXME NOW
  catAGraphs [
        -- tso = CurrentTSO;
        mkAssign (CmmLocal tso) stgCurrentTSO,
        -- stack = tso->stackobj;
        mkAssign (CmmLocal stack) (CmmLoad (cmmOffset (CmmReg (CmmLocal tso)) tso_stackobj) bWord),
        -- Sp = stack->sp;
        mkAssign sp (CmmLoad (cmmOffset (CmmReg (CmmLocal stack)) stack_SP) bWord),
        -- SpLim = stack->stack + RESERVED_STACK_WORDS;
        mkAssign spLim (cmmOffsetW (cmmOffset (CmmReg (CmmLocal stack)) stack_STACK)
                                    rESERVED_STACK_WORDS),
        openNursery,
        -- and load the current cost centre stack from the TSO when profiling:
        if opt_SccProfilingOn then
          storeCurCCS
            (CmmLoad (cmmOffset (CmmReg (CmmLocal tso)) tso_CCCS) ccsType)
        else mkNop]
emitLoadThreadState :: LocalReg -> LocalReg -> FCode ()
emitLoadThreadState tso stack = emit $ loadThreadState tso stack

openNursery :: CmmAGraph
openNursery = catAGraphs [
        -- Hp = CurrentNursery->free - 1;
        mkAssign hp (cmmOffsetW (CmmLoad nursery_bdescr_free bWord) (-1)),

        -- HpLim = CurrentNursery->start +
        --              CurrentNursery->blocks*BLOCK_SIZE_W - 1;
        mkAssign hpLim
            (cmmOffsetExpr
                (CmmLoad nursery_bdescr_start bWord)
                (cmmOffset
                  (CmmMachOp mo_wordMul [
                    CmmMachOp (MO_SS_Conv W32 wordWidth)
                      [CmmLoad nursery_bdescr_blocks b32],
                    CmmLit (mkIntCLit bLOCK_SIZE)
                   ])
                  (-1)
                )
            )
   ]
emitOpenNursery :: FCode ()
emitOpenNursery = emit openNursery

nursery_bdescr_free, nursery_bdescr_start, nursery_bdescr_blocks :: CmmExpr
nursery_bdescr_free   = cmmOffset stgCurrentNursery oFFSET_bdescr_free
nursery_bdescr_start  = cmmOffset stgCurrentNursery oFFSET_bdescr_start
nursery_bdescr_blocks = cmmOffset stgCurrentNursery oFFSET_bdescr_blocks

tso_stackobj, tso_CCCS, stack_STACK, stack_SP :: ByteOff
tso_stackobj = closureField oFFSET_StgTSO_stackobj
tso_CCCS     = closureField oFFSET_StgTSO_cccs
stack_STACK  = closureField oFFSET_StgStack_stack
stack_SP     = closureField oFFSET_StgStack_sp


closureField :: ByteOff -> ByteOff
closureField off = off + fixedHdrSize * wORD_SIZE

stgSp, stgHp, stgCurrentTSO, stgCurrentNursery :: CmmExpr
stgSp             = CmmReg sp
stgHp             = CmmReg hp
stgCurrentTSO     = CmmReg currentTSO
stgCurrentNursery = CmmReg currentNursery

sp, spLim, hp, hpLim, currentTSO, currentNursery :: CmmReg
sp                = CmmGlobal Sp
spLim             = CmmGlobal SpLim
hp                = CmmGlobal Hp
hpLim             = CmmGlobal HpLim
currentTSO        = CmmGlobal CurrentTSO
currentNursery    = CmmGlobal CurrentNursery

-- -----------------------------------------------------------------------------
-- For certain types passed to foreign calls, we adjust the actual
-- value passed to the call.  For ByteArray#/Array# we pass the
-- address of the actual array, not the address of the heap object.

getFCallArgs :: [StgArg] -> FCode [(CmmExpr, ForeignHint)]
-- (a) Drop void args
-- (b) Add foreign-call shim code
-- It's (b) that makes this differ from getNonVoidArgAmodes

getFCallArgs args
  = do  { mb_cmms <- mapM get args
        ; return (catMaybes mb_cmms) }
  where
    get arg | isVoidRep arg_rep
            = return Nothing
            | otherwise
            = do { cmm <- getArgAmode (NonVoid arg)
                 ; return (Just (add_shim arg_ty cmm, hint)) }
            where
              arg_ty  = stgArgType arg
              arg_rep = typePrimRep arg_ty
              hint    = typeForeignHint arg_ty

add_shim :: Type -> CmmExpr -> CmmExpr
add_shim arg_ty expr
  | tycon == arrayPrimTyCon || tycon == mutableArrayPrimTyCon
  = cmmOffsetB expr arrPtrsHdrSize

  | tycon == byteArrayPrimTyCon || tycon == mutableByteArrayPrimTyCon
  = cmmOffsetB expr arrWordsHdrSize

  | otherwise = expr
  where
    UnaryRep rep_ty = repType arg_ty
    tycon           = tyConAppTyCon rep_ty
        -- should be a tycon app, since this is a foreign call
