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
import StgCmmLayout

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
import DynFlags
import Maybes
import Outputable
import BasicTypes

import Control.Monad
import Prelude hiding( succ )

-----------------------------------------------------------------------------
-- Code generation for Foreign Calls
-----------------------------------------------------------------------------

-- | emit code for a foreign call, and return the results to the sequel.
--
cgForeignCall :: ForeignCall            -- the op
              -> [StgArg]               -- x,y    arguments
              -> Type                   -- result type
              -> FCode ReturnKind

cgForeignCall (CCall (CCallSpec target cconv safety)) stg_args res_ty
  = do  { cmm_args <- getFCallArgs stg_args
        ; (res_regs, res_hints) <- newUnboxedTupleRegs res_ty
        ; let ((call_args, arg_hints), cmm_target)
                = case target of
                   StaticTarget _   _      False ->
                       panic "cgForeignCall: unexpected FFI value import"
                   StaticTarget lbl mPkgId True
                     -> let labelSource
                                = case mPkgId of
                                        Nothing         -> ForeignLabelInThisPackage
                                        Just pkgId      -> ForeignLabelInPackage pkgId
                            size = call_size cmm_args
                        in  ( unzip cmm_args
                            , CmmLit (CmmLabel
                                        (mkForeignLabel lbl size labelSource IsFunction)))

                   DynamicTarget    ->  case cmm_args of
                                           (fn,_):rest -> (unzip rest, fn)
                                           [] -> panic "cgForeignCall []"
              fc = ForeignConvention cconv arg_hints res_hints
              call_target = ForeignTarget cmm_target fc

        -- we want to emit code for the call, and then emitReturn.
        -- However, if the sequel is AssignTo, we shortcut a little
        -- and generate a foreign call that assigns the results
        -- directly.  Otherwise we end up generating a bunch of
        -- useless "r = r" assignments, which are not merely annoying:
        -- they prevent the common block elimination from working correctly
        -- in the case of a safe foreign call.
        -- See Note [safe foreign call convention]
        --
        ; sequel <- getSequel
        ; case sequel of
            AssignTo assign_to_these _ ->
                emitForeignCall safety assign_to_these call_target
                                     call_args CmmMayReturn

            _something_else ->
                do { _ <- emitForeignCall safety res_regs call_target
                                     call_args CmmMayReturn
                   ; emitReturn (map (CmmReg . CmmLocal) res_regs)
                   }
         }
  where
        -- in the stdcall calling convention, the symbol needs @size appended
        -- to it, where size is the total number of bytes of arguments.  We
        -- attach this info to the CLabel here, and the CLabel pretty printer
        -- will generate the suffix when the label is printed.
      call_size args
        | StdCallConv <- cconv = Just (sum (map arg_size args))
        | otherwise            = Nothing

        -- ToDo: this might not be correct for 64-bit API
      arg_size (arg, _) = max (widthInBytes $ typeWidth $ cmmExprType arg)
                               wORD_SIZE

{- Note [safe foreign call convention]

The simple thing to do for a safe foreign call would be the same as an
unsafe one: just

    emitForeignCall ...
    emitReturn ...

but consider what happens in this case

   case foo x y z of
     (# s, r #) -> ...

The sequel is AssignTo [r].  The call to newUnboxedTupleRegs picks [r]
as the result reg, and we generate

  r = foo(x,y,z) returns to L1  -- emitForeignCall
 L1:
  r = r  -- emitReturn
  goto L2
L2:
  ...

Now L1 is a proc point (by definition, it is the continuation of the
safe foreign call).  If L2 does a heap check, then L2 will also be a
proc point.

Furthermore, the stack layout algorithm has to arrange to save r
somewhere between the call and the jump to L1, which is annoying: we
would have to treat r differently from the other live variables, which
have to be saved *before* the call.

So we adopt a special convention for safe foreign calls: the results
are copied out according to the NativeReturn convention by the call,
and the continuation of the call should copyIn the results.  (The
copyOut code is actually inserted when the safe foreign call is
lowered later).  The result regs attached to the safe foreign call are
only used temporarily to hold the results before they are copied out.

We will now generate this:

  r = foo(x,y,z) returns to L1
 L1:
  r = R1  -- copyIn, inserted by mkSafeCall
  goto L2
 L2:
  ... r ...

And when the safe foreign call is lowered later (see Note [lower safe
foreign calls]) we get this:

  suspendThread()
  r = foo(x,y,z)
  resumeThread()
  R1 = r  -- copyOut, inserted by lowerSafeForeignCall
  jump L1
 L1:
  r = R1  -- copyIn, inserted by mkSafeCall
  goto L2
 L2:
  ... r ...

Now consider what happens if L2 does a heap check: the Adams
optimisation kicks in and commons up L1 with the heap-check
continuation, resulting in just one proc point instead of two. Yay!
-}


emitCCall :: [(CmmFormal,ForeignHint)]
          -> CmmExpr
          -> [(CmmActual,ForeignHint)]
          -> FCode ()
emitCCall hinted_results fn hinted_args
  = void $ emitForeignCall PlayRisky results target args CmmMayReturn
  where
    (args, arg_hints) = unzip hinted_args
    (results, result_hints) = unzip hinted_results
    target = ForeignTarget fn fc
    fc = ForeignConvention CCallConv arg_hints result_hints


emitPrimCall :: [CmmFormal] -> CallishMachOp -> [CmmActual] -> FCode ()
emitPrimCall res op args
  = void $ emitForeignCall PlayRisky res (PrimTarget op) args CmmMayReturn

-- alternative entry point, used by CmmParse
emitForeignCall
        :: Safety
        -> [CmmFormal]          -- where to put the results
        -> ForeignTarget        -- the op
        -> [CmmActual]          -- arguments
        -> CmmReturnInfo        -- This can say "never returns"
                                --   only RTS procedures do this
        -> FCode ReturnKind
emitForeignCall safety results target args _ret
  | not (playSafe safety) = do
    dflags <- getDynFlags
    let (caller_save, caller_load) = callerSaveVolatileRegs dflags
    emit caller_save
    emit $ mkUnsafeCall target results args
    emit caller_load
    return AssignedDirectly

  | otherwise = do
    dflags <- getDynFlags
    updfr_off <- getUpdFrameOff
    temp_target <- load_target_into_temp target
    k <- newLabelC
    let (off, copyout) = copyInOflow dflags NativeReturn (Young k) results
       -- see Note [safe foreign call convention]
    emit $
           (    mkStore (CmmStackSlot (Young k) (widthInBytes wordWidth))
                        (CmmLit (CmmBlock k))
            <*> mkLast (CmmForeignCall { tgt  = temp_target
                                       , res  = results
                                       , args = args
                                       , succ = k
                                       , updfr = updfr_off
                                       , intrbl = playInterruptible safety })
            <*> mkLabel k
            <*> copyout
           )
    return (ReturnedTo k off)


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
        emitAssign (CmmLocal reg) e
        return (CmmReg (CmmLocal reg))

-- -----------------------------------------------------------------------------
-- Save/restore the thread state in the TSO

-- This stuff can't be done in suspendThread/resumeThread, because it
-- refers to global registers which aren't available in the C world.

saveThreadState :: DynFlags -> CmmAGraph
saveThreadState dflags =
  -- CurrentTSO->stackobj->sp = Sp;
  mkStore (cmmOffset (CmmLoad (cmmOffset stgCurrentTSO (tso_stackobj dflags)) bWord) (stack_SP dflags)) stgSp
  <*> closeNursery
  -- and save the current cost centre stack in the TSO when profiling:
  <*> if dopt Opt_SccProfilingOn dflags then
        mkStore (cmmOffset stgCurrentTSO (tso_CCCS dflags)) curCCS
      else mkNop

emitSaveThreadState :: BlockId -> FCode ()
emitSaveThreadState bid = do
  dflags <- getDynFlags

  -- CurrentTSO->stackobj->sp = Sp;
  emitStore (cmmOffset (CmmLoad (cmmOffset stgCurrentTSO (tso_stackobj dflags)) bWord) (stack_SP dflags))
                 (CmmStackSlot (Young bid) (widthInBytes (typeWidth gcWord)))
  emit closeNursery
  -- and save the current cost centre stack in the TSO when profiling:
  when (dopt Opt_SccProfilingOn dflags) $
        emitStore (cmmOffset stgCurrentTSO (tso_CCCS dflags)) curCCS

   -- CurrentNursery->free = Hp+1;
closeNursery :: CmmAGraph
closeNursery = mkStore nursery_bdescr_free (cmmOffsetW stgHp 1)

loadThreadState :: DynFlags -> LocalReg -> LocalReg -> CmmAGraph
loadThreadState dflags tso stack = do
  -- tso <- newTemp gcWord -- TODO FIXME NOW
  -- stack <- newTemp gcWord -- TODO FIXME NOW
  catAGraphs [
        -- tso = CurrentTSO;
        mkAssign (CmmLocal tso) stgCurrentTSO,
        -- stack = tso->stackobj;
        mkAssign (CmmLocal stack) (CmmLoad (cmmOffset (CmmReg (CmmLocal tso)) (tso_stackobj dflags)) bWord),
        -- Sp = stack->sp;
        mkAssign sp (CmmLoad (cmmOffset (CmmReg (CmmLocal stack)) (stack_SP dflags)) bWord),
        -- SpLim = stack->stack + RESERVED_STACK_WORDS;
        mkAssign spLim (cmmOffsetW (cmmOffset (CmmReg (CmmLocal stack)) (stack_STACK dflags))
                                    rESERVED_STACK_WORDS),
        openNursery,
        -- and load the current cost centre stack from the TSO when profiling:
        if dopt Opt_SccProfilingOn dflags then
          storeCurCCS
            (CmmLoad (cmmOffset (CmmReg (CmmLocal tso)) (tso_CCCS dflags)) ccsType)
        else mkNop]
emitLoadThreadState :: LocalReg -> LocalReg -> FCode ()
emitLoadThreadState tso stack = do dflags <- getDynFlags
                                   emit $ loadThreadState dflags tso stack

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

tso_stackobj, tso_CCCS, stack_STACK, stack_SP :: DynFlags -> ByteOff
tso_stackobj dflags = closureField dflags oFFSET_StgTSO_stackobj
tso_CCCS     dflags = closureField dflags oFFSET_StgTSO_cccs
stack_STACK  dflags = closureField dflags oFFSET_StgStack_stack
stack_SP     dflags = closureField dflags oFFSET_StgStack_sp


closureField :: DynFlags -> ByteOff -> ByteOff
closureField dflags off = off + fixedHdrSize dflags * wORD_SIZE

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
                 ; dflags <- getDynFlags
                 ; return (Just (add_shim dflags arg_ty cmm, hint)) }
            where
              arg_ty  = stgArgType arg
              arg_rep = typePrimRep arg_ty
              hint    = typeForeignHint arg_ty

add_shim :: DynFlags -> Type -> CmmExpr -> CmmExpr
add_shim dflags arg_ty expr
  | tycon == arrayPrimTyCon || tycon == mutableArrayPrimTyCon
  = cmmOffsetB expr (arrPtrsHdrSize dflags)

  | tycon == byteArrayPrimTyCon || tycon == mutableByteArrayPrimTyCon
  = cmmOffsetB expr (arrWordsHdrSize dflags)

  | otherwise = expr
  where
    UnaryRep rep_ty = repType arg_ty
    tycon           = tyConAppTyCon rep_ty
        -- should be a tycon app, since this is a foreign call
