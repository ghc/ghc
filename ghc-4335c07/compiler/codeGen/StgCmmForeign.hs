{-# LANGUAGE CPP #-}

-----------------------------------------------------------------------------
--
-- Code generation for foreign calls.
--
-- (c) The University of Glasgow 2004-2006
--
-----------------------------------------------------------------------------

module StgCmmForeign (
  cgForeignCall,
  emitPrimCall, emitCCall,
  emitForeignCall,     -- For CmmParse
  emitSaveThreadState,
  saveThreadState,
  emitLoadThreadState,
  loadThreadState,
  emitOpenNursery,
  emitCloseNursery,
 ) where

#include "HsVersions.h"

import GhcPrelude hiding( succ, (<*>) )

import StgSyn
import StgCmmProf (storeCurCCS, ccsType, curCCS)
import StgCmmEnv
import StgCmmMonad
import StgCmmUtils
import StgCmmClosure
import StgCmmLayout

import BlockId (newBlockId)
import Cmm
import CmmUtils
import MkGraph
import Type
import RepType
import TysPrim
import CLabel
import SMRep
import ForeignCall
import DynFlags
import Maybes
import Outputable
import UniqSupply
import BasicTypes

import Control.Monad

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
  = do  { dflags <- getDynFlags
        ; let -- in the stdcall calling convention, the symbol needs @size appended
              -- to it, where size is the total number of bytes of arguments.  We
              -- attach this info to the CLabel here, and the CLabel pretty printer
              -- will generate the suffix when the label is printed.
            call_size args
              | StdCallConv <- cconv = Just (sum (map arg_size args))
              | otherwise            = Nothing

              -- ToDo: this might not be correct for 64-bit API
            arg_size (arg, _) = max (widthInBytes $ typeWidth $ cmmExprType dflags arg)
                                     (wORD_SIZE dflags)
        ; cmm_args <- getFCallArgs stg_args
        ; (res_regs, res_hints) <- newUnboxedTupleRegs res_ty
        ; let ((call_args, arg_hints), cmm_target)
                = case target of
                   StaticTarget _ _   _      False ->
                       panic "cgForeignCall: unexpected FFI value import"
                   StaticTarget _ lbl mPkgId True
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
              fc = ForeignConvention cconv arg_hints res_hints CmmMayReturn
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
                emitForeignCall safety assign_to_these call_target call_args

            _something_else ->
                do { _ <- emitForeignCall safety res_regs call_target call_args
                   ; emitReturn (map (CmmReg . CmmLocal) res_regs)
                   }
         }

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
  = void $ emitForeignCall PlayRisky results target args
  where
    (args, arg_hints) = unzip hinted_args
    (results, result_hints) = unzip hinted_results
    target = ForeignTarget fn fc
    fc = ForeignConvention CCallConv arg_hints result_hints CmmMayReturn


emitPrimCall :: [CmmFormal] -> CallishMachOp -> [CmmActual] -> FCode ()
emitPrimCall res op args
  = void $ emitForeignCall PlayRisky res (PrimTarget op) args

-- alternative entry point, used by CmmParse
emitForeignCall
        :: Safety
        -> [CmmFormal]          -- where to put the results
        -> ForeignTarget        -- the op
        -> [CmmActual]          -- arguments
        -> FCode ReturnKind
emitForeignCall safety results target args
  | not (playSafe safety) = do
    dflags <- getDynFlags
    let (caller_save, caller_load) = callerSaveVolatileRegs dflags
    emit caller_save
    target' <- load_target_into_temp target
    args' <- mapM maybe_assign_temp args
    emit $ mkUnsafeCall target' results args'
    emit caller_load
    return AssignedDirectly

  | otherwise = do
    dflags <- getDynFlags
    updfr_off <- getUpdFrameOff
    target' <- load_target_into_temp target
    args' <- mapM maybe_assign_temp args
    k <- newBlockId
    let (off, _, copyout) = copyInOflow dflags NativeReturn (Young k) results []
       -- see Note [safe foreign call convention]
    tscope <- getTickScope
    emit $
           (    mkStore (CmmStackSlot (Young k) (widthInBytes (wordWidth dflags)))
                        (CmmLit (CmmBlock k))
            <*> mkLast (CmmForeignCall { tgt  = target'
                                       , res  = results
                                       , args = args'
                                       , succ = k
                                       , ret_args = off
                                       , ret_off = updfr_off
                                       , intrbl = playInterruptible safety })
            <*> mkLabel k tscope
            <*> copyout
           )
    return (ReturnedTo k off)

load_target_into_temp :: ForeignTarget -> FCode ForeignTarget
load_target_into_temp (ForeignTarget expr conv) = do
  tmp <- maybe_assign_temp expr
  return (ForeignTarget tmp conv)
load_target_into_temp other_target@(PrimTarget _) =
  return other_target

-- What we want to do here is create a new temporary for the foreign
-- call argument if it is not safe to use the expression directly,
-- because the expression mentions caller-saves GlobalRegs (see
-- Note [Register Parameter Passing]).
--
-- However, we can't pattern-match on the expression here, because
-- this is used in a loop by CmmParse, and testing the expression
-- results in a black hole.  So we always create a temporary, and rely
-- on CmmSink to clean it up later.  (Yuck, ToDo).  The generated code
-- ends up being the same, at least for the RTS .cmm code.
--
maybe_assign_temp :: CmmExpr -> FCode CmmExpr
maybe_assign_temp e = do
  dflags <- getDynFlags
  reg <- newTemp (cmmExprType dflags e)
  emitAssign (CmmLocal reg) e
  return (CmmReg (CmmLocal reg))

-- -----------------------------------------------------------------------------
-- Save/restore the thread state in the TSO

-- This stuff can't be done in suspendThread/resumeThread, because it
-- refers to global registers which aren't available in the C world.

emitSaveThreadState :: FCode ()
emitSaveThreadState = do
  dflags <- getDynFlags
  code <- saveThreadState dflags
  emit code

-- | Produce code to save the current thread state to @CurrentTSO@
saveThreadState :: MonadUnique m => DynFlags -> m CmmAGraph
saveThreadState dflags = do
  tso <- newTemp (gcWord dflags)
  close_nursery <- closeNursery dflags tso
  pure $ catAGraphs [
    -- tso = CurrentTSO;
    mkAssign (CmmLocal tso) stgCurrentTSO,
    -- tso->stackobj->sp = Sp;
    mkStore (cmmOffset dflags
                       (CmmLoad (cmmOffset dflags
                                           (CmmReg (CmmLocal tso))
                                           (tso_stackobj dflags))
                                (bWord dflags))
                       (stack_SP dflags))
            stgSp,
    close_nursery,
    -- and save the current cost centre stack in the TSO when profiling:
    if gopt Opt_SccProfilingOn dflags then
        mkStore (cmmOffset dflags (CmmReg (CmmLocal tso)) (tso_CCCS dflags)) curCCS
      else mkNop
    ]

emitCloseNursery :: FCode ()
emitCloseNursery = do
  dflags <- getDynFlags
  tso <- newTemp (bWord dflags)
  code <- closeNursery dflags tso
  emit $ mkAssign (CmmLocal tso) stgCurrentTSO <*> code

{- |
@closeNursery dflags tso@ produces code to close the nursery.
A local register holding the value of @CurrentTSO@ is expected for
efficiency.

Closing the nursery corresponds to the following code:

@
  tso = CurrentTSO;
  cn = CurrentNuresry;

  // Update the allocation limit for the current thread.  We don't
  // check to see whether it has overflowed at this point, that check is
  // made when we run out of space in the current heap block (stg_gc_noregs)
  // and in the scheduler when context switching (schedulePostRunThread).
  tso->alloc_limit -= Hp + WDS(1) - cn->start;

  // Set cn->free to the next unoccupied word in the block
  cn->free = Hp + WDS(1);
@
-}
closeNursery :: MonadUnique m => DynFlags -> LocalReg -> m CmmAGraph
closeNursery df tso = do
  let tsoreg  = CmmLocal tso
  cnreg      <- CmmLocal <$> newTemp (bWord df)
  pure $ catAGraphs [
    mkAssign cnreg stgCurrentNursery,

    -- CurrentNursery->free = Hp+1;
    mkStore (nursery_bdescr_free df cnreg) (cmmOffsetW df stgHp 1),

    let alloc =
           CmmMachOp (mo_wordSub df)
              [ cmmOffsetW df stgHp 1
              , CmmLoad (nursery_bdescr_start df cnreg) (bWord df)
              ]

        alloc_limit = cmmOffset df (CmmReg tsoreg) (tso_alloc_limit df)
    in

    -- tso->alloc_limit += alloc
    mkStore alloc_limit (CmmMachOp (MO_Sub W64)
                               [ CmmLoad alloc_limit b64
                               , CmmMachOp (mo_WordTo64 df) [alloc] ])
   ]

emitLoadThreadState :: FCode ()
emitLoadThreadState = do
  dflags <- getDynFlags
  code <- loadThreadState dflags
  emit code

-- | Produce code to load the current thread state from @CurrentTSO@
loadThreadState :: MonadUnique m => DynFlags -> m CmmAGraph
loadThreadState dflags = do
  tso <- newTemp (gcWord dflags)
  stack <- newTemp (gcWord dflags)
  open_nursery <- openNursery dflags tso
  pure $ catAGraphs [
    -- tso = CurrentTSO;
    mkAssign (CmmLocal tso) stgCurrentTSO,
    -- stack = tso->stackobj;
    mkAssign (CmmLocal stack) (CmmLoad (cmmOffset dflags (CmmReg (CmmLocal tso)) (tso_stackobj dflags)) (bWord dflags)),
    -- Sp = stack->sp;
    mkAssign sp (CmmLoad (cmmOffset dflags (CmmReg (CmmLocal stack)) (stack_SP dflags)) (bWord dflags)),
    -- SpLim = stack->stack + RESERVED_STACK_WORDS;
    mkAssign spLim (cmmOffsetW dflags (cmmOffset dflags (CmmReg (CmmLocal stack)) (stack_STACK dflags))
                                (rESERVED_STACK_WORDS dflags)),
    -- HpAlloc = 0;
    --   HpAlloc is assumed to be set to non-zero only by a failed
    --   a heap check, see HeapStackCheck.cmm:GC_GENERIC
    mkAssign hpAlloc (zeroExpr dflags),
    open_nursery,
    -- and load the current cost centre stack from the TSO when profiling:
    if gopt Opt_SccProfilingOn dflags
       then storeCurCCS
              (CmmLoad (cmmOffset dflags (CmmReg (CmmLocal tso))
                 (tso_CCCS dflags)) (ccsType dflags))
       else mkNop
   ]


emitOpenNursery :: FCode ()
emitOpenNursery = do
  dflags <- getDynFlags
  tso <- newTemp (bWord dflags)
  code <- openNursery dflags tso
  emit $ mkAssign (CmmLocal tso) stgCurrentTSO <*> code

{- |
@openNursery dflags tso@ produces code to open the nursery. A local register
holding the value of @CurrentTSO@ is expected for efficiency.

Opening the nursery corresponds to the following code:

@
   tso = CurrentTSO;
   cn = CurrentNursery;
   bdfree = CurrentNuresry->free;
   bdstart = CurrentNuresry->start;

   // We *add* the currently occupied portion of the nursery block to
   // the allocation limit, because we will subtract it again in
   // closeNursery.
   tso->alloc_limit += bdfree - bdstart;

   // Set Hp to the last occupied word of the heap block.  Why not the
   // next unocupied word?  Doing it this way means that we get to use
   // an offset of zero more often, which might lead to slightly smaller
   // code on some architectures.
   Hp = bdfree - WDS(1);

   // Set HpLim to the end of the current nursery block (note that this block
   // might be a block group, consisting of several adjacent blocks.
   HpLim = bdstart + CurrentNursery->blocks*BLOCK_SIZE_W - 1;
@
-}
openNursery :: MonadUnique m => DynFlags -> LocalReg -> m CmmAGraph
openNursery df tso = do
  let tsoreg =  CmmLocal tso
  cnreg      <- CmmLocal <$> newTemp (bWord df)
  bdfreereg  <- CmmLocal <$> newTemp (bWord df)
  bdstartreg <- CmmLocal <$> newTemp (bWord df)

  -- These assignments are carefully ordered to reduce register
  -- pressure and generate not completely awful code on x86.  To see
  -- what code we generate, look at the assembly for
  -- stg_returnToStackTop in rts/StgStartup.cmm.
  pure $ catAGraphs [
     mkAssign cnreg stgCurrentNursery,
     mkAssign bdfreereg  (CmmLoad (nursery_bdescr_free df cnreg)  (bWord df)),

     -- Hp = CurrentNursery->free - 1;
     mkAssign hp (cmmOffsetW df (CmmReg bdfreereg) (-1)),

     mkAssign bdstartreg (CmmLoad (nursery_bdescr_start df cnreg) (bWord df)),

     -- HpLim = CurrentNursery->start +
     --              CurrentNursery->blocks*BLOCK_SIZE_W - 1;
     mkAssign hpLim
         (cmmOffsetExpr df
             (CmmReg bdstartreg)
             (cmmOffset df
               (CmmMachOp (mo_wordMul df) [
                 CmmMachOp (MO_SS_Conv W32 (wordWidth df))
                   [CmmLoad (nursery_bdescr_blocks df cnreg) b32],
                 mkIntExpr df (bLOCK_SIZE df)
                ])
               (-1)
             )
         ),

     -- alloc = bd->free - bd->start
     let alloc =
           CmmMachOp (mo_wordSub df) [CmmReg bdfreereg, CmmReg bdstartreg]

         alloc_limit = cmmOffset df (CmmReg tsoreg) (tso_alloc_limit df)
     in

     -- tso->alloc_limit += alloc
     mkStore alloc_limit (CmmMachOp (MO_Add W64)
                               [ CmmLoad alloc_limit b64
                               , CmmMachOp (mo_WordTo64 df) [alloc] ])

   ]

nursery_bdescr_free, nursery_bdescr_start, nursery_bdescr_blocks
  :: DynFlags -> CmmReg -> CmmExpr
nursery_bdescr_free   dflags cn =
  cmmOffset dflags (CmmReg cn) (oFFSET_bdescr_free dflags)
nursery_bdescr_start  dflags cn =
  cmmOffset dflags (CmmReg cn) (oFFSET_bdescr_start dflags)
nursery_bdescr_blocks dflags cn =
  cmmOffset dflags (CmmReg cn) (oFFSET_bdescr_blocks dflags)

tso_stackobj, tso_CCCS, tso_alloc_limit, stack_STACK, stack_SP :: DynFlags -> ByteOff
tso_stackobj dflags = closureField dflags (oFFSET_StgTSO_stackobj dflags)
tso_alloc_limit dflags = closureField dflags (oFFSET_StgTSO_alloc_limit dflags)
tso_CCCS     dflags = closureField dflags (oFFSET_StgTSO_cccs dflags)
stack_STACK  dflags = closureField dflags (oFFSET_StgStack_stack dflags)
stack_SP     dflags = closureField dflags (oFFSET_StgStack_sp dflags)


closureField :: DynFlags -> ByteOff -> ByteOff
closureField dflags off = off + fixedHdrSize dflags

stgSp, stgHp, stgCurrentTSO, stgCurrentNursery :: CmmExpr
stgSp             = CmmReg sp
stgHp             = CmmReg hp
stgCurrentTSO     = CmmReg currentTSO
stgCurrentNursery = CmmReg currentNursery

sp, spLim, hp, hpLim, currentTSO, currentNursery, hpAlloc :: CmmReg
sp                = CmmGlobal Sp
spLim             = CmmGlobal SpLim
hp                = CmmGlobal Hp
hpLim             = CmmGlobal HpLim
currentTSO        = CmmGlobal CurrentTSO
currentNursery    = CmmGlobal CurrentNursery
hpAlloc           = CmmGlobal HpAlloc

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
    get arg | null arg_reps
            = return Nothing
            | otherwise
            = do { cmm <- getArgAmode (NonVoid arg)
                 ; dflags <- getDynFlags
                 ; return (Just (add_shim dflags arg_ty cmm, hint)) }
            where
              arg_ty   = stgArgType arg
              arg_reps = typePrimRep arg_ty
              hint     = typeForeignHint arg_ty

add_shim :: DynFlags -> Type -> CmmExpr -> CmmExpr
add_shim dflags arg_ty expr
  | tycon == arrayPrimTyCon || tycon == mutableArrayPrimTyCon
  = cmmOffsetB dflags expr (arrPtrsHdrSize dflags)

  | tycon == smallArrayPrimTyCon || tycon == smallMutableArrayPrimTyCon
  = cmmOffsetB dflags expr (smallArrPtrsHdrSize dflags)

  | tycon == byteArrayPrimTyCon || tycon == mutableByteArrayPrimTyCon
  = cmmOffsetB dflags expr (arrWordsHdrSize dflags)

  | otherwise = expr
  where
    tycon           = tyConAppTyCon (unwrapType arg_ty)
        -- should be a tycon app, since this is a foreign call
