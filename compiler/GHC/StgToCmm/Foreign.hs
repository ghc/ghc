-----------------------------------------------------------------------------
--
-- Code generation for foreign calls.
--
-- (c) The University of Glasgow 2004-2006
--
-----------------------------------------------------------------------------

module GHC.StgToCmm.Foreign (
  cgForeignCall,
  emitPrimCall,
  emitCCall,
  emitCCallNeverReturns,
  emitForeignCall,
  emitSaveThreadState,
  saveThreadState,
  emitLoadThreadState,
  emitSaveRegs,
  emitRestoreRegs,
  emitPushArgRegs,
  emitPopArgRegs,
  loadThreadState,
  emitOpenNursery,
  emitCloseNursery,
 ) where

import GHC.Prelude hiding( succ, (<*>) )

import GHC.Platform
import GHC.Platform.Profile

import GHC.Stg.Syntax
import GHC.StgToCmm.Prof (storeCurCCS, ccsType)
import GHC.StgToCmm.Monad
import GHC.StgToCmm.Utils
import GHC.StgToCmm.Closure
import GHC.StgToCmm.Layout

import GHC.Cmm.BlockId (newBlockId)
import GHC.Cmm
import GHC.Cmm.Utils
import GHC.Cmm.Graph
import GHC.Cmm.CallConv
import GHC.Core.Type
import GHC.Types.RepType
import GHC.Cmm.CLabel
import GHC.Runtime.Heap.Layout
import GHC.Types.ForeignCall
import GHC.Data.Maybe
import GHC.Utils.Panic
import GHC.Types.Unique.Supply
import GHC.Types.Basic
import GHC.Unit.Types

import GHC.Core.TyCo.Rep
import GHC.Builtin.Types.Prim
import GHC.Utils.Misc (zipEqual)

import Control.Monad

-----------------------------------------------------------------------------
-- Code generation for Foreign Calls
-----------------------------------------------------------------------------

-- | Emit code for a foreign call, and return the results to the sequel.
-- Precondition: the length of the arguments list is the same as the
-- arity of the foreign function.
cgForeignCall :: ForeignCall            -- the op
              -> Type                   -- type of foreign function
              -> [StgArg]               -- x,y    arguments
              -> Type                   -- result type
              -> FCode ReturnKind

cgForeignCall (CCall (CCallSpec target cconv safety)) typ stg_args res_ty
  = do  { cmm_args <- getFCallArgs stg_args typ
        -- ; traceM $ show cmm_args
        ; (res_regs, res_hints) <- newUnboxedTupleRegs res_ty
        ; let ((call_args, arg_hints), cmm_target)
                = case target of
                   StaticTarget _ _   _      False ->
                       panic "cgForeignCall: unexpected FFI value import"
                   StaticTarget _ lbl mPkgId True
                     -> let labelSource
                                = case mPkgId of
                                        Nothing         -> ForeignLabelInThisPackage
                                        Just pkgId      -> ForeignLabelInPackage (toUnitId pkgId)
                        in  ( unzip cmm_args
                            , CmmLit (CmmLabel
                                        (mkForeignLabel lbl labelSource IsFunction)))

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
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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

And when the safe foreign call is lowered later (see Note [Lower safe
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


emitCCall' :: CmmReturnInfo
           -> [(CmmFormal,ForeignHint)]
           -> CmmExpr
           -> [(CmmActual,ForeignHint)]
           -> FCode ()
emitCCall' ret_info hinted_results fn hinted_args
  = void $ emitForeignCall PlayRisky results target args
  where
    (args, arg_hints) = unzip hinted_args
    (results, result_hints) = unzip hinted_results
    target = ForeignTarget fn fc
    fc = ForeignConvention CCallConv arg_hints result_hints ret_info

emitCCall :: [(CmmFormal,ForeignHint)]
          -> CmmExpr
          -> [(CmmActual,ForeignHint)]
          -> FCode ()
emitCCall = emitCCall' CmmMayReturn

emitCCallNeverReturns
  :: [(CmmFormal,ForeignHint)]
  -> CmmExpr
  -> [(CmmActual,ForeignHint)]
  -> FCode ()
emitCCallNeverReturns = emitCCall' CmmNeverReturns


emitPrimCall :: [CmmFormal] -> CallishMachOp -> [CmmActual] -> FCode ()
emitPrimCall res op args
  = void $ emitForeignCall PlayRisky res (PrimTarget op) args

-- alternative entry point, used by GHC.Cmm.Parser
emitForeignCall
        :: Safety
        -> [CmmFormal]          -- where to put the results
        -> ForeignTarget        -- the op
        -> [CmmActual]          -- arguments
        -> FCode ReturnKind
emitForeignCall safety results target args
  | not (playSafe safety) = do
    platform <- getPlatform
    let (caller_save, caller_load) = callerSaveVolatileRegs platform
    emit caller_save
    target' <- load_target_into_temp target
    args' <- mapM maybe_assign_temp args
    emit $ mkUnsafeCall target' results args'
    emit caller_load
    return AssignedDirectly

  | otherwise = do
    profile <- getProfile
    platform <- getPlatform
    updfr_off <- getUpdFrameOff
    target' <- load_target_into_temp target
    args' <- mapM maybe_assign_temp args
    k <- newBlockId
    let (off, _, copyout) = copyInOflow profile NativeReturn (Young k) results []
       -- see Note [safe foreign call convention]
    tscope <- getTickScope
    emit $
           (    mkStore (CmmStackSlot (Young k) (widthInBytes (wordWidth platform)))
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

-- Note [Saving foreign call target to local]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- What we want to do here is create a new temporary for the foreign
-- call argument if it is not safe to use the expression directly,
-- because the expression mentions caller-saves GlobalRegs (see
-- Note [Register parameter passing]).
--
-- However, we can't pattern-match on the expression here, because
-- this is used in a loop by GHC.Cmm.Parser, and testing the
-- expression results in a black hole.  So when there exist
-- caller-saves GlobalRegs, we create a temporary, and rely on
-- GHC.Cmm.Sink to clean it up later. The generated code ends up being
-- the same if -fcmm-sink is enabled (implied by -O).
--
-- When there doesn't exist caller-save GlobalRegs, keep the original
-- target in place. This matters for the wasm backend, otherwise it
-- cannot infer the target symbol's correct foreign function type in
-- unoptimized Cmm. For instance:
--
-- foreign import ccall unsafe "foo" c_foo :: IO ()
--
-- Without optimization, previously this would lower to something like:
--
-- [Test.c_foo_entry() { //  []
--          { []
--          }
--      {offset
--        cDk:
--            goto cDm;
--        cDm:
--            _cDj::I32 = foo;
--            call "ccall" arg hints:  []  result hints:  [] (_cDj::I32)();
--            R1 = GHC.Tuple.()_closure+1;
--            call (I32[P32[Sp]])(R1) args: 4, res: 0, upd: 4;
--      }
--  },
--
-- The wasm backend only sees "foo" being assigned to a local, but
-- there's no type signature associated with a CLabel! So it has to
-- emit a dummy .functype directive and fingers crossed that wasm-ld
-- tolerates function type mismatch. THis is horrible, not future
-- proof against upstream toolchain upgrades, and already known to
-- break in certain cases (e.g. when LTO objects are involved).
--
-- Therefore, on wasm as well as other targets that don't risk
-- mentioning caller-saved GlobalRegs in a foreign call target, just
-- keep the original call target in place and don't assign it to a
-- local. So this would now lower to something like:
--
-- [Test.c_foo_entry() { //  []
--          { []
--          }
--      {offset
--        cDo:
--            goto cDq;
--        cDq:
--            call "ccall" arg hints:  []  result hints:  [] foo();
--            R1 = GHC.Tuple.()_closure+1;
--            call (I32[P32[Sp]])(R1) args: 4, res: 0, upd: 4;
--      }
--  },
--
-- Since "foo" appears at call site directly, the wasm backend would
-- now be able to infer its type signature correctly.

maybe_assign_temp :: CmmExpr -> FCode CmmExpr
maybe_assign_temp e = do
  do_save <- stgToCmmSaveFCallTargetToLocal <$> getStgToCmmConfig
  if do_save
    then do
      platform <- getPlatform
      reg <- newTemp (cmmExprType platform e)
      emitAssign (CmmLocal reg) e
      return (CmmReg (CmmLocal reg))
    else
      pure e

-- -----------------------------------------------------------------------------
-- Save/restore the thread state in the TSO

-- This stuff can't be done in suspendThread/resumeThread, because it
-- refers to global registers which aren't available in the C world.

emitSaveThreadState :: FCode ()
emitSaveThreadState = do
  profile <- getProfile
  code <- saveThreadState profile
  emit code

-- | Produce code to save the current thread state to @CurrentTSO@
saveThreadState :: MonadUnique m => Profile -> m CmmAGraph
saveThreadState profile = do
  let platform = profilePlatform profile
  tso <- newTemp (gcWord platform)
  close_nursery <- closeNursery profile tso
  pure $ catAGraphs
   [ -- tso = CurrentTSO;
     mkAssign (CmmLocal tso) (currentTSOExpr platform)

   , -- tso->stackobj->sp = Sp;
     mkStore (cmmOffset platform
                        (cmmLoadBWord platform (cmmOffset platform
                                            (CmmReg (CmmLocal tso))
                                            (tso_stackobj profile)))
                        (stack_SP profile))
             (spExpr platform)

    , close_nursery

    , -- and save the current cost centre stack in the TSO when profiling:
      if profileIsProfiling profile
         then mkStore (cmmOffset platform (CmmReg (CmmLocal tso)) (tso_CCCS profile))
                      (cccsExpr platform)
         else mkNop
    ]



-- | Save STG registers
--
-- STG registers must be saved around a C call, just in case the STG
-- register is mapped to a caller-saves machine register.  Normally we
-- don't need to worry about this the code generator has already
-- loaded any live STG registers into variables for us, but in
-- hand-written low-level Cmm code where we don't know which registers
-- are live, we might have to save them all.
emitSaveRegs :: FCode ()
emitSaveRegs = do
   platform <- getPlatform
   let regs = realArgRegsCover platform
       save = catAGraphs (map (callerSaveGlobalReg platform) regs)
   emit save

-- | Restore STG registers (see 'emitSaveRegs')
emitRestoreRegs :: FCode ()
emitRestoreRegs = do
   platform <- getPlatform
   let regs    = realArgRegsCover platform
       restore = catAGraphs (map (callerRestoreGlobalReg platform) regs)
   emit restore

-- | Push a subset of STG registers onto the stack, specified by the bitmap
--
-- Sometimes, a "live" subset of the STG registers needs to be saved on the
-- stack, for example when storing an unboxed tuple to be used in the GHCi
-- bytecode interpreter.
--
-- The "live registers" bitmap corresponds to the list of registers given by
-- 'allArgRegsCover', with the least significant bit indicating liveness of
-- the first register in the list.
--
-- Each register is saved to a stack slot of one or more machine words, even
-- if the register size itself is smaller.
--
-- The resulting Cmm code looks like this, with a line for each real or
-- virtual register used for returning tuples:
--
--    ...
--    if((mask & 2) != 0) { Sp_adj(-1); Sp(0) = R2; }
--    if((mask & 1) != 0) { Sp_adj(-1); Sp(0) = R1; }
--
-- See Note [GHCi and native call registers]

emitPushArgRegs :: CmmExpr -> FCode ()
emitPushArgRegs regs_live = do
  platform <- getPlatform
  let regs = zip (allArgRegsCover platform) [0..]
      save_arg (reg, n) =
        let mask     = CmmLit (CmmInt (1 `shiftL` n) (wordWidth platform))
            live     = cmmAndWord platform regs_live mask
            cond     = cmmNeWord platform live (zeroExpr platform)
            reg_ty   = globalRegSpillType platform reg
            width    = roundUpToWords platform
                                      (widthInBytes $ typeWidth reg_ty)
            adj_sp   = mkAssign (spReg platform)
                                (cmmOffset platform (spExpr platform) (negate width))
            save_reg = mkStore (spExpr platform) (CmmReg $ CmmGlobal $ GlobalRegUse reg reg_ty)
        in mkCmmIfThen cond $ catAGraphs [adj_sp, save_reg]
  emit . catAGraphs =<< mapM save_arg (reverse $ regs)

-- | Pop a subset of STG registers from the stack (see 'emitPushArgRegs')
emitPopArgRegs :: CmmExpr -> FCode ()
emitPopArgRegs regs_live = do
  platform <- getPlatform
  let regs = zip (allArgRegsCover platform) [0..]
      save_arg (reg, n) =
        let mask     = CmmLit (CmmInt (1 `shiftL` n) (wordWidth platform))
            live     = cmmAndWord platform regs_live mask
            cond     = cmmNeWord platform live (zeroExpr platform)
            reg_ty   = globalRegSpillType platform reg
            width    = roundUpToWords platform
                                      (widthInBytes $ typeWidth reg_ty)
            adj_sp   = mkAssign (spReg platform)
                                (cmmOffset platform (spExpr platform) width)
            restore_reg = mkAssign (CmmGlobal $ GlobalRegUse reg reg_ty)
                                   (CmmLoad (spExpr platform) reg_ty NaturallyAligned)
        in mkCmmIfThen cond $ catAGraphs [restore_reg, adj_sp]
  emit . catAGraphs =<< mapM save_arg regs


emitCloseNursery :: FCode ()
emitCloseNursery = do
  profile <- getProfile
  let platform = profilePlatform profile
  tso <- newTemp (bWord platform)
  code <- closeNursery profile tso
  emit $ mkAssign (CmmLocal tso) (currentTSOExpr platform) <*> code

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
closeNursery :: MonadUnique m => Profile -> LocalReg -> m CmmAGraph
closeNursery profile tso = do
  let tsoreg   = CmmLocal tso
      platform = profilePlatform profile
  cnreg      <- CmmLocal <$> newTemp (bWord platform)
  pure $ catAGraphs [
    mkAssign cnreg (currentNurseryExpr platform),

    -- CurrentNursery->free = Hp+1;
    mkStore (nursery_bdescr_free platform cnreg) (cmmOffsetW platform (hpExpr platform) 1),

    let alloc =
           CmmMachOp (mo_wordSub platform)
              [ cmmOffsetW platform (hpExpr platform) 1
              , cmmLoadBWord platform (nursery_bdescr_start platform cnreg)
              ]

        alloc_limit = cmmOffset platform (CmmReg tsoreg) (tso_alloc_limit profile)
    in

    -- tso->alloc_limit += alloc
    mkStore alloc_limit (CmmMachOp (MO_Sub W64)
                               [ CmmLoad alloc_limit b64 NaturallyAligned
                               , CmmMachOp (mo_WordTo64 platform) [alloc] ])
   ]

emitLoadThreadState :: FCode ()
emitLoadThreadState = do
  profile <- getProfile
  code <- loadThreadState profile
  emit code

-- | Produce code to load the current thread state from @CurrentTSO@
loadThreadState :: MonadUnique m => Profile -> m CmmAGraph
loadThreadState profile = do
  let platform = profilePlatform profile
  tso <- newTemp (gcWord platform)
  stack <- newTemp (gcWord platform)
  open_nursery <- openNursery profile tso
  pure $ catAGraphs [
    -- tso = CurrentTSO;
    mkAssign (CmmLocal tso) (currentTSOExpr platform),
    -- stack = tso->stackobj;
    mkAssign (CmmLocal stack) (cmmLoadBWord platform (cmmOffset platform (CmmReg (CmmLocal tso)) (tso_stackobj profile))),
    -- Sp = stack->sp;
    mkAssign (spReg platform) (cmmLoadBWord platform (cmmOffset platform (CmmReg (CmmLocal stack)) (stack_SP profile))),
    -- SpLim = stack->stack + RESERVED_STACK_WORDS;
    mkAssign (spLimReg platform) (cmmOffsetW platform (cmmOffset platform (CmmReg (CmmLocal stack)) (stack_STACK profile))
                                (pc_RESERVED_STACK_WORDS (platformConstants platform))),
    -- HpAlloc = 0;
    --   HpAlloc is assumed to be set to non-zero only by a failed
    --   a heap check, see HeapStackCheck.cmm:GC_GENERIC
    mkAssign (hpAllocReg platform) (zeroExpr platform),
    open_nursery,
    -- and load the current cost centre stack from the TSO when profiling:
    if profileIsProfiling profile
       then let ccs_ptr = cmmOffset platform (CmmReg (CmmLocal tso)) (tso_CCCS profile)
            in storeCurCCS platform (CmmLoad ccs_ptr (ccsType platform) NaturallyAligned)
       else mkNop
   ]


emitOpenNursery :: FCode ()
emitOpenNursery = do
  profile <- getProfile
  let platform = profilePlatform profile
  tso <- newTemp (bWord platform)
  code <- openNursery profile tso
  emit $ mkAssign (CmmLocal tso) (currentTSOExpr platform) <*> code

{- |
@openNursery profile tso@ produces code to open the nursery. A local register
holding the value of @CurrentTSO@ is expected for efficiency.

Opening the nursery corresponds to the following code:

@
   tso = CurrentTSO;
   cn = CurrentNursery;
   bdfree = CurrentNursery->free;
   bdstart = CurrentNursery->start;

   // We *add* the currently occupied portion of the nursery block to
   // the allocation limit, because we will subtract it again in
   // closeNursery.
   tso->alloc_limit += bdfree - bdstart;

   // Set Hp to the last occupied word of the heap block.  Why not the
   // next unoccupied word?  Doing it this way means that we get to use
   // an offset of zero more often, which might lead to slightly smaller
   // code on some architectures.
   Hp = bdfree - WDS(1);

   // Set HpLim to the end of the current nursery block (note that this block
   // might be a block group, consisting of several adjacent blocks.
   HpLim = bdstart + CurrentNursery->blocks*BLOCK_SIZE_W - 1;
@
-}
openNursery :: MonadUnique m => Profile -> LocalReg -> m CmmAGraph
openNursery profile tso = do
  let tsoreg   = CmmLocal tso
      platform = profilePlatform profile
  cnreg      <- CmmLocal <$> newTemp (bWord platform)
  bdfreereg  <- CmmLocal <$> newTemp (bWord platform)
  bdstartreg <- CmmLocal <$> newTemp (bWord platform)

  -- These assignments are carefully ordered to reduce register
  -- pressure and generate not completely awful code on x86.  To see
  -- what code we generate, look at the assembly for
  -- stg_returnToStackTop in rts/StgStartup.cmm.
  pure $ catAGraphs [
     mkAssign cnreg (currentNurseryExpr platform),
     mkAssign bdfreereg  (cmmLoadBWord platform (nursery_bdescr_free platform cnreg)),

     -- Hp = CurrentNursery->free - 1;
     mkAssign (hpReg platform) (cmmOffsetW platform (CmmReg bdfreereg) (-1)),

     mkAssign bdstartreg (cmmLoadBWord platform (nursery_bdescr_start platform cnreg)),

     -- HpLim = CurrentNursery->start +
     --              CurrentNursery->blocks*BLOCK_SIZE_W - 1;
     mkAssign (hpLimReg platform)
         (cmmOffsetExpr platform
             (CmmReg bdstartreg)
             (cmmOffset platform
               (CmmMachOp (mo_wordMul platform)
                 [ CmmMachOp (MO_SS_Conv W32 (wordWidth platform))
                     [CmmLoad (nursery_bdescr_blocks platform cnreg) b32 NaturallyAligned]
                 , mkIntExpr platform (pc_BLOCK_SIZE (platformConstants platform))
                 ])
               (-1)
             )
         ),

     -- alloc = bd->free - bd->start
     let alloc =
           CmmMachOp (mo_wordSub platform) [CmmReg bdfreereg, CmmReg bdstartreg]

         alloc_limit = cmmOffset platform (CmmReg tsoreg) (tso_alloc_limit profile)
     in

     -- tso->alloc_limit += alloc
     mkStore alloc_limit (CmmMachOp (MO_Add W64)
                               [ CmmLoad alloc_limit b64 NaturallyAligned
                               , CmmMachOp (mo_WordTo64 platform) [alloc] ])

   ]

nursery_bdescr_free, nursery_bdescr_start, nursery_bdescr_blocks
  :: Platform -> CmmReg -> CmmExpr
nursery_bdescr_free   platform cn =
  cmmOffset platform (CmmReg cn) (pc_OFFSET_bdescr_free (platformConstants platform))
nursery_bdescr_start  platform cn =
  cmmOffset platform (CmmReg cn) (pc_OFFSET_bdescr_start (platformConstants platform))
nursery_bdescr_blocks platform cn =
  cmmOffset platform (CmmReg cn) (pc_OFFSET_bdescr_blocks (platformConstants platform))

tso_stackobj, tso_CCCS, tso_alloc_limit, stack_STACK, stack_SP :: Profile -> ByteOff
tso_stackobj    profile = closureField profile (pc_OFFSET_StgTSO_stackobj    (profileConstants profile))
tso_alloc_limit profile = closureField profile (pc_OFFSET_StgTSO_alloc_limit (profileConstants profile))
tso_CCCS        profile = closureField profile (pc_OFFSET_StgTSO_cccs        (profileConstants profile))
stack_STACK     profile = closureField profile (pc_OFFSET_StgStack_stack     (profileConstants profile))
stack_SP        profile = closureField profile (pc_OFFSET_StgStack_sp        (profileConstants profile))


closureField :: Profile -> ByteOff -> ByteOff
closureField profile off = off + fixedHdrSize profile

-- Note [Unlifted boxed arguments to foreign calls]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- For certain types passed to foreign calls, we adjust the actual
-- value passed to the call.  For ByteArray#, Array# and SmallArray#,
-- we pass the address of the array's payload, not the address of
-- the heap object. For example, consider:
--   foreign import "c_foo" foo :: ByteArray# -> Int# -> IO ()
-- At a Haskell call like `foo x y`, we'll generate a C call that
-- is more like
--   c_foo( x+8, y )
-- where the "+8" takes the heap pointer (x :: ByteArray#) and moves
-- it past the header words of the ByteArray object to point directly
-- to the data inside the ByteArray#. (The exact offset depends
-- on the target architecture and on profiling) By contrast, (y :: Int#)
-- requires no such adjustment.
--
-- This adjustment is performed by 'add_shim'. The size of the
-- adjustment depends on the type of heap object. But
-- how can we determine that type? There are two available options.
-- We could use the types of the actual values that the foreign call
-- has been applied to, or we could use the types present in the
-- foreign function's type. Prior to GHC 8.10, we used the former
-- strategy since it's a little more simple. However, in issue #16650
-- and more compellingly in the comments of
-- https://gitlab.haskell.org/ghc/ghc/merge_requests/939, it was
-- demonstrated that this leads to bad behavior in the presence
-- of unsafeCoerce#. Returning to the above example, suppose the
-- Haskell call looked like
--   foo (unsafeCoerce# p)
-- where the types of expressions comprising the arguments are
--   p :: (Any :: TYPE 'UnliftedRep)
--   i :: Int#
-- so that the unsafe-coerce is between Any and ByteArray#.
-- These two types have the same kind (they are both represented by
-- a heap pointer) so no GC errors will occur if we do this unsafe coerce.
-- By the time this gets to the code generator the cast has been
-- discarded so we have
--   foo p y
-- But we *must* adjust the pointer to p by a ByteArray# shim,
-- *not* by an Any shim (the Any shim involves no offset at all).
--
-- To avoid this bad behavior, we adopt the second strategy: use
-- the types present in the foreign function's type.
-- In collectStgFArgTypes, we convert the foreign function's
-- type to a list of StgFArgType. Then, in add_shim, we interpret
-- these as numeric offsets.

getFCallArgs ::
     [StgArg]
  -> Type -- the type of the foreign function
  -> FCode [(CmmExpr, ForeignHint)]
-- (a) Drop void args
-- (b) Add foreign-call shim code
-- It's (b) that makes this differ from getNonVoidArgAmodes
-- Precondition: args and typs have the same length
-- See Note [Unlifted boxed arguments to foreign calls]
getFCallArgs args typ
  = do  { mb_cmms <- mapM get (zipEqual "getFCallArgs" args (collectStgFArgTypes typ))
        ; return (catMaybes mb_cmms) }
  where
    get (arg,typ)
      | null arg_reps
      = return Nothing
      | otherwise
      = do { cmm <- getArgAmode (NonVoid arg)
           ; profile <- getProfile
           ; return (Just (add_shim profile typ cmm, hint)) }
      where
        arg_ty   = stgArgType arg
        arg_reps = typePrimRep arg_ty
        hint     = typeForeignHint arg_ty

-- The minimum amount of information needed to determine
-- the offset to apply to an argument to a foreign call.
-- See Note [Unlifted boxed arguments to foreign calls]
data StgFArgType
  = StgPlainType
  | StgArrayType
  | StgSmallArrayType
  | StgByteArrayType

-- See Note [Unlifted boxed arguments to foreign calls]
add_shim :: Profile -> StgFArgType -> CmmExpr -> CmmExpr
add_shim profile ty expr = case ty of
  StgPlainType      -> expr
  StgArrayType      -> cmmOffsetB platform expr (arrPtrsHdrSize profile)
  StgSmallArrayType -> cmmOffsetB platform expr (smallArrPtrsHdrSize profile)
  StgByteArrayType  -> cmmOffsetB platform expr (arrWordsHdrSize profile)
  where
    platform = profilePlatform profile

-- From a function, extract information needed to determine
-- the offset of each argument when used as a C FFI argument.
-- See Note [Unlifted boxed arguments to foreign calls]
collectStgFArgTypes :: Type -> [StgFArgType]
collectStgFArgTypes = go []
  where
    -- Skip foralls
    go bs (ForAllTy _ res) = go bs res
    go bs (AppTy{}) = reverse bs
    go bs (TyConApp{}) = reverse bs
    go bs (LitTy{}) = reverse bs
    go bs (TyVarTy{}) = reverse bs
    go  _ (CastTy{}) = panic "myCollectTypeArgs: CastTy"
    go  _ (CoercionTy{}) = panic "myCollectTypeArgs: CoercionTy"
    go bs (FunTy {ft_arg = arg, ft_res=res}) =
      go (typeToStgFArgType arg:bs) res

-- Choose the offset based on the type. For anything other
-- than an unlifted boxed type, there is no offset.
-- See Note [Unlifted boxed arguments to foreign calls]
typeToStgFArgType :: Type -> StgFArgType
typeToStgFArgType typ
  | tycon == arrayPrimTyCon = StgArrayType
  | tycon == mutableArrayPrimTyCon = StgArrayType
  | tycon == smallArrayPrimTyCon = StgSmallArrayType
  | tycon == smallMutableArrayPrimTyCon = StgSmallArrayType
  | tycon == byteArrayPrimTyCon = StgByteArrayType
  | tycon == mutableByteArrayPrimTyCon = StgByteArrayType
  | otherwise = StgPlainType
  where
  -- Should be a tycon app, since this is a foreign call. We look
  -- through newtypes so the offset does not change if a user replaces
  -- a type in a foreign function signature with a representationally
  -- equivalent newtype.
  tycon = tyConAppTyCon (unwrapType typ)
