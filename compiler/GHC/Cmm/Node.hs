{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns   #-}

-- CmmNode type for representation using Hoopl graphs.

module GHC.Cmm.Node (
     CmmNode(..), CmmFormal, CmmActual, CmmTickish,
     UpdFrameOffset, Convention(..),
     ForeignConvention(..), ForeignTarget(..), foreignTargetHints,
     CmmReturnInfo(..),
     mapExp, mapExpDeep, wrapRecExp, foldExp, foldExpDeep, wrapRecExpf,
     mapExpM, mapExpDeepM, wrapRecExpM, mapSuccessors, mapCollectSuccessors,

     -- * Tick scopes
     CmmTickScope(..), isTickSubScope, combineTickScopes,
  ) where

import GHC.Prelude hiding (succ)

import GHC.Platform.Regs
import GHC.Cmm.Expr
import GHC.Cmm.Switch
import GHC.Driver.Session
import GHC.Data.FastString
import GHC.Types.ForeignCall
import GHC.Utils.Outputable
import GHC.Runtime.Heap.Layout
import GHC.Core (Tickish)
import qualified GHC.Types.Unique as U

import GHC.Cmm.Dataflow.Block
import GHC.Cmm.Dataflow.Graph
import GHC.Cmm.Dataflow.Collections
import GHC.Cmm.Dataflow.Label
import Data.Maybe
import Data.List (tails,sortBy)
import GHC.Types.Unique (nonDetCmpUnique)
import GHC.Utils.Misc


------------------------
-- CmmNode

#define ULabel {-# UNPACK #-} !Label

data CmmNode e x where
  CmmEntry :: ULabel -> CmmTickScope -> CmmNode C O

  CmmComment :: FastString -> CmmNode O O

    -- Tick annotation, covering Cmm code in our tick scope. We only
    -- expect non-code @Tickish@ at this point (e.g. @SourceNote@).
    -- See Note [CmmTick scoping details]
  CmmTick :: !CmmTickish -> CmmNode O O

    -- Unwind pseudo-instruction, encoding stack unwinding
    -- instructions for a debugger. This describes how to reconstruct
    -- the "old" value of a register if we want to navigate the stack
    -- up one frame. Having unwind information for @Sp@ will allow the
    -- debugger to "walk" the stack.
    --
    -- See Note [What is this unwinding business?] in "GHC.Cmm.DebugBlock"
  CmmUnwind :: [(GlobalReg, Maybe CmmExpr)] -> CmmNode O O

  CmmAssign :: !CmmReg -> !CmmExpr -> CmmNode O O
    -- Assign to register

  CmmStore :: !CmmExpr -> !CmmExpr -> CmmNode O O
    -- Assign to memory location.  Size is
    -- given by cmmExprType of the rhs.

  CmmUnsafeForeignCall ::       -- An unsafe foreign call;
                                -- see Note [Foreign calls]
                                -- Like a "fat machine instruction"; can occur
                                -- in the middle of a block
      ForeignTarget ->          -- call target
      [CmmFormal] ->            -- zero or more results
      [CmmActual] ->            -- zero or more arguments
      CmmNode O O
      -- Semantics: clobbers any GlobalRegs for which callerSaves r == True
      -- See Note [Unsafe foreign calls clobber caller-save registers]
      --
      -- Invariant: the arguments and the ForeignTarget must not
      -- mention any registers for which GHC.Platform.callerSaves
      -- is True.  See Note [Register parameter passing].

  CmmBranch :: ULabel -> CmmNode O C
                                   -- Goto another block in the same procedure

  CmmCondBranch :: {                 -- conditional branch
      cml_pred :: CmmExpr,
      cml_true, cml_false :: ULabel,
      cml_likely :: Maybe Bool       -- likely result of the conditional,
                                     -- if known
  } -> CmmNode O C

  CmmSwitch
    :: CmmExpr       -- Scrutinee, of some integral type
    -> SwitchTargets -- Cases. See [Note SwitchTargets]
    -> CmmNode O C

  CmmCall :: {                -- A native call or tail call
      cml_target :: CmmExpr,  -- never a CmmPrim to a CallishMachOp!

      cml_cont :: Maybe Label,
          -- Label of continuation (Nothing for return or tail call)
          --
          -- Note [Continuation BlockIds]: these BlockIds are called
          -- Continuation BlockIds, and are the only BlockIds that can
          -- occur in CmmExprs, namely as (CmmLit (CmmBlock b)) or
          -- (CmmStackSlot (Young b) _).

      cml_args_regs :: [GlobalReg],
          -- The argument GlobalRegs (Rx, Fx, Dx, Lx) that are passed
          -- to the call.  This is essential information for the
          -- native code generator's register allocator; without
          -- knowing which GlobalRegs are live it has to assume that
          -- they are all live.  This list should only include
          -- GlobalRegs that are mapped to real machine registers on
          -- the target platform.

      cml_args :: ByteOff,
          -- Byte offset, from the *old* end of the Area associated with
          -- the Label (if cml_cont = Nothing, then Old area), of
          -- youngest outgoing arg.  Set the stack pointer to this before
          -- transferring control.
          -- (NB: an update frame might also have been stored in the Old
          --      area, but it'll be in an older part than the args.)

      cml_ret_args :: ByteOff,
          -- For calls *only*, the byte offset for youngest returned value
          -- This is really needed at the *return* point rather than here
          -- at the call, but in practice it's convenient to record it here.

      cml_ret_off :: ByteOff
        -- For calls *only*, the byte offset of the base of the frame that
        -- must be described by the info table for the return point.
        -- The older words are an update frames, which have their own
        -- info-table and layout information

        -- From a liveness point of view, the stack words older than
        -- cml_ret_off are treated as live, even if the sequel of
        -- the call goes into a loop.
  } -> CmmNode O C

  CmmForeignCall :: {           -- A safe foreign call; see Note [Foreign calls]
                                -- Always the last node of a block
      tgt   :: ForeignTarget,   -- call target and convention
      res   :: [CmmFormal],     -- zero or more results
      args  :: [CmmActual],     -- zero or more arguments; see Note [Register parameter passing]
      succ  :: ULabel,          -- Label of continuation
      ret_args :: ByteOff,      -- same as cml_ret_args
      ret_off :: ByteOff,       -- same as cml_ret_off
      intrbl:: Bool             -- whether or not the call is interruptible
  } -> CmmNode O C

{- Note [Foreign calls]
~~~~~~~~~~~~~~~~~~~~~~~
A CmmUnsafeForeignCall is used for *unsafe* foreign calls;
a CmmForeignCall call is used for *safe* foreign calls.

Unsafe ones are mostly easy: think of them as a "fat machine
instruction".  In particular, they do *not* kill all live registers,
just the registers they return to (there was a bit of code in GHC that
conservatively assumed otherwise.)  However, see [Register parameter passing].

Safe ones are trickier.  A safe foreign call
     r = f(x)
ultimately expands to
     push "return address"      -- Never used to return to;
                                -- just points an info table
     save registers into TSO
     call suspendThread
     r = f(x)                   -- Make the call
     call resumeThread
     restore registers
     pop "return address"
We cannot "lower" a safe foreign call to this sequence of Cmms, because
after we've saved Sp all the Cmm optimiser's assumptions are broken.

Note that a safe foreign call needs an info table.

So Safe Foreign Calls must remain as last nodes until the stack is
made manifest in GHC.Cmm.LayoutStack, where they are lowered into the above
sequence.
-}

{- Note [Unsafe foreign calls clobber caller-save registers]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

A foreign call is defined to clobber any GlobalRegs that are mapped to
caller-saves machine registers (according to the prevailing C ABI).
GHC.StgToCmm.Utils.callerSaves tells you which GlobalRegs are caller-saves.

This is a design choice that makes it easier to generate code later.
We could instead choose to say that foreign calls do *not* clobber
caller-saves regs, but then we would have to figure out which regs
were live across the call later and insert some saves/restores.

Furthermore when we generate code we never have any GlobalRegs live
across a call, because they are always copied-in to LocalRegs and
copied-out again before making a call/jump.  So all we have to do is
avoid any code motion that would make a caller-saves GlobalReg live
across a foreign call during subsequent optimisations.
-}

{- Note [Register parameter passing]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
On certain architectures, some registers are utilized for parameter
passing in the C calling convention.  For example, in x86-64 Linux
convention, rdi, rsi, rdx and rcx (as well as r8 and r9) may be used for
argument passing.  These are registers R3-R6, which our generated
code may also be using; as a result, it's necessary to save these
values before doing a foreign call.  This is done during initial
code generation in callerSaveVolatileRegs in GHC.StgToCmm.Utils.

However, one result of doing this is that the contents of these registers may
mysteriously change if referenced inside the arguments.  This is dangerous, so
you'll need to disable inlining much in the same way is done in GHC.Cmm.Sink
currently.  We should fix this!
-}

---------------------------------------------
-- Eq instance of CmmNode

deriving instance Eq (CmmNode e x)

----------------------------------------------
-- Hoopl instances of CmmNode

instance NonLocal CmmNode where
  entryLabel (CmmEntry l _) = l

  successors (CmmBranch l) = [l]
  successors (CmmCondBranch {cml_true=t, cml_false=f}) = [f, t] -- meets layout constraint
  successors (CmmSwitch _ ids) = switchTargetsToList ids
  successors (CmmCall {cml_cont=l}) = maybeToList l
  successors (CmmForeignCall {succ=l}) = [l]


--------------------------------------------------
-- Various helper types

type CmmActual = CmmExpr
type CmmFormal = LocalReg

type UpdFrameOffset = ByteOff

-- | A convention maps a list of values (function arguments or return
-- values) to registers or stack locations.
data Convention
  = NativeDirectCall
       -- ^ top-level Haskell functions use @NativeDirectCall@, which
       -- maps arguments to registers starting with R2, according to
       -- how many registers are available on the platform.  This
       -- convention ignores R1, because for a top-level function call
       -- the function closure is implicit, and doesn't need to be passed.
  | NativeNodeCall
       -- ^ non-top-level Haskell functions, which pass the address of
       -- the function closure in R1 (regardless of whether R1 is a
       -- real register or not), and the rest of the arguments in
       -- registers or on the stack.
  | NativeReturn
       -- ^ a native return.  The convention for returns depends on
       -- how many values are returned: for just one value returned,
       -- the appropriate register is used (R1, F1, etc.). regardless
       -- of whether it is a real register or not.  For multiple
       -- values returned, they are mapped to registers or the stack.
  | Slow
       -- ^ Slow entry points: all args pushed on the stack
  | GC
       -- ^ Entry to the garbage collector: uses the node reg!
       -- (TODO: I don't think we need this --SDM)
  deriving( Eq )

data ForeignConvention
  = ForeignConvention
        CCallConv               -- Which foreign-call convention
        [ForeignHint]           -- Extra info about the args
        [ForeignHint]           -- Extra info about the result
        CmmReturnInfo
  deriving Eq

data CmmReturnInfo
  = CmmMayReturn
  | CmmNeverReturns
  deriving ( Eq )

data ForeignTarget        -- The target of a foreign call
  = ForeignTarget                -- A foreign procedure
        CmmExpr                  -- Its address
        ForeignConvention        -- Its calling convention
  | PrimTarget            -- A possibly-side-effecting machine operation
        CallishMachOp            -- Which one
  deriving Eq

foreignTargetHints :: ForeignTarget -> ([ForeignHint], [ForeignHint])
foreignTargetHints target
  = ( res_hints ++ repeat NoHint
    , arg_hints ++ repeat NoHint )
  where
    (res_hints, arg_hints) =
       case target of
          PrimTarget op -> callishMachOpHints op
          ForeignTarget _ (ForeignConvention _ arg_hints res_hints _) ->
             (res_hints, arg_hints)

--------------------------------------------------
-- Instances of register and slot users / definers

instance UserOfRegs LocalReg (CmmNode e x) where
  foldRegsUsed dflags f !z n = case n of
    CmmAssign _ expr -> fold f z expr
    CmmStore addr rval -> fold f (fold f z addr) rval
    CmmUnsafeForeignCall t _ args -> fold f (fold f z t) args
    CmmCondBranch expr _ _ _ -> fold f z expr
    CmmSwitch expr _ -> fold f z expr
    CmmCall {cml_target=tgt} -> fold f z tgt
    CmmForeignCall {tgt=tgt, args=args} -> fold f (fold f z tgt) args
    _ -> z
    where fold :: forall a b. UserOfRegs LocalReg a
               => (b -> LocalReg -> b) -> b -> a -> b
          fold f z n = foldRegsUsed dflags f z n

instance UserOfRegs GlobalReg (CmmNode e x) where
  foldRegsUsed dflags f !z n = case n of
    CmmAssign _ expr -> fold f z expr
    CmmStore addr rval -> fold f (fold f z addr) rval
    CmmUnsafeForeignCall t _ args -> fold f (fold f z t) args
    CmmCondBranch expr _ _ _ -> fold f z expr
    CmmSwitch expr _ -> fold f z expr
    CmmCall {cml_target=tgt, cml_args_regs=args} -> fold f (fold f z args) tgt
    CmmForeignCall {tgt=tgt, args=args} -> fold f (fold f z tgt) args
    _ -> z
    where fold :: forall a b.  UserOfRegs GlobalReg a
               => (b -> GlobalReg -> b) -> b -> a -> b
          fold f z n = foldRegsUsed dflags f z n

instance (Ord r, UserOfRegs r CmmReg) => UserOfRegs r ForeignTarget where
  -- The (Ord r) in the context is necessary here
  -- See Note [Recursive superclasses] in GHC.Tc.TyCl.Instance
  foldRegsUsed _      _ !z (PrimTarget _)      = z
  foldRegsUsed dflags f !z (ForeignTarget e _) = foldRegsUsed dflags f z e

instance DefinerOfRegs LocalReg (CmmNode e x) where
  foldRegsDefd dflags f !z n = case n of
    CmmAssign lhs _ -> fold f z lhs
    CmmUnsafeForeignCall _ fs _ -> fold f z fs
    CmmForeignCall {res=res} -> fold f z res
    _ -> z
    where fold :: forall a b. DefinerOfRegs LocalReg a
               => (b -> LocalReg -> b) -> b -> a -> b
          fold f z n = foldRegsDefd dflags f z n

instance DefinerOfRegs GlobalReg (CmmNode e x) where
  foldRegsDefd dflags f !z n = case n of
    CmmAssign lhs _ -> fold f z lhs
    CmmUnsafeForeignCall tgt _ _  -> fold f z (foreignTargetRegs tgt)
    CmmCall        {} -> fold f z activeRegs
    CmmForeignCall {} -> fold f z activeRegs
                      -- See Note [Safe foreign calls clobber STG registers]
    _ -> z
    where fold :: forall a b. DefinerOfRegs GlobalReg a
               => (b -> GlobalReg -> b) -> b -> a -> b
          fold f z n = foldRegsDefd dflags f z n

          platform = targetPlatform dflags
          activeRegs = activeStgRegs platform
          activeCallerSavesRegs = filter (callerSaves platform) activeRegs

          foreignTargetRegs (ForeignTarget _ (ForeignConvention _ _ _ CmmNeverReturns)) = []
          foreignTargetRegs _ = activeCallerSavesRegs

-- Note [Safe foreign calls clobber STG registers]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- During stack layout phase every safe foreign call is expanded into a block
-- that contains unsafe foreign call (instead of safe foreign call) and ends
-- with a normal call (See Note [Foreign calls]). This means that we must
-- treat safe foreign call as if it was a normal call (because eventually it
-- will be). This is important if we try to run sinking pass before stack
-- layout phase. Consider this example of what might go wrong (this is cmm
-- code from stablename001 test). Here is code after common block elimination
-- (before stack layout):
--
--  c1q6:
--      _s1pf::P64 = R1;
--      _c1q8::I64 = performMajorGC;
--      I64[(young<c1q9> + 8)] = c1q9;
--      foreign call "ccall" arg hints:  []  result hints:  [] (_c1q8::I64)(...)
--                   returns to c1q9 args: ([]) ress: ([])ret_args: 8ret_off: 8;
--  c1q9:
--      I64[(young<c1qb> + 8)] = c1qb;
--      R1 = _s1pc::P64;
--      call stg_makeStableName#(R1) returns to c1qb, args: 8, res: 8, upd: 8;
--
-- If we run sinking pass now (still before stack layout) we will get this:
--
--  c1q6:
--      I64[(young<c1q9> + 8)] = c1q9;
--      foreign call "ccall" arg hints:  []  result hints:  [] performMajorGC(...)
--                   returns to c1q9 args: ([]) ress: ([])ret_args: 8ret_off: 8;
--  c1q9:
--      I64[(young<c1qb> + 8)] = c1qb;
--      _s1pf::P64 = R1;         <------ _s1pf sunk past safe foreign call
--      R1 = _s1pc::P64;
--      call stg_makeStableName#(R1) returns to c1qb, args: 8, res: 8, upd: 8;
--
-- Notice that _s1pf was sunk past a foreign call. When we run stack layout
-- safe call to performMajorGC will be turned into:
--
--  c1q6:
--      _s1pc::P64 = P64[Sp + 8];
--      I64[Sp - 8] = c1q9;
--      Sp = Sp - 8;
--      I64[I64[CurrentTSO + 24] + 16] = Sp;
--      P64[CurrentNursery + 8] = Hp + 8;
--      (_u1qI::I64) = call "ccall" arg hints:  [PtrHint,]
--                           result hints:  [PtrHint] suspendThread(BaseReg, 0);
--      call "ccall" arg hints:  []  result hints:  [] performMajorGC();
--      (_u1qJ::I64) = call "ccall" arg hints:  [PtrHint]
--                           result hints:  [PtrHint] resumeThread(_u1qI::I64);
--      BaseReg = _u1qJ::I64;
--      _u1qK::P64 = CurrentTSO;
--      _u1qL::P64 = I64[_u1qK::P64 + 24];
--      Sp = I64[_u1qL::P64 + 16];
--      SpLim = _u1qL::P64 + 192;
--      HpAlloc = 0;
--      Hp = I64[CurrentNursery + 8] - 8;
--      HpLim = I64[CurrentNursery] + (%MO_SS_Conv_W32_W64(I32[CurrentNursery + 48]) * 4096 - 1);
--      call (I64[Sp])() returns to c1q9, args: 8, res: 8, upd: 8;
--  c1q9:
--      I64[(young<c1qb> + 8)] = c1qb;
--      _s1pf::P64 = R1;         <------ INCORRECT!
--      R1 = _s1pc::P64;
--      call stg_makeStableName#(R1) returns to c1qb, args: 8, res: 8, upd: 8;
--
-- Notice that c1q6 now ends with a call. Sinking _s1pf::P64 = R1 past that
-- call is clearly incorrect. This is what would happen if we assumed that
-- safe foreign call has the same semantics as unsafe foreign call. To prevent
-- this we need to treat safe foreign call as if was normal call.

-----------------------------------
-- mapping Expr in GHC.Cmm.Node

mapForeignTarget :: (CmmExpr -> CmmExpr) -> ForeignTarget -> ForeignTarget
mapForeignTarget exp   (ForeignTarget e c) = ForeignTarget (exp e) c
mapForeignTarget _   m@(PrimTarget _)      = m

wrapRecExp :: (CmmExpr -> CmmExpr) -> CmmExpr -> CmmExpr
-- Take a transformer on expressions and apply it recursively.
-- (wrapRecExp f e) first recursively applies itself to sub-expressions of e
--                  then  uses f to rewrite the resulting expression
wrapRecExp f (CmmMachOp op es)    = f (CmmMachOp op $ map (wrapRecExp f) es)
wrapRecExp f (CmmLoad addr ty)    = f (CmmLoad (wrapRecExp f addr) ty)
wrapRecExp f e                    = f e

mapExp :: (CmmExpr -> CmmExpr) -> CmmNode e x -> CmmNode e x
mapExp _ f@(CmmEntry{})                          = f
mapExp _ m@(CmmComment _)                        = m
mapExp _ m@(CmmTick _)                           = m
mapExp f   (CmmUnwind regs)                      = CmmUnwind (map (fmap (fmap f)) regs)
mapExp f   (CmmAssign r e)                       = CmmAssign r (f e)
mapExp f   (CmmStore addr e)                     = CmmStore (f addr) (f e)
mapExp f   (CmmUnsafeForeignCall tgt fs as)      = CmmUnsafeForeignCall (mapForeignTarget f tgt) fs (map f as)
mapExp _ l@(CmmBranch _)                         = l
mapExp f   (CmmCondBranch e ti fi l)             = CmmCondBranch (f e) ti fi l
mapExp f   (CmmSwitch e ids)                     = CmmSwitch (f e) ids
mapExp f   n@CmmCall {cml_target=tgt}            = n{cml_target = f tgt}
mapExp f   (CmmForeignCall tgt fs as succ ret_args updfr intrbl) = CmmForeignCall (mapForeignTarget f tgt) fs (map f as) succ ret_args updfr intrbl

mapExpDeep :: (CmmExpr -> CmmExpr) -> CmmNode e x -> CmmNode e x
mapExpDeep f = mapExp $ wrapRecExp f

------------------------------------------------------------------------
-- mapping Expr in GHC.Cmm.Node, but not performing allocation if no changes

mapForeignTargetM :: (CmmExpr -> Maybe CmmExpr) -> ForeignTarget -> Maybe ForeignTarget
mapForeignTargetM f (ForeignTarget e c) = (\x -> ForeignTarget x c) `fmap` f e
mapForeignTargetM _ (PrimTarget _)      = Nothing

wrapRecExpM :: (CmmExpr -> Maybe CmmExpr) -> (CmmExpr -> Maybe CmmExpr)
-- (wrapRecExpM f e) first recursively applies itself to sub-expressions of e
--                   then  gives f a chance to rewrite the resulting expression
wrapRecExpM f n@(CmmMachOp op es)  = maybe (f n) (f . CmmMachOp op)    (mapListM (wrapRecExpM f) es)
wrapRecExpM f n@(CmmLoad addr ty)  = maybe (f n) (f . flip CmmLoad ty) (wrapRecExpM f addr)
wrapRecExpM f e                    = f e

mapExpM :: (CmmExpr -> Maybe CmmExpr) -> CmmNode e x -> Maybe (CmmNode e x)
mapExpM _ (CmmEntry{})              = Nothing
mapExpM _ (CmmComment _)            = Nothing
mapExpM _ (CmmTick _)               = Nothing
mapExpM f (CmmUnwind regs)          = CmmUnwind `fmap` mapM (\(r,e) -> mapM f e >>= \e' -> pure (r,e')) regs
mapExpM f (CmmAssign r e)           = CmmAssign r `fmap` f e
mapExpM f (CmmStore addr e)         = (\[addr', e'] -> CmmStore addr' e') `fmap` mapListM f [addr, e]
mapExpM _ (CmmBranch _)             = Nothing
mapExpM f (CmmCondBranch e ti fi l) = (\x -> CmmCondBranch x ti fi l) `fmap` f e
mapExpM f (CmmSwitch e tbl)         = (\x -> CmmSwitch x tbl)       `fmap` f e
mapExpM f (CmmCall tgt mb_id r o i s) = (\x -> CmmCall x mb_id r o i s) `fmap` f tgt
mapExpM f (CmmUnsafeForeignCall tgt fs as)
    = case mapForeignTargetM f tgt of
        Just tgt' -> Just (CmmUnsafeForeignCall tgt' fs (mapListJ f as))
        Nothing   -> (\xs -> CmmUnsafeForeignCall tgt fs xs) `fmap` mapListM f as
mapExpM f (CmmForeignCall tgt fs as succ ret_args updfr intrbl)
    = case mapForeignTargetM f tgt of
        Just tgt' -> Just (CmmForeignCall tgt' fs (mapListJ f as) succ ret_args updfr intrbl)
        Nothing   -> (\xs -> CmmForeignCall tgt fs xs succ ret_args updfr intrbl) `fmap` mapListM f as

-- share as much as possible
mapListM :: (a -> Maybe a) -> [a] -> Maybe [a]
mapListM f xs = let (b, r) = mapListT f xs
                in if b then Just r else Nothing

mapListJ :: (a -> Maybe a) -> [a] -> [a]
mapListJ f xs = snd (mapListT f xs)

mapListT :: (a -> Maybe a) -> [a] -> (Bool, [a])
mapListT f xs = foldr g (False, []) (zip3 (tails xs) xs (map f xs))
    where g (_,   y, Nothing) (True, ys)  = (True,  y:ys)
          g (_,   _, Just y)  (True, ys)  = (True,  y:ys)
          g (ys', _, Nothing) (False, _)  = (False, ys')
          g (_,   _, Just y)  (False, ys) = (True,  y:ys)

mapExpDeepM :: (CmmExpr -> Maybe CmmExpr) -> CmmNode e x -> Maybe (CmmNode e x)
mapExpDeepM f = mapExpM $ wrapRecExpM f

-----------------------------------
-- folding Expr in GHC.Cmm.Node

foldExpForeignTarget :: (CmmExpr -> z -> z) -> ForeignTarget -> z -> z
foldExpForeignTarget exp (ForeignTarget e _) z = exp e z
foldExpForeignTarget _   (PrimTarget _)      z = z

-- Take a folder on expressions and apply it recursively.
-- Specifically (wrapRecExpf f e z) deals with CmmMachOp and CmmLoad
-- itself, delegating all the other CmmExpr forms to 'f'.
wrapRecExpf :: (CmmExpr -> z -> z) -> CmmExpr -> z -> z
wrapRecExpf f e@(CmmMachOp _ es) z = foldr (wrapRecExpf f) (f e z) es
wrapRecExpf f e@(CmmLoad addr _) z = wrapRecExpf f addr (f e z)
wrapRecExpf f e                  z = f e z

foldExp :: (CmmExpr -> z -> z) -> CmmNode e x -> z -> z
foldExp _ (CmmEntry {}) z                         = z
foldExp _ (CmmComment {}) z                       = z
foldExp _ (CmmTick {}) z                          = z
foldExp f (CmmUnwind xs) z                        = foldr (maybe id f) z (map snd xs)
foldExp f (CmmAssign _ e) z                       = f e z
foldExp f (CmmStore addr e) z                     = f addr $ f e z
foldExp f (CmmUnsafeForeignCall t _ as) z         = foldr f (foldExpForeignTarget f t z) as
foldExp _ (CmmBranch _) z                         = z
foldExp f (CmmCondBranch e _ _ _) z               = f e z
foldExp f (CmmSwitch e _) z                       = f e z
foldExp f (CmmCall {cml_target=tgt}) z            = f tgt z
foldExp f (CmmForeignCall {tgt=tgt, args=args}) z = foldr f (foldExpForeignTarget f tgt z) args

foldExpDeep :: (CmmExpr -> z -> z) -> CmmNode e x -> z -> z
foldExpDeep f = foldExp (wrapRecExpf f)

-- -----------------------------------------------------------------------------

mapSuccessors :: (Label -> Label) -> CmmNode O C -> CmmNode O C
mapSuccessors f (CmmBranch bid)         = CmmBranch (f bid)
mapSuccessors f (CmmCondBranch p y n l) = CmmCondBranch p (f y) (f n) l
mapSuccessors f (CmmSwitch e ids)       = CmmSwitch e (mapSwitchTargets f ids)
mapSuccessors _ n = n

mapCollectSuccessors :: forall a. (Label -> (Label,a)) -> CmmNode O C
                     -> (CmmNode O C, [a])
mapCollectSuccessors f (CmmBranch bid)
  = let (bid', acc) = f bid in (CmmBranch bid', [acc])
mapCollectSuccessors f (CmmCondBranch p y n l)
  = let (bidt, acct) = f y
        (bidf, accf) = f n
    in  (CmmCondBranch p bidt bidf l, [accf, acct])
mapCollectSuccessors f (CmmSwitch e ids)
  = let lbls = switchTargetsToList ids :: [Label]
        lblMap = mapFromList $ zip lbls (map f lbls) :: LabelMap (Label, a)
    in ( CmmSwitch e
          (mapSwitchTargets
            (\l -> fst $ mapFindWithDefault (error "impossible") l lblMap) ids)
          , map snd (mapElems lblMap)
        )
mapCollectSuccessors _ n = (n, [])

-- -----------------------------------------------------------------------------

-- | Tickish in Cmm context (annotations only)
type CmmTickish = Tickish ()

-- | Tick scope identifier, allowing us to reason about what
-- annotations in a Cmm block should scope over. We especially take
-- care to allow optimisations to reorganise blocks without losing
-- tick association in the process.
data CmmTickScope
  = GlobalScope
    -- ^ The global scope is the "root" of the scope graph. Every
    -- scope is a sub-scope of the global scope. It doesn't make sense
    -- to add ticks to this scope. On the other hand, this means that
    -- setting this scope on a block means no ticks apply to it.

  | SubScope !U.Unique CmmTickScope
    -- ^ Constructs a new sub-scope to an existing scope. This allows
    -- us to translate Core-style scoping rules (see @tickishScoped@)
    -- into the Cmm world. Suppose the following code:
    --
    --   tick<1> case ... of
    --             A -> tick<2> ...
    --             B -> tick<3> ...
    --
    -- We want the top-level tick annotation to apply to blocks
    -- generated for the A and B alternatives. We can achieve that by
    -- generating tick<1> into a block with scope a, while the code
    -- for alternatives A and B gets generated into sub-scopes a/b and
    -- a/c respectively.

  | CombinedScope CmmTickScope CmmTickScope
    -- ^ A combined scope scopes over everything that the two given
    -- scopes cover. It is therefore a sub-scope of either scope. This
    -- is required for optimisations. Consider common block elimination:
    --
    --   A -> tick<2> case ... of
    --     C -> [common]
    --   B -> tick<3> case ... of
    --     D -> [common]
    --
    -- We will generate code for the C and D alternatives, and figure
    -- out afterwards that it's actually common code. Scoping rules
    -- dictate that the resulting common block needs to be covered by
    -- both tick<2> and tick<3>, therefore we need to construct a
    -- scope that is a child to *both* scope. Now we can do that - if
    -- we assign the scopes a/c and b/d to the common-ed up blocks,
    -- the new block could have a combined tick scope a/c+b/d, which
    -- both tick<2> and tick<3> apply to.

-- Note [CmmTick scoping details]:
--
-- The scope of a @CmmTick@ is given by the @CmmEntry@ node of the
-- same block. Note that as a result of this, optimisations making
-- tick scopes more specific can *reduce* the amount of code a tick
-- scopes over. Fixing this would require a separate @CmmTickScope@
-- field for @CmmTick@. Right now we do not do this simply because I
-- couldn't find an example where it actually mattered -- multiple
-- blocks within the same scope generally jump to each other, which
-- prevents common block elimination from happening in the first
-- place. But this is no strong reason, so if Cmm optimisations become
-- more involved in future this might have to be revisited.

-- | Output all scope paths.
scopeToPaths :: CmmTickScope -> [[U.Unique]]
scopeToPaths GlobalScope           = [[]]
scopeToPaths (SubScope u s)        = map (u:) (scopeToPaths s)
scopeToPaths (CombinedScope s1 s2) = scopeToPaths s1 ++ scopeToPaths s2

-- | Returns the head uniques of the scopes. This is based on the
-- assumption that the @Unique@ of @SubScope@ identifies the
-- underlying super-scope. Used for efficient equality and comparison,
-- see below.
scopeUniques :: CmmTickScope -> [U.Unique]
scopeUniques GlobalScope           = []
scopeUniques (SubScope u _)        = [u]
scopeUniques (CombinedScope s1 s2) = scopeUniques s1 ++ scopeUniques s2

-- Equality and order is based on the head uniques defined above. We
-- take care to short-cut the (extremely) common cases.
instance Eq CmmTickScope where
  GlobalScope    == GlobalScope     = True
  GlobalScope    == _               = False
  _              == GlobalScope     = False
  (SubScope u _) == (SubScope u' _) = u == u'
  (SubScope _ _) == _               = False
  _              == (SubScope _ _)  = False
  scope          == scope'          =
    sortBy nonDetCmpUnique (scopeUniques scope) ==
    sortBy nonDetCmpUnique (scopeUniques scope')
    -- This is still deterministic because
    -- the order is the same for equal lists

-- This is non-deterministic but we do not currently support deterministic
-- code-generation. See Note [Unique Determinism and code generation]
-- See Note [No Ord for Unique]
instance Ord CmmTickScope where
  compare GlobalScope    GlobalScope     = EQ
  compare GlobalScope    _               = LT
  compare _              GlobalScope     = GT
  compare (SubScope u _) (SubScope u' _) = nonDetCmpUnique u u'
  compare scope scope'                   = cmpList nonDetCmpUnique
     (sortBy nonDetCmpUnique $ scopeUniques scope)
     (sortBy nonDetCmpUnique $ scopeUniques scope')

instance Outputable CmmTickScope where
  ppr GlobalScope     = text "global"
  ppr (SubScope us GlobalScope)
                      = ppr us
  ppr (SubScope us s) = ppr s <> char '/' <> ppr us
  ppr combined        = parens $ hcat $ punctuate (char '+') $
                        map (hcat . punctuate (char '/') . map ppr . reverse) $
                        scopeToPaths combined

-- | Checks whether two tick scopes are sub-scopes of each other. True
-- if the two scopes are equal.
isTickSubScope :: CmmTickScope -> CmmTickScope -> Bool
isTickSubScope = cmp
  where cmp _              GlobalScope             = True
        cmp GlobalScope    _                       = False
        cmp (CombinedScope s1 s2) s'               = cmp s1 s' && cmp s2 s'
        cmp s              (CombinedScope s1' s2') = cmp s s1' || cmp s s2'
        cmp (SubScope u s) s'@(SubScope u' _)      = u == u' || cmp s s'

-- | Combine two tick scopes. The new scope should be sub-scope of
-- both parameters. We simplify automatically if one tick scope is a
-- sub-scope of the other already.
combineTickScopes :: CmmTickScope -> CmmTickScope -> CmmTickScope
combineTickScopes s1 s2
  | s1 `isTickSubScope` s2 = s1
  | s2 `isTickSubScope` s1 = s2
  | otherwise              = CombinedScope s1 s2
