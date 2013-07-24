-- CmmNode type for representation using Hoopl graphs.
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module CmmNode (
     CmmNode(..), CmmFormal, CmmActual,
     UpdFrameOffset, Convention(..),
     ForeignConvention(..), ForeignTarget(..), foreignTargetHints,
     CmmReturnInfo(..),
     mapExp, mapExpDeep, wrapRecExp, foldExp, foldExpDeep, wrapRecExpf,
     mapExpM, mapExpDeepM, wrapRecExpM, mapSuccessors
  ) where

import CodeGen.Platform
import CmmExpr
import DynFlags
import FastString
import ForeignCall
import SMRep

import Compiler.Hoopl
import Data.Maybe
import Data.List (tails)
import Prelude hiding (succ)


------------------------
-- CmmNode

#define ULabel {-# UNPACK #-} !Label

data CmmNode e x where
  CmmEntry :: ULabel -> CmmNode C O

  CmmComment :: FastString -> CmmNode O O

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
      -- See Note [foreign calls clobber GlobalRegs]
      --
      -- Invariant: the arguments and the ForeignTarget must not
      -- mention any registers for which CodeGen.Platform.callerSaves
      -- is True.  See Note [Register Parameter Passing].

  CmmBranch :: ULabel -> CmmNode O C
                                   -- Goto another block in the same procedure

  CmmCondBranch :: {                 -- conditional branch
      cml_pred :: CmmExpr,
      cml_true, cml_false :: ULabel
  } -> CmmNode O C

  CmmSwitch :: CmmExpr -> [Maybe Label] -> CmmNode O C -- Table branch
      -- The scrutinee is zero-based;
      --      zero -> first block
      --      one  -> second block etc
      -- Undefined outside range, and when there's a Nothing

  CmmCall :: {                -- A native call or tail call
      cml_target :: CmmExpr,  -- never a CmmPrim to a CallishMachOp!

      cml_cont :: Maybe Label,
          -- Label of continuation (Nothing for return or tail call)
          --
          -- Note [Continuation BlockId]: these BlockIds are called
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
made manifest in CmmLayoutStack, where they are lowered into the above
sequence.
-}

{- Note [foreign calls clobber GlobalRegs]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

A foreign call is defined to clobber any GlobalRegs that are mapped to
caller-saves machine registers (according to the prevailing C ABI).
StgCmmUtils.callerSaves tells you which GlobalRegs are caller-saves.

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
code generation in callerSaveVolatileRegs in StgCmmUtils.hs.  However,
one result of doing this is that the contents of these registers
may mysteriously change if referenced inside the arguments.  This
is dangerous, so you'll need to disable inlining much in the same
way is done in cmm/CmmOpt.hs currently.  We should fix this!
-}

---------------------------------------------
-- Eq instance of CmmNode

deriving instance Eq (CmmNode e x)

----------------------------------------------
-- Hoopl instances of CmmNode

instance NonLocal CmmNode where
  entryLabel (CmmEntry l) = l

  successors (CmmBranch l) = [l]
  successors (CmmCondBranch {cml_true=t, cml_false=f}) = [f, t] -- meets layout constraint
  successors (CmmSwitch _ ls) = catMaybes ls
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
  foldRegsUsed dflags f z n = case n of
    CmmAssign _ expr -> fold f z expr
    CmmStore addr rval -> fold f (fold f z addr) rval
    CmmUnsafeForeignCall t _ args -> fold f (fold f z t) args
    CmmCondBranch expr _ _ -> fold f z expr
    CmmSwitch expr _ -> fold f z expr
    CmmCall {cml_target=tgt} -> fold f z tgt
    CmmForeignCall {tgt=tgt, args=args} -> fold f (fold f z tgt) args
    _ -> z
    where fold :: forall a b.
                       UserOfRegs LocalReg a =>
                       (b -> LocalReg -> b) -> b -> a -> b
          fold f z n = foldRegsUsed dflags f z n

instance UserOfRegs GlobalReg (CmmNode e x) where
  foldRegsUsed dflags f z n = case n of
    CmmAssign _ expr -> fold f z expr
    CmmStore addr rval -> fold f (fold f z addr) rval
    CmmUnsafeForeignCall t _ args -> fold f (fold f z t) args
    CmmCondBranch expr _ _ -> fold f z expr
    CmmSwitch expr _ -> fold f z expr
    CmmCall {cml_target=tgt, cml_args_regs=args} -> fold f (fold f z args) tgt
    CmmForeignCall {tgt=tgt, args=args} -> fold f (fold f z tgt) args
    _ -> z
    where fold :: forall a b.
                       UserOfRegs GlobalReg a =>
                       (b -> GlobalReg -> b) -> b -> a -> b
          fold f z n = foldRegsUsed dflags f z n

instance UserOfRegs r CmmExpr => UserOfRegs r ForeignTarget where
  foldRegsUsed _      _ z (PrimTarget _)      = z
  foldRegsUsed dflags f z (ForeignTarget e _) = foldRegsUsed dflags f z e

instance DefinerOfRegs LocalReg (CmmNode e x) where
  foldRegsDefd dflags f z n = case n of
    CmmAssign lhs _ -> fold f z lhs
    CmmUnsafeForeignCall _ fs _ -> fold f z fs
    CmmForeignCall {res=res} -> fold f z res
    _ -> z
    where fold :: forall a b.
                   DefinerOfRegs LocalReg a =>
                   (b -> LocalReg -> b) -> b -> a -> b
          fold f z n = foldRegsDefd dflags f z n

instance DefinerOfRegs GlobalReg (CmmNode e x) where
  foldRegsDefd dflags f z n = case n of
    CmmAssign lhs _ -> fold f z lhs
    CmmUnsafeForeignCall tgt _ _  -> fold f z (foreignTargetRegs tgt)
    CmmCall {} -> fold f z activeRegs
    CmmForeignCall {tgt=tgt} -> fold f z (foreignTargetRegs tgt)
    _ -> z
    where fold :: forall a b.
                   DefinerOfRegs GlobalReg a =>
                   (b -> GlobalReg -> b) -> b -> a -> b
          fold f z n = foldRegsDefd dflags f z n

          platform = targetPlatform dflags
          activeRegs = activeStgRegs platform
          activeCallerSavesRegs = filter (callerSaves platform) activeRegs

          foreignTargetRegs (ForeignTarget _ (ForeignConvention _ _ _ CmmNeverReturns)) = []
          foreignTargetRegs _ = activeCallerSavesRegs


-----------------------------------
-- mapping Expr in CmmNode

mapForeignTarget :: (CmmExpr -> CmmExpr) -> ForeignTarget -> ForeignTarget
mapForeignTarget exp   (ForeignTarget e c) = ForeignTarget (exp e) c
mapForeignTarget _   m@(PrimTarget _)      = m

-- Take a transformer on expressions and apply it recursively.
wrapRecExp :: (CmmExpr -> CmmExpr) -> CmmExpr -> CmmExpr
wrapRecExp f (CmmMachOp op es)    = f (CmmMachOp op $ map (wrapRecExp f) es)
wrapRecExp f (CmmLoad addr ty)    = f (CmmLoad (wrapRecExp f addr) ty)
wrapRecExp f e                    = f e

mapExp :: (CmmExpr -> CmmExpr) -> CmmNode e x -> CmmNode e x
mapExp _ f@(CmmEntry _)                          = f
mapExp _ m@(CmmComment _)                        = m
mapExp f   (CmmAssign r e)                       = CmmAssign r (f e)
mapExp f   (CmmStore addr e)                     = CmmStore (f addr) (f e)
mapExp f   (CmmUnsafeForeignCall tgt fs as)      = CmmUnsafeForeignCall (mapForeignTarget f tgt) fs (map f as)
mapExp _ l@(CmmBranch _)                         = l
mapExp f   (CmmCondBranch e ti fi)               = CmmCondBranch (f e) ti fi
mapExp f   (CmmSwitch e tbl)                     = CmmSwitch (f e) tbl
mapExp f   n@CmmCall {cml_target=tgt}            = n{cml_target = f tgt}
mapExp f   (CmmForeignCall tgt fs as succ ret_args updfr intrbl) = CmmForeignCall (mapForeignTarget f tgt) fs (map f as) succ ret_args updfr intrbl

mapExpDeep :: (CmmExpr -> CmmExpr) -> CmmNode e x -> CmmNode e x
mapExpDeep f = mapExp $ wrapRecExp f

------------------------------------------------------------------------
-- mapping Expr in CmmNode, but not performing allocation if no changes

mapForeignTargetM :: (CmmExpr -> Maybe CmmExpr) -> ForeignTarget -> Maybe ForeignTarget
mapForeignTargetM f (ForeignTarget e c) = (\x -> ForeignTarget x c) `fmap` f e
mapForeignTargetM _ (PrimTarget _)      = Nothing

wrapRecExpM :: (CmmExpr -> Maybe CmmExpr) -> (CmmExpr -> Maybe CmmExpr)
wrapRecExpM f n@(CmmMachOp op es)  = maybe (f n) (f . CmmMachOp op)    (mapListM (wrapRecExpM f) es)
wrapRecExpM f n@(CmmLoad addr ty)  = maybe (f n) (f . flip CmmLoad ty) (wrapRecExpM f addr)
wrapRecExpM f e                    = f e

mapExpM :: (CmmExpr -> Maybe CmmExpr) -> CmmNode e x -> Maybe (CmmNode e x)
mapExpM _ (CmmEntry _)              = Nothing
mapExpM _ (CmmComment _)            = Nothing
mapExpM f (CmmAssign r e)           = CmmAssign r `fmap` f e
mapExpM f (CmmStore addr e)         = (\[addr', e'] -> CmmStore addr' e') `fmap` mapListM f [addr, e]
mapExpM _ (CmmBranch _)             = Nothing
mapExpM f (CmmCondBranch e ti fi)   = (\x -> CmmCondBranch x ti fi) `fmap` f e
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
-- folding Expr in CmmNode

foldExpForeignTarget :: (CmmExpr -> z -> z) -> ForeignTarget -> z -> z
foldExpForeignTarget exp (ForeignTarget e _) z = exp e z
foldExpForeignTarget _   (PrimTarget _)      z = z

-- Take a folder on expressions and apply it recursively.
wrapRecExpf :: (CmmExpr -> z -> z) -> CmmExpr -> z -> z
wrapRecExpf f e@(CmmMachOp _ es) z = foldr (wrapRecExpf f) (f e z) es
wrapRecExpf f e@(CmmLoad addr _) z = wrapRecExpf f addr (f e z)
wrapRecExpf f e                  z = f e z

foldExp :: (CmmExpr -> z -> z) -> CmmNode e x -> z -> z
foldExp _ (CmmEntry {}) z                         = z
foldExp _ (CmmComment {}) z                       = z
foldExp f (CmmAssign _ e) z                       = f e z
foldExp f (CmmStore addr e) z                     = f addr $ f e z
foldExp f (CmmUnsafeForeignCall t _ as) z         = foldr f (foldExpForeignTarget f t z) as
foldExp _ (CmmBranch _) z                         = z
foldExp f (CmmCondBranch e _ _) z                 = f e z
foldExp f (CmmSwitch e _) z                       = f e z
foldExp f (CmmCall {cml_target=tgt}) z            = f tgt z
foldExp f (CmmForeignCall {tgt=tgt, args=args}) z = foldr f (foldExpForeignTarget f tgt z) args

foldExpDeep :: (CmmExpr -> z -> z) -> CmmNode e x -> z -> z
foldExpDeep f = foldExp (wrapRecExpf f)

-- -----------------------------------------------------------------------------

mapSuccessors :: (Label -> Label) -> CmmNode O C -> CmmNode O C
mapSuccessors f (CmmBranch bid)        = CmmBranch (f bid)
mapSuccessors f (CmmCondBranch p y n)  = CmmCondBranch p (f y) (f n)
mapSuccessors f (CmmSwitch e arms)     = CmmSwitch e (map (fmap f) arms)
mapSuccessors _ n = n

