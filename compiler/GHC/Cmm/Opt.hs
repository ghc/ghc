-----------------------------------------------------------------------------
--
-- Cmm optimisation
--
-- (c) The University of Glasgow 2006
--
-----------------------------------------------------------------------------
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PatternSynonyms #-}
module GHC.Cmm.Opt (
        constantFoldNode,
        constantFoldExpr,
        cmmMachOpFold,
        cmmMachOpFoldM,
        Opt, runOpt
 ) where

import GHC.Prelude

import GHC.Cmm.Dataflow.Block
import GHC.Cmm.Utils
import GHC.Cmm
import GHC.Cmm.Config

import GHC.Types.Unique.Supply

import GHC.Utils.Misc
import GHC.Utils.Panic
import GHC.Platform

import Data.Maybe
import Data.Word
import GHC.Exts (oneShot)
import Control.Monad

constantFoldNode :: CmmNode e x -> Opt (CmmNode e x)
constantFoldNode (CmmUnsafeForeignCall (PrimTarget op) res args)
  = traverse constantFoldExprOpt args >>= cmmCallishMachOpFold op res
constantFoldNode node
  = mapExpOpt constantFoldExprOpt node

constantFoldExprOpt :: CmmExpr -> Opt CmmExpr
constantFoldExprOpt e = wrapRecExpOpt f e
  where
    f (CmmMachOp op args)
      = do
        cfg <- getConfig
        case cmmMachOpFold (cmmPlatform cfg) op args of
          CmmMachOp op' args' -> fromMaybe (CmmMachOp op' args') <$> cmmMachOpFoldOptM cfg op' args'
          e -> pure e
    f (CmmRegOff r 0) = pure (CmmReg r)
    f e = pure e

constantFoldExpr :: Platform -> CmmExpr -> CmmExpr
constantFoldExpr platform = wrapRecExp f
  where f (CmmMachOp op args) = cmmMachOpFold platform op args
        f (CmmRegOff r 0) = CmmReg r
        f e = e

-- -----------------------------------------------------------------------------
-- MachOp constant folder

-- Now, try to constant-fold the MachOps.  The arguments have already
-- been optimized and folded.

cmmMachOpFold
    :: Platform
    -> MachOp       -- The operation from an CmmMachOp
    -> [CmmExpr]    -- The optimized arguments
    -> CmmExpr

cmmMachOpFold platform op args = fromMaybe (CmmMachOp op args) (cmmMachOpFoldM platform op args)

-- Returns Nothing if no changes, useful for Hoopl, also reduces
-- allocation!
cmmMachOpFoldM
    :: Platform
    -> MachOp
    -> [CmmExpr]
    -> Maybe CmmExpr

cmmMachOpFoldM _ op [CmmLit (CmmInt x rep)]
  = Just $! case op of
      MO_S_Neg _ -> CmmLit (CmmInt (-x) rep)
      MO_Not _   -> CmmLit (CmmInt (complement x) rep)

        -- these are interesting: we must first narrow to the
        -- "from" type, in order to truncate to the correct size.
        -- The final narrow/widen to the destination type
        -- is implicit in the CmmLit.
      MO_SF_Round _frm to -> CmmLit (CmmFloat (fromInteger x) to)
      MO_SS_Conv  from to -> CmmLit (CmmInt (narrowS from x) to)
      MO_UU_Conv  from to -> CmmLit (CmmInt (narrowU from x) to)
      MO_XX_Conv  from to -> CmmLit (CmmInt (narrowS from x) to)

      _ -> panic $ "cmmMachOpFoldM: unknown unary op: " ++ show op

-- Eliminate shifts that are wider than the shiftee
cmmMachOpFoldM _ op [_shiftee, CmmLit (CmmInt shift _)]
  | Just width <- isShift op
  , shift >= fromIntegral (widthInBits width)
  = Just $! CmmLit (CmmInt 0 width)
  where
    isShift (MO_Shl   w) = Just w
    isShift (MO_U_Shr w) = Just w
    isShift (MO_S_Shr w) = Just w
    isShift _            = Nothing

-- Eliminate conversion NOPs
cmmMachOpFoldM _ (MO_SS_Conv rep1 rep2) [x] | rep1 == rep2 = Just x
cmmMachOpFoldM _ (MO_UU_Conv rep1 rep2) [x] | rep1 == rep2 = Just x
cmmMachOpFoldM _ (MO_XX_Conv rep1 rep2) [x] | rep1 == rep2 = Just x

-- Eliminate nested conversions where possible
cmmMachOpFoldM platform conv_outer [CmmMachOp conv_inner [x]]
  | Just (rep1,rep2,signed1) <- isIntConversion conv_inner,
    Just (_,   rep3,signed2) <- isIntConversion conv_outer
  = case () of
        -- widen then narrow to the same size is a nop
      _ | rep1 < rep2 && rep1 == rep3 -> Just x
        -- Widen then narrow to different size: collapse to single conversion
        -- but remember to use the signedness from the widening, just in case
        -- the final conversion is a widen.
        | rep1 < rep2 && rep2 > rep3 ->
            Just $! cmmMachOpFold platform (intconv signed1 rep1 rep3) [x]
        -- Nested widenings: collapse if the signedness is the same
        | rep1 < rep2 && rep2 < rep3 && signed1 == signed2 ->
            Just $! cmmMachOpFold platform (intconv signed1 rep1 rep3) [x]
        -- Nested narrowings: collapse
        | rep1 > rep2 && rep2 > rep3 ->
            Just $! cmmMachOpFold platform (MO_UU_Conv rep1 rep3) [x]
        | otherwise ->
            Nothing
  where
        isIntConversion (MO_UU_Conv rep1 rep2)
          = Just (rep1,rep2,False)
        isIntConversion (MO_SS_Conv rep1 rep2)
          = Just (rep1,rep2,True)
        isIntConversion _ = Nothing

        intconv True  = MO_SS_Conv
        intconv False = MO_UU_Conv

cmmMachOpFoldM platform mop [CmmLit (CmmInt x xrep), CmmLit (CmmInt y _)]
  = case mop of
        -- for comparisons: don't forget to narrow the arguments before
        -- comparing, since they might be out of range.
        MO_Eq _   -> Just $! CmmLit (CmmInt (if x_u == y_u then 1 else 0) (wordWidth platform))
        MO_Ne _   -> Just $! CmmLit (CmmInt (if x_u /= y_u then 1 else 0) (wordWidth platform))

        MO_U_Gt _ -> Just $! CmmLit (CmmInt (if x_u >  y_u then 1 else 0) (wordWidth platform))
        MO_U_Ge _ -> Just $! CmmLit (CmmInt (if x_u >= y_u then 1 else 0) (wordWidth platform))
        MO_U_Lt _ -> Just $! CmmLit (CmmInt (if x_u <  y_u then 1 else 0) (wordWidth platform))
        MO_U_Le _ -> Just $! CmmLit (CmmInt (if x_u <= y_u then 1 else 0) (wordWidth platform))

        MO_S_Gt _ -> Just $! CmmLit (CmmInt (if x_s >  y_s then 1 else 0) (wordWidth platform))
        MO_S_Ge _ -> Just $! CmmLit (CmmInt (if x_s >= y_s then 1 else 0) (wordWidth platform))
        MO_S_Lt _ -> Just $! CmmLit (CmmInt (if x_s <  y_s then 1 else 0) (wordWidth platform))
        MO_S_Le _ -> Just $! CmmLit (CmmInt (if x_s <= y_s then 1 else 0) (wordWidth platform))

        MO_Add r -> Just $! CmmLit (CmmInt (x + y) r)
        MO_Sub r -> Just $! CmmLit (CmmInt (x - y) r)
        MO_Mul r -> Just $! CmmLit (CmmInt (x * y) r)
        MO_U_Quot r | y /= 0 -> Just $! CmmLit (CmmInt (x_u `quot` y_u) r)
        MO_U_Rem  r | y /= 0 -> Just $! CmmLit (CmmInt (x_u `rem`  y_u) r)
        MO_S_Quot r | y /= 0 -> Just $! CmmLit (CmmInt (x_s `quot` y_s) r)
        MO_S_Rem  r | y /= 0 -> Just $! CmmLit (CmmInt (x_s `rem`  y_s) r)

        MO_And   r -> Just $! CmmLit (CmmInt (x .&. y) r)
        MO_Or    r -> Just $! CmmLit (CmmInt (x .|. y) r)
        MO_Xor   r -> Just $! CmmLit (CmmInt (x `xor` y) r)

        MO_Shl   r -> Just $! CmmLit (CmmInt (x   `shiftL` fromIntegral y) r)
        MO_U_Shr r -> Just $! CmmLit (CmmInt (x_u `shiftR` fromIntegral y) r)
        MO_S_Shr r -> Just $! CmmLit (CmmInt (x_s `shiftR` fromIntegral y) r)

        _          -> Nothing

   where
        x_u = narrowU xrep x
        y_u = narrowU xrep y
        x_s = narrowS xrep x
        y_s = narrowS xrep y


-- When possible, shift the constants to the right-hand side, so that we
-- can match for strength reductions.  Note that the code generator will
-- also assume that constants have been shifted to the right when
-- possible.

cmmMachOpFoldM platform op [x@(CmmLit _), y]
   | not (isLit y) && isCommutableMachOp op
   = Just $! (cmmMachOpFold platform op [y, x])

-- Turn (a+b)+c into a+(b+c) where possible.  Because literals are
-- moved to the right, it is more likely that we will find
-- opportunities for constant folding when the expression is
-- right-associated.
--
-- ToDo: this appears to introduce a quadratic behaviour due to the
-- nested cmmMachOpFold.  Can we fix this?
--
-- Why do we check isLit arg1?  If arg1 is a lit, it means that arg2
-- is also a lit (otherwise arg1 would be on the right).  If we
-- put arg1 on the left of the rearranged expression, we'll get into a
-- loop:  (x1+x2)+x3 => x1+(x2+x3)  => (x2+x3)+x1 => x2+(x3+x1) ...
--
-- Also don't do it if arg1 is PicBaseReg, so that we don't separate the
-- PicBaseReg from the corresponding label (or label difference).
--
cmmMachOpFoldM platform mop1 [CmmMachOp mop2 [arg1,arg2], arg3]
   | mop2 `associates_with` mop1
     && not (isLit arg1) && not (isPicReg arg1)
   = Just $! (cmmMachOpFold platform mop2 [arg1, cmmMachOpFold platform mop1 [arg2,arg3]])
   where
     MO_Add{} `associates_with` MO_Sub{} = True
     mop1 `associates_with` mop2 =
        mop1 == mop2 && isAssociativeMachOp mop1

-- special case: (a - b) + c  ==>  a + (c - b)
cmmMachOpFoldM platform mop1@(MO_Add{}) [CmmMachOp mop2@(MO_Sub{}) [arg1,arg2], arg3]
   | not (isLit arg1) && not (isPicReg arg1)
   = Just $! (cmmMachOpFold platform mop1 [arg1, cmmMachOpFold platform mop2 [arg3,arg2]])

-- special case: (PicBaseReg + lit) + N  ==>  PicBaseReg + (lit+N)
--
-- this is better because lit+N is a single link-time constant (e.g. a
-- CmmLabelOff), so the right-hand expression needs only one
-- instruction, whereas the left needs two.  This happens when pointer
-- tagging gives us label+offset, and PIC turns the label into
-- PicBaseReg + label.
--
cmmMachOpFoldM _ MO_Add{} [ CmmMachOp op@MO_Add{} [pic, CmmLit lit]
                          , CmmLit (CmmInt n rep) ]
  | isPicReg pic
  = Just $! CmmMachOp op [pic, CmmLit $ cmmOffsetLit lit off ]
  where off = fromIntegral (narrowS rep n)

-- Make a RegOff if we can. We don't perform this optimization if rep is greater
-- than the host word size because we use an Int to store the offset. See
-- #24893 and #24700. This should be fixed to ensure that optimizations don't
-- depend on the compiler host platform.
cmmMachOpFoldM _ (MO_Add _) [CmmReg reg, CmmLit (CmmInt n rep)]
  | validOffsetRep rep
  = Just $! cmmRegOff reg (fromIntegral (narrowS rep n))
cmmMachOpFoldM _ (MO_Add _) [CmmRegOff reg off, CmmLit (CmmInt n rep)]
  | validOffsetRep rep
  = Just $! cmmRegOff reg (off + fromIntegral (narrowS rep n))
cmmMachOpFoldM _ (MO_Sub _) [CmmReg reg, CmmLit (CmmInt n rep)]
  | validOffsetRep rep
  = Just $! cmmRegOff reg (- fromIntegral (narrowS rep n))
cmmMachOpFoldM _ (MO_Sub _) [CmmRegOff reg off, CmmLit (CmmInt n rep)]
  | validOffsetRep rep
  = Just $! cmmRegOff reg (off - fromIntegral (narrowS rep n))

-- Fold label(+/-)offset into a CmmLit where possible

cmmMachOpFoldM _ (MO_Add _) [CmmLit lit, CmmLit (CmmInt i rep)]
  | validOffsetRep rep
  = Just $! CmmLit (cmmOffsetLit lit (fromIntegral (narrowU rep i)))
cmmMachOpFoldM _ (MO_Add _) [CmmLit (CmmInt i rep), CmmLit lit]
  | validOffsetRep rep
  = Just $! CmmLit (cmmOffsetLit lit (fromIntegral (narrowU rep i)))
cmmMachOpFoldM _ (MO_Sub _) [CmmLit lit, CmmLit (CmmInt i rep)]
  | validOffsetRep rep
  = Just $! CmmLit (cmmOffsetLit lit (fromIntegral (negate (narrowU rep i))))


-- Comparison of literal with widened operand: perform the comparison
-- at the smaller width, as long as the literal is within range.

-- We can't do the reverse trick, when the operand is narrowed:
-- narrowing throws away bits from the operand, there's no way to do
-- the same comparison at the larger size.

cmmMachOpFoldM platform cmp [CmmMachOp conv [x], CmmLit (CmmInt i _)]
  |     -- powerPC NCG has a TODO for I8/I16 comparisons, so don't try
    platformArch platform `elem` [ArchX86, ArchX86_64],
        -- if the operand is widened:
    Just (rep, signed, narrow_fn) <- maybe_conversion conv,
        -- and this is a comparison operation:
    Just narrow_cmp <- maybe_comparison cmp rep signed,
        -- and the literal fits in the smaller size:
    i == narrow_fn rep i
        -- then we can do the comparison at the smaller size
  = Just $! (cmmMachOpFold platform narrow_cmp [x, CmmLit (CmmInt i rep)])
 where
    maybe_conversion (MO_UU_Conv from to)
        | to > from
        = Just (from, False, narrowU)
    maybe_conversion (MO_SS_Conv from to)
        | to > from
        = Just (from, True, narrowS)

        -- don't attempt to apply this optimisation when the source
        -- is a float; see #1916
    maybe_conversion _ = Nothing

        -- careful (#2080): if the original comparison was signed, but
        -- we were doing an unsigned widen, then we must do an
        -- unsigned comparison at the smaller size.
    maybe_comparison (MO_U_Gt _) rep _     = Just (MO_U_Gt rep)
    maybe_comparison (MO_U_Ge _) rep _     = Just (MO_U_Ge rep)
    maybe_comparison (MO_U_Lt _) rep _     = Just (MO_U_Lt rep)
    maybe_comparison (MO_U_Le _) rep _     = Just (MO_U_Le rep)
    maybe_comparison (MO_Eq   _) rep _     = Just (MO_Eq   rep)
    maybe_comparison (MO_S_Gt _) rep True  = Just (MO_S_Gt rep)
    maybe_comparison (MO_S_Ge _) rep True  = Just (MO_S_Ge rep)
    maybe_comparison (MO_S_Lt _) rep True  = Just (MO_S_Lt rep)
    maybe_comparison (MO_S_Le _) rep True  = Just (MO_S_Le rep)
    maybe_comparison (MO_S_Gt _) rep False = Just (MO_U_Gt rep)
    maybe_comparison (MO_S_Ge _) rep False = Just (MO_U_Ge rep)
    maybe_comparison (MO_S_Lt _) rep False = Just (MO_U_Lt rep)
    maybe_comparison (MO_S_Le _) rep False = Just (MO_U_Le rep)
    maybe_comparison _ _ _ = Nothing

-- We can often do something with constants of 0, 1 and (-1) ...
-- See Note [Comparison operators]

cmmMachOpFoldM platform mop [x, y@(CmmLit (CmmInt 0 _))]
  = case mop of
        -- Arithmetic
        MO_Add   _ -> Just x   -- x + 0 = x
        MO_Sub   _ -> Just x   -- x - 0 = x
        MO_Mul   _ -> Just y   -- x * 0 = 0

        -- Logical operations
        MO_And   _ -> Just y   -- x &     0 = 0
        MO_Or    _ -> Just x   -- x |     0 = x
        MO_Xor   _ -> Just x   -- x `xor` 0 = x

        -- Shifts
        MO_Shl   _ -> Just x   -- x << 0 = x
        MO_S_Shr _ -> Just x   -- ditto shift-right
        MO_U_Shr _ -> Just x

        -- Comparisons; these ones are trickier
        -- See Note [Comparison operators]
        MO_Ne    _ | isComparisonExpr x -> Just x                -- (x > y) != 0  =  x > y
        MO_Eq    _ | Just x' <- maybeInvertCmmExpr x -> Just x'  -- (x > y) == 0  =  x <= y
        MO_U_Gt  _ | isComparisonExpr x -> Just x                -- (x > y) > 0   =  x > y
        MO_S_Gt  _ | isComparisonExpr x -> Just x                -- ditto
        MO_U_Lt  _ | isComparisonExpr x -> Just zero             -- (x > y) < 0  =  0
        MO_S_Lt  _ | isComparisonExpr x -> Just zero
        MO_U_Ge  _ | isComparisonExpr x -> Just one              -- (x > y) >= 0  =  1
        MO_S_Ge  _ | isComparisonExpr x -> Just one

        MO_U_Le  _ | Just x' <- maybeInvertCmmExpr x -> Just x'  -- (x > y) <= 0  =  x <= y
        MO_S_Le  _ | Just x' <- maybeInvertCmmExpr x -> Just x'
        _ -> Nothing
  where
    zero = CmmLit (CmmInt 0 (wordWidth platform))
    one  = CmmLit (CmmInt 1 (wordWidth platform))

cmmMachOpFoldM platform mop [x, (CmmLit (CmmInt 1 rep))]
  = case mop of
        -- Arithmetic: x*1 = x, etc
        MO_Mul    _ -> Just x
        MO_S_Quot _ -> Just x
        MO_U_Quot _ -> Just x
        MO_S_Rem  _ -> Just $! CmmLit (CmmInt 0 rep)
        MO_U_Rem  _ -> Just $! CmmLit (CmmInt 0 rep)

        -- Comparisons; trickier
        -- See Note [Comparison operators]
        MO_Ne    _ | Just x' <- maybeInvertCmmExpr x -> Just x'  -- (x>y) != 1  =  x<=y
        MO_Eq    _ | isComparisonExpr x -> Just x                -- (x>y) == 1  =  x>y
        MO_U_Lt  _ | Just x' <- maybeInvertCmmExpr x -> Just x'  -- (x>y) < 1   =  x<=y
        MO_S_Lt  _ | Just x' <- maybeInvertCmmExpr x -> Just x'  -- ditto
        MO_U_Gt  _ | isComparisonExpr x -> Just zero             -- (x>y) > 1   = 0
        MO_S_Gt  _ | isComparisonExpr x -> Just zero
        MO_U_Le  _ | isComparisonExpr x -> Just one              -- (x>y) <= 1  = 1
        MO_S_Le  _ | isComparisonExpr x -> Just one
        MO_U_Ge  _ | isComparisonExpr x -> Just x                -- (x>y) >= 1  = x>y
        MO_S_Ge  _ | isComparisonExpr x -> Just x
        _ -> Nothing
  where
    zero = CmmLit (CmmInt 0 (wordWidth platform))
    one  = CmmLit (CmmInt 1 (wordWidth platform))

-- Now look for multiplication/division by powers of 2 (integers).

cmmMachOpFoldM platform mop [x, (CmmLit (CmmInt n _))]
  = case mop of
        MO_Mul rep
           | Just p <- exactLog2 n ->
                 Just $! (cmmMachOpFold platform (MO_Shl rep) [x, CmmLit (CmmInt p $ wordWidth platform)])
        -- The optimization for division by power of 2 is technically duplicated, but since at least one other part of ghc uses
        -- the pure `constantFoldExpr` this remains
        MO_U_Quot rep
           | Just p <- exactLog2 n ->
                 Just $! (cmmMachOpFold platform (MO_U_Shr rep) [x, CmmLit (CmmInt p $ wordWidth platform)])
        MO_U_Rem rep
           | Just _ <- exactLog2 n ->
                 Just $! (cmmMachOpFold platform (MO_And rep) [x, CmmLit (CmmInt (n - 1) rep)])
        MO_S_Quot rep
           | Just p <- exactLog2 n,
             CmmReg _ <- x ->
                Just $! (cmmMachOpFold platform (MO_S_Shr rep)
                  [signedQuotRemHelper platform n x rep p, CmmLit (CmmInt p $ wordWidth platform)])
        MO_S_Rem rep
           | Just p <- exactLog2 n,
             CmmReg _ <- x ->
                -- We replace (x `rem` 2^p) by (x - (x `quot` 2^p) * 2^p).
                -- Moreover, we fuse MO_S_Shr (last operation of MO_S_Quot)
                -- and MO_S_Shl (multiplication by 2^p) into a single MO_And operation.
                Just $! (cmmMachOpFold platform (MO_Sub rep)
                    [x, cmmMachOpFold platform (MO_And rep)
                      [signedQuotRemHelper platform n x rep p, CmmLit (CmmInt (- n) rep)]])
        _ -> Nothing

-- ToDo (#7116): optimise floating-point multiplication, e.g. x*2.0 -> x+x
-- Unfortunately this needs a unique supply because x might not be a
-- register.  See #2253 (program 6) for an example.


-- Anything else is just too hard.

cmmMachOpFoldM _ _ _ = Nothing

-- | Check that a literal width is compatible with the host word size used to
-- store offsets. This should be fixed properly (using larger types to store
-- literal offsets). See #24893
validOffsetRep :: Width -> Bool
validOffsetRep rep = widthInBits rep <= finiteBitSize (undefined :: Int)


{- Note [Comparison operators]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If we have
   CmmCondBranch ((x>#y) == 1) t f
we really want to convert to
   CmmCondBranch (x>#y) t f

That's what the constant-folding operations on comparison operators do above.
-}

-- -----------------------------------------------------------------------------
-- Utils

isPicReg :: CmmExpr -> Bool
isPicReg (CmmReg (CmmGlobal (GlobalRegUse PicBaseReg _))) = True
isPicReg _ = False

canOptimizeDivision :: CmmConfig -> Width -> Bool
canOptimizeDivision cfg rep = cmmOptConstDivision cfg &&
  -- we can either widen the arguments to simulate mul2 or use mul2 directly for the platform word size
  (rep < wordWidth platform || (rep == wordWidth platform && cmmAllowMul2 cfg))
  where platform = cmmPlatform cfg

-- -----------------------------------------------------------------------------
-- Folding callish machops

cmmCallishMachOpFold :: CallishMachOp -> [CmmFormal] -> [CmmActual] -> Opt (CmmNode O O)
cmmCallishMachOpFold op res args =
  fromMaybe (CmmUnsafeForeignCall (PrimTarget op) res args) <$> (getConfig >>= \cfg -> cmmCallishMachOpFoldM cfg op res args)

cmmCallishMachOpFoldM :: CmmConfig -> CallishMachOp -> [CmmFormal] -> [CmmActual] -> Opt (Maybe (CmmNode O O))

-- If possible move the literals to the right, the following cases assume that to be the case
cmmCallishMachOpFoldM cfg op res [x@(CmmLit _),y]
  | isCommutableCallishMachOp op && not (isLit y) = cmmCallishMachOpFoldM cfg op res [y,x]

-- Both arguments are literals, replace with the result
cmmCallishMachOpFoldM _ op res [CmmLit (CmmInt x _), CmmLit (CmmInt y _)]
  = case op of
    MO_S_Mul2 rep
      | [rHiNeeded,rHi,rLo] <- res -> do
          let resSz = widthInBits rep
              resVal = (narrowS rep x) * (narrowS rep y)
              high = resVal `shiftR` resSz
              low = narrowS rep resVal
              isHiNeeded = high /= low `shiftR` resSz
              isHiNeededVal = if isHiNeeded then 1 else 0
          prependNode $! CmmAssign (CmmLocal rHiNeeded) (CmmLit $ CmmInt isHiNeededVal rep)
          prependNode $! CmmAssign (CmmLocal rHi) (CmmLit $ CmmInt high rep)
          pure . Just $! CmmAssign (CmmLocal rLo) (CmmLit $ CmmInt low rep)
    MO_U_Mul2 rep
      | [rHi,rLo] <- res -> do
          let resSz = widthInBits rep
              resVal = (narrowU rep x) * (narrowU rep y)
              high = resVal `shiftR` resSz
              low = narrowU rep resVal
          prependNode $! CmmAssign (CmmLocal rHi) (CmmLit $ CmmInt high rep)
          pure . Just $! CmmAssign (CmmLocal rLo) (CmmLit $ CmmInt low rep)
    MO_S_QuotRem rep
      | [rQuot, rRem] <- res,
        y /= 0 -> do
          let (q,r) = quotRem (narrowS rep x) (narrowS rep y)
          prependNode $! CmmAssign (CmmLocal rQuot) (CmmLit $ CmmInt q rep)
          pure . Just $! CmmAssign (CmmLocal rRem) (CmmLit $ CmmInt r rep)
    MO_U_QuotRem rep
      | [rQuot, rRem] <- res,
        y /= 0 -> do
          let (q,r) = quotRem (narrowU rep x) (narrowU rep y)
          prependNode $! CmmAssign (CmmLocal rQuot) (CmmLit $ CmmInt q rep)
          pure . Just $! CmmAssign (CmmLocal rRem) (CmmLit $ CmmInt r rep)
    _ -> pure Nothing

-- 0, 1 or -1 as one of the constants

cmmCallishMachOpFoldM _ op res [_, CmmLit (CmmInt 0 _)]
  = case op of
    -- x * 0 == 0
    MO_S_Mul2 rep
      | [rHiNeeded, rHi, rLo] <- res -> do
        prependNode $! CmmAssign (CmmLocal rHiNeeded) (CmmLit $ CmmInt 0 rep)
        prependNode $! CmmAssign (CmmLocal rHi) (CmmLit $ CmmInt 0 rep)
        pure . Just $! CmmAssign (CmmLocal rLo) (CmmLit $ CmmInt 0 rep)
    -- x * 0 == 0
    MO_U_Mul2 rep
      | [rHi, rLo] <- res -> do
        prependNode $! CmmAssign (CmmLocal rHi) (CmmLit $ CmmInt 0 rep)
        pure . Just $! CmmAssign (CmmLocal rLo) (CmmLit $ CmmInt 0 rep)
    _ -> pure Nothing

cmmCallishMachOpFoldM _ op res [CmmLit (CmmInt 0 _), _]
  = case op of
    -- 0 quotRem d == (0,0)
    MO_S_QuotRem rep
      | [rQuot, rRem] <- res -> do
      prependNode $! CmmAssign (CmmLocal rQuot) (CmmLit $ CmmInt 0 rep)
      pure . Just $! CmmAssign (CmmLocal rRem) (CmmLit $ CmmInt 0 rep)
    -- 0 quotRem d == (0,0)
    MO_U_QuotRem rep
      | [rQuot,rRem] <- res -> do
      prependNode $! CmmAssign (CmmLocal rQuot) (CmmLit $ CmmInt 0 rep)
      pure . Just $! CmmAssign (CmmLocal rRem) (CmmLit $ CmmInt 0 rep)
    _ -> pure Nothing

cmmCallishMachOpFoldM cfg op res [x, CmmLit (CmmInt 1 _)]
  = case op of
    -- x * 1 == x -- Note: The high word needs to be a sign extension of the low word, so we use a sign extending shift
    MO_S_Mul2 rep
      | [rHiNeeded, rHi, rLo] <- res -> do
        let platform = cmmPlatform cfg
            wordRep = wordWidth platform
            repInBits = toInteger $ widthInBits rep
        prependNode $! CmmAssign (CmmLocal rHiNeeded) (CmmLit $ CmmInt 0 rep)
        prependNode $! CmmAssign (CmmLocal rHi) (cmmMachOpFold platform (MO_S_Shr rep) [x, CmmLit $ CmmInt (repInBits - 1) wordRep])
        pure . Just $! CmmAssign (CmmLocal rLo) x
    -- x * 1 == x
    MO_U_Mul2 rep
      | [rHi, rLo] <- res -> do
        prependNode $! CmmAssign (CmmLocal rHi) (CmmLit $ CmmInt 0 rep)
        pure . Just $! CmmAssign (CmmLocal rLo) x
    -- x quotRem 1 == (x, 0)
    MO_S_QuotRem rep
      | [rQuot, rRem] <- res -> do
        prependNode $! CmmAssign (CmmLocal rQuot) x
        pure . Just $! CmmAssign (CmmLocal rRem) (CmmLit $ CmmInt 0 rep)
    -- x quotRem 1 == (x, 0)
    MO_U_QuotRem rep
      | [rQuot, rRem] <- res -> do
        prependNode $! CmmAssign (CmmLocal rQuot) x
        pure . Just $! CmmAssign (CmmLocal rRem) (CmmLit $ CmmInt 0 rep)
    _ -> pure Nothing

-- handle quotRem with a constant divisor

cmmCallishMachOpFoldM cfg op res [n, CmmLit (CmmInt d' _)]
  = case op of
    MO_S_QuotRem rep
      | Just p <- exactLog2 d,
        [rQuot,rRem] <- res -> do
          n' <- intoRegister n (cmmBits rep)
          -- first prepend the optimized division by a power 2
          prependNode $! CmmAssign (CmmLocal rQuot)
            (cmmMachOpFold platform (MO_S_Shr rep)
              [signedQuotRemHelper platform d n' rep p, CmmLit (CmmInt p $ wordWidth platform)])
          -- then output an optimized remainder by a power of 2
          pure . Just $! CmmAssign (CmmLocal rRem)
            (cmmMachOpFold platform (MO_Sub rep)
              [n', cmmMachOpFold platform (MO_And rep)
                [signedQuotRemHelper platform d n' rep p, CmmLit (CmmInt (- d) rep)]])
      | canOptimizeDivision cfg rep,
        d /= (-1), d /= 0, d /= 1,
        [rQuot,rRem] <- res -> do
          -- we are definitely going to use n multiple times, so put it into a register
          n' <- intoRegister n (cmmBits rep)
          -- generate an optimized (signed) division of n by d
          q <- generateDivisionBySigned platform cfg rep n' d
          -- we also need the result multiple times to calculate the remainder
          q' <- intoRegister q (cmmBits rep)

          prependNode $! CmmAssign (CmmLocal rQuot) q'
          -- The remainder now becomes n - q * d
          pure . Just $! CmmAssign (CmmLocal rRem) $ CmmMachOp (MO_Sub rep) [n', CmmMachOp (MO_Mul rep) [q', CmmLit $ CmmInt d rep]]
      where
        platform = cmmPlatform cfg
        d = narrowS rep d'
    MO_U_QuotRem rep
      | Just p <- exactLog2 d,
        [rQuot,rRem] <- res -> do
          -- first prepend the optimized division by a power 2
          prependNode $! CmmAssign (CmmLocal rQuot) $ CmmMachOp (MO_U_Shr rep) [n, CmmLit (CmmInt p $ wordWidth platform)]
          -- then output an optimized remainder by a power of 2
          pure . Just $! CmmAssign (CmmLocal rRem) $ CmmMachOp (MO_And rep) [n, CmmLit (CmmInt (d - 1) rep)]
      | canOptimizeDivision cfg rep,
        d /= 0, d /= 1,
        [rQuot,rRem] <- res -> do
          -- we are definitely going to use n multiple times, so put it into a register
          n' <- intoRegister n (cmmBits rep)
          -- generate an optimized (unsigned) division of n by d
          q <- generateDivisionByUnsigned platform cfg rep n' d
          -- we also need the result multiple times to calculate the remainder
          q' <- intoRegister q (cmmBits rep)

          prependNode $! CmmAssign (CmmLocal rQuot) q'
          -- The remainder now becomes n - q * d
          pure . Just $! CmmAssign (CmmLocal rRem) $ CmmMachOp (MO_Sub rep) [n', CmmMachOp (MO_Mul rep) [q', CmmLit $ CmmInt d rep]]
      where
        platform = cmmPlatform cfg
        d = narrowU rep d'
    _ -> pure Nothing

cmmCallishMachOpFoldM _ _ _ _ = pure Nothing

-- -----------------------------------------------------------------------------
-- Specialized constant folding for MachOps which sometimes need to expand into multiple nodes

cmmMachOpFoldOptM :: CmmConfig -> MachOp -> [CmmExpr] -> Opt (Maybe CmmExpr)

cmmMachOpFoldOptM cfg op [n, CmmLit (CmmInt d' _)] =
  case op of
    MO_S_Quot rep
      -- recheck for power of 2 division. This may not be handled by cmmMachOpFoldM if n is not in a register
      | Just p <- exactLog2 d -> do
        n' <- intoRegister n (cmmBits rep)
        pure . Just $! cmmMachOpFold platform (MO_S_Shr rep)
          [ signedQuotRemHelper platform d n' rep p
          , CmmLit (CmmInt p $ wordWidth platform)
          ]
      | canOptimizeDivision cfg rep,
        d /= (-1), d /= 0, d /= 1 -> Just <$!> generateDivisionBySigned platform cfg rep n d
      where d = narrowS rep d'
    MO_S_Rem rep
      -- recheck for power of 2 remainder. This may not be handled by cmmMachOpFoldM if n is not in a register
      | Just p <- exactLog2 d -> do
        n' <- intoRegister n (cmmBits rep)
        pure . Just $! cmmMachOpFold platform (MO_Sub rep)
          [ n'
          , cmmMachOpFold platform (MO_And rep)
              [ signedQuotRemHelper platform d n' rep p
              , CmmLit (CmmInt (- d) rep)
              ]
          ]
      | canOptimizeDivision cfg rep,
        d /= (-1), d /= 0, d /= 1 -> do
        n' <- intoRegister n (cmmBits rep)
        -- first generate the division
        q <- generateDivisionBySigned platform cfg rep n' d
        -- then calculate the remainder by n - q * d
        pure . Just $! CmmMachOp (MO_Sub rep) [n', CmmMachOp (MO_Mul rep) [q, CmmLit $ CmmInt d rep]]
      where d = narrowS rep d'
    MO_U_Quot rep
      -- No need to recheck power of 2 division because cmmMachOpFoldM always handles that case
      | canOptimizeDivision cfg rep,
        d /= 0, d /= 1, Nothing <- exactLog2 d -> Just <$!> generateDivisionByUnsigned platform cfg rep n d
      where d = narrowU rep d'
    MO_U_Rem rep
      -- No need to recheck power of 2 remainder because cmmMachOpFoldM always handles that case
      | canOptimizeDivision cfg rep,
        d /= 0, d /= 1, Nothing <- exactLog2 d -> do
        n' <- intoRegister n (cmmBits rep)
        -- first generate the division
        q <- generateDivisionByUnsigned platform cfg rep n d
        -- then calculate the remainder by n - q * d
        pure . Just $! CmmMachOp (MO_Sub rep) [n', CmmMachOp (MO_Mul rep) [q, CmmLit $ CmmInt d rep]]
      where d = narrowU rep d'
    _ -> pure Nothing
  where platform = cmmPlatform cfg

cmmMachOpFoldOptM _ _ _ = pure Nothing

-- -----------------------------------------------------------------------------
-- Utils for prepending new nodes

-- Move an expression into a register to possibly use it multiple times
intoRegister :: CmmExpr -> CmmType -> Opt CmmExpr
intoRegister e@(CmmReg _) _ = pure e
intoRegister expr ty = do
  u <- getUniqueM
  let reg = LocalReg u ty
  CmmReg (CmmLocal reg) <$ prependNode (CmmAssign (CmmLocal reg) expr)

prependNode :: CmmNode O O -> Opt ()
prependNode n = Opt $ \_ xs -> pure (xs ++ [n], ())

-- -----------------------------------------------------------------------------
-- Division by constants utils

-- Helper for division by a power of 2
-- In contrast with unsigned integers, for signed ones
-- shift right is not the same as quot, because it rounds
-- to minus infinity, whereas quot rounds toward zero.
-- To fix this up, we add one less than the divisor to the
-- dividend if it is a negative number.
--
-- to avoid a test/jump, we use the following sequence:
--      x1 = x >> word_size-1  (all 1s if -ve, all 0s if +ve)
--      x2 = y & (divisor-1)
--      result = x + x2
-- this could be done a bit more simply using conditional moves,
-- but we're processor independent here.
--
-- we optimize the divide by 2 case slightly, generating
--      x1 = x >> word_size-1  (unsigned)
--      return = x + x1
signedQuotRemHelper :: Platform -> Integer -> CmmExpr -> Width -> Integer -> CmmExpr
signedQuotRemHelper platform n x rep p = CmmMachOp (MO_Add rep) [x, x2]
  where
    bits = fromIntegral (widthInBits rep) - 1
    shr = if p == 1 then MO_U_Shr rep else MO_S_Shr rep
    x1 = CmmMachOp shr [x, CmmLit (CmmInt bits $ wordWidth platform)]
    x2 = if p == 1 then x1 else
          CmmMachOp (MO_And rep) [x1, CmmLit (CmmInt (n-1) rep)]

{- Note: [Division by constants]

Integer division is floor(n / d), the goal is to find m,p
such that floor((m * n) / 2^p) = floor(n / d).

The idea being: n/d = n * (1/d). But we cannot store 1/d in an integer without
some error, so we choose some 2^p / d such that the error ends up small and
thus vanishes when we divide by 2^p again.

The algorithm below to generate these numbers is taken from Hacker's Delight
Second Edition Chapter 10 "Integer division by constants". The chapter also
contains proof that this method does indeed produce correct results.

However this is a much more literal interpretation of the algorithm,
which we can use because of the unbounded Integer type. Hacker's Delight
also provides a much more complex algorithm which computes these numbers
without the need to exceed the word size, but that is not necessary here.
-}

generateDivisionBySigned :: Platform -> CmmConfig -> Width -> CmmExpr -> Integer -> Opt CmmExpr

-- Sanity checks, division will generate incorrect results or undesirable code for these cases
-- cmmMachOpFoldM and cmmMachOpFoldOptM should have already handled these cases!
generateDivisionBySigned _ _ _ _ 0 = panic "generate signed division with 0"
generateDivisionBySigned _ _ _ _ 1 = panic "generate signed division with 1"
generateDivisionBySigned _ _ _ _ (-1) = panic "generate signed division with -1"
generateDivisionBySigned _ _ _ _ d | Just _ <- exactLog2 d = panic $ "generate signed division with " ++ show d

generateDivisionBySigned platform _cfg rep n divisor = do
  -- We only duplicate n' if we actually need to add/subtract it, so we may not need it in a register
  n' <- if sign == 0 then pure n else intoRegister n resRep

  -- Set up mul2
  (shift', qExpr) <- mul2 n'

  -- add/subtract n if necessary
  let qExpr' = case sign of
        1  -> CmmMachOp (MO_Add rep) [qExpr, n']
        -1 -> CmmMachOp (MO_Sub rep) [qExpr, n']
        _  -> qExpr

  qExpr'' <- intoRegister (cmmMachOpFold platform (MO_S_Shr rep) [qExpr', CmmLit $ CmmInt shift' wordRep]) resRep

  -- Lastly add the sign of the quotient to correct for negative results
  pure $! cmmMachOpFold platform
    (MO_Add rep) [qExpr'', cmmMachOpFold platform (MO_U_Shr rep) [qExpr'', CmmLit $ CmmInt (toInteger $ widthInBits rep - 1) wordRep]]
  where
    resRep = cmmBits rep
    wordRep = wordWidth platform
    (magic, sign, shift) = divisionMagicS rep divisor
    -- generate the multiply with the magic number
    mul2 n
      -- Using mul2 for sub-word sizes regresses for signed integers only
      | rep == wordWidth platform = do
        (r1, r2, r3) <- (,,) <$> getUniqueM <*> getUniqueM <*> getUniqueM
        let rg1    = LocalReg r1 resRep
            resReg = LocalReg r2 resRep
            rg3    = LocalReg r3 resRep
        res <- CmmReg (CmmLocal resReg) <$ prependNode (CmmUnsafeForeignCall (PrimTarget (MO_S_Mul2 rep)) [rg1, resReg, rg3] [n, CmmLit $ CmmInt magic rep])
        pure (shift, res)
      -- widen the register and multiply without the MUL2 instruction
      -- if we don't need an additional add after this we can combine the shifts
      | otherwise = pure (if sign == 0 then 0 else shift, res)
          where
            wordRep = wordWidth platform
            -- (n * magic) >> widthInBits + (if sign == 0 then shift else 0) -- With conversion in between to not overflow
            res = cmmMachOpFold platform (MO_SS_Conv wordRep rep)
                    [ cmmMachOpFold platform (MO_S_Shr wordRep)
                      [ cmmMachOpFold platform (MO_Mul wordRep)
                        [ cmmMachOpFold platform (MO_SS_Conv rep wordRep) [n]
                        , CmmLit $ CmmInt magic wordRep
                        ]
                      -- Check if we need to generate an add/subtract later. If not we can combine this with the postshift
                      , CmmLit $ CmmInt ((if sign == 0 then toInteger shift else 0) + (toInteger $ widthInBits rep)) wordRep
                      ]
                    ]

-- See hackers delight for how and why this works (chapter in note [Division by constants])
divisionMagicS :: Width -> Integer -> (Integer, Integer, Integer)
divisionMagicS rep divisor = (magic, sign, toInteger $ p - wSz)
  where
    sign = if divisor > 0
      then if magic < 0 then 1 else 0
      else if magic < 0 then 0 else -1
    wSz = widthInBits rep
    ad = abs divisor
    t = (1 `shiftL` (wSz - 1)) + if divisor > 0 then 0 else 1
    anc = t - 1 - rem t ad
    go p'
      | twoP > anc * (ad - rem twoP ad) = p'
      | otherwise = go (p' + 1)
      where twoP = 1 `shiftL` p'
    p = go wSz
    am = (twoP + ad - rem twoP ad) `quot` ad
      where twoP = 1 `shiftL` p
    magic = narrowS rep $ if divisor > 0 then am else -am

generateDivisionByUnsigned :: Platform -> CmmConfig -> Width -> CmmExpr -> Integer -> Opt CmmExpr
-- Sanity checks, division will generate incorrect results or undesirable code for these cases
-- cmmMachOpFoldM and cmmMachOpFoldOptM should have already handled these cases!
generateDivisionByUnsigned _ _ _ _ 0 = panic "generate signed division with 0"
generateDivisionByUnsigned _ _ _ _ 1 = panic "generate signed division with 1"
generateDivisionByUnsigned _ _ _ _ d | Just _ <- exactLog2 d = panic $ "generate signed division with " ++ show d

generateDivisionByUnsigned platform cfg rep n divisor = do
  -- We only duplicate n' if we actually need to add/subtract it, so we may not need it in a register
  n' <- if not needsAdd -- Invariant: We also never preshift if we need an add, thus we don't need n in a register
    then pure $! cmmMachOpFold platform (MO_U_Shr rep) [n, CmmLit $ CmmInt preShift wordRep]
    else intoRegister n resRep

  -- Set up mul2
  (postShift', qExpr) <- mul2 n'

  -- add/subtract n if necessary
  let qExpr' = if needsAdd
        -- This is qExpr + (n - qExpr) / 2 = (qExpr + n) / 2 but with a guarantee that it'll not overflow
        then cmmMachOpFold platform (MO_Add rep)
          [ cmmMachOpFold platform (MO_U_Shr rep)
            [ cmmMachOpFold platform (MO_Sub rep) [n', qExpr]
            , CmmLit $ CmmInt 1 wordRep
            ]
          , qExpr
          ]
        else qExpr
      -- If we already divided by 2 in the add, remember to shift one bit less
      -- Hacker's Delight, Edition 2 Page 234: postShift > 0 if we needed an add, except if the divisor
      -- is 1, which we checked for above
      finalShift = if needsAdd then postShift' - 1 else postShift'

  -- apply the final postShift
  pure $! cmmMachOpFold platform (MO_U_Shr rep) [qExpr', CmmLit $ CmmInt finalShift wordRep]
  where
    resRep = cmmBits rep
    wordRep = wordWidth platform
    (preShift, magic, needsAdd, postShift) =
        let withPre = divisionMagicU rep True  divisor
            noPre   = divisionMagicU rep False divisor
        in case (withPre, noPre) of
          -- Use whatever does not cause us to take the expensive case
          ((_, _, False, _), (_, _, True, _)) -> withPre
          -- If we cannot avoid the expensive case, don't bother with the pre shift
          _ -> noPre
    -- generate the multiply with the magic number
    mul2 n
      | rep == wordWidth platform || (cmmAllowMul2 cfg && needsAdd) = do
        (r1, r2) <- (,) <$> getUniqueM <*> getUniqueM
        let rg1    = LocalReg r1 resRep
            resReg = LocalReg r2 resRep
        res <- CmmReg (CmmLocal resReg) <$ prependNode (CmmUnsafeForeignCall (PrimTarget (MO_U_Mul2 rep)) [resReg, rg1] [n, CmmLit $ CmmInt magic rep])
        pure (postShift, res)
      | otherwise = do
        pure (if needsAdd then postShift else 0, res)
          where
            wordRep = wordWidth platform
            -- (n * magic) >> widthInBits + (if sign == 0 then shift else 0) -- With conversion in between to not overflow
            res = cmmMachOpFold platform (MO_UU_Conv wordRep rep)
              [ cmmMachOpFold platform (MO_U_Shr wordRep)
                [ cmmMachOpFold platform (MO_Mul wordRep)
                  [ cmmMachOpFold platform (MO_UU_Conv rep wordRep) [n]
                  , CmmLit $ CmmInt magic wordRep
                  ]
                -- Check if we need to generate an add later. If not we can combine this with the postshift
                , CmmLit $ CmmInt ((if needsAdd then 0 else postShift) + (toInteger $ widthInBits rep)) wordRep
                ]
              ]

-- See hackers delight for how and why this works (chapter in note [Division by constants])
-- The preshift isn't described there, but the idea is:
-- If a divisor d has n trailing zeros, then d is a multiple of 2^n. Since we want to divide x by d
-- we can also calculate (x / 2^n) / (d / 2^n) which may then not require an extra addition.
--
-- The addition performs: quotient + dividend, but we need to avoid overflows, so we actually need to
-- calculate: quotient + (dividend - quotient) / 2 = (quotient + dividend) / 2
-- Thus if the preshift can avoid all of this, we have 1 operation in place of 3.
--
-- The decision to use the preshift is made somewhere else, here we only report if the addition is needed
divisionMagicU :: Width -> Bool -> Integer -> (Integer, Integer, Bool, Integer)
divisionMagicU rep doPreShift divisor = (toInteger zeros, magic, needsAdd, toInteger $ p - wSz)
  where
    wSz = widthInBits rep
    zeros = if doPreShift then countTrailingZeros $ fromInteger @Word64 divisor else 0
    d = divisor `shiftR` zeros
    ones = ((1 `shiftL` wSz) - 1) `shiftR` zeros
    nc = ones - rem (ones - d) d
    go p'
      | twoP > nc * (d - 1 - rem (twoP - 1) d) = p'
      | otherwise = go (p' + 1)
      where twoP = 1 `shiftL` p'
    p = go wSz
    m = (twoP + d - 1 - rem (twoP - 1) d) `quot` d
      where twoP = 1 `shiftL` p
    needsAdd = d < 1 `shiftL` (p - wSz)
    magic = if needsAdd then m - (ones + 1) else m

-- -----------------------------------------------------------------------------
-- Opt monad

newtype Opt a = OptI { runOptI :: CmmConfig -> [CmmNode O O] -> UniqSM ([CmmNode O O], a) }

-- | Pattern synonym for 'Opt', as described in Note [The one-shot state
-- monad trick].
pattern Opt :: (CmmConfig -> [CmmNode O O] -> UniqSM ([CmmNode O O], a)) -> Opt a
pattern Opt f <- OptI f
  where Opt f = OptI . oneShot $ \cfg -> oneShot $ \out -> f cfg out
{-# COMPLETE Opt #-}

runOpt :: CmmConfig -> Opt a -> UniqSM ([CmmNode O O], a)
runOpt cf (Opt g) = g cf []

getConfig :: Opt CmmConfig
getConfig = Opt $ \cf xs -> pure (xs, cf)

instance Functor Opt where
  fmap f (Opt g) = Opt $ \cf xs -> fmap (fmap f) (g cf xs)

instance Applicative Opt where
  pure a = Opt $ \_ xs -> pure (xs, a)
  ff <*> fa = do
    f <- ff
    f <$> fa

instance Monad Opt where
  Opt g >>= f = Opt $ \cf xs -> do
    (ys, a) <- g cf xs
    runOptI (f a) cf ys

instance MonadUnique Opt where
  getUniqueSupplyM = Opt $ \_ xs -> (xs,) <$> getUniqueSupplyM
  getUniqueM       = Opt $ \_ xs -> (xs,) <$> getUniqueM
  getUniquesM      = Opt $ \_ xs -> (xs,) <$> getUniquesM

mapForeignTargetOpt :: (CmmExpr -> Opt CmmExpr) -> ForeignTarget -> Opt ForeignTarget
mapForeignTargetOpt exp   (ForeignTarget e c) = flip ForeignTarget c <$> exp e
mapForeignTargetOpt _   m@(PrimTarget _)      = pure m

wrapRecExpOpt :: (CmmExpr -> Opt CmmExpr) -> CmmExpr -> Opt CmmExpr
wrapRecExpOpt f (CmmMachOp op es)       = traverse (wrapRecExpOpt f) es >>= f . CmmMachOp op
wrapRecExpOpt f (CmmLoad addr ty align) = wrapRecExpOpt f addr >>= \newAddr -> f (CmmLoad newAddr ty align)
wrapRecExpOpt f e                       = f e

mapExpOpt :: (CmmExpr -> Opt CmmExpr) -> CmmNode e x -> Opt (CmmNode e x)
mapExpOpt _ f@(CmmEntry{})                          = pure f
mapExpOpt _ m@(CmmComment _)                        = pure m
mapExpOpt _ m@(CmmTick _)                           = pure m
mapExpOpt f   (CmmUnwind regs)                      = CmmUnwind <$> traverse (traverse (traverse f)) regs
mapExpOpt f   (CmmAssign r e)                       = CmmAssign r <$> f e
mapExpOpt f   (CmmStore addr e align)               = CmmStore <$> f addr <*> f e <*> pure align
mapExpOpt f   (CmmUnsafeForeignCall tgt fs as)      = CmmUnsafeForeignCall <$> mapForeignTargetOpt f tgt <*> pure fs <*> traverse f as
mapExpOpt _ l@(CmmBranch _)                         = pure l
mapExpOpt f   (CmmCondBranch e ti fi l)             = f e >>= \newE -> pure (CmmCondBranch newE ti fi l)
mapExpOpt f   (CmmSwitch e ids)                     = flip CmmSwitch ids <$> f e
mapExpOpt f   n@CmmCall {cml_target=tgt}            = f tgt >>= \newTgt -> pure n{cml_target = newTgt}
mapExpOpt f   (CmmForeignCall tgt fs as succ ret_args updfr intrbl)
                                                    = do
                                                      newTgt <- mapForeignTargetOpt f tgt
                                                      newAs <- traverse f as
                                                      pure $ CmmForeignCall newTgt fs newAs succ ret_args updfr intrbl
