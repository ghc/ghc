-----------------------------------------------------------------------------
--
-- Cmm optimisation
--
-- (c) The University of Glasgow 2006
--
-----------------------------------------------------------------------------

module CmmOpt (
        cmmMachOpFold,
        cmmMachOpFoldM
 ) where

#include "HsVersions.h"

import CmmUtils
import Cmm
import DynFlags

import FastTypes
import Outputable
import Platform

import Data.Bits
import Data.Maybe

-- -----------------------------------------------------------------------------
-- MachOp constant folder

-- Now, try to constant-fold the MachOps.  The arguments have already
-- been optimized and folded.

cmmMachOpFold
    :: DynFlags
    -> MachOp       -- The operation from an CmmMachOp
    -> [CmmExpr]    -- The optimized arguments
    -> CmmExpr

cmmMachOpFold dflags op args = fromMaybe (CmmMachOp op args) (cmmMachOpFoldM dflags op args)

-- Returns Nothing if no changes, useful for Hoopl, also reduces
-- allocation!
cmmMachOpFoldM
    :: DynFlags
    -> MachOp
    -> [CmmExpr]
    -> Maybe CmmExpr

cmmMachOpFoldM _ op [CmmLit (CmmInt x rep)]
  = Just $ case op of
      MO_S_Neg _ -> CmmLit (CmmInt (-x) rep)
      MO_Not _   -> CmmLit (CmmInt (complement x) rep)

        -- these are interesting: we must first narrow to the
        -- "from" type, in order to truncate to the correct size.
        -- The final narrow/widen to the destination type
        -- is implicit in the CmmLit.
      MO_SF_Conv _from to -> CmmLit (CmmFloat (fromInteger x) to)
      MO_SS_Conv  from to -> CmmLit (CmmInt (narrowS from x) to)
      MO_UU_Conv  from to -> CmmLit (CmmInt (narrowU from x) to)

      _ -> panic "cmmMachOpFoldM: unknown unary op"


-- Eliminate conversion NOPs
cmmMachOpFoldM _ (MO_SS_Conv rep1 rep2) [x] | rep1 == rep2 = Just x
cmmMachOpFoldM _ (MO_UU_Conv rep1 rep2) [x] | rep1 == rep2 = Just x

-- Eliminate nested conversions where possible
cmmMachOpFoldM dflags conv_outer [CmmMachOp conv_inner [x]]
  | Just (rep1,rep2,signed1) <- isIntConversion conv_inner,
    Just (_,   rep3,signed2) <- isIntConversion conv_outer
  = case () of
        -- widen then narrow to the same size is a nop
      _ | rep1 < rep2 && rep1 == rep3 -> Just x
        -- Widen then narrow to different size: collapse to single conversion
        -- but remember to use the signedness from the widening, just in case
        -- the final conversion is a widen.
        | rep1 < rep2 && rep2 > rep3 ->
            Just $ cmmMachOpFold dflags (intconv signed1 rep1 rep3) [x]
        -- Nested widenings: collapse if the signedness is the same
        | rep1 < rep2 && rep2 < rep3 && signed1 == signed2 ->
            Just $ cmmMachOpFold dflags (intconv signed1 rep1 rep3) [x]
        -- Nested narrowings: collapse
        | rep1 > rep2 && rep2 > rep3 ->
            Just $ cmmMachOpFold dflags (MO_UU_Conv rep1 rep3) [x]
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

-- ToDo: a narrow of a load can be collapsed into a narrow load, right?
-- but what if the architecture only supports word-sized loads, should
-- we do the transformation anyway?

cmmMachOpFoldM dflags mop [CmmLit (CmmInt x xrep), CmmLit (CmmInt y _)]
  = case mop of
        -- for comparisons: don't forget to narrow the arguments before
        -- comparing, since they might be out of range.
        MO_Eq _   -> Just $ CmmLit (CmmInt (if x_u == y_u then 1 else 0) (wordWidth dflags))
        MO_Ne _   -> Just $ CmmLit (CmmInt (if x_u /= y_u then 1 else 0) (wordWidth dflags))

        MO_U_Gt _ -> Just $ CmmLit (CmmInt (if x_u >  y_u then 1 else 0) (wordWidth dflags))
        MO_U_Ge _ -> Just $ CmmLit (CmmInt (if x_u >= y_u then 1 else 0) (wordWidth dflags))
        MO_U_Lt _ -> Just $ CmmLit (CmmInt (if x_u <  y_u then 1 else 0) (wordWidth dflags))
        MO_U_Le _ -> Just $ CmmLit (CmmInt (if x_u <= y_u then 1 else 0) (wordWidth dflags))

        MO_S_Gt _ -> Just $ CmmLit (CmmInt (if x_s >  y_s then 1 else 0) (wordWidth dflags))
        MO_S_Ge _ -> Just $ CmmLit (CmmInt (if x_s >= y_s then 1 else 0) (wordWidth dflags))
        MO_S_Lt _ -> Just $ CmmLit (CmmInt (if x_s <  y_s then 1 else 0) (wordWidth dflags))
        MO_S_Le _ -> Just $ CmmLit (CmmInt (if x_s <= y_s then 1 else 0) (wordWidth dflags))

        MO_Add r -> Just $ CmmLit (CmmInt (x + y) r)
        MO_Sub r -> Just $ CmmLit (CmmInt (x - y) r)
        MO_Mul r -> Just $ CmmLit (CmmInt (x * y) r)
        MO_U_Quot r | y /= 0 -> Just $ CmmLit (CmmInt (x_u `quot` y_u) r)
        MO_U_Rem  r | y /= 0 -> Just $ CmmLit (CmmInt (x_u `rem`  y_u) r)
        MO_S_Quot r | y /= 0 -> Just $ CmmLit (CmmInt (x `quot` y) r)
        MO_S_Rem  r | y /= 0 -> Just $ CmmLit (CmmInt (x `rem` y) r)

        MO_And   r -> Just $ CmmLit (CmmInt (x .&. y) r)
        MO_Or    r -> Just $ CmmLit (CmmInt (x .|. y) r)
        MO_Xor   r -> Just $ CmmLit (CmmInt (x `xor` y) r)

        MO_Shl   r -> Just $ CmmLit (CmmInt (x `shiftL` fromIntegral y) r)
        MO_U_Shr r -> Just $ CmmLit (CmmInt (x_u `shiftR` fromIntegral y) r)
        MO_S_Shr r -> Just $ CmmLit (CmmInt (x `shiftR` fromIntegral y) r)

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

cmmMachOpFoldM dflags op [x@(CmmLit _), y]
   | not (isLit y) && isCommutableMachOp op
   = Just (cmmMachOpFold dflags op [y, x])

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
cmmMachOpFoldM dflags mop1 [CmmMachOp mop2 [arg1,arg2], arg3]
   | mop2 `associates_with` mop1
     && not (isLit arg1) && not (isPicReg arg1)
   = Just (cmmMachOpFold dflags mop2 [arg1, cmmMachOpFold dflags mop1 [arg2,arg3]])
   where
     MO_Add{} `associates_with` MO_Sub{} = True
     mop1 `associates_with` mop2 =
        mop1 == mop2 && isAssociativeMachOp mop1

-- special case: (a - b) + c  ==>  a + (c - b)
cmmMachOpFoldM dflags mop1@(MO_Add{}) [CmmMachOp mop2@(MO_Sub{}) [arg1,arg2], arg3]
   | not (isLit arg1) && not (isPicReg arg1)
   = Just (cmmMachOpFold dflags mop1 [arg1, cmmMachOpFold dflags mop2 [arg3,arg2]])

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
  = Just $ CmmMachOp op [pic, CmmLit $ cmmOffsetLit lit off ]
  where off = fromIntegral (narrowS rep n)

-- Make a RegOff if we can
cmmMachOpFoldM _ (MO_Add _) [CmmReg reg, CmmLit (CmmInt n rep)]
  = Just $ cmmRegOff reg (fromIntegral (narrowS rep n))
cmmMachOpFoldM _ (MO_Add _) [CmmRegOff reg off, CmmLit (CmmInt n rep)]
  = Just $ cmmRegOff reg (off + fromIntegral (narrowS rep n))
cmmMachOpFoldM _ (MO_Sub _) [CmmReg reg, CmmLit (CmmInt n rep)]
  = Just $ cmmRegOff reg (- fromIntegral (narrowS rep n))
cmmMachOpFoldM _ (MO_Sub _) [CmmRegOff reg off, CmmLit (CmmInt n rep)]
  = Just $ cmmRegOff reg (off - fromIntegral (narrowS rep n))

-- Fold label(+/-)offset into a CmmLit where possible

cmmMachOpFoldM _ (MO_Add _) [CmmLit lit, CmmLit (CmmInt i rep)]
  = Just $ CmmLit (cmmOffsetLit lit (fromIntegral (narrowU rep i)))
cmmMachOpFoldM _ (MO_Add _) [CmmLit (CmmInt i rep), CmmLit lit]
  = Just $ CmmLit (cmmOffsetLit lit (fromIntegral (narrowU rep i)))
cmmMachOpFoldM _ (MO_Sub _) [CmmLit lit, CmmLit (CmmInt i rep)]
  = Just $ CmmLit (cmmOffsetLit lit (fromIntegral (negate (narrowU rep i))))


-- Comparison of literal with widened operand: perform the comparison
-- at the smaller width, as long as the literal is within range.

-- We can't do the reverse trick, when the operand is narrowed:
-- narrowing throws away bits from the operand, there's no way to do
-- the same comparison at the larger size.

cmmMachOpFoldM dflags cmp [CmmMachOp conv [x], CmmLit (CmmInt i _)]
  |     -- powerPC NCG has a TODO for I8/I16 comparisons, so don't try
    platformArch (targetPlatform dflags) `elem` [ArchX86, ArchX86_64],
        -- if the operand is widened:
    Just (rep, signed, narrow_fn) <- maybe_conversion conv,
        -- and this is a comparison operation:
    Just narrow_cmp <- maybe_comparison cmp rep signed,
        -- and the literal fits in the smaller size:
    i == narrow_fn rep i
        -- then we can do the comparison at the smaller size
  = Just (cmmMachOpFold dflags narrow_cmp [x, CmmLit (CmmInt i rep)])
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

-- We can often do something with constants of 0 and 1 ...

cmmMachOpFoldM dflags mop [x, y@(CmmLit (CmmInt 0 _))]
  = case mop of
        MO_Add   _ -> Just x
        MO_Sub   _ -> Just x
        MO_Mul   _ -> Just y
        MO_And   _ -> Just y
        MO_Or    _ -> Just x
        MO_Xor   _ -> Just x
        MO_Shl   _ -> Just x
        MO_S_Shr _ -> Just x
        MO_U_Shr _ -> Just x
        MO_Ne    _ | isComparisonExpr x -> Just x
        MO_Eq    _ | Just x' <- maybeInvertCmmExpr x -> Just x'
        MO_U_Gt  _ | isComparisonExpr x -> Just x
        MO_S_Gt  _ | isComparisonExpr x -> Just x
        MO_U_Lt  _ | isComparisonExpr x -> Just $ CmmLit (CmmInt 0 (wordWidth dflags))
        MO_S_Lt  _ | isComparisonExpr x -> Just $ CmmLit (CmmInt 0 (wordWidth dflags))
        MO_U_Ge  _ | isComparisonExpr x -> Just $ CmmLit (CmmInt 1 (wordWidth dflags))
        MO_S_Ge  _ | isComparisonExpr x -> Just $ CmmLit (CmmInt 1 (wordWidth dflags))
        MO_U_Le  _ | Just x' <- maybeInvertCmmExpr x -> Just x'
        MO_S_Le  _ | Just x' <- maybeInvertCmmExpr x -> Just x'
        _ -> Nothing

cmmMachOpFoldM dflags mop [x, (CmmLit (CmmInt 1 rep))]
  = case mop of
        MO_Mul    _ -> Just x
        MO_S_Quot _ -> Just x
        MO_U_Quot _ -> Just x
        MO_S_Rem  _ -> Just $ CmmLit (CmmInt 0 rep)
        MO_U_Rem  _ -> Just $ CmmLit (CmmInt 0 rep)
        MO_Ne    _ | Just x' <- maybeInvertCmmExpr x -> Just x'
        MO_Eq    _ | isComparisonExpr x -> Just x
        MO_U_Lt  _ | Just x' <- maybeInvertCmmExpr x -> Just x'
        MO_S_Lt  _ | Just x' <- maybeInvertCmmExpr x -> Just x'
        MO_U_Gt  _ | isComparisonExpr x -> Just $ CmmLit (CmmInt 0 (wordWidth dflags))
        MO_S_Gt  _ | isComparisonExpr x -> Just $ CmmLit (CmmInt 0 (wordWidth dflags))
        MO_U_Le  _ | isComparisonExpr x -> Just $ CmmLit (CmmInt 1 (wordWidth dflags))
        MO_S_Le  _ | isComparisonExpr x -> Just $ CmmLit (CmmInt 1 (wordWidth dflags))
        MO_U_Ge  _ | isComparisonExpr x -> Just x
        MO_S_Ge  _ | isComparisonExpr x -> Just x
        _ -> Nothing

-- Now look for multiplication/division by powers of 2 (integers).

cmmMachOpFoldM dflags mop [x, (CmmLit (CmmInt n _))]
  = case mop of
        MO_Mul rep
           | Just p <- exactLog2 n ->
                 Just (cmmMachOpFold dflags (MO_Shl rep) [x, CmmLit (CmmInt p rep)])
        MO_U_Quot rep
           | Just p <- exactLog2 n ->
                 Just (cmmMachOpFold dflags (MO_U_Shr rep) [x, CmmLit (CmmInt p rep)])
        MO_S_Quot rep
           | Just p <- exactLog2 n,
             CmmReg _ <- x ->   -- We duplicate x below, hence require
                                -- it is a reg.  FIXME: remove this restriction.
                -- shift right is not the same as quot, because it rounds
                -- to minus infinity, whereasq quot rounds toward zero.
                -- To fix this up, we add one less than the divisor to the
                -- dividend if it is a negative number.
                --
                -- to avoid a test/jump, we use the following sequence:
                --      x1 = x >> word_size-1  (all 1s if -ve, all 0s if +ve)
                --      x2 = y & (divisor-1)
                --      result = (x+x2) >>= log2(divisor)
                -- this could be done a bit more simply using conditional moves,
                -- but we're processor independent here.
                --
                -- we optimise the divide by 2 case slightly, generating
                --      x1 = x >> word_size-1  (unsigned)
                --      return = (x + x1) >>= log2(divisor)
                let
                    bits = fromIntegral (widthInBits rep) - 1
                    shr = if p == 1 then MO_U_Shr rep else MO_S_Shr rep
                    x1 = CmmMachOp shr [x, CmmLit (CmmInt bits rep)]
                    x2 = if p == 1 then x1 else
                         CmmMachOp (MO_And rep) [x1, CmmLit (CmmInt (n-1) rep)]
                    x3 = CmmMachOp (MO_Add rep) [x, x2]
                in
                Just (cmmMachOpFold dflags (MO_S_Shr rep) [x3, CmmLit (CmmInt p rep)])
        _ -> Nothing

-- ToDo (#7116): optimise floating-point multiplication, e.g. x*2.0 -> x+x
-- Unfortunately this needs a unique supply because x might not be a
-- register.  See #2253 (program 6) for an example.


-- Anything else is just too hard.

cmmMachOpFoldM _ _ _ = Nothing

-- -----------------------------------------------------------------------------
-- exactLog2

-- This algorithm for determining the $\log_2$ of exact powers of 2 comes
-- from GCC.  It requires bit manipulation primitives, and we use GHC
-- extensions.  Tough.
--
-- Used to be in MachInstrs --SDM.
-- ToDo: remove use of unboxery --SDM.

-- Unboxery removed in favor of FastInt; but is the function supposed to fail
-- on inputs >= 2147483648, or was that just an implementation artifact?
-- And is this speed-critical, or can we just use Integer operations
-- (including Data.Bits)?
--  --Isaac Dupree

exactLog2 :: Integer -> Maybe Integer
exactLog2 x_
  = if (x_ <= 0 || x_ >= 2147483648) then
       Nothing
    else
       case iUnbox (fromInteger x_) of { x ->
       if (x `bitAndFastInt` negateFastInt x) /=# x then
          Nothing
       else
          Just (toInteger (iBox (pow2 x)))
       }
  where
    pow2 x | x ==# _ILIT(1) = _ILIT(0)
           | otherwise = _ILIT(1) +# pow2 (x `shiftR_FastInt` _ILIT(1))

-- -----------------------------------------------------------------------------
-- Utils

isLit :: CmmExpr -> Bool
isLit (CmmLit _) = True
isLit _          = False

isComparisonExpr :: CmmExpr -> Bool
isComparisonExpr (CmmMachOp op _) = isComparisonMachOp op
isComparisonExpr _                  = False

isPicReg :: CmmExpr -> Bool
isPicReg (CmmReg (CmmGlobal PicBaseReg)) = True
isPicReg _ = False
