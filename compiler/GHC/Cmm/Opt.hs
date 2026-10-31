-----------------------------------------------------------------------------
--
-- Cmm optimisation
--
-- (c) The University of Glasgow 2006
--
-----------------------------------------------------------------------------
module GHC.Cmm.Opt (
        constantFoldNode,
        constantFoldExpr,
        cmmMachOpFold,
        cmmMachOpFoldM
 ) where

import GHC.Prelude

import GHC.Cmm.Utils
import GHC.Cmm
import GHC.Utils.Misc

import GHC.Utils.Panic
import GHC.Utils.Outputable
import GHC.Platform
import GHC.Types.Literal.Floating

import Data.Maybe
import Control.Monad (zipWithM, guard)
import GHC.Float


constantFoldNode :: Platform -> CmmNode e x -> CmmNode e x
constantFoldNode platform = mapExp (constantFoldExpr platform)

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
cmmMachOpFoldM _ (MO_V_Broadcast lg _w) exprs =
  case exprs of
    [CmmLit l] -> Just $! CmmLit (CmmVec $ replicate lg l)
    _ -> Nothing
cmmMachOpFoldM _ (MO_VF_Broadcast lg _w) exprs =
  case exprs of
    [CmmLit l] -> Just $! CmmLit (CmmVec $ replicate lg l)
    _ -> Nothing

cmmMachOpFoldM plat (MO_V_Extract l _)  [v, (CmmLit (CmmInt idx W32))]
  | idx >= 0, idx < fromIntegral l
  = do
    es <- vectorElements_maybe plat v
    es !! fromInteger idx

cmmMachOpFoldM plat (MO_VF_Extract l _) [v, (CmmLit (CmmInt idx W32))]
  | idx >= 0, idx < fromIntegral l
  = do
    es <- vectorElements_maybe plat v
    es !! fromInteger idx

cmmMachOpFoldM plat op [v, newval@(CmmLit _), CmmLit (CmmInt idx W32)]
  | MO_V_Insert  l _ <- op = foldToVecLit l
  | MO_VF_Insert l _ <- op = foldToVecLit l
  where foldToVecLit l = do
          guard (idx >= 0 && idx < fromIntegral l)
          ls <- vectorElements_maybe plat v
          lits <- sequence $ map toLit_maybe (replaceAt (fromIntegral idx) (Just newval) ls)
          Just $! CmmLit (CmmVec lits)
        toLit_maybe (Just (CmmLit l)) = Just l
        toLit_maybe _ = Nothing

cmmMachOpFoldM _ op [CmmLit (CmmInt x rep)]
  | MO_WF_Bitcast width <- op = case width of
      W32 | res <- castWord32ToFloat (fromInteger x)
          -> Just (CmmLit (CmmFloat (floatToLitFloating res) LitFloat))
      W64 | res <- castWord64ToDouble (fromInteger x)
          -> Just (CmmLit (CmmFloat (doubleToLitFloating res) LitDouble))
      _ -> Nothing
  | otherwise
  = Just $! case op of
      MO_S_Neg _ -> CmmLit (CmmInt (narrowS rep (-x)) rep)
      MO_Not _   -> CmmLit (CmmInt (complement x) rep)

      -- these are interesting: we must first narrow to the
      -- "from" type, in order to truncate to the correct size.
      -- The final narrow/widen to the destination type
      -- is implicit in the CmmLit.
      MO_SS_Conv  from to -> CmmLit (CmmInt (narrowS from x) to)
      MO_UU_Conv  from to -> CmmLit (CmmInt (narrowU from x) to)
      MO_XX_Conv  from to -> CmmLit (CmmInt (narrowS from x) to)

      MO_SF_Round from to ->
        -- No ExcessPrecision for Cmm constant folding, so narrow appropriately.
        let (f, fty) = case to of
                  W32 -> ( floatToLitFloating (fromInteger $ narrowS from x), LitFloat)
                  W64 -> (doubleToLitFloating (fromInteger $ narrowS from x), LitDouble)
                  _ -> pprPanic "MO_SF_Round" (ppr to)
        in CmmLit (CmmFloat f fty)

      MO_F_Neg{}          -> invalidArgPanic
      MO_FS_Truncate{}    -> invalidArgPanic
      MO_FF_Conv{}        -> invalidArgPanic
      MO_FW_Bitcast{}     -> invalidArgPanic
      MO_VS_Neg{}         -> invalidArgPanic
      MO_VF_Neg{}         -> invalidArgPanic
      MO_RelaxedRead{}    -> invalidArgPanic
      MO_AlignmentCheck{} -> invalidArgPanic

      _ -> panic $ "cmmMachOpFoldM: unknown unary op: " ++ show op
      where invalidArgPanic = pprPanic "cmmMachOpFoldM" $
              text "Found" <+> pprMachOp op
                <+> text "illegally applied to an int literal"

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

        MO_Add r -> Just $! CmmLit (CmmInt (narrowU r $ x + y) r)
        MO_Sub r -> Just $! CmmLit (CmmInt (narrowS r $ x - y) r)
        MO_Mul r -> Just $! CmmLit (CmmInt (narrowU r $ x * y) r)
        MO_U_Quot r | y /= 0 -> Just $! CmmLit (CmmInt (x_u `quot` y_u) r)
        MO_U_Rem  r | y /= 0 -> Just $! CmmLit (CmmInt (x_u `rem`  y_u) r)
        MO_S_Quot r | y /= 0 -> Just $! CmmLit (CmmInt (x_s `quot` y_s) r)
        MO_S_Rem  r | y /= 0 -> Just $! CmmLit (CmmInt (x_s `rem`  y_s) r)

        MO_And   r -> Just $! CmmLit (CmmInt (x .&. y) r)
        MO_Or    r -> Just $! CmmLit (CmmInt (x .|. y) r)
        MO_Xor   r -> Just $! CmmLit (CmmInt (x `xor` y) r)

        MO_Shl   r -> Just $! CmmLit (CmmInt (narrowU r $ x   `shiftL` fromIntegral y) r)
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
-- An addition with a literal c is left alone: its literal stays last, as
-- Note [Additive normal form] puts it.  An addition is reassociated by
-- reassocAdd, which builds the steps that fold nothing as they stand.
--
cmmMachOpFoldM platform mop1 [CmmMachOp mop2 [arg1,arg2], arg3]
   | mop1 == mop2 && isAssociativeMachOp mop1
     && not (isLit arg1) && not (isPicReg arg1)
     && not (isAdd mop1 && isLit arg3)
   = Just $! case mop1 of
       MO_Add w -> reassocAdd platform mop1 w arg1 arg2 arg3
       _        -> cmmMachOpFold platform mop2 [arg1, cmmMachOpFold platform mop1 [arg2,arg3]]
   where
     isAdd MO_Add{} = True
     isAdd _        = False

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

-- Additions and subtractions: see Note [Additive normal form].
cmmMachOpFoldM platform (MO_Add w) [x, y]
   | Just e <- addNormalise platform w False x y
   = Just $! e
cmmMachOpFoldM platform (MO_Sub w) [x, y]
   | Just e <- addNormalise platform w True x y
   = Just $! e


-- Comparison of literal with widened operand: perform the comparison
-- at the smaller width, as long as the literal is within range.

-- We can't do the reverse trick, when the operand is narrowed:
-- narrowing throws away bits from the operand, there's no way to do
-- the same comparison at the larger size.

cmmMachOpFoldM platform cmp [CmmMachOp conv [x], CmmLit (CmmInt i _)]
  |     -- if the operand is widened:
    Just (rep, signed, narrow_fn) <- maybe_conversion conv,
        -- and this is a comparison operation:
    Just narrow_cmp <- maybe_comparison cmp rep signed,
        -- and the literal fits in the smaller size:
    i == narrow_fn rep i
        -- then we can do the comparison at the smaller size
  = Just $! (cmmMachOpFold platform narrow_cmp [x, CmmLit (CmmInt i rep)])

-- Comparison of two operands widened alike, from the same width and both
-- signed or both unsigned: perform the comparison at the smaller width.
-- A widening keeps its operands' equality and unsigned order, and a sign
-- extension their signed order too, so a signed comparison of zero-extended
-- operands becomes an unsigned one, as with a literal (#2080).

cmmMachOpFoldM platform cmp [CmmMachOp conv1 [x], CmmMachOp conv2 [y]]
  | conv1 == conv2,
    Just (rep, signed, _) <- maybe_conversion conv1,
    Just narrow_cmp <- maybe_comparison cmp rep signed
  = Just $! (cmmMachOpFold platform narrow_cmp [x, y])

-- We can often do something with constants of 0 and 1 ...
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
--
-- Naively this is as simple a matter as left/right bit shifts,
-- but the Cmm representation if integral values quickly complicated the matter.
--
-- We must carefully narrow the value to be within the range of values for the
-- type's logical bit-width. However, Cmm only represents values as *signed*
-- integers internally yet the logical type may be unsigned. If we are dealing
-- with a negative integer type at width @_w@, the only negative number that
-- wraps around to be a positive power of 2 after calling narrowU is -2^(_w - 1)
-- which wraps round to 2^(_w - 1), and multiplying by -2^(_w - 1) is indeed
-- the same as a left shift by (w - 1), so this is OK.
--
-- ToDo: See #25664 (comment 605821) describing a change to the Cmm literal representation.
-- When/If this is completed, this code must be refactored to account for the explicit width sizes.
cmmMachOpFoldM platform mop [x, (CmmLit (CmmInt n _w))]
  = case mop of
        MO_Mul rep
           | Just p <- exactLog2 (narrowU rep n) ->
                 Just $! (cmmMachOpFold platform (MO_Shl rep) [x, CmmLit (CmmInt p $ wordWidth platform)])
        MO_U_Quot rep
           | Just p <- exactLog2 (narrowU rep n) ->
                 Just $! (cmmMachOpFold platform (MO_U_Shr rep) [x, CmmLit (CmmInt p $ wordWidth platform)])
        MO_U_Rem rep
           | Just _ <- exactLog2 (narrowU rep n)  ->
                 Just $! (cmmMachOpFold platform (MO_And rep) [x, CmmLit (CmmInt (n - 1) rep)])
        MO_S_Quot rep
           | Just p <- exactLog2 (narrowS rep n),
             CmmReg _ <- x ->   -- We duplicate x in signedQuotRemHelper, hence require
                                -- it is a reg.  FIXME: remove this restriction.
                Just $! (cmmMachOpFold platform (MO_S_Shr rep)
                  [signedQuotRemHelper rep p, CmmLit (CmmInt p $ wordWidth platform)])
        MO_S_Rem rep
           | Just p <- exactLog2 (narrowS rep n),
             CmmReg _ <- x ->   -- We duplicate x in signedQuotRemHelper, hence require
                                -- it is a reg.  FIXME: remove this restriction.
                -- We replace (x `rem` 2^p) by (x - (x `quot` 2^p) * 2^p).
                -- Moreover, we fuse MO_S_Shr (last operation of MO_S_Quot)
                -- and MO_S_Shl (multiplication by 2^p) into a single MO_And operation.
                Just $! (cmmMachOpFold platform (MO_Sub rep)
                    [x, cmmMachOpFold platform (MO_And rep)
                      [signedQuotRemHelper rep p, CmmLit (CmmInt (- n) rep)]])
        _ -> Nothing
  where
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
    -- we optimise the divide by 2 case slightly, generating
    --      x1 = x >> word_size-1  (unsigned)
    --      return = x + x1
    signedQuotRemHelper :: Width -> Integer -> CmmExpr
    signedQuotRemHelper rep p = CmmMachOp (MO_Add rep) [x, x2]
      where
        bits = fromIntegral (widthInBits rep) - 1
        shr = if p == 1 then MO_U_Shr rep else MO_S_Shr rep
        x1 = CmmMachOp shr [x, CmmLit (CmmInt bits $ wordWidth platform)]
        x2 = if p == 1 then x1 else
             CmmMachOp (MO_And rep) [x1, CmmLit (CmmInt (n-1) rep)]

-- Many vector MachOps are simply element-wise scalar MachOps. For these, we reduce
-- to the scalar case using 'vectorMachOpScalarMachOp_maybe' and 'vectorElements_maybe'.

-- Unary vector MachOps.
cmmMachOpFoldM plat op [v]
  | Just scalar_op <- vectorMachOpToScalarMachOp_maybe op
  = do es <- vectorElements_maybe plat v
       ls <- mapM (foldToLit plat scalar_op) es
       Just $! CmmLit $ CmmVec ls

  where foldToLit plat mop (Just a) = do
          CmmLit l <- cmmMachOpFoldM plat mop [a]
          return l
        foldToLit _ _ _ = Nothing

-- Binary vector MachOps.
cmmMachOpFoldM plat op [v1, v2]
  | Just scalar_op <- vectorMachOpToScalarMachOp_maybe op
  = do
      es1 <- vectorElements_maybe plat v1
      es2 <- vectorElements_maybe plat v2
      ls <- zipWithM (foldToLit plat scalar_op) es1 es2
      Just $! CmmLit $ CmmVec ls
  -- MIN/MAX don't have scalar equivalents, so handle them manually.
  | MO_VS_Max _ w <- op = do
      es1 <- vectorElements_maybe plat v1
      es2 <- vectorElements_maybe plat v2
      ls <- zipWithM (foldOp (narrowS w) max) es1 es2
      Just $! CmmLit $ CmmVec ls
  | MO_VU_Max _ w <- op = do
      es1 <- vectorElements_maybe plat v1
      es2 <- vectorElements_maybe plat v2
      ls <- zipWithM (foldOp (narrowU w) max) es1 es2
      Just $! CmmLit $ CmmVec ls
  | MO_VS_Min _ w <- op = do
      es1 <- vectorElements_maybe plat v1
      es2 <- vectorElements_maybe plat v2
      ls <- zipWithM (foldOp (narrowS w) min) es1 es2
      Just $! CmmLit $ CmmVec ls
  | MO_VU_Min _ w <- op = do
      es1 <- vectorElements_maybe plat v1
      es2 <- vectorElements_maybe plat v2
      ls <- zipWithM (foldOp (narrowU w) min) es1 es2
      Just $! CmmLit $ CmmVec ls

  where
    foldToLit plat mop (Just a1) (Just a2) = do
      CmmLit l <- cmmMachOpFoldM plat mop [a1, a2]
      return l
    foldToLit _ _ _ _  = Nothing

    foldOp do_narrow op
      (Just (CmmLit (CmmInt x rep)))
      (Just (CmmLit (CmmInt y _)))
        = Just $! CmmInt (do_narrow x `op` do_narrow y) rep
    foldOp _ _ _ _ = Nothing


-- ToDo (#7116): optimise floating-point multiplication, e.g. x*2.0 -> x+x
-- Unfortunately this needs a unique supply because x might not be a
-- register.  See #2253 (program 6) for an example.


-- Anything else is just too hard.

cmmMachOpFoldM _ _ _ = Nothing

{- Note [Additive normal form]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
An addition or subtraction at width w is put into one of these forms, its
literals summed into one constant k /= 0:

    e + k   e having no literal among the operands of its additions and
            subtractions at w, a CmmRegOff counting as an addition; a CmmReg
            r for e makes it the CmmRegOff r k
    k - e   where every other term is subtracted: -e + k would cost x86
            a negation
    e       where the literals cancel

k is an integer literal, a sum built here adding it even when negative, or
a label literal with its offset; e - k is left as it is where e has no
constant, the x86 backend emitting as many instructions for it as for
e + (-k).  So (a - b) + 1, (a + 1) - b and a - (b - 1) all become
(a - b) + 1, a + (b + 1) and (a + 1) + b become (a + b) + 1, and (a + 1) - 2
becomes a + (-1).  Cmm's integer arithmetic wraps at its width, so that any
rearrangement of the terms is an identity of the integers modulo 2^w; the
constant is summed in Integer and narrowed once.  Each step looks only at the
top of its two operands, which folding bottom-up has already put in this form.
A label added to PicBaseReg stays with it (see the special case below), and
a sum with two labels, or a label subtracted, is left alone.

This replaces the reassociations (a + b) - c ==> a + (b - c) and
(a - b) + c ==> a + (c - b), which put a literal b on the left of
a subtraction or folded nothing: (x * y + 1) - w became x * y + (1 - w),
(i - s) + (-1) became i + ((-1) - s), x86 paying an instruction for either;
and the reassociation (a + b) + c ==> a + (b + c) no longer moves a literal c
from the top.
-}

-- | An operand of an addition or subtraction at some width: no term, a term
-- added or a term subtracted, ...
data AddTerm = NoTerm | Plus !CmmExpr | Minus !CmmExpr

-- | ... and the constant added: an integer, or a label literal with its offset.
data AddConst = KInt !Integer | KLab !CmmLit

-- | What addView finds in an operand: a term alone, a constant alone, or both.
data AddShape = TermOnly | ConstOnly | TermConst

addShape :: Width -> CmmExpr -> AddShape
{-# INLINE addShape #-}
addShape w e = case addView w e of
  Nothing             -> TermOnly
  Just (NoTerm, _)    -> ConstOnly
  Just _              -> TermConst

-- | The term and the constant of an operand at width w in the additive normal
-- form, or Nothing where the operand is a term alone.
addView :: Width -> CmmExpr -> Maybe (AddTerm, AddConst)
addView w e = case e of
  CmmLit (CmmInt i _)                     -> Just (NoTerm, KInt i)
  CmmLit l | isLabelLit l                 -> Just (NoTerm, KLab l)
  CmmMachOp (MO_Add w') [a, CmmLit l] | w' == w -> case l of
    CmmInt i _                            -> Just (Plus a, KInt i)
    _ | isLabelLit l, not (isPicReg a)    -> Just (Plus a, KLab l)
    _                                     -> Nothing
  CmmMachOp (MO_Sub w') [a, CmmLit (CmmInt i _)]
    | w' == w                             -> Just (Plus a, KInt (negate i))
  CmmMachOp (MO_Sub w') [CmmLit (CmmInt i _), a]
    | w' == w                             -> Just (Minus a, KInt i)
  CmmRegOff r o
    | typeWidth (cmmRegType r) == w       -> Just (Plus (CmmReg r), KInt (toInteger o))
  _                                       -> Nothing

isLabelLit :: CmmLit -> Bool
isLabelLit CmmLabel{}    = True
isLabelLit CmmLabelOff{} = True
isLabelLit _             = False

-- | x + y (or x - y, if the flag is set) at width w in the additive normal
-- form, given x and y in it; Nothing where it is that already, or where it
-- cannot be (two labels, or a label subtracted).
addNormalise :: Platform -> Width -> Bool -> CmmExpr -> CmmExpr -> Maybe CmmExpr
addNormalise platform w sub x y = case (addShape w x, addShape w y) of
  (TermOnly, TermOnly)              -> Nothing  -- no literal
  (TermOnly, ConstOnly)             -> Nothing  -- e + k, e - k
  (ConstOnly, TermOnly)
    | sub                           -> Nothing  -- k - e
  _ -> do
    let !(tx, kx) = fromMaybe (Plus x, KInt 0) (addView w x)
        !(ty, ky) = fromMaybe (Plus y, KInt 0) (addView w y)
    k <- if sub then constSub kx ky else constAdd kx ky
    Just $! build (combine tx ty) k
  where
    combine NoTerm    t         = if sub then negT t else t
    combine t         NoTerm    = t
    combine (Plus a)  (Plus b)  = Plus (op2 sub a b)
    combine (Plus a)  (Minus b) = Plus (op2 (not sub) a b)
    combine (Minus a) (Plus b)
      | sub                     = Minus (op2 False a b)
      | otherwise               = Plus (op2 True b a)
    combine (Minus a) (Minus b)
      | sub                     = Plus (op2 True b a)
      | otherwise               = Minus (op2 False a b)

    negT NoTerm    = NoTerm
    negT (Plus a)  = Minus a
    negT (Minus a) = Plus a

    op2 s a b = cmmMachOpFold platform (if s then MO_Sub w else MO_Add w) [a, b]

    constAdd (KInt a) (KInt b) = Just (KInt (a + b))
    constAdd (KLab l) (KInt b) = labOff l b
    constAdd (KInt a) (KLab l) = labOff l a
    constAdd _        _        = Nothing
    constSub k (KInt b) = constAdd k (KInt (negate b))
    constSub _ _        = Nothing
    labOff l i
      | validOffsetRep w = Just (KLab (cmmOffsetLit l (fromInteger (narrowS w i))))
      | otherwise        = Nothing

    build NoTerm    (KInt i) = CmmLit (CmmInt (narrowS w i) w)
    build NoTerm    (KLab l) = CmmLit l
    -- e + k folds further only where e is a literal, a register, a register
    -- offset, a sum (with PicBaseReg, or not in this form) or a difference
    -- with a constant; else it is built as it stands
    build (Plus e)  (KInt i)
      | k == 0               = e
      | CmmReg r <- e
      , validOffsetRep w     = cmmRegOff r (fromInteger k)
      | folds e              = cmmMachOpFold platform (MO_Add w) [e, CmmLit (CmmInt k w)]
      | otherwise            = CmmMachOp (MO_Add w) [e, CmmLit (CmmInt k w)]
      where k = narrowS w i
            folds CmmLit{}               = True
            folds CmmReg{}               = True
            folds CmmRegOff{}            = True
            folds (CmmMachOp MO_Add{} _) = True
            folds e'                     = case addShape w e' of
                                             TermOnly -> False
                                             _        -> True
    build (Plus e)  (KLab l) = CmmMachOp (MO_Add w) [e, CmmLit l]
    build (Minus e) (KInt i) = CmmMachOp (MO_Sub w) [CmmLit (CmmInt (narrowS w i) w), e]
    build (Minus e) (KLab l) = CmmMachOp (MO_Sub w) [CmmLit l, e]

-- | The reassociation (a + b) + c ==> a + (b + c) of the addition op at width
-- w, a being neither a literal nor PicBaseReg and c no literal: cmmMachOpFold
-- of a + cmmMachOpFold (b + c), the inner fold reassociating again where b is
-- such a sum.  Where a is neither a literal, a register offset nor a MachOp
-- and the inner result is a sum at w whose right operand is no literal,
-- neither operand has a constant (Note [Additive normal form]) and no other
-- clause matches, so that the fold returns the sum as it stands: it is built
-- so, and is then known to be such a sum one step up.  Folded, each of
-- the n(n-1)/2 steps that turn a left-nested sum of n terms right-nested
-- tested both of its operands for a constant.
reassocAdd :: Platform -> MachOp -> Width -> CmmExpr -> CmmExpr -> CmmExpr
           -> CmmExpr
reassocAdd platform op w a0 b0 c = case go a0 b0 of Reassoc e _ -> e
  where
    go a b = case inner of
        Reassoc bc True | plain a -> Reassoc (CmmMachOp op [a, bc]) True
        Reassoc bc _              -> folded (cmmMachOpFold platform op [a, bc])
      where
        inner = case b of
          CmmMachOp (MO_Add w') [p, q]
            | w' == w, not (isLit p), not (isPicReg p) -> go p q
          _ -> folded (cmmMachOpFold platform op [b, c])
    -- e, and whether it is a sum at w whose right operand is no literal
    folded e = Reassoc e $ case e of
      CmmMachOp (MO_Add w') [_, y] -> w' == w && not (isLit y)
      _                            -> False
    plain CmmLit{}    = False
    plain CmmRegOff{} = False
    plain CmmMachOp{} = False
    plain _           = True

-- | A result of reassocAdd's steps, and whether it is a sum at its width whose
-- right operand is no literal.
data Reassoc = Reassoc !CmmExpr !Bool

-- For a widening conversion, which the comparison folds above remove: its
-- source width, whether it extends the sign, and how a literal is narrowed to
-- that width.
maybe_conversion :: MachOp -> Maybe (Width, Bool, Width -> Integer -> Integer)
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
maybe_comparison :: MachOp -> Width -> Bool -> Maybe MachOp
maybe_comparison (MO_U_Gt _) rep _     = Just (MO_U_Gt rep)
maybe_comparison (MO_U_Ge _) rep _     = Just (MO_U_Ge rep)
maybe_comparison (MO_U_Lt _) rep _     = Just (MO_U_Lt rep)
maybe_comparison (MO_U_Le _) rep _     = Just (MO_U_Le rep)
maybe_comparison (MO_Eq   _) rep _     = Just (MO_Eq   rep)
maybe_comparison (MO_Ne   _) rep _     = Just (MO_Ne   rep)
maybe_comparison (MO_S_Gt _) rep True  = Just (MO_S_Gt rep)
maybe_comparison (MO_S_Ge _) rep True  = Just (MO_S_Ge rep)
maybe_comparison (MO_S_Lt _) rep True  = Just (MO_S_Lt rep)
maybe_comparison (MO_S_Le _) rep True  = Just (MO_S_Le rep)
maybe_comparison (MO_S_Gt _) rep False = Just (MO_U_Gt rep)
maybe_comparison (MO_S_Ge _) rep False = Just (MO_U_Ge rep)
maybe_comparison (MO_S_Lt _) rep False = Just (MO_U_Lt rep)
maybe_comparison (MO_S_Le _) rep False = Just (MO_U_Le rep)
maybe_comparison _ _ _ = Nothing

-- | Check that a literal width is compatible with the host word size used to
-- store offsets. This should be fixed properly (using larger types to store
-- literal offsets). See #24893
validOffsetRep :: Width -> Bool
validOffsetRep rep = widthInBits rep <= finiteBitSize (undefined :: Int)


-- Is this a vector 'MachOp' that is an element-wise lift of
-- a scalar 'MachOp'? If so, returns the corresponding scalar 'MachOp'.
vectorMachOpToScalarMachOp_maybe :: MachOp -> Maybe MachOp
vectorMachOpToScalarMachOp_maybe m = case m of
  MO_VS_Neg _ w -> Just $ MO_S_Neg w
  MO_VF_Neg _ w -> Just $ MO_F_Neg w
  MO_V_Add  _ w -> Just $ MO_Add w
  MO_V_Sub  _ w -> Just $ MO_Sub w
  MO_V_Mul  _ w -> Just $ MO_Mul w
  MO_VF_Add _ w -> Just $ MO_F_Add w
  MO_VF_Sub _ w -> Just $ MO_F_Sub w
  MO_VF_Mul _ w -> Just $ MO_F_Mul w
  MO_VF_Min _ w -> Just $ MO_F_Min w
  MO_VF_Max _ w -> Just $ MO_F_Max w
  MO_V_And  _ w -> Just $ MO_And w
  MO_V_Or   _ w -> Just $ MO_Or w
  MO_V_Xor  _ w -> Just $ MO_Xor w
  _ -> Nothing


-- | Helper function that tells us what we know about the elements of a vector.
--
-- Returns 'Nothing' for non-vectors, and @[Nothing, Nothing, ...]@ for vectors
-- with unknown elements.
vectorElements_maybe :: Platform -> CmmExpr -> Maybe [Maybe CmmExpr]
vectorElements_maybe _plat (CmmLit (CmmVec es)) = Just $! map (Just . CmmLit) es

vectorElements_maybe _plat (CmmMachOp (MO_V_Broadcast l _) args)
  | [CmmLit v] <- args = Just $! replicate l (Just $! CmmLit v)
vectorElements_maybe _plat (CmmMachOp (MO_VF_Broadcast l _) args)
  | [CmmLit v] <- args = Just $! replicate l (Just $! CmmLit v)

vectorElements_maybe plat (CmmMachOp (MO_V_Insert _ _) args)
  | [v, e, (CmmLit (CmmInt i _w))] <- args
  , Just es <- vectorElements_maybe plat v
      = Just $! (replaceAt (fromInteger i) (Just $! e) es)

vectorElements_maybe plat (CmmMachOp (MO_VF_Insert _ _) args)
  | [v, e, (CmmLit (CmmInt i _w))] <- args
  , Just es <- vectorElements_maybe plat v
    = Just $! (replaceAt (fromInteger i) (Just $! e) es)

vectorElements_maybe plat (CmmMachOp mop _)
  | isVecType result_type = Just $! replicate (vecLength result_type) Nothing
  where result_type = machOpResultType plat mop []

vectorElements_maybe _plat (CmmReg reg)
  | isVecType reg_type = Just $! replicate (vecLength reg_type) Nothing
  where reg_type = cmmRegType reg

vectorElements_maybe _ _ = Nothing


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
