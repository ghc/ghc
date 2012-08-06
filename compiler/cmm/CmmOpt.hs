-----------------------------------------------------------------------------
--
-- Cmm optimisation
--
-- (c) The University of Glasgow 2006
--
-----------------------------------------------------------------------------

module CmmOpt (
        cmmEliminateDeadBlocks,
        cmmMiniInline,
        cmmMachOpFold,
        cmmMachOpFoldM,
        cmmLoopifyForC,
 ) where

#include "HsVersions.h"

import OldCmm
import OldPprCmm
import CmmNode (wrapRecExp)
import CmmUtils
import DynFlags
import CLabel

import UniqFM
import Unique
import Util
import FastTypes
import Outputable
import Platform
import BlockId

import Data.Bits
import Data.Maybe
import Data.List

-- -----------------------------------------------------------------------------
-- Eliminates dead blocks

{-
We repeatedly expand the set of reachable blocks until we hit a
fixpoint, and then prune any blocks that were not in this set.  This is
actually a required optimization, as dead blocks can cause problems
for invariants in the linear register allocator (and possibly other
places.)
-}

-- Deep fold over statements could probably be abstracted out, but it
-- might not be worth the effort since OldCmm is moribund
cmmEliminateDeadBlocks :: [CmmBasicBlock] -> [CmmBasicBlock]
cmmEliminateDeadBlocks [] = []
cmmEliminateDeadBlocks blocks@(BasicBlock base_id _:_) =
    let -- Calculate what's reachable from what block
        reachableMap = foldl' f emptyUFM blocks -- lazy in values
            where f m (BasicBlock block_id stmts) = addToUFM m block_id (reachableFrom stmts)
        reachableFrom stmts = foldl stmt [] stmts
            where
                stmt m CmmNop = m
                stmt m (CmmComment _) = m
                stmt m (CmmAssign _ e) = expr m e
                stmt m (CmmStore e1 e2) = expr (expr m e1) e2
                stmt m (CmmCall c _ as _) = f (actuals m as) c
                    where f m (CmmCallee e _) = expr m e
                          f m (CmmPrim _ Nothing) = m
                          f m (CmmPrim _ (Just stmts)) = foldl' stmt m stmts
                stmt m (CmmBranch b) = b:m
                stmt m (CmmCondBranch e b) = b:(expr m e)
                stmt m (CmmSwitch e bs) = catMaybes bs ++ expr m e
                stmt m (CmmJump e _) = expr m e
                stmt m (CmmReturn) = m
                actuals m as = foldl' (\m h -> expr m (hintlessCmm h)) m as
                -- We have to do a deep fold into CmmExpr because
                -- there may be a BlockId in the CmmBlock literal.
                expr m (CmmLit l) = lit m l
                expr m (CmmLoad e _) = expr m e
                expr m (CmmReg _) = m
                expr m (CmmMachOp _ es) = foldl' expr m es
                expr m (CmmStackSlot _ _) = m
                expr m (CmmRegOff _ _) = m
                lit m (CmmBlock b) = b:m
                lit m _ = m
        -- go todo done
        reachable = go [base_id] (setEmpty :: BlockSet)
          where go []     m = m
                go (x:xs) m
                    | setMember x m = go xs m
                    | otherwise     = go (add ++ xs) (setInsert x m)
                        where add = fromMaybe (panic "cmmEliminateDeadBlocks: unknown block")
                                              (lookupUFM reachableMap x)
    in filter (\(BasicBlock block_id _) -> setMember block_id reachable) blocks

-- -----------------------------------------------------------------------------
-- The mini-inliner

{-
This pass inlines assignments to temporaries.  Temporaries that are
only used once are unconditionally inlined.  Temporaries that are used
two or more times are only inlined if they are assigned a literal.  It
works as follows:

  - count uses of each temporary
  - for each temporary:
        - attempt to push it forward to the statement that uses it
        - only push forward past assignments to other temporaries
          (assumes that temporaries are single-assignment)
        - if we reach the statement that uses it, inline the rhs
          and delete the original assignment.

[N.B. In the Quick C-- compiler, this optimization is achieved by a
 combination of two dataflow passes: forward substitution (peephole
 optimization) and dead-assignment elimination.  ---NR]

Possible generalisations: here is an example from factorial

Fac_zdwfac_entry:
    cmG:
        _smi = R2;
        if (_smi != 0) goto cmK;
        R1 = R3;
        jump I64[Sp];
    cmK:
        _smn = _smi * R3;
        R2 = _smi + (-1);
        R3 = _smn;
        jump Fac_zdwfac_info;

We want to inline _smi and _smn.  To inline _smn:

   - we must be able to push forward past assignments to global regs.
     We can do this if the rhs of the assignment we are pushing
     forward doesn't refer to the global reg being assigned to; easy
     to test.

To inline _smi:

   - It is a trivial replacement, reg for reg, but it occurs more than
     once.
   - We can inline trivial assignments even if the temporary occurs
     more than once, as long as we don't eliminate the original assignment
     (this doesn't help much on its own).
   - We need to be able to propagate the assignment forward through jumps;
     if we did this, we would find that it can be inlined safely in all
     its occurrences.
-}

countUses :: UserOfLocalRegs a => a -> UniqFM Int
countUses a = foldRegsUsed (\m r -> addToUFM_C (+) m r 1) emptyUFM a

cmmMiniInline :: DynFlags -> [CmmBasicBlock] -> [CmmBasicBlock]
cmmMiniInline dflags blocks = map do_inline blocks
  where do_inline (BasicBlock id stmts)
          = BasicBlock id (cmmMiniInlineStmts dflags (countUses blocks) stmts)

cmmMiniInlineStmts :: DynFlags -> UniqFM Int -> [CmmStmt] -> [CmmStmt]
cmmMiniInlineStmts _      _    [] = []
cmmMiniInlineStmts dflags uses (stmt@(CmmAssign (CmmLocal (LocalReg u _)) expr) : stmts)
        -- not used: just discard this assignment
  | 0 <- lookupWithDefaultUFM uses 0 u
  = cmmMiniInlineStmts dflags uses stmts

        -- used (foldable to small thing): try to inline at all the use sites
  | Just n <- lookupUFM uses u,
    e <- wrapRecExp foldExp expr,
    isTiny e
  =
     ncgDebugTrace ("nativeGen: inlining " ++ showSDoc dflags (pprStmt stmt)) $
     case lookForInlineMany u e stmts of
         (m, stmts')
             | n == m -> cmmMiniInlineStmts dflags (delFromUFM uses u) stmts'
             | otherwise ->
                 stmt : cmmMiniInlineStmts dflags (adjustUFM (\x -> x - m) uses u) stmts'

        -- used once (non-literal): try to inline at the use site
  | Just 1 <- lookupUFM uses u,
    Just stmts' <- lookForInline u expr stmts
  = 
     ncgDebugTrace ("nativeGen: inlining " ++ showSDoc dflags (pprStmt stmt)) $
     cmmMiniInlineStmts dflags uses stmts'
 where
  isTiny (CmmLit _) = True
  isTiny (CmmReg (CmmGlobal _)) = True
         -- not CmmLocal: that might invalidate the usage analysis results
  isTiny _ = False

  platform = targetPlatform dflags
  foldExp (CmmMachOp op args) = cmmMachOpFold platform op args
  foldExp e = e

  ncgDebugTrace str x = if ncgDebugIsOn then trace str x else x

cmmMiniInlineStmts platform uses (stmt:stmts)
  = stmt : cmmMiniInlineStmts platform uses stmts

-- | Takes a register, a 'CmmLit' expression assigned to that
-- register, and a list of statements.  Inlines the expression at all
-- use sites of the register.  Returns the number of substituations
-- made and the, possibly modified, list of statements.
lookForInlineMany :: Unique -> CmmExpr -> [CmmStmt] -> (Int, [CmmStmt])
lookForInlineMany u expr stmts = lookForInlineMany' u expr regset stmts
    where regset = foldRegsUsed extendRegSet emptyRegSet expr

lookForInlineMany' :: Unique -> CmmExpr -> RegSet -> [CmmStmt] -> (Int, [CmmStmt])
lookForInlineMany' _ _ _ [] = (0, [])
lookForInlineMany' u expr regset stmts@(stmt : rest)
  | Just n <- lookupUFM (countUses stmt) u, okToInline expr stmt
  = let stmt' = inlineStmt u expr stmt in
    if okToSkip stmt' u expr regset
       then case lookForInlineMany' u expr regset rest of
                       (m, stmts) -> let z = n + m
                                     in z `seq` (z, stmt' : stmts)
       else (n, stmt' : rest)

  | okToSkip stmt u expr regset
  = case lookForInlineMany' u expr regset rest of
      (n, stmts) -> (n, stmt : stmts)

  | otherwise
  = (0, stmts)


lookForInline :: Unique -> CmmExpr -> [CmmStmt] -> Maybe [CmmStmt]
lookForInline u expr stmts = lookForInline' u expr regset stmts
    where regset = foldRegsUsed extendRegSet emptyRegSet expr

lookForInline' :: Unique -> CmmExpr -> RegSet -> [CmmStmt] -> Maybe [CmmStmt]
lookForInline' _ _    _      [] = panic "lookForInline' []"
lookForInline' u expr regset (stmt : rest)
  | Just 1 <- lookupUFM (countUses stmt) u, okToInline expr stmt
  = Just (inlineStmt u expr stmt : rest)

  | okToSkip stmt u expr regset
  = case lookForInline' u expr regset rest of
           Nothing    -> Nothing
           Just stmts -> Just (stmt:stmts)

  | otherwise 
  = Nothing


-- we don't inline into CmmCall if the expression refers to global
-- registers.  This is a HACK to avoid global registers clashing with
-- C argument-passing registers, really the back-end ought to be able
-- to handle it properly, but currently neither PprC nor the NCG can
-- do it.  See also CgForeignCall:load_args_into_temps.
okToInline :: CmmExpr -> CmmStmt -> Bool
okToInline expr CmmCall{} = hasNoGlobalRegs expr
okToInline _ _ = True

-- Expressions aren't side-effecting.  Temporaries may or may not
-- be single-assignment depending on the source (the old code
-- generator creates single-assignment code, but hand-written Cmm
-- and Cmm from the new code generator is not single-assignment.)
-- So we do an extra check to make sure that the register being
-- changed is not one we were relying on.  I don't know how much of a
-- performance hit this is (we have to create a regset for every
-- instruction.) -- EZY
okToSkip :: CmmStmt -> Unique -> CmmExpr -> RegSet -> Bool
okToSkip stmt u expr regset
   = case stmt of
         CmmNop -> True
         CmmComment{} -> True
         CmmAssign (CmmLocal r@(LocalReg u' _)) _rhs | u' /= u && not (r `elemRegSet` regset) -> True
         CmmAssign g@(CmmGlobal _) _rhs -> not (g `regUsedIn` expr)
         CmmStore _ _ -> not_a_load expr
         _other -> False
  where
    not_a_load (CmmMachOp _ args) = all not_a_load args
    not_a_load (CmmLoad _ _) = False
    not_a_load _ = True

inlineStmt :: Unique -> CmmExpr -> CmmStmt -> CmmStmt
inlineStmt u a (CmmAssign r e) = CmmAssign r (inlineExpr u a e)
inlineStmt u a (CmmStore e1 e2) = CmmStore (inlineExpr u a e1) (inlineExpr u a e2)
inlineStmt u a (CmmCall target regs es ret)
   = CmmCall (infn target) regs es' ret
   where infn (CmmCallee fn cconv) = CmmCallee (inlineExpr u a fn) cconv
         infn (CmmPrim p mStmts) = CmmPrim p (fmap (map (inlineStmt u a)) mStmts)
         es' = [ (CmmHinted (inlineExpr u a e) hint) | (CmmHinted e hint) <- es ]
inlineStmt u a (CmmCondBranch e d) = CmmCondBranch (inlineExpr u a e) d
inlineStmt u a (CmmSwitch e d) = CmmSwitch (inlineExpr u a e) d
inlineStmt u a (CmmJump e live) = CmmJump (inlineExpr u a e) live
inlineStmt _ _ other_stmt = other_stmt

inlineExpr :: Unique -> CmmExpr -> CmmExpr -> CmmExpr
inlineExpr u a e@(CmmReg (CmmLocal (LocalReg u' _)))
  | u == u' = a
  | otherwise = e
inlineExpr u a e@(CmmRegOff (CmmLocal (LocalReg u' rep)) off)
  | u == u' = CmmMachOp (MO_Add width) [a, CmmLit (CmmInt (fromIntegral off) width)]
  | otherwise = e
  where
    width = typeWidth rep
inlineExpr u a (CmmLoad e rep) = CmmLoad (inlineExpr u a e) rep
inlineExpr u a (CmmMachOp op es) = CmmMachOp op (map (inlineExpr u a) es)
inlineExpr _ _ other_expr = other_expr

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
            Just $ cmmMachOpFold platform (intconv signed1 rep1 rep3) [x]
        -- Nested widenings: collapse if the signedness is the same
        | rep1 < rep2 && rep2 < rep3 && signed1 == signed2 ->
            Just $ cmmMachOpFold platform (intconv signed1 rep1 rep3) [x]
        -- Nested narrowings: collapse
        | rep1 > rep2 && rep2 > rep3 ->
            Just $ cmmMachOpFold platform (MO_UU_Conv rep1 rep3) [x]
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

cmmMachOpFoldM _ mop [CmmLit (CmmInt x xrep), CmmLit (CmmInt y _)]
  = case mop of
        -- for comparisons: don't forget to narrow the arguments before
        -- comparing, since they might be out of range.
        MO_Eq _   -> Just $ CmmLit (CmmInt (if x_u == y_u then 1 else 0) wordWidth)
        MO_Ne _   -> Just $ CmmLit (CmmInt (if x_u /= y_u then 1 else 0) wordWidth)

        MO_U_Gt _ -> Just $ CmmLit (CmmInt (if x_u >  y_u then 1 else 0) wordWidth)
        MO_U_Ge _ -> Just $ CmmLit (CmmInt (if x_u >= y_u then 1 else 0) wordWidth)
        MO_U_Lt _ -> Just $ CmmLit (CmmInt (if x_u <  y_u then 1 else 0) wordWidth)
        MO_U_Le _ -> Just $ CmmLit (CmmInt (if x_u <= y_u then 1 else 0) wordWidth)

        MO_S_Gt _ -> Just $ CmmLit (CmmInt (if x_s >  y_s then 1 else 0) wordWidth)
        MO_S_Ge _ -> Just $ CmmLit (CmmInt (if x_s >= y_s then 1 else 0) wordWidth)
        MO_S_Lt _ -> Just $ CmmLit (CmmInt (if x_s <  y_s then 1 else 0) wordWidth)
        MO_S_Le _ -> Just $ CmmLit (CmmInt (if x_s <= y_s then 1 else 0) wordWidth)

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

cmmMachOpFoldM platform op [x@(CmmLit _), y]
   | not (isLit y) && isCommutableMachOp op
   = Just (cmmMachOpFold platform op [y, x])

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
   = Just (cmmMachOpFold platform mop2 [arg1, cmmMachOpFold platform mop1 [arg2,arg3]])
   where
     MO_Add{} `associates_with` MO_Sub{} = True
     mop1 `associates_with` mop2 =
        mop1 == mop2 && isAssociativeMachOp mop1

-- special case: (a - b) + c  ==>  a + (c - b)
cmmMachOpFoldM platform mop1@(MO_Add{}) [CmmMachOp mop2@(MO_Sub{}) [arg1,arg2], arg3]
   | not (isLit arg1) && not (isPicReg arg1)
   = Just (cmmMachOpFold platform mop1 [arg1, cmmMachOpFold platform mop2 [arg3,arg2]])

-- Make a RegOff if we can
cmmMachOpFoldM _ (MO_Add _) [CmmReg reg, CmmLit (CmmInt n rep)]
  = Just $ CmmRegOff reg (fromIntegral (narrowS rep n))
cmmMachOpFoldM _ (MO_Add _) [CmmRegOff reg off, CmmLit (CmmInt n rep)]
  = Just $ CmmRegOff reg (off + fromIntegral (narrowS rep n))
cmmMachOpFoldM _ (MO_Sub _) [CmmReg reg, CmmLit (CmmInt n rep)]
  = Just $ CmmRegOff reg (- fromIntegral (narrowS rep n))
cmmMachOpFoldM _ (MO_Sub _) [CmmRegOff reg off, CmmLit (CmmInt n rep)]
  = Just $ CmmRegOff reg (off - fromIntegral (narrowS rep n))

-- Fold label(+/-)offset into a CmmLit where possible

cmmMachOpFoldM _ (MO_Add _) [CmmLit (CmmLabel lbl), CmmLit (CmmInt i rep)]
  = Just $ CmmLit (CmmLabelOff lbl (fromIntegral (narrowU rep i)))
cmmMachOpFoldM _ (MO_Add _) [CmmLit (CmmInt i rep), CmmLit (CmmLabel lbl)]
  = Just $ CmmLit (CmmLabelOff lbl (fromIntegral (narrowU rep i)))
cmmMachOpFoldM _ (MO_Sub _) [CmmLit (CmmLabel lbl), CmmLit (CmmInt i rep)]
  = Just $ CmmLit (CmmLabelOff lbl (fromIntegral (negate (narrowU rep i))))


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
  = Just (cmmMachOpFold platform narrow_cmp [x, CmmLit (CmmInt i rep)])
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

cmmMachOpFoldM _ mop [x, y@(CmmLit (CmmInt 0 _))]
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
        MO_U_Lt  _ | isComparisonExpr x -> Just $ CmmLit (CmmInt 0 wordWidth)
        MO_S_Lt  _ | isComparisonExpr x -> Just $ CmmLit (CmmInt 0 wordWidth)
        MO_U_Ge  _ | isComparisonExpr x -> Just $ CmmLit (CmmInt 1 wordWidth)
        MO_S_Ge  _ | isComparisonExpr x -> Just $ CmmLit (CmmInt 1 wordWidth)
        MO_U_Le  _ | Just x' <- maybeInvertCmmExpr x -> Just x'
        MO_S_Le  _ | Just x' <- maybeInvertCmmExpr x -> Just x'
        _ -> Nothing

cmmMachOpFoldM _ mop [x, (CmmLit (CmmInt 1 rep))]
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
        MO_U_Gt  _ | isComparisonExpr x -> Just $ CmmLit (CmmInt 0 wordWidth)
        MO_S_Gt  _ | isComparisonExpr x -> Just $ CmmLit (CmmInt 0 wordWidth)
        MO_U_Le  _ | isComparisonExpr x -> Just $ CmmLit (CmmInt 1 wordWidth)
        MO_S_Le  _ | isComparisonExpr x -> Just $ CmmLit (CmmInt 1 wordWidth)
        MO_U_Ge  _ | isComparisonExpr x -> Just x
        MO_S_Ge  _ | isComparisonExpr x -> Just x
        _ -> Nothing

-- Now look for multiplication/division by powers of 2 (integers).

cmmMachOpFoldM platform mop [x, (CmmLit (CmmInt n _))]
  = case mop of
        MO_Mul rep
           | Just p <- exactLog2 n ->
                 Just (cmmMachOpFold platform (MO_Shl rep) [x, CmmLit (CmmInt p rep)])
        MO_U_Quot rep
           | Just p <- exactLog2 n ->
                 Just (cmmMachOpFold platform (MO_U_Shr rep) [x, CmmLit (CmmInt p rep)])
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
                Just (cmmMachOpFold platform (MO_S_Shr rep) [x3, CmmLit (CmmInt p rep)])
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
-- Loopify for C

{-
 This is a simple pass that replaces tail-recursive functions like this:

   fac() {
     ...
     jump fac();
   }

 with this:

  fac() {
   L:
     ...
     goto L;
  }

  the latter generates better C code, because the C compiler treats it
  like a loop, and brings full loop optimisation to bear.

  In my measurements this makes little or no difference to anything
  except factorial, but what the hell.
-}

cmmLoopifyForC :: DynFlags -> RawCmmDecl -> RawCmmDecl
-- XXX: revisit if we actually want to do this
-- cmmLoopifyForC p@(CmmProc Nothing _ _) = p  -- only if there's an info table, ignore case alts
cmmLoopifyForC dflags (CmmProc infos entry_lbl
                 (ListGraph blocks@(BasicBlock top_id _ : _))) =
--  pprTrace "jump_lbl" (ppr jump_lbl <+> ppr entry_lbl) $
  CmmProc infos entry_lbl (ListGraph blocks')
  where blocks' = [ BasicBlock id (map do_stmt stmts)
                  | BasicBlock id stmts <- blocks ]

        do_stmt (CmmJump (CmmLit (CmmLabel lbl)) _) | lbl == jump_lbl
                = CmmBranch top_id
        do_stmt stmt = stmt

        jump_lbl | tablesNextToCode dflags = toInfoLbl entry_lbl
                 | otherwise               = entry_lbl

cmmLoopifyForC _ top = top

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

