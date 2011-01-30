{-# OPTIONS -w #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and fix
-- any warnings in the module. See
--     http://hackage.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#Warnings
-- for details

-----------------------------------------------------------------------------
--
-- Cmm optimisation
--
-- (c) The University of Glasgow 2006
--
-----------------------------------------------------------------------------

module CmmOpt (
	cmmMiniInline,
	cmmMachOpFold,
	cmmLoopifyForC,
 ) where

#include "HsVersions.h"

import OldCmm
import CmmUtils
import CLabel
import StaticFlags

import UniqFM
import Unique
import FastTypes
import Outputable

import Data.Bits
import Data.Word
import Data.Int

-- -----------------------------------------------------------------------------
-- The mini-inliner

{-
This pass inlines assignments to temporaries that are used just
once.  It works as follows:

  - count uses of each temporary
  - for each temporary that occurs just once:
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
countUses a = foldRegsUsed (\m r -> addToUFM m r (count m r + 1)) emptyUFM a
  where count m r = lookupWithDefaultUFM m (0::Int) r

cmmMiniInline :: [CmmBasicBlock] -> [CmmBasicBlock]
cmmMiniInline blocks = map do_inline blocks 
  where do_inline (BasicBlock id stmts)
          = BasicBlock id (cmmMiniInlineStmts (countUses blocks) stmts)

cmmMiniInlineStmts :: UniqFM Int -> [CmmStmt] -> [CmmStmt]
cmmMiniInlineStmts uses [] = []
cmmMiniInlineStmts uses (stmt@(CmmAssign (CmmLocal (LocalReg u _)) expr) : stmts)
        -- not used at all: just discard this assignment
  | Nothing <- lookupUFM uses u
  = cmmMiniInlineStmts uses stmts

        -- used once: try to inline at the use site
  | Just 1 <- lookupUFM uses u,
    Just stmts' <- lookForInline u expr stmts
  = 
#ifdef NCG_DEBUG
     trace ("nativeGen: inlining " ++ showSDoc (pprStmt stmt)) $
#endif
     cmmMiniInlineStmts uses stmts'

cmmMiniInlineStmts uses (stmt:stmts)
  = stmt : cmmMiniInlineStmts uses stmts

lookForInline u expr (stmt : rest)
  | Just 1 <- lookupUFM (countUses stmt) u, ok_to_inline
  = Just (inlineStmt u expr stmt : rest)

  | ok_to_skip
  = case lookForInline u expr rest of
           Nothing    -> Nothing
           Just stmts -> Just (stmt:stmts)

  | otherwise 
  = Nothing

  where
	-- we don't inline into CmmCall if the expression refers to global
	-- registers.  This is a HACK to avoid global registers clashing with
	-- C argument-passing registers, really the back-end ought to be able
	-- to handle it properly, but currently neither PprC nor the NCG can
	-- do it.  See also CgForeignCall:load_args_into_temps.
    ok_to_inline = case stmt of
		     CmmCall{} -> hasNoGlobalRegs expr
		     _ -> True

   -- We can skip over assignments to other tempoararies, because we
   -- know that expressions aren't side-effecting and temporaries are
   -- single-assignment.
    ok_to_skip = case stmt of
                 CmmNop -> True
                 CmmAssign (CmmLocal (LocalReg u' _)) rhs | u' /= u -> True
                 CmmAssign g@(CmmGlobal _) rhs -> not (g `regUsedIn` expr)
                 _other -> False


inlineStmt :: Unique -> CmmExpr -> CmmStmt -> CmmStmt
inlineStmt u a (CmmAssign r e) = CmmAssign r (inlineExpr u a e)
inlineStmt u a (CmmStore e1 e2) = CmmStore (inlineExpr u a e1) (inlineExpr u a e2)
inlineStmt u a (CmmCall target regs es srt ret)
   = CmmCall (infn target) regs es' srt ret
   where infn (CmmCallee fn cconv) = CmmCallee (inlineExpr u a fn) cconv
	 infn (CmmPrim p) = CmmPrim p
	 es' = [ (CmmHinted (inlineExpr u a e) hint) | (CmmHinted e hint) <- es ]
inlineStmt u a (CmmCondBranch e d) = CmmCondBranch (inlineExpr u a e) d
inlineStmt u a (CmmSwitch e d) = CmmSwitch (inlineExpr u a e) d
inlineStmt u a (CmmJump e d) = CmmJump (inlineExpr u a e) d
inlineStmt u a other_stmt = other_stmt

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
inlineExpr u a other_expr = other_expr

-- -----------------------------------------------------------------------------
-- MachOp constant folder

-- Now, try to constant-fold the MachOps.  The arguments have already
-- been optimized and folded.

cmmMachOpFold
    :: MachOp	    	-- The operation from an CmmMachOp
    -> [CmmExpr]   	-- The optimized arguments
    -> CmmExpr

cmmMachOpFold op arg@[CmmLit (CmmInt x rep)]
  = case op of
      MO_S_Neg r -> CmmLit (CmmInt (-x) rep)
      MO_Not r   -> CmmLit (CmmInt (complement x) rep)

	-- these are interesting: we must first narrow to the 
	-- "from" type, in order to truncate to the correct size.
	-- The final narrow/widen to the destination type
	-- is implicit in the CmmLit.
      MO_SF_Conv from to -> CmmLit (CmmFloat (fromInteger x) to)
      MO_SS_Conv from to -> CmmLit (CmmInt (narrowS from x) to)
      MO_UU_Conv from to -> CmmLit (CmmInt (narrowU from x) to)

      _ -> panic "cmmMachOpFold: unknown unary op"


-- Eliminate conversion NOPs
cmmMachOpFold (MO_SS_Conv rep1 rep2) [x] | rep1 == rep2 = x
cmmMachOpFold (MO_UU_Conv rep1 rep2) [x] | rep1 == rep2 = x

-- Eliminate nested conversions where possible
cmmMachOpFold conv_outer args@[CmmMachOp conv_inner [x]]
  | Just (rep1,rep2,signed1) <- isIntConversion conv_inner,
    Just (_,   rep3,signed2) <- isIntConversion conv_outer
  = case () of
	-- widen then narrow to the same size is a nop
      _ | rep1 < rep2 && rep1 == rep3 -> x
	-- Widen then narrow to different size: collapse to single conversion
	-- but remember to use the signedness from the widening, just in case
	-- the final conversion is a widen.
	| rep1 < rep2 && rep2 > rep3 ->
	    cmmMachOpFold (intconv signed1 rep1 rep3) [x]
	-- Nested widenings: collapse if the signedness is the same
	| rep1 < rep2 && rep2 < rep3 && signed1 == signed2 ->
	    cmmMachOpFold (intconv signed1 rep1 rep3) [x]
	-- Nested narrowings: collapse
	| rep1 > rep2 && rep2 > rep3 ->
	    cmmMachOpFold (MO_UU_Conv rep1 rep3) [x]
	| otherwise ->
	    CmmMachOp conv_outer args
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

cmmMachOpFold mop args@[CmmLit (CmmInt x xrep), CmmLit (CmmInt y _)]
  = case mop of
	-- for comparisons: don't forget to narrow the arguments before
	-- comparing, since they might be out of range.
    	MO_Eq r   -> CmmLit (CmmInt (if x_u == y_u then 1 else 0) wordWidth)
    	MO_Ne r   -> CmmLit (CmmInt (if x_u /= y_u then 1 else 0) wordWidth)

    	MO_U_Gt r -> CmmLit (CmmInt (if x_u >  y_u then 1 else 0) wordWidth)
    	MO_U_Ge r -> CmmLit (CmmInt (if x_u >= y_u then 1 else 0) wordWidth)
    	MO_U_Lt r -> CmmLit (CmmInt (if x_u <  y_u then 1 else 0) wordWidth)
    	MO_U_Le r -> CmmLit (CmmInt (if x_u <= y_u then 1 else 0) wordWidth)

    	MO_S_Gt r -> CmmLit (CmmInt (if x_s >  y_s then 1 else 0) wordWidth) 
    	MO_S_Ge r -> CmmLit (CmmInt (if x_s >= y_s then 1 else 0) wordWidth)
    	MO_S_Lt r -> CmmLit (CmmInt (if x_s <  y_s then 1 else 0) wordWidth)
    	MO_S_Le r -> CmmLit (CmmInt (if x_s <= y_s then 1 else 0) wordWidth)

    	MO_Add r -> CmmLit (CmmInt (x + y) r)
    	MO_Sub r -> CmmLit (CmmInt (x - y) r)
    	MO_Mul r -> CmmLit (CmmInt (x * y) r)
    	MO_U_Quot r | y /= 0 -> CmmLit (CmmInt (x_u `quot` y_u) r)
    	MO_U_Rem  r | y /= 0 -> CmmLit (CmmInt (x_u `rem`  y_u) r)
    	MO_S_Quot r | y /= 0 -> CmmLit (CmmInt (x `quot` y) r)
    	MO_S_Rem  r | y /= 0 -> CmmLit (CmmInt (x `rem` y) r)

	MO_And   r -> CmmLit (CmmInt (x .&. y) r)
	MO_Or    r -> CmmLit (CmmInt (x .|. y) r)
	MO_Xor   r -> CmmLit (CmmInt (x `xor` y) r)

        MO_Shl   r -> CmmLit (CmmInt (x `shiftL` fromIntegral y) r)
        MO_U_Shr r -> CmmLit (CmmInt (x_u `shiftR` fromIntegral y) r)
        MO_S_Shr r -> CmmLit (CmmInt (x `shiftR` fromIntegral y) r)

	other      -> CmmMachOp mop args

   where
	x_u = narrowU xrep x
	y_u = narrowU xrep y
	x_s = narrowS xrep x
	y_s = narrowS xrep y
	

-- When possible, shift the constants to the right-hand side, so that we
-- can match for strength reductions.  Note that the code generator will
-- also assume that constants have been shifted to the right when
-- possible.

cmmMachOpFold op [x@(CmmLit _), y]
   | not (isLit y) && isCommutableMachOp op 
   = cmmMachOpFold op [y, x]

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
cmmMachOpFold mop1 [CmmMachOp mop2 [arg1,arg2], arg3]
   | mop1 == mop2 && isAssociativeMachOp mop1
     && not (isLit arg1) && not (isPicReg arg1)
   = cmmMachOpFold mop1 [arg1, cmmMachOpFold mop2 [arg2,arg3]]

-- Make a RegOff if we can
cmmMachOpFold (MO_Add _) [CmmReg reg, CmmLit (CmmInt n rep)]
  = CmmRegOff reg (fromIntegral (narrowS rep n))
cmmMachOpFold (MO_Add _) [CmmRegOff reg off, CmmLit (CmmInt n rep)]
  = CmmRegOff reg (off + fromIntegral (narrowS rep n))
cmmMachOpFold (MO_Sub _) [CmmReg reg, CmmLit (CmmInt n rep)]
  = CmmRegOff reg (- fromIntegral (narrowS rep n))
cmmMachOpFold (MO_Sub _) [CmmRegOff reg off, CmmLit (CmmInt n rep)]
  = CmmRegOff reg (off - fromIntegral (narrowS rep n))

-- Fold label(+/-)offset into a CmmLit where possible

cmmMachOpFold (MO_Add _) [CmmLit (CmmLabel lbl), CmmLit (CmmInt i rep)]
  = CmmLit (CmmLabelOff lbl (fromIntegral (narrowU rep i)))
cmmMachOpFold (MO_Add _) [CmmLit (CmmInt i rep), CmmLit (CmmLabel lbl)]
  = CmmLit (CmmLabelOff lbl (fromIntegral (narrowU rep i)))
cmmMachOpFold (MO_Sub _) [CmmLit (CmmLabel lbl), CmmLit (CmmInt i rep)]
  = CmmLit (CmmLabelOff lbl (fromIntegral (negate (narrowU rep i))))


-- Comparison of literal with widened operand: perform the comparison
-- at the smaller width, as long as the literal is within range.

-- We can't do the reverse trick, when the operand is narrowed:
-- narrowing throws away bits from the operand, there's no way to do
-- the same comparison at the larger size.

#if i386_TARGET_ARCH || x86_64_TARGET_ARCH
-- powerPC NCG has a TODO for I8/I16 comparisons, so don't try

cmmMachOpFold cmp [CmmMachOp conv [x], CmmLit (CmmInt i _)]
  |     -- if the operand is widened:
    Just (rep, signed, narrow_fn) <- maybe_conversion conv,
        -- and this is a comparison operation:
    Just narrow_cmp <- maybe_comparison cmp rep signed,
        -- and the literal fits in the smaller size:
    i == narrow_fn rep i
        -- then we can do the comparison at the smaller size
  = cmmMachOpFold narrow_cmp [x, CmmLit (CmmInt i rep)]
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

#endif

-- We can often do something with constants of 0 and 1 ...

cmmMachOpFold mop args@[x, y@(CmmLit (CmmInt 0 _))]
  = case mop of
    	MO_Add   r -> x
    	MO_Sub   r -> x
    	MO_Mul   r -> y
    	MO_And   r -> y
    	MO_Or    r -> x
    	MO_Xor   r -> x
    	MO_Shl   r -> x
    	MO_S_Shr r -> x
    	MO_U_Shr r -> x
        MO_Ne    r | isComparisonExpr x -> x
	MO_Eq    r | Just x' <- maybeInvertCmmExpr x -> x'
	MO_U_Gt  r | isComparisonExpr x -> x
	MO_S_Gt  r | isComparisonExpr x -> x
	MO_U_Lt  r | isComparisonExpr x -> CmmLit (CmmInt 0 wordWidth)
	MO_S_Lt  r | isComparisonExpr x -> CmmLit (CmmInt 0 wordWidth)
	MO_U_Ge  r | isComparisonExpr x -> CmmLit (CmmInt 1 wordWidth)
	MO_S_Ge  r | isComparisonExpr x -> CmmLit (CmmInt 1 wordWidth)
	MO_U_Le  r | Just x' <- maybeInvertCmmExpr x -> x'
	MO_S_Le  r | Just x' <- maybeInvertCmmExpr x -> x'
    	other    -> CmmMachOp mop args

cmmMachOpFold mop args@[x, y@(CmmLit (CmmInt 1 rep))]
  = case mop of
    	MO_Mul    r -> x
    	MO_S_Quot r -> x
    	MO_U_Quot r -> x
    	MO_S_Rem  r -> CmmLit (CmmInt 0 rep)
    	MO_U_Rem  r -> CmmLit (CmmInt 0 rep)
        MO_Ne    r | Just x' <- maybeInvertCmmExpr x -> x'
	MO_Eq    r | isComparisonExpr x -> x
	MO_U_Lt  r | Just x' <- maybeInvertCmmExpr x -> x'
	MO_S_Lt  r | Just x' <- maybeInvertCmmExpr x -> x'
	MO_U_Gt  r | isComparisonExpr x -> CmmLit (CmmInt 0 wordWidth)
	MO_S_Gt  r | isComparisonExpr x -> CmmLit (CmmInt 0 wordWidth)
	MO_U_Le  r | isComparisonExpr x -> CmmLit (CmmInt 1 wordWidth)
	MO_S_Le  r | isComparisonExpr x -> CmmLit (CmmInt 1 wordWidth)
	MO_U_Ge  r | isComparisonExpr x -> x
	MO_S_Ge  r | isComparisonExpr x -> x
    	other       -> CmmMachOp mop args

-- Now look for multiplication/division by powers of 2 (integers).

cmmMachOpFold mop args@[x, y@(CmmLit (CmmInt n _))]
  = case mop of
    	MO_Mul rep
	   | Just p <- exactLog2 n ->
                 CmmMachOp (MO_Shl rep) [x, CmmLit (CmmInt p rep)]
    	MO_U_Quot rep
	   | Just p <- exactLog2 n ->
                 CmmMachOp (MO_U_Shr rep) [x, CmmLit (CmmInt p rep)]
    	MO_S_Quot rep
	   | Just p <- exactLog2 n, 
	     CmmReg _ <- x ->	-- We duplicate x below, hence require
				-- it is a reg.  FIXME: remove this restriction.
		-- shift right is not the same as quot, because it rounds
		-- to minus infinity, whereasq quot rounds toward zero.
		-- To fix this up, we add one less than the divisor to the
		-- dividend if it is a negative number.
		--
		-- to avoid a test/jump, we use the following sequence:
		-- 	x1 = x >> word_size-1  (all 1s if -ve, all 0s if +ve)
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
                CmmMachOp (MO_S_Shr rep) [x3, CmmLit (CmmInt p rep)]
    	other
           -> unchanged
    where
       unchanged = CmmMachOp mop args

-- Anything else is just too hard.

cmmMachOpFold mop args = CmmMachOp mop args

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

cmmLoopifyForC :: RawCmmTop -> RawCmmTop
cmmLoopifyForC p@(CmmProc info entry_lbl
                 (ListGraph blocks@(BasicBlock top_id _ : _)))
  | null info = p  -- only if there's an info table, ignore case alts
  | otherwise =  
--  pprTrace "jump_lbl" (ppr jump_lbl <+> ppr entry_lbl) $
  CmmProc info entry_lbl (ListGraph blocks')
  where blocks' = [ BasicBlock id (map do_stmt stmts)
		  | BasicBlock id stmts <- blocks ]

        do_stmt (CmmJump (CmmLit (CmmLabel lbl)) _) | lbl == jump_lbl
		= CmmBranch top_id
	do_stmt stmt = stmt

	jump_lbl | tablesNextToCode = entryLblToInfoLbl entry_lbl
		 | otherwise        = entry_lbl

cmmLoopifyForC top = top

-- -----------------------------------------------------------------------------
-- Utils

isLit (CmmLit _) = True
isLit _          = False

isComparisonExpr :: CmmExpr -> Bool
isComparisonExpr (CmmMachOp op _) = isComparisonMachOp op
isComparisonExpr _other 	    = False

isPicReg (CmmReg (CmmGlobal PicBaseReg)) = True
isPicReg _ = False

