
{-# OPTIONS -fno-warn-tabs #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and
-- detab the module (please do the detabbing in a separate patch). See
--     http://ghc.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#TabsvsSpaces
-- for details

module SPARC.Imm (
	-- immediate values
	Imm(..),
	strImmLit,
	litToImm
)

where

import Cmm
import CLabel

import Outputable

-- | An immediate value.
--	Not all of these are directly representable by the machine. 
--	Things like ImmLit are slurped out and put in a data segment instead.
--
data Imm
	= ImmInt	Int

	-- Sigh.
	| ImmInteger	Integer	    

	-- AbstractC Label (with baggage)
	| ImmCLbl	CLabel	    

	-- Simple string
	| ImmLit	SDoc
	| ImmIndex	CLabel Int
	| ImmFloat	Rational
	| ImmDouble	Rational

	| ImmConstantSum  Imm Imm
	| ImmConstantDiff Imm Imm

	| LO 	Imm		   
	| HI 	Imm


-- | Create a ImmLit containing this string.
strImmLit :: String -> Imm
strImmLit s = ImmLit (text s)


-- | Convert a CmmLit to an Imm.
-- 	Narrow to the width: a CmmInt might be out of
-- 	range, but we assume that ImmInteger only contains
-- 	in-range values.  A signed value should be fine here.
--
litToImm :: CmmLit -> Imm
litToImm lit
 = case lit of
 	CmmInt i w		-> ImmInteger (narrowS w i)
	CmmFloat f W32		-> ImmFloat f
	CmmFloat f W64		-> ImmDouble f
	CmmLabel l		-> ImmCLbl l
	CmmLabelOff l off	-> ImmIndex l off

	CmmLabelDiffOff l1 l2 off
	 -> ImmConstantSum
		(ImmConstantDiff (ImmCLbl l1) (ImmCLbl l2))
		(ImmInt off)

        _               -> panic "SPARC.Regs.litToImm: no match"


