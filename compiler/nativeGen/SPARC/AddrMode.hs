
{-# OPTIONS -fno-warn-tabs #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and
-- detab the module (please do the detabbing in a separate patch). See
--     http://hackage.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#TabsvsSpaces
-- for details

module SPARC.AddrMode (
	AddrMode(..),
	addrOffset
)

where

import SPARC.Imm
import SPARC.Base
import Reg

-- addressing modes ------------------------------------------------------------

-- | Represents a memory address in an instruction.
--	Being a RISC machine, the SPARC addressing modes are very regular.
--
data AddrMode
	= AddrRegReg	Reg Reg		-- addr = r1 + r2
	| AddrRegImm	Reg Imm		-- addr = r1 + imm


-- | Add an integer offset to the address in an AddrMode.
--
addrOffset :: AddrMode -> Int -> Maybe AddrMode
addrOffset addr off
  = case addr of
      AddrRegImm r (ImmInt n)
       | fits13Bits n2 -> Just (AddrRegImm r (ImmInt n2))
       | otherwise     -> Nothing
       where n2 = n + off

      AddrRegImm r (ImmInteger n)
       | fits13Bits n2 -> Just (AddrRegImm r (ImmInt (fromInteger n2)))
       | otherwise     -> Nothing
       where n2 = n + toInteger off

      AddrRegReg r (RegReal (RealRegSingle 0))
       | fits13Bits off -> Just (AddrRegImm r (ImmInt off))
       | otherwise     -> Nothing
       
      _ -> Nothing
