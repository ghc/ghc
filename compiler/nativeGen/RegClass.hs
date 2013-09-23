{-# OPTIONS -fno-warn-tabs #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and
-- detab the module (please do the detabbing in a separate patch). See
--     http://ghc.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#TabsvsSpaces
-- for details


-- | An architecture independent description of a register's class.
module RegClass 
	( RegClass (..) )

where

import	Outputable
import	Unique


-- | The class of a register. 
--	Used in the register allocator.
--	We treat all registers in a class as being interchangable.
--
data RegClass 
	= RcInteger 
	| RcFloat
	| RcDouble
	| RcDoubleSSE -- x86 only: the SSE regs are a separate class
	deriving Eq


instance Uniquable RegClass where
    getUnique RcInteger	= mkRegClassUnique 0
    getUnique RcFloat	= mkRegClassUnique 1
    getUnique RcDouble	= mkRegClassUnique 2
    getUnique RcDoubleSSE = mkRegClassUnique 3

instance Outputable RegClass where
    ppr RcInteger	= Outputable.text "I"
    ppr RcFloat		= Outputable.text "F"
    ppr RcDouble	= Outputable.text "D"
    ppr RcDoubleSSE	= Outputable.text "S"
