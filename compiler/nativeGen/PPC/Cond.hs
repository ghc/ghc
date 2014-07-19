
{-# OPTIONS_GHC -fno-warn-tabs #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and
-- detab the module (please do the detabbing in a separate patch). See
--     http://ghc.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#TabsvsSpaces
-- for details

module PPC.Cond (
	Cond(..),
	condNegate,
	condUnsigned,
	condToSigned,
	condToUnsigned,
)

where

import Panic

data Cond
	= ALWAYS
	| EQQ
	| GE
	| GEU
	| GTT
	| GU
	| LE
	| LEU
	| LTT
	| LU
	| NE
	deriving Eq


condNegate :: Cond -> Cond
condNegate ALWAYS  = panic "condNegate: ALWAYS"
condNegate EQQ     = NE
condNegate GE      = LTT
condNegate GEU     = LU
condNegate GTT     = LE
condNegate GU      = LEU
condNegate LE      = GTT
condNegate LEU     = GU
condNegate LTT     = GE
condNegate LU      = GEU
condNegate NE      = EQQ

-- Condition utils
condUnsigned :: Cond -> Bool
condUnsigned GU  = True
condUnsigned LU  = True
condUnsigned GEU = True
condUnsigned LEU = True
condUnsigned _   = False

condToSigned :: Cond -> Cond
condToSigned GU  = GTT
condToSigned LU  = LTT
condToSigned GEU = GE
condToSigned LEU = LE
condToSigned x   = x

condToUnsigned :: Cond -> Cond
condToUnsigned GTT = GU
condToUnsigned LTT = LU
condToUnsigned GE  = GEU
condToUnsigned LE  = LEU
condToUnsigned x   = x
