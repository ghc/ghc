
module X86.Cond (
	Cond(..),
	condUnsigned,
	condToSigned,
	condToUnsigned
)

where

data Cond
	= ALWAYS	-- What's really used? ToDo
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
	| NEG
	| POS
	| CARRY
	| OFLO
	| PARITY
	| NOTPARITY
	deriving Eq

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
