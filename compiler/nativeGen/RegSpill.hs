
module RegSpill (
	regSpill
)

where

#include "HsVersions.h"

import RegLiveness
import RegAllocInfo
import MachRegs
import MachInstrs
import Cmm

import Unique
import UniqFM
import UniqSet
import UniqSupply
import Outputable

import Data.List
import Data.Maybe


-- | Spill all these virtual regs to memory
--	TODO: 	see if we can split some of the live ranges instead of just globally
--		spilling the virtual reg.
--
--	TODO:	On ciscy x86 and x86_64 we don't nessesarally have to add a mov instruction
--		when making spills. If an instr is using a spilled virtual we may be able to
--		address the spill slot directly.
--
regSpill
	:: [LiveCmmTop]			-- ^ the code
	-> UniqSet Int			-- ^ available stack slots
	-> UniqSet Reg			-- ^ the regs to spill
	-> UniqSM
		([LiveCmmTop]		-- ^ code will spill instructions
		, UniqSet Int)		-- ^ left over slots

regSpill code slotsFree regs

	-- not enough slots to spill these regs
	| sizeUniqSet slotsFree < sizeUniqSet regs
	= pprPanic "regSpill: out of spill slots!"
		(  text "   regs to spill = " <> ppr (sizeUniqSet regs)
		$$ text "   slots left    = " <> ppr (sizeUniqSet slotsFree))

	| otherwise
	= do
		-- allocate a slot for each of the spilled regs
		let slots	= take (sizeUniqSet regs) $ uniqSetToList slotsFree
		let regSlotMap	= listToUFM
				$ zip (uniqSetToList regs) slots

		-- grab the unique supply from the monad
		us	<- getUs

		-- run the spiller on all the blocks
		let (# code', _ #)	=
			runSpill (mapM (mapBlockTopM (regSpill_block regSlotMap)) code)
				 (initSpillS us)

		return	( code'
			, minusUniqSet slotsFree (mkUniqSet slots) )


regSpill_block regSlotMap (BasicBlock i instrs)
 = do	instrss'	<- mapM (regSpill_instr regSlotMap) instrs
 	return	$ BasicBlock i (concat instrss')


regSpill_instr _ 	li@(Instr (DELTA delta) _)
 = do
 	setDelta delta
	return [li]

regSpill_instr _	li@(Instr _ Nothing)
 = do	return [li]


regSpill_instr regSlotMap
	(Instr instr (Just live))
 = do
	-- work out which regs are read and written in this instr
	let RU rlRead rlWritten	= regUsage instr

	-- sometimes a register is listed as being read more than once,
	--	nub this so we don't end up inserting two lots of spill code.
	let rsRead_		= nub rlRead
	let rsWritten_		= nub rlWritten

	-- if a reg is modified, it appears in both lists, want to undo this..
	let rsRead		= rsRead_    \\ rsWritten_
	let rsWritten		= rsWritten_ \\ rsRead_
	let rsModify		= intersect rsRead_ rsWritten_

	-- work out if any of the regs being used are currently being spilled.
	let rsSpillRead		= filter (\r -> elemUFM r regSlotMap) rsRead
	let rsSpillWritten	= filter (\r -> elemUFM r regSlotMap) rsWritten
	let rsSpillModify	= filter (\r -> elemUFM r regSlotMap) rsModify

	-- rewrite the instr and work out spill code.
	(instr1, prepost1)	<- mapAccumLM (spillRead   regSlotMap) instr  rsSpillRead
	(instr2, prepost2)	<- mapAccumLM (spillWrite  regSlotMap) instr1 rsSpillWritten
	(instr3, prepost3)	<- mapAccumLM (spillModify regSlotMap) instr2 rsSpillModify

	let (mPrefixes, mPostfixes)	= unzip (prepost1 ++ prepost2 ++ prepost3)
	let prefixes			= concat mPrefixes
	let postfixes			= concat mPostfixes

	-- final code
	let instrs'	=  map (\i -> Instr i Nothing) prefixes
			++ [ Instr instr3 Nothing ]
			++ map (\i -> Instr i Nothing) postfixes

	return
{-		$ pprTrace "* regSpill_instr spill"
			(  text "instr  = " <> ppr instr
			$$ text "read   = " <> ppr rsSpillRead
			$$ text "write  = " <> ppr rsSpillWritten
			$$ text "mod    = " <> ppr rsSpillModify
			$$ text "-- out"
			$$ (vcat $ map ppr instrs')
			$$ text " ")
-}
		$ instrs'


spillRead regSlotMap instr reg
	| Just slot	<- lookupUFM regSlotMap reg
	= do	delta		<- getDelta
	 	(instr', nReg)	<- patchInstr reg instr

		let pre	 	= [ COMMENT FSLIT("spill read")
				  , mkLoadInstr nReg delta slot ]

	 	return	( instr', (pre, []))

	| otherwise	= panic "RegSpill.spillRead: no slot defined for spilled reg"

spillWrite regSlotMap instr reg
	| Just slot	<- lookupUFM regSlotMap reg
	= do	delta		<- getDelta
	 	(instr', nReg)	<- patchInstr reg instr

		let post	= [ COMMENT FSLIT("spill write")
				  , mkSpillInstr nReg delta slot ]

	 	return	( instr', ([], post))

	| otherwise	= panic "RegSpill.spillWrite: no slot defined for spilled reg"

spillModify regSlotMap instr reg
	| Just slot	<- lookupUFM regSlotMap reg
	= do	delta		<- getDelta
		(instr', nReg)	<- patchInstr reg instr

		let pre		= [ COMMENT FSLIT("spill mod load")
				  , mkLoadInstr  nReg delta slot ]

		let post	= [ COMMENT FSLIT("spill mod write")
				  , mkSpillInstr nReg delta slot ]

		return	( instr', (pre, post))

	| otherwise	= panic "RegSpill.spillModify: no slot defined for spilled reg"


-- | rewrite uses of this virtual reg in an instr to use a different virtual reg
patchInstr :: Reg -> Instr -> SpillM (Instr, Reg)
patchInstr reg instr
 = do	nUnique		<- newUnique
 	let nReg	= renameVirtualReg nUnique reg
	let instr'	= patchReg1 reg nReg instr
	return		(instr', nReg)

patchReg1 :: Reg -> Reg -> Instr -> Instr
patchReg1 old new instr
 = let	patchF r
		| r == old	= new
		| otherwise	= r
   in	patchRegs instr patchF


-------------------------------------------------------------------------------------------
-- Spiller monad

data SpillS
	= SpillS
	{ stateDelta	:: Int
	, stateUS	:: UniqSupply }

initSpillS uniqueSupply
	= SpillS
	{ stateDelta	= 0
	, stateUS	= uniqueSupply }

newtype SpillM a
	= SpillM
	{ runSpill :: SpillS  -> (# a, SpillS #) }

instance Monad SpillM where
    return x	= SpillM $ \s -> (# x, s #)

    m >>= n	= SpillM $ \s ->
			case runSpill m s of
			  (# r, s' #)	-> runSpill (n r) s'

setDelta :: Int -> SpillM ()
setDelta delta
	= SpillM $ \s -> (# (), s { stateDelta = delta } #)

getDelta  :: SpillM Int
getDelta = SpillM $ \s -> (# stateDelta s, s #)

newUnique :: SpillM Unique
newUnique
	= SpillM $ \s
	-> case splitUniqSupply (stateUS s) of
		(us1, us2)
		  -> 	(# uniqFromSupply us1
		  	, s { stateUS = us2 } #)

mapAccumLM _ s []	= return (s, [])
mapAccumLM f s (x:xs)
 = do
	(s1, x')	<- f s x
	(s2, xs')	<- mapAccumLM f s1 xs
	return		(s2, x' : xs')


