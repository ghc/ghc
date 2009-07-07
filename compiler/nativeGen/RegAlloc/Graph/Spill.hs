
{-# OPTIONS -fno-warn-missing-signatures #-}

module RegAlloc.Graph.Spill (
	regSpill,
	SpillStats(..),
	accSpillSL
)

where

import RegAlloc.Liveness
import Instruction
import Reg
import Cmm

import State
import Unique
import UniqFM
import UniqSet
import UniqSupply
import Outputable

import Data.List


-- | Spill all these virtual regs to memory
--	TODO: 	see if we can split some of the live ranges instead of just globally
--		spilling the virtual reg.
--
--	TODO:	On ciscy x86 and x86_64 we don't nessesarally have to add a mov instruction
--		when making spills. If an instr is using a spilled virtual we may be able to
--		address the spill slot directly.
--
regSpill
	:: Instruction instr
	=> [LiveCmmTop instr]		-- ^ the code
	-> UniqSet Int			-- ^ available stack slots
	-> UniqSet VirtualReg		-- ^ the regs to spill
	-> UniqSM
		([LiveCmmTop instr]	-- code will spill instructions
		, UniqSet Int		-- left over slots
		, SpillStats )		-- stats about what happened during spilling

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
		let (code', state')	=
			runState (mapM (mapBlockTopM (regSpill_block regSlotMap)) code)
				 (initSpillS us)

		return	( code'
			, minusUniqSet slotsFree (mkUniqSet slots)
			, makeSpillStats state')


regSpill_block regSlotMap (BasicBlock i instrs)
 = do	instrss'	<- mapM (regSpill_instr regSlotMap) instrs
 	return	$ BasicBlock i (concat instrss')


regSpill_instr
	:: Instruction instr
	=> UniqFM Int 
	-> LiveInstr instr -> SpillM [LiveInstr instr]

-- | The thing we're spilling shouldn't already have spill or reloads in it
regSpill_instr	_ SPILL{}
	= panic "regSpill_instr: unexpected SPILL"

regSpill_instr	_ RELOAD{}
	= panic "regSpill_instr: unexpected RELOAD"


regSpill_instr _	li@(Instr _ Nothing)
 = do	return [li]

regSpill_instr regSlotMap
	(Instr instr (Just _))
 = do
	-- work out which regs are read and written in this instr
	let RU rlRead rlWritten	= regUsageOfInstr instr

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
	let instrs'	=  prefixes
			++ [Instr instr3 Nothing]
			++ postfixes

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
	= do 	(instr', nReg)	<- patchInstr reg instr

		modify $ \s -> s
			{ stateSpillSL 	= addToUFM_C accSpillSL (stateSpillSL s) reg (reg, 0, 1) }

	 	return	( instr'
			, ( [RELOAD slot nReg]
			  , []) )

	| otherwise	= panic "RegSpill.spillRead: no slot defined for spilled reg"


spillWrite regSlotMap instr reg
	| Just slot	<- lookupUFM regSlotMap reg
	= do 	(instr', nReg)	<- patchInstr reg instr

		modify $ \s -> s
			{ stateSpillSL 	= addToUFM_C accSpillSL (stateSpillSL s) reg (reg, 1, 0) }

	 	return	( instr'
			, ( []
			  , [SPILL nReg slot]))

	| otherwise	= panic "RegSpill.spillWrite: no slot defined for spilled reg"


spillModify regSlotMap instr reg
	| Just slot	<- lookupUFM regSlotMap reg
	= do	(instr', nReg)	<- patchInstr reg instr

		modify $ \s -> s
			{ stateSpillSL 	= addToUFM_C accSpillSL (stateSpillSL s) reg (reg, 1, 1) }

		return	( instr'
			, ( [RELOAD slot nReg]
			  , [SPILL nReg slot]))

	| otherwise	= panic "RegSpill.spillModify: no slot defined for spilled reg"



-- | rewrite uses of this virtual reg in an instr to use a different virtual reg
patchInstr 
	:: Instruction instr
	=> Reg -> instr -> SpillM (instr, Reg)

patchInstr reg instr
 = do	nUnique		<- newUnique
 	let nReg	= case reg of 
				RegVirtual vr 	-> RegVirtual (renameVirtualReg nUnique vr)
				RegReal{}	-> panic "RegAlloc.Graph.Spill.patchIntr: not patching real reg"
	let instr'	= patchReg1 reg nReg instr
	return		(instr', nReg)

patchReg1 
	:: Instruction instr
	=> Reg -> Reg -> instr -> instr

patchReg1 old new instr
 = let	patchF r
		| r == old	= new
		| otherwise	= r
   in	patchRegsOfInstr instr patchF


------------------------------------------------------
-- Spiller monad

data SpillS
	= SpillS
	{ stateUS	:: UniqSupply
	, stateSpillSL	:: UniqFM (Reg, Int, Int) } -- ^ spilled reg vs number of times vreg was loaded, stored

initSpillS uniqueSupply
	= SpillS
	{ stateUS	= uniqueSupply
	, stateSpillSL	= emptyUFM }

type SpillM a	= State SpillS a

newUnique :: SpillM Unique
newUnique
 = do	us	<- gets stateUS
 	case splitUniqSupply us of
	 (us1, us2)
	  -> do let uniq = uniqFromSupply us1
	  	modify $ \s -> s { stateUS = us2 }
		return uniq

accSpillSL (r1, s1, l1) (_, s2, l2)
	= (r1, s1 + s2, l1 + l2)


----------------------------------------------------
-- Spiller stats

data SpillStats
	= SpillStats
	{ spillStoreLoad	:: UniqFM (Reg, Int, Int) }

makeSpillStats :: SpillS -> SpillStats
makeSpillStats s
	= SpillStats
	{ spillStoreLoad	= stateSpillSL s }

instance Outputable SpillStats where
 ppr stats
 	= (vcat $ map (\(r, s, l) -> ppr r <+> int s <+> int l)
			$ eltsUFM (spillStoreLoad stats))

