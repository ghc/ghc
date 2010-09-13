-----------------------------------------------------------------------------
--
-- The register liveness determinator
--
-- (c) The University of Glasgow 2004
--
-----------------------------------------------------------------------------
{-# OPTIONS -Wall -fno-warn-name-shadowing #-}

module RegAlloc.Liveness (
	RegSet,
	RegMap, emptyRegMap,
	BlockMap, emptyBlockMap,
	LiveCmmTop,
	InstrSR	  (..),
	LiveInstr (..),
	Liveness (..),
	LiveInfo (..),
	LiveBasicBlock,

	mapBlockTop, 	mapBlockTopM,
	mapGenBlockTop,	mapGenBlockTopM,
	stripLive,
	stripLiveBlock,
	slurpConflicts,
	slurpReloadCoalesce,
	eraseDeltasLive,
	patchEraseLive,
	patchRegsLiveInstr,
	reverseBlocksInTops,
	regLiveness,
	natCmmTopToLive
  ) where


import Reg
import Instruction

import BlockId
import Cmm hiding (RegSet)
import PprCmm()

import Digraph
import Outputable
import Unique
import UniqSet
import UniqFM
import UniqSupply
import Bag
import State
import FastString

import Data.List
import Data.Maybe

-----------------------------------------------------------------------------
type RegSet = UniqSet Reg

type RegMap a = UniqFM a

emptyRegMap :: UniqFM a
emptyRegMap = emptyUFM

type BlockMap a = BlockEnv a

emptyBlockMap :: BlockEnv a
emptyBlockMap = emptyBlockEnv


-- | A top level thing which carries liveness information.
type LiveCmmTop instr
	= GenCmmTop
		CmmStatic
		LiveInfo
		[SCC (LiveBasicBlock instr)]


-- | The register allocator also wants to use SPILL/RELOAD meta instructions,
--	so we'll keep those here.
data InstrSR instr
	-- | A real machine instruction
	= Instr  instr

	-- | spill this reg to a stack slot
	| SPILL  Reg Int

	-- | reload this reg from a stack slot
	| RELOAD Int Reg

instance Instruction instr => Instruction (InstrSR instr) where
	regUsageOfInstr i
	 = case i of
		Instr  instr	-> regUsageOfInstr instr
		SPILL  reg _	-> RU [reg] []
		RELOAD _ reg	-> RU [] [reg]

	patchRegsOfInstr i f
	 = case i of
		Instr instr	-> Instr (patchRegsOfInstr instr f)
		SPILL  reg slot	-> SPILL (f reg) slot
		RELOAD slot reg	-> RELOAD slot (f reg)

	isJumpishInstr i
	 = case i of
		Instr instr	-> isJumpishInstr instr
		_		-> False

	jumpDestsOfInstr i
	 = case i of
		Instr instr	-> jumpDestsOfInstr instr 
		_		-> []

	patchJumpInstr i f
	 = case i of
		Instr instr	-> Instr (patchJumpInstr instr f)
		_		-> i

	mkSpillInstr		= error "mkSpillInstr[InstrSR]: Not making SPILL meta-instr"
	mkLoadInstr		= error "mkLoadInstr[InstrSR]: Not making LOAD meta-instr"

	takeDeltaInstr i
	 = case i of
		Instr instr	-> takeDeltaInstr instr
		_		-> Nothing

	isMetaInstr i
	 = case i of
		Instr instr	-> isMetaInstr instr
		_		-> False

	mkRegRegMoveInstr r1 r2	= Instr (mkRegRegMoveInstr r1 r2)

	takeRegRegMoveInstr i
	 = case i of
		Instr instr	-> takeRegRegMoveInstr instr
		_		-> Nothing

	mkJumpInstr target	= map Instr (mkJumpInstr target)
		


-- | An instruction with liveness information.
data LiveInstr instr
	= LiveInstr (InstrSR instr) (Maybe Liveness)

-- | Liveness information.
-- 	The regs which die are ones which are no longer live in the *next* instruction
-- 	in this sequence.
-- 	(NB. if the instruction is a jump, these registers might still be live
-- 	at the jump target(s) - you have to check the liveness at the destination
-- 	block to find out).

data Liveness
	= Liveness
	{ liveBorn	:: RegSet	-- ^ registers born in this instruction (written to for first time).
	, liveDieRead	:: RegSet	-- ^ registers that died because they were read for the last time.
	, liveDieWrite	:: RegSet }	-- ^ registers that died because they were clobbered by something.


-- | Stash regs live on entry to each basic block in the info part of the cmm code.
data LiveInfo
	= LiveInfo
		[CmmStatic]			-- cmm static stuff
		(Maybe BlockId)			-- id of the first block
		(Maybe (BlockMap RegSet))	-- argument locals live on entry to this block

-- | A basic block with liveness information.
type LiveBasicBlock instr
	= GenBasicBlock (LiveInstr instr)


instance Outputable instr
      => Outputable (InstrSR instr) where

	ppr (Instr realInstr)
	   = ppr realInstr

	ppr (SPILL reg slot)
	   = hcat [
	   	ptext (sLit "\tSPILL"),
		char ' ',
		ppr reg,
		comma,
		ptext (sLit "SLOT") <> parens (int slot)]

	ppr (RELOAD slot reg)
	   = hcat [
	   	ptext (sLit "\tRELOAD"),
		char ' ',
		ptext (sLit "SLOT") <> parens (int slot),
		comma,
		ppr reg]

instance Outputable instr 
      => Outputable (LiveInstr instr) where

	ppr (LiveInstr instr Nothing)
	 = ppr instr

	ppr (LiveInstr instr (Just live))
	 =  ppr instr
		$$ (nest 8
			$ vcat
			[ pprRegs (ptext (sLit "# born:    ")) (liveBorn live)
			, pprRegs (ptext (sLit "# r_dying: ")) (liveDieRead live)
			, pprRegs (ptext (sLit "# w_dying: ")) (liveDieWrite live) ]
		    $+$ space)

	 where 	pprRegs :: SDoc -> RegSet -> SDoc
	 	pprRegs name regs
		 | isEmptyUniqSet regs	= empty
		 | otherwise		= name <> (hcat $ punctuate space $ map ppr $ uniqSetToList regs)

instance Outputable LiveInfo where
	ppr (LiveInfo static firstId liveOnEntry)
		=  (vcat $ map ppr static)
		$$ text "# firstId     = " <> ppr firstId
		$$ text "# liveOnEntry = " <> ppr liveOnEntry



-- | map a function across all the basic blocks in this code
--
mapBlockTop
	:: (LiveBasicBlock instr -> LiveBasicBlock instr)
	-> LiveCmmTop instr -> LiveCmmTop instr

mapBlockTop f cmm
	= evalState (mapBlockTopM (\x -> return $ f x) cmm) ()


-- | map a function across all the basic blocks in this code (monadic version)
--
mapBlockTopM
	:: Monad m
	=> (LiveBasicBlock instr -> m (LiveBasicBlock instr))
	-> LiveCmmTop instr -> m (LiveCmmTop instr)

mapBlockTopM _ cmm@(CmmData{})
	= return cmm

mapBlockTopM f (CmmProc header label params sccs)
 = do	sccs'	<- mapM (mapSCCM f) sccs
 	return	$ CmmProc header label params sccs'

mapSCCM :: Monad m => (a -> m b) -> SCC a -> m (SCC b)
mapSCCM	f (AcyclicSCC x)	
 = do	x'	<- f x
	return	$ AcyclicSCC x'

mapSCCM f (CyclicSCC xs)
 = do	xs'	<- mapM f xs
	return	$ CyclicSCC xs'


-- map a function across all the basic blocks in this code
mapGenBlockTop
	:: (GenBasicBlock             i -> GenBasicBlock            i)
	-> (GenCmmTop d h (ListGraph i) -> GenCmmTop d h (ListGraph i))

mapGenBlockTop f cmm
	= evalState (mapGenBlockTopM (\x -> return $ f x) cmm) ()


-- | map a function across all the basic blocks in this code (monadic version)
mapGenBlockTopM
	:: Monad m
	=> (GenBasicBlock            i  -> m (GenBasicBlock            i))
	-> (GenCmmTop d h (ListGraph i) -> m (GenCmmTop d h (ListGraph i)))

mapGenBlockTopM _ cmm@(CmmData{})
	= return cmm

mapGenBlockTopM f (CmmProc header label params (ListGraph blocks))
 = do	blocks'	<- mapM f blocks
 	return	$ CmmProc header label params (ListGraph blocks')


-- | Slurp out the list of register conflicts and reg-reg moves from this top level thing.
--	Slurping of conflicts and moves is wrapped up together so we don't have
--	to make two passes over the same code when we want to build the graph.
--
slurpConflicts 
	:: Instruction instr
	=> LiveCmmTop instr 
	-> (Bag (UniqSet Reg), Bag (Reg, Reg))

slurpConflicts live
	= slurpCmm (emptyBag, emptyBag) live

 where	slurpCmm   rs  CmmData{}		= rs
 	slurpCmm   rs (CmmProc info _ _ sccs)
		= foldl' (slurpSCC info) rs sccs

	slurpSCC  info rs (AcyclicSCC b)	
		= slurpBlock info rs b

	slurpSCC  info rs (CyclicSCC bs)
		= foldl'  (slurpBlock info) rs bs

	slurpBlock info rs (BasicBlock blockId instrs)	
		| LiveInfo _ _ (Just blockLive)	<- info
		, Just rsLiveEntry		<- lookupBlockEnv blockLive blockId
		, (conflicts, moves)		<- slurpLIs rsLiveEntry rs instrs
		= (consBag rsLiveEntry conflicts, moves)

		| otherwise
		= panic "Liveness.slurpConflicts: bad block"

	slurpLIs rsLive (conflicts, moves) []
		= (consBag rsLive conflicts, moves)

	slurpLIs rsLive rs (LiveInstr _ Nothing     : lis)	
		= slurpLIs rsLive rs lis
		
	slurpLIs rsLiveEntry (conflicts, moves) (LiveInstr instr (Just live) : lis)
	 = let
		-- regs that die because they are read for the last time at the start of an instruction
		--	are not live across it.
	 	rsLiveAcross	= rsLiveEntry `minusUniqSet` (liveDieRead live)

		-- regs live on entry to the next instruction.
		--	be careful of orphans, make sure to delete dying regs _after_ unioning
		--	in the ones that are born here.
	 	rsLiveNext 	= (rsLiveAcross `unionUniqSets` (liveBorn     live))
					        `minusUniqSet`  (liveDieWrite live)

		-- orphan vregs are the ones that die in the same instruction they are born in.
		--	these are likely to be results that are never used, but we still
		--	need to assign a hreg to them..
		rsOrphans	= intersectUniqSets
					(liveBorn live)
					(unionUniqSets (liveDieWrite live) (liveDieRead live))

		--
		rsConflicts	= unionUniqSets rsLiveNext rsOrphans

	  in	case takeRegRegMoveInstr instr of
	  	 Just rr	-> slurpLIs rsLiveNext
		 			( consBag rsConflicts conflicts
					, consBag rr moves) lis

	  	 Nothing	-> slurpLIs rsLiveNext
		 			( consBag rsConflicts conflicts
					, moves) lis


-- | For spill\/reloads
--
--	SPILL  v1, slot1
--	...
--	RELOAD slot1, v2
--
--	If we can arrange that v1 and v2 are allocated to the same hreg it's more likely
--	the spill\/reload instrs can be cleaned and replaced by a nop reg-reg move.
--
--
slurpReloadCoalesce 
	:: forall instr. Instruction instr
	=> LiveCmmTop instr
	-> Bag (Reg, Reg)

slurpReloadCoalesce live
	= slurpCmm emptyBag live

 where	
        slurpCmm :: Bag (Reg, Reg)
                 -> GenCmmTop t t1 [SCC (LiveBasicBlock instr)]
                 -> Bag (Reg, Reg)
        slurpCmm cs CmmData{}	= cs
 	slurpCmm cs (CmmProc _ _ _ sccs)
		= slurpComp cs (flattenSCCs sccs)

        slurpComp :: Bag (Reg, Reg)
                     -> [LiveBasicBlock instr]
                     -> Bag (Reg, Reg)
	slurpComp  cs blocks
	 = let	(moveBags, _)	= runState (slurpCompM blocks) emptyUFM
	   in	unionManyBags (cs : moveBags)

        slurpCompM :: [LiveBasicBlock instr]
                   -> State (UniqFM [UniqFM Reg]) [Bag (Reg, Reg)]
	slurpCompM blocks
	 = do	-- run the analysis once to record the mapping across jumps.
	 	mapM_	(slurpBlock False) blocks

		-- run it a second time while using the information from the last pass.
		--	We /could/ run this many more times to deal with graphical control
		--	flow and propagating info across multiple jumps, but it's probably
		--	not worth the trouble.
		mapM	(slurpBlock True) blocks

        slurpBlock :: Bool -> LiveBasicBlock instr
                   -> State (UniqFM [UniqFM Reg]) (Bag (Reg, Reg))
	slurpBlock propagate (BasicBlock blockId instrs)
	 = do	-- grab the slot map for entry to this block
	 	slotMap		<- if propagate
					then getSlotMap blockId
					else return emptyUFM

	 	(_, mMoves)	<- mapAccumLM slurpLI slotMap instrs
	   	return $ listToBag $ catMaybes mMoves

	slurpLI :: UniqFM Reg 				-- current slotMap
		-> LiveInstr instr
		-> State (UniqFM [UniqFM Reg]) 		-- blockId -> [slot -> reg]
							-- 	for tracking slotMaps across jumps

			 ( UniqFM Reg			-- new slotMap
			 , Maybe (Reg, Reg))		-- maybe a new coalesce edge

	slurpLI slotMap li

		-- remember what reg was stored into the slot
		| LiveInstr (SPILL reg slot) _	<- li
		, slotMap'			<- addToUFM slotMap slot reg
		= return (slotMap', Nothing)

		-- add an edge betwen the this reg and the last one stored into the slot
		| LiveInstr (RELOAD slot reg) _	<- li
		= case lookupUFM slotMap slot of
			Just reg2
			 | reg /= reg2	-> return (slotMap, Just (reg, reg2))
			 | otherwise	-> return (slotMap, Nothing)

			Nothing		-> return (slotMap, Nothing)

		-- if we hit a jump, remember the current slotMap
		| LiveInstr (Instr instr) _	<- li
		, targets			<- jumpDestsOfInstr instr
		, not $ null targets
		= do	mapM_	(accSlotMap slotMap) targets
			return	(slotMap, Nothing)

		| otherwise
		= return (slotMap, Nothing)

	-- record a slotmap for an in edge to this block
	accSlotMap slotMap blockId
		= modify (\s -> addToUFM_C (++) s blockId [slotMap])

	-- work out the slot map on entry to this block
	--	if we have slot maps for multiple in-edges then we need to merge them.
	getSlotMap blockId
	 = do	map		<- get
	 	let slotMaps	= fromMaybe [] (lookupUFM map blockId)
		return		$ foldr mergeSlotMaps emptyUFM slotMaps

	mergeSlotMaps :: UniqFM Reg -> UniqFM Reg -> UniqFM Reg
	mergeSlotMaps map1 map2
		= listToUFM
		$ [ (k, r1)	| (k, r1)	<- ufmToList map1
				, case lookupUFM map2 k of
					Nothing	-> False
					Just r2	-> r1 == r2 ]


-- | Strip away liveness information, yielding NatCmmTop
stripLive 
	:: (Outputable instr, Instruction instr)
	=> LiveCmmTop instr 
	-> NatCmmTop instr

stripLive live
	= stripCmm live

 where	stripCmm (CmmData sec ds)	= CmmData sec ds

 	stripCmm (CmmProc (LiveInfo info (Just first_id) _) label params sccs)
	 = let	final_blocks	= flattenSCCs sccs
		
		-- make sure the block that was first in the input list
		--	stays at the front of the output. This is the entry point
		--	of the proc, and it needs to come first.
		((first':_), rest')
				= partition ((== first_id) . blockId) final_blocks

	   in	CmmProc info label params
                          (ListGraph $ map stripLiveBlock $ first' : rest')

	-- procs used for stg_split_markers don't contain any blocks, and have no first_id.
	stripCmm (CmmProc (LiveInfo info Nothing _) label params [])
	 =	CmmProc info label params (ListGraph [])

	-- If the proc has blocks but we don't know what the first one was, then we're dead.
	stripCmm proc
		 = pprPanic "RegAlloc.Liveness.stripLive: no first_id on proc" (ppr proc)
			

-- | Strip away liveness information from a basic block,
--	and make real spill instructions out of SPILL, RELOAD pseudos along the way.

stripLiveBlock
	:: Instruction instr
	=> LiveBasicBlock instr
	-> NatBasicBlock instr

stripLiveBlock (BasicBlock i lis)
 = 	BasicBlock i instrs'

 where 	(instrs', _)
 		= runState (spillNat [] lis) 0

	spillNat acc []
	 = 	return (reverse acc)

	spillNat acc (LiveInstr (SPILL reg slot) _ : instrs)
	 = do	delta	<- get
	 	spillNat (mkSpillInstr reg delta slot : acc) instrs

	spillNat acc (LiveInstr (RELOAD slot reg) _ : instrs)
	 = do	delta	<- get
	 	spillNat (mkLoadInstr reg delta slot : acc) instrs

	spillNat acc (LiveInstr (Instr instr) _ : instrs)
	 | Just i <- takeDeltaInstr instr
	 = do	put i
	 	spillNat acc instrs

	spillNat acc (LiveInstr (Instr instr) _ : instrs)
	 =	spillNat (instr : acc) instrs


-- | Erase Delta instructions.

eraseDeltasLive 
	:: Instruction instr
	=> LiveCmmTop instr
	-> LiveCmmTop instr

eraseDeltasLive cmm
	= mapBlockTop eraseBlock cmm
 where
 	eraseBlock (BasicBlock id lis)
		= BasicBlock id
		$ filter (\(LiveInstr i _) -> not $ isJust $ takeDeltaInstr i)
		$ lis


-- | Patch the registers in this code according to this register mapping.
--	also erase reg -> reg moves when the reg is the same.
--	also erase reg -> reg moves when the destination dies in this instr.

patchEraseLive
	:: Instruction instr
	=> (Reg -> Reg)
	-> LiveCmmTop instr -> LiveCmmTop instr

patchEraseLive patchF cmm
	= patchCmm cmm
 where
	patchCmm cmm@CmmData{}	= cmm

	patchCmm (CmmProc info label params sccs)
	 | LiveInfo static id (Just blockMap)	<- info
	 = let 	
	 	patchRegSet set	= mkUniqSet $ map patchF $ uniqSetToList set
		blockMap'	= mapBlockEnv patchRegSet blockMap

		info'		= LiveInfo static id (Just blockMap')
	   in	CmmProc info' label params $ map patchSCC sccs

	 | otherwise
	 = panic "RegAlloc.Liveness.patchEraseLive: no blockMap"

	patchSCC (AcyclicSCC b)	 = AcyclicSCC (patchBlock b)
	patchSCC (CyclicSCC  bs) = CyclicSCC  (map patchBlock bs)

 	patchBlock (BasicBlock id lis)
		= BasicBlock id $ patchInstrs lis

	patchInstrs []		= []
	patchInstrs (li : lis)

		| LiveInstr i (Just live)	<- li'
		, Just (r1, r2)	<- takeRegRegMoveInstr i
		, eatMe r1 r2 live
		= patchInstrs lis

		| otherwise
		= li' : patchInstrs lis

		where	li'	= patchRegsLiveInstr patchF li

	eatMe	r1 r2 live
		-- source and destination regs are the same
		| r1 == r2	= True

		-- desination reg is never used
		| elementOfUniqSet r2 (liveBorn live)
		, elementOfUniqSet r2 (liveDieRead live) || elementOfUniqSet r2 (liveDieWrite live)
		= True

		| otherwise	= False


-- | Patch registers in this LiveInstr, including the liveness information.
--
patchRegsLiveInstr
	:: Instruction instr
	=> (Reg -> Reg)
	-> LiveInstr instr -> LiveInstr instr

patchRegsLiveInstr patchF li
 = case li of
	LiveInstr instr Nothing
	 -> LiveInstr (patchRegsOfInstr instr patchF) Nothing

	LiveInstr instr (Just live)
	 -> LiveInstr
	 	(patchRegsOfInstr instr patchF)
		(Just live
			{ -- WARNING: have to go via lists here because patchF changes the uniq in the Reg
			  liveBorn	= mkUniqSet $ map patchF $ uniqSetToList $ liveBorn live
			, liveDieRead	= mkUniqSet $ map patchF $ uniqSetToList $ liveDieRead live
			, liveDieWrite	= mkUniqSet $ map patchF $ uniqSetToList $ liveDieWrite live })


--------------------------------------------------------------------------------
-- | Convert a NatCmmTop to a LiveCmmTop, with empty liveness information

natCmmTopToLive 
	:: Instruction instr
	=> NatCmmTop instr
	-> LiveCmmTop instr

natCmmTopToLive (CmmData i d)
	= CmmData i d

natCmmTopToLive (CmmProc info lbl params (ListGraph []))
	= CmmProc (LiveInfo info Nothing Nothing)
		  lbl params []

natCmmTopToLive (CmmProc info lbl params (ListGraph blocks@(first : _)))
 = let	first_id	= blockId first
	sccs		= sccBlocks blocks
	sccsLive	= map (fmap (\(BasicBlock l instrs) -> 
					BasicBlock l (map (\i -> LiveInstr (Instr i) Nothing) instrs)))
			$ sccs
				
   in	CmmProc (LiveInfo info (Just first_id) Nothing)
		lbl params sccsLive


sccBlocks 
	:: Instruction instr
	=> [NatBasicBlock instr] 
	-> [SCC (NatBasicBlock instr)]

sccBlocks blocks = stronglyConnCompFromEdgedVertices graph
  where
	getOutEdges :: Instruction instr => [instr] -> [BlockId]
	getOutEdges instrs = concat $ map jumpDestsOfInstr instrs

	graph = [ (block, getUnique id, map getUnique (getOutEdges instrs))
		| block@(BasicBlock id instrs) <- blocks ]


---------------------------------------------------------------------------------
-- Annotate code with register liveness information
--
regLiveness
	:: (Outputable instr, Instruction instr)
	=> LiveCmmTop instr
	-> UniqSM (LiveCmmTop instr)

regLiveness (CmmData i d)
	= returnUs $ CmmData i d

regLiveness (CmmProc info lbl params [])
	| LiveInfo static mFirst _	<- info
	= returnUs $ CmmProc
			(LiveInfo static mFirst (Just emptyBlockEnv))
			lbl params []

regLiveness (CmmProc info lbl params sccs)
	| LiveInfo static mFirst _	<- info
	= let	(ann_sccs, block_live)	= computeLiveness sccs

   	  in	returnUs $ CmmProc (LiveInfo static mFirst (Just block_live))
			   lbl params ann_sccs


-- -----------------------------------------------------------------------------
-- | Check ordering of Blocks
--	The computeLiveness function requires SCCs to be in reverse dependent order.
--	If they're not the liveness information will be wrong, and we'll get a bad allocation.
--	Better to check for this precondition explicitly or some other poor sucker will
--	waste a day staring at bad assembly code..
--	
checkIsReverseDependent
	:: Instruction instr
	=> [SCC (LiveBasicBlock instr)]		-- ^ SCCs of blocks that we're about to run the liveness determinator on.
	-> Maybe BlockId			-- ^ BlockIds that fail the test (if any)
	
checkIsReverseDependent sccs'
 = go emptyUniqSet sccs'

 where 	go _ []
	 = Nothing
	
	go blocksSeen (AcyclicSCC block : sccs)
	 = let	dests		= slurpJumpDestsOfBlock block
		blocksSeen'	= unionUniqSets blocksSeen $ mkUniqSet [blockId block]
		badDests	= dests `minusUniqSet` blocksSeen'
	   in	case uniqSetToList badDests of
		 []		-> go blocksSeen' sccs
		 bad : _	-> Just bad
		
	go blocksSeen (CyclicSCC blocks : sccs)
	 = let	dests		= unionManyUniqSets $ map slurpJumpDestsOfBlock blocks
		blocksSeen'	= unionUniqSets blocksSeen $ mkUniqSet $ map blockId blocks
		badDests	= dests `minusUniqSet` blocksSeen'
	   in	case uniqSetToList badDests of
		 []		-> go blocksSeen' sccs
		 bad : _	-> Just bad
		
	slurpJumpDestsOfBlock (BasicBlock _ instrs)
	 	= unionManyUniqSets
		$ map (mkUniqSet . jumpDestsOfInstr) 
		 	[ i | LiveInstr i _ <- instrs]


-- | If we've compute liveness info for this code already we have to reverse
--   the SCCs in each top to get them back to the right order so we can do it again.
reverseBlocksInTops :: LiveCmmTop instr -> LiveCmmTop instr
reverseBlocksInTops top
 = case top of
	CmmData{}			-> top
	CmmProc info lbl params sccs	-> CmmProc info lbl params (reverse sccs)

	
-- | Computing liveness
-- 	
--  On entry, the SCCs must be in "reverse" order: later blocks may transfer
--  control to earlier ones only, else `panic`.
-- 
--  The SCCs returned are in the *opposite* order, which is exactly what we
--  want for the next pass.
--
computeLiveness
	:: (Outputable instr, Instruction instr)
	=> [SCC (LiveBasicBlock instr)]
	-> ([SCC (LiveBasicBlock instr)],	-- instructions annotated with list of registers
						-- which are "dead after this instruction".
	       BlockMap RegSet)			-- blocks annontated with set of live registers
						-- on entry to the block.

computeLiveness sccs
 = case checkIsReverseDependent sccs of
	Nothing		-> livenessSCCs emptyBlockMap [] sccs
	Just bad	-> pprPanic "RegAlloc.Liveness.computeLivenss"
	 			(vcat 	[ text "SCCs aren't in reverse dependent order"
					, text "bad blockId" <+> ppr bad 
					, ppr sccs])

livenessSCCs
       :: Instruction instr
       => BlockMap RegSet
       -> [SCC (LiveBasicBlock instr)]		-- accum
       -> [SCC (LiveBasicBlock instr)]
       -> ( [SCC (LiveBasicBlock instr)]
       	  , BlockMap RegSet)

livenessSCCs blockmap done [] 
	= (done, blockmap)

livenessSCCs blockmap done (AcyclicSCC block : sccs)
 = let	(blockmap', block')	= livenessBlock blockmap block
   in	livenessSCCs blockmap' (AcyclicSCC block' : done) sccs

livenessSCCs blockmap done
	(CyclicSCC blocks : sccs) =
	livenessSCCs blockmap' (CyclicSCC blocks':done) sccs
 where      (blockmap', blocks')
	        = iterateUntilUnchanged linearLiveness equalBlockMaps
	                              blockmap blocks

            iterateUntilUnchanged
                :: (a -> b -> (a,c)) -> (a -> a -> Bool)
                -> a -> b
                -> (a,c)

	    iterateUntilUnchanged f eq a b
	        = head $
	          concatMap tail $
	          groupBy (\(a1, _) (a2, _) -> eq a1 a2) $
	          iterate (\(a, _) -> f a b) $
	          (a, panic "RegLiveness.livenessSCCs")


            linearLiveness 
	    	:: Instruction instr
		=> BlockMap RegSet -> [LiveBasicBlock instr]
            	-> (BlockMap RegSet, [LiveBasicBlock instr])

            linearLiveness = mapAccumL livenessBlock

                -- probably the least efficient way to compare two
                -- BlockMaps for equality.
	    equalBlockMaps a b
	        = a' == b'
	      where a' = map f $ blockEnvToList a
	            b' = map f $ blockEnvToList b
	            f (key,elt) = (key, uniqSetToList elt)



-- | Annotate a basic block with register liveness information.
--
livenessBlock
	:: Instruction instr
	=> BlockMap RegSet
	-> LiveBasicBlock instr
	-> (BlockMap RegSet, LiveBasicBlock instr)

livenessBlock blockmap (BasicBlock block_id instrs)
 = let
 	(regsLiveOnEntry, instrs1)
 		= livenessBack emptyUniqSet blockmap [] (reverse instrs)
 	blockmap'	= extendBlockEnv blockmap block_id regsLiveOnEntry

	instrs2		= livenessForward regsLiveOnEntry instrs1

	output		= BasicBlock block_id instrs2

   in	( blockmap', output)

-- | Calculate liveness going forwards,
--	filling in when regs are born

livenessForward
	:: Instruction instr
	=> RegSet			-- regs live on this instr
	-> [LiveInstr instr] -> [LiveInstr instr]

livenessForward _           []	= []
livenessForward rsLiveEntry (li@(LiveInstr instr mLive) : lis)
	| Nothing		<- mLive
	= li : livenessForward rsLiveEntry lis

	| Just live	<- mLive
	, RU _ written	<- regUsageOfInstr instr
	= let
		-- Regs that are written to but weren't live on entry to this instruction
		--	are recorded as being born here.
		rsBorn		= mkUniqSet
				$ filter (\r -> not $ elementOfUniqSet r rsLiveEntry) written

		rsLiveNext	= (rsLiveEntry `unionUniqSets` rsBorn)
					`minusUniqSet` (liveDieRead live)
					`minusUniqSet` (liveDieWrite live)

	in LiveInstr instr (Just live { liveBorn = rsBorn })
		: livenessForward rsLiveNext lis

livenessForward _ _ 		= panic "RegLiveness.livenessForward: no match"


-- | Calculate liveness going backwards,
--	filling in when regs die, and what regs are live across each instruction

livenessBack
	:: Instruction instr 
	=> RegSet			-- regs live on this instr
	-> BlockMap RegSet   		-- regs live on entry to other BBs
	-> [LiveInstr instr]   		-- instructions (accum)
	-> [LiveInstr instr]		-- instructions
	-> (RegSet, [LiveInstr instr])

livenessBack liveregs _        done []  = (liveregs, done)

livenessBack liveregs blockmap acc (instr : instrs)
 = let	(liveregs', instr')	= liveness1 liveregs blockmap instr
   in	livenessBack liveregs' blockmap (instr' : acc) instrs


-- don't bother tagging comments or deltas with liveness
liveness1 
	:: Instruction instr
	=> RegSet 
	-> BlockMap RegSet 
	-> LiveInstr instr
	-> (RegSet, LiveInstr instr)

liveness1 liveregs _ (LiveInstr instr _)
	| isMetaInstr instr
	= (liveregs, LiveInstr instr Nothing)

liveness1 liveregs blockmap (LiveInstr instr _)

	| not_a_branch
	= (liveregs1, LiveInstr instr
			(Just $ Liveness
			{ liveBorn	= emptyUniqSet
			, liveDieRead	= mkUniqSet r_dying
			, liveDieWrite	= mkUniqSet w_dying }))

	| otherwise
	= (liveregs_br, LiveInstr instr
			(Just $ Liveness
			{ liveBorn	= emptyUniqSet
			, liveDieRead	= mkUniqSet r_dying_br
			, liveDieWrite	= mkUniqSet w_dying }))

	where
	    RU read written = regUsageOfInstr instr

	    -- registers that were written here are dead going backwards.
	    -- registers that were read here are live going backwards.
	    liveregs1   = (liveregs `delListFromUniqSet` written)
	                            `addListToUniqSet` read

	    -- registers that are not live beyond this point, are recorded
	    --  as dying here.
	    r_dying     = [ reg | reg <- read, reg `notElem` written,
	                      not (elementOfUniqSet reg liveregs) ]

	    w_dying     = [ reg | reg <- written,
	                     not (elementOfUniqSet reg liveregs) ]

	    -- union in the live regs from all the jump destinations of this
	    -- instruction.
	    targets      = jumpDestsOfInstr instr -- where we go from here
	    not_a_branch = null targets

	    targetLiveRegs target
                  = case lookupBlockEnv blockmap target of
                                Just ra -> ra
                                Nothing -> emptyRegMap

            live_from_branch = unionManyUniqSets (map targetLiveRegs targets)

	    liveregs_br = liveregs1 `unionUniqSets` live_from_branch

            -- registers that are live only in the branch targets should
            -- be listed as dying here.
            live_branch_only = live_from_branch `minusUniqSet` liveregs
            r_dying_br  = uniqSetToList (mkUniqSet r_dying `unionUniqSets`
                                        live_branch_only)


