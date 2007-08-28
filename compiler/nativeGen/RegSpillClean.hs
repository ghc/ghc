-- | Clean out unneeded spill/reload instrs
--
-- * Handling of join points
--
--   B1:                          B2:
--    ...                          ...
--       RELOAD SLOT(0), %r1          RELOAD SLOT(0), %r1
--       ... A ...                    ... B ...
--       jump B3                      jump B3
--
--                B3: ... C ...
--                    RELOAD SLOT(0), %r1
--                    ...
--
-- the plan:
--	So long as %r1 hasn't been written to in A, B or C then we don't need the
--	reload in B3.
--
--	What we really care about here is that on the entry to B3, %r1 will always
--	have the same value that is in SLOT(0) (ie, %r1 is _valid_)
--
--	This also works if the reloads in B1/B2 were spills instead, because
--	spilling %r1 to a slot makes that slot have the same value as %r1.
--
--
module RegSpillClean (
	cleanSpills
)
where

import RegLiveness
import RegAllocInfo
import MachRegs
import MachInstrs
import Cmm

import UniqSet
import UniqFM
import State
import Outputable

import Data.Maybe
import Data.List

type Slot	= Int

-- | Clean out unneeded spill/reloads from this top level thing.
cleanSpills :: LiveCmmTop -> LiveCmmTop
cleanSpills cmm
	= evalState (cleanSpin 0 cmm) initCleanS

-- | do one pass of cleaning
cleanSpin :: Int -> LiveCmmTop -> CleanM LiveCmmTop

{-
cleanSpin spinCount code
 = do	jumpValid	<- gets sJumpValid
	pprTrace "cleanSpin"
	 	(  int spinCount
		$$ text "--- code"
		$$ ppr code
		$$ text "--- joins"
		$$ ppr jumpValid)
	 $ cleanSpin' spinCount code
-}

cleanSpin spinCount code
 = do
 	-- init count of cleaned spills/reloads
	modify $ \s -> s
		{ sCleanedSpillsAcc	= 0
		, sCleanedReloadsAcc	= 0 }

 	code'	<- mapBlockTopM cleanBlock code

	-- During the cleaning of each block we collected information about what regs
	--	were valid across each jump. Based on this, work out whether it will be
	--	safe to erase reloads after join points for the next pass.
	collateJoinPoints

	-- remember how many spills/reloads we cleaned in this pass
	spills		<- gets sCleanedSpillsAcc
	reloads		<- gets sCleanedReloadsAcc
	modify $ \s -> s
		{ sCleanedCount	= (spills, reloads) : sCleanedCount s }

	-- if nothing was cleaned in this pass or the last one
	--	then we're done and it's time to bail out
	cleanedCount	<- gets sCleanedCount
	if take 2 cleanedCount == [(0, 0), (0, 0)]
	   then return code

	-- otherwise go around again
	   else cleanSpin (spinCount + 1) code'


-- | Clean one basic block
cleanBlock :: LiveBasicBlock -> CleanM LiveBasicBlock
cleanBlock (BasicBlock id instrs)
 = do	jumpValid	<- gets sJumpValid
 	let assoc	= case lookupUFM jumpValid id of
				Just assoc	-> assoc
				Nothing		-> emptyAssoc

 	instrs_reload	<- cleanReload assoc        [] instrs
 	instrs_spill	<- cleanSpill  emptyUniqSet [] instrs_reload
	return	$ BasicBlock id instrs_spill


-- | Clean out unneeded reload instructions.
--	Walking forwards across the code
--	  On a reload, if we know a reg already has the same value as a slot
--	  then we don't need to do the reload.
--
cleanReload
	:: Assoc Reg Slot 	-- ^ a reg and slot are associated when they have the same value.
	-> [LiveInstr]		-- ^ acc
	-> [LiveInstr] 		-- ^ instrs to clean (in backwards order)
	-> CleanM [LiveInstr]	-- ^ cleaned instrs  (in forward   order)

cleanReload assoc acc []
	= return acc

cleanReload assoc acc (li@(Instr instr live) : instrs)

	| SPILL reg slot	<- instr
	= let	assoc'	= addAssoc reg slot	-- doing the spill makes reg and slot the same value
			$ deleteBAssoc slot 	-- slot value changes on spill
			$ assoc
	  in	cleanReload assoc' (li : acc) instrs

	| RELOAD slot reg	<- instr
	= if elemAssoc reg slot assoc

           -- reg and slot had the same value before reload
	   --	we don't need the reload.
	   then	do
		modify $ \s -> s { sCleanedReloadsAcc = sCleanedReloadsAcc s + 1 }
	   	cleanReload assoc acc instrs

	   -- reg and slot had different values before reload
	   else
	    let assoc'	= addAssoc reg slot	-- doing the reload makes reg and slot the same value
			$ deleteAAssoc reg	-- reg value changes on reload
			$ assoc
	    in	cleanReload assoc' (li : acc) instrs

	-- on a jump, remember the reg/slot association.
	| targets		<- jumpDests instr []
	, not $ null targets
	= do	mapM_ (accJumpValid assoc) targets
		cleanReload assoc (li : acc) instrs

	-- writing to a reg changes its value.
	| RU read written	<- regUsage instr
	= let assoc'	= foldr deleteAAssoc assoc written
	  in  cleanReload assoc' (li : acc) instrs


-- | Clean out unneeded spill instructions.
--	Walking backwards across the code.
--	 If there were no reloads from a slot between a spill and the last one
--	 then the slot was never read and we don't need the spill.

cleanSpill
	:: UniqSet Int 		-- ^ slots that have been spilled, but not reload from
	-> [LiveInstr]		-- ^ acc
	-> [LiveInstr]		-- ^ instrs to clean (in forwards order)
	-> CleanM [LiveInstr]	-- ^ cleaned instrs  (in backwards order)

cleanSpill unused acc []
	= return  acc

cleanSpill unused acc (li@(Instr instr live) : instrs)
	| SPILL reg slot	<- instr
	= if elementOfUniqSet slot unused

	   -- we can erase this spill because the slot won't be read until after the next one
	   then do
		modify $ \s -> s { sCleanedSpillsAcc = sCleanedSpillsAcc s + 1 }
	   	cleanSpill unused acc instrs

	   else do
		-- slots start off unused
		let unused'	= addOneToUniqSet unused slot
	   	cleanSpill unused' (li : acc) instrs

	-- if we reload from a slot then it's no longer unused
	| RELOAD slot reg	<- instr
	, unused'		<- delOneFromUniqSet unused slot
	= cleanSpill unused' (li : acc) instrs

	-- some other instruction
	| otherwise
	= cleanSpill unused (li : acc) instrs


-- collateJoinPoints:
--
-- | Look at information about what regs were valid across jumps and work out
--	whether it's safe to avoid reloads after join points.
--
collateJoinPoints :: CleanM ()
collateJoinPoints
 = modify $ \s -> s
 	{ sJumpValid	= mapUFM intersects (sJumpValidAcc s)
	, sJumpValidAcc	= emptyUFM }

intersects :: [Assoc Reg Slot]	-> Assoc Reg Slot
intersects []		= emptyAssoc
intersects assocs	= foldl1' intersectAssoc assocs



---------------
type CleanM = State CleanS
data CleanS
	= CleanS
	{ -- regs which are valid at the start of each block.
	  sJumpValid		:: UniqFM (Assoc Reg Slot)

 	  -- collecting up what regs were valid across each jump.
	  --	in the next pass we can collate these and write the results
	  --	to sJumpValid.
	, sJumpValidAcc		:: UniqFM [Assoc Reg Slot]

	  -- spills/reloads cleaned each pass (latest at front)
	, sCleanedCount		:: [(Int, Int)]

	  -- spills/reloads that have been cleaned in this pass so far.
	, sCleanedSpillsAcc	:: Int
	, sCleanedReloadsAcc	:: Int }

initCleanS
	= CleanS
	{ sJumpValid		= emptyUFM
	, sJumpValidAcc		= emptyUFM

	, sCleanedCount		= []

	, sCleanedSpillsAcc	= 0
	, sCleanedReloadsAcc	= 0 }


-- | Remember that these regs were valid before a jump to this block
accJumpValid :: Assoc Reg Slot -> BlockId -> CleanM ()
accJumpValid regs target
 	= modify $ \s -> s {
		sJumpValidAcc = addToUFM_C (++)
					(sJumpValidAcc s)
					target
					[regs] }


--------------
-- An association table / many to many mapping.
--	TODO: 	implement this better than a simple association list.
--		two maps of sets, one for each direction would be better
--
data Assoc a b
	= Assoc
	{ aList	:: [(a, b)] }

-- | an empty association
emptyAssoc :: Assoc a b
emptyAssoc = Assoc { aList = [] }


-- | add an association to the table.
addAssoc
	:: (Eq a, Eq b)
	=> a -> b -> Assoc a b -> Assoc a b

addAssoc a b m	= m { aList = (a, b) : aList m }


-- | check if these two things are associated
elemAssoc
	:: (Eq a, Eq b)
	=> a -> b -> Assoc a b -> Bool
elemAssoc a b m	= elem (a, b) $ aList m


-- | delete all associations with this A element
deleteAAssoc
	:: Eq a
	=> a -> Assoc a b -> Assoc a b

deleteAAssoc x m
	= m { aList = [ (a, b)	| (a, b) <- aList m
				, a /= x ] }


-- | delete all associations with this B element
deleteBAssoc
	:: Eq b
	=> b -> Assoc a b -> Assoc a b

deleteBAssoc x m
	= m { aList = [ (a, b) 	| (a, b) <- aList m
				, b /= x ] }


-- | intersect two associations
intersectAssoc
	:: (Eq a, Eq b)
	=> Assoc a b -> Assoc a b -> Assoc a b

intersectAssoc a1 a2
	= emptyAssoc
	{ aList	= intersect (aList a1) (aList a2) }

