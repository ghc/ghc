
{-# OPTIONS -fno-warn-tabs #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and
-- detab the module (please do the detabbing in a separate patch). See
--     http://hackage.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#TabsvsSpaces
-- for details

-- | The assignment of virtual registers to stack slots

-- 	We have lots of stack slots. Memory-to-memory moves are a pain on most
-- 	architectures. Therefore, we avoid having to generate memory-to-memory moves
-- 	by simply giving every virtual register its own stack slot.

-- 	The StackMap stack map keeps track of virtual register - stack slot
-- 	associations and of which stack slots are still free. Once it has been
-- 	associated, a stack slot is never "freed" or removed from the StackMap again,
-- 	it remains associated until we are done with the current CmmProc.
--
module RegAlloc.Linear.StackMap (
	StackSlot,
	StackMap(..),
	emptyStackMap,
	getStackSlotFor
)

where

import RegAlloc.Linear.FreeRegs

import Outputable
import Platform
import UniqFM
import Unique


-- | Identifier for a stack slot.
type StackSlot = Int

data StackMap 
	= StackMap 
	{ -- | The slots that are still available to be allocated.
	  stackMapFreeSlots	:: [StackSlot]

	  -- | Assignment of vregs to stack slots.
	, stackMapAssignment	:: UniqFM StackSlot }


-- | An empty stack map, with all slots available.
emptyStackMap :: Platform -> StackMap
emptyStackMap platform = StackMap [0 .. maxSpillSlots platform] emptyUFM


-- | If this vreg unique already has a stack assignment then return the slot number,
--	otherwise allocate a new slot, and update the map.
--
getStackSlotFor :: StackMap -> Unique -> (StackMap, Int)

getStackSlotFor (StackMap [] _) _

        -- This happens all the time when trying to compile darcs' SHA1.hs, see Track #1993
	--	SHA1.lhs has also been added to the Crypto library on Hackage,
	--	so we see this all the time.  
	--
	-- It would be better to automatically invoke the graph allocator, or do something
	--	else besides panicing, but that's a job for a different day.  -- BL 2009/02
	--
	= panic $   "RegAllocLinear.getStackSlotFor: out of stack slots\n"
		++  "   If you are trying to compile SHA1.hs from the crypto library then this\n"
		++  "   is a known limitation in the linear allocator.\n"
		++  "\n"
		++  "   Try enabling the graph colouring allocator with -fregs-graph instead."
		++  "   You can still file a bug report if you like.\n"
		
getStackSlotFor fs@(StackMap (freeSlot:stack') reserved) reg =
    case lookupUFM reserved reg of
    	Just slot	-> (fs, slot)
    	Nothing		-> (StackMap stack' (addToUFM reserved reg freeSlot), freeSlot)

