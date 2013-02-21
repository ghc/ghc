
-- | The assignment of virtual registers to stack slots

--      We have lots of stack slots. Memory-to-memory moves are a pain on most
--      architectures. Therefore, we avoid having to generate memory-to-memory moves
--      by simply giving every virtual register its own stack slot.

--      The StackMap stack map keeps track of virtual register - stack slot
--      associations and of which stack slots are still free. Once it has been
--      associated, a stack slot is never "freed" or removed from the StackMap again,
--      it remains associated until we are done with the current CmmProc.
--
module RegAlloc.Linear.StackMap (
        StackSlot,
        StackMap(..),
        emptyStackMap,
        getStackSlotFor,
        getStackUse
)

where

import DynFlags
import UniqFM
import Unique


-- | Identifier for a stack slot.
type StackSlot = Int

data StackMap 
        = StackMap 
        { -- | The slots that are still available to be allocated.
          stackMapNextFreeSlot  :: !Int

          -- | Assignment of vregs to stack slots.
        , stackMapAssignment    :: UniqFM StackSlot }


-- | An empty stack map, with all slots available.
emptyStackMap :: DynFlags -> StackMap
emptyStackMap _ = StackMap 0 emptyUFM


-- | If this vreg unique already has a stack assignment then return the slot number,
--      otherwise allocate a new slot, and update the map.
--
getStackSlotFor :: StackMap -> Unique -> (StackMap, Int)

getStackSlotFor fs@(StackMap _ reserved) reg
  | Just slot <- lookupUFM reserved reg  =  (fs, slot)

getStackSlotFor (StackMap freeSlot reserved) reg =
    (StackMap (freeSlot+1) (addToUFM reserved reg freeSlot), freeSlot)

-- | Return the number of stack slots that were allocated
getStackUse :: StackMap -> Int
getStackUse (StackMap freeSlot _) = freeSlot

