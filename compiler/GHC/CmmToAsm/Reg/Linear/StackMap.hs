
-- | The assignment of virtual registers to stack slots

--      We have lots of stack slots. Memory-to-memory moves are a pain on most
--      architectures. Therefore, we avoid having to generate memory-to-memory moves
--      by simply giving every virtual register its own stack slot.

--      The StackMap stack map keeps track of virtual register - stack slot
--      associations and of which stack slots are still free. Once it has been
--      associated, a stack slot is never "freed" or removed from the StackMap again,
--      it remains associated until we are done with the current CmmProc.
--
module GHC.CmmToAsm.Reg.Linear.StackMap (
        StackSlot,
        StackMap(..),
        emptyStackMap,
        getStackSlotFor,
        getStackUse
)

where

import GHC.Prelude

import GHC.Types.Unique.FM
import GHC.Types.Unique
import GHC.CmmToAsm.Format


-- | Identifier for a stack slot.
type StackSlot = Int

data StackMap
        = StackMap
        { -- | The slots that are still available to be allocated.
          stackMapNextFreeSlot  :: !Int

          -- See Note [UniqFM and the register allocator]
          -- | Assignment of vregs to stack slots.
        , stackMapAssignment    :: UniqFM Unique StackSlot }


-- | An empty stack map, with all slots available.
emptyStackMap :: StackMap
emptyStackMap = StackMap 0 emptyUFM


-- | If this vreg unique already has a stack assignment then return the slot number,
--      otherwise allocate a new slot, and update the map.
--
getStackSlotFor :: StackMap -> Format -> Unique -> (StackMap, Int)

getStackSlotFor fs@(StackMap _ reserved) _fmt regUnique
  | Just slot <- lookupUFM reserved regUnique  =  (fs, slot)

getStackSlotFor (StackMap freeSlot reserved) fmt regUnique =
  let
    nbSlots = (formatInBytes fmt + 7) `div` 8
  in
    (StackMap (freeSlot+nbSlots) (addToUFM reserved regUnique freeSlot), freeSlot)

-- | Return the number of stack slots that were allocated
getStackUse :: StackMap -> Int
getStackUse (StackMap freeSlot _) = freeSlot

