module SPARC.Stack (
        spRel,
        fpRel,
        spillSlotToOffset,
        maxSpillSlots
)

where

import SPARC.AddrMode
import SPARC.Regs
import SPARC.Base
import SPARC.Imm

import DynFlags
import Outputable

-- | Get an AddrMode relative to the address in sp.
--      This gives us a stack relative addressing mode for volatile
--      temporaries and for excess call arguments.
--
spRel :: Int            -- ^ stack offset in words, positive or negative
      -> AddrMode

spRel n = AddrRegImm sp (ImmInt (n * wordLength))


-- | Get an address relative to the frame pointer.
--      This doesn't work work for offsets greater than 13 bits; we just hope for the best
--
fpRel :: Int -> AddrMode
fpRel n
        = AddrRegImm fp (ImmInt (n * wordLength))


-- | Convert a spill slot number to a *byte* offset, with no sign.
--
spillSlotToOffset :: DynFlags -> Int -> Int
spillSlotToOffset dflags slot
        | slot >= 0 && slot < maxSpillSlots dflags
        = 64 + spillSlotSize * slot

        | otherwise
        = pprPanic "spillSlotToOffset:"
                      (   text "invalid spill location: " <> int slot
                      $$  text "maxSpillSlots:          " <> int (maxSpillSlots dflags))


-- | The maximum number of spill slots available on the C stack.
--      If we use up all of the slots, then we're screwed.
--
--      Why do we reserve 64 bytes, instead of using the whole thing??
--              -- BL 2009/02/15
--
maxSpillSlots :: DynFlags -> Int
maxSpillSlots dflags
        = ((spillAreaLength dflags - 64) `div` spillSlotSize) - 1
