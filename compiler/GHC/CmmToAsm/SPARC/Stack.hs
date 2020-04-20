module GHC.CmmToAsm.SPARC.Stack (
        spRel,
        fpRel,
        spillSlotToOffset,
        maxSpillSlots
)

where

import GHC.Prelude

import GHC.CmmToAsm.SPARC.AddrMode
import GHC.CmmToAsm.SPARC.Regs
import GHC.CmmToAsm.SPARC.Base
import GHC.CmmToAsm.SPARC.Imm
import GHC.CmmToAsm.Config

import GHC.Utils.Outputable

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
spillSlotToOffset :: NCGConfig -> Int -> Int
spillSlotToOffset config slot
        | slot >= 0 && slot < maxSpillSlots config
        = 64 + spillSlotSize * slot

        | otherwise
        = pprPanic "spillSlotToOffset:"
                      (   text "invalid spill location: " <> int slot
                      $$  text "maxSpillSlots:          " <> int (maxSpillSlots config))


-- | The maximum number of spill slots available on the C stack.
--      If we use up all of the slots, then we're screwed.
--
--      Why do we reserve 64 bytes, instead of using the whole thing??
--              -- BL 2009/02/15
--
maxSpillSlots :: NCGConfig -> Int
maxSpillSlots config
        = ((ncgSpillPreallocSize config - 64) `div` spillSlotSize) - 1
