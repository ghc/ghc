-- | Free regs map for x86_64, and the register the linear allocator takes where
-- none of a virtual register's preferences is free ('fallbackReg')
module GHC.CmmToAsm.Reg.Linear.X86_64 where

import GHC.Prelude

import GHC.CmmToAsm.X86.Regs
import GHC.Platform.Reg.Class.Unified
import GHC.Platform.Reg
import GHC.Platform
import GHC.Utils.Outputable

import GHC.CmmToAsm.Reg.Linear.Base (RA_Context (..), Loc (..), regsOfLoc,
                                     BlockAssignment, lookupBlockAssignment,
                                     DyingMoves (..), liveInto)
import GHC.CmmToAsm.Reg.Liveness (BlockMap)
import GHC.CmmToAsm.Reg.Regs (Regs)
import GHC.Cmm.BlockId (BlockId)
import GHC.Cmm.Dataflow.Label (LabelMap, mapLookup, mapFindWithDefault,
                               setMember)
import GHC.Cmm.Reg (GlobalReg (..))
import GHC.Platform.X86_64 (globalRegMaybe)
import GHC.Types.Unique (Unique, getUnique)
import GHC.Types.Unique.FM (lookupWithDefaultUFM_Directly, nonDetEltsUFM)
import GHC.Utils.Panic (panic)

import Data.Maybe (fromMaybe, listToMaybe, isNothing)
import Data.Word
import GHC.Exts (lazy)

newtype FreeRegs = FreeRegs Word64
    deriving (Show,Outputable)

noFreeRegs :: FreeRegs
noFreeRegs = FreeRegs 0

releaseReg :: RealReg -> FreeRegs -> FreeRegs
releaseReg (RealRegSingle n) (FreeRegs f)
        = FreeRegs (setBit f n)

initFreeRegs :: Platform -> FreeRegs
initFreeRegs platform
        = foldl' (flip releaseReg) noFreeRegs (allocatableRegs platform)

getFreeRegs :: RegClass -> FreeRegs -> [RealReg] -- lazily
getFreeRegs cls (FreeRegs f) =
  case cls of
    RcInteger ->
      [ RealRegSingle i
      | i <- intregnos PW8
      , testBit f i
      ]
    RcFloatOrVector ->
      [ RealRegSingle i
      | i <- xmmregnos PW8
      , testBit f i
      ]

allocateReg :: RealReg -> FreeRegs -> FreeRegs
allocateReg (RealRegSingle r) (FreeRegs f)
        = FreeRegs (clearBit f r)

-- | 'GHC.CmmToAsm.Reg.Linear.FreeRegs.frFallbackReg' on
-- x86-64: of the free registers, the first in the order
-- Note [Register preferences in the linear allocator] in
-- GHC.CmmToAsm.Reg.Linear gives.
fallbackReg :: RA_Context FreeRegs -> BlockAssignment -> Bool -> Unique
            -> [RealReg] -> RealReg
fallbackReg RA_Context { rc_jumpLocs = locs, rc_block = blk, rc_succs = succs
                       , rc_joins = joins, rc_live = live, rc_dying = dying
                       , rc_clobbers = clobbers }
            bassig noPrefs u free@(r0 : _)
  = let !r1 = r1Reg
        !cl = lookupWithDefaultUFM_Directly clobbers 0 u
        kept (RealRegSingle n) = not (testBit cl n)
        -- the free ones to choose from: for one with preferences, those no jump
        -- target's assignment gives a virtual register if any remain, and all
        -- of them otherwise
        fs | noPrefs   = free
           | otherwise = case filter (`notElem` reserved) free of
                           []  -> free
                           fs' -> fs'
        -- and of those, the ones the blocks up to three jumps ahead do not
        -- move another vreg into where it dies while the vreg is live into
        -- them, if any remain: given one, the vreg would keep that one from
        -- being computed in it, or have to move out of its way. Not for a vreg
        -- live across a C call, which leaves its register there anyway, nor
        -- where a jump target not yet recorded that the vreg is live into has
        -- other predecessors, which then have to give it the same register
        !ahead | cl .&. cCallClobbers == cCallClobbers = 0
               | otherwise = movedAhead
        !wanted = ahead .&. complement (ahead `shiftR` 32)
        ok (RealRegSingle n) = not (testBit wanted n)
        joinNext = any (\b -> b `setMember` joins && liveInto u live b
                              && isNothing (lookupBlockAssignment b bassig))
                       (succsOf blk)
        -- the real registers that the blocks up to three jumps ahead of the
        -- current one, those the vreg is live into, move vregs into where they
        -- die, as bits by register number, and above them, shifted by 32, those
        -- they move the vreg itself into
        movedAhead :: Word64
        movedAhead = aheadWalks u live succs dying 3 0 (succsOf blk)
        succsOf b = mapFindWithDefault [] b succs
        reserved = [ r | assig <- locs, Loc loc _ <- nonDetEltsUFM assig
                       , r <- regsOfLoc loc ]
        -- those no instruction writes while the vreg is live past it, then the
        -- rest, R1's last in each part
        part q p = [ r | r <- fs, q r, p r, r /= r1 ]
                   ++ [ r1 | r1 `elem` fs, q r1, p r1 ]
        {-# INLINE part #-}  -- so that the lists fuse
        any' _ = True
        -- The choice stays the list's own box, not a new one per call. So
        -- r /= r1 comes last in part: tested first, against a literal, it
        -- leaves the element to be rebuilt. And rd is returned under lazy:
        -- the result being a small product, demand analysis would otherwise
        -- unbox rd (Note [Unboxed demand on function bodies returning small
        -- products] in GHC.Core.Opt.DmdAnal), and worker/wrapper box it anew.
        -- Together they save 0.067% of T3294's compile allocation at -O0, on
        -- GHC 10.1.20261103; neither alone saves any.
        rd = fromMaybe r0 (listToMaybe (part any' kept ++ part any' (not . kept)))
    in if ok rd then lazy rd
       else case listToMaybe (part ok kept ++ part ok (not . kept)) of
              Just r | not joinNext -> r
              _                     -> lazy rd
fallbackReg _ _ _ _ [] = panic "fallbackReg: no free register"

-- | R1's register, which an evaluation and a return write.
r1Reg :: RealReg
r1Reg = fromMaybe (panic "r1Reg: R1 has no register")
                  (globalRegMaybe (VanillaReg 1))

-- | The registers of x86-64 that a C call writes on both System V and Windows,
-- rax, rcx, rdx and r8 to r11, as bits by register number: a vreg whose
-- clobbering writes include all of them is live across a C call.
cCallClobbers :: Word64
cCallClobbers = 0xF0D

-- | fallbackReg's walk: from the given blocks, up to the given number
-- of jumps, through the blocks the vreg with the given unique is live into,
-- the registers their moves into real registers where vregs die go to, as bits
-- by register number, and above them, shifted by 32, those of the vreg's own
-- such moves, or'ed into the given ones. Top-level, so that a call allocates
-- nothing.
aheadWalks :: Unique -> BlockMap Regs -> LabelMap [BlockId]
           -> LabelMap DyingMoves
           -> Int -> Word64 -> [BlockId] -> Word64
aheadWalks !_ _ _ _ !_ !acc [] = acc
aheadWalks u live succs dying d acc (b : bs)
  = aheadWalks u live succs dying d (aheadWalk u live succs dying d acc b) bs

aheadWalk :: Unique -> BlockMap Regs -> LabelMap [BlockId]
          -> LabelMap DyingMoves
          -> Int -> Word64 -> BlockId -> Word64
aheadWalk !u live succs dying !d !acc b
  | not (liveInto u live b) = acc
  | Just (DyingMoves m ds) <- mapLookup b dying
  , let !acc' = acc .|. m .|. lastDestBit u ds
  = if d <= 1 then acc'
    else aheadWalks u live succs dying (d - 1) acc' (mapFindWithDefault [] b succs)
  | d <= 1    = acc
  | otherwise = aheadWalks u live succs dying (d - 1) acc (mapFindWithDefault [] b succs)

-- | As 'GHC.CmmToAsm.Reg.Linear.lastDest', the register as a bit above 32
-- by its number, or 0.
lastDestBit :: Unique -> [(VirtualReg, RealReg)] -> Word64
lastDestBit !u = go 0
  where
    go !acc [] = acc
    go !acc ((v, RealRegSingle n) : ms)
      | getUnique v == u = go (bit (n + 32)) ms
      | otherwise        = go acc ms
