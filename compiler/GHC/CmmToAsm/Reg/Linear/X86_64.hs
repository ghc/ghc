-- | Free regs map for x86_64, and the register the linear allocator takes where
-- none of a virtual register's preferences is free ('fallbackReg')
module GHC.CmmToAsm.Reg.Linear.X86_64 where

import GHC.Prelude

import GHC.CmmToAsm.X86.Regs
import GHC.Platform.Reg.Class.Unified
import GHC.Platform.Reg
import GHC.Platform
import GHC.Utils.Outputable

import GHC.CmmToAsm.Reg.Linear.Base (RA_Context (..), Loc (..), regsOfLoc)
import GHC.Cmm.Reg (GlobalReg (..))
import GHC.Platform.X86_64 (globalRegMaybe)
import GHC.Types.Unique (Unique)
import GHC.Types.Unique.FM (lookupWithDefaultUFM_Directly, nonDetEltsUFM)
import GHC.Utils.Panic (panic)

import Data.Maybe (fromMaybe, listToMaybe)
import Data.Word

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
fallbackReg :: RA_Context FreeRegs -> Bool -> Unique -> [RealReg]
            -> RealReg
fallbackReg RA_Context { rc_jumpLocs = locs, rc_clobbers = clobbers }
            noPrefs u free@(r0 : _)
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
        reserved = [ r | assig <- locs, Loc loc _ <- nonDetEltsUFM assig
                       , r <- regsOfLoc loc ]
        -- those no instruction writes while the vreg is live past it, then the
        -- rest, R1's last in each part
        part p = [ r | r <- fs, r /= r1, p r ]
                 ++ [ r1 | r1 `elem` fs, p r1 ]
        {-# INLINE part #-}  -- so that the lists fuse
    in fromMaybe r0 (listToMaybe (part kept ++ part (not . kept)))
fallbackReg _ _ _ [] = panic "fallbackReg: no free register"

-- | R1's register, which an evaluation and a return write.
r1Reg :: RealReg
r1Reg = fromMaybe (panic "r1Reg: R1 has no register")
                  (globalRegMaybe (VanillaReg 1))
