{-# LANGUAGE DerivingStrategies #-}

module GHC.CmmToAsm.Reg.Regs (
        Regs(..),
        noRegs,
        addRegMaxFmt, addRegsMaxFmt,
        mkRegsMaxFmt,
        minusCoveredRegs,
        minusRegs,
        unionRegsMaxFmt,
        unionManyRegsMaxFmt,
        intersectRegsMaxFmt,
        shrinkingRegs,
        mapRegs,
        elemRegs, lookupReg,

  ) where

import GHC.Prelude

import GHC.Platform.Reg     ( Reg )
import GHC.CmmToAsm.Format  ( Format, RegWithFormat(..), isVecFormat )

import GHC.Utils.Outputable ( Outputable )
import GHC.Types.Unique     ( Uniquable(..) )
import GHC.Types.Unique.Set

import Data.Coerce ( coerce )

-----------------------------------------------------------------------------

-- | A set of registers, with their respective formats, mostly for use in
-- register liveness analysis.  See Note [Register formats in liveness analysis]
-- in GHC.CmmToAsm.Reg.Liveness.
newtype Regs = Regs { getRegs :: UniqSet RegWithFormat }
  deriving newtype (Eq, Outputable)

maxRegWithFormat :: RegWithFormat -> RegWithFormat -> RegWithFormat
maxRegWithFormat r1@(RegWithFormat _ fmt1) r2@(RegWithFormat _ fmt2)
  = if fmt1 >= fmt2
    then r1
    else r2
  -- Re-using one of the arguments avoids allocating a new 'RegWithFormat',
  -- compared with returning 'RegWithFormat r1 (max fmt1 fmt2)'.

noRegs :: Regs
noRegs = Regs emptyUniqSet

addRegsMaxFmt :: Regs -> [RegWithFormat] -> Regs
addRegsMaxFmt = foldl' addRegMaxFmt

mkRegsMaxFmt :: [RegWithFormat] -> Regs
mkRegsMaxFmt = addRegsMaxFmt noRegs

addRegMaxFmt :: Regs -> RegWithFormat -> Regs
addRegMaxFmt = coerce $ strictAddOneToUniqSet_C maxRegWithFormat
  -- Don't build up thunks when combining with 'maxRegWithFormat'

-- | Remove 2nd argument registers from the 1st argument, but only
-- if the format in the second argument is at least as large as the format
-- in the first argument.
minusCoveredRegs :: Regs -> Regs -> Regs
minusCoveredRegs = coerce $ minusUniqSet_C f
  where
    f :: RegWithFormat -> RegWithFormat -> Maybe RegWithFormat
    f r1@(RegWithFormat _ fmt1) (RegWithFormat _ fmt2) =
      if fmt2 >= fmt1
           ||
         not ( isVecFormat fmt1 )
          -- See Wrinkle [Don't allow scalar partial writes]
          -- in Note [Register formats in liveness analysis] in GHC.CmmToAsm.Reg.Liveness.
      then Nothing
      else Just r1

-- | Remove 2nd argument registers from the 1st argument, regardless of format.
--
-- See also 'minusCoveredRegs', which looks at the formats.
minusRegs :: Regs -> Regs -> Regs
minusRegs = coerce $ minusUniqSet @RegWithFormat

unionRegsMaxFmt :: Regs -> Regs -> Regs
unionRegsMaxFmt = coerce $ strictUnionUniqSets_C maxRegWithFormat
  -- Don't build up thunks when combining with 'maxRegWithFormat'

unionManyRegsMaxFmt :: [Regs] -> Regs
unionManyRegsMaxFmt = coerce $ strictUnionManyUniqSets_C maxRegWithFormat
  -- Don't build up thunks when combining with 'maxRegWithFormat'

intersectRegsMaxFmt :: Regs -> Regs -> Regs
intersectRegsMaxFmt = coerce $ strictIntersectUniqSets_C maxRegWithFormat
  -- Don't build up thunks when combining with 'maxRegWithFormat'

-- | Computes the set of registers in both arguments whose size is smaller in
-- the second argument than in the first.
shrinkingRegs :: Regs -> Regs -> Regs
shrinkingRegs = coerce $ minusUniqSet_C f
  where
    f :: RegWithFormat -> RegWithFormat -> Maybe RegWithFormat
    f (RegWithFormat _ fmt1) r2@(RegWithFormat _ fmt2)
      | fmt2 < fmt1
      = Just r2
      | otherwise
      = Nothing

-- | Map a function that may change the 'Unique' of the register,
-- which entails going via lists.
--
-- See Note [UniqSet invariant] in GHC.Types.Unique.Set.
mapRegs :: (Reg -> Reg) -> Regs -> Regs
mapRegs f (Regs live) =
  Regs $
    mapUniqSet (\ (RegWithFormat r fmt) -> RegWithFormat (f r) fmt) live

elemRegs :: Reg -> Regs -> Bool
elemRegs r (Regs live) = elemUniqSet_Directly (getUnique r) live

lookupReg :: Reg -> Regs -> Maybe Format
lookupReg r (Regs live) =
  regWithFormat_format <$> lookupUniqSet_Directly live (getUnique r)
