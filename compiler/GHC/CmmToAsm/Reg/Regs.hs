{-# LANGUAGE DerivingStrategies #-}

module GHC.CmmToAsm.Reg.Regs (
        Regs(..),
        noRegs,
        addRegMaxFmt, addRegsMaxFmt,
        mkRegsMaxFmt,
        minusCoveredRegs, delCoveredRegs,
        minusRegs,
        unionRegsMaxFmt,
        unionManyRegsMaxFmt,
        intersectRegsMaxFmt,
        mapRegs,
        elemRegs, lookupReg,

  ) where

import GHC.Prelude

import GHC.Platform.Reg     ( Reg )
import GHC.CmmToAsm.Format  ( Format(..), RegWithFormat(..), isVecFormat,
                              formatToWidth )

import GHC.Utils.Outputable ( Outputable )
import GHC.Types.Unique     ( Uniquable(..) )
import GHC.Types.Unique.FM  ( ufmToIntMap )
import GHC.Types.Unique.Set

import Data.Coerce ( coerce )
import Data.Functor.Classes ( liftEq )

-----------------------------------------------------------------------------

-- | A set of registers, with their respective formats, mostly for use in
-- register liveness analysis.  See Note [Register formats in liveness analysis]
-- in GHC.CmmToAsm.Reg.Liveness.
newtype Regs = Regs { getRegs :: UniqSet RegWithFormat }
  deriving newtype Outputable

-- | The same registers at the same formats (#27619).
instance Eq Regs where
  Regs a == Regs b = liftEq sameFormat (toMap a) (toMap b)
    where toMap = ufmToIntMap . getUniqSet
          sameFormat r q = regWithFormat_format r == regWithFormat_format q

-- | Compares two formats of a register: two vectors by width (#27619),
-- 'Format''s derived 'Ord' ranking a vector by its number of elements before
-- their format, which puts VecFormat 16 FmtInt8 above the wider
-- VecFormat 4 FmtDouble; two vectors of one width, and any other two
-- formats, by the derived 'Ord', which ranks the scalar formats of a register
-- class by width and every vector above every scalar. The order is total,
-- as (FmtFix) in Note [Register formats in liveness analysis]
-- in GHC.CmmToAsm.Reg.Liveness needs.
compareFormats :: Format -> Format -> Ordering
compareFormats fmt1@VecFormat{} fmt2@VecFormat{}
  | w1 /= w2 = compare w1 w2
  where w1 = formatToWidth fmt1
        w2 = formatToWidth fmt2
compareFormats fmt1 fmt2 = compare fmt1 fmt2

maxRegWithFormat :: RegWithFormat -> RegWithFormat -> RegWithFormat
maxRegWithFormat r1@(RegWithFormat _ fmt1) r2@(RegWithFormat _ fmt2)
  = case compareFormats fmt1 fmt2 of
      LT -> r2
      _  -> r1
  -- Re-using one of the arguments avoids allocating a new 'RegWithFormat'
  -- at the larger of the two formats. With 'compareFormats' comparing
  -- 'formatInBytes' in place of 'formatToWidth', GHC split this function into
  -- a worker that rebuilt its result, and the compiler allocated 0.54%, 0.44%
  -- and 0.44% more compiling !16269's CmmDiamondChain at -O0, -O1 and -O2.

noRegs :: Regs
noRegs = Regs emptyUniqSet

addRegsMaxFmt :: Regs -> [RegWithFormat] -> Regs
addRegsMaxFmt = foldl' addRegMaxFmt

mkRegsMaxFmt :: [RegWithFormat] -> Regs
mkRegsMaxFmt = addRegsMaxFmt noRegs

addRegMaxFmt :: Regs -> RegWithFormat -> Regs
addRegMaxFmt = coerce $ strictAddOneToUniqSet_C maxRegWithFormat
  -- Don't build up thunks when combining with 'maxRegWithFormat'

-- | Remove 2nd argument registers from the 1st argument: one at a scalar format
-- in the first argument whatever its format in the second, and one at a vector
-- format only if its format in the second is at least as wide.
minusCoveredRegs :: Regs -> Regs -> Regs
minusCoveredRegs = coerce $ minusUniqSet_C f
  where
    f :: RegWithFormat -> RegWithFormat -> Maybe RegWithFormat
    f r1@(RegWithFormat _ fmt1) (RegWithFormat _ fmt2)
      | covered fmt1 fmt2 = Nothing
      | otherwise         = Just r1

-- | @minusCoveredRegs rs (mkRegsMaxFmt ws)@ for a list ws that names each
-- register once, without building the set of ws.
delCoveredRegs :: Regs -> [RegWithFormat] -> Regs
delCoveredRegs = foldl' del
  where
    del rs@(Regs live) (RegWithFormat r fmt2)
      | Just (RegWithFormat _ fmt1) <- lookupUniqSet_Directly live u
      , covered fmt1 fmt2 = Regs (delOneFromUniqSet_Directly live u)
      | otherwise         = rs
      where u = getUnique r

-- | Whether a write at the second format kills a value at the first: a scalar
-- value always, a vector value only where the write is at least as wide.
covered :: Format -> Format -> Bool
covered fmt1 fmt2 = not (isVecFormat fmt1)
                      -- See Wrinkle [Don't allow scalar partial writes]
                      -- in Note [Register formats in liveness analysis] in GHC.CmmToAsm.Reg.Liveness.
                 || formatToWidth fmt2 >= formatToWidth fmt1

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
