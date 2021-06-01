module GHC.Cmm.Reg.Set (
      RegSet
    , emptyRegSet, elemRegSet, extendRegSet, deleteFromRegSet, mkRegSet
    , plusRegSet, minusRegSet, timesRegSet, sizeRegSet, nullRegSet
    , regSetToList
  ) where

import GHC.Prelude

import           Data.Set ( Set )
import qualified Data.Set as Set

-- | Sets of registers

-- These are used for dataflow facts, and a common operation is taking
-- the union of two RegSets and then asking whether the union is the
-- same as one of the inputs.  UniqSet isn't good here, because
-- sizeUniqSet is O(n) whereas Set.size is O(1), so we use ordinary
-- Sets.

type RegSet r     = Set r

emptyRegSet             :: RegSet r
nullRegSet              :: RegSet r -> Bool
elemRegSet              :: Ord r => r -> RegSet r -> Bool
extendRegSet            :: Ord r => RegSet r -> r -> RegSet r
deleteFromRegSet        :: Ord r => RegSet r -> r -> RegSet r
mkRegSet                :: Ord r => [r] -> RegSet r
minusRegSet, plusRegSet, timesRegSet :: Ord r => RegSet r -> RegSet r -> RegSet r
sizeRegSet              :: RegSet r -> Int
regSetToList            :: RegSet r -> [r]

emptyRegSet      = Set.empty
nullRegSet       = Set.null
elemRegSet       = Set.member
extendRegSet     = flip Set.insert
deleteFromRegSet = flip Set.delete
mkRegSet         = Set.fromList
minusRegSet      = Set.difference
plusRegSet       = Set.union
timesRegSet      = Set.intersection
sizeRegSet       = Set.size
regSetToList     = Set.toList
