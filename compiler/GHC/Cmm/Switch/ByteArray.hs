module GHC.Cmm.Switch.ByteArray (
     ByteArraySwitchTargets(..),
     byteArraySwitchTargetsToList,
     mapByteArraySwitchTargets,
     byteArraySwitchTargetsFallThrough,
     eqByteArraySwitchTargetWith,
  ) where

import GHC.Prelude

import Data.ByteString (ByteString)
import Data.Function (on)
import Data.List (groupBy)
import GHC.Cmm.Dataflow.Label (Label)

import qualified Data.Map as M

-- | This type differs from the integral SwitchTargets type
-- in several ways:
--
-- * The values tested against are byte sequences, not numbers.
-- * Since there are an infinite number of byte sequences, it is
--   not possible for a user to write a case statement that matches
--   all of them. Consequently, the default value is mandatory
--   (for integral switching, it is optional).
-- * Signedness and range are not tracked. Signedness is not
--   applicable to byte sequences. Range is not useful for compilation.
data ByteArraySwitchTargets =
    ByteArraySwitchTargets
        Label                    -- Default value
        (M.Map ByteString Label) -- The branches
    deriving (Show, Eq)

-- | The list of all labels occurring in the ByteArraySwitchTargets value.
byteArraySwitchTargetsToList :: ByteArraySwitchTargets -> [Label]
byteArraySwitchTargetsToList (ByteArraySwitchTargets def branches)
    = def : M.elems branches

-- | Changes all labels mentioned in the ByteArraySwitchTargets value
mapByteArraySwitchTargets ::
     (Label -> Label)
  -> ByteArraySwitchTargets
  -> ByteArraySwitchTargets
mapByteArraySwitchTargets f (ByteArraySwitchTargets def branches)
    = ByteArraySwitchTargets (f def) (fmap f branches)

-- | Groups cases with equal targets, suitable for pretty-printing to a
-- c-like switch statement with fall-through semantics.
byteArraySwitchTargetsFallThrough ::
     ByteArraySwitchTargets
  -> ([([ByteString], Label)], Label)
byteArraySwitchTargetsFallThrough (ByteArraySwitchTargets def branches) =
  (groups, def)
  where
    groups = map (\xs -> (map fst xs, snd (head xs))) $
             groupBy ((==) `on` snd) $
             M.toList branches

-- | Custom equality helper, needed for "GHC.Cmm.CommonBlockElim"
eqByteArraySwitchTargetWith ::
     (Label -> Label -> Bool)
  -> ByteArraySwitchTargets
  -> ByteArraySwitchTargets
  -> Bool
eqByteArraySwitchTargetWith eq (ByteArraySwitchTargets def1 ids1)
                               (ByteArraySwitchTargets def2 ids2) =
    (def1 `eq` def2) && goList (M.toList ids1) (M.toList ids2)
  where
    goList [] [] = True
    goList ((i1,l1):ls1) ((i2,l2):ls2) = i1 == i2 && l1 `eq` l2 && goList ls1 ls2
    goList _ _ = False
