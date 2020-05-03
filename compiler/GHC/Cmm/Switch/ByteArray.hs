module GHC.Cmm.Switch.ByteArray (
     ByteArraySwitchTargets(..),
     ByteArraySwitchTarget(..),
     ByteArraySwitchPlan(..),
     ByteArrayStrategy(..),
     byteArraySwitchTargetsToList,
     mapByteArraySwitchTargets,
     byteArraySwitchTargetsFallThrough,
     eqByteArraySwitchTargetWith,
     createByteArraySwitchPlan
  ) where

import GHC.Prelude

import Data.ByteString (ByteString)
import Data.Function (on)
import Data.List (groupBy)
import GHC.Cmm.Expr (CmmLit)
import GHC.Cmm.Dataflow.Label (Label)

import qualified Data.ByteString as ByteString
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
data ByteArraySwitchTargets = ByteArraySwitchTargets
  !Label -- Default value
  [ByteArraySwitchTarget] -- The branches
  deriving (Show, Eq)

data ByteArraySwitchTarget = ByteArraySwitchTarget
  { bastKey :: !ByteString
  , bastKeyLit :: CmmLit
  , bastTarget :: {-# UNPACK #-} !Label
  }

instance Eq ByteArraySwitchTarget where
  ByteArraySwitchTarget a1 _ b1 == ByteArraySwitchTarget a2 _ b2 =
    a1 == a2 && b1 == b2

instance Show ByteArraySwitchTarget where
  show _ = "ByteArraySwitchTarget{..}"

data ByteArraySwitchPlan = ByteArraySwitchPlan
  !Label -- Default value
  !(M.Map Int ByteArrayStrategy) -- Groups matches by length 

data ByteArrayStrategy
  = OneNoHash !ByteArraySwitchTarget
  | ManyHash [ByteArraySwitchTarget]

-- | The list of all labels occurring in the ByteArraySwitchTargets value.
byteArraySwitchTargetsToList :: ByteArraySwitchTargets -> [Label]
byteArraySwitchTargetsToList (ByteArraySwitchTargets def branches)
    = def : map bastTarget branches

-- | Changes all labels mentioned in the ByteArraySwitchTargets value
mapByteArraySwitchTargets ::
     (Label -> Label)
  -> ByteArraySwitchTargets
  -> ByteArraySwitchTargets
mapByteArraySwitchTargets f (ByteArraySwitchTargets def branches) =
  ByteArraySwitchTargets
    (f def)
    (fmap (\t -> t {bastTarget = f (bastTarget t)}) branches)

-- | Groups cases with equal targets, suitable for pretty-printing to a
-- c-like switch statement with fall-through semantics.
byteArraySwitchTargetsFallThrough ::
     ByteArraySwitchTargets
  -> ([([ByteString], Label)], Label)
byteArraySwitchTargetsFallThrough (ByteArraySwitchTargets def branches) =
  (groups, def)
  where
    groups = map (\xs -> (map bastKey xs, bastTarget (head xs))) $
             groupBy ((==) `on` bastTarget) $
             branches

-- | Custom equality helper, needed for "GHC.Cmm.CommonBlockElim"
eqByteArraySwitchTargetWith ::
     (Label -> Label -> Bool)
  -> ByteArraySwitchTargets
  -> ByteArraySwitchTargets
  -> Bool
eqByteArraySwitchTargetWith eq (ByteArraySwitchTargets def1 ids1)
                               (ByteArraySwitchTargets def2 ids2) =
    (def1 `eq` def2) && goList ids1 ids2
  where
    goList [] [] = True
    goList (ByteArraySwitchTarget a1 _ b1:ls1) (ByteArraySwitchTarget a2 _ b2:ls2) =
      a1 == a2 && b1 `eq` b2 && goList ls1 ls2
    goList _ _ = False

-- | This function creates a ByteArraySwitchPlan from a ByteArraySwitchTargets
-- value, grouping keys of equal length and deciding on a hashing scheme.
createByteArraySwitchPlan :: ByteArraySwitchTargets -> ByteArraySwitchPlan
createByteArraySwitchPlan (ByteArraySwitchTargets defLabel arms) =
  ByteArraySwitchPlan defLabel $ foldl'
    (\m t -> M.alter
      ( \x -> case x of
        Nothing -> Just (OneNoHash t)
        Just old -> case old of
          OneNoHash t0 -> Just (ManyHash [t,t0])
          ManyHash ts -> Just (ManyHash (t : ts))
      ) (ByteString.length (bastKey t)) m
    ) M.empty arms
