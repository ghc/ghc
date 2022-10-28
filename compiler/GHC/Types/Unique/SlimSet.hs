-- | This module defines 'UniqSlimSet', which is more space-efficient than
-- 'UniqSet'.
module GHC.Types.Unique.SlimSet (
    -- * Data type
    UniqSlimSet, getUniqSlimSet,
    -- * Building
    emptyUniqSlimSet,
    extendUniqSlimSet, extendUniqSlimSetList,
    mkUniqSlimSet,
    delUniqSlimSet, delUniqSlimSetList,
    minusUniqSlimSet, unionUniqSlimSet, unionUniqSlimSets,
    ufmDom,
    -- * Querying
    isEmptyUniqSlimSet, sizeUniqSlimSet, elemUniqSlimSet
  ) where

import GHC.Prelude

import GHC.Types.Unique.FM
import GHC.Utils.Outputable
import GHC.Types.Unique

import qualified Data.IntSet as S

-- | Models sets of 'Uniquable' things.
-- Compared to 'UniqSet', it does not support lookup or mapping operations, but
-- in return can be backed by an 'IntSet' rather than an 'IntMap'. That means a
-- much more space efficient representation, because leaves can be encoded as
-- bitmaps.
newtype UniqSlimSet a = UniqSlimSet { getUniqSlimSet' :: S.IntSet }
  deriving Eq

getUniqSlimSet :: UniqSlimSet a -> S.IntSet
getUniqSlimSet = getUniqSlimSet'

k :: Uniquable a => a -> Int
k v = getKey (getUnique v)

emptyUniqSlimSet :: UniqSlimSet a
emptyUniqSlimSet = UniqSlimSet S.empty

elemUniqSlimSet :: Uniquable a => a -> UniqSlimSet a -> Bool
elemUniqSlimSet v (UniqSlimSet s) = k v `S.member` s

isEmptyUniqSlimSet :: UniqSlimSet a -> Bool
isEmptyUniqSlimSet (UniqSlimSet s) = S.null s

delUniqSlimSet :: Uniquable a => UniqSlimSet a -> a -> UniqSlimSet a
delUniqSlimSet (UniqSlimSet s) v = UniqSlimSet $ k v `S.delete` s

delUniqSlimSetList :: Uniquable a => UniqSlimSet a -> [a] -> UniqSlimSet a
delUniqSlimSetList s vs = s `minusUniqSlimSet` mkUniqSlimSet vs

minusUniqSlimSet :: UniqSlimSet a -> UniqSlimSet a -> UniqSlimSet a
minusUniqSlimSet (UniqSlimSet s) (UniqSlimSet s') = UniqSlimSet $ s `S.difference` s'

-- | Beware, O(n)!
sizeUniqSlimSet :: UniqSlimSet a -> Int
sizeUniqSlimSet (UniqSlimSet s) = S.size s

mkUniqSlimSet :: Uniquable a => [a] -> UniqSlimSet a
mkUniqSlimSet vs = UniqSlimSet $ S.fromList $ map k vs

ufmDom :: UniqFM k v -> UniqSlimSet k
ufmDom ufm = UniqSlimSet $ ufmToSet_Directly ufm

extendUniqSlimSet :: Uniquable a => a -> UniqSlimSet a -> UniqSlimSet a
extendUniqSlimSet v (UniqSlimSet s) = UniqSlimSet $ S.insert (k v) s

extendUniqSlimSetList :: Uniquable a => [a] -> UniqSlimSet a -> UniqSlimSet a
extendUniqSlimSetList vs s = s `unionUniqSlimSet` mkUniqSlimSet vs

unionUniqSlimSet :: UniqSlimSet a -> UniqSlimSet a -> UniqSlimSet a
unionUniqSlimSet (UniqSlimSet set1) (UniqSlimSet set2) = UniqSlimSet (set1 `S.union` set2)

unionUniqSlimSets :: [UniqSlimSet a] -> UniqSlimSet a
unionUniqSlimSets = foldl' (flip unionUniqSlimSet) emptyUniqSlimSet

instance Outputable (UniqSlimSet a) where
    ppr (UniqSlimSet s) = braces $
        hcat $ punctuate comma [ ppr (getUnique i) | i <- S.toList s]
