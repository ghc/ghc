module T14285a where

import qualified Data.Foldable as F
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import Prelude hiding (null)

data Set k = Set IS.IntSet

empty = Set IS.empty


null (Set a) = IS.null a

sfromList :: (Enum a, Foldable c) => c a -> Set a
sfromList xs = Set $ IS.fromList $ Prelude.map fromEnum $ F.toList xs

{-# inlineable fromList #-}
fromList :: Enum k => [(k,v)] -> Map k v
fromList kvs =
  Map $ IM.fromList $ Prelude.map (\(k,v) -> (fromEnum k, v)) kvs


newtype Map k v = Map { unMap :: (IM.IntMap v) } deriving (Eq, Ord)

{-# inlineable findWithDefault #-}
findWithDefault d k (Map m) = IM.findWithDefault d (fromEnum k) m

data Rel a b = Rel !(Map a (Set b)) !(Map b (Set a))

{-# INLINEABLE images #-}
images x (Rel f b) = findWithDefault empty x f
{-# INLINEABLE pre_images #-}
pre_images x rel = images x $ mirrorRel rel
{-# INLINEABLE mirrorRel #-}
mirrorRel :: Rel a b -> Rel b a
mirrorRel (Rel f g) = Rel g f
