{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-} -- Required since foldl' moves to the prelude

module General.Intern(
    Intern, Id(..),
    empty, insert, add, lookup, toList, fromList
    ) where

import Development.Shake.Classes
import Foreign.Storable
import Data.Word
import qualified Data.HashMap.Strict as Map
import Data.List(foldl')
import Prelude hiding (lookup)


-- Invariant: The first field is the highest value in the Map
data Intern a = Intern {-# UNPACK #-} !Word32 !(Map.HashMap a Id)

newtype Id = Id Word32
    deriving (Eq,Hashable,Ord,Binary,Show,NFData,Storable)

empty :: Intern a
empty = Intern 0 Map.empty


insert :: (Eq a, Hashable a) => a -> Id -> Intern a -> Intern a
insert k v@(Id i) (Intern n mp) = Intern (max n i) $ Map.insert k v mp


add :: (Eq a, Hashable a) => a -> Intern a -> (Intern a, Id)
add k (Intern v mp) = (Intern v2 $ Map.insert k (Id v2) mp, Id v2)
    where v2 = v + 1


lookup :: (Eq a, Hashable a) => a -> Intern a -> Maybe Id
lookup k (Intern _ mp) = Map.lookup k mp


toList :: Intern a -> [(a, Id)]
toList (Intern _ mp) = Map.toList mp


fromList :: (Eq a, Hashable a) => [(a, Id)] -> Intern a
fromList xs = Intern (foldl' max 0 [i | (_, Id i) <- xs]) (Map.fromList xs)
