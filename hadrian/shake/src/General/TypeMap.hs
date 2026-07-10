{-# LANGUAGE ExistentialQuantification, ConstraintKinds, KindSignatures, GADTs, ScopedTypeVariables, Rank2Types #-}

module General.TypeMap(
    Map, empty, singleton, insert, map, lookup, unionWith, toList, size
    ) where

import qualified Data.HashMap.Strict as Map
import Data.Typeable
import Unsafe.Coerce
import Data.Functor
import qualified Prelude
import Prelude hiding (lookup, map)


data F f = forall a . F !(f a)

unF :: F f -> f a
unF x = case x of F x -> unsafeCoerce x

newtype Map f = Map (Map.HashMap TypeRep (F f))

empty :: Map f
empty = Map Map.empty

singleton :: Typeable a => f a -> Map f
singleton x = Map $ Map.singleton (typeRep x) (F x)

insert :: Typeable a => f a -> Map f -> Map f
insert x (Map mp) = Map $ Map.insert (typeRep x) (F x) mp

lookup :: forall a f . Typeable a => Map f -> Maybe (f a)
lookup (Map mp) = unF <$> Map.lookup (typeRep (Proxy :: Proxy a)) mp

unionWith :: (forall a . f a -> f a -> f a) -> Map f -> Map f -> Map f
unionWith f (Map mp1) (Map mp2) = Map $ Map.unionWith (\x1 x2 -> F $ f (unF x1) (unF x2)) mp1 mp2

map :: (forall a . f1 a -> f2 a) -> Map f1 -> Map f2
map f (Map mp) = Map $ Map.map (\(F a) -> F $ f a) mp

toList :: (forall a . f a -> b) -> Map f -> [b]
toList f (Map mp) = Prelude.map (\(F a) -> f a) $ Map.elems mp

size :: Map f -> Int
size (Map mp) = Map.size mp
