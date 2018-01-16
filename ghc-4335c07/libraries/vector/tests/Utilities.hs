{-# LANGUAGE FlexibleInstances, GADTs #-}
module Utilities where

import Test.QuickCheck

import qualified Data.Vector as DV
import qualified Data.Vector.Generic as DVG
import qualified Data.Vector.Primitive as DVP
import qualified Data.Vector.Storable as DVS
import qualified Data.Vector.Unboxed as DVU
import qualified Data.Vector.Fusion.Bundle as S

import Control.Monad (foldM, foldM_, zipWithM, zipWithM_)
import Control.Monad.Trans.Writer
import Data.Function (on)
import Data.Functor.Identity
import Data.List ( sortBy )
import Data.Monoid

instance Show a => Show (S.Bundle v a) where
    show s = "Data.Vector.Fusion.Bundle.fromList " ++ show (S.toList s)


instance Arbitrary a => Arbitrary (DV.Vector a) where
    arbitrary = fmap DV.fromList arbitrary

instance CoArbitrary a => CoArbitrary (DV.Vector a) where
    coarbitrary = coarbitrary . DV.toList

instance (Arbitrary a, DVP.Prim a) => Arbitrary (DVP.Vector a) where
    arbitrary = fmap DVP.fromList arbitrary

instance (CoArbitrary a, DVP.Prim a) => CoArbitrary (DVP.Vector a) where
    coarbitrary = coarbitrary . DVP.toList

instance (Arbitrary a, DVS.Storable a) => Arbitrary (DVS.Vector a) where
    arbitrary = fmap DVS.fromList arbitrary

instance (CoArbitrary a, DVS.Storable a) => CoArbitrary (DVS.Vector a) where
    coarbitrary = coarbitrary . DVS.toList

instance (Arbitrary a, DVU.Unbox a) => Arbitrary (DVU.Vector a) where
    arbitrary = fmap DVU.fromList arbitrary

instance (CoArbitrary a, DVU.Unbox a) => CoArbitrary (DVU.Vector a) where
    coarbitrary = coarbitrary . DVU.toList

instance Arbitrary a => Arbitrary (S.Bundle v a) where
    arbitrary = fmap S.fromList arbitrary

instance CoArbitrary a => CoArbitrary (S.Bundle v a) where
    coarbitrary = coarbitrary . S.toList

instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = fmap Identity arbitrary

instance CoArbitrary a => CoArbitrary (Identity a) where
    coarbitrary = coarbitrary . runIdentity

instance Arbitrary a => Arbitrary (Writer a ()) where
    arbitrary = fmap (writer . ((,) ())) arbitrary

instance CoArbitrary a => CoArbitrary (Writer a ()) where
    coarbitrary = coarbitrary . runWriter

class (Testable (EqTest a), Conclusion (EqTest a)) => TestData a where
  type Model a
  model :: a -> Model a
  unmodel :: Model a -> a

  type EqTest a
  equal :: a -> a -> EqTest a

instance Eq a => TestData (S.Bundle v a) where
  type Model (S.Bundle v a) = [a]
  model = S.toList
  unmodel = S.fromList

  type EqTest (S.Bundle v a) = Property
  equal x y = property (x == y)

instance Eq a => TestData (DV.Vector a) where
  type Model (DV.Vector a) = [a]
  model = DV.toList
  unmodel = DV.fromList

  type EqTest (DV.Vector a) = Property
  equal x y = property (x == y)

instance (Eq a, DVP.Prim a) => TestData (DVP.Vector a) where
  type Model (DVP.Vector a) = [a]
  model = DVP.toList
  unmodel = DVP.fromList

  type EqTest (DVP.Vector a) = Property
  equal x y = property (x == y)

instance (Eq a, DVS.Storable a) => TestData (DVS.Vector a) where
  type Model (DVS.Vector a) = [a]
  model = DVS.toList
  unmodel = DVS.fromList

  type EqTest (DVS.Vector a) = Property
  equal x y = property (x == y)

instance (Eq a, DVU.Unbox a) => TestData (DVU.Vector a) where
  type Model (DVU.Vector a) = [a]
  model = DVU.toList
  unmodel = DVU.fromList

  type EqTest (DVU.Vector a) = Property
  equal x y = property (x == y)

#define id_TestData(ty) \
instance TestData ty where { \
  type Model ty = ty;        \
  model = id;                \
  unmodel = id;              \
                             \
  type EqTest ty = Property; \
  equal x y = property (x == y) }

id_TestData(())
id_TestData(Bool)
id_TestData(Int)
id_TestData(Float)
id_TestData(Double)
id_TestData(Ordering)

-- Functorish models
-- All of these need UndecidableInstances although they are actually well founded. Oh well.
instance (Eq a, TestData a) => TestData (Maybe a) where
  type Model (Maybe a) = Maybe (Model a)
  model = fmap model
  unmodel = fmap unmodel

  type EqTest (Maybe a) = Property
  equal x y = property (x == y)

instance (Eq a, TestData a) => TestData [a] where
  type Model [a] = [Model a]
  model = fmap model
  unmodel = fmap unmodel

  type EqTest [a] = Property
  equal x y = property (x == y)

instance (Eq a, TestData a) => TestData (Identity a) where
  type Model (Identity a) = Identity (Model a)
  model = fmap model
  unmodel = fmap unmodel

  type EqTest (Identity a) = Property
  equal = (property .) . on (==) runIdentity

instance (Eq a, TestData a, Monoid a) => TestData (Writer a ()) where
  type Model (Writer a ()) = Writer (Model a) ()
  model = mapWriter model
  unmodel = mapWriter unmodel

  type EqTest (Writer a ()) = Property
  equal = (property .) . on (==) execWriter

instance (Eq a, Eq b, TestData a, TestData b) => TestData (a,b) where
  type Model (a,b) = (Model a, Model b)
  model (a,b) = (model a, model b)
  unmodel (a,b) = (unmodel a, unmodel b)

  type EqTest (a,b) = Property
  equal x y = property (x == y)

instance (Eq a, Eq b, Eq c, TestData a, TestData b, TestData c) => TestData (a,b,c) where
  type Model (a,b,c) = (Model a, Model b, Model c)
  model (a,b,c) = (model a, model b, model c)
  unmodel (a,b,c) = (unmodel a, unmodel b, unmodel c)

  type EqTest (a,b,c) = Property
  equal x y = property (x == y)

instance (Arbitrary a, Show a, TestData a, TestData b) => TestData (a -> b) where
  type Model (a -> b) = Model a -> Model b
  model f = model . f . unmodel
  unmodel f = unmodel . f . model

  type EqTest (a -> b) = a -> EqTest b
  equal f g x = equal (f x) (g x)

newtype P a = P { unP :: EqTest a }

instance TestData a => Testable (P a) where
  property (P a) = property a

infix 4 `eq`
eq :: TestData a => a -> Model a -> P a
eq x y = P (equal x (unmodel y))

class Conclusion p where
  type Predicate p

  predicate :: Predicate p -> p -> p

instance Conclusion Property where
  type Predicate Property = Bool

  predicate = (==>)

instance Conclusion p => Conclusion (a -> p) where
  type Predicate (a -> p) = a -> Predicate p

  predicate f p = \x -> predicate (f x) (p x)

infixr 0 ===>
(===>) :: TestData a => Predicate (EqTest a) -> P a -> P a
p ===> P a = P (predicate p a)

notNull2 _ xs = not $ DVG.null xs
notNullS2 _ s = not $ S.null s

-- Generators
index_value_pairs :: Arbitrary a => Int -> Gen [(Int,a)]
index_value_pairs 0 = return [] 
index_value_pairs m = sized $ \n ->
  do
    len <- choose (0,n)
    is <- sequence [choose (0,m-1) | i <- [1..len]]
    xs <- vector len
    return $ zip is xs

indices :: Int -> Gen [Int]
indices 0 = return []
indices m = sized $ \n ->
  do
    len <- choose (0,n)
    sequence [choose (0,m-1) | i <- [1..len]]


-- Additional list functions
singleton x = [x]
snoc xs x = xs ++ [x]
generate n f = [f i | i <- [0 .. n-1]]
slice i n xs = take n (drop i xs)
backpermute xs is = map (xs!!) is
prescanl f z = init . scanl f z
postscanl f z = tail . scanl f z
prescanr f z = tail . scanr f z
postscanr f z = init . scanr f z

accum :: (a -> b -> a) -> [a] -> [(Int,b)] -> [a]
accum f xs ps = go xs ps' 0
  where
    ps' = sortBy (\p q -> compare (fst p) (fst q)) ps

    go (x:xs) ((i,y) : ps) j
      | i == j     = go (f x y : xs) ps j
    go (x:xs) ps j = x : go xs ps (j+1)
    go [] _ _      = []  

(//) :: [a] -> [(Int, a)] -> [a]
xs // ps = go xs ps' 0
  where
    ps' = sortBy (\p q -> compare (fst p) (fst q)) ps

    go (x:xs) ((i,y) : ps) j
      | i == j     = go (y:xs) ps j
    go (x:xs) ps j = x : go xs ps (j+1)
    go [] _ _      = []


withIndexFirst m f = m (uncurry f) . zip [0..]

imap :: (Int -> a -> a) -> [a] -> [a]
imap = withIndexFirst map

imapM :: Monad m => (Int -> a -> m a) -> [a] -> m [a]
imapM = withIndexFirst mapM

imapM_ :: Monad m => (Int -> a -> m b) -> [a] -> m ()
imapM_ = withIndexFirst mapM_

izipWith :: (Int -> a -> a -> a) -> [a] -> [a] -> [a]
izipWith = withIndexFirst zipWith

izipWithM :: Monad m => (Int -> a -> a -> m a) -> [a] -> [a] -> m [a]
izipWithM = withIndexFirst zipWithM

izipWithM_ :: Monad m => (Int -> a -> a -> m b) -> [a] -> [a] -> m ()
izipWithM_ = withIndexFirst zipWithM_

izipWith3 :: (Int -> a -> a -> a -> a) -> [a] -> [a] -> [a] -> [a]
izipWith3 = withIndexFirst zipWith3

ifilter :: (Int -> a -> Bool) -> [a] -> [a]
ifilter f = map snd . withIndexFirst filter f

indexedLeftFold fld f z = fld (uncurry . f) z . zip [0..]

ifoldl :: (a -> Int -> a -> a) -> a -> [a] -> a
ifoldl = indexedLeftFold foldl

ifoldr :: (Int -> a -> b -> b) -> b -> [a] -> b
ifoldr f z = foldr (uncurry f) z . zip [0..]

ifoldM :: Monad m => (a -> Int -> a -> m a) -> a -> [a] -> m a
ifoldM = indexedLeftFold foldM

ifoldM_ :: Monad m => (b -> Int -> a -> m b) -> b -> [a] -> m ()
ifoldM_ = indexedLeftFold foldM_

minIndex :: Ord a => [a] -> Int
minIndex = fst . foldr1 imin . zip [0..]
  where
    imin (i,x) (j,y) | x <= y    = (i,x)
                     | otherwise = (j,y)

maxIndex :: Ord a => [a] -> Int
maxIndex = fst . foldr1 imax . zip [0..]
  where
    imax (i,x) (j,y) | x >= y    = (i,x)
                     | otherwise = (j,y)

