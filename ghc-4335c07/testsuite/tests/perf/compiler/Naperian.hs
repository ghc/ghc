-- This was added a general test of compiler performance.

-- Author: Austin Seipp


{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
module Naperian where

import qualified Prelude
import           Prelude hiding      ( lookup, length, replicate, zipWith )

import qualified Data.IntMap         as IntMap
import           Data.List           ( intercalate )
import           Data.Kind           ( Type, Constraint )
import           Control.Applicative ( liftA2 )
import qualified GHC.Exts            as L (IsList(..))
import           GHC.Prim
import           GHC.TypeLits

import qualified Data.Vector         as Vector

import           Data.Foldable       ( toList )

--------------------------------------------------------------------------------
-- Miscellaneous

-- | The finite set of type-bounded Naturals. A value of type @'Fin' n@ has
-- exactly @n@ inhabitants, the natural numbers from @[0..n-1]@.
data Finite :: Nat -> Type where
  Fin :: Int -> Finite n
  deriving (Eq, Show)

-- | Create a type-bounded finite number @'Fin' n@ from a runtime integer,
-- bounded to a statically known limit. If the input value @x > n@, then
-- @'Nothing'@ is returned. Otherwise, returns @'Just' (x :: 'Fin' n)@.
finite :: forall n. KnownNat n => Int -> Maybe (Finite n)
finite x = case (x > y) of
  True  -> Nothing
  False -> Just (Fin x)
  where y = fromIntegral (natVal' (proxy# :: Proxy# n))

-- | \"'Applicative' zipping\".
azipWith :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
azipWith h xs ys = (pure h <*> xs) <*> ys

-- | Format a vector to make it look nice.
showVector :: [String] -> String
showVector xs = "<" ++ intercalate "," xs ++ ">"

--------------------------------------------------------------------------------
-- Pairs

-- | The cartesian product of @'a'@, equivalent to @(a, a)@.
data Pair a = Pair a a
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

instance Applicative Pair where
  pure a = Pair a a
  Pair k g <*> Pair a b = Pair (k a) (g b)

--------------------------------------------------------------------------------
-- Vectors

newtype Vector (n :: Nat) a = Vector (Vector.Vector a)
  deriving (Eq, Ord, Functor, Foldable, Traversable)

instance Show a => Show (Vector n a) where
  show = showVector . map show . toList

instance KnownNat n => Applicative (Vector n) where
  pure  = replicate
  (<*>) = zipWith ($)

instance (KnownNat n, Traversable (Vector n)) => L.IsList (Vector n a) where
  type Item (Vector n a) = a
  toList = Data.Foldable.toList

  fromList xs = case fromList xs of
    Nothing -> error "Demanded vector of a list that wasn't the proper length"
    Just ys -> ys

tail :: Vector (n + 1) a -> Vector n a
tail (Vector v) = Vector (Vector.tail v)

fromList :: forall n a. KnownNat n => [a] -> Maybe (Vector n a)
fromList xs = case (Prelude.length xs == sz) of
  False -> Nothing
  True  -> Just (Vector $ Vector.fromList xs)
  where sz = fromIntegral (natVal' (proxy# :: Proxy# n)) :: Int

zipWith :: (a -> b -> c) -> Vector n a -> Vector n b -> Vector n c
zipWith f (Vector a) (Vector b) = Vector (Vector.zipWith f a b)

length :: forall n a. KnownNat n => Vector n a -> Int
length _ = fromIntegral $ natVal' (proxy# :: Proxy# n)

replicate :: forall n a. KnownNat n => a -> Vector n a
replicate v = Vector (Vector.replicate sz v) where
  sz = fromIntegral (natVal' (proxy# :: Proxy# n)) :: Int

index :: Vector n a -> Finite n -> a
index (Vector v) (Fin n) = (Vector.!) v n

viota :: forall n. KnownNat n => Vector n (Finite n)
viota = Vector (fmap Fin (Vector.enumFromN 0 sz)) where
  sz = fromIntegral (natVal' (proxy# :: Proxy# n)) :: Int

--------------------------------------------------------------------------------
-- Naperian functors

-- | Naperian functors.

-- A useful way of thinking about a Naperian functor is that if we have a value
-- of type @v :: f a@ for some @'Naperian' f@, then we can think of @f a@ as a
-- bag of objects, with the ability to pick out the @a@ values inside the bag,
-- for each and every @a@ inside @f@. For example, in order to look up a value
-- @a@ inside a list @[a]@, we could use a function @[a] -> Int -> a@, which is
-- exactly @'(Prelude.!!)'@
--
-- The lookup function acts like a logarithm of the @'Functor' f@. Intuitively,
-- a Haskell function @f :: a -> b@ acts like the exponential @b^a@ if we intuit
-- types as an algebraic quantity. The logarithm of some value @x = b^a@ is
-- defined as @log_b(x) = a@, so given @x@ and a base @b@, it finds the exponent
-- @a@. In Haskell terms, this would be like finding the input value @a@ to a
-- function @f :: a -> b@, given a @b@, so it is a reverse mapping from the
-- outputs of @f@ back to its inputs.
--
-- A @'Naperian'@ functor @f@ is precisely a functor @f@ such that for any value
-- of type @f a@, we have a way of finding every single @a@ inside.
class Functor f => Naperian f where
  {-# MINIMAL lookup, (tabulate | positions) #-}

  -- | The \"logarithm\" of @f@. This type represents the 'input' you use to
  -- look up values inside @f a@. For example, if you have a list @[a]@, and
  -- you want to look up a value, then you use an @'Int'@ to index into
  -- the list. In this case, @'Log' [a] = Int@. If you have a type-bounded
  -- Vector @'Vector' (n :: 'Nat') a@, then @'Log' ('Vector' n)@ is the
  -- range of integers @[0..n-1]@ (represented here as @'Finite' n@.)
  type Log f

  -- | Look up an element @a@ inside @f a@. If you read this function type in
  -- english, it says \"if you give me an @f a@, then I will give you a
  -- function, so you can look up the elements of @f a@ and get back an @a@\"
  lookup :: f a -> (Log f -> a)

  -- | Tabulate a @'Naperian'@. This creates @f a@ values by mapping the logarithm
  -- of @f@ onto every \"position\" inside @f a@
  tabulate :: (Log f -> a) -> f a
  tabulate h = fmap h positions

  -- | Find every position in the \"space\" of the @'Naperian' f@.
  positions :: f (Log f)
  positions = tabulate id

-- | The transposition of two @'Naperian'@ functors @f@ and @g@.
transpose :: (Naperian f, Naperian g) => f (g a) -> g (f a)
transpose = tabulate . fmap tabulate . flip . fmap lookup . lookup

instance Naperian Pair where
  type Log Pair = Bool
  lookup (Pair x y) b = if b then y else x

  positions = Pair False True

instance KnownNat n => Naperian (Vector n) where
  type Log (Vector n) = Finite n

  lookup    = index
  positions = viota

--------------------------------------------------------------------------------
-- Dimensions

class (Applicative f, Naperian f, Traversable f) => Dimension f where
  size :: f a -> Int
  size = Prelude.length . toList

instance               Dimension Pair       where size = const 2
instance KnownNat n => Dimension (Vector n) where size = length

inner :: (Num a, Dimension f) => f a -> f a -> a
inner xs ys = sum (liftA2 (*) xs ys)

matrix :: ( Num a
          , Dimension f
          , Dimension g
          , Dimension h
          ) => f (g a)
            -> g (h a)
            -> f (h a)
matrix xss yss = liftA2 (liftA2 inner) (fmap pure xss) (pure (transpose yss))

--------------------------------------------------------------------------------
-- Hyper-dimensional stuff

-- | Arbitrary-rank Hypercuboids, parameterized over their dimension.
data Hyper :: [Type -> Type] -> Type -> Type where
  Scalar :: a -> Hyper '[] a
  Prism  :: (Dimension f, Shapely fs) => Hyper fs (f a) -> Hyper (f : fs) a

point :: Hyper '[] a -> a
point (Scalar a) = a

crystal :: Hyper (f : fs) a -> Hyper fs (f a)
crystal (Prism x) = x

instance Show a => Show (Hyper fs a) where
  show = showHyper . fmap show where
    showHyper :: Hyper gs String -> String
    showHyper (Scalar s) = s
    showHyper (Prism x)  = showHyper (fmap (showVector . toList) x)

{--
class HyperLift f fs where
  hyper :: (Shapely fs, Dimension f) => f a -> Hyper (f : fs) a

instance HyperLift f '[] where
  hyper = Prism . Scalar

instance (Shapely fs, HyperLift f fs) => HyperLift f (f : fs) where
  hyper = Prism . (\x -> (hyper $ _ x))
--}

class Shapely fs where
  hreplicate :: a -> Hyper fs a
  hsize      :: Hyper fs a -> Int

instance Shapely '[] where
  hreplicate a = Scalar a
  hsize        = const 1

instance (Dimension f, Shapely fs) => Shapely (f : fs) where
  hreplicate a = Prism (hreplicate (pure a))
  hsize (Prism x) = size (first x) * hsize x

instance Functor (Hyper fs) where
  fmap f (Scalar a) = Scalar (f a)
  fmap f (Prism x)  = Prism (fmap (fmap f) x)

instance Shapely fs => Applicative (Hyper fs) where
  pure  = hreplicate
  (<*>) = hzipWith ($)

hzipWith :: (a -> b -> c) -> Hyper fs a -> Hyper fs b -> Hyper fs c
hzipWith f (Scalar a) (Scalar b) = Scalar (f a b)
hzipWith f (Prism x)  (Prism y)  = Prism (hzipWith (azipWith f) x y)

first :: Shapely fs => Hyper fs a -> a
first (Scalar a) = a
first (Prism x)  = head (toList (first x))

-- | Generalized transposition over arbitrary-rank hypercuboids.
transposeH :: Hyper (f : (g : fs)) a
           -> Hyper (g : (f : fs)) a
transposeH (Prism (Prism x)) = Prism (Prism (fmap transpose x))

-- | Fold over a single dimension of a Hypercuboid.
foldrH :: (a -> a -> a) -> a -> Hyper (f : fs) a -> Hyper fs a
foldrH f z (Prism x) = fmap (foldr f z) x

-- | Lift an unary function from values to hypercuboids of values.
unary :: Shapely fs => (a -> b) -> (Hyper fs a -> Hyper fs b)
unary = fmap

-- | Lift a binary function from values to two sets of hypercuboids, which can
-- be aligned properly.
binary :: ( Compatible fs gs
          , Max fs gs ~ hs
          , Alignable fs hs
          , Alignable gs hs
          ) => (a -> b -> c)
            -> Hyper fs a
            -> Hyper gs b
            -> Hyper hs c
binary f x y = hzipWith f (align x) (align y)

up :: (Shapely fs, Dimension f) => Hyper fs a -> Hyper (f : fs) a
up = Prism . fmap pure

-- | Generalized, rank-polymorphic inner product.
innerH :: ( Max fs gs ~  (f : hs)
          , Alignable fs (f : hs)
          , Alignable gs (f : hs)
          , Compatible fs gs
          , Num a
          ) => Hyper fs a
            -> Hyper gs a
            -> Hyper hs a
innerH xs ys = foldrH (+) 0 (binary (*) xs ys)

-- | Generalized, rank-polymorphic matrix product.
matrixH :: ( Num a
           , Dimension f
           , Dimension g
           , Dimension h
           ) => Hyper '[ g, f ] a
             -> Hyper '[ h, g ] a
             -> Hyper '[ h, f ] a
matrixH x y = case (crystal x, transposeH y) of
  (xs, Prism (Prism ys)) -> hzipWith inner (up xs) (Prism (up ys))

--------------------------------------------------------------------------------
-- Alignment

class (Shapely fs, Shapely gs) => Alignable fs gs where
  align :: Hyper fs a -> Hyper gs a

instance Alignable '[] '[] where
  align = id

instance (Dimension f, Alignable fs gs) => Alignable (f : fs) (f : gs) where
  align (Prism x) = Prism (align x)

instance (Dimension f, Shapely fs) => Alignable '[] (f : fs) where
  align (Scalar a) = hreplicate a

type family Max (fs :: [Type -> Type]) (gs :: [Type -> Type]) :: [Type -> Type] where
  Max '[]      '[]      = '[]
  Max '[]      (f : gs) = f : gs
  Max (f : fs) '[]      = f : fs
  Max (f : fs) (f : gs) = f : Max fs gs

type family Compatible (fs :: [Type -> Type]) (gs :: [Type -> Type]) :: Constraint where
  Compatible '[] '[]           = ()
  Compatible '[] (f : gs)      = ()
  Compatible (f : fs) '[]      = ()
  Compatible (f : fs) (f : gs) = Compatible fs gs
  Compatible a b               = TypeError (
         'Text "Mismatched dimensions!"
   ':$$: 'Text "The dimension " ':<>: 'ShowType a ':<>: 'Text " can't be aligned with"
   ':$$: 'Text "the dimension " ':<>: 'ShowType b)

--------------------------------------------------------------------------------
-- Flattened, sparse Hypercuboids

elements :: Shapely fs => Hyper fs a -> [a]
elements (Scalar a) = [a]
elements (Prism a)  = concat (map toList (elements a))

data Flat fs a where
  Flat :: Shapely fs => Vector.Vector a -> Flat fs a

instance Functor (Flat fs) where
  fmap f (Flat v) = Flat (fmap f v)

instance Show a => Show (Flat fs a) where
  show = showHyper . fmap show where
    showHyper :: Flat gs String -> String
    showHyper (Flat v) = showVector (toList v)

flatten :: Shapely fs => Hyper fs a -> Flat fs a
flatten hs = Flat (Vector.fromList (elements hs))

data Sparse fs a where
  Sparse :: Shapely fs => a -> IntMap.IntMap a -> Sparse fs a

unsparse :: forall fs a. Shapely fs => Sparse fs a -> Flat fs a
unsparse (Sparse e xs) = Flat (Vector.unsafeAccum (flip const) vs as)
  where
    as     = IntMap.assocs xs
    vs     = Vector.replicate l e
    l      = hsize (hreplicate () :: Hyper fs ())

--------------------------------------------------------------------------------
-- Examples

type Matrix n m v = Vector n (Vector m v)

example1 :: Int
example1 = inner v1 v2 where
  v1 = [ 1, 2, 3 ] :: Vector 3 Int
  v2 = [ 4, 5, 6 ] :: Vector 3 Int

example2 :: Matrix 2 2 Int
example2 = matrix m1 m2 where
  m1 = [ [ 1, 2, 3 ]
       , [ 4, 5, 6 ]
       ] :: Matrix 2 3 Int

  m2 = [ [ 9, 8 ]
       , [ 6, 5 ]
       , [ 3, 2 ]
       ] :: Matrix 3 2 Int

example3 :: Hyper '[] Int
example3 = innerH v1 v2 where
  v1 = Prism (Scalar [1, 2, 3]) :: Hyper '[Vector 3] Int
  v2 = Prism (Scalar [4, 5, 6]) :: Hyper '[Vector 3] Int

example4 :: Hyper '[Vector 2, Vector 2] Int
example4 = matrixH v1 v2 where
  x = [ [ 1, 2, 3 ]
      , [ 4, 5, 6 ]
      ] :: Matrix 2 3 Int

  y = [ [ 9, 8 ]
      , [ 6, 5 ]
      , [ 3, 2 ]
      ] :: Matrix 3 2 Int

  v1 = Prism (Prism (Scalar x)) :: Hyper '[Vector 3, Vector 2] Int
  v2 = Prism (Prism (Scalar y)) :: Hyper '[Vector 2, Vector 3] Int
