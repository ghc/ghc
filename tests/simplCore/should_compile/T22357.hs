{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module T22358
  ( FunctorWithIndex(..)
  , FoldableWithIndex(..)
  , TraversableWithIndex(..)
  ) where

import Control.Applicative (Const(..))
import Data.Coerce (Coercible, coerce)
import Data.Monoid (Dual(..), Endo(..))
import Data.Tree (Tree (..))

class Functor f => FunctorWithIndex i f | f -> i where
  imap :: (i -> a -> b) -> f a -> f b

class Foldable f => FoldableWithIndex i f | f -> i where
  ifoldMap :: Monoid m => (i -> a -> m) -> f a -> m

  ifoldl :: (i -> b -> a -> b) -> b -> f a -> b
  ifoldl f z t = appEndo (getDual (ifoldMap (\ i -> Dual #. Endo #. flip (f i)) t)) z
  {-# INLINE ifoldl #-}

ifoldMapDefault :: (TraversableWithIndex i f, Monoid m) => (i -> a -> m) -> f a -> m
ifoldMapDefault f = getConst #. itraverse (Const #.. f)
{-# INLINE ifoldMapDefault #-}

class (FunctorWithIndex i t, FoldableWithIndex i t, Traversable t) => TraversableWithIndex i t | t -> i where
  itraverse :: Applicative f => (i -> a -> f b) -> t a -> f (t b)

instance FunctorWithIndex Int [] where
  imap f = go 0 where
    go !_ []     = []
    go !n (x:xs) = f n x : go (n + 1) xs
  {-# INLINE imap #-}
instance FoldableWithIndex Int [] where
  ifoldMap = ifoldMapDefault
  {-# INLINE ifoldMap #-}
instance TraversableWithIndex Int [] where
  itraverse f = traverse (uncurry' f) . zip [0..]
  {-# INLINE itraverse #-}

instance FoldableWithIndex [Int] Tree where
  ifoldMap f (Node a as) = f [] a `mappend` ifoldMap (\i -> ifoldMap (f . (:) i)) as
  {-# INLINE ifoldMap #-}

(#.) :: Coercible b c => (b -> c) -> (a -> b) -> (a -> c)
_ #. x = coerce x

(#..) :: Coercible b c => (b -> c) -> (i -> a -> b) -> (i -> a -> c)
_ #.. x = coerce x
infixr 9 #., #..
{-# INLINE (#.) #-}
{-# INLINE (#..)#-}

uncurry' :: (a -> b -> c) -> (a, b) -> c
uncurry' f (a, b) = f a b
{-# INLINE uncurry' #-}
