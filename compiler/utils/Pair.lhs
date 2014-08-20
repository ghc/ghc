
A simple homogeneous pair type with useful Functor, Applicative, and
Traversable instances.

\begin{code}
{-# LANGUAGE CPP #-}

module Pair ( Pair(..), unPair, toPair, swap ) where

#include "HsVersions.h"

import Outputable
import Data.Monoid
import Control.Applicative
import Data.Foldable
import Data.Traversable

data Pair a = Pair { pFst :: a, pSnd :: a }
-- Note that Pair is a *unary* type constructor
-- whereas (,) is binary

-- The important thing about Pair is that it has a *homogenous*
-- Functor instance, so you can easily apply the same function
-- to both components
instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)

instance Applicative Pair where
  pure x = Pair x x
  (Pair f g) <*> (Pair x y) = Pair (f x) (g y)

instance Foldable Pair where
  foldMap f (Pair x y) = f x `mappend` f y

instance Traversable Pair where
  traverse f (Pair x y) = Pair <$> f x <*> f y

instance Outputable a => Outputable (Pair a) where
  ppr (Pair a b) = ppr a <+> char '~' <+> ppr b

unPair :: Pair a -> (a,a)
unPair (Pair x y) = (x,y)

toPair :: (a,a) -> Pair a
toPair (x,y) = Pair x y

swap :: Pair a -> Pair a
swap (Pair x y) = Pair y x
\end{code}