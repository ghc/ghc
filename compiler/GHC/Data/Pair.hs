{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}

{-
A simple homogeneous pair type with useful Functor, Applicative, and
Traversable instances.
-}



module GHC.Data.Pair
   ( Pair(..)
   , unPair
   , toPair
   , swap
   , pLiftFst, pLiftSnd
   , unzipPairs
   )
where

import GHC.Prelude

import GHC.Utils.Outputable
import qualified Data.Semigroup as Semi

import GHC.Generics (Generic, Generically(..))

data Pair a = Pair { pFst :: a, pSnd :: a }
  deriving (Foldable, Functor, Traversable, Generic)
  deriving (Semigroup, Monoid) via Generically (Pair a)

-- Note that Pair is a *unary* type constructor
-- whereas (,) is binary

-- The important thing about Pair is that it has a *homogeneous*
-- Functor instance, so you can easily apply the same function
-- to both components

instance Applicative Pair where
  pure x = Pair x x
  (Pair f g) <*> (Pair x y) = Pair (f x) (g y)

instance Outputable a => Outputable (Pair a) where
  ppr (Pair a b) = ppr a <+> char '~' <+> ppr b

unPair :: Pair a -> (a,a)
unPair (Pair x y) = (x,y)

toPair :: (a,a) -> Pair a
toPair (x,y) = Pair x y

swap :: Pair a -> Pair a
swap (Pair x y) = Pair y x

pLiftFst :: (a -> a) -> Pair a -> Pair a
pLiftFst f (Pair a b) = Pair (f a) b

pLiftSnd :: (a -> a) -> Pair a -> Pair a
pLiftSnd f (Pair a b) = Pair a (f b)

unzipPairs :: [Pair a] -> ([a], [a])
unzipPairs [] = ([], [])
unzipPairs (Pair a b : prs) = (a:as, b:bs)
  where
    !(as,bs) = unzipPairs prs
    -- This makes the unzip work eagerly, building no thunks at
    -- the cost of doing all the work up-front.

instance Foldable1 Pair where
    foldMap1 f (Pair a b) = f a Semi.<> f b
