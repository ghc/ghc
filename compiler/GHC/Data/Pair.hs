{-
A simple homogeneous pair type with useful Functor, Applicative, and
Traversable instances.
-}


{-# LANGUAGE DeriveTraversable #-}

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

data Pair a = Pair { pFst :: a, pSnd :: a }
  deriving (Foldable, Functor, Traversable)
-- Note that Pair is a *unary* type constructor
-- whereas (,) is binary

-- The important thing about Pair is that it has a *homogeneous*
-- Functor instance, so you can easily apply the same function
-- to both components

instance Applicative Pair where
  pure x = Pair x x
  (Pair f g) <*> (Pair x y) = Pair (f x) (g y)

instance Semi.Semigroup a => Semi.Semigroup (Pair a) where
  Pair a1 b1 <> Pair a2 b2 =  Pair (a1 Semi.<> a2) (b1 Semi.<> b2)

instance (Semi.Semigroup a, Monoid a) => Monoid (Pair a) where
  mempty = Pair mempty mempty
  mappend = (Semi.<>)

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
