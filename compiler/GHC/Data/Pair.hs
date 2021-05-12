{-
A simple homogeneous pair type with useful Functor, Applicative, and
Traversable instances.
-}


{-# LANGUAGE DeriveFunctor #-}

module GHC.Data.Pair
   ( Pair(..)
   , unPair
   , toPair
   , swap
   , pLiftFst
   , pLiftSnd
   )
where

import GHC.Prelude

import GHC.Utils.Outputable
import qualified Data.Semigroup as Semi

data Pair a = Pair { pFst :: a, pSnd :: a }
  deriving (Functor)
-- Note that Pair is a *unary* type constructor
-- whereas (,) is binary

-- The important thing about Pair is that it has a *homogeneous*
-- Functor instance, so you can easily apply the same function
-- to both components

instance Applicative Pair where
  pure x = Pair x x
  (Pair f g) <*> (Pair x y) = Pair (f x) (g y)

instance Foldable Pair where
  foldMap f (Pair x y) = f x `mappend` f y

instance Traversable Pair where
  traverse f (Pair x y) = Pair <$> f x <*> f y

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
