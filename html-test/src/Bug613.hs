{-# LANGUAGE Haskell2010 #-}
module Bug613  where

import Prelude (Either(Left, Right))

class Functor f where
  fmap :: (a -> b) -> f a -> f b

instance Functor (Either a) where
  fmap _ (Left x) = Left x
  fmap f (Right y) = Right (f y)

-- | Phantom type a0 is added to block the first renaming from a to a0. This ensures that the renamer doesn't create a new conflict
data ThreeVars a0 a b = ThreeVars a b

instance Functor (ThreeVars a0 a) where
  fmap f (ThreeVars a b) = ThreeVars a (f b)
