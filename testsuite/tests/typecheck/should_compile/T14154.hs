{-# Language RankNTypes, DerivingStrategies, TypeApplications,
    ScopedTypeVariables, GADTs, PolyKinds #-}

module T14154 where

newtype Ran g h a
  = MkRan (forall b. (a -> g b) -> h b)

newtype Swap p f g a where
  MkSwap :: p g f a -> Swap p f g a

ireturn :: forall m i a. a -> m i i a
ireturn = undefined

xs = case ireturn @(Swap Ran) 'a' of
       MkSwap (MkRan f) -> f print
