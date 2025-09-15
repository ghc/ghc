{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
module T15290d where

class C a where
  c :: Int -> forall b. b -> a

instance C Int where
  c _ _ = 42

newtype Age = MkAge Int
  deriving C
