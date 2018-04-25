{-# LANGUAGE TypeOperators
           , DataKinds
           , PolyKinds
           , TypeFamilies
           , GADTs
           , UndecidableInstances
           , RankNTypes
           , ScopedTypeVariables
  #-}
module Test11321 where

data instance Sing (z :: [a])
  = z ~ '[] =>
    SNil
  | forall (m :: a)
           (n :: [a]). z ~ (:) m n =>
    SCons (Sing m) (Sing n)
