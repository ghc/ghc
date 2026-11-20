{-# LANGUAGE ExperimentalDependentTypes, GADTs #-}

module BarCase where

import Vector

type Explicit a = exists (n :: Nat) | Vec n a

bad :: Explicit a -> Bool
bad selection = case selection of
    Nil -> False
    Cons _ _ -> True
