{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE EmptyCase #-}
module Bug where

import Data.Kind

data Nat = Z | S Nat

data Down :: Nat -> Type where
  Down :: !(Down n) -> Down (S n)

data Up :: Nat -> Type where
  Up :: !(Up (S n)) -> Up n

f :: Down n -> ()
f (Down r) = ()

f' :: Down (S (S (S (S Z)))) -> ()
f' (Down r) = ()

g :: Up n -> ()
g (Up r) = ()
