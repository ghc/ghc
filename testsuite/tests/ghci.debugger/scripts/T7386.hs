{-# LANGUAGE GADTs, DataKinds, KindSignatures, TypeFamilies, PolyKinds #-}

module T7386 where

data Nat = Zero | Succ Nat
data family Sing (a :: k)
data instance Sing (a :: Nat) where
   SZero :: Sing Zero
   SSucc :: Sing n -> Sing (Succ n)

