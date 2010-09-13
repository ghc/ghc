{-# LANGUAGE TypeFamilies, GADTs, RankNTypes, EmptyDataDecls #-}

module ShouldCompile where

data Z
data S a

type family Sum n m
type instance Sum n Z = n
type instance Sum n (S m) = S (Sum n m)

data Nat n where
  NZ :: Nat Z
  NS :: (S n ~ sn) => Nat n -> Nat sn

data EQ a b = forall q . (a ~ b) => Refl

zerol :: Nat n -> EQ n (Sum Z n)
zerol NZ = Refl
-- zerol (NS n) = case zerol n of Refl -> Refl
