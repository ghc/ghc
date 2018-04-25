{-# LANGUAGE DataKinds, PolyKinds, TypeOperators, TypeFamilies #-}

module T10188 where

data Peano = Zero | Succ Peano

type family Length (as :: [k]) :: Peano where
  Length (a : as) = Succ (Length as)
  Length '[]      = Zero

type family Length' (as :: [k]) :: Peano where
  Length' ((:) a as) = Succ (Length' as)
  Length' '[]        = Zero
