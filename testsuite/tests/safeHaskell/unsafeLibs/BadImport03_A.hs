{-# LANGUAGE Trustworthy #-}
module BadImport03_A (
        Nat, zero, succ', showNat
  ) where

data Nat = NatC Int

zero :: Nat
zero = NatC 0

succ' :: Nat -> Nat
succ' (NatC n) = NatC $ n + 1

showNat :: Nat -> String
showNat (NatC n) = "Nat " ++ show n

