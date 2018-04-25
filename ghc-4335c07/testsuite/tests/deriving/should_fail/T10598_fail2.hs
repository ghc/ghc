{-# LANGUAGE DerivingStrategies #-}
module T10598_fail2 where

data    A = A Int deriving anyclass Eq
newtype B = B Int deriving newtype  Eq
