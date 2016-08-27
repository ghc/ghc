{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module T10598_fail1 where

class Z f where
  z :: f a b

data    A     = A Int deriving newtype  Show
newtype B     = B Int deriving bespoke  Num
data    C a b = C Int deriving anyclass Z
