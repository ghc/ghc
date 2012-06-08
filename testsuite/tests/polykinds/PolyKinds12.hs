{-# LANGUAGE DataKinds, PolyKinds, TypeFamilies, GADTs #-}

module PolyKinds12 where

type family If1 (b::Bool) (t::k) (f::k) :: k
type instance If1 True t f = t
type instance If1 False t f = f

data SBool b where
  STrue :: SBool True
  SFalse :: SBool False

test1 :: SBool b -> If1 b Int Char
test1 STrue = 42
test1 SFalse = 'H'

test2 :: SBool b -> If1 b Int Char
test2 STrue = 42
test2 SFalse = 'H'

type family Apply (f :: k1 -> k2) (x :: k1) :: k2
type instance Apply f x = f x

higher1v1 :: SBool b -> (If1 b Maybe []) Char
higher1v1 STrue = Just 'H'
higher1v1 SFalse = "Hello"

higher1v2 :: SBool b -> Apply (If1 b Maybe []) Char
higher1v2 STrue = Just 'H'
higher1v2 SFalse = "Hello"

higher2 :: SBool b -> If1 b Maybe [] Int
higher2 STrue  = Just 42
higher2 SFalse = [45]
