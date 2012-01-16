{-# LANGUAGE DataKinds, PolyKinds, TypeFamilies, GADTs #-}

module PolyKinds12 where

type family If1 b t f
type instance If1 True t f = t
type instance If1 False t f = f

type family If2 (b :: Bool) t f
type instance If2 True t f = t
type instance If2 False t f = f

data SBool b where
  STrue :: SBool True
  SFalse :: SBool False

test1 :: SBool b -> If1 b Int Char
test1 STrue = 42
test1 SFalse = 'H'

test2 :: SBool b -> If2 b Int Char
test2 STrue = 42
test2 SFalse = 'H'

type family Apply f x
type instance Apply f x = f x

-- Does not work because we do not abstract the return kind of type families
-- Currently If1 returns kind *, which is too restrictive
higher1v1 :: SBool b -> (If1 b Maybe []) Char
higher1v1 STrue = Just 'H'
higher1v1 SFalse = "Hello"

higher1v2 :: SBool b -> Apply (If1 b Maybe []) Char
higher1v2 STrue = Just 'H'
higher1v2 SFalse = "Hello"

-- higher2 :: SBool b -> (If2 b Maybe []) Int
-- higher2 STrue = Just 42
-- higher2 SFalse = "Hello"
