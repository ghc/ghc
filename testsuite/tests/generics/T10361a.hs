{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
module T10361a where

class C1 a where
  type T1 a
  type instance T1 a = Char

class C2 a where -- equivalent to C1
  type T2 a
  type instance T2 a = Char

class C3 a where -- equivalent to C1, C2
  type T3 a
  type instance T3 a = Char

data A = B
  deriving C1

deriving instance C2 A

instance C3 A

test1 :: T1 A
test1 = 'x'

test2 :: T2 A
test2 = 'x'

test3 :: T3 A
test3 = 'x'
