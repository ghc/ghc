{-# LANGUAGE InstanceSigs, TypeFamilies #-}
module T9582 where

class C a where
  type T a
  m :: a -> T a

instance C Int where
  type T Int = String
  m :: Int -> String
  m _ = "bla"

-- Method signature does not match class; it should be m :: T Int
--    In the instance declaration for ‘C Int’
