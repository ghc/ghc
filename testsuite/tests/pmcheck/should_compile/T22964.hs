{-# LANGUAGE TypeData #-}
module X where

type data T1 a where
  A1 :: T1 Int
  B1 :: T1 a

f1 :: T1 a -> ()
f1 x = case x of {}

type data T2 a where
  A2 :: T2 Int

f2 :: T2 a -> ()
f2 x = case x of {}
