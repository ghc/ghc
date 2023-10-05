{-# LANGUAGE TypeData #-}
module T22948b where

type data T a where
  A :: T Int

f :: T a -> ()
f !x = ()
