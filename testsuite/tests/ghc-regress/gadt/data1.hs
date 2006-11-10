{-# OPTIONS -fglasgow-exts #-}

-- Trac #289

module ShouldCompile where

class C a where
  f :: a -> Bool

data T a where
  MkT :: (C a) => a -> T a

tf1 :: T Int -> Bool
tf1 (MkT aa) = f aa

tf2 :: T a -> Bool
tf2 (MkT aa) = f aa
