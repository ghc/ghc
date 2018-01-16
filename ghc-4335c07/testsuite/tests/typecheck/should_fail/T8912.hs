{-# LANGUAGE ImplicitParams #-}
module T8912 where

class C a where
  toInt :: a -> Int

instance (?imp :: Int) => C [a] where
  toInt _ = ?imp

test :: Int
test = let ?imp = 5 in toInt "Hello, world"
