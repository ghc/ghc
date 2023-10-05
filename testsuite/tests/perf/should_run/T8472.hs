{-# LANGUAGE MagicHash #-}

module Main (f, main) where

import GHC.Exts(Ptr(..))
import Foreign.Ptr

-- We should be able to inline this function.
f :: Ptr Int -> Int -> Int
f =
  let x = "foo"#
  in \p n -> n + (Ptr x `minusPtr` p)

main :: IO ()
main = print $ x `mod` 2 == (x + 4) `mod` 2
  where
    x = go (10000::Int) 4
    go 0 a = a
    go n a = go (n-1) (f nullPtr a)
