{-# LANGUAGE ScopedTypeVariables #-}
-- test the representation of unboxed literals

module Main
where

import Language.Haskell.TH

$(
  [d|
     foo :: Int -> Int
     foo (x :: Int) = x
   |]
 )

main :: IO ()
main = return ()

