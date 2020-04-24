{-# LANGUAGE MagicHash #-}
{-# LANGUAGE BangPatterns #-}

module Main where

import Language.Haskell.TH.Lib
import GHC.Ptr
import Foreign.ForeignPtr

main :: IO ()
main = do

   let
      !x = "abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyz"#
      !y = "ABCDEabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyz"#

   p1 <- newForeignPtr_ (Ptr x)
   p2 <- newForeignPtr_ (Ptr y)

   let
      b1 = mkBytes p1  0 5
      b2 = mkBytes p1 10 5
      b3 = mkBytes p1 26 5
      b4 = mkBytes p2  5 5
      b5 = mkBytes p2 10 5

   let myCmp a b = putStrLn $ "compare " ++ show a ++ " to " ++ show b ++ " => " ++ show (compare a b)

   putStr "same pointer, same offset, same bytes: "
   myCmp b1 b1
   putStr "same pointer, different offset, same bytes: "
   myCmp b1 b3
   putStr "same pointer, different offset, different bytes: "
   myCmp b1 b2
   putStr "same pointer, different offset, different bytes: "
   myCmp b2 b1
   putStr "different pointer, different offset, same bytes: "
   myCmp b1 b4
   putStr "different pointer, different offset, different bytes: "
   myCmp b1 b5
