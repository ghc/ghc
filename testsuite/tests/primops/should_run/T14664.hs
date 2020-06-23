{-# LANGUAGE MagicHash #-}

module Main where

import GHC.Exts
import GHC.Prim.Exception
import Control.Exception

main :: IO ()
main = do

   let
      printE :: ArithException -> IO ()
      printE = print

   catch raiseUnderflow printE
   catch raiseOverflow  printE
   catch raiseDivZero   printE
