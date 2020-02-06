{-# LANGUAGE MagicHash #-}

module Main where

import GHC.Exts
import Control.Exception

main :: IO ()
main = do

   let
      printE :: ArithException -> IO ()
      printE = print

   catch (raiseUnderflow# void#) printE
   catch (raiseOverflow#  void#) printE
   catch (raiseDivZero#   void#) printE
