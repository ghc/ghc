{-# LANGUAGE MagicHash #-}
module Main where

import Control.Exception
import System.Exit
import GHC.Exts

main :: IO ()
main = do
  let a = throw $ toException ExitSuccess :: Int#
  return ()
