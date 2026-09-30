{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE Safe #-}
module Main where

-- Importing a wired-in in module should honour trustworthiness
import GHC.Internal.Char (chr)

main :: IO ()
main = print (chr 120)
