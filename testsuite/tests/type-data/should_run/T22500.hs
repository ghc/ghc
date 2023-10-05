-- Check that a quoted data type declaration is printed correctly
{-# LANGUAGE TemplateHaskellQuotes, TypeData #-}

module Main where

import Language.Haskell.TH
import Language.Haskell.TH.Ppr

main = putStrLn . pprint =<< runQ [d| type data Nat = Zero | Succ Nat |]
