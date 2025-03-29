{-# LANGUAGE NamedDefaults #-}
module Main (
    Stringify(..),
    default Stringify,
    main
  ) where

class Stringify a where
  stringify :: a -> String

instance Stringify Int where
  stringify n = "Int: " ++ show n

instance Stringify Bool where
  stringify b = "Bool: " ++ show b

instance Stringify [Char] where
  stringify s = "String: " ++ s

default Stringify (Bool)

-- Detect if it compiles without hanging when the following conditions are met:
-- 1. Stringify is defined in this module.
-- 2. export the default `Stringify Bool`.

-- Previously, this code hang during the export class hydration at the end of compilation.
-- see ticket #25858

main :: IO ()
main = print $ (stringify . read) "True"
