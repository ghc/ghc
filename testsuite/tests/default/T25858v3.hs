{-# LANGUAGE NamedDefaults #-}

module Main where

import T25858v3_helper

main = print (pf "Foo")
 where
  pf =  show . read


-- variant of of T25858v4.hs
-- detect if we hang on [type] for class default when importing.
-- where the [Type] is defined in the same module with the default declaration.
-- Variant of Ticket #25858
