{-# LANGUAGE NamedDefaults #-}

module Main (
    default Show,
    Foo(..),
    main,
  ) where

data Foo = Foo
instance Show Foo where
  show _ = "Foo"
instance Read Foo where
  readsPrec _ s = [(Foo, s)]

default Show (Foo)

main = print (pf "Foo")
  where
   pf =  show . read

-- variant of of T25858v3.hs
-- detect if we hang on [type] for class default.
-- where the [Type] is defined in the same module with the default declaration.
-- Variant of Ticket #25858
