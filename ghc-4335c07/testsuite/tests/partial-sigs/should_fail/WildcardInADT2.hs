{-# LANGUAGE PartialTypeSignatures #-}
module WildcardInADT2 where

data Foo a = Foo { get :: Either _ a }
