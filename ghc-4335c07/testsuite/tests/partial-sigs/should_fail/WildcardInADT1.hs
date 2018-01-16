{-# LANGUAGE PartialTypeSignatures #-}
module WildcardInADT1 where

data Foo a = Foo (Either _ a)
