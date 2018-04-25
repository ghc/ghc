{-# LANGUAGE PartialTypeSignatures #-}
module WildcardInDeriving where

data Foo a = Foo a
           deriving (_)
