{-# LANGUAGE KindSignatures, GADTs, DataKinds #-}
{-# OPTIONS -Wno-star-is-type #-}
module Foo where

data Foo1 :: [*] -> * where

-- ghc <= 7.10 reported (before "Add kind equalities to GHC"):
--
--     T10379.hs:9:16: parse error on input ‘]’
data Foo2 :: ([] *) -> * where
