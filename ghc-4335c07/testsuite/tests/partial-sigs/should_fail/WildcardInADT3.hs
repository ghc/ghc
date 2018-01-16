{-# LANGUAGE PartialTypeSignatures, RankNTypes #-}
module WildcardInADT3 where

data Foo a = Foo { get :: _ => a }
