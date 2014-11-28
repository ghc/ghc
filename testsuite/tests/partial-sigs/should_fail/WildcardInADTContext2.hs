{-# LANGUAGE PartialTypeSignatures, NamedWildcards, DatatypeContexts #-}
module WildcardInADTContext2 where

data (Eq _a) => Foo a = Foo { getFoo :: a }
