{-# LANGUAGE PartialTypeSignatures #-}
module WildcardInTypeSynonymLHS where

type Foo (a :: _) = Int
