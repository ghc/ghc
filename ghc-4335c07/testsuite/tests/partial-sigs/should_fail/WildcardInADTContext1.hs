{-# LANGUAGE PartialTypeSignatures, DatatypeContexts #-}
module WildcardInADTContext where

data (Eq a, _) => Foo a = Foo { getFoo :: a }
