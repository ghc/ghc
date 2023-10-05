{-# LANGUAGE PartialTypeSignatures, GADTs #-}
module WildcardInGADT2 where

data Foo a where
  Foo :: (Eq a, _) => Maybe a -> Foo a
