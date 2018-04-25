{-# LANGUAGE PartialTypeSignatures, GADTs #-}
module WildcardInGADT1 where

data Foo a where
  Foo :: Either a _ -> Foo a
