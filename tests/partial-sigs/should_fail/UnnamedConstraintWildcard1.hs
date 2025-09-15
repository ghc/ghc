{-# LANGUAGE PartialTypeSignatures #-}
module UnnamedConstraintWildcard1 where

foo :: Show _ => a -> String
foo x = show x
