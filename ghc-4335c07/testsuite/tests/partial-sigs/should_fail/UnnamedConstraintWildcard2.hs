{-# LANGUAGE PartialTypeSignatures #-}
module UnnamedConstraintWildcard2 where

foo :: _ a => a -> String
foo x = show x
