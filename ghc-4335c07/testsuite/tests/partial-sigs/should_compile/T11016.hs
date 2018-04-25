{-# LANGUAGE ImplicitParams, PartialTypeSignatures #-}

module T11016 where

f1 :: (?x :: Int, _) => Int
f1 = ?x

f2 :: (?x :: Int) => _
f2 = ?x
