{-# LANGUAGE GADTs #-}
module A where

data Term where
  BinaryTerm :: {-# UNPACK #-} !Bool -> tag -> Term

f :: Term -> String
f (BinaryTerm _ _) = "hello"
