
{-# OPTIONS_GHC -fglasgow-exts #-}
module Gadt23_AST where

data Exp_;

data AST :: * -> * -> * where
  Var   :: String -> AST Exp_ tag
  Tag   :: tag    -> AST a tag -> AST a tag

