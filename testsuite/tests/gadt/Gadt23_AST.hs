{-# LANGUAGE GADTs, KindSignatures #-}

module Gadt23_AST where

import Data.Kind (Type)

data Exp_;

data AST :: Type -> Type -> Type where
  Var   :: String -> AST Exp_ tag
  Tag   :: tag    -> AST a tag -> AST a tag

