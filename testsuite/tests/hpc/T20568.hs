{-# LANGUAGE RebindableSyntax #-}

-- | Test for #20586 regression: rebindable if shouldn't crash
module Main where

import Prelude

main :: IO ()
main = print $ program
  where
    program :: AST
    program =
      if AstBool True
      then AstInt 1
      else AstInt 2

data AST =
    AstGT AST AST
  | AstInt Integer
  | AstBool Bool
  | IfThenElse AST AST AST
  deriving Show

ifThenElse :: AST -> AST -> AST -> AST
ifThenElse = IfThenElse
