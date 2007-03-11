
module Main where

import Gadt23_AST

data Foo = Foo { bar :: Int }

convert :: AST a tag -> AST a Foo
convert t = case t of
  Var v     -> Tag (Foo 42) $ Var v
  Tag t e   -> Tag (Foo 42) $ convert e

main = return ()

