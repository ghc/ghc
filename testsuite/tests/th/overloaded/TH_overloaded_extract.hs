{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where
-- A simple test to check that defining a custom instance is easily
-- possible and extraction works as expected.

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Data.Functor.Identity


instance Quote Identity where
  -- Not the correct implementation, just for testing
  newName s = Identity (Name (mkOccName s) NameS)

main = do
  print $ runIdentity [| 1 + 2 |]
  print $ runIdentity [| \x -> 1 + 2 |]
  print $ runIdentity [d| data Foo = Foo |]
  print $ runIdentity [p| () |]
  print $ runIdentity [t| [Int] |]
  print $ unType $ runIdentity [|| (+1) ||]

