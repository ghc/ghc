{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LiftCSP #-}
module Main where
-- A test to check that CSP works with overloaded quotes

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Data.Functor.Identity


instance Quote Identity where
  -- Not the correct implementation, just for testing
  newName s = Identity (Name (mkOccName s) NameS)

main = do
  print $ runIdentity ((\x -> [| x |]) ())
  print $ runIdentity $ unTypeCode ((\x -> [|| x ||]) ())

