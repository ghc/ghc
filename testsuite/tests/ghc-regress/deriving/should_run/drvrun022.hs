{-# LANGUAGE DeriveDataTypeable #-}

module Main where

-- GHC 6.4.1 output "testz" in z-encoded form!

import Data.Generics

data TestZ = TestZ { testz :: Int }
             deriving (Show, Read, Eq, Data, Typeable)

main = print $ constrFields . toConstr $ TestZ { testz = 2 }
