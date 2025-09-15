{-# LANGUAGE DataKinds, NoStarIsType, ExplicitNamespaces #-}

module Main where

import GHC.TypeLits
import Prelude hiding ((++), (**))
import GHC.Types

data Proxy a = P

main = do
  print (natVal (P @(2 ++ 2 ** 2)))
  print (2 ++ 2 ** 2)
  print (2 + 2 * 2)

type a ++ b = a + b
type a ** b = a * b

a ++ b = a + b
a ** b = a * b

-- reversed fixity of * and +
infixl 6 type **
infixl 7 type ++

-- the same fixity as in * and +
infixl 7 data **
infixl 6 data ++
