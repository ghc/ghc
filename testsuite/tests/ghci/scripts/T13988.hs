{-# LANGUAGE TypeInType, GADTs #-}

module T13988 where

import Data.Kind

data Foo (a :: k) where
  MkFoo :: (k ~ Type) => Foo (a :: k)
