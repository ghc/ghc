{-# LANGUAGE GADTs, DataKinds #-}

module T9096a where

import GHC.Types
import GHC.Prim

data Foo a where
  MkFoo :: FUN Omega a (Foo a)
