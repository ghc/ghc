{-# LANGUAGE TypeFamilies, DataKinds #-}

module T9097 where

import GHC.Exts

type family Foo x where
  Foo True = False
  Foo False = False
  Foo Any = True
