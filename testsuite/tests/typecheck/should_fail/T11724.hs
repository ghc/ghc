{-# LANGUAGE TypeInType #-}

module T11724 where

import GHC.Exts

data Foo (r :: RuntimeRep) (a :: TYPEvis r) = Foo a
