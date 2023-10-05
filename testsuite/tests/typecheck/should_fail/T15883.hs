{-# Language KindSignatures #-}
{-# Language PolyKinds #-}
{-# Language RankNTypes #-}

module T15883 where

import GHC.Exts

newtype Foo rep = MkFoo (forall (a :: TYPE rep). a)
