{-# Language RankNTypes #-}
{-# Language KindSignatures #-}
{-# Language PolyKinds #-}
{-# Language UnliftedNewtypes #-}

module UnliftedNewtypesForall where

import GHC.Exts

newtype Foo rep = MkFoo (forall (a :: TYPE rep). a)
