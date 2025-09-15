{-# language GADTSyntax #-}
{-# language KindSignatures #-}
{-# language MagicHash #-}
{-# language UnliftedNewtypes #-}

module UnliftedNewtypesMismatchedKindRecord where

import GHC.Exts

newtype Foo :: TYPE 'IntRep where
  FooC :: { getFoo :: Word# } -> Foo
