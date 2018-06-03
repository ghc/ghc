{-# language MagicHash #-}
{-# language UnboxedTuples #-}
{-# language UnliftedNewtypes #-}

module UnliftedNewtypesInfinite where

import GHC.Exts (Int#)

newtype Foo = FooC (# Int#, Foo #)
