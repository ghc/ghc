{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Test2 where

import GHC.Internal.Prim

foo :: Int# -> Int#
foo = notI#
