{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Test where

import GHC.Internal.Prim

foo :: Int# -> Int#
foo = notI#
