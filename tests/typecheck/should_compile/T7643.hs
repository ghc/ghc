{-# LANGUAGE MagicHash, TypeFamilies #-}

module T7643 where

import GHC.Exts

type family T
type instance T = RealWorld

foo :: () -> State# T
foo _ = unsafeCoerce# realWorld#
