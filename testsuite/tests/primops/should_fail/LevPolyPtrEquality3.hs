{-# LANGUAGE MagicHash #-}

module LevPolyPtrEquality3 where

import GHC.Exts
  ( Int#
  , unsafeCoerce#, reallyUnsafePtrEquality#
  )

f :: a -> b -> Int#
f a b = unsafeCoerce# reallyUnsafePtrEquality# a b
