{-# LANGUAGE MagicHash #-}

module T21152 where

import GHC.Exts ( Int#, reallyUnsafePtrEquality# )

reallyUnsafePtrEquality :: a -> a -> Int#
reallyUnsafePtrEquality = reallyUnsafePtrEquality#
