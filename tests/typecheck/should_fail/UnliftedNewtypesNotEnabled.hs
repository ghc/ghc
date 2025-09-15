{-# LANGUAGE MagicHash #-}

module UnliftedNewtypesNotEnabled
  ( Baz(..)
  ) where

import GHC.Exts (Int#)

newtype Baz = Baz Int#
