{-# LANGUAGE DataKinds, GADTs, KindSignatures #-}
module NativeClosedIndexedContextLib where

import Numeric.Natural (Natural)

data Context (b :: Bool) where
  Context :: Show a => a -> Context 'True

unsupported :: Natural -> Context 'True
unsupported _ = errorWithoutStackTrace "indexed dictionary producer ran"
