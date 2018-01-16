{-# LANGUAGE InstanceSigs, PolyKinds #-}

module T9833 where

import Control.Applicative

data NullableInterp a = NullI Bool

instance Functor     NullableInterp where
  fmap = undefined
instance Applicative NullableInterp where
  pure  = undefined
  (<*>) = undefined

instance Alternative NullableInterp where
  empty :: NullableInterp a
  empty = undefined
  (<|>) = undefined