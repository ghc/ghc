{-# LANGUAGE DefaultSignatures #-}

module T20588c where

import Data.Kind

class C (a :: Type) where
  meth :: a
  default meth :: Monoid a => a
  meth = mempty
