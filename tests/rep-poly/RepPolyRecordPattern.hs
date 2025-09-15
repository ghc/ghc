{-# LANGUAGE UnliftedNewtypes #-}

module RepPolyRecordPattern where

import GHC.Exts

newtype X (a :: TYPE rep) = MkX { fld :: a }

class C rep (a :: TYPE rep) where
  meth :: () -> a

upd :: forall rep (a :: TYPE rep). X a -> a
upd ( MkX bndr_a ) = bndr_a
