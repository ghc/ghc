{-# LANGUAGE UnliftedNewtypes #-}

module RepPolyRecordUpdate where

import GHC.Exts

newtype X (a :: TYPE rep) = MkX { fld :: a }

class C rep (a :: TYPE rep) where
  meth :: () -> a

upd :: C rep a => X b -> X a
upd x = x { fld = meth () }
