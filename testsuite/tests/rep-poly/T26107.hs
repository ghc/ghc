{-# LANGUAGE GADTs, UnboxedTuples #-}
module T26107 where

import Data.Kind
import GHC.Exts

type T :: TYPE rep -> Type
data T a where
  A :: T Bool
  B :: T (# #)

f :: forall rep (a :: TYPE rep). T a -> a
f A = True
f B = (# #)
