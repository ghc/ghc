{-# LANGUAGE GADTs #-}
module T20278 where

import Data.Kind
import GHC.Exts

type X1 :: TYPE rep -> Type
data X1 a where
  MkX1 :: { fld1a :: a, fld1b :: Int } -> X1 a

upd1 :: forall rep (a :: TYPE rep). X1 a -> X1 a
upd1 x = x { fld1b = 3 }

type X2 :: Type -> Type
data X2 a where
  MkX2 :: { fld2a :: b, fld2b :: Int } -> X2 (Maybe b)

upd2 :: X2 a -> X2 a
upd2 x = x { fld2b = 3 }
