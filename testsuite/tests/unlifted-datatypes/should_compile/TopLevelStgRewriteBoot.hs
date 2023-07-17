{-# LANGUAGE UnliftedDatatypes #-}

module TopLevelStgRewriteBoot where

import {-# SOURCE #-} TopLevelStgRewriteBoota
import GHC.Exts
import Data.Kind (Type)

x :: Bool
x = True

type Box :: UnliftedType -> Type
data Box a = Box a

type B :: Type -> UnliftedType
data B a = B !a

b :: Box (B Bool)
b = Box (B a)