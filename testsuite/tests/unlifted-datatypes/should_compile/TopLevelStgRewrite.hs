{-# OPTIONS_GHC -O #-}
{-# LANGUAGE UnliftedDatatypes #-}

module TopLevelStgRewrite where

import TopLevelStgRewritea
import GHC.Exts
import Data.Kind (Type)

type Box :: UnliftedType -> Type
data Box a = Box a

type B :: Type -> UnliftedType
data B a = B !a

b :: Box (B Bool)
b = Box (B a)
