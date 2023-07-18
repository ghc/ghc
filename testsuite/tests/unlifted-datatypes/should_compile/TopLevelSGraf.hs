{-# LANGUAGE UnliftedDatatypes #-}

module TopLevelSGraf where

import GHC.Exts
import Data.Kind

type Box :: UnliftedType -> Type
data Box a = Box a
type UMaybe :: Type -> UnliftedType
data UMaybe a = UJust !a | UNothing

y :: Int
y = sum [0..100]
{-# OPAQUE y #-}

x :: Box (UMaybe Int)
x = Box (UJust y)
{-# OPAQUE x #-}

main = case x of
  Box (UJust n) -> print n
