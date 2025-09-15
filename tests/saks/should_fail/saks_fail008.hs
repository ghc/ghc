{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE KindSignatures #-}

module SAKS_Fail008 where

import Data.Kind (Type)

type T :: Type -> (Type -> Type) -> Type
data T a (b :: Type -> Type) x1 (x2 :: Type -> Type)
