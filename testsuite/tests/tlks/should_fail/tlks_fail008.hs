{-# LANGUAGE TopLevelKindSignatures #-}
{-# LANGUAGE KindSignatures #-}

module TLKS_Fail008 where

import Data.Kind (Type)

type T :: Type -> (Type -> Type) -> Type
data T a (b :: Type -> Type) x1 (x2 :: Type -> Type)
