{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}

module T20875 where

import Data.Kind (Type)

type family ToKind (b :: Bool) :: Type
type family ToType (b :: Bool) :: ToKind b

type instance ToKind TRUE = Type -> Type
type instance ToType TRUE = Maybe

type TRUE = 'True

