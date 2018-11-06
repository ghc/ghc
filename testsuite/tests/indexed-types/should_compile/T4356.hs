{-# LANGUAGE TypeFamilies #-}
module T4356 where

import Data.Kind (Type)

type family T t :: Type -> Type -> Type
type instance T Bool = (->)

f :: T Bool Bool Bool
f = not
