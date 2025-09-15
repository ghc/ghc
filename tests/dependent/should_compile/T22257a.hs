{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module T22257a where

import Data.Kind (Type)

type family F (x :: Type) :: Type
type family G (x :: Type) (y :: F x) :: Type

$(return [])

type instance F () = Type
type instance G () k = k
