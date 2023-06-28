{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}

module T14668b where

import Prelude
import Data.Kind

type family KeyKind (obj :: Type) :: Type
type family ValKind (obj :: Type) :: Type

type family Get (key :: KeyKind a) (obj :: a) :: ValKind a

data Map (k :: Type) (v :: Type) = Map [(k,v)]

type instance KeyKind (Map k v) = k
type instance ValKind (Map k v) = v

type instance Get k ('Map ('(k,v) ': _)) = v

