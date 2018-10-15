{-# LANGUAGE TypeFamilies #-}
module Tc245_A where

import Data.Kind (Type)

class Foo a where
    data Bar a :: Type -> Type
