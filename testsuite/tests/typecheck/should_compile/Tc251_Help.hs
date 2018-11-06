{-# LANGUAGE TypeFamilies #-}
module Tc251_Help where

import Data.Kind (Type)

class Cls a where
    type Fam a :: Type
    type Fam a = Maybe a
