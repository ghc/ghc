{-# LANGUAGE TypeFamilies #-}

module T7536 where

import Data.Kind

type T (v :: Type) = Int

type family TF a :: *
type instance TF (T a) = a

