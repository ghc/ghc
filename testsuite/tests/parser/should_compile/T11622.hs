{-# LANGUAGE TypeFamilies #-}
module T11622 where

import Data.Kind (Type)

type family F a where
  F _ = Int :: Type
