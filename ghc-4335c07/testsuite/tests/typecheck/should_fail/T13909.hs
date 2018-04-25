{-# LANGUAGE TypeInType #-}
module T13909 where

import Data.Kind

data Hm (k :: Type) (a :: k) :: Type

class HasName (a :: k) where
  getName :: proxy a -> String

instance HasName Hm where
  getName _ = "Hm"
