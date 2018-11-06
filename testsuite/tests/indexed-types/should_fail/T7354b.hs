{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
module T7354b where

import Data.Kind (Type)

type family Base t :: Type -> Type

class Unfoldable t where
  embed :: Base t t -> t
