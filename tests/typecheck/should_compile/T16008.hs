{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module T16008 where

import Data.Kind

class C k where
  type S :: k -> Type

data D :: Type -> Type
data SD :: forall a. D a -> Type

instance C (D a) where
  type S = SD
