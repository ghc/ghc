{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
module T15829 where

import Data.Kind

data A :: Type -> Type
data B a :: A a -> Type
type C a = B a
