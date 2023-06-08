{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
module T15122 where

import Data.Kind
import Data.Proxy

type IsStar :: k -> Type
data IsStar a where
  IsStar :: IsStar (a :: Type)

type family F (a :: k) :: k

foo :: (F a ~ F b) => IsStar a -> Proxy b
                   -> Proxy (F a) -> Proxy (F b)
foo IsStar _ p = p
