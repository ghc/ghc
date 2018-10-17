{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
module T15122 where

import Data.Kind
import Data.Proxy

data IsStar (a :: k) where
  IsStar :: IsStar (a :: *)

type family F (a :: k) :: k

foo :: (F a ~ F b) => IsStar a -> Proxy b
                   -> Proxy (F a) -> Proxy (F b)
foo IsStar _ p = p
