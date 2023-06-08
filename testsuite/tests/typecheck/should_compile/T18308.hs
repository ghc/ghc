{-# LANGUAGE TypeFamilies #-}
module T18308 where

import Data.Kind (Type)
import Data.Proxy (Proxy)

class Cls where
    type Fam (k :: Type) (a :: k) :: Type
    mtd :: Proxy k -> Proxy (a :: k) -> Fam k a -> Int
