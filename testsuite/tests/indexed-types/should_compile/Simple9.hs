{-# LANGUAGE TypeFamilies #-}

module Simple9 where

import Data.Kind (Type)

-- The test succeeds with
--
-- type family F a b
-- type instance F () b = Maybe b

type family F a :: Type -> Type
type instance F () = Maybe

type family G a
type instance G (Maybe a) = Int

foo :: G (F () a) -> Int
foo x = x

