{-# LANGUAGE TypeFamilies #-}

module T15368 where

import Data.Kind (Type)

transitive :: (a, b) -> (b, c) -> (a, c)
transitive = undefined

trigger :: a -> b -> (F a b, F b a)
trigger _ _ = _ `transitive` trigger _ _

type family F (n :: Type) (m :: Type) :: Type
