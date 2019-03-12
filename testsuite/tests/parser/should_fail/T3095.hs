{-# LANGUAGE KindSignatures #-}
-- #3095
module T3095 where

import Data.Kind (Type)

class Bla (forall x . x :: Type) where
