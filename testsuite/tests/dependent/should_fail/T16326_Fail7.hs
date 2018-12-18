{-# LANGUAGE PolyKinds #-}
module T16326_Fail7 where

import Data.Kind

-- Make sure that this doesn't parse as something goofy like
--  forall (forall :: Type -> Type) (k :: Type). forall k -> k -> Type
data Foo :: forall k -> k -> Type
