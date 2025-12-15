{-# LANGUAGE DeriveAnyClass #-}

module T16731 where

import Data.Kind

class C (a :: Type) (b :: Type)

type T :: forall a. a -> Type
data T (x :: z) deriving (C z)
