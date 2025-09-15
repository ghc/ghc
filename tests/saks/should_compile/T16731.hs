{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module T16731 where

import Data.Kind

class C (a :: Type) (b :: Type)

type T :: forall a. a -> Type
data T (x :: z) deriving (C z)
