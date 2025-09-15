{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
module T14904a where

import Data.Kind

type F :: (forall a. g a) -> Type
type family F f :: Type where
  F (f :: forall a. g a) = Int
