{-# LANGUAGE TypeFamilies, RankNTypes, TypeInType #-}

module T14904 where

import Data.Kind

type family F (f :: forall a. g a) where
  F (f :: forall a. g a) = Int

type family F' f :: Type where
  F' ((f :: forall a. g a) :: forall a. g a) = Int
