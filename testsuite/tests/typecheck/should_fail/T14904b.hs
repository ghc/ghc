{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
module T14904b where

import Data.Kind

type family F f :: Type where
  F ((f :: forall a. g a) :: forall a. g a) = Int
