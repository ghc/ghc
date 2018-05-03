{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
module T14904a where

import Data.Kind

type family F (f :: forall a. g a) :: Type where
  F (f :: forall a. g a) = Int
