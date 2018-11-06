{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
module T11416 where

import Data.Kind

type ConstantT a b = a

newtype T f (a :: ConstantT Type f) = T (f a)
  deriving Functor

data family TFam1 (f :: k1) (a :: k2)
newtype instance TFam1 f (ConstantT a f) = TFam1 (f a)
  deriving Functor

data family TFam2 (f :: k1) (a :: k2)
newtype instance TFam2 f (a :: ConstantT Type f) = TFam2 (f a)
  deriving Functor
