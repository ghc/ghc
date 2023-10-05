{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE PolyKinds #-}
module T10524 where

import Data.Data

newtype WrappedFunctor f a = WrapFunctor (f a) deriving (Data, Typeable)

-- WrappedFunctor :: forall k. (k -> *) -> k -> *
