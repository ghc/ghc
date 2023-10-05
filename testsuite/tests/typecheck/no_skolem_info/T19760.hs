{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Bug where

import Data.Kind

f :: forall a (p :: Maybe a -> Type) (m :: Maybe a). p m
f = go
  where
    go :: forall a' (m' :: Maybe a'). p m'
    go = undefined
