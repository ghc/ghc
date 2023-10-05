{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
module T15795 where

import Data.Kind

type KindOf (a :: k) = k

data T :: forall j (a :: j). KindOf a -> Type where
  MkT :: forall k (b :: k). T b

f :: forall k (b :: k). T b
f = error "urk"
