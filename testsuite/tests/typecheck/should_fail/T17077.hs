{-# LANGUAGE RankNTypes, PolyKinds #-}
{-# OPTIONS_GHC -fprint-explicit-kinds #-}
module T17077 where

import Data.Proxy

t :: Proxy (z :: forall k. a)
t = t
