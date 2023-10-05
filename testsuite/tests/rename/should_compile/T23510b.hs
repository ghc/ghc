{-# OPTIONS -Wimplicit-rhs-quantification #-}
{-# LANGUAGE DataKinds, TypeAbstractions #-}
module T23510b where

import Data.Proxy

type T1 :: forall k . Maybe k
type T1 @a = 'Nothing :: Maybe a

type T2 :: forall k j . k -> Either k j
type T2 @a @b = 'Left :: a -> Either a b

type T3 :: forall {k} (d :: k) . Proxy k
type T3  @(a :: k) = 'Proxy :: Proxy k
