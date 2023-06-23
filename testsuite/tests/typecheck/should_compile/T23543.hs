{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
module T23543 where

type N :: forall a. Maybe a
type N = ('Nothing :: forall a. Maybe a)

type L :: forall a. [a]
type L = ('[] :: forall a. [a])
