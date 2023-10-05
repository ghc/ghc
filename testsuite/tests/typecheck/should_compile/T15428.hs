{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
module T15428 where

data Flurmp
type family Pure (x :: a) :: f a

type T = Pure Flurmp Flurmp
