{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
module T21752A (
    module GHC.Exts
) where

import GHC.Exts

type T = TYPE

type O = One
type M = Many

--type F = FUN
