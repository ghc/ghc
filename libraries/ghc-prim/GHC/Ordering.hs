
{-# OPTIONS_GHC -XNoImplicitPrelude #-}
{-# OPTIONS_GHC -XGenerics          #-}

module GHC.Ordering where

import GHC.Generics (Representable0)


default ()

data Ordering = LT | EQ | GT
        deriving Representable0

