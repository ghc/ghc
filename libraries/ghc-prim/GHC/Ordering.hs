{-# LANGUAGE Trustworthy #-}
{-# OPTIONS_GHC -XNoImplicitPrelude #-}
{-# OPTIONS_GHC -XDeriveGeneric     #-}

module GHC.Ordering where

import GHC.Generics (Generic)


default ()

data Ordering = LT | EQ | GT
        deriving Generic

