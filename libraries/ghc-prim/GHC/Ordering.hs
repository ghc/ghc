{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude, DeriveGeneric #-}

module GHC.Ordering where

import GHC.Generics (Generic)


default ()

data Ordering = LT | EQ | GT
        deriving Generic

