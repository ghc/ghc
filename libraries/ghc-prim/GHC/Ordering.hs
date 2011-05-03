
{-# OPTIONS_GHC -XNoImplicitPrelude #-}
{-# OPTIONS_GHC -XGenerics          #-}

module GHC.Ordering where

-- We need generics behind the scenes for the Ordering definition
import GHC.Generics ()
import GHC.CString ()

default ()

data Ordering = LT | EQ | GT

