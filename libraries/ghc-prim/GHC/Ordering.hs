
{-# OPTIONS_GHC -XNoImplicitPrelude #-}
{-# OPTIONS_GHC -XNoGenerics        #-}

module GHC.Ordering where

-- We need Inl etc behind the scenes for the Ordering definition
import GHC.Generics ()

default ()

data Ordering = LT | EQ | GT

