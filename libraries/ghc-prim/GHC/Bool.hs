
{-# OPTIONS_GHC -XNoImplicitPrelude #-}

module GHC.Bool where

-- We need Inl etc behind the scenes for the Bool definition
import GHC.Generics ()

default ()

data Bool = False | True
