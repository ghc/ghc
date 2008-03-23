
{-# OPTIONS_GHC -fno-implicit-prelude #-}

module GHC.Bool where

-- We need Inl etc behind the scenes for the Bool definition
import GHC.Generics

default ()

data Bool = False | True
