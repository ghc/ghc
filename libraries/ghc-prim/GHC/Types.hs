
{-# OPTIONS_GHC -XNoImplicitPrelude #-}

module GHC.Types where

import GHC.Prim
-- We need Inl etc behind the scenes for the type definitions
import GHC.Generics ()

infixr 5 :

data [] a = [] | a : [a]

data Char = C# Char#

