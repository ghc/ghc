
{-# OPTIONS_GHC -XNoImplicitPrelude #-}

module GHC.Types where

import GHC.Prim

infixr 5 :

data [] a = [] | a : [a]

data Char = C# Char#

