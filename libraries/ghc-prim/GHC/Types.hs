
{-# OPTIONS_GHC -XNoImplicitPrelude #-}

module GHC.Types where

infixr 5 :

data [] a = [] | a : [a]

