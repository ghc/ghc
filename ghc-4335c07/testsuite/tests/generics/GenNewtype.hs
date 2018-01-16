{-# LANGUAGE DeriveGeneric #-}

module Main where

import GHC.Generics

data    X = X   deriving Generic
newtype Y = Y X deriving Generic

main = print [isNewtype (from X), isNewtype (from (Y X))]
