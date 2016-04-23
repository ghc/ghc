{-# LANGUAGE ExtendedDefaultRules #-}

module T11974b where

default (Either, Monad, [], Maybe, Either Bool, Integer, Double, Blah)

data Blah
