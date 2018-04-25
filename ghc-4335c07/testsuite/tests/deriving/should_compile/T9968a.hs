{-# LANGUAGE DeriveAnyClass #-}

module T9968 where

import Data.Bifunctor

data Blah a b = A a | B b
  deriving (Bifunctor)
