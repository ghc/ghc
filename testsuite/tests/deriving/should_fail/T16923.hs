{-# LANGUAGE DerivingVia #-}
module T16923 where

data Foo deriving () via Maybe Maybe
