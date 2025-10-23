{-# LANGUAGE ExplicitNamespaces #-}
module T12488a
  ( T (type A)
  , (:!) (type (:/))
  ) where

data T = A

data (:!) = (:/)