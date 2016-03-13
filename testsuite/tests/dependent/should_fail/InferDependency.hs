{-# LANGUAGE TypeInType #-}

module InferDependency where

data Proxy k (a :: k)
data Proxy2 k a = P (Proxy k a)
