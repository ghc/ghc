{-# LANGUAGE TypeData, DataKinds #-}
module T22332b where

type data X1 = T
data X2 = T
data Proxy a

f :: Proxy T
f = f :: Proxy 'T
