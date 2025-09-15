{-# LANGUAGE TypeData #-}
module TDRecursive where

type data T f = K (f (K Int))
