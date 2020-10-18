{-# LANGUAGE NoTypeOperators #-}

module T18862a where

f :: (a ~ b) => a -> b
f = id
