{-# LANGUAGE DatatypeContexts #-}

module T4325 where

data Ord a => Heap a b = Empty | Node a b [Heap a b]
    deriving Eq

