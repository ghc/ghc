{-# LANGUAGE BangPatterns #-}

module Proposal229a where

data T a b = a :! b

(!) :: x -> T a b -> (x, a, b)
~u ! !(!m :! !n) = (u, m, n)
