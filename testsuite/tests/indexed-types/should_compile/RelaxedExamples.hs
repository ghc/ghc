{-# LANGUAGE TypeFamilies #-}

module RelaxedExamples where

type family F1 a
type family F2 a
type family F3 a
type family F4 a

type instance F1 x      = x
type instance F2 [Bool] = F2 Char
type instance F3 (a, b) = (F3 a, F3 b)
type instance F4 x      = (x, x)