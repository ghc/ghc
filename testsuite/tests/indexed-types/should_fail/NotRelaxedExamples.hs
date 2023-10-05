{-# LANGUAGE TypeFamilies #-}

module NotRelaxedExamples where

type family F1 a
type family F2 a
type family F3 a

type instance F1 Char = F1 (F1 Char)
type instance F2 [x]  = F2 [x]
type instance F3 Bool = F3 [Char]
