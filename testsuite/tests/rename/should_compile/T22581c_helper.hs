{-# LANGUAGE TypeData, TypeFamilies #-}

module T22581c_helper (K(T), C((#), Tf, Df)) where

type data K = T

class C a where
  type a # b
  type Tf a
  data Df a