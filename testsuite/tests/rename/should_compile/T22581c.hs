{-# LANGUAGE ExplicitNamespaces #-}

module T22581c where

import T22581c_helper (K(type T), C(type (#), type Tf, type Df))

type T' :: K
type T' = T

type C' a = C a
type (#.) a b = a # b
type Tf' a = Tf a
type Df' a = Df a