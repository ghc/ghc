{-# LANGUAGE TypeFamilies, TypeOperators #-}

-- Test infix type constructors in type families

module Infix where

type family x :+: y
type instance Int :+: Int = Int

