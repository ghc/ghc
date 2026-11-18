{-# LANGUAGE DataKinds, ExperimentalDependentTypes, TemplateHaskell, TypeFamilies #-}
module NativeSourceLengthFamily where

import Data.Kind (Type)

type family Element a :: Type
data Nat = Z | S Nat

count :: [Element ()] -> Nat
count []       = Z
count (_ : xs) = S (count xs)

-- Finish checking the ordinary function before looking up its equations.
$(pure [])

type Answer = count ('[] :: [Element ()])
