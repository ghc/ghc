{-# LANGUAGE DataKinds, ExperimentalDependentTypes, TypeOperators #-}
module NativeTypeNumeralWrong where

import Data.Proxy
import Data.Type.Equality
import PackedVecLibrary

two :: Vec Bool 2
two = VCons True VNil

infixr 5 <||>
(<||>) :: Bool -> Bool -> Bool
False <||> b = b
True  <||> b = True

unknown :: Proxy b -> (b <||> 'False) :~: 'True
unknown _ = Refl

tagBoth :: foreach (n :: Nat) -> foreach (m :: Nat) -> (Proxy n, Proxy m)
tagBoth _n _m = (Proxy, Proxy)

-- Only the second retained operand lacks a checked index.
badSecond :: foreach (n :: Nat) -> foreach (m :: Nat) -> (Proxy (n + m), Proxy (1 :: Nat))
badSecond n m = tagBoth (n + m) 1
