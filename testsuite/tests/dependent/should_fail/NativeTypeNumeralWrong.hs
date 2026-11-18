{-# LANGUAGE DataKinds, ExperimentalDependentTypes, NoStarIsType, TypeOperators #-}
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
badSecond n m = tagBoth (n + m) (-1)

wrongGrouping :: foreach (rows :: Nat) -> foreach (cols :: Nat) -> Vec Bool ((1 + rows) * cols)
wrongGrouping rows cols = replicateNative (1 + rows * cols) True

-- The local definition supplies the product, but addition inspects its first argument.
wrongLocalSuccessor :: foreach (height :: Nat) -> foreach (width :: Nat) -> Vec Bool (1 + height * width)
wrongLocalSuccessor height width =
    let cells = times height width
    in replicateNative (cells + 1) True
