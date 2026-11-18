{-# LANGUAGE DataKinds, ExperimentalDependentTypes, FlexibleContexts, NoStarIsType, RankNTypes, TypeOperators #-}
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

-- The runtime dictionary can multiply differently from the checked function.
givenProduct :: Num Nat => foreach (slices :: Nat) -> foreach (channels :: Nat)
             -> Vec Bool (S (times slices channels))
givenProduct slices channels =
    let actualSize = slices * channels
    in replicateNative (S actualSize) True

withProduct :: Bool -> (foreach (r :: Nat) -> foreach (c :: Nat) -> Vec Bool (times r c)) -> Bool
withProduct x build = x && and (toList (build 1 1))

{-# NOINLINE concrete #-}
concrete :: Bool -> Bool
concrete x = x

{-# NOINLINE supplied #-}
supplied :: Num Nat => Bool -> Bool
supplied x = x

-- Only the second rule quantifies over a dictionary from its left-hand side.
-- The speculative solve of its RHS uses the concrete instance first.
{-# RULES "concrete product" forall x. concrete x =
    withProduct x (\r c -> let cells = r * c in replicateNative cells x) #-}
{-# RULES "supplied product" forall x. supplied x =
    withProduct x (\r c -> let cells = r * c in replicateNative cells x) #-}
