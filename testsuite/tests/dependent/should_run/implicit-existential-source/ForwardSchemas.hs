{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, RankNTypes, RequiredTypeArguments, TypeFamilies #-}
module ForwardSchemas
    ( HiddenBar, Restricted, barValue, barIsEmpty, restrict, keepInput, lateResult
    , SlotBox, slotBox, slotLabel, Ranked, ranked, inspectRanked
    ) where

import Data.Kind (Type)
import Data.Proxy (Proxy(..))
import qualified GHC.Exists as Exists
import Vector

type HiddenBar a = exists (n :: Nat) | Vec n a
type Restricted n a = exists (m :: Nat). (Vec n a, Vec m a)

barValue :: HiddenBar a
barValue = Exists.Pack (type 'Z) Nil

barIsEmpty :: HiddenBar a -> Bool
barIsEmpty p = case p of
    Exists.Pack _ xs -> case xs of
        Nil -> True
        Cons _ _ -> False

restrict :: Vec n a -> Vec m a -> Restricted n a
restrict ls rs = (ls, rs)

-- | Erase the second length, leaving the input length visible in the result.
keepInput :: Vec n a -> Restricted n a
keepInput xs = restrict xs Nil

type Stamped stamp a = exists (n :: Nat). (Proxy stamp, Vec n a)

-- | The stamp can be selected after the first value argument.
lateResult :: Bool -> forall stamp. Vec n a -> Stamped stamp a
lateResult _ xs = (Proxy, xs)

type family Slot (w :: Type) :: Type
type SlotBox a = exists (w :: Type) | (a, Proxy (Slot w))

slotBox :: a -> SlotBox a
slotBox x = Exists.Pack (type Bool) (x, Proxy)

slotLabel :: SlotBox a -> a
slotLabel p = case p of Exists.Pack _ (x, _) -> x

type Ranked = exists (w :: Type) | forall b. b -> (b, Proxy w)

ranked :: Ranked
ranked = Exists.Pack (type Bool) (\x -> (x, Proxy))

inspectRanked :: Ranked -> Int
inspectRanked p = case p of Exists.Pack _ f -> fst (f (11 :: Int))
