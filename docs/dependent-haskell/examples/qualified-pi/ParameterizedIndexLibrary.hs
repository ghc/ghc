{-# LANGUAGE ExperimentalDependentTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Retain ordinary parameterized values and refine their constructor payloads.
module ParameterizedIndexLibrary where

import Data.Kind (Type)
import PackedVecLibrary

-- | Count list constructors without inspecting the Boolean elements.
type family Count (bs :: [Bool]) :: Nat where
    Count '[] = 'Z
    Count (b ': bs) = 'S (Count bs)

-- | Build a vector whose length follows the actual runtime list.
listVector :: foreach (bs :: [Bool]) -> Vec Int (Count bs)
listVector [] = VNil
listVector (b : bs) = VCons (if b then 1 else 0) (listVector bs)

-- | Store a list and a vector indexed by that same retained list.
data ListVector where
    PackListVector :: foreach (bs :: [Bool]) -> Vec Int (Count bs) -> ListVector

-- | Package the actual input list without evaluating its fields.
packListVector :: [Bool] -> ListVector
packListVector bs = PackListVector bs (listVector bs)

-- | Open an imported package and observe its list and vector independently.
openListVector :: ListVector -> (Int, [Int])
openListVector (PackListVector bs xs) = (length bs, toList xs)

-- | Select a payload from an ordinary optional runtime length.
type family OptionalPayload (m :: Maybe Nat) :: Type where
    OptionalPayload 'Nothing = ()
    OptionalPayload ('Just n) = Vec Int n

-- | Refine a vector payload by the actual optional length constructor.
optionalSize :: foreach (m :: Maybe Nat) -> OptionalPayload m -> Int
optionalSize Nothing ignored = 0
optionalSize (Just n) xs = length (toList xs)

-- | Pass a constructed parameterized index with an opaque runtime field.
optionalRoundtrip :: Nat -> Int
optionalRoundtrip n = optionalSize (Just n) (replicateNative n 17)

-- | A uniform recursive datatype with one ordinary erased parameter.
data Tree a = Leaf a | Node (Tree a) (Tree a)

-- | Select a leaf payload from its Boolean value.
type family LeafPayload (b :: Bool) :: Type where
    LeafPayload 'True = Int
    LeafPayload 'False = Bool

-- | Select nested payloads from the actual runtime tree.
type family TreePayload (tree :: Tree Bool) :: Type where
    TreePayload ('Leaf b) = LeafPayload b
    TreePayload ('Node left right) = (TreePayload left, TreePayload right)

-- | Build the payload selected by the retained tree constructors and leaves.
treePayload :: foreach (tree :: Tree Bool) -> TreePayload tree
treePayload (Leaf b) = case b of
    True -> 11
    False -> False
treePayload (Node left right) = (treePayload left, treePayload right)

-- | Render a payload after refining its tree and leaf constructors.
renderTree :: foreach (tree :: Tree Bool) -> TreePayload tree -> String
renderTree (Leaf b) value = case b of
    True -> show value
    False -> show value
renderTree (Node left right) pair = case pair of
    (x, y) -> "(" ++ renderTree left x ++ "," ++ renderTree right y ++ ")"

-- | Refine a call result while preserving the opaque list argument.
vectorFromListCall :: foreach (f :: [Bool] -> Nat) -> foreach (bs :: [Bool]) -> Vec Int (f bs)
vectorFromListCall f bs = case f bs of
    Z -> VNil
    S n -> VCons 7 (replicateNative n 7)

-- | Consume the scoped result of a call with a parameterized input domain.
observeListCall :: ([Bool] -> Nat) -> [Bool] -> [Int]
observeListCall f bs = toList (vectorFromListCall f bs)
