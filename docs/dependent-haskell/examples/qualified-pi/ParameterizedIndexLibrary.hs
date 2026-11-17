{-# LANGUAGE ExperimentalDependentTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
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

{- Note [Closed-list positions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Fin is the ordinary finite-position GADT used in Agda. Its bound is a promoted
Nat. Count reduces after a retained list constructor refines the actual list.
The inner Fin case then supplies the ordinary successor equality for recursion.
Explicit constructor type arguments name Count rest. An unconstrained meta
cannot stand for that native-dependent bound in the current source checker.

The bound proves a position is below the list length. It does not identify the
list. Equal-count evidence permits reuse with another list. It does not imply
equal list values. lookupRuntime uses one actual list for both operations.

mkFinClosed rejects negative positions before inspecting the list. Construction
and selection traverse only the required prefix. Neither needs the full length,
so finite access to a cyclic list can terminate. indexClosed inspects the chosen
head only when its result is demanded. The empty case still forces its witness.
These functions do not establish totality or eliminate runtime bounds checks.
-}

-- | Carry a position below an ordinary promoted natural bound.
data Fin :: Nat -> Type where
    FZ :: forall (n :: Nat). Fin ('S n)
    FS :: forall (n :: Nat). Fin n -> Fin ('S n)

-- | Select an element with a position below the retained list's length.
-- See Note [Closed-list positions].
indexClosed :: foreach (xs :: [Bool]) -> Fin (Count xs) -> Bool
indexClosed [] impossible = case impossible of {}
indexClosed (value : rest) position = case position of
    FZ -> value
    FS smaller -> indexClosed rest smaller

-- | Check a signed position and return its finite-bound witness.
-- See Note [Closed-list positions].
mkFinClosed :: foreach (xs :: [Bool]) -> Int -> Maybe (Fin (Count xs))
mkFinClosed xs position =
    if position < 0
        then Nothing
        else case xs of
            [] -> Nothing
            _value : rest ->
                if position == 0
                    then Just (FZ @(Count rest))
                    else case mkFinClosed rest (position - 1) of
                        Nothing -> Nothing
                        Just smaller -> Just (FS @(Count rest) smaller)

-- | Construct and consume a checked position with the same actual list.
-- See Note [Closed-list positions].
lookupRuntime :: [Bool] -> Int -> Maybe Bool
lookupRuntime xs position = case mkFinClosed xs position of
    Nothing -> Nothing
    Just checked -> Just (indexClosed xs checked)

{- Note [Generic list positions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
GenericCount records the list shape. It does not inspect the elements.
Retained list refinement supplies the bound for the ordinary Fin case.
The element type can contain an earlier runtime index. Keep that complete type
when returning the selected head. A function element remains an ordinary value.
Selection does not grant native call permission or function equality.

Use the same actual list to construct and consume the witness in lookupGeneric.
Write the constructor bound explicitly as GenericCount rest. Fin proves a bound;
it does not identify the list. Count and GenericCount remain separate families.

mkFinGeneric checks a negative position before inspecting the list. Both workers
traverse only the required prefix. They do not compute the full length. They can
select a finite position from a cyclic list. The caller controls demand on the
selected head. The empty case still forces its witness. These functions do not
establish totality or remove runtime bounds checks.
See Note [Closed-list positions] for the ordinary Fin representation.
-}

-- | Select an element with a position below the retained list's length.
-- See Note [Generic list positions].
indexGeneric :: forall (a :: Type). foreach (xs :: [a]) -> Fin (GenericCount xs) -> a
indexGeneric [] impossible = case impossible of {}
indexGeneric (value : rest) position = case position of
    FZ -> value
    FS smaller -> indexGeneric @a rest smaller

-- | Check a signed position without inspecting the list elements.
-- See Note [Generic list positions].
mkFinGeneric :: forall (a :: Type). foreach (xs :: [a]) -> Int -> Maybe (Fin (GenericCount xs))
mkFinGeneric xs position =
    if position < 0
        then Nothing
        else case xs of
            [] -> Nothing
            _value : rest ->
                if position == 0
                    then Just (FZ @(GenericCount rest))
                    else case mkFinGeneric @a rest (position - 1) of
                        Nothing -> Nothing
                        Just smaller -> Just (FS @(GenericCount rest) smaller)

-- | Construct and consume a checked position with the same actual list.
-- See Note [Generic list positions].
lookupGeneric :: forall (a :: Type). [a] -> Int -> Maybe a
lookupGeneric xs position = case mkFinGeneric @a xs position of
    Nothing -> Nothing
    Just checked -> Just (indexGeneric @a xs checked)

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

-- | Use a phantom parameter to distinguish nominal and representational evidence.
data DomainToken (tag :: Type) = DomainToken

-- | Reduce a closed token parameter with ordinary type-family evaluation.
type family NormalizeTokenTag (tag :: Type) :: Type where
    NormalizeTokenTag Bool = Bool

-- | Retain the family application in the source domain.
type SourceToken = DomainToken (NormalizeTokenTag Bool)

-- | Use the nominally equal domain without the family application.
type TargetToken = DomainToken Bool

{- Note [Source nominal domain conversion]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The two token domains differ by an ordinary nominal family reduction.
DomainToken has a phantom parameter. Representational weakening alone would
lose the proof needed to transport an index. Keep the nominal proof in the
source argument cast and retained function cast. Neither cast evaluates the
token. Casting the function also preserves its bottom behavior.

numericTokenVector leaves an ordinary payload choice before its retained
arguments. specializedTokenVector selects Int while comparing the converted
function bodies. This does not assign a runtime index to a metavariable.
-}

-- | Build a vector after an opaque runtime call selects its length.
tokenVector :: foreach (f :: SourceToken -> Nat) -> a -> foreach (x :: SourceToken) -> Vec a (f x)
tokenVector f value x = case f x of
    Z -> VNil
    S n -> VCons value (replicateNative n value)

-- | Convert a retained domain without expanding the original function.
-- See Note [Source nominal domain conversion].
adaptTokenVector :: foreach (f :: SourceToken -> Nat) -> (foreach (x :: SourceToken) -> Vec a (f x)) -> foreach (y :: TargetToken) -> Vec a (f y)
adaptTokenVector f original = original

-- | Convert the retained domain in the reverse nominal direction.
adaptTokenVectorBack :: foreach (f :: SourceToken -> Nat) -> (foreach (y :: TargetToken) -> Vec a (f y)) -> foreach (x :: SourceToken) -> Vec a (f x)
adaptTokenVectorBack f original = original

-- | Convert an actual argument before instantiating the retained result.
-- See Note [Source nominal domain conversion].
applyTokenVector :: foreach (f :: SourceToken -> Nat) -> foreach (y :: TargetToken) -> Vec Int (f y)
applyTokenVector f y = tokenVector f 7 y

-- | Leave the ordinary numeric payload choice before the retained arguments.
numericTokenVector :: forall a. Num a => foreach (f :: SourceToken -> Nat) -> foreach (x :: SourceToken) -> Vec a (f x)
numericTokenVector f = tokenVector @a f 42

-- | Infer the ordinary payload while comparing the converted retained bodies.
-- See Note [Source nominal domain conversion].
specializedTokenVector :: foreach (f :: SourceToken -> Nat) -> foreach (y :: TargetToken) -> Vec Int (f y)
specializedTokenVector f = numericTokenVector f

-- | Force the converted function without applying its retained argument.
forceTokenFunction :: foreach (f :: SourceToken -> Nat) -> (foreach (x :: SourceToken) -> Vec Int (f x)) -> IO ()
forceTokenFunction f worker =
    seq @(foreach (y :: TargetToken) -> Vec Int (f y)) (adaptTokenVector f worker) (pure ())

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
