{-# LANGUAGE BangPatterns, LinearTypes, MonoLocalBinds, ImpredicativeTypes, LambdaCase, NoExperimentalDependentTypes, RequiredTypeArguments, ScopedTypeVariables #-}
module RankedPackageClient
    ( runGuarded, runConditional, runArgumentWitness, runProjections, projectAt, runDeclaredWitnesses, runExplicitBranches
    , runRankedInput
    , runMixedPackage, runProducedPackage, runForwardedPackage, runLabelledPackage
    , runSelectedPackage, firstSelected, firstSelectedFilter, firstByOrdering, firstForwarded, runLocalPattern, readCount, refilter, refilterRows, refilterNumericZip
    , packageFiltered, readFiltered
    , swapRowsLet, swapRowsCase
    ) where

import AlignedList (zipWithExact)
import Data.Bool (bool)
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy(..))
import qualified ErasedPackage
import ImplicitVector (Filtered, FilteredWith, Rows(..), HiddenRows, cells, filterVec, packRows, rowLists, withVector)
import PackedVecLibrary (Nat(..), Vec(..), toList)
import RankedPackage
    ( Callback(..), CallbackPair, GuardedBuilder, Hidden, PolyId, PolyMaybe
    , TwoWitnessBuilder, atTwoTypes, consumeIdentity, fromLeft, fromRight
    , observeCallback, observeGuarded, observeTwo, pairFor, withWitness
    , InputAt, RankedInput, observeRankedInput
    )

-- | Both projections use the package chosen for the same input.
projectAt :: Bool -> (Int, Bool)
projectAt b = snd (pairFor b) (fst (pairFor b))

-- | Apply both selectors to a pair with one hidden type.
runProjections :: (Int, Bool)
runProjections = project (id, atTwoTypes)
  where
    project :: CallbackPair -> (Int, Bool)
    project p = snd p (fst p)

-- | Use the declared consumer to infer the witness for both guards.
builder :: GuardedBuilder
builder = \cases
    ready input | ready && input -> consumeIdentity id
                | otherwise -> Callback id atTwoTypes

-- | Skip the input when the first flag is false.
-- Run the typed branch when both flags are true.
runGuarded :: ((Int, Bool), (Int, Bool))
runGuarded = (observeGuarded builder False (error "unused guard input"),
              observeGuarded builder True True)

-- | Use one hidden callback type for both arms of the conditional producer.
runConditional :: Bool -> Bool -> (Int, Bool)
runConditional flag input =
    observeGuarded
        (\choose ready -> if choose
            then Callback id atTwoTypes
            else if ready then consumeIdentity id else error "selected conditional arm")
        flag input

-- | Hide the polymorphic identity beneath the callback's forall.
runRankedInput :: Bool -> Int
runRankedInput useCallback = observeRankedInput useCallback packed
  where
    packed :: RankedInput
    packed = \receive -> produce receive

    produce :: InputAt PolyId -> Int
    produce receive = fromMaybe 43 (receive readIdentity id)

    readIdentity :: PolyId -> Int
    readIdentity f = case atTwoTypes f of
        (n, accepted) -> if accepted then n else 0

-- | Infer the hidden callback type from the argument of id.
runArgumentWitness :: Bool -> (Int, Bool)
runArgumentWitness flag =
    observeGuarded (\_ input -> id (prepare input)) True flag
  where
    prepare :: Bool -> Callback PolyId
    prepare input = Callback @PolyId id (\f -> (f 7, f input))

-- | Read one witness from a type argument and two from separate equations.
runDeclaredWitnesses :: ((Int, Bool), ((Int, Bool), (Int, Bool)))
runDeclaredWitnesses =
    ( observeGuarded required True True
    , (observeTwo joint False (error "unused callback input"), observeTwo joint True False)
    )
  where
    required :: GuardedBuilder
    required = \_ input -> (withWitness @Int (type PolyId) Proxy) id (\f -> (f 7, f input))

    joint :: TwoWitnessBuilder
    joint = \cases
        False input -> fromLeft input
        True input -> fromRight input

-- | Select each branch's witness and leave an unused result field lazy.
runExplicitBranches :: ((Int, Bool), (Int, Bool), Int)
runExplicitBranches =
    ( observeCallback (choose True True)
    , observeCallback (choose False False)
    , fst (observeCallback (choose False (error "unused callback input")))
    )
  where
    choose :: Bool -> Bool -> Hidden
    choose branch input =
        if branch
            then Callback @PolyId id (\f -> (f 7, f input))
            else Callback @PolyMaybe Just
                (\f -> (maybe 0 id (f 17), maybe False id (f input)))

-- | Hide the outer witness in an ordinary module and read the number.
runMixedPackage :: Int -> Int
runMixedPackage n = ErasedPackage.readMixedNumber (hide n)

-- | Pass the produced outer package directly to its polymorphic reader.
runProducedPackage :: Int -> Int
runProducedPackage n = ErasedPackage.readNumber (hide n)

-- | Pass the producer's result through a polymorphic function.
runForwardedPackage :: Int -> Int
runForwardedPackage n = ErasedPackage.readNumber (id (hide n))

-- | Read the label and number while keeping both witnesses hidden.
runLabelledPackage :: Int -> (String, Int)
runLabelledPackage n = ErasedPackage.readLabelledNumber (id (hide n))

-- | Read the selected package and leave the other package unevaluated.
runSelectedPackage :: Bool -> (String, Int)
runSelectedPackage chooseLeft
    | chooseLeft = readSelected (hide 37) unused
    | otherwise = readSelected unused (hide 41)
  where
    unused :: ErasedPackage.MixedNumber
    unused = error "unselected package"

    readSelected :: ErasedPackage.MixedNumber -> ErasedPackage.MixedNumber -> (String, Int)
    readSelected left right = ErasedPackage.readLabelledNumber (bool right left chooseLeft)

-- | Bind the two fields under the same hidden witness.
runLocalPattern :: Bool -> (Int, Bool)
runLocalPattern flag = let (value, reader) = pairFor flag in reader value

-- | Hide the first witness after packing the second.
hide :: Int -> ErasedPackage.MixedNumber
hide value = ErasedPackage.packNumber value

-- | Filter at the length hidden by the first filter and return a new package.
refilter :: (a -> Bool) -> (a -> Bool) -> Vec a n -> Filtered a
refilter first second xs =
    let kept = withVector (filterVec second) (filterVec first xs)
    in kept

-- | Store a vector with a reader at the length chosen by the filter.
packageFiltered :: (a -> Bool) -> (forall k. Vec a k -> r) -> Vec a n -> FilteredWith a r
packageFiltered predicate reader xs = (filterVec predicate xs, reader)

-- | Open the stored vector and apply its reader.
readFiltered :: FilteredWith a r -> r
readFiltered package = let (xs, reader) = package in reader xs

-- | Open the hidden state and pass it once to the stored reader.
readCount :: ErasedPackage.EncodedCount %1 -> Int
readCount encoded = let !(state, reader) = encoded in reader state

-- | Apply a final filter to each independently selected row.
refilterRows :: forall a n m. (a -> Bool) -> (a -> Bool) -> (a -> Bool)
             -> Vec a n -> Vec a m -> ([a], [a])
refilterRows leftTest rightTest keep left right =
    let rows = withRows (select keep) (filterVec leftTest left) (filterVec rightTest right)
    in rowLists rows
  where
    select :: (a -> Bool) -> forall i j. Vec a i -> Vec a j -> HiddenRows a
    select p xs ys = packRows (filterVec p xs) (filterVec p ys)

    withRows :: (forall i j. Vec a i -> Vec a j -> r) -> Vec a l -> Vec a k -> r
    withRows use xs ys = use xs ys

-- | Swap both lengths under the case and share the resulting package.
swapRowsLet :: HiddenRows a -> HiddenRows a
swapRowsLet rows =
    let result = case rows of Rows left right -> Rows right left
    in result

-- | Forward the fixed package type through a variable case.
swapRowsCase :: HiddenRows a -> HiddenRows a
swapRowsCase rows =
    case (case rows of Rows left right -> Rows right left) of
        result -> result

-- | Zip equal-length inputs, then map and filter at an erased input length.
refilterNumericZip :: (Int -> Bool) -> Int -> Int -> Int -> Int -> [Int]
refilterNumericZip keep x y u v =
    let kept = withVector (mapFilter keep) (filterVec odd zipped)
    in cells kept
  where
    zipped :: Vec Int ('S ('S 'Z))
    zipped = zipWithExact [x, y] [u, v] (+)

    mapFilter :: (Int -> Bool) -> Vec Int n -> Filtered Int
    mapFilter p xs = filterVec p (increment xs)

    increment :: Vec Int n -> Vec Int n
    increment VNil = VNil
    increment (VCons value rest) = VCons (value + 1) (increment rest)

-- | Open the selected vector and leave the other operand unevaluated.
firstSelected :: Bool -> Filtered a -> Filtered a -> [a]
firstSelected flag left right = take 1 (toList (if flag then id left else right))

-- | Read one cell from the selected filter result.
firstSelectedFilter :: Bool -> (a -> Bool) -> Vec a n -> Vec a m -> [a]
firstSelectedFilter flag keep left right =
    take 1 (toList (if flag then filterVec keep left else filterVec keep right))

-- | Read one element from the package selected by the ordering.
firstByOrdering :: Ordering -> Filtered a -> Filtered a -> Filtered a -> [a]
firstByOrdering order below same above =
    take 1 (toList (case order of
        LT -> below
        EQ -> id same
        GT -> above))

-- | Forward a package through a polymorphic call before reading its vector.
firstForwarded :: Filtered a -> [a]
firstForwarded xs = take 1 (toList (id xs))
