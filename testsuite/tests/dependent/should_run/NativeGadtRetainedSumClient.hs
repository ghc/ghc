{-# LANGUAGE DataKinds, GADTs, NoExperimentalDependentTypes #-}
module Main (main) where

import Control.Exception (ErrorCall, displayException, evaluate, try)
import Control.Monad (unless)
import Data.IORef (newIORef, readIORef)
import Data.List (isPrefixOf)
import Data.Maybe (isJust)
import GHC.Sigma (Sigma(Pair))
import NativeGadtRetainedSumProvider

paired :: Nested a -> [(a, a)]
paired (Nested (Pair _ (xs, Pair _ (ys, _)))) = cells (zipVec xs ys)

outerCells :: Nested a -> [a]
outerCells (Nested (Pair _ (xs, _))) = cells xs

isEmptyPack :: Packed Bool -> Bool
isEmptyPack (Pair _ xs) = isJust (emptyWitness xs)

rebuildDirect :: DirectVec n -> DirectVec n
rebuildDirect (DirectVec n xs) = DirectVec n xs

openDirect :: Packed Bool -> [Bool]
openDirect (Pair n xs) =
    let wrapped = DirectVec n xs
    in case rebuildDirect wrapped of DirectVec _ ys -> cells ys

phantomPayload :: Phantom -> Bool
phantomPayload (Pair _ xs) = sameLength xs empty
  where
    empty :: Vec Bool 'Z
    empty = VNil

require :: Bool -> String -> IO ()
require ok label = unless ok (fail label)

throws :: String -> a -> IO ()
throws label value = do
    result <- try @ErrorCall (evaluate value)
    case result of
        Left err -> require (label `isPrefixOf` displayException err) (displayException err)
        Right _ -> fail ("Expected " ++ label)

main :: IO ()
main = do
    let one = VCons True VNil
        other = VCons False VNil
        value = makeNested (S Z) (S (S Z)) one other (VCons False one)
        hidden = makeHidden (S (S Z)) one other (VCons False one)
        aligned = [(True, False)]
    require (isEmptyPack (pack Z VNil)) "inferred empty payload index"
    require (not (isEmptyPack (pack (S Z) (VCons True (error "unused tail")))))
        "inferred nonempty payload index without demanding its tail"
    require (phantomPayload phantom) "the reduced payload determines the index"
    require (take 1 (openDirect (pack (S Z) (VCons True (error "direct tail")))) == [True])
        "a direct result keeps the index opened by the dependent sum"
    n <- newIORef (S Z) >>= readIORef
    unused <- newIORef (error "direct length" :: Nat) >>= readIORef
    let vector = DirectVec n (error "direct vector")
        atLength = DirectAtLength unused (AtLength 37)
    require ((case rebuildDirect vector of DirectVec len _ -> lengthValue len) == 1)
        "a direct result leaves the vector unevaluated"
    require ((case atLength of DirectAtLength _ (AtLength value) -> value) == 37)
        "a direct result leaves the length unevaluated"
    throws "direct vector" (case rebuildDirect vector of DirectVec _ xs -> cells xs)
    throws "direct length" (case atLength of DirectAtLength len _ -> lengthValue len)
    require (storedLengths value == (1, 2)) "stored lengths"
    require (innerCells value == [False, True]) "inner cells"
    require (paired value == aligned) "shared outer index"
    require (withHidden hidden (\_ xs ys _ -> cells (zipVec xs ys)) == aligned)
        "constructor-local erased index"
    require (innerCells outerWitnessBottom == [True]) "lazy outer witness and vectors"
    require (paired innerWitnessBottom == aligned) "lazy inner witness and vector"
    require (outerCells innerPairBottom == [True]) "lazy inner pair"
    throws "outer witness" (fst (storedLengths outerWitnessBottom))
    throws "inner witness" (snd (storedLengths innerWitnessBottom))
    throws "second vector" (innerCells innerWitnessBottom)
    throws "inner pair" (innerCells innerPairBottom)
    require (withLater (makeLater (S (S Z)) (S Z) (VCons False one) (VCons 'x' VNil))
        (\n m xs ys -> (lengthValue n, lengthValue m, cells xs, cells ys)) == (2, 1, [False, True], "x"))
        "earlier retained constructor argument"
    require (withLater laterWitnessBottom (\_ _ _ ys -> cells ys) == "x") "lazy earlier constructor argument"
    throws "earlier witness" (withLater laterWitnessBottom (\n _ _ _ -> lengthValue n))
    case fieldScope of
        FieldScope n produce (Pair _ (outer, _)) ->
            case produce (S Z) True one of
                Pair m (_, _, inner, _) ->
                    require ((lengthValue n, lengthValue m, cells outer, cells inner)
                        == (2, 0, [False, True], [True])) "field-local retained length"
