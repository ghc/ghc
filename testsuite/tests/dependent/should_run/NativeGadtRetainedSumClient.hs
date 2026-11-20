{-# LANGUAGE GADTs, NoExperimentalDependentTypes #-}
module Main (main) where

import Control.Exception (ErrorCall, displayException, evaluate, try)
import Control.Monad (unless)
import Data.List (isPrefixOf)
import GHC.Sigma (Sigma(Pair))
import NativeGadtRetainedSumProvider

paired :: Nested a -> [(a, a)]
paired (Nested (Pair _ (xs, Pair _ (ys, _)))) = cells (zipVec xs ys)

outerCells :: Nested a -> [a]
outerCells (Nested (Pair _ (xs, _))) = cells xs

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
