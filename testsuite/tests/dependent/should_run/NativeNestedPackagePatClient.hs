{-# LANGUAGE DataKinds, GADTs, NoExperimentalDependentTypes, TemplateHaskell, TypeApplications #-}
module Main where

import Control.Exception (ErrorCall, displayException, evaluate, try)
import Control.Monad (unless)
import Data.IORef (newIORef, readIORef)
import Data.List (isInfixOf)
import GHC.Sigma (Sigma(Pair))
import qualified Language.Haskell.TH as TH
import NativeGadtRetainedSumProvider
import NativeNestedPackagePatProvider
import qualified PackedVecLibrary as Packed

headCell :: Vec a ('S m) -> a
headCell (VCons x _) = x

first :: NestedResult a -> Maybe a
first (NestedResult (Pair _ (_, Pair _ (_, _, v@(VCons _ _))))) = Just (headCell v)
first (NestedResult (Pair _ (_, Pair _ (_, _, VNil)))) = Nothing

control :: NestedResult a -> Maybe a
control (NestedResult (Pair _ (_, Pair _ (_, _, result)))) = case result of
    VCons x _ -> Just x
    VNil -> Nothing

guarded :: Bool -> Inputs a -> Maybe a
guarded enabled (Inputs (NestedResult (Pair _ (_, Pair _ (_, _, VCons x _)))) other)
    | enabled, VCons _ _ <- other = Just x
guarded _ _ = Nothing

guardControl :: Bool -> Inputs a -> Maybe a
guardControl enabled (Inputs (NestedResult (Pair _ (_, Pair _ (_, _, result)))) other) =
    case result of
        VCons x _ | enabled -> case other of
            VCons _ _ -> Just x
            VNil -> Nothing
        _ -> Nothing

quoted :: TH.Code TH.Q (NestedResult Bool -> Maybe Bool)
quoted = [|| \input -> case input of
    NestedResult (Pair _ (_, Pair _ (_, _, VCons x _))) -> Just x
    NestedResult (Pair _ (_, Pair _ (_, _, VNil))) -> Nothing ||]

checkDemand :: String -> a -> IO ()
checkDemand message value = do
    result <- try @ErrorCall (evaluate value)
    case result of
        Left err ->
            unless (takeWhile (/= '\n') (displayException err) == message) $
                fail (displayException err)
        Right _ -> fail ("expected " ++ message)

-- | Read the padding length and the vector from the nested package.
observePadding :: PaddedVector ('Packed.S 'Packed.Z) -> (Int, String)
observePadding (Pair padding (Pair _ vector)) = (Packed.natToInt padding, Packed.toList vector)

main :: IO ()
main = do
    padding <- newIORef (Packed.S (Packed.S Packed.Z)) >>= readIORef
    let padded = observePadding (padVector padding)
    unless (padded == (2, "x..")) (fail (show padded))
    let values = map seal [outerWitnessBottom, emptyInput]
    print (map first values, map control values)
    checkDemand "second vector" (first (seal innerWitnessBottom))
    checkDemand "second vector" (control (seal innerWitnessBottom))
    checkDemand "inner pair" (first (seal innerPairBottom))
    checkDemand "inner pair" (control (seal innerPairBottom))
    let bottomOther = sealInputs outerWitnessBottom
        guardInputs = [(False, bottomOther), (True, sealInputs emptyInput), (True, sealInputs fullInput)]
    print (map (uncurry guarded) guardInputs, map (uncurry guardControl) guardInputs)
    checkDemand "first vector" (guarded True bottomOther)
    checkDemand "first vector" (guardControl True bottomOther)
    putStrLn "selected vector, inner pair and guard package forced"
    ast <- TH.runQ (TH.unTypeCode quoted)
    unless (all (`isInfixOf` TH.pprint ast) ["VCons", "VNil"]) $
        fail "missing payload constructors from the typed quotation"
    putStrLn "typed quotation preserved the payload constructors"
