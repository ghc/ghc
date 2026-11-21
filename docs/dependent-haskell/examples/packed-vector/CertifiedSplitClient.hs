{-# LANGUAGE DataKinds, GADTs, NoExperimentalDependentTypes, TypeApplications #-}
{-# OPTIONS_GHC -fno-enable-rewrite-rules #-}
module CertifiedSplitClient (splitObservation, checkDemand, checkedObservation, checkBounds, pairedObservation) where

import CertifiedSplit (SplitAtWidth(..), SplitBothAtWidth(..), checkedSplitBoth, checkedSplitAtValue, splitAtValue)
import Control.Exception (ErrorCall, displayException, evaluate, try)
import Data.IORef (newIORef, readIORef)
import Data.Type.Equality ((:~:)(Refl))
import GHC.Sigma (Sigma(Pair))
import qualified PackedVecLibrary as P

-- | Read both pieces and their append after opening the equality.
atTwo :: P.Vec Bool ('P.S ('P.S ('P.S 'P.Z))) -> ([Bool], [Bool], [Bool])
atTwo v = case splitAtValue @Bool @('P.S 'P.Z) (P.S (P.S P.Z)) v of
    Pair left (Pair right Refl) -> (P.toList left, P.toList right, P.toList (P.appendVec left right))

-- | Split at a length obtained at runtime.
atRuntime :: P.Nat -> ([Bool], [Bool])
atRuntime cut =
    let input = P.replicateNative (P.plus cut (P.S P.Z)) True
    in case splitAtValue @Bool @('P.S 'P.Z) cut input of
        Pair left (Pair right Refl) -> (P.toList left, P.toList right)

-- | Demand the zero-cut proof while leaving the original vector lazy.
atZero :: P.Vec Bool ('P.S 'P.Z) -> Bool
atZero v = case splitAtValue @Bool @('P.S 'P.Z) P.Z v of
    Pair _ (Pair _ Refl) -> True

-- | Demand the proof for a one-cell prefix.
atOne :: P.Vec Bool ('P.S ('P.S 'P.Z)) -> Bool
atOne v = case splitAtValue @Bool @('P.S 'P.Z) (P.S P.Z) v of
    Pair _ (Pair _ Refl) -> True

-- | Observe the contents for a written cut and for a cut read from IO.
splitObservation :: IO (([Bool], [Bool], [Bool]), ([Bool], [Bool]))
splitObservation = do
    cut <- newIORef (P.S (P.S P.Z)) >>= readIORef
    input <- newIORef (P.VCons True (P.VCons False (P.VCons True P.VNil))) >>= readIORef
    pure (atTwo input, atRuntime cut)

-- | Check which inputs a caller must evaluate to obtain the equality.
checkDemand :: IO (Bool, Bool, Bool)
checkDemand = do
    forced <- try (evaluate (atOne (error "forced input"))) :: IO (Either ErrorCall Bool)
    pure (atZero (error "forced zero input"), atOne (P.VCons (error "forced head") (error "forced suffix")),
           either ((== "forced input") . takeWhile (/= '\n') . displayException) (const False) forced)

-- | Use both equations after reading independent lengths.
contents :: P.Nat -> P.Nat -> [Bool] -> Maybe (Int, [Bool], [Bool], [Bool])
contents cut total list = do
    input <- P.fromListExact total list
    split <- checkedSplitAtValue cut total input
    case split of
        Pair suffix (SplitAtWidth (Pair left (Pair right Refl))) ->
            Just (P.natToInt suffix, P.toList left, P.toList right, P.toList (P.appendVec left right))

-- | Ask for the length proof while the original vector is bottom.
bound :: P.Nat -> P.Nat -> Maybe Int
bound cut total =
    let missing :: a -> a
        missing _ = error "forced checked input"
        input = missing (P.replicateNative total True)
    in case checkedSplitAtValue cut total input of
        Nothing -> Nothing
        Just (Pair suffix (SplitAtWidth _)) -> Just (P.natToInt suffix)

-- | Read the cut, length, and list independently. Also try an excessive cut.
checkedObservation :: IO [Maybe (Int, [Bool], [Bool], [Bool])]
checkedObservation = do
    one <- newIORef (P.S P.Z) >>= readIORef
    two <- newIORef (P.S (P.S P.Z)) >>= readIORef
    values <- newIORef [True,False] >>= readIORef
    pure [contents one two values, contents (P.S two) two values]

-- | Observe the bounds without evaluating the missing vector in any case.
checkBounds :: [Maybe Int]
checkBounds = [bound P.Z P.Z, bound one two, bound two two, bound (P.S two) two, bound one P.Z]
  where
    one = P.S P.Z
    two = P.S one

-- | Read two vectors of the same length and inspect each reconstruction proof.
pairedContents :: P.Nat -> P.Nat -> [Bool] -> [Bool] -> Maybe (([Bool], [Bool]), ([Bool], [Bool]))
pairedContents cut total first second = do
    xs <- P.fromListExact total first
    ys <- P.fromListExact total second
    split <- checkedSplitBoth cut total xs ys
    case split of
        Pair _ (SplitBothAtWidth (Pair xl (Pair xr Refl)) (Pair yl (Pair yr Refl))) ->
            Just ((P.toList xl, P.toList xr), (P.toList yl, P.toList yr))

-- | Read the first proof without evaluating the second input.
firstPrefix :: P.Nat -> P.Nat -> Maybe [Bool]
firstPrefix cut total =
    let xs = P.replicateNative total True
        missing :: a -> a
        missing _ = error "forced second vector"
        ys = missing xs
    in case checkedSplitBoth cut total xs ys of
        Just (Pair _ (SplitBothAtWidth (Pair prefix (Pair _ Refl)) _)) -> Just (P.toList prefix)
        Nothing -> Nothing

-- | Try one valid cut and one excessive cut from an ordinary module.
pairedObservation :: IO (Maybe (([Bool], [Bool]), ([Bool], [Bool])), Maybe [Bool], Maybe [Bool])
pairedObservation = do
    cut <- newIORef (P.S P.Z) >>= readIORef
    total <- newIORef (P.S (P.S P.Z)) >>= readIORef
    pure (pairedContents cut total [True,False] [False,True],
          firstPrefix cut total, firstPrefix (P.S total) total)
