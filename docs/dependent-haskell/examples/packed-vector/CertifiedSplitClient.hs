{-# LANGUAGE DataKinds, GADTs, NoExperimentalDependentTypes, TypeApplications #-}
{-# OPTIONS_GHC -fno-enable-rewrite-rules #-}
module CertifiedSplitClient (splitObservation, checkDemand) where

import CertifiedSplit (splitAtValue)
import Control.Exception (ErrorCall, displayException, evaluate, try)
import Data.IORef (newIORef, readIORef)
import Data.Type.Equality ((:~~:)(HRefl))
import GHC.Sigma (Sigma(Pair))
import qualified PackedVecLibrary as P

-- | Read both pieces and their append after opening the equality.
atTwo :: P.Vec Bool ('P.S ('P.S ('P.S 'P.Z))) -> ([Bool], [Bool], [Bool])
atTwo v = case splitAtValue @Bool @('P.S 'P.Z) (P.S (P.S P.Z)) v of
    Pair left (Pair right HRefl) -> (P.toList left, P.toList right, P.toList (P.appendVec left right))

-- | Split at a length obtained at runtime.
atRuntime :: P.Nat -> ([Bool], [Bool])
atRuntime cut =
    let input = P.replicateNative (P.plus cut (P.S P.Z)) True
    in case splitAtValue @Bool @('P.S 'P.Z) cut input of
        Pair left (Pair right HRefl) -> (P.toList left, P.toList right)

-- | Demand the zero-cut proof while leaving the original vector lazy.
atZero :: P.Vec Bool ('P.S 'P.Z) -> Bool
atZero v = case splitAtValue @Bool @('P.S 'P.Z) P.Z v of
    Pair _ (Pair _ HRefl) -> True

-- | Demand the proof for a one-cell prefix.
atOne :: P.Vec Bool ('P.S ('P.S 'P.Z)) -> Bool
atOne v = case splitAtValue @Bool @('P.S 'P.Z) (P.S P.Z) v of
    Pair _ (Pair _ HRefl) -> True

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
