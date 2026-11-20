{-# LANGUAGE DataKinds, GADTs, NoExperimentalDependentTypes, TypeApplications #-}
{-# OPTIONS_GHC -fno-enable-rewrite-rules #-}

module OrdinaryOpenVectorAppend (observeAppendEmpty, appendContents) where

import Data.Type.Equality ((:~:)(Refl))
import OpenVectorAppend (appendEmpty, appendEmptyAt)
import qualified PackedVecLibrary as P

-- | Use the imported proof and read the head with a suspended tail.
observeAppendEmpty :: (String, Bool)
observeAppendEmpty =
    case appendEmpty @Bool @('P.S 'P.Z) @('P.VCons 'True 'P.VNil) of
      proof@Refl -> (show proof, headOf (P.appendVec P.VNil input))
  where
    input :: P.Vec Bool ('P.S 'P.Z)
    input = P.VCons True (error "forced append suffix tail")

    headOf :: P.Vec Bool ('P.S 'P.Z) -> Bool
    headOf (P.VCons first _) = first

-- | Read the result after checking the proof for this input vector.
appendContents :: P.Vec a n -> [a]
appendContents xs = case appendEmptyAt xs of
    Refl -> P.toList (P.appendVec P.VNil xs)
