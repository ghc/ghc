{-# LANGUAGE NoExperimentalDependentTypes #-}
{-# LANGUAGE TemplateHaskell #-}
module NativeScopedSumSignatureReifyClient where

import Language.Haskell.TH
import NativeScopedSumSignatureReifyProvider (localRoundTrip, provided)

$(do VarI _ ty _ <- reify 'provided
     let name = mkName "importRoundTrip"
     pure [SigD name ty, ValD (VarP name) (NormalB (VarE 'localRoundTrip)) []])
