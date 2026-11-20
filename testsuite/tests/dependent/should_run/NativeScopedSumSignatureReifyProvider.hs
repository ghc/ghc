{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExperimentalDependentTypes #-}
{-# LANGUAGE TemplateHaskell #-}
module NativeScopedSumSignatureReifyProvider (provided, localRoundTrip) where

import GHC.Sigma (Sigma(Pair))
import Language.Haskell.TH

data Flag (tag :: Bool) = Flag Int

provided :: some (tag :: Bool) | Flag tag
provided = Pair True (Flag 24)

$(do VarI _ ty _ <- reify 'provided
     let name = mkName "localRoundTrip"
     pure [SigD name ty, ValD (VarP name) (NormalB (VarE 'provided)) []])
