{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExperimentalDependentTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
module NativeScopedSumSignatureReifyProvider
  ( provided, localRoundTrip, Cell(..), Stored(..), GStored(..) ) where

import GHC.Sigma (Sigma(Pair))
import Language.Haskell.TH

data Flag (tag :: Bool) = Flag Int

data Cell a (tag :: Bool) = Cell a

newtype Stored a = Stored { stored :: some (tag :: Bool) | Cell a tag }

newtype GStored a where
  GStored :: { gstored :: some (tag :: Bool) | Cell b tag } -> GStored b

provided :: some (tag :: Bool) | Flag tag
provided = Pair True (Flag 24)

$(do VarI _ ty _ <- reify 'provided
     let name = mkName "localRoundTrip"
     pure [SigD name ty, ValD (VarP name) (NormalB (VarE 'provided)) []])
