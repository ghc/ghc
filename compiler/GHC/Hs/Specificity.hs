{-# OPTIONS_GHC -Wno-orphans #-}
module GHC.Hs.Specificity where

import Prelude
import Control.DeepSeq (NFData(..))

import GHC.Utils.Outputable
import GHC.Utils.Binary

import Language.Haskell.Syntax.Specificity

{- *********************************************************************
*                                                                      *
*                   ForAllTyFlag
*                                                                      *
********************************************************************* -}

instance Outputable ForAllTyFlag where
  ppr Required  = text "[req]"
  ppr Specified = text "[spec]"
  ppr Inferred  = text "[infrd]"

instance Binary Specificity where
  put_ bh SpecifiedSpec = putByte bh 0
  put_ bh InferredSpec  = putByte bh 1

  get bh = do
    h <- getByte bh
    case h of
      0 -> return SpecifiedSpec
      _ -> return InferredSpec

instance Binary ForAllTyFlag where
  put_ bh Required  = putByte bh 0
  put_ bh Specified = putByte bh 1
  put_ bh Inferred  = putByte bh 2

  get bh = do
    h <- getByte bh
    case h of
      0 -> return Required
      1 -> return Specified
      _ -> return Inferred

instance NFData Specificity where
  rnf SpecifiedSpec = ()
  rnf InferredSpec = ()
instance NFData ForAllTyFlag where
  rnf (Invisible spec) = rnf spec
  rnf Required = ()

