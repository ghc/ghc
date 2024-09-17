{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Register classes for architectures which don't have separate registers
-- for scalar floating-point values separate from vector registers.
module GHC.Platform.Reg.Class.Unified
  ( RegClass ( RcInteger, RcFloatOrVector )
  , pprRegClass, allRegClasses
  )

where

import GHC.Utils.Outputable ( SDoc, text )
import GHC.Platform.Reg.Class ( RegClass(..) )

pattern RcInteger, RcFloatOrVector :: RegClass
pattern RcInteger       = RegClass 0
pattern RcFloatOrVector = RegClass 1
{-# COMPLETE RcInteger, RcFloatOrVector #-}

pprRegClass :: RegClass -> SDoc
pprRegClass = \case
  RcInteger       -> text "I"
  RcFloatOrVector -> text "F"

allRegClasses :: [RegClass]
allRegClasses = [RcInteger, RcFloatOrVector]
