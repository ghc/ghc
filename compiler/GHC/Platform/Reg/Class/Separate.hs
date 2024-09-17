{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Register classes for architectures which have registers
-- for scalar floating-point values that are separate from all vector registers.
module GHC.Platform.Reg.Class.Separate
  ( RegClass ( RcInteger, RcFloat, RcVector )
  , pprRegClass, allRegClasses
  )

where

import GHC.Utils.Outputable ( SDoc, text )
import GHC.Platform.Reg.Class ( RegClass(..) )

pattern RcInteger, RcFloat, RcVector :: RegClass
pattern RcInteger = RegClass 0
pattern RcFloat   = RegClass 1
pattern RcVector  = RegClass 2
{-# COMPLETE RcInteger, RcFloat, RcVector #-}

pprRegClass :: RegClass -> SDoc
pprRegClass = \case
  RcInteger -> text "I"
  RcFloat   -> text "F"
  RcVector  -> text "V"

allRegClasses :: [RegClass]
allRegClasses = [RcInteger, RcFloat, RcVector]
