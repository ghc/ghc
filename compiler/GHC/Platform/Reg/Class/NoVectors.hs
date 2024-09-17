{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Register classes for architectures which don't have any vector registers.
module GHC.Platform.Reg.Class.NoVectors
  ( RegClass ( RcInteger, RcFloat )
  , pprRegClass, allRegClasses
  )

where

import GHC.Utils.Outputable ( SDoc, text )
import GHC.Platform.Reg.Class ( RegClass(..) )

pattern RcInteger, RcFloat :: RegClass
pattern RcInteger = RegClass 0
pattern RcFloat   = RegClass 1
{-# COMPLETE RcInteger, RcFloat #-}

pprRegClass :: RegClass -> SDoc
pprRegClass = \case
  RcInteger -> text "I"
  RcFloat   -> text "F"

allRegClasses :: [RegClass]
allRegClasses = [RcInteger, RcFloat]
