{-# LANGUAGE ExplicitLevelImports #-}
module StagedPunUses where

import PunUseData (A(T))
import qualified PunUseData as Value (A(T))
import splice PunUseType (T)

punned :: A
punned = T

qualifiedUse :: A
qualifiedUse = Value.T
