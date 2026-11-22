{-# LANGUAGE TypeAbstractions #-}
module TypePatternPunUses where

import Data.List (List)
import Data.Tuple.Experimental (Tuple2)
import PunUseData
import qualified PunUseType as Q
import PunUseType (T)

data P a = Witness

listPat :: P (List a) -> Bool
listPat (Witness @([b])) = True

emptyListTyConPat :: P (List a) -> Bool
emptyListTyConPat (Witness @([] b)) = True

tuplePat :: P (Tuple2 a b) -> Bool
tuplePat (Witness @((s, t))) = True

typePat :: P Q.T -> Bool
typePat (Witness @T) = True

explicitPat :: P Q.T -> Bool
explicitPat (Witness @Q.T) = True

namedListPat :: P (List a) -> Bool
namedListPat (Witness @(List b)) = True
