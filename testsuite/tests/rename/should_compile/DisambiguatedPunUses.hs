{-# LANGUAGE DataKinds, ExplicitNamespaces, NoListTuplePuns, PolyKinds #-}

module DisambiguatedPunUses (type Ty.T, data Con.T, typ, term, listSyntax, tupleSyntax, promoted, promotedList) where

import Data.List (List)
import Data.Tuple.Experimental (Tuple2)
import qualified PunUseData as Con (A, data T)
import qualified PunUseType as Ty (type T)

data Witness a = WitnessValue

typ :: Ty.T -> Ty.T
typ x = x

term :: Con.A
term = Con.T

promoted :: Witness Con.T
promoted = WitnessValue

listSyntax :: List Int
listSyntax = []

tupleSyntax :: Tuple2 Int Bool
tupleSyntax = (1, True)

promotedList :: Witness [Bool]
promotedList = WitnessValue
