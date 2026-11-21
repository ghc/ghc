{-# LANGUAGE DataKinds, PolyKinds #-}

module PunUses (T, typ, term, qualifiedTerm, patternUse, promoted, tupleSyntax, listSyntax) where

import PunUseData
import PunUseType
import qualified PunUseData as Both
import qualified PunUseType as Both

data Witness a = WitnessValue

typ :: T
typ = X

term :: A
term = T

qualifiedTerm :: A
qualifiedTerm = Both.T

patternUse :: A -> Bool
patternUse T = True

promoted :: Witness 'T
promoted = WitnessValue

tupleSyntax :: (a, b) -> ()
tupleSyntax _ = ()

listSyntax :: [] a -> [a]
listSyntax xs = xs
