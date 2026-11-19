{-# LANGUAGE DataKinds, ExperimentalDependentTypes, TypeOperators #-}
module Main where

import Data.Type.Equality ((:~:)(Refl))
import qualified ExistentialVec as E
import qualified IndexedVec as V
import qualified NativeClosedVectorPair as P

cells :: V.two 3 :~: 'V.VCons 3 ('V.VCons 4 'V.VNil)
cells = Refl

first :: V.safeHead (V.two 3) :~: 3
first = Refl

both :: V.readTwo (V.two 3) :~: 34
both = Refl

pair :: P.readPair (P.twoVectors 4) :~: 5
pair = Refl

writtenExistential :: E.readSomeVec ('E.SomeVec ('E.SS ('E.SS 'E.SZ))
                                               ('V.VCons 4 ('V.VCons 5 'V.VNil))) :~: '[4, 5]
writtenExistential = Refl

main :: IO ()
main = case (cells, first, both, pair, writtenExistential) of
  (Refl, Refl, Refl, Refl, Refl) -> print (V.safeHead (V.two 3), V.readTwo (V.two 3))
