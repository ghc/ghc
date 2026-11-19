{-# LANGUAGE DataKinds, ExperimentalDependentTypes, TypeOperators #-}
module Main where

import Data.Type.Equality ((:~:)(Refl))
import qualified ExistentialVec as E
import qualified IndexedVec as V
import qualified NativeClosedIndexedRoute as R
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

empty :: E.someVec 0 :~: 'E.SomeVec 'E.SZ 'V.VNil
empty = Refl

one :: E.someVec 3 :~: 'E.SomeVec ('E.SS 'E.SZ) ('V.VCons 3 'V.VNil)
one = Refl

two :: E.someVec 4 :~: 'E.SomeVec ('E.SS ('E.SS 'E.SZ)) ('V.VCons 4 ('V.VCons 5 'V.VNil))
two = Refl

computed :: E.readSomeVec (E.someVec 4) :~: '[4, 5]
computed = Refl

-- The recursive fare is the first field, and this index has two base cases.
fare :: R.ticket 4 :~: 'R.Ticket ('R.OnWay 'R.AtHome) ('R.Change ('R.Paid 4) 5)
fare = Refl

main :: IO ()
main = case (cells, first, both, pair, writtenExistential, empty, one, two, computed, fare) of
  (Refl, Refl, Refl, Refl, Refl, Refl, Refl, Refl, Refl, Refl) ->
    print (V.safeHead (V.two 3), V.readTwo (V.two 3))
