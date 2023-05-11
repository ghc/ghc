{-# LANGUAGE DataKinds, UnliftedNewtypes, TypeFamilies #-}

module T23308 where

import Data.Proxy
import GHC.Exts

-- Check that we don't panic in the middle of typechecking
-- when there is an invalid newtype in a knot-tied group of TyCons.

data A1 = A1 !B1
newtype B1 = B1 C1 C1
data C1 = C1 A1


data A2 = A2 !B2
newtype B2 where { B2 :: forall (x :: C2). Proxy x -> B2 }
data C2 = C2 A2

type F2' :: forall {k}. k -> TYPE WordRep
type family F2' a where {}
data A2' = A2' !B2'
newtype B2' where { B2' :: forall (x :: C2'). F2' x -> B2' }
data C2' = C2' A2'


data A3 = A3 !B3
newtype B3 where { B3 :: forall (x :: C2). B2 }
data C3 = C3 A3


data A4 = A4 !(B4 Int)
newtype B4 a where { B4 :: C4 -> B4 Int }
data C4 = C4 A4


data A5 = A5 !(B5 Int)
newtype B5 a where { B5 :: Num a => B5 (a, a) }
data C5 = C5 A5
