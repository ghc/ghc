{-# LANGUAGE GADTs, ViewPatterns, PatternSynonyms, DataKinds, Arrows, TypeOperators, TypeFamilies, UndecidableInstances, AllowAmbiguousTypes #-}
module T9985 where

import Control.Arrow

data Nat = Z | S Nat
data Vec n a where
  VNil  :: Vec Z a
  VCons :: a -> Vec n a -> Vec (S n) a
viewVNil :: Vec Z a -> ()
viewVNil VNil = ()
viewVCons :: Vec (S n) a -> (a, Vec n a)
viewVCons (VCons a as) = (a, as)
pattern (:>) :: a -> Vec n a -> Vec (S n) a
pattern a :> as <- (viewVCons -> (a, as))
pattern VNil' <- (viewVNil -> ())

type family n + m where
  n + Z   = n
  n + S m = S (n + m)

type family P2 n where
  P2 Z     = S Z
  P2 (S n) = P2 n + P2 n

class A n where
  a :: Arrow b => b (Vec (P2 n) a) a
instance A Z where
  a = proc (a :> VNil) -> returnA -< a
