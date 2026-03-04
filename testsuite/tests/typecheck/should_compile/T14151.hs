{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
module T14151 where

newtype HFix h a = HFix (h (HFix h) a)

class EqForall f where
  eqForall :: f a -> f a -> Bool

class EqHetero h where
  eqHetero :: (forall x. f x -> f x -> Bool) -> h f a -> h f a -> Bool

instance EqHetero h => EqForall (HFix h) where
  eqForall (HFix a) (HFix b) = eqHetero eqForall a b 

instance EqHetero h => Eq (HFix h a) where
  (==) = eqForall
