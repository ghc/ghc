{-# LANGUAGE DataKinds, DeriveFunctor, FlexibleInstances, GADTs, KindSignatures, StandaloneDeriving #-}
module T8678 where

data {- kind -} Nat = Z | S Nat

-- GADT in parameter other than the last
data NonStandard :: Nat -> * -> * -> * where
    Standard :: a -> NonStandard (S n) a b
    Non :: NonStandard n a b -> b -> NonStandard (S n) a b

deriving instance (Show a, Show b) => Show (NonStandard n a b)
deriving instance Functor (NonStandard n a)
