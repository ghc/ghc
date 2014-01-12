{-# LANGUAGE GADTs, RankNTypes, TypeFamilies, FlexibleContexts, ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}
module Absurd where

data Z = Z
newtype S n = S n
class Nat n where
   caseNat :: (n ~ Z => r) -> (forall p. (n ~ S p, Nat p) => p -> r) -> n -> r
instance Nat Z where
   caseNat = error "urk1"
instance Nat n => Nat (S n) where
   caseNat = error "urk2"

-- empty type
newtype Naught = Naught (forall a. a)
-- types are equal!
data TEq a b where
   TEq :: (a ~ b) => TEq a b

type family NatEqProves m n
type instance NatEqProves (S m) (S n) = TEq m n

noConf :: (Nat m, Nat n) => m -> TEq m n -> NatEqProves m n
noConf = undefined
predEq :: TEq (S a) (S b) -> TEq a b
predEq = undefined

data IsEq a b = Yes (TEq a b) | No (TEq a b -> Naught)

natEqDec :: forall m n. (Nat m, Nat n) => m -> n -> IsEq m n
natEqDec m n = caseNat undefined mIsS m where
    mIsS :: forall pm. (m ~ S pm, Nat pm) => pm -> IsEq m n
    mIsS pm = caseNat undefined nIsS n where
        nIsS :: forall pn. (n ~ S pn, Nat pn) => pn -> IsEq m n
        nIsS pn = case natEqDec pm pn of
            Yes TEq -> Yes TEq
            No contr -> No (contr . noConf m)
--            No contr -> No (contr . predEq)

-- strange things:
-- (1) commenting out the "Yes" case or changing it to "undefined" makes compilation succeed
-- (2) replacing the "No" line with with the commented out "No" line makes compilation succeed