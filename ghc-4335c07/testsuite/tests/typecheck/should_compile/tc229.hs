{-# OPTIONS_GHC -Wno-redundant-constraints -Wno-simplifiable-class-constraints #-}

-- trac #1406: Constraint doesn't reduce in the presence of quantified
--             type variables

{-# LANGUAGE FlexibleInstances, UndecidableInstances, RankNTypes,
             MultiParamTypeClasses, FunctionalDependencies #-}

module Problem where

data Z
data S a

class HPrefix l
instance (NSub (S Z) ndiff, HDrop ndiff l l) => HPrefix l
-- Weird test case: (NSub (S Z) ndiff) is simplifiable

class NSub n1 n3 | n1 -> n3
instance NSub Z Z
instance NSub n1 n3 => NSub (S n1) n3

class HDrop n l1 l2 | n l1 -> l2
instance HDrop Z l l

t_hPrefix :: HPrefix l => l -> ()
-- Weird test case: (HPrefix l) is simplifiable
t_hPrefix = undefined

-- In ghc 6.6.1 this works...
thr' :: (forall r. l -> a) -> a
thr' f = f undefined
thP4' = thr' t_hPrefix

-- ... but this doesn't work...?
thr :: (forall r. r -> a) -> a
thr f = f undefined
thP4 = thr t_hPrefix

