{-# LANGUAGE TypeFamilies, KindSignatures, AllowAmbiguousTypes #-}

module T27878b where

type family IsJust (m :: Maybe k) :: Bool where
  IsJust ('Just x) = 'True
  IsJust 'Nothing  = 'False

f :: forall k (m :: Maybe k). (IsJust m ~ 'True) => Int -> Int
f = error "urk"

g _ = f 3   -- Triggered checkValidSubst error in `instantiateFunDepEqns`o
