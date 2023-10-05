{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
module T19742 where

import Data.Kind

type Id :: Type -> Type
type family Id x where
  Id x = x

type EqSameNat :: Id Type
type EqSameNat = ()

useNatEq :: EqSameNat
useNatEq = ()

decCongS :: ()
decCongS = case useNatEq of () -> ()
