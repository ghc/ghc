{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
module T10507 where

import Data.Type.Equality ( (:~:)(Refl) )
import Prelude (Maybe(..), undefined)
import GHC.TypeLits ( Nat, type (<=))

data V (n::Nat)

testEq :: (m <= n) => V m -> V n -> Maybe (m :~: n)
testEq = undefined


uext :: (1 <= m, m <= n) => V m -> V n -> V n
uext e w =
  case testEq e w of
    Just Refl -> e
