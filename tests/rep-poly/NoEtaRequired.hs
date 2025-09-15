{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE DataKinds, TypeFamilies, PolyKinds, StandaloneKindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module NoEtaRequired where

import Data.Proxy
import Data.Type.Equality ( (:~~:)(..) )
import GHC.Exts ( TYPE, RuntimeRep(..) )

type Id :: k -> k
type family Id a where

type T :: TYPE r -> TYPE (Id r)
type family T a where

test :: forall r (a :: TYPE r). a :~~: T a -> ()
test HRefl =
  let
    f :: (a -> a) -> ()
    f _ = ()
    g :: T a -> T a
    g = undefined
  in f g
-- This test makes sure we DO NOT eta-expand 'g' to '\ x -> g x' when trying
-- to make 'f g' typecheck. We CANNOT eta-expand here, as the binder 'x' would
-- not have a fixed runtime representation.

