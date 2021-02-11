{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedLabels       #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE RebindableSyntax       #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}

module Labels where

-- base
import Prelude
import Data.Kind
  ( Type )
import GHC.TypeLits
  ( Symbol, KnownSymbol )

--------------------------------------------------------------------------

data Label (k :: Symbol) (a :: Type) = Label

class IsLabel k a v | v -> a, v -> k where
  fromLabel :: v

--  fromLabel :: forall {k1} {k2} (k3 :: k1) (a :: k2) v.
--               IsLabel {k1} {k2} k3 a v => v

instance KnownSymbol k => IsLabel k a (Label k a) where
  fromLabel = Label @k @a

foo :: Label k a -> ()
foo _ = ()

test :: ()
test = foo (#label @Bool)

