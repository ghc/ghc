{-# LANGUAGE AllowAmbiguousTypes
           , DataKinds
           , ExplicitForAll
           , KindSignatures
           , OverloadedLabels
           , RebindableSyntax
           , ScopedTypeVariables
           , ImplicitPrelude
  #-}

import GHC.TypeLits (Symbol, KnownSymbol, symbolVal)
import Data.Proxy

foo = #foo
  where
    fromLabel :: forall (x :: Symbol) . ()
    fromLabel = ()

bar = #bar
  where
    fromLabel :: forall (x :: Symbol) . KnownSymbol x => String
    fromLabel = symbolVal (Proxy :: Proxy x)

main = do print foo
          print bar
