{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module T26255b where

import Data.Proxy
import GHC.TypeLits

type MinVersion = 1

class
  ( KnownNat (ProtVerLow era)
  , MinVersion <= ProtVerLow era
  , KnownSymbol (EraName era)
  ) =>
  Era era
  where
  type EraName era = (r :: Symbol) | r -> era

  type ProtVerLow era :: Nat

  eraName :: Proxy era -> String
  eraName _ = symbolVal (Proxy :: Proxy (EraName era))

data FooEra

instance Era FooEra where
  type EraName FooEra = "Foo"
  type ProtVerLow FooEra = 1

data BarEra

instance Era BarEra where
  type EraName BarEra = "Bar"
  type ProtVerLow BarEra = 2

fromEraName :: (Era era, EraName era ~ name) => Proxy (name :: Symbol) -> Proxy era
fromEraName _ = Proxy

noCompileErrorMessage :: IO ()
noCompileErrorMessage = putStrLn $ eraName $ fromEraName (Proxy :: Proxy "Bar")

brokenCompileErrorMessage2 :: IO ()
brokenCompileErrorMessage2 = putStrLn $ eraName $ head $ fromEraName (Proxy :: Proxy "Bar")
