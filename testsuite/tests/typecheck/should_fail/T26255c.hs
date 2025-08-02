{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module T26255c where

import Data.Kind
import Data.Proxy
import GHC.TypeLits

type MinVersion = 1

class
  ( KnownNat (ProtVerLow era)
  , MinVersion <= ProtVerLow era
  ) =>
  Era era
  where
  type ProtVerLow era :: Nat

newtype EraFamily era = EraFamily Int

class Era era => NewEra era where
  eraFamilySize :: EraFamily era -> Int

printEraFamilySize :: EraFamily era -> IO ()
printEraFamilySize = print . eraFamilySize
