{-# LANGUAGE DataKinds, ExperimentalDependentTypes, NamedWildCards #-}
{-# LANGUAGE PartialTypeSignatures, ScopedTypeVariables, TypeApplications #-}
module NativeOpenRetryInterfaceProducer (upper) where

import qualified RetryCharacters
import Data.Proxy (Proxy(..))

upper :: Proxy _c -> Proxy (RetryCharacters.title _c)
upper (_ :: Proxy 'a') = Proxy @'A'
