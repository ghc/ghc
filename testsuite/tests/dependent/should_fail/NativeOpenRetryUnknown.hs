{-# LANGUAGE DataKinds, ExperimentalDependentTypes, NamedWildCards #-}
{-# LANGUAGE PartialTypeSignatures, ScopedTypeVariables, TypeApplications #-}
module NativeOpenRetryUnknown where
import qualified RetryCharacters
import Data.Proxy (Proxy(..))
upper :: Proxy _c -> Proxy (RetryCharacters.title _c)
upper _ = Proxy @'A'
