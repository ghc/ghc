{-# LANGUAGE Haskell2010, Safe #-}
{-# LANGUAGE DataKinds, ExperimentalDependentTypes, NamedWildCards #-}
{-# LANGUAGE PartialTypeSignatures, ScopedTypeVariables, TypeApplications #-}
module NativeOpenRetrySafe where
import qualified RetryCharacters
import Data.Proxy (Proxy(..))
upper :: Proxy _c -> Proxy (RetryCharacters.title _c)
upper (_ :: Proxy 'a') = Proxy @'A'
