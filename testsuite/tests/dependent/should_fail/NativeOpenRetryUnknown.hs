{-# LANGUAGE DataKinds, ExperimentalDependentTypes, NamedWildCards #-}
{-# LANGUAGE PartialTypeSignatures, ScopedTypeVariables, TypeApplications #-}
module NativeOpenRetryUnknown where
import qualified Data.Char
import Data.Proxy (Proxy(..))
upper :: Proxy _c -> Proxy (Data.Char.toUpper _c)
upper _ = Proxy @'A'
