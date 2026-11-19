{-# LANGUAGE DataKinds, ExperimentalDependentTypes, NamedWildCards #-}
{-# LANGUAGE PartialTypeSignatures, ScopedTypeVariables, TypeApplications #-}
module NativeOpenRetryWrong where
import qualified Data.Char
import Data.Proxy (Proxy(..))
upper :: Proxy _c -> Proxy (Data.Char.toTitle _c)
upper (_ :: Proxy 'a') = Proxy @'Z'
