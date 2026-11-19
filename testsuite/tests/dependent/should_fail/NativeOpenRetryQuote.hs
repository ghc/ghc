{-# LANGUAGE DataKinds, ExperimentalDependentTypes, NamedWildCards #-}
{-# LANGUAGE PartialTypeSignatures, ScopedTypeVariables, TemplateHaskell, TypeApplications #-}
module NativeOpenRetryQuote where
import qualified Data.Char
import Data.Proxy (Proxy(..))
import Language.Haskell.TH.Syntax (Code, Q)
quoted :: Code Q (Proxy 'A')
quoted = [|| let
  upper :: Proxy _c -> Proxy (Data.Char.toTitle _c)
  upper (_ :: Proxy 'a') = Proxy @'A'
  in upper (Proxy @'a') ||]
