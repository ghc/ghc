{-# LANGUAGE BangPatterns, DataKinds, ExperimentalDependentTypes, LinearTypes #-}
{-# LANGUAGE NamedWildCards, PartialTypeSignatures, TypeApplications #-}
module NativeOpenRetryNoGen where
import GHC.Types (Multiplicity(Many))
import qualified RetryCharacters
import Data.Proxy (Proxy(..))

pair :: (Proxy 'A', Bool)
pair =
  let x :: Proxy (RetryCharacters.title _c)
      %Many !(x, y) = (Proxy @'A', True)
  in (x, y)
