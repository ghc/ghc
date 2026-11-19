{-# LANGUAGE BangPatterns, DataKinds, ExperimentalDependentTypes, LinearTypes #-}
{-# LANGUAGE NamedWildCards, PartialTypeSignatures, TypeApplications #-}
module NativeOpenRetryNoGen where
import GHC.Types (Multiplicity(Many))
import qualified Data.Char
import Data.Proxy (Proxy(..))

pair :: (Proxy 'A', Bool)
pair =
  let x :: Proxy (Data.Char.toTitle _c)
      %Many !(x, y) = (Proxy @'A', True)
  in (x, y)
