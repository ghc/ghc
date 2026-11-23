{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, NamedWildCards #-}
{-# LANGUAGE PartialTypeSignatures, TypeApplications #-}
module NativeOpenRetryGiven where

import qualified RetryCharacters
import Data.Proxy (Proxy(..))

data Tag (c :: Char) where
  OnlyA :: Tag 'a'
  Other :: Tag c

upper :: Tag _c -> Proxy (RetryCharacters.title _c)
upper OnlyA = Proxy @'A'
upper Other = Proxy @'A'
