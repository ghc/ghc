{-# LANGUAGE DataKinds, NoExperimentalDependentTypes, TypeApplications #-}
module WrongGroundPair where

import Data.Proxy (Proxy(..))
import PreludePairExport (first)

wrong = first (Proxy @'( 'True, 'x')) (Proxy @'False)
