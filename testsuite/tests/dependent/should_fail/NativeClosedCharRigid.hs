{-# LANGUAGE DataKinds, ExperimentalDependentTypes, KindSignatures, ScopedTypeVariables #-}
module NativeClosedCharRigid where

import qualified Data.Char
import Data.Proxy (Proxy)

rigid :: forall (c :: Char). Proxy c -> Proxy (Data.Char.toUpper c)
rigid = undefined
