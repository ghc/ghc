{-# LANGUAGE DataKinds, ExperimentalDependentTypes, PolyKinds, ScopedTypeVariables, TypeApplications #-}
module NativeClosedCharWildcard where

import qualified Data.Char
import Data.Proxy

p :: forall (c :: Char). Proxy '( 'True, c)
p = Proxy @'(Data.Char.isDigit '7', _)
