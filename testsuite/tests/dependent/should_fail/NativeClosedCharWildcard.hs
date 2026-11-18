{-# LANGUAGE DataKinds, ExperimentalDependentTypes, PolyKinds, ScopedTypeVariables, TypeApplications #-}
module NativeClosedCharWildcard where

import qualified Data.Char
import Data.Proxy

p :: forall (c :: Char). Proxy '( 'X', c)
p = Proxy @'(Data.Char.toUpper 'x', _)
