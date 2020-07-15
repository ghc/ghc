{-# LANGUAGE ImplicitParams, TypeApplications, ScopedTypeVariables, QuantifiedConstraints, ImpredicativeTypes #-}

module Foo where

import Control.Monad
import GHC.Stack

wcs :: (?callStack :: CallStack) => ( (?callStack :: CallStack) => a ) -> a
wcs x = error "urk"

info :: IO ()
info  = wcs $ (when True $ return ())
