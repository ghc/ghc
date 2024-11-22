{-# LANGUAGE Trustworthy #-}

{-
Module      :  Data.String.Experimental
Copyright   :  (c) The GHC Team
License     :  see libraries/ghc-experimental/LICENSE

Maintainer  :  ghc-devs@haskell.org
Stability   :  experimental
Portability :  non-portable (GHC extensions)

This module exports experimental features that might live in Data.String one day.
-}
module Data.String.Experimental (
  module X,
) where

import Data.String.Interpolate.Class.Experimental as X
import Data.String.Interpolate.Default.Experimental as X
