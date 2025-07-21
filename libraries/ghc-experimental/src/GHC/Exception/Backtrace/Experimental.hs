{-
Module      :  GHC.Exception.Backtrace.Experimental
Copyright   :  (c) The GHC Team
License     :  see libraries/ghc-experimental/LICENSE

Maintainer  :  ghc-devs@haskell.org
Stability   :  experimental
Portability :  non-portable (GHC extensions)

This module exposes experimental extensions to the Backtrace mechanism of GHC.
-}
module GHC.Exception.Backtrace.Experimental (
  -- * Collecting exception annotations (like backtraces)
  CollectExceptionAnnotationMechanism,
  getCollectExceptionAnnotationMechanism,
  setCollectExceptionAnnotation,
  collectExceptionAnnotation,
  ) where

import GHC.Internal.Exception.Backtrace
