{-# LANGUAGE Safe #-}
{-# OPTIONS_HADDOCK not-home #-}

-- |
--
-- Module      :  GHC.TopHandler
-- Copyright   :  (c) The University of Glasgow, 2001-2002
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  ghc-devs@haskell.org
-- Stability   :  deprecated (<https://github.com/haskell/core-libraries-committee/issues/393>)
-- Portability :  non-portable (GHC Extensions)
--
-- Support for catching exceptions raised during top-level computations
-- (e.g. @Main.main@, 'Control.Concurrent.forkIO', and foreign exports)
--
-- /The API of this module is unstable and not meant to be consumed by the general public./
-- If you absolutely must depend on it, make sure to use a tight upper
-- bound, e.g., @base < 4.X@ rather than @base < 5@, because the interface can
-- change rapidly without much warning.
--

#if __GLASGOW_HASKELL__ >= 1002
#error "GHC.TopHandler should be removed in GHC 10.02."
#endif

module GHC.TopHandler
  {-# DEPRECATED ["GHC.TopHandler is deprecated and will be removed in GHC 10.02. Please use GHC.Conc where possible and the ghc-internal package otherwise."] #-}
    (runMainIO,
     runIO,
     runIOFastExit,
     runNonIO,
     topHandler,
     topHandlerFastExit,
     reportStackOverflow,
     reportError,
     flushStdHandles
     ) where

import GHC.Internal.TopHandler
