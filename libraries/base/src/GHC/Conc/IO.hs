{-# OPTIONS_HADDOCK not-home #-}

-- |
--
-- Module      :  GHC.Conc.IO
-- Copyright   :  (c) The University of Glasgow, 1994-2002
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  ghc-devs@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC extensions)
--
-- Basic concurrency stuff.
--
-- /The API of this module is unstable and not meant to be consumed by the general public./
-- If you absolutely must depend on it, make sure to use a tight upper
-- bound, e.g., @base < 4.X@ rather than @base < 5@, because the interface can
-- change rapidly without much warning.
--

module GHC.Conc.IO
    (ensureIOManagerIsRunning,
     ioManagerCapabilitiesChanged,
     interruptIOManager,
     -- *  Waiting
     threadDelay,
     registerDelay,
     threadWaitRead,
     threadWaitWrite,
     threadWaitReadSTM,
     threadWaitWriteSTM,
     closeFdWith
     ) where

import GHC.Internal.Conc.IO
