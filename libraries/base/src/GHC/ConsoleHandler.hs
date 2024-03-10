{-# LANGUAGE CPP #-}
{-# LANGUAGE Safe #-}

-- |
-- Module      :  GHC.ConsoleHandler
-- Copyright   :  (c) The University of Glasgow
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  ghc-devs@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC extensions)
--
-- NB. the contents of this module are only available on Windows.
--
-- Installing Win32 console handlers.
--

#if !defined(mingw32_HOST_OS)

module GHC.ConsoleHandler () where

-- See W1 of Note [Tracking dependencies on primitives] in GHC.Internal.Base
import Prelude () -- for build ordering

#else

module GHC.ConsoleHandler
        ( Handler(..)
        , installHandler
        , ConsoleEvent(..)
        ) where

import GHC.Internal.ConsoleHandler

#endif

