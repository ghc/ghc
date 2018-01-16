{-# LANGUAGE CPP #-}
{-# LANGUAGE Safe #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  System.Posix.Signals.Exts
-- Copyright   :  (c) The University of Glasgow 2002
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  non-portable (requires POSIX, includes Linuxisms/BSDisms)
--
-- non-POSIX signal support commonly available
--
-----------------------------------------------------------------------------

#include "HsUnixConfig.h"
##include "HsUnixConfig.h"

#ifdef HAVE_SIGNAL_H
#include <signal.h>
#endif

module System.Posix.Signals.Exts (
  module System.Posix.Signals
  , sigINFO
  , sigWINCH
  , infoEvent
  , windowChange
  ) where

import Foreign.C
import System.Posix.Signals

sigINFO   :: CInt
sigINFO   = CONST_SIGINFO

sigWINCH   :: CInt
sigWINCH   = CONST_SIGWINCH


infoEvent :: Signal
infoEvent = sigINFO

windowChange :: Signal
windowChange = sigWINCH
