-----------------------------------------------------------------------------
-- |
-- Module      :  System.Exit
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Exiting the program.
--
-----------------------------------------------------------------------------

module System.Exit
    ( 
      ExitCode(ExitSuccess,ExitFailure)
    , exitWith      -- :: ExitCode -> IO a
    , exitFailure   -- :: IO a
  ) where

import Prelude

#ifdef __GLASGOW_HASKELL__
import GHC.IOBase
#endif

#ifdef __HUGS__
import Hugs.Prelude
import Hugs.Exception
#endif

#ifdef __NHC__
import System
  ( ExitCode(..)
  , exitWith
  )
#endif

-- ---------------------------------------------------------------------------
-- exitWith

-- `exitWith code' terminates the program, returning `code' to the
-- program's caller.  Before it terminates, any open or semi-closed
-- handles are first closed.

#ifndef __NHC__
exitWith :: ExitCode -> IO a
exitWith ExitSuccess = throwIO (ExitException ExitSuccess)
exitWith code@(ExitFailure n)
  | n /= 0 = throwIO (ExitException code)
#ifdef __GLASGOW_HASKELL__
  | otherwise = ioError (IOError Nothing InvalidArgument "exitWith" "ExitFailure 0" Nothing)
#endif
#endif  /* ! __NHC__ */

exitFailure :: IO a
exitFailure = exitWith (ExitFailure 1)
