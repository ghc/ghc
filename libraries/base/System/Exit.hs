-----------------------------------------------------------------------------
-- |
-- Module      :  System.Exit
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/core/LICENSE)
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

-- ---------------------------------------------------------------------------
-- exitWith

-- `exitWith code' terminates the program, returning `code' to the
-- program's caller.  Before it terminates, any open or semi-closed
-- handles are first closed.

exitWith :: ExitCode -> IO a
exitWith ExitSuccess = throw (ExitException ExitSuccess)
exitWith code@(ExitFailure n) 
  | n == 0 = ioException (IOError Nothing InvalidArgument "exitWith" "ExitFailure 0" Nothing)
  | otherwise = throw (ExitException code)

exitFailure :: IO a
exitFailure = exitWith (ExitFailure 1)
