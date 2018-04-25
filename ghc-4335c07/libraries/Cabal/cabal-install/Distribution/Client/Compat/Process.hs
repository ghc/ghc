{-# LANGUAGE CPP #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Client.Compat.Process
-- Copyright   :  (c) 2013 Liu Hao, Brent Yorgey
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  cabal-devel@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Cross-platform utilities for invoking processes.
--
-----------------------------------------------------------------------------

module Distribution.Client.Compat.Process (
  readProcessWithExitCode
) where

#if !MIN_VERSION_base(4,6,0)
import           Prelude           hiding (catch)
#endif

import           Control.Exception (catch, throw)
import           System.Exit       (ExitCode (ExitFailure))
import           System.IO.Error   (isDoesNotExistError, isPermissionError)
import qualified System.Process    as P

-- | @readProcessWithExitCode@ creates an external process, reads its
--   standard output and standard error strictly, waits until the
--   process terminates, and then returns the @ExitCode@ of the
--   process, the standard output, and the standard error.
--
--   See the documentation of the version from @System.Process@ for
--   more information.
--
--   The version from @System.Process@ behaves inconsistently across
--   platforms when an executable with the given name is not found: in
--   some cases it returns an @ExitFailure@, in others it throws an
--   exception.  This variant catches \"does not exist\" and
--   \"permission denied\" exceptions and turns them into
--   @ExitFailure@s.
readProcessWithExitCode :: FilePath -> [String] -> String -> IO (ExitCode, String, String)
readProcessWithExitCode cmd args input =
  P.readProcessWithExitCode cmd args input
    `catch` \e -> if isDoesNotExistError e || isPermissionError e
                    then return (ExitFailure 127, "", "")
                    else throw e
