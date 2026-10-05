{-# LANGUAGE CPP #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE MagicHash #-}

-- |
--
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

module System.Exit
    (ExitCode(ExitSuccess, ExitFailure),
     exitWith,
     exitFailure,
     exitSuccess,
     die
     ) where

import GHC.IO.Exception (ExitCode (ExitSuccess, ExitFailure), exitWith)
import Control.Monad ((>>))
import Data.String (String)
import System.IO (IO, hPutStrLn, stderr)
#if __GLASGOW_HASKELL__ >= 1001
import GHC.Internal.Num as Rebindable( Num, fromInteger ) -- For known-key names
import qualified GHC.Internal.Stack.Types as Rebindable (SrcLoc(..), pushCallStack, emptyCallStack)
import qualified GHC.Internal.Types as Rebindable (unpackCString#, unpackCStringUtf8#)
#endif

-- | The computation 'exitFailure' is equivalent to
-- 'exitWith' @(@'ExitFailure' /exitfail/@)@,
-- where /exitfail/ is implementation-dependent.
exitFailure :: IO a
exitFailure = exitWith (ExitFailure 1)

-- | The computation 'exitSuccess' is equivalent to
-- 'exitWith' 'ExitSuccess', It terminates the program
-- successfully.
exitSuccess :: IO a
exitSuccess = exitWith ExitSuccess

-- | Write given error message to `stderr` and terminate with `exitFailure`.
--
-- @since base-4.8.0.0
die :: String -> IO a
die err = hPutStrLn stderr err >> exitFailure
