{-# LANGUAGE CPP, ScopedTypeVariables, LambdaCase #-}

-- | Defines a simple exception type and utilities to throw it. The
-- 'PlainGhcException' type is a subset of the 'Panic.GhcException'
-- type.  It omits the exception constructors that involve
-- pretty-printing via 'Outputable.SDoc'.
--
-- There are two reasons for this:
--
-- 1. To avoid import cycles / use of boot files. "Outputable" has
-- many transitive dependencies. To throw exceptions from these
-- modules, the functions here can be used without introducing import
-- cycles.
--
-- 2. To reduce the number of modules that need to be compiled to
-- object code when loading GHC into GHCi. See #13101
module PlainPanic
  ( PlainGhcException(..)
  , showPlainGhcException

  , panic, sorry, pgmError
  , cmdLineError, cmdLineErrorIO
  , assertPanic

  , progName
  ) where

#include "HsVersions.h"

import Config
import Exception
import GHC.Stack
import GhcPrelude
import System.Environment
import System.IO.Unsafe

-- | This type is very similar to 'Panic.GhcException', but it omits
-- the constructors that involve pretty-printing via
-- 'Outputable.SDoc'.  Due to the implementation of 'fromException'
-- for 'Panic.GhcException', this type can be caught as a
-- 'Panic.GhcException'.
--
-- Note that this should only be used for throwing exceptions, not for
-- catching, as 'Panic.GhcException' will not be converted to this
-- type when catching.
data PlainGhcException
  -- | Some other fatal signal (SIGHUP,SIGTERM)
  = PlainSignal Int

  -- | Prints the short usage msg after the error
  | PlainUsageError        String

  -- | A problem with the command line arguments, but don't print usage.
  | PlainCmdLineError      String

  -- | The 'impossible' happened.
  | PlainPanic             String

  -- | The user tickled something that's known not to work yet,
  --   but we're not counting it as a bug.
  | PlainSorry             String

  -- | An installation problem.
  | PlainInstallationError String

  -- | An error in the user's code, probably.
  | PlainProgramError      String

instance Exception PlainGhcException

instance Show PlainGhcException where
  showsPrec _ e@(PlainProgramError _) = showPlainGhcException e
  showsPrec _ e@(PlainCmdLineError _) = showString "<command line>: " . showPlainGhcException e
  showsPrec _ e = showString progName . showString ": " . showPlainGhcException e

-- | The name of this GHC.
progName :: String
progName = unsafePerformIO (getProgName)
{-# NOINLINE progName #-}

-- | Short usage information to display when we are given the wrong cmd line arguments.
short_usage :: String
short_usage = "Usage: For basic information, try the `--help' option."

-- | Append a description of the given exception to this string.
showPlainGhcException :: PlainGhcException -> ShowS
showPlainGhcException =
  \case
    PlainSignal n -> showString "signal: " . shows n
    PlainUsageError str -> showString str . showChar '\n' . showString short_usage
    PlainCmdLineError str -> showString str
    PlainPanic s -> panicMsg (showString s)
    PlainSorry s -> sorryMsg (showString s)
    PlainInstallationError str -> showString str
    PlainProgramError str -> showString str

  where
    sorryMsg :: ShowS -> ShowS
    sorryMsg s =
        showString "sorry! (unimplemented feature or known bug)\n"
      . showString ("  (GHC version " ++ cProjectVersion ++ " for " ++ TargetPlatform_NAME ++ "):\n\t")
      . s . showString "\n"

    panicMsg :: ShowS -> ShowS
    panicMsg s =
        showString "panic! (the 'impossible' happened)\n"
      . showString ("  (GHC version " ++ cProjectVersion ++ " for " ++ TargetPlatform_NAME ++ "):\n\t")
      . s . showString "\n\n"
      . showString "Please report this as a GHC bug:  https://www.haskell.org/ghc/reportabug\n"

throwPlainGhcException :: PlainGhcException -> a
throwPlainGhcException = Exception.throw

-- | Panics and asserts.
panic, sorry, pgmError :: String -> a
panic    x = unsafeDupablePerformIO $ do
   stack <- ccsToStrings =<< getCurrentCCS x
   if null stack
      then throwPlainGhcException (PlainPanic x)
      else throwPlainGhcException (PlainPanic (x ++ '\n' : renderStack stack))

sorry    x = throwPlainGhcException (PlainSorry x)
pgmError x = throwPlainGhcException (PlainProgramError x)

cmdLineError :: String -> a
cmdLineError = unsafeDupablePerformIO . cmdLineErrorIO

cmdLineErrorIO :: String -> IO a
cmdLineErrorIO x = do
  stack <- ccsToStrings =<< getCurrentCCS x
  if null stack
    then throwPlainGhcException (PlainCmdLineError x)
    else throwPlainGhcException (PlainCmdLineError (x ++ '\n' : renderStack stack))

-- | Throw a failed assertion exception for a given filename and line number.
assertPanic :: String -> Int -> a
assertPanic file line =
  Exception.throw (Exception.AssertionFailed
           ("ASSERT failed! file " ++ file ++ ", line " ++ show line))
