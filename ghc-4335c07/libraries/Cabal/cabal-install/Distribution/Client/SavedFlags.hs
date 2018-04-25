{-# LANGUAGE DeriveDataTypeable #-}

module Distribution.Client.SavedFlags
       ( readCommandFlags, writeCommandFlags
       , readSavedArgs, writeSavedArgs
       ) where

import Distribution.Simple.Command
import Distribution.Simple.UserHooks ( Args )
import Distribution.Simple.Utils
       ( createDirectoryIfMissingVerbose, unintersperse )
import Distribution.Verbosity

import Control.Exception ( Exception, throwIO )
import Control.Monad ( liftM )
import Data.List ( intercalate )
import Data.Maybe ( fromMaybe )
import Data.Typeable
import System.Directory ( doesFileExist )
import System.FilePath ( takeDirectory )


writeSavedArgs :: Verbosity -> FilePath -> [String] -> IO ()
writeSavedArgs verbosity path args = do
  createDirectoryIfMissingVerbose
    (lessVerbose verbosity) True (takeDirectory path)
  writeFile path (intercalate "\0" args)


-- | Write command-line flags to a file, separated by null characters. This
-- format is also suitable for the @xargs -0@ command. Using the null
-- character also avoids the problem of escaping newlines or spaces,
-- because unlike other whitespace characters, the null character is
-- not valid in command-line arguments.
writeCommandFlags :: Verbosity -> FilePath -> CommandUI flags -> flags -> IO ()
writeCommandFlags verbosity path command flags =
  writeSavedArgs verbosity path (commandShowOptions command flags)


readSavedArgs :: FilePath -> IO (Maybe [String])
readSavedArgs path = do
  exists <- doesFileExist path
  if exists
     then liftM (Just . unintersperse '\0') (readFile path)
    else return Nothing


-- | Read command-line arguments, separated by null characters, from a file.
-- Returns the default flags if the file does not exist.
readCommandFlags :: FilePath -> CommandUI flags -> IO flags
readCommandFlags path command = do
  savedArgs <- liftM (fromMaybe []) (readSavedArgs path)
  case (commandParseArgs command True savedArgs) of
    CommandHelp _ -> throwIO (SavedArgsErrorHelp savedArgs)
    CommandList _ -> throwIO (SavedArgsErrorList savedArgs)
    CommandErrors errs -> throwIO (SavedArgsErrorOther savedArgs errs)
    CommandReadyToGo (mkFlags, _) ->
      return (mkFlags (commandDefaultFlags command))

-- -----------------------------------------------------------------------------
-- * Exceptions
-- -----------------------------------------------------------------------------

data SavedArgsError
    = SavedArgsErrorHelp Args
    | SavedArgsErrorList Args
    | SavedArgsErrorOther Args [String]
  deriving (Typeable)

instance Show SavedArgsError where
  show (SavedArgsErrorHelp args) =
    "unexpected flag '--help', saved command line was:\n"
    ++ intercalate " " args
  show (SavedArgsErrorList args) =
    "unexpected flag '--list-options', saved command line was:\n"
    ++ intercalate " " args
  show (SavedArgsErrorOther args errs) =
    "saved command line was:\n"
    ++ intercalate " " args ++ "\n"
    ++ "encountered errors:\n"
    ++ intercalate "\n" errs

instance Exception SavedArgsError
