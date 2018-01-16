{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveDataTypeable #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Simple.Program.Types
-- Copyright   :  Isaac Jones 2006, Duncan Coutts 2007-2009
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- This provides an abstraction which deals with configuring and running
-- programs. A 'Program' is a static notion of a known program. A
-- 'ConfiguredProgram' is a 'Program' that has been found on the current
-- machine and is ready to be run (possibly with some user-supplied default
-- args). Configuring a program involves finding its location and if necessary
-- finding its version. There's reasonable default behavior for trying to find
-- \"foo\" in PATH, being able to override its location, etc.
--
module Distribution.Simple.Program.Types (
    -- * Program and functions for constructing them
    Program(..),
    ProgramSearchPath,
    ProgramSearchPathEntry(..),
    simpleProgram,

    -- * Configured program and related functions
    ConfiguredProgram(..),
    programPath,
    suppressOverrideArgs,
    ProgArg,
    ProgramLocation(..),
    simpleConfiguredProgram,
  ) where

import Prelude ()
import Distribution.Compat.Prelude

import Distribution.Simple.Program.Find
import Distribution.Version
import Distribution.Verbosity

import qualified Data.Map as Map

-- | Represents a program which can be configured.
--
-- Note: rather than constructing this directly, start with 'simpleProgram' and
-- override any extra fields.
--
data Program = Program {
       -- | The simple name of the program, eg. ghc
       programName :: String,

       -- | A function to search for the program if its location was not
       -- specified by the user. Usually this will just be a call to
       -- 'findProgramOnSearchPath'.
       --
       -- It is supplied with the prevailing search path which will typically
       -- just be used as-is, but can be extended or ignored as needed.
       --
       -- For the purpose of change monitoring, in addition to the location
       -- where the program was found, it returns all the other places that
       -- were tried.
       --
       programFindLocation :: Verbosity -> ProgramSearchPath
                              -> IO (Maybe (FilePath, [FilePath])),

       -- | Try to find the version of the program. For many programs this is
       -- not possible or is not necessary so it's OK to return Nothing.
       programFindVersion :: Verbosity -> FilePath -> IO (Maybe Version),

       -- | A function to do any additional configuration after we have
       -- located the program (and perhaps identified its version). For example
       -- it could add args, or environment vars.
       programPostConf :: Verbosity -> ConfiguredProgram -> IO ConfiguredProgram
     }
instance Show Program where
  show (Program name _ _ _) = "Program: " ++ name

type ProgArg = String

-- | Represents a program which has been configured and is thus ready to be run.
--
-- These are usually made by configuring a 'Program', but if you have to
-- construct one directly then start with 'simpleConfiguredProgram' and
-- override any extra fields.
--
data ConfiguredProgram = ConfiguredProgram {
       -- | Just the name again
       programId :: String,

       -- | The version of this program, if it is known.
       programVersion :: Maybe Version,

       -- | Default command-line args for this program.
       -- These flags will appear first on the command line, so they can be
       -- overridden by subsequent flags.
       programDefaultArgs :: [String],

       -- | Override command-line args for this program.
       -- These flags will appear last on the command line, so they override
       -- all earlier flags.
       programOverrideArgs :: [String],

       -- | Override environment variables for this program.
       -- These env vars will extend\/override the prevailing environment of
       -- the current to form the environment for the new process.
       programOverrideEnv :: [(String, Maybe String)],

       -- | A key-value map listing various properties of the program, useful
       -- for feature detection. Populated during the configuration step, key
       -- names depend on the specific program.
       programProperties :: Map.Map String String,

       -- | Location of the program. eg. @\/usr\/bin\/ghc-6.4@
       programLocation :: ProgramLocation,

       -- | In addition to the 'programLocation' where the program was found,
       -- these are additional locations that were looked at. The combination
       -- of ths found location and these not-found locations can be used to
       -- monitor to detect when the re-configuring the program might give a
       -- different result (e.g. found in a different location).
       --
       programMonitorFiles :: [FilePath]
     }
  deriving (Eq, Generic, Read, Show, Typeable)

instance Binary ConfiguredProgram

-- | Where a program was found. Also tells us whether it's specified by user or
-- not.  This includes not just the path, but the program as well.
data ProgramLocation
    = UserSpecified { locationPath :: FilePath }
      -- ^The user gave the path to this program,
      -- eg. --ghc-path=\/usr\/bin\/ghc-6.6
    | FoundOnSystem { locationPath :: FilePath }
      -- ^The program was found automatically.
      deriving (Eq, Generic, Read, Show)

instance Binary ProgramLocation

-- | The full path of a configured program.
programPath :: ConfiguredProgram -> FilePath
programPath = locationPath . programLocation

-- | Suppress any extra arguments added by the user.
suppressOverrideArgs :: ConfiguredProgram -> ConfiguredProgram
suppressOverrideArgs prog = prog { programOverrideArgs = [] }

-- | Make a simple named program.
--
-- By default we'll just search for it in the path and not try to find the
-- version name. You can override these behaviours if necessary, eg:
--
-- > (simpleProgram "foo") { programFindLocation = ... , programFindVersion ... }
--
simpleProgram :: String -> Program
simpleProgram name = Program {
    programName         = name,
    programFindLocation = \v p -> findProgramOnSearchPath v p name,
    programFindVersion  = \_ _ -> return Nothing,
    programPostConf     = \_ p -> return p
  }

-- | Make a simple 'ConfiguredProgram'.
--
-- > simpleConfiguredProgram "foo" (FoundOnSystem path)
--
simpleConfiguredProgram :: String -> ProgramLocation -> ConfiguredProgram
simpleConfiguredProgram name loc = ConfiguredProgram {
     programId           = name,
     programVersion      = Nothing,
     programDefaultArgs  = [],
     programOverrideArgs = [],
     programOverrideEnv  = [],
     programProperties   = Map.empty,
     programLocation     = loc,
     programMonitorFiles = [] -- did not look in any other locations
  }
