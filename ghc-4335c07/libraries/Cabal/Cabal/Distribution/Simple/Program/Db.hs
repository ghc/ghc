{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveDataTypeable #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Simple.Program.Db
-- Copyright   :  Isaac Jones 2006, Duncan Coutts 2007-2009
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- This provides a 'ProgramDb' type which holds configured and not-yet
-- configured programs. It is the parameter to lots of actions elsewhere in
-- Cabal that need to look up and run programs. If we had a Cabal monad,
-- the 'ProgramDb' would probably be a reader or state component of it.
--
-- One nice thing about using it is that any program that is
-- registered with Cabal will get some \"configure\" and \".cabal\"
-- helpers like --with-foo-args --foo-path= and extra-foo-args.
--
-- There's also a hook for adding programs in a Setup.lhs script.  See
-- hookedPrograms in 'Distribution.Simple.UserHooks'.  This gives a
-- hook user the ability to get the above flags and such so that they
-- don't have to write all the PATH logic inside Setup.lhs.

module Distribution.Simple.Program.Db (
    -- * The collection of configured programs we can run
    ProgramDb,
    emptyProgramDb,
    defaultProgramDb,
    restoreProgramDb,

    -- ** Query and manipulate the program db
    addKnownProgram,
    addKnownPrograms,
    lookupKnownProgram,
    knownPrograms,
    getProgramSearchPath,
    setProgramSearchPath,
    modifyProgramSearchPath,
    userSpecifyPath,
    userSpecifyPaths,
    userMaybeSpecifyPath,
    userSpecifyArgs,
    userSpecifyArgss,
    userSpecifiedArgs,
    lookupProgram,
    updateProgram,
    configuredPrograms,

    -- ** Query and manipulate the program db
    configureProgram,
    configureAllKnownPrograms,
    unconfigureProgram,
    lookupProgramVersion,
    reconfigurePrograms,
    requireProgram,
    requireProgramVersion,

  ) where

import Prelude ()
import Distribution.Compat.Prelude

import Distribution.Simple.Program.Types
import Distribution.Simple.Program.Find
import Distribution.Simple.Program.Builtin
import Distribution.Simple.Utils
import Distribution.Version
import Distribution.Text
import Distribution.Verbosity

import Control.Monad (join)
import Data.Tuple (swap)
import qualified Data.Map as Map

-- ------------------------------------------------------------
-- * Programs database
-- ------------------------------------------------------------

-- | The configuration is a collection of information about programs. It
-- contains information both about configured programs and also about programs
-- that we are yet to configure.
--
-- The idea is that we start from a collection of unconfigured programs and one
-- by one we try to configure them at which point we move them into the
-- configured collection. For unconfigured programs we record not just the
-- 'Program' but also any user-provided arguments and location for the program.
data ProgramDb = ProgramDb {
        unconfiguredProgs :: UnconfiguredProgs,
        progSearchPath    :: ProgramSearchPath,
        configuredProgs   :: ConfiguredProgs
    }
  deriving (Typeable)

type UnconfiguredProgram = (Program, Maybe FilePath, [ProgArg])
type UnconfiguredProgs   = Map.Map String UnconfiguredProgram
type ConfiguredProgs     = Map.Map String ConfiguredProgram


emptyProgramDb :: ProgramDb
emptyProgramDb = ProgramDb Map.empty defaultProgramSearchPath Map.empty

defaultProgramDb :: ProgramDb
defaultProgramDb = restoreProgramDb builtinPrograms emptyProgramDb


-- internal helpers:
updateUnconfiguredProgs :: (UnconfiguredProgs -> UnconfiguredProgs)
                        -> ProgramDb -> ProgramDb
updateUnconfiguredProgs update progdb =
  progdb { unconfiguredProgs = update (unconfiguredProgs progdb) }

updateConfiguredProgs :: (ConfiguredProgs -> ConfiguredProgs)
                      -> ProgramDb -> ProgramDb
updateConfiguredProgs update progdb =
  progdb { configuredProgs = update (configuredProgs progdb) }


-- Read & Show instances are based on listToFM

-- | Note that this instance does not preserve the known 'Program's.
-- See 'restoreProgramDb' for details.
--
instance Show ProgramDb where
  show = show . Map.toAscList . configuredProgs

-- | Note that this instance does not preserve the known 'Program's.
-- See 'restoreProgramDb' for details.
--
instance Read ProgramDb where
  readsPrec p s =
    [ (emptyProgramDb { configuredProgs = Map.fromList s' }, r)
    | (s', r) <- readsPrec p s ]

-- | Note that this instance does not preserve the known 'Program's.
-- See 'restoreProgramDb' for details.
--
instance Binary ProgramDb where
  put db = do
    put (progSearchPath db)
    put (configuredProgs db)

  get = do
    searchpath <- get
    progs      <- get
    return $! emptyProgramDb {
      progSearchPath  = searchpath,
      configuredProgs = progs
    }


-- | The 'Read'\/'Show' and 'Binary' instances do not preserve all the
-- unconfigured 'Programs' because 'Program' is not in 'Read'\/'Show' because
-- it contains functions. So to fully restore a deserialised 'ProgramDb' use
-- this function to add back all the known 'Program's.
--
-- * It does not add the default programs, but you probably want them, use
--   'builtinPrograms' in addition to any extra you might need.
--
restoreProgramDb :: [Program] -> ProgramDb -> ProgramDb
restoreProgramDb = addKnownPrograms


-- -------------------------------
-- Managing unconfigured programs

-- | Add a known program that we may configure later
--
addKnownProgram :: Program -> ProgramDb -> ProgramDb
addKnownProgram prog = updateUnconfiguredProgs $
  Map.insertWith combine (programName prog) (prog, Nothing, [])
  where combine _ (_, path, args) = (prog, path, args)


addKnownPrograms :: [Program] -> ProgramDb -> ProgramDb
addKnownPrograms progs progdb = foldl' (flip addKnownProgram) progdb progs


lookupKnownProgram :: String -> ProgramDb -> Maybe Program
lookupKnownProgram name =
  fmap (\(p,_,_)->p) . Map.lookup name . unconfiguredProgs


knownPrograms :: ProgramDb -> [(Program, Maybe ConfiguredProgram)]
knownPrograms progdb =
  [ (p,p') | (p,_,_) <- Map.elems (unconfiguredProgs progdb)
           , let p' = Map.lookup (programName p) (configuredProgs progdb) ]

-- | Get the current 'ProgramSearchPath' used by the 'ProgramDb'.
-- This is the default list of locations where programs are looked for when
-- configuring them. This can be overridden for specific programs (with
-- 'userSpecifyPath'), and specific known programs can modify or ignore this
-- search path in their own configuration code.
--
getProgramSearchPath :: ProgramDb -> ProgramSearchPath
getProgramSearchPath = progSearchPath

-- | Change the current 'ProgramSearchPath' used by the 'ProgramDb'.
-- This will affect programs that are configured from here on, so you
-- should usually set it before configuring any programs.
--
setProgramSearchPath :: ProgramSearchPath -> ProgramDb -> ProgramDb
setProgramSearchPath searchpath db = db { progSearchPath = searchpath }

-- | Modify the current 'ProgramSearchPath' used by the 'ProgramDb'.
-- This will affect programs that are configured from here on, so you
-- should usually modify it before configuring any programs.
--
modifyProgramSearchPath :: (ProgramSearchPath -> ProgramSearchPath)
                        -> ProgramDb
                        -> ProgramDb
modifyProgramSearchPath f db =
  setProgramSearchPath (f $ getProgramSearchPath db) db

-- |User-specify this path.  Basically override any path information
-- for this program in the configuration. If it's not a known
-- program ignore it.
--
userSpecifyPath :: String   -- ^Program name
                -> FilePath -- ^user-specified path to the program
                -> ProgramDb -> ProgramDb
userSpecifyPath name path = updateUnconfiguredProgs $
  flip Map.update name $ \(prog, _, args) -> Just (prog, Just path, args)


userMaybeSpecifyPath :: String -> Maybe FilePath
                     -> ProgramDb -> ProgramDb
userMaybeSpecifyPath _    Nothing progdb     = progdb
userMaybeSpecifyPath name (Just path) progdb = userSpecifyPath name path progdb


-- |User-specify the arguments for this program.  Basically override
-- any args information for this program in the configuration. If it's
-- not a known program, ignore it..
userSpecifyArgs :: String    -- ^Program name
                -> [ProgArg] -- ^user-specified args
                -> ProgramDb
                -> ProgramDb
userSpecifyArgs name args' =
    updateUnconfiguredProgs
      (flip Map.update name $
         \(prog, path, args) -> Just (prog, path, args ++ args'))
  . updateConfiguredProgs
      (flip Map.update name $
         \prog -> Just prog { programOverrideArgs = programOverrideArgs prog
                                                 ++ args' })


-- | Like 'userSpecifyPath' but for a list of progs and their paths.
--
userSpecifyPaths :: [(String, FilePath)]
                 -> ProgramDb
                 -> ProgramDb
userSpecifyPaths paths progdb =
  foldl' (\progdb' (prog, path) -> userSpecifyPath prog path progdb') progdb paths


-- | Like 'userSpecifyPath' but for a list of progs and their args.
--
userSpecifyArgss :: [(String, [ProgArg])]
                 -> ProgramDb
                 -> ProgramDb
userSpecifyArgss argss progdb =
  foldl' (\progdb' (prog, args) -> userSpecifyArgs prog args progdb') progdb argss


-- | Get the path that has been previously specified for a program, if any.
--
userSpecifiedPath :: Program -> ProgramDb -> Maybe FilePath
userSpecifiedPath prog =
  join . fmap (\(_,p,_)->p) . Map.lookup (programName prog) . unconfiguredProgs


-- | Get any extra args that have been previously specified for a program.
--
userSpecifiedArgs :: Program -> ProgramDb -> [ProgArg]
userSpecifiedArgs prog =
  maybe [] (\(_,_,as)->as) . Map.lookup (programName prog) . unconfiguredProgs


-- -----------------------------
-- Managing configured programs

-- | Try to find a configured program
lookupProgram :: Program -> ProgramDb -> Maybe ConfiguredProgram
lookupProgram prog = Map.lookup (programName prog) . configuredProgs


-- | Update a configured program in the database.
updateProgram :: ConfiguredProgram -> ProgramDb
                                   -> ProgramDb
updateProgram prog = updateConfiguredProgs $
  Map.insert (programId prog) prog


-- | List all configured programs.
configuredPrograms :: ProgramDb -> [ConfiguredProgram]
configuredPrograms = Map.elems . configuredProgs

-- ---------------------------
-- Configuring known programs

-- | Try to configure a specific program. If the program is already included in
-- the collection of unconfigured programs then we use any user-supplied
-- location and arguments. If the program gets configured successfully it gets
-- added to the configured collection.
--
-- Note that it is not a failure if the program cannot be configured. It's only
-- a failure if the user supplied a location and the program could not be found
-- at that location.
--
-- The reason for it not being a failure at this stage is that we don't know up
-- front all the programs we will need, so we try to configure them all.
-- To verify that a program was actually successfully configured use
-- 'requireProgram'.
--
configureProgram :: Verbosity
                 -> Program
                 -> ProgramDb
                 -> IO ProgramDb
configureProgram verbosity prog progdb = do
  let name = programName prog
  maybeLocation <- case userSpecifiedPath prog progdb of
    Nothing   ->
      programFindLocation prog verbosity (progSearchPath progdb)
      >>= return . fmap (swap . fmap FoundOnSystem . swap)
    Just path -> do
      absolute <- doesExecutableExist path
      if absolute
        then return (Just (UserSpecified path, []))
        else findProgramOnSearchPath verbosity (progSearchPath progdb) path
             >>= maybe (die' verbosity notFound)
                       (return . Just . swap . fmap UserSpecified . swap)
      where notFound = "Cannot find the program '" ++ name
                     ++ "'. User-specified path '"
                     ++ path ++ "' does not refer to an executable and "
                     ++ "the program is not on the system path."
  case maybeLocation of
    Nothing -> return progdb
    Just (location, triedLocations) -> do
      version <- programFindVersion prog verbosity (locationPath location)
      newPath <- programSearchPathAsPATHVar (progSearchPath progdb)
      let configuredProg        = ConfiguredProgram {
            programId           = name,
            programVersion      = version,
            programDefaultArgs  = [],
            programOverrideArgs = userSpecifiedArgs prog progdb,
            programOverrideEnv  = [("PATH", Just newPath)],
            programProperties   = Map.empty,
            programLocation     = location,
            programMonitorFiles = triedLocations
          }
      configuredProg' <- programPostConf prog verbosity configuredProg
      return (updateConfiguredProgs (Map.insert name configuredProg') progdb)


-- | Configure a bunch of programs using 'configureProgram'. Just a 'foldM'.
--
configurePrograms :: Verbosity
                  -> [Program]
                  -> ProgramDb
                  -> IO ProgramDb
configurePrograms verbosity progs progdb =
  foldM (flip (configureProgram verbosity)) progdb progs


-- | Unconfigure a program.  This is basically a hack and you shouldn't
-- use it, but it can be handy for making sure a 'requireProgram'
-- actually reconfigures.
unconfigureProgram :: String -> ProgramDb -> ProgramDb
unconfigureProgram progname =
  updateConfiguredProgs $ Map.delete progname

-- | Try to configure all the known programs that have not yet been configured.
--
configureAllKnownPrograms :: Verbosity
                          -> ProgramDb
                          -> IO ProgramDb
configureAllKnownPrograms verbosity progdb =
  configurePrograms verbosity
    [ prog | (prog,_,_) <- Map.elems notYetConfigured ] progdb
  where
    notYetConfigured = unconfiguredProgs progdb
      `Map.difference` configuredProgs progdb


-- | reconfigure a bunch of programs given new user-specified args. It takes
-- the same inputs as 'userSpecifyPath' and 'userSpecifyArgs' and for all progs
-- with a new path it calls 'configureProgram'.
--
reconfigurePrograms :: Verbosity
                    -> [(String, FilePath)]
                    -> [(String, [ProgArg])]
                    -> ProgramDb
                    -> IO ProgramDb
reconfigurePrograms verbosity paths argss progdb = do
  configurePrograms verbosity progs
   . userSpecifyPaths paths
   . userSpecifyArgss argss
   $ progdb

  where
    progs = catMaybes [ lookupKnownProgram name progdb | (name,_) <- paths ]


-- | Check that a program is configured and available to be run.
--
-- It raises an exception if the program could not be configured, otherwise
-- it returns the configured program.
--
requireProgram :: Verbosity -> Program -> ProgramDb
               -> IO (ConfiguredProgram, ProgramDb)
requireProgram verbosity prog progdb = do

  -- If it's not already been configured, try to configure it now
  progdb' <- case lookupProgram prog progdb of
    Nothing -> configureProgram verbosity prog progdb
    Just _  -> return progdb

  case lookupProgram prog progdb' of
    Nothing             -> die' verbosity notFound
    Just configuredProg -> return (configuredProg, progdb')

  where notFound       = "The program '" ++ programName prog
                      ++ "' is required but it could not be found."


-- | Check that a program is configured and available to be run.
--
-- Additionally check that the program version number is suitable and return
-- it. For example you could require 'AnyVersion' or @'orLaterVersion'
-- ('Version' [1,0] [])@
--
-- It returns the configured program, its version number and a possibly updated
-- 'ProgramDb'. If the program could not be configured or the version is
-- unsuitable, it returns an error value.
--
lookupProgramVersion
  :: Verbosity -> Program -> VersionRange -> ProgramDb
  -> IO (Either String (ConfiguredProgram, Version, ProgramDb))
lookupProgramVersion verbosity prog range programDb = do

  -- If it's not already been configured, try to configure it now
  programDb' <- case lookupProgram prog programDb of
    Nothing -> configureProgram verbosity prog programDb
    Just _  -> return programDb

  case lookupProgram prog programDb' of
    Nothing                           -> return $! Left notFound
    Just configuredProg@ConfiguredProgram { programLocation = location } ->
      case programVersion configuredProg of
        Just version
          | withinRange version range ->
            return $! Right (configuredProg, version ,programDb')
          | otherwise                 ->
            return $! Left (badVersion version location)
        Nothing                       ->
          return $! Left (unknownVersion location)

  where notFound       = "The program '"
                      ++ programName prog ++ "'" ++ versionRequirement
                      ++ " is required but it could not be found."
        badVersion v l = "The program '"
                      ++ programName prog ++ "'" ++ versionRequirement
                      ++ " is required but the version found at "
                      ++ locationPath l ++ " is version " ++ display v
        unknownVersion l = "The program '"
                      ++ programName prog ++ "'" ++ versionRequirement
                      ++ " is required but the version of "
                      ++ locationPath l ++ " could not be determined."
        versionRequirement
          | isAnyVersion range = ""
          | otherwise          = " version " ++ display range

-- | Like 'lookupProgramVersion', but raises an exception in case of error
-- instead of returning 'Left errMsg'.
--
requireProgramVersion :: Verbosity -> Program -> VersionRange
                      -> ProgramDb
                      -> IO (ConfiguredProgram, Version, ProgramDb)
requireProgramVersion verbosity prog range programDb =
  join $ either (die' verbosity) return `fmap`
  lookupProgramVersion verbosity prog range programDb
