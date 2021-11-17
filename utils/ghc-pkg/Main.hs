{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- We never want to link against terminfo while bootstrapping.
#if defined(BOOTSTRAPPING)
#if defined(WITH_TERMINFO)
#undef WITH_TERMINFO
#endif
#endif

-- Fine if this comes from make/Hadrian or the pre-built base.
#include <ghcplatform.h>

-----------------------------------------------------------------------------
--
-- (c) The University of Glasgow 2004-2009.
--
-- Package management tool
--
-----------------------------------------------------------------------------

module Main (main) where

import qualified GHC.Unit.Database as GhcPkg
import GHC.Unit.Database hiding (mkMungePathUrl)
import GHC.HandleEncoding
import GHC.BaseDir (getBaseDir)
import GHC.Settings.Utils (getTargetArchOS, maybeReadFuzzy)
import GHC.Platform.Host (hostPlatformArchOS)
import GHC.UniqueSubdir (uniqueSubdir)
import qualified GHC.Data.ShortText as ST
import GHC.Version ( cProjectVersion )
import qualified Distribution.Simple.PackageIndex as PackageIndex
import qualified Data.Graph as Graph
import qualified Distribution.ModuleName as ModuleName
import Distribution.ModuleName (ModuleName)
import Distribution.InstalledPackageInfo as Cabal
import qualified Distribution.Parsec as Cabal
import Distribution.Package hiding (installedUnitId)
import Distribution.Text
import Distribution.Version
import Distribution.Backpack
import Distribution.Pretty (Pretty (..))
import Distribution.Types.UnqualComponentName
import Distribution.Types.LibraryName
import Distribution.Types.MungedPackageName
import Distribution.Types.MungedPackageId
import Distribution.Simple.Utils (toUTF8BS, writeUTF8File, readUTF8File)
import qualified Data.Version as Version
import System.FilePath as FilePath
import qualified System.FilePath.Posix as FilePath.Posix
import System.Directory ( getXdgDirectory, createDirectoryIfMissing, getAppUserDataDirectory,
                          getModificationTime, XdgDirectory ( XdgData ) )
import Text.Printf

import Prelude

import System.Console.GetOpt
import qualified Control.Exception as Exception
import Data.Maybe
import Data.Bifunctor

import Data.Char ( toLower )
import Control.Monad
import System.Directory ( doesDirectoryExist, getDirectoryContents,
                          doesFileExist, removeFile,
                          getCurrentDirectory )
import System.Exit ( exitWith, ExitCode(..) )
import System.Environment ( getArgs, getProgName, getEnv )
import System.IO
import System.IO.Error
import GHC.IO           ( catchException )
import GHC.IO.Exception (IOErrorType(InappropriateType))
import Data.List ( group, sort, sortBy, nub, partition, find
                 , intercalate, intersperse, foldl', unfoldr
                 , isInfixOf, isSuffixOf, isPrefixOf, stripPrefix )
import Control.Concurrent
import qualified Data.Foldable as F
import qualified Data.Traversable as F
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.ByteString as BS

#if defined(mingw32_HOST_OS)
import GHC.ConsoleHandler
#else
import System.Posix hiding (fdToHandle)
#endif

#if defined(GLOB)
import qualified System.Info(os)
#endif

#if defined(WITH_TERMINFO)
import System.Console.Terminfo as Terminfo
#endif

#if defined(mingw32_HOST_OS)
# if defined(i386_HOST_ARCH)
#  define WINDOWS_CCONV stdcall
# elif defined(x86_64_HOST_ARCH)
#  define WINDOWS_CCONV ccall
# else
#  error Unknown mingw32 arch
# endif
#endif

-- | Short-circuit 'any' with a \"monadic predicate\".
anyM :: (Monad m) => (a -> m Bool) -> [a] -> m Bool
anyM _ [] = return False
anyM p (x:xs) = do
  b <- p x
  if b
    then return True
    else anyM p xs

-- -----------------------------------------------------------------------------
-- Entry point

main :: IO ()
main = do
  configureHandleEncoding
  args <- getArgs

  case getOpt Permute (flags ++ deprecFlags) args of
        (cli,_,[]) | FlagHelp `elem` cli -> do
           prog <- getProgramName
           bye (usageInfo (usageHeader prog) flags)
        (cli,_,[]) | FlagVersion `elem` cli ->
           bye ourCopyright
        (cli,nonopts,[]) ->
           case getVerbosity Normal cli of
           Right v -> runit v cli nonopts
           Left err -> die err
        (_,_,errors) -> do
           prog <- getProgramName
           die (concat errors ++ shortUsage prog)

-- -----------------------------------------------------------------------------
-- Command-line syntax

data Flag
  = FlagUser
  | FlagGlobal
  | FlagHelp
  | FlagVersion
  | FlagConfig FilePath
  | FlagGlobalConfig FilePath
  | FlagUserConfig FilePath
  | FlagForce
  | FlagForceFiles
  | FlagMultiInstance
  | FlagExpandEnvVars
  | FlagExpandPkgroot
  | FlagNoExpandPkgroot
  | FlagSimpleOutput
  | FlagNamesOnly
  | FlagIgnoreCase
  | FlagNoUserDb
  | FlagVerbosity (Maybe String)
  | FlagUnitId
  | FlagShowUnitIds
  deriving Eq

flags :: [OptDescr Flag]
flags = [
  Option [] ["user"] (NoArg FlagUser)
        "use the current user's package database",
  Option [] ["global"] (NoArg FlagGlobal)
        "use the global package database",
  Option ['f'] ["package-db"] (ReqArg FlagConfig "FILE/DIR")
        "use the specified package database",
  Option [] ["package-conf"] (ReqArg FlagConfig "FILE/DIR")
        "use the specified package database (DEPRECATED)",
  Option [] ["global-package-db"] (ReqArg FlagGlobalConfig "DIR")
        "location of the global package database",
  Option [] ["no-user-package-db"] (NoArg FlagNoUserDb)
        "never read the user package database",
  Option [] ["user-package-db"] (ReqArg FlagUserConfig "DIR")
        "location of the user package database (use instead of default)",
  Option [] ["no-user-package-conf"] (NoArg FlagNoUserDb)
        "never read the user package database (DEPRECATED)",
  Option [] ["force"] (NoArg FlagForce)
         "ignore missing dependencies, directories, and libraries",
  Option [] ["force-files"] (NoArg FlagForceFiles)
         "ignore missing directories and libraries only",
  Option [] ["enable-multi-instance"] (NoArg FlagMultiInstance)
        "allow registering multiple instances of the same package version",
  Option [] ["expand-env-vars"] (NoArg FlagExpandEnvVars)
        "expand environment variables (${name}-style) in input package descriptions",
  Option [] ["expand-pkgroot"] (NoArg FlagExpandPkgroot)
        "expand ${pkgroot}-relative paths to absolute in output package descriptions",
  Option [] ["no-expand-pkgroot"] (NoArg FlagNoExpandPkgroot)
        "preserve ${pkgroot}-relative paths in output package descriptions",
  Option ['?'] ["help"] (NoArg FlagHelp)
        "display this help and exit",
  Option ['V'] ["version"] (NoArg FlagVersion)
        "output version information and exit",
  Option [] ["simple-output"] (NoArg FlagSimpleOutput)
        "print output in easy-to-parse format for some commands",
  Option [] ["show-unit-ids"] (NoArg FlagShowUnitIds)
        "print unit-ids instead of package identifiers",
  Option [] ["names-only"] (NoArg FlagNamesOnly)
        "only print package names, not versions; can only be used with list --simple-output",
  Option [] ["ignore-case"] (NoArg FlagIgnoreCase)
        "ignore case for substring matching",
  Option [] ["ipid", "unit-id"] (NoArg FlagUnitId)
        "interpret package arguments as unit IDs (e.g. installed package IDs)",
  Option ['v'] ["verbose"] (OptArg FlagVerbosity "Verbosity")
        "verbosity level (0-2, default 1)"
  ]

data Verbosity = Silent | Normal | Verbose
    deriving (Show, Eq, Ord)

getVerbosity :: Verbosity -> [Flag] -> Either String Verbosity
getVerbosity v [] = Right v
getVerbosity _ (FlagVerbosity Nothing    : fs) = getVerbosity Verbose fs
getVerbosity _ (FlagVerbosity (Just "0") : fs) = getVerbosity Silent  fs
getVerbosity _ (FlagVerbosity (Just "1") : fs) = getVerbosity Normal  fs
getVerbosity _ (FlagVerbosity (Just "2") : fs) = getVerbosity Verbose fs
getVerbosity _ (FlagVerbosity v : _) = Left ("Bad verbosity: " ++ show v)
getVerbosity v (_ : fs) = getVerbosity v fs

deprecFlags :: [OptDescr Flag]
deprecFlags = [
        -- put deprecated flags here
  ]

ourCopyright :: String
ourCopyright = "GHC package manager version " ++ GHC.Version.cProjectVersion ++ "\n"

shortUsage :: String -> String
shortUsage prog = "For usage information see '" ++ prog ++ " --help'."

usageHeader :: String -> String
usageHeader prog = substProg prog $
  "Usage:\n" ++
  "  $p init {path}\n" ++
  "    Create and initialise a package database at the location {path}.\n" ++
  "    Packages can be registered in the new database using the register\n" ++
  "    command with --package-db={path}.  To use the new database with GHC,\n" ++
  "    use GHC's -package-db flag.\n" ++
  "\n" ++
  "  $p register {filename | -}\n" ++
  "    Register the package using the specified installed package\n" ++
  "    description. The syntax for the latter is given in the $p\n" ++
  "    documentation.  The input file should be encoded in UTF-8.\n" ++
  "\n" ++
  "  $p update {filename | -}\n" ++
  "    Register the package, overwriting any other package with the\n" ++
  "    same name. The input file should be encoded in UTF-8.\n" ++
  "\n" ++
  "  $p unregister [pkg-id] \n" ++
  "    Unregister the specified packages in the order given.\n" ++
  "\n" ++
  "  $p expose {pkg-id}\n" ++
  "    Expose the specified package.\n" ++
  "\n" ++
  "  $p hide {pkg-id}\n" ++
  "    Hide the specified package.\n" ++
  "\n" ++
  "  $p trust {pkg-id}\n" ++
  "    Trust the specified package.\n" ++
  "\n" ++
  "  $p distrust {pkg-id}\n" ++
  "    Distrust the specified package.\n" ++
  "\n" ++
  "  $p list [pkg]\n" ++
  "    List registered packages in the global database, and also the\n" ++
  "    user database if --user is given. If a package name is given\n" ++
  "    all the registered versions will be listed in ascending order.\n" ++
  "    Accepts the --simple-output flag.\n" ++
  "\n" ++
  "  $p dot\n" ++
  "    Generate a graph of the package dependencies in a form suitable\n" ++
  "    for input for the graphviz tools.  For example, to generate a PDF\n" ++
  "    of the dependency graph: ghc-pkg dot | tred | dot -Tpdf >pkgs.pdf\n" ++
  "\n" ++
  "  $p find-module {module}\n" ++
  "    List registered packages exposing module {module} in the global\n" ++
  "    database, and also the user database if --user is given.\n" ++
  "    All the registered versions will be listed in ascending order.\n" ++
  "    Accepts the --simple-output flag.\n" ++
  "\n" ++
  "  $p latest {pkg-id}\n" ++
  "    Prints the highest registered version of a package.\n" ++
  "\n" ++
  "  $p check\n" ++
  "    Check the consistency of package dependencies and list broken packages.\n" ++
  "    Accepts the --simple-output flag.\n" ++
  "\n" ++
  "  $p describe {pkg}\n" ++
  "    Give the registered description for the specified package. The\n" ++
  "    description is returned in precisely the syntax required by $p\n" ++
  "    register.\n" ++
  "\n" ++
  "  $p field {pkg} {field}\n" ++
  "    Extract the specified field of the package description for the\n" ++
  "    specified package. Accepts comma-separated multiple fields.\n" ++
  "\n" ++
  "  $p dump\n" ++
  "    Dump the registered description for every package.  This is like\n" ++
  "    \"ghc-pkg describe '*'\", except that it is intended to be used\n" ++
  "    by tools that parse the results, rather than humans.  The output is\n" ++
  "    always encoded in UTF-8, regardless of the current locale.\n" ++
  "\n" ++
  "  $p recache\n" ++
  "    Regenerate the package database cache.  This command should only be\n" ++
  "    necessary if you added a package to the database by dropping a file\n" ++
  "    into the database directory manually.  By default, the global DB\n" ++
  "    is recached; to recache a different DB use --user or --package-db\n" ++
  "    as appropriate.\n" ++
  "\n" ++
  " Substring matching is supported for {module} in find-module and\n" ++
  " for {pkg} in list, describe, and field, where a '*' indicates\n" ++
  " open substring ends (prefix*, *suffix, *infix*).  Use --ipid to\n" ++
  " match against the installed package ID instead.\n" ++
  "\n" ++
  "  When asked to modify a database (register, unregister, update,\n"++
  "  hide, expose, and also check), ghc-pkg modifies the global database by\n"++
  "  default.  Specifying --user causes it to act on the user database,\n"++
  "  or --package-db can be used to act on another database\n"++
  "  entirely. When multiple of these options are given, the rightmost\n"++
  "  one is used as the database to act upon.\n"++
  "\n"++
  "  Commands that query the package database (list, tree, latest, describe,\n"++
  "  field) operate on the list of databases specified by the flags\n"++
  "  --user, --global, and --package-db.  If none of these flags are\n"++
  "  given, the default is --global --user.\n"++
  "\n" ++
  " The following optional flags are also accepted:\n"

substProg :: String -> String -> String
substProg _ [] = []
substProg prog ('$':'p':xs) = prog ++ substProg prog xs
substProg prog (c:xs) = c : substProg prog xs

-- -----------------------------------------------------------------------------
-- Do the business

data Force = NoForce | ForceFiles | ForceAll | CannotForce
  deriving (Eq,Ord)

-- | Enum flag representing argument type
data AsPackageArg
    = AsUnitId
    | AsDefault

-- | Represents how a package may be specified by a user on the command line.
data PackageArg
    -- | A package identifier foo-0.1, or a glob foo-*
    = Id GlobPackageIdentifier
    -- | An installed package ID foo-0.1-HASH.  This is guaranteed to uniquely
    -- match a single entry in the package database.
    | IUId UnitId
    -- | A glob against the package name.  The first string is the literal
    -- glob, the second is a function which returns @True@ if the argument
    -- matches.
    | Substring String (String->Bool)

runit :: Verbosity -> [Flag] -> [String] -> IO ()
runit verbosity cli nonopts = do
  installSignalHandlers -- catch ^C and clean up
  when (verbosity >= Verbose)
    (putStr ourCopyright)
  prog <- getProgramName
  let
        force
          | FlagForce `elem` cli        = ForceAll
          | FlagForceFiles `elem` cli   = ForceFiles
          | otherwise                   = NoForce
        as_arg | FlagUnitId `elem` cli = AsUnitId
               | otherwise             = AsDefault
        multi_instance = FlagMultiInstance `elem` cli
        expand_env_vars= FlagExpandEnvVars `elem` cli
        mexpand_pkgroot= foldl' accumExpandPkgroot Nothing cli
          where accumExpandPkgroot _ FlagExpandPkgroot   = Just True
                accumExpandPkgroot _ FlagNoExpandPkgroot = Just False
                accumExpandPkgroot x _                   = x

        splitFields fields = unfoldr splitComma (',':fields)
          where splitComma "" = Nothing
                splitComma fs = Just $ break (==',') (tail fs)

        -- | Parses a glob into a predicate which tests if a string matches
        -- the glob.  Returns Nothing if the string in question is not a glob.
        -- At the moment, we only support globs at the beginning and/or end of
        -- strings.  This function respects case sensitivity.
        --
        -- >>> fromJust (substringCheck "*") "anything"
        -- True
        --
        -- >>> fromJust (substringCheck "string") "string"
        -- True
        --
        -- >>> fromJust (substringCheck "*bar") "foobar"
        -- True
        --
        -- >>> fromJust (substringCheck "foo*") "foobar"
        -- True
        --
        -- >>> fromJust (substringCheck "*ooba*") "foobar"
        -- True
        --
        -- >>> fromJust (substringCheck "f*bar") "foobar"
        -- False
        substringCheck :: String -> Maybe (String -> Bool)
        substringCheck ""    = Nothing
        substringCheck "*"   = Just (const True)
        substringCheck [_]   = Nothing
        substringCheck (h:t) =
          case (h, init t, last t) of
            ('*',s,'*') -> Just (isInfixOf (f s) . f)
            ('*',_, _ ) -> Just (isSuffixOf (f t) . f)
            ( _ ,s,'*') -> Just (isPrefixOf (f (h:s)) . f)
            _           -> Nothing
          where f | FlagIgnoreCase `elem` cli = map toLower
                  | otherwise                 = id
#if defined(GLOB)
        glob x | System.Info.os=="mingw32" = do
          -- glob echoes its argument, after win32 filename globbing
          (_,o,_,_) <- runInteractiveCommand ("glob "++x)
          txt <- hGetContents o
          return (read txt)
        glob x | otherwise = return [x]
#endif
  --
  -- first, parse the command
  case nonopts of
#if defined(GLOB)
    -- dummy command to demonstrate usage and permit testing
    -- without messing things up; use glob to selectively enable
    -- windows filename globbing for file parameters
    -- register, update, FlagGlobalConfig, FlagConfig; others?
    ["glob", filename] -> do
        print filename
        glob filename >>= print
#endif
    ["init", filename] ->
        initPackageDB filename verbosity cli
    ["register", filename] ->
        registerPackage filename verbosity cli
                        multi_instance
                        expand_env_vars False force
    ["update", filename] ->
        registerPackage filename verbosity cli
                        multi_instance
                        expand_env_vars True force
    "unregister" : pkgarg_strs@(_:_) -> do
        forM_ pkgarg_strs $ \pkgarg_str -> do
          pkgarg <- readPackageArg as_arg pkgarg_str
          unregisterPackage pkgarg verbosity cli force
    ["expose", pkgarg_str] -> do
        pkgarg <- readPackageArg as_arg pkgarg_str
        exposePackage pkgarg verbosity cli force
    ["hide",   pkgarg_str] -> do
        pkgarg <- readPackageArg as_arg pkgarg_str
        hidePackage pkgarg verbosity cli force
    ["trust",    pkgarg_str] -> do
        pkgarg <- readPackageArg as_arg pkgarg_str
        trustPackage pkgarg verbosity cli force
    ["distrust", pkgarg_str] -> do
        pkgarg <- readPackageArg as_arg pkgarg_str
        distrustPackage pkgarg verbosity cli force
    ["list"] -> do
        listPackages verbosity cli Nothing Nothing
    ["list", pkgarg_str] ->
        case substringCheck pkgarg_str of
          Nothing -> do pkgarg <- readPackageArg as_arg pkgarg_str
                        listPackages verbosity cli (Just pkgarg) Nothing
          Just m -> listPackages verbosity cli
                                 (Just (Substring pkgarg_str m)) Nothing
    ["dot"] -> do
        showPackageDot verbosity cli
    ["find-module", mod_name] -> do
        let match = maybe (==mod_name) id (substringCheck mod_name)
        listPackages verbosity cli Nothing (Just match)
    ["latest", pkgid_str] -> do
        pkgid <- readGlobPkgId pkgid_str
        latestPackage verbosity cli pkgid
    ["describe", pkgid_str] -> do
        pkgarg <- case substringCheck pkgid_str of
          Nothing -> readPackageArg as_arg pkgid_str
          Just m  -> return (Substring pkgid_str m)
        describePackage verbosity cli pkgarg (fromMaybe False mexpand_pkgroot)

    ["field", pkgid_str, fields] -> do
        pkgarg <- case substringCheck pkgid_str of
          Nothing -> readPackageArg as_arg pkgid_str
          Just m  -> return (Substring pkgid_str m)
        describeField verbosity cli pkgarg
                      (splitFields fields) (fromMaybe True mexpand_pkgroot)

    ["check"] -> do
        checkConsistency verbosity cli

    ["dump"] -> do
        dumpUnits verbosity cli (fromMaybe False mexpand_pkgroot)

    ["recache"] -> do
        recache verbosity cli

    [] -> do
        die ("missing command\n" ++ shortUsage prog)
    (_cmd:_) -> do
        die ("command-line syntax error\n" ++ shortUsage prog)

parseCheck :: Cabal.Parsec a => String -> String -> IO a
parseCheck str what =
  case Cabal.eitherParsec str of
    Left e  -> die ("cannot parse \'" ++ str ++ "\' as a " ++ what ++ ": " ++ e)
    Right x -> pure x

-- | Either an exact 'PackageIdentifier', or a glob for all packages
-- matching 'PackageName'.
data GlobPackageIdentifier
    = ExactPackageIdentifier MungedPackageId
    | GlobPackageIdentifier  MungedPackageName

displayGlobPkgId :: GlobPackageIdentifier -> String
displayGlobPkgId (ExactPackageIdentifier pid) = display pid
displayGlobPkgId (GlobPackageIdentifier pn) = display pn ++ "-*"

readGlobPkgId :: String -> IO GlobPackageIdentifier
readGlobPkgId str
  | "-*" `isSuffixOf` str =
    GlobPackageIdentifier <$> parseCheck (init (init str)) "package identifier (glob)"
  | otherwise = ExactPackageIdentifier <$> parseCheck str "package identifier (exact)"

readPackageArg :: AsPackageArg -> String -> IO PackageArg
readPackageArg AsUnitId str = IUId <$> parseCheck str "installed package id"
readPackageArg AsDefault str = Id <$> readGlobPkgId str

-- -----------------------------------------------------------------------------
-- Package databases

-- Some commands operate on a single database:
--      register, unregister, expose, hide, trust, distrust
-- however these commands also check the union of the available databases
-- in order to check consistency.  For example, register will check that
-- dependencies exist before registering a package.
--
-- Some commands operate  on multiple databases, with overlapping semantics:
--      list, describe, field

data PackageDB (mode :: GhcPkg.DbMode)
  = PackageDB {
      location, locationAbsolute :: !FilePath,
      -- We need both possibly-relative and definitely-absolute package
      -- db locations. This is because the relative location is used as
      -- an identifier for the db, so it is important we do not modify it.
      -- On the other hand we need the absolute path in a few places
      -- particularly in relation to the ${pkgroot} stuff.

      packageDbLock :: !(GhcPkg.DbOpenMode mode GhcPkg.PackageDbLock),
      -- If package db is open in read write mode, we keep its lock around for
      -- transactional updates.

      packages :: [InstalledPackageInfo]
    }

type PackageDBStack = [PackageDB 'GhcPkg.DbReadOnly]
        -- A stack of package databases.  Convention: head is the topmost
        -- in the stack.

-- | Selector for picking the right package DB to modify as 'register' and
-- 'recache' operate on the database on top of the stack, whereas 'modify'
-- changes the first database that contains a specific package.
data DbModifySelector = TopOne | ContainsPkg PackageArg

allPackagesInStack :: PackageDBStack -> [InstalledPackageInfo]
allPackagesInStack = concatMap packages

-- | Retain only the part of the stack up to and including the given package
-- DB (where the global package DB is the bottom of the stack). The resulting
-- package DB stack contains exactly the packages that packages from the
-- specified package DB can depend on, since dependencies can only extend
-- down the stack, not up (e.g. global packages cannot depend on user
-- packages).
stackUpTo :: FilePath -> PackageDBStack -> PackageDBStack
stackUpTo to_modify = dropWhile ((/= to_modify) . location)

getPkgDatabases :: Verbosity
                -> GhcPkg.DbOpenMode mode DbModifySelector
                -> Bool    -- use the user db
                -> Bool    -- read caches, if available
                -> Bool    -- expand vars, like ${pkgroot} and $topdir
                -> [Flag]
                -> IO (PackageDBStack,
                          -- the real package DB stack: [global,user] ++
                          -- DBs specified on the command line with -f.
                       GhcPkg.DbOpenMode mode (PackageDB mode),
                          -- which one to modify, if any
                       PackageDBStack)
                          -- the package DBs specified on the command
                          -- line, or [global,user] otherwise.  This
                          -- is used as the list of package DBs for
                          -- commands that just read the DB, such as 'list'.

getPkgDatabases verbosity mode use_user use_cache expand_vars my_flags = do
  -- Second we determine the location of the global package config.  On Windows,
  -- this is found relative to the ghc-pkg.exe binary, whereas on Unix the
  -- location is passed to the binary using the --global-package-db flag by the
  -- wrapper script.
  let err_msg = "missing --global-package-db option, location of global package database unknown\n"
  global_conf <-
     case [ f | FlagGlobalConfig f <- my_flags ] of
        -- See Note [Base Dir] for more information on the base dir / top dir.
        [] -> do mb_dir <- getBaseDir
                 case mb_dir of
                   Nothing  -> die err_msg
                   Just dir -> do
                     r <- lookForPackageDBIn dir
                     case r of
                       Nothing -> die ("Can't find package database in " ++ dir)
                       Just path -> return path
        fs -> return (last fs)

  -- The value of the $topdir variable used in some package descriptions
  -- Note that the way we calculate this is slightly different to how it
  -- is done in ghc itself. We rely on the convention that the global
  -- package db lives in ghc's libdir.
  top_dir <- absolutePath (takeDirectory global_conf)

  let no_user_db = FlagNoUserDb `elem` my_flags

  -- get the location of the user package database, and create it if necessary
  -- getXdgDirectory can fail (e.g. if $HOME isn't set)

  mb_user_conf <-
    case [ f | FlagUserConfig f <- my_flags ] of
      _ | no_user_db -> return Nothing
      [] -> do
        -- See Note [Settings File] about this file, and why we need GHC to share it with us.
        let settingsFile = top_dir </> "settings"
        exists_settings_file <- doesFileExist settingsFile
        targetArchOS <- case exists_settings_file of
          False -> do
            warn $ "WARNING: settings file doesn't exist " ++ show settingsFile
            warn "cannot know target platform so guessing target == host (native compiler)."
            pure hostPlatformArchOS
          True -> do
            settingsStr <- readFile settingsFile
            mySettings <- case maybeReadFuzzy settingsStr of
              Just s -> pure $ Map.fromList s
              -- It's excusable to not have a settings file (for now at
              -- least) but completely inexcusable to have a malformed one.
              Nothing -> die $ "Can't parse settings file " ++ show settingsFile
            case getTargetArchOS settingsFile mySettings of
              Right archOS -> pure archOS
              Left e -> die e
        let subdir = uniqueSubdir targetArchOS

            getFirstSuccess :: [IO a] -> IO (Maybe a)
            getFirstSuccess [] = pure Nothing
            getFirstSuccess (a:as) = tryIO a >>= \case
              Left _ -> getFirstSuccess as
              Right d -> pure (Just d)
        -- The appdir used to be in ~/.ghc but to respect the XDG specification
        -- we want to move it under $XDG_DATA_HOME/
        -- However, old tooling (like cabal) might still write package environments
        -- to the old directory, so we prefer that if a subdirectory of ~/.ghc
        -- with the correct target and GHC version exists.
        --
        -- i.e. if ~/.ghc/$UNIQUE_SUBDIR exists we prefer that
        -- otherwise we use $XDG_DATA_HOME/$UNIQUE_SUBDIR
        --
        -- UNIQUE_SUBDIR is typically a combination of the target platform and GHC version
        m_appdir <- getFirstSuccess $ map (fmap (</> subdir))
          [ getAppUserDataDirectory "ghc"  -- this is ~/.ghc/
          , getXdgDirectory XdgData "ghc"  -- this is $XDG_DATA_HOME/
          ]
        case m_appdir of
          Nothing -> return Nothing
          Just dir -> do
            lookForPackageDBIn dir >>= \case
              Nothing -> return (Just (dir </> "package.conf.d", False))
              Just f  -> return (Just (f, True))
      fs -> return (Just (last fs, True))

  -- If the user database exists, and for "use_user" commands (which includes
  -- "ghc-pkg check" and all commands that modify the db) we will attempt to
  -- use the user db.
  let sys_databases
        | Just (user_conf,user_exists) <- mb_user_conf,
          use_user || user_exists = [user_conf, global_conf]
        | otherwise               = [global_conf]

  e_pkg_path <- tryIO (System.Environment.getEnv "GHC_PACKAGE_PATH")
  let env_stack =
        case e_pkg_path of
                Left  _ -> sys_databases
                Right path
                  | not (null path) && isSearchPathSeparator (last path)
                  -> splitSearchPath (init path) ++ sys_databases
                  | otherwise
                  -> splitSearchPath path

        -- The "global" database is always the one at the bottom of the stack.
        -- This is the database we modify by default.
      virt_global_conf = last env_stack

  let db_flags = [ f | Just f <- map is_db_flag my_flags ]
         where is_db_flag FlagUser
                      | Just (user_conf, _user_exists) <- mb_user_conf
                      = Just user_conf
               is_db_flag FlagGlobal     = Just virt_global_conf
               is_db_flag (FlagConfig f) = Just f
               is_db_flag _              = Nothing

  let flag_db_names | null db_flags = env_stack
                    | otherwise     = reverse (nub db_flags)

  -- For a "modify" command, treat all the databases as
  -- a stack, where we are modifying the top one, but it
  -- can refer to packages in databases further down the
  -- stack.

  -- -f flags on the command line add to the database
  -- stack, unless any of them are present in the stack
  -- already.
  let final_stack = filter (`notElem` env_stack)
                     [ f | FlagConfig f <- reverse my_flags ]
                     ++ env_stack

      top_db = if null db_flags
               then virt_global_conf
               else last db_flags

  (db_stack, db_to_operate_on) <- getDatabases top_dir mb_user_conf
                                               flag_db_names final_stack top_db

  let flag_db_stack = [ db | db_name <- flag_db_names,
                        db <- db_stack, location db == db_name ]

  when (verbosity > Normal) $ do
    infoLn ("db stack: " ++ show (map location db_stack))
    F.forM_ db_to_operate_on $ \db ->
      infoLn ("modifying: " ++ (location db))
    infoLn ("flag db stack: " ++ show (map location flag_db_stack))

  return (db_stack, db_to_operate_on, flag_db_stack)
  where
    getDatabases top_dir mb_user_conf flag_db_names
                 final_stack top_db = case mode of
      -- When we open in read only mode, we simply read all of the databases/
      GhcPkg.DbOpenReadOnly -> do
        db_stack <- mapM readDatabase final_stack
        return (db_stack, GhcPkg.DbOpenReadOnly)

      -- The only package db we open in read write mode is the one on the top of
      -- the stack.
      GhcPkg.DbOpenReadWrite TopOne -> do
        (db_stack, mto_modify) <- stateSequence Nothing
          [ \case
              to_modify@(Just _) -> (, to_modify) <$> readDatabase db_path
              Nothing -> if db_path /= top_db
                then (, Nothing) <$> readDatabase db_path
                else do
                  db <- readParseDatabase verbosity mb_user_conf
                                          mode use_cache db_path
                    `catchException` couldntOpenDbForModification db_path
                  let ro_db = db { packageDbLock = GhcPkg.DbOpenReadOnly }
                  return (ro_db, Just db)
          | db_path <- final_stack ]

        to_modify <- case mto_modify of
          Just db -> return db
          Nothing -> die "no database selected for modification"

        return (db_stack, GhcPkg.DbOpenReadWrite to_modify)

      -- The package db we open in read write mode is the first one included in
      -- flag_db_names that contains specified package. Therefore we need to
      -- open each one in read/write mode first and decide whether it's for
      -- modification based on its contents.
      GhcPkg.DbOpenReadWrite (ContainsPkg pkgarg) -> do
        (db_stack, mto_modify) <- stateSequence Nothing
          [ \case
              to_modify@(Just _) -> (, to_modify) <$> readDatabase db_path
              Nothing -> if db_path `notElem` flag_db_names
                then (, Nothing) <$> readDatabase db_path
                else do
                  let hasPkg :: PackageDB mode -> Bool
                      hasPkg = not . null . findPackage pkgarg . packages

                      openRo (e::IOError) = do
                        db <- readDatabase db_path
                        if hasPkg db
                          then couldntOpenDbForModification db_path e
                          else return (db, Nothing)

                  -- If we fail to open the database in read/write mode, we need
                  -- to check if it's for modification first before throwing an
                  -- error, so we attempt to open it in read only mode.
                  Exception.handle openRo $ do
                    db <- readParseDatabase verbosity mb_user_conf
                                            mode use_cache db_path
                    let ro_db = db { packageDbLock = GhcPkg.DbOpenReadOnly }
                    if hasPkg db
                      then return (ro_db, Just db)
                      else do
                        -- If the database is not for modification after all,
                        -- drop the write lock as we are already finished with
                        -- the database.
                        case packageDbLock db of
                          GhcPkg.DbOpenReadWrite lock ->
                            GhcPkg.unlockPackageDb lock
                        return (ro_db, Nothing)
          | db_path <- final_stack ]

        to_modify <- case mto_modify of
          Just db -> return db
          Nothing -> cannotFindPackage pkgarg Nothing

        return (db_stack, GhcPkg.DbOpenReadWrite to_modify)
      where
        couldntOpenDbForModification :: FilePath -> IOError -> IO a
        couldntOpenDbForModification db_path e = die $ "Couldn't open database "
          ++ db_path ++ " for modification: " ++ show e

        -- Parse package db in read-only mode.
        readDatabase :: FilePath -> IO (PackageDB 'GhcPkg.DbReadOnly)
        readDatabase db_path = do
          db <- readParseDatabase verbosity mb_user_conf
                                  GhcPkg.DbOpenReadOnly use_cache db_path
          if expand_vars
            then return $ mungePackageDBPaths top_dir db
            else return db

    stateSequence :: Monad m => s -> [s -> m (a, s)] -> m ([a], s)
    stateSequence s []     = return ([], s)
    stateSequence s (m:ms) = do
      (a, s')   <- m s
      (as, s'') <- stateSequence s' ms
      return (a : as, s'')

lookForPackageDBIn :: FilePath -> IO (Maybe FilePath)
lookForPackageDBIn dir = do
  let path_dir = dir </> "package.conf.d"
  exists_dir <- doesDirectoryExist path_dir
  if exists_dir then return (Just path_dir) else do
    let path_file = dir </> "package.conf"
    exists_file <- doesFileExist path_file
    if exists_file then return (Just path_file) else return Nothing

readParseDatabase :: forall mode t. Verbosity
                  -> Maybe (FilePath,Bool)
                  -> GhcPkg.DbOpenMode mode t
                  -> Bool -- use cache
                  -> FilePath
                  -> IO (PackageDB mode)
readParseDatabase verbosity mb_user_conf mode use_cache path
  -- the user database (only) is allowed to be non-existent
  | Just (user_conf,False) <- mb_user_conf, path == user_conf
  = do lock <- F.forM mode $ \_ -> do
         createDirectoryIfMissing True path
         GhcPkg.lockPackageDb cache
       mkPackageDB [] lock
  | otherwise
  = do e <- tryIO $ getDirectoryContents path
       case e of
         Left err
           | ioeGetErrorType err == InappropriateType -> do
              -- We provide a limited degree of backwards compatibility for
              -- old single-file style db:
              mdb <- tryReadParseOldFileStyleDatabase verbosity
                       mb_user_conf mode use_cache path
              case mdb of
                Just db -> return db
                Nothing ->
                  die $ "ghc no longer supports single-file style package "
                     ++ "databases (" ++ path ++ ") use 'ghc-pkg init'"
                     ++ "to create the database with the correct format."

           | otherwise -> ioError err
         Right fs
           | not use_cache -> ignore_cache (const $ return ())
           | otherwise -> do
              e_tcache <- tryIO $ getModificationTime cache
              case e_tcache of
                Left ex -> do
                  whenReportCacheErrors $
                    if isDoesNotExistError ex
                      then
                        -- It's fine if the cache is not there as long as the
                        -- database is empty.
                        when (not $ null confs) $ do
                            warn ("WARNING: cache does not exist: " ++ cache)
                            warn ("ghc will fail to read this package db. " ++
                                  recacheAdvice)
                      else do
                        warn ("WARNING: cache cannot be read: " ++ show ex)
                        warn "ghc will fail to read this package db."
                  ignore_cache (const $ return ())
                Right tcache -> do
                  when (verbosity >= Verbose) $ do
                      warn ("Timestamp " ++ show tcache ++ " for " ++ cache)
                  -- If any of the .conf files is newer than package.cache, we
                  -- assume that cache is out of date.
                  cache_outdated <- (`anyM` confs) $ \conf ->
                    (tcache <) <$> getModificationTime conf
                  if not cache_outdated
                      then do
                          when (verbosity > Normal) $
                             infoLn ("using cache: " ++ cache)
                          GhcPkg.readPackageDbForGhcPkg cache mode
                            >>= uncurry mkPackageDB
                      else do
                          whenReportCacheErrors $ do
                              warn ("WARNING: cache is out of date: " ++ cache)
                              warn ("ghc will see an old view of this " ++
                                    "package db. " ++ recacheAdvice)
                          ignore_cache $ \file -> do
                            when (verbosity >= Verbose) $ do
                              tFile <- getModificationTime file
                              let rel = case tcache `compare` tFile of
                                    LT -> " (NEWER than cache)"
                                    GT -> " (older than cache)"
                                    EQ -> " (same as cache)"
                              warn ("Timestamp " ++ show tFile
                                ++ " for " ++ file ++ rel)
            where
                 confs = map (path </>) $ filter (".conf" `isSuffixOf`) fs

                 ignore_cache :: (FilePath -> IO ()) -> IO (PackageDB mode)
                 ignore_cache checkTime = do
                     -- If we're opening for modification, we need to acquire a
                     -- lock even if we don't open the cache now, because we are
                     -- going to modify it later.
                     lock <- F.mapM (const $ GhcPkg.lockPackageDb cache) mode
                     let doFile f = do checkTime f
                                       parseSingletonPackageConf verbosity f
                     pkgs <- mapM doFile confs
                     mkPackageDB pkgs lock

                 -- We normally report cache errors for read-only commands,
                 -- since modify commands will usually fix the cache.
                 whenReportCacheErrors = when $ verbosity > Normal
                   || verbosity >= Normal && GhcPkg.isDbOpenReadMode mode
  where
    cache = path </> cachefilename

    recacheAdvice
      | Just (user_conf, True) <- mb_user_conf, path == user_conf
      = "Use 'ghc-pkg recache --user' to fix."
      | otherwise
      = "Use 'ghc-pkg recache' to fix."

    mkPackageDB :: [InstalledPackageInfo]
                -> GhcPkg.DbOpenMode mode GhcPkg.PackageDbLock
                -> IO (PackageDB mode)
    mkPackageDB pkgs lock = do
      path_abs <- absolutePath path
      return $ PackageDB {
          location = path,
          locationAbsolute = path_abs,
          packageDbLock = lock,
          packages = pkgs
        }

parseSingletonPackageConf :: Verbosity -> FilePath -> IO InstalledPackageInfo
parseSingletonPackageConf verbosity file = do
  when (verbosity > Normal) $ infoLn ("reading package config: " ++ file)
  BS.readFile file >>= fmap fst . parsePackageInfo

cachefilename :: FilePath
cachefilename = "package.cache"

mungePackageDBPaths :: FilePath -> PackageDB mode -> PackageDB mode
mungePackageDBPaths top_dir db@PackageDB { packages = pkgs } =
    db { packages = map (mungePackagePaths top_dir pkgroot) pkgs }
  where
    pkgroot = takeDirectory $ dropTrailingPathSeparator (locationAbsolute db)
    -- It so happens that for both styles of package db ("package.conf"
    -- files and "package.conf.d" dirs) the pkgroot is the parent directory
    -- ${pkgroot}/package.conf  or  ${pkgroot}/package.conf.d/

-- | Perform path/URL variable substitution as per the Cabal ${pkgroot} spec
-- (http://www.haskell.org/pipermail/libraries/2009-May/011772.html)
-- Paths/URLs can be relative to ${pkgroot} or ${pkgrooturl}.
-- The "pkgroot" is the directory containing the package database.
--
-- Also perform a similar substitution for the older GHC-specific
-- "$topdir" variable. The "topdir" is the location of the ghc
-- installation (obtained from the -B option).
mungePackagePaths :: FilePath -> FilePath
                  -> InstalledPackageInfo -> InstalledPackageInfo
mungePackagePaths top_dir pkgroot pkg =
   -- TODO: similar code is duplicated in GHC.Unit.Database
    pkg {
      importDirs  = munge_paths (importDirs pkg),
      includeDirs = munge_paths (includeDirs pkg),
      libraryDirs = munge_paths (libraryDirs pkg),
      libraryDynDirs = munge_paths (libraryDynDirs pkg),
      frameworkDirs = munge_paths (frameworkDirs pkg),
      haddockInterfaces = munge_paths (haddockInterfaces pkg),
      -- haddock-html is allowed to be either a URL or a file
      haddockHTMLs = munge_paths (munge_urls (haddockHTMLs pkg))
    }
  where
    munge_paths = map munge_path
    munge_urls  = map munge_url
    (munge_path,munge_url) = mkMungePathUrl top_dir pkgroot

mkMungePathUrl :: FilePath -> FilePath -> (FilePath -> FilePath, FilePath -> FilePath)
mkMungePathUrl top_dir pkgroot = (munge_path, munge_url)
   where
    munge_path p
      | Just p' <- stripVarPrefix "${pkgroot}" p = pkgroot ++ p'
      | Just p' <- stripVarPrefix "$topdir"    p = top_dir ++ p'
      | otherwise                                = p

    munge_url p
      | Just p' <- stripVarPrefix "${pkgrooturl}" p = toUrlPath pkgroot p'
      | Just p' <- stripVarPrefix "$httptopdir"   p = toUrlPath top_dir p'
      | otherwise                                   = p

    toUrlPath r p = "file:///"
                 -- URLs always use posix style '/' separators:
                 ++ FilePath.Posix.joinPath
                        (r : -- We need to drop a leading "/" or "\\"
                             -- if there is one:
                             dropWhile (all isPathSeparator)
                                       (FilePath.splitDirectories p))

    -- We could drop the separator here, and then use </> above. However,
    -- by leaving it in and using ++ we keep the same path separator
    -- rather than letting FilePath change it to use \ as the separator
    stripVarPrefix var path = case stripPrefix var path of
                              Just [] -> Just []
                              Just cs@(c : _) | isPathSeparator c -> Just cs
                              _ -> Nothing

-- -----------------------------------------------------------------------------
-- Workaround for old single-file style package dbs

-- Single-file style package dbs have been deprecated for some time, but
-- it turns out that Cabal was using them in one place. So this code is for a
-- workaround to allow older Cabal versions to use this newer ghc.

-- We check if the file db contains just "[]" and if so, we look for a new
-- dir-style db in path.d/, ie in a dir next to the given file.
-- We cannot just replace the file with a new dir style since Cabal still
-- assumes it's a file and tries to overwrite with 'writeFile'.

-- ghc itself also cooperates in this workaround

tryReadParseOldFileStyleDatabase :: Verbosity -> Maybe (FilePath, Bool)
                                 -> GhcPkg.DbOpenMode mode t -> Bool -> FilePath
                                 -> IO (Maybe (PackageDB mode))
tryReadParseOldFileStyleDatabase verbosity mb_user_conf
                                 mode use_cache path = do
  -- assumes we've already established that path exists and is not a dir
  content <- readFile path `catchIO` \_ -> return ""
  if take 2 content == "[]"
    then do
      path_abs <- absolutePath path
      let path_dir = adjustOldDatabasePath path
      warn $ "Warning: ignoring old file-style db and trying " ++ path_dir
      direxists <- doesDirectoryExist path_dir
      if direxists
        then do
          db <- readParseDatabase verbosity mb_user_conf mode use_cache path_dir
          -- but pretend it was at the original location
          return $ Just db {
              location         = path,
              locationAbsolute = path_abs
            }
         else do
           lock <- F.forM mode $ \_ -> do
             createDirectoryIfMissing True path_dir
             GhcPkg.lockPackageDb $ path_dir </> cachefilename
           return $ Just PackageDB {
               location         = path,
               locationAbsolute = path_abs,
               packageDbLock    = lock,
               packages         = []
             }

    -- if the path is not a file, or is not an empty db then we fail
    else return Nothing

adjustOldFileStylePackageDB :: PackageDB mode -> IO (PackageDB mode)
adjustOldFileStylePackageDB db = do
  -- assumes we have not yet established if it's an old style or not
  mcontent <- liftM Just (readFile (location db)) `catchIO` \_ -> return Nothing
  case fmap (take 2) mcontent of
    -- it is an old style and empty db, so look for a dir kind in location.d/
    Just "[]" -> return db {
        location         = adjustOldDatabasePath $ location db,
        locationAbsolute = adjustOldDatabasePath $ locationAbsolute db
      }
    -- it is old style but not empty, we have to bail
    Just  _   -> die $ "ghc no longer supports single-file style package "
                    ++ "databases (" ++ location db ++ ") use 'ghc-pkg init'"
                    ++ "to create the database with the correct format."
    -- probably not old style, carry on as normal
    Nothing   -> return db

adjustOldDatabasePath :: FilePath -> FilePath
adjustOldDatabasePath = (<.> "d")

-- -----------------------------------------------------------------------------
-- Creating a new package DB

initPackageDB :: FilePath -> Verbosity -> [Flag] -> IO ()
initPackageDB filename verbosity _flags = do
  let eexist = die ("cannot create: " ++ filename ++ " already exists")
  b1 <- doesFileExist filename
  when b1 eexist
  b2 <- doesDirectoryExist filename
  when b2 eexist
  createDirectoryIfMissing True filename
  lock <- GhcPkg.lockPackageDb $ filename </> cachefilename
  filename_abs <- absolutePath filename
  changeDB verbosity [] PackageDB {
      location = filename,
      locationAbsolute = filename_abs,
      packageDbLock = GhcPkg.DbOpenReadWrite lock,
      packages = []
    }
    -- We can get away with passing an empty stack here, because the new DB is
    -- going to be initially empty, so no dependencies are going to be actually
    -- looked up.
    []

-- -----------------------------------------------------------------------------
-- Registering

registerPackage :: FilePath
                -> Verbosity
                -> [Flag]
                -> Bool              -- multi_instance
                -> Bool              -- expand_env_vars
                -> Bool              -- update
                -> Force
                -> IO ()
registerPackage input verbosity my_flags multi_instance
                expand_env_vars update force = do
  (db_stack, GhcPkg.DbOpenReadWrite db_to_operate_on, _flag_dbs) <-
    getPkgDatabases verbosity (GhcPkg.DbOpenReadWrite TopOne)
      True{-use user-} True{-use cache-} False{-expand vars-} my_flags

  let to_modify = location db_to_operate_on

  s <-
    case input of
      "-" -> do
        when (verbosity >= Normal) $
            info "Reading package info from stdin ... "
        -- fix the encoding to UTF-8, since this is an interchange format
        hSetEncoding stdin utf8
        getContents
      f   -> do
        when (verbosity >= Normal) $
            info ("Reading package info from " ++ show f ++ " ... ")
        readUTF8File f

  expanded <- if expand_env_vars then expandEnvVars s force
                                 else return s

  (pkg, ws) <- parsePackageInfo $ toUTF8BS expanded
  when (verbosity >= Normal) $
      infoLn "done."

  -- report any warnings from the parse phase
  _ <- reportValidateErrors verbosity [] ws
         (display (mungedId pkg) ++ ": Warning: ") Nothing

  -- validate the expanded pkg, but register the unexpanded
  pkgroot <- absolutePath (takeDirectory to_modify)
  let top_dir = takeDirectory (location (last db_stack))
      pkg_expanded = mungePackagePaths top_dir pkgroot pkg

  let truncated_stack = stackUpTo to_modify db_stack
  -- truncate the stack for validation, because we don't allow
  -- packages lower in the stack to refer to those higher up.
  validatePackageConfig pkg_expanded verbosity truncated_stack
                        multi_instance update force

  let
     -- In the normal mode, we only allow one version of each package, so we
     -- remove all instances with the same source package id as the one we're
     -- adding. In the multi instance mode we don't do that, thus allowing
     -- multiple instances with the same source package id.
     removes = [ RemovePackage p
               | not multi_instance,
                 p <- packages db_to_operate_on,
                 mungedId p == mungedId pkg,
                 -- Only remove things that were instantiated the same way!
                 instantiatedWith p == instantiatedWith pkg ]
  --
  changeDB verbosity (removes ++ [AddPackage pkg]) db_to_operate_on db_stack

parsePackageInfo
        :: BS.ByteString
        -> IO (InstalledPackageInfo, [ValidateWarning])
parsePackageInfo str =
  case parseInstalledPackageInfo str of
    Right (warnings, ok) -> pure (mungePackageInfo ok, ws)
      where
        ws = [ msg | msg <- warnings
                   , not ("Unrecognized field pkgroot" `isPrefixOf` msg) ]
    Left err -> die (unlines (F.toList err))

mungePackageInfo :: InstalledPackageInfo -> InstalledPackageInfo
mungePackageInfo ipi = ipi

-- -----------------------------------------------------------------------------
-- Making changes to a package database

data DBOp = RemovePackage InstalledPackageInfo
          | AddPackage    InstalledPackageInfo
          | ModifyPackage InstalledPackageInfo

changeDB :: Verbosity
         -> [DBOp]
         -> PackageDB 'GhcPkg.DbReadWrite
         -> PackageDBStack
         -> IO ()
changeDB verbosity cmds db db_stack = do
  let db' = updateInternalDB db cmds
  db'' <- adjustOldFileStylePackageDB db'
  createDirectoryIfMissing True (location db'')
  changeDBDir verbosity cmds db'' db_stack

updateInternalDB :: PackageDB 'GhcPkg.DbReadWrite
                 -> [DBOp] -> PackageDB 'GhcPkg.DbReadWrite
updateInternalDB db cmds = db{ packages = foldl do_cmd (packages db) cmds }
 where
  do_cmd pkgs (RemovePackage p) =
    filter ((/= installedUnitId p) . installedUnitId) pkgs
  do_cmd pkgs (AddPackage p) = p : pkgs
  do_cmd pkgs (ModifyPackage p) =
    do_cmd (do_cmd pkgs (RemovePackage p)) (AddPackage p)


changeDBDir :: Verbosity
            -> [DBOp]
            -> PackageDB 'GhcPkg.DbReadWrite
            -> PackageDBStack
            -> IO ()
changeDBDir verbosity cmds db db_stack = do
  mapM_ do_cmd cmds
  updateDBCache verbosity db db_stack
 where
  do_cmd (RemovePackage p) = do
    let file = location db </> display (installedUnitId p) <.> "conf"
    when (verbosity > Normal) $ infoLn ("removing " ++ file)
    removeFileSafe file
  do_cmd (AddPackage p) = do
    let file = location db </> display (installedUnitId p) <.> "conf"
    when (verbosity > Normal) $ infoLn ("writing " ++ file)
    writeUTF8File file (showInstalledPackageInfo p)
  do_cmd (ModifyPackage p) =
    do_cmd (AddPackage p)

updateDBCache :: Verbosity
              -> PackageDB 'GhcPkg.DbReadWrite
              -> PackageDBStack
              -> IO ()
updateDBCache verbosity db db_stack = do
  let filename = location db </> cachefilename
      db_stack_below = stackUpTo (location db) db_stack

      pkgsCabalFormat :: [InstalledPackageInfo]
      pkgsCabalFormat = packages db

      -- | All the packages we can legally depend on in this step.
      dependablePkgsCabalFormat :: [InstalledPackageInfo]
      dependablePkgsCabalFormat = allPackagesInStack db_stack_below

      pkgsGhcCacheFormat :: [(PackageCacheFormat, Bool)]
      pkgsGhcCacheFormat
        -- See Note [Recompute abi-depends]
        = map (recomputeValidAbiDeps dependablePkgsCabalFormat)
        $ map convertPackageInfoToCacheFormat
          pkgsCabalFormat

      hasAnyAbiDepends :: InstalledPackageInfo -> Bool
      hasAnyAbiDepends x = length (abiDepends x) > 0

  -- warn when we find any (possibly-)bogus abi-depends fields;
  -- Note [Recompute abi-depends]
  when (verbosity >= Normal) $ do
    let definitelyBrokenPackages =
          nub
            . sort
            . map (unPackageName . GhcPkg.unitPackageName . fst)
            . filter snd
            $ pkgsGhcCacheFormat
    when (definitelyBrokenPackages /= []) $ do
      warn "the following packages have broken abi-depends fields:"
      forM_ definitelyBrokenPackages $ \pkg ->
        warn $ "    " ++ pkg
    when (verbosity > Normal) $ do
      let possiblyBrokenPackages =
            nub
              . sort
              . filter (not . (`elem` definitelyBrokenPackages))
              . map (unPackageName . pkgName . packageId)
              . filter hasAnyAbiDepends
              $ pkgsCabalFormat
      when (possiblyBrokenPackages /= []) $ do
          warn $
            "the following packages have correct abi-depends, " ++
            "but may break in the future:"
          forM_ possiblyBrokenPackages $ \pkg ->
            warn $ "    " ++ pkg

  when (verbosity > Normal) $
      infoLn ("writing cache " ++ filename)

  let d = fmap (fromPackageCacheFormat . fst) pkgsGhcCacheFormat
  GhcPkg.writePackageDb filename d pkgsCabalFormat
    `catchIO` \e ->
      if isPermissionError e
      then die $ filename ++ ": you don't have permission to modify this file"
      else ioError e

  case packageDbLock db of
    GhcPkg.DbOpenReadWrite lock -> GhcPkg.unlockPackageDb lock

type PackageCacheFormat = GhcPkg.GenericUnitInfo
                            PackageIdentifier
                            PackageName
                            UnitId
                            ModuleName
                            OpenModule

{- Note [Recompute abi-depends]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Like most fields, `ghc-pkg` relies on who-ever is performing package
registration to fill in fields; this includes the `abi-depends` field present
for the package.

However, this was likely a mistake, and is not very robust; in certain cases,
versions of Cabal may use bogus abi-depends fields for a package when doing
builds. Why? Because package database information is aggressively cached; it is
possible to work Cabal into a situation where it uses a cached version of
`abi-depends`, rather than the one in the actual database after it has been
recomputed.

However, there is an easy fix: ghc-pkg /already/ knows the `abi-depends` of a
package, because they are the ABIs of the packages pointed at by the `depends`
field. So it can simply look up the abi from the dependencies in the original
database, and ignore whatever the system registering gave it.

So, instead, we do two things here:

  - We throw away the information for a registered package's `abi-depends` field.

  - We recompute it: we simply look up the unit ID of the package in the original
    database, and use *its* abi-depends.

See #14381, and Cabal issue #4728.

Additionally, because we are throwing away the original (declared) ABI deps, we
return a boolean that indicates whether any abi-depends were actually
overridden.

-}

recomputeValidAbiDeps :: [InstalledPackageInfo]
                      -> PackageCacheFormat
                      -> (PackageCacheFormat, Bool)
recomputeValidAbiDeps db pkg =
  (pkg { GhcPkg.unitAbiDepends = newAbiDeps }, abiDepsUpdated)
  where
    newAbiDeps =
      catMaybes . flip map (GhcPkg.unitAbiDepends pkg) $ \(k, _) ->
        case filter (\d -> installedUnitId d == k) db of
          [x] -> Just (k, ST.pack $ unAbiHash (abiHash x))
          _   -> Nothing
    abiDepsUpdated =
      GhcPkg.unitAbiDepends pkg /= newAbiDeps


-- | Convert from PackageCacheFormat to DbUnitInfo (the format used in
-- Ghc.PackageDb to store into the database)
fromPackageCacheFormat :: PackageCacheFormat -> GhcPkg.DbUnitInfo
fromPackageCacheFormat = GhcPkg.mapGenericUnitInfo
     mkUnitId' mkPackageIdentifier' mkPackageName' mkModuleName' mkModule'
   where
     displayBS :: Pretty a => a -> BS.ByteString
     displayBS            = toUTF8BS . display
     mkPackageIdentifier' = displayBS
     mkPackageName'       = displayBS
     mkComponentId'       = displayBS
     mkUnitId'            = displayBS
     mkModuleName'        = displayBS
     mkInstUnitId' i      = case i of
       IndefFullUnitId cid insts -> DbInstUnitId (mkComponentId' cid)
                                                 (fmap (bimap mkModuleName' mkModule') (Map.toList insts))
       DefiniteUnitId uid        -> DbUnitId (mkUnitId' (unDefUnitId uid))
     mkModule' m = case m of
       OpenModule uid n -> DbModule (mkInstUnitId' uid) (mkModuleName' n)
       OpenModuleVar n  -> DbModuleVar  (mkModuleName' n)

convertPackageInfoToCacheFormat :: InstalledPackageInfo -> PackageCacheFormat
convertPackageInfoToCacheFormat pkg =
    GhcPkg.GenericUnitInfo {
       GhcPkg.unitId             = installedUnitId pkg,
       GhcPkg.unitInstanceOf     = mkUnitId (unComponentId (installedComponentId pkg)),
       GhcPkg.unitInstantiations = instantiatedWith pkg,
       GhcPkg.unitPackageId      = sourcePackageId pkg,
       GhcPkg.unitPackageName    = packageName pkg,
       GhcPkg.unitPackageVersion = Version.Version (versionNumbers (packageVersion pkg)) [],
       GhcPkg.unitComponentName  =
         fmap (mkPackageName . unUnqualComponentName) (libraryNameString $ sourceLibName pkg),
       GhcPkg.unitDepends        = depends pkg,
       GhcPkg.unitAbiDepends     = map (\(AbiDependency k v) -> (k,ST.pack $ unAbiHash v)) (abiDepends pkg),
       GhcPkg.unitAbiHash        = ST.pack $ unAbiHash (abiHash pkg),
       GhcPkg.unitImportDirs     = map ST.pack $ importDirs pkg,
       GhcPkg.unitLibraries      = map ST.pack $ hsLibraries pkg,
       GhcPkg.unitExtDepLibsSys  = map ST.pack $ extraLibraries pkg,
       GhcPkg.unitExtDepLibsGhc  = map ST.pack $ extraGHCiLibraries pkg,
       GhcPkg.unitLibraryDirs    = map ST.pack $ libraryDirs pkg,
       GhcPkg.unitLibraryDynDirs = map ST.pack $ libraryDynDirs pkg,
       GhcPkg.unitExtDepFrameworks = map ST.pack $ frameworks pkg,
       GhcPkg.unitExtDepFrameworkDirs = map ST.pack $ frameworkDirs pkg,
       GhcPkg.unitLinkerOptions  = map ST.pack $ ldOptions pkg,
       GhcPkg.unitCcOptions      = map ST.pack $ ccOptions pkg,
       GhcPkg.unitIncludes       = map ST.pack $ includes pkg,
       GhcPkg.unitIncludeDirs    = map ST.pack $ includeDirs pkg,
       GhcPkg.unitHaddockInterfaces = map ST.pack $ haddockInterfaces pkg,
       GhcPkg.unitHaddockHTMLs   = map ST.pack $ haddockHTMLs pkg,
       GhcPkg.unitExposedModules = map convertExposed (exposedModules pkg),
       GhcPkg.unitHiddenModules  = hiddenModules pkg,
       GhcPkg.unitIsIndefinite   = indefinite pkg,
       GhcPkg.unitIsExposed      = exposed pkg,
       GhcPkg.unitIsTrusted      = trusted pkg
    }
  where
    convertExposed (ExposedModule n reexport) = (n, reexport)

-- -----------------------------------------------------------------------------
-- Exposing, Hiding, Trusting, Distrusting, Unregistering are all similar

exposePackage :: PackageArg -> Verbosity -> [Flag] -> Force -> IO ()
exposePackage = modifyPackage (\p -> ModifyPackage p{exposed=True})

hidePackage :: PackageArg -> Verbosity -> [Flag] -> Force -> IO ()
hidePackage = modifyPackage (\p -> ModifyPackage p{exposed=False})

trustPackage :: PackageArg -> Verbosity -> [Flag] -> Force -> IO ()
trustPackage = modifyPackage (\p -> ModifyPackage p{trusted=True})

distrustPackage :: PackageArg -> Verbosity -> [Flag] -> Force -> IO ()
distrustPackage = modifyPackage (\p -> ModifyPackage p{trusted=False})

unregisterPackage :: PackageArg -> Verbosity -> [Flag] -> Force -> IO ()
unregisterPackage = modifyPackage RemovePackage

modifyPackage
  :: (InstalledPackageInfo -> DBOp)
  -> PackageArg
  -> Verbosity
  -> [Flag]
  -> Force
  -> IO ()
modifyPackage fn pkgarg verbosity my_flags force = do
  (db_stack, GhcPkg.DbOpenReadWrite db, _flag_dbs) <-
    getPkgDatabases verbosity (GhcPkg.DbOpenReadWrite $ ContainsPkg pkgarg)
      True{-use user-} True{-use cache-} False{-expand vars-} my_flags

  let db_name = location db
      pkgs    = packages db

      -- Get package respecting flags...
      ps = findPackage pkgarg pkgs

  -- This shouldn't happen if getPkgDatabases picks the DB correctly.
  when (null ps) $ cannotFindPackage pkgarg $ Just db

  let pks = map installedUnitId ps

      cmds = [ fn pkg | pkg <- pkgs, installedUnitId pkg `elem` pks ]
      new_db = updateInternalDB db cmds
      new_db_ro = new_db { packageDbLock = GhcPkg.DbOpenReadOnly }

      -- ...but do consistency checks with regards to the full stack
      old_broken = brokenPackages (allPackagesInStack db_stack)
      rest_of_stack = filter ((/= db_name) . location) db_stack
      new_stack = new_db_ro : rest_of_stack
      new_broken = brokenPackages (allPackagesInStack new_stack)
      newly_broken = filter ((`notElem` map installedUnitId old_broken)
                            . installedUnitId) new_broken
  --
  let displayQualPkgId pkg
        | [_] <- filter ((== pkgid) . mungedId)
                        (allPackagesInStack db_stack)
            = display pkgid
        | otherwise = display pkgid ++ "@" ++ display (installedUnitId pkg)
        where pkgid = mungedId pkg
  when (not (null newly_broken)) $
      dieOrForceAll force ("unregistering would break the following packages: "
              ++ unwords (map displayQualPkgId newly_broken))

  changeDB verbosity cmds db db_stack

recache :: Verbosity -> [Flag] -> IO ()
recache verbosity my_flags = do
  (_db_stack, GhcPkg.DbOpenReadWrite db_to_operate_on, _flag_dbs) <-
    getPkgDatabases verbosity (GhcPkg.DbOpenReadWrite TopOne)
      True{-use user-} False{-no cache-} False{-expand vars-} my_flags
  changeDB verbosity [] db_to_operate_on _db_stack

-- -----------------------------------------------------------------------------
-- Listing packages

listPackages ::  Verbosity -> [Flag] -> Maybe PackageArg
             -> Maybe (String->Bool)
             -> IO ()
listPackages verbosity my_flags mPackageName mModuleName = do
  let simple_output = FlagSimpleOutput `elem` my_flags
  (db_stack, GhcPkg.DbOpenReadOnly, flag_db_stack) <-
    getPkgDatabases verbosity GhcPkg.DbOpenReadOnly
      False{-use user-} True{-use cache-} False{-expand vars-} my_flags

  let db_stack_filtered -- if a package is given, filter out all other packages
        | Just this <- mPackageName =
            [ db{ packages = filter (this `matchesPkg`) (packages db) }
            | db <- flag_db_stack ]
        | Just match <- mModuleName = -- packages which expose mModuleName
            [ db{ packages = filter (match `exposedInPkg`) (packages db) }
            | db <- flag_db_stack ]
        | otherwise = flag_db_stack

      db_stack_sorted
          = [ db{ packages = sort_pkgs (packages db) }
            | db <- db_stack_filtered ]
          where sort_pkgs = sortBy cmpPkgIds
                cmpPkgIds pkg1 pkg2 =
                   case mungedName p1 `compare` mungedName p2 of
                        LT -> LT
                        GT -> GT
                        EQ -> case mungedVersion p1 `compare` mungedVersion p2 of
                                LT -> LT
                                GT -> GT
                                EQ -> installedUnitId pkg1 `compare` installedUnitId pkg2
                   where (p1,p2) = (mungedId pkg1, mungedId pkg2)

      stack = reverse db_stack_sorted

      match `exposedInPkg` pkg = any match (map display $ exposedModules pkg)

      pkg_map = allPackagesInStack db_stack
      broken = map installedUnitId (brokenPackages pkg_map)

      show_normal PackageDB{ location = db_name, packages = pkg_confs } =
          do hPutStrLn stdout db_name
             if null pkg_confs
                 then hPutStrLn stdout "    (no packages)"
                 else hPutStrLn stdout $ unlines (map ("    " ++) (map pp_pkg pkg_confs))
           where
                 pp_pkg p
                   | installedUnitId p `elem` broken = printf "{%s}" doc
                   | exposed p = doc
                   | otherwise = printf "(%s)" doc
                   where doc | verbosity >= Verbose = printf "%s (%s)" pkg (display (installedUnitId p))
                             | otherwise            = pkg
                          where
                          pkg = display (mungedId p)

      show_simple = simplePackageList my_flags . allPackagesInStack

  when (not (null broken) && not simple_output && verbosity /= Silent) $ do
     prog <- getProgramName
     warn ("WARNING: there are broken packages.  Run '" ++ prog ++ " check' for more details.")

  if simple_output then show_simple stack else do

#if !defined(WITH_TERMINFO)
    mapM_ show_normal stack
#else
    let
       show_colour withF db@PackageDB{ packages = pkg_confs } =
           if null pkg_confs
           then termText (location db) <#> termText "\n    (no packages)\n"
           else
               mconcat $ map (<#> termText "\n") $
                           (termText (location db)
                            : map (termText "    " <#>) (map pp_pkg pkg_confs))
          where
                   pp_pkg p
                     | installedUnitId p `elem` broken = withF Red  doc
                     | exposed p                       = doc
                     | otherwise                       = withF Blue doc
                     where doc | verbosity >= Verbose
                               = termText (printf "%s (%s)" pkg (display (installedUnitId p)))
                               | otherwise
                               = termText pkg
                            where
                            pkg = display (mungedId p)

    is_tty <- hIsTerminalDevice stdout
    if not is_tty
       then mapM_ show_normal stack
       else do tty <- Terminfo.setupTermFromEnv
               case Terminfo.getCapability tty withForegroundColor of
                   Nothing -> mapM_ show_normal stack
                   Just w  -> runTermOutput tty $ mconcat $
                                                  map (show_colour w) stack
#endif

simplePackageList :: [Flag] -> [InstalledPackageInfo] -> IO ()
simplePackageList my_flags pkgs = do
   let showPkg :: InstalledPackageInfo -> String
       showPkg | FlagShowUnitIds `elem` my_flags = display . installedUnitId
               | FlagNamesOnly `elem` my_flags   = display . mungedName . mungedId
               | otherwise                       = display . mungedId
       strs = map showPkg pkgs
   when (not (null pkgs)) $
      hPutStrLn stdout $ concat $ intersperse " " strs

showPackageDot :: Verbosity -> [Flag] -> IO ()
showPackageDot verbosity myflags = do
  (_, GhcPkg.DbOpenReadOnly, flag_db_stack) <-
    getPkgDatabases verbosity GhcPkg.DbOpenReadOnly
      False{-use user-} True{-use cache-} False{-expand vars-} myflags

  let all_pkgs = allPackagesInStack flag_db_stack
      ipix  = PackageIndex.fromList all_pkgs

  putStrLn "digraph {"
  let quote s = '"':s ++ "\""
  mapM_ putStrLn [ quote from ++ " -> " ++ quote to
                 | p <- all_pkgs,
                   let from = display (mungedId p),
                   key <- depends p,
                   Just dep <- [PackageIndex.lookupUnitId ipix key],
                   let to = display (mungedId dep)
                 ]
  putStrLn "}"

-- -----------------------------------------------------------------------------
-- Prints the highest (hidden or exposed) version of a package

-- ToDo: This is no longer well-defined with unit ids, because the
-- dependencies may be varying versions
latestPackage ::  Verbosity -> [Flag] -> GlobPackageIdentifier -> IO ()
latestPackage verbosity my_flags pkgid = do
  (_, GhcPkg.DbOpenReadOnly, flag_db_stack) <-
    getPkgDatabases verbosity GhcPkg.DbOpenReadOnly
      False{-use user-} True{-use cache-} False{-expand vars-} my_flags

  ps <- findPackages flag_db_stack (Id pkgid)
  case ps of
    [] -> die "no matches"
    _  -> show_pkg . maximum . map mungedId $ ps
  where
    show_pkg pid = hPutStrLn stdout (display pid)

-- -----------------------------------------------------------------------------
-- Describe

describePackage :: Verbosity -> [Flag] -> PackageArg -> Bool -> IO ()
describePackage verbosity my_flags pkgarg expand_pkgroot = do
  (_, GhcPkg.DbOpenReadOnly, flag_db_stack) <-
    getPkgDatabases verbosity GhcPkg.DbOpenReadOnly
      False{-use user-} True{-use cache-} expand_pkgroot my_flags
  dbs <- findPackagesByDB flag_db_stack pkgarg
  doDump expand_pkgroot [ (pkg, locationAbsolute db)
                        | (db, pkgs) <- dbs, pkg <- pkgs ]

dumpUnits :: Verbosity -> [Flag] -> Bool -> IO ()
dumpUnits verbosity my_flags expand_pkgroot = do
  (_, GhcPkg.DbOpenReadOnly, flag_db_stack) <-
    getPkgDatabases verbosity GhcPkg.DbOpenReadOnly
      False{-use user-} True{-use cache-} expand_pkgroot my_flags
  doDump expand_pkgroot [ (pkg, locationAbsolute db)
                        | db <- flag_db_stack, pkg <- packages db ]

doDump :: Bool -> [(InstalledPackageInfo, FilePath)] -> IO ()
doDump expand_pkgroot pkgs = do
  -- fix the encoding to UTF-8, since this is an interchange format
  hSetEncoding stdout utf8
  putStrLn $
    intercalate "---\n"
    [ if expand_pkgroot
        then showInstalledPackageInfo pkg
        else showInstalledPackageInfo pkg ++ pkgrootField
    | (pkg, pkgloc) <- pkgs
    , let pkgroot      = takeDirectory pkgloc
          pkgrootField = "pkgroot: " ++ show pkgroot ++ "\n" ]

-- PackageId is can have globVersion for the version
findPackages :: PackageDBStack -> PackageArg -> IO [InstalledPackageInfo]
findPackages db_stack pkgarg
  = fmap (concatMap snd) $ findPackagesByDB db_stack pkgarg

findPackage :: PackageArg -> [InstalledPackageInfo] -> [InstalledPackageInfo]
findPackage pkgarg pkgs = filter (pkgarg `matchesPkg`) pkgs

findPackagesByDB :: PackageDBStack -> PackageArg
                 -> IO [(PackageDB 'GhcPkg.DbReadOnly, [InstalledPackageInfo])]
findPackagesByDB db_stack pkgarg
  = case [ (db, matched)
         | db <- db_stack,
           let matched = findPackage pkgarg $ packages db,
           not (null matched) ] of
        [] -> cannotFindPackage pkgarg Nothing
        ps -> return ps

cannotFindPackage :: PackageArg -> Maybe (PackageDB mode) -> IO a
cannotFindPackage pkgarg mdb = die $ "cannot find package " ++ pkg_msg pkgarg
  ++ maybe "" (\db -> " in " ++ location db) mdb
  where
    pkg_msg (Id pkgid)           = displayGlobPkgId pkgid
    pkg_msg (IUId ipid)          = display ipid
    pkg_msg (Substring pkgpat _) = "matching " ++ pkgpat

matches :: GlobPackageIdentifier -> MungedPackageId -> Bool
GlobPackageIdentifier pn `matches` pid'
  = (pn == mungedName pid')
ExactPackageIdentifier pid `matches` pid'
  = mungedName pid == mungedName pid' &&
    (mungedVersion pid == mungedVersion pid' || mungedVersion pid == nullVersion)

matchesPkg :: PackageArg -> InstalledPackageInfo -> Bool
(Id pid)        `matchesPkg` pkg = pid `matches` mungedId pkg
(IUId ipid)     `matchesPkg` pkg = ipid == installedUnitId pkg
(Substring _ m) `matchesPkg` pkg = m (display (mungedId pkg))

-- -----------------------------------------------------------------------------
-- Field

describeField :: Verbosity -> [Flag] -> PackageArg -> [String] -> Bool -> IO ()
describeField verbosity my_flags pkgarg fields expand_pkgroot = do
  (_, GhcPkg.DbOpenReadOnly, flag_db_stack) <-
    getPkgDatabases verbosity GhcPkg.DbOpenReadOnly
      False{-use user-} True{-use cache-} expand_pkgroot my_flags
  fns <- mapM toField fields
  ps <- findPackages flag_db_stack pkgarg
  mapM_ (selectFields fns) ps
  where showFun = if FlagSimpleOutput `elem` my_flags
                  then showSimpleInstalledPackageInfoField
                  else showInstalledPackageInfoField
        toField f = case showFun f of
                    Nothing -> die ("unknown field: " ++ f)
                    Just fn -> return fn
        selectFields fns pinfo = mapM_ (\fn->putStrLn (fn pinfo)) fns


-- -----------------------------------------------------------------------------
-- Check: Check consistency of installed packages

checkConsistency :: Verbosity -> [Flag] -> IO ()
checkConsistency verbosity my_flags = do
  (db_stack, GhcPkg.DbOpenReadOnly, _) <-
    getPkgDatabases verbosity GhcPkg.DbOpenReadOnly
      True{-use user-} True{-use cache-} True{-expand vars-} my_flags
      -- although check is not a modify command, we do need to use the user
      -- db, because we may need it to verify package deps.

  let simple_output = FlagSimpleOutput `elem` my_flags
  let unitid_output = FlagShowUnitIds `elem` my_flags

  let pkgs = allPackagesInStack db_stack

      checkPackage :: InstalledPackageInfo -> IO [InstalledPackageInfo]
      checkPackage p = do
         (_,es,ws) <- runValidate $ checkPackageConfig p verbosity db_stack
                                                       True True
         if null es
            then do
              when (not simple_output) $ do
                  _ <- reportValidateErrors verbosity [] ws "" Nothing
                  return ()
              return []
            else do
              when (not simple_output) $ do
                  reportError ("There are problems in package " ++ display (mungedId p) ++ ":")
                  _ <- reportValidateErrors verbosity es ws "  " Nothing
                  return ()
              return [p]

  broken_pkgs <- concat `fmap` mapM checkPackage pkgs

  let filterOut pkgs1 pkgs2 = filter not_in pkgs2
        where not_in p = mungedId p `notElem` all_ps
              all_ps = map mungedId pkgs1

  let not_broken_pkgs = filterOut broken_pkgs pkgs
      (_, trans_broken_pkgs) = closure [] not_broken_pkgs

      all_broken_pkgs :: [InstalledPackageInfo]
      all_broken_pkgs = broken_pkgs ++ trans_broken_pkgs

  when (not (null all_broken_pkgs)) $ do
    if simple_output
      then simplePackageList my_flags all_broken_pkgs
      else do
        let disp :: InstalledPackageInfo -> String
            disp | unitid_output = display . installedUnitId
                 | otherwise     = display . mungedId
        reportError ("\nThe following packages are broken, either because they have a problem\n"++
                "listed above, or because they depend on a broken package.")
        mapM_ (hPutStrLn stderr . disp) all_broken_pkgs

  when (not (null all_broken_pkgs)) $ exitWith (ExitFailure 1)


closure :: [InstalledPackageInfo] -> [InstalledPackageInfo]
        -> ([InstalledPackageInfo], [InstalledPackageInfo])
closure pkgs db_stack = go pkgs db_stack
 where
   go avail not_avail =
     case partition (depsAvailable avail) not_avail of
        ([],        not_avail') -> (avail, not_avail')
        (new_avail, not_avail') -> go (new_avail ++ avail) not_avail'

   depsAvailable :: [InstalledPackageInfo] -> InstalledPackageInfo
                 -> Bool
   depsAvailable pkgs_ok pkg = null dangling
        where dangling = filter (`notElem` pids) (depends pkg)
              pids = map installedUnitId pkgs_ok

        -- we want mutually recursive groups of package to show up
        -- as broken. (#1750)

brokenPackages :: [InstalledPackageInfo] -> [InstalledPackageInfo]
brokenPackages pkgs = snd (closure [] pkgs)

-----------------------------------------------------------------------------
-- Sanity-check a new package config, and automatically build GHCi libs
-- if requested.

type ValidateError   = (Force,String)
type ValidateWarning = String

newtype Validate a = V { runValidate :: IO (a, [ValidateError],[ValidateWarning]) }

instance Functor Validate where
    fmap = liftM

instance Applicative Validate where
    pure a = V $ pure (a, [], [])
    (<*>) = ap

instance Monad Validate where
   m >>= k = V $ do
      (a, es, ws) <- runValidate m
      (b, es', ws') <- runValidate (k a)
      return (b,es++es',ws++ws')

verror :: Force -> String -> Validate ()
verror f s = V (return ((),[(f,s)],[]))

vwarn :: String -> Validate ()
vwarn s = V (return ((),[],["Warning: " ++ s]))

liftIO :: IO a -> Validate a
liftIO k = V (k >>= \a -> return (a,[],[]))

-- returns False if we should die
reportValidateErrors :: Verbosity -> [ValidateError] -> [ValidateWarning]
                     -> String -> Maybe Force -> IO Bool
reportValidateErrors verbosity es ws prefix mb_force = do
  when (verbosity >= Normal) $ mapM_ (warn . (prefix++)) ws
  oks <- mapM report es
  return (and oks)
  where
    report (f,s)
      | Just force <- mb_force
      = if (force >= f)
           then do when (verbosity >= Normal) $
                        reportError (prefix ++ s ++ " (ignoring)")
                   return True
           else if f < CannotForce
                   then do reportError (prefix ++ s ++ " (use --force to override)")
                           return False
                   else do reportError err
                           return False
      | otherwise = do reportError err
                       return False
      where
             err = prefix ++ s

validatePackageConfig :: InstalledPackageInfo
                      -> Verbosity
                      -> PackageDBStack
                      -> Bool   -- multi_instance
                      -> Bool   -- update, or check
                      -> Force
                      -> IO ()
validatePackageConfig pkg verbosity db_stack
                      multi_instance update force = do
  (_,es,ws) <- runValidate $
                 checkPackageConfig pkg verbosity db_stack
                                    multi_instance update
  ok <- reportValidateErrors verbosity es ws
          (display (mungedId pkg) ++ ": ") (Just force)
  when (not ok) $ exitWith (ExitFailure 1)

checkPackageConfig :: InstalledPackageInfo
                      -> Verbosity
                      -> PackageDBStack
                      -> Bool   -- multi_instance
                      -> Bool   -- update, or check
                      -> Validate ()
checkPackageConfig pkg verbosity db_stack
                   multi_instance update = do
  checkPackageId pkg
  checkUnitId pkg db_stack update
  checkDuplicates db_stack pkg multi_instance update
  mapM_ (checkDep db_stack) (depends pkg)
  checkDuplicateDepends (depends pkg)
  mapM_ (checkDir False "import-dirs")  (importDirs pkg)
  mapM_ (checkDir True  "library-dirs") (libraryDirs pkg)
  mapM_ (checkDir True  "dynamic-library-dirs") (libraryDynDirs pkg)
  mapM_ (checkDir True  "include-dirs") (includeDirs pkg)
  mapM_ (checkDir True  "framework-dirs") (frameworkDirs pkg)
  mapM_ (checkFile   True "haddock-interfaces") (haddockInterfaces pkg)
  mapM_ (checkDirURL True "haddock-html")       (haddockHTMLs pkg)
  checkDuplicateModules pkg
  checkExposedModules db_stack pkg
  checkOtherModules pkg
  let has_code = Set.null (openModuleSubstFreeHoles (Map.fromList (instantiatedWith pkg)))
  when has_code $ mapM_ (checkHSLib verbosity (libraryDirs pkg ++ libraryDynDirs pkg)) (hsLibraries pkg)
  -- ToDo: check these somehow?
  --    extra_libraries :: [String],
  --    c_includes      :: [String],

-- When the package name and version are put together, sometimes we can
-- end up with a package id that cannot be parsed.  This will lead to
-- difficulties when the user wants to refer to the package later, so
-- we check that the package id can be parsed properly here.
checkPackageId :: InstalledPackageInfo -> Validate ()
checkPackageId ipi =
  let str = display (mungedId ipi) in
  case Cabal.eitherParsec str :: Either String MungedPackageId of
    Left e -> verror CannotForce ("invalid package identifier: '" ++ str ++ "': " ++ e)
    Right _ -> pure ()

checkUnitId :: InstalledPackageInfo -> PackageDBStack -> Bool
                -> Validate ()
checkUnitId ipi db_stack update = do
  let uid = installedUnitId ipi
  when (null (display uid)) $ verror CannotForce "missing id field"
  when (display uid /= compatPackageKey ipi) $
    verror CannotForce $ "installed package info from too old version of Cabal "
                      ++ "(key field does not match id field)"
  let dups = [ p | p <- allPackagesInStack db_stack,
                   installedUnitId p == uid ]
  when (not update && not (null dups)) $
    verror CannotForce $
        "package(s) with this id already exist: " ++
         unwords (map (display.installedUnitId) dups)

checkDuplicates :: PackageDBStack -> InstalledPackageInfo
                -> Bool -> Bool-> Validate ()
checkDuplicates db_stack pkg multi_instance update = do
  let
        pkgid = mungedId pkg
        pkgs  = packages (head db_stack)
  --
  -- Check whether this package id already exists in this DB
  --
  when (not update && not multi_instance
                   && (pkgid `elem` map mungedId pkgs)) $
       verror CannotForce $
          "package " ++ display pkgid ++ " is already installed"

  let
        uncasep = map toLower . display
        dups = filter ((== uncasep pkgid) . uncasep) (map mungedId pkgs)

  when (not update && not multi_instance
                   && not (null dups)) $ verror ForceAll $
        "Package names may be treated case-insensitively in the future.\n"++
        "Package " ++ display pkgid ++
        " overlaps with: " ++ unwords (map display dups)

checkDir, checkFile, checkDirURL :: Bool -> String -> FilePath -> Validate ()
checkDir  = checkPath False True
checkFile = checkPath False False
checkDirURL = checkPath True True

checkPath :: Bool -> Bool -> Bool -> String -> FilePath -> Validate ()
checkPath url_ok is_dir warn_only thisfield d
 | url_ok && ("http://"  `isPrefixOf` d
           || "https://" `isPrefixOf` d) = return ()

 | url_ok
 , Just d' <- stripPrefix "file://" d
 = checkPath False is_dir warn_only thisfield d'

   -- Note: we don't check for $topdir/${pkgroot} here. We rely on these
   -- variables having been expanded already, see mungePackagePaths.

 | isRelative d = verror ForceFiles $
                     thisfield ++ ": " ++ d ++ " is a relative path which "
                  ++ "makes no sense (as there is nothing for it to be "
                  ++ "relative to). You can make paths relative to the "
                  ++ "package database itself by using ${pkgroot}."
        -- relative paths don't make any sense; #4134
 | otherwise = do
   there <- liftIO $ if is_dir then doesDirectoryExist d else doesFileExist d
   when (not there) $
       let msg = thisfield ++ ": " ++ d ++ " doesn't exist or isn't a "
                                        ++ if is_dir then "directory" else "file"
       in
       if warn_only
          then vwarn msg
          else verror ForceFiles msg

checkDep :: PackageDBStack -> UnitId -> Validate ()
checkDep db_stack pkgid
  | pkgid `elem` pkgids = return ()
  | otherwise = verror ForceAll ("dependency \"" ++ display pkgid
                                 ++ "\" doesn't exist")
  where
        all_pkgs = allPackagesInStack db_stack
        pkgids = map installedUnitId all_pkgs

checkDuplicateDepends :: [UnitId] -> Validate ()
checkDuplicateDepends deps
  | null dups = return ()
  | otherwise = verror ForceAll ("package has duplicate dependencies: " ++
                                     unwords (map display dups))
  where
       dups = [ p | (p:_:_) <- group (sort deps) ]

checkHSLib :: Verbosity -> [String] -> String -> Validate ()
checkHSLib _verbosity dirs lib = do
  let filenames = ["lib" ++ lib ++ ".a",
                   "lib" ++ lib ++ "_p.a",
                   "lib" ++ lib ++ "-ghc" ++ GHC.Version.cProjectVersion ++ ".so",
                   "lib" ++ lib ++ "-ghc" ++ GHC.Version.cProjectVersion ++ ".dylib",
                            lib ++ "-ghc" ++ GHC.Version.cProjectVersion ++ ".dll"]
  b <- liftIO $ doesFileExistOnPath filenames dirs
  when (not b) $
    verror ForceFiles ("cannot find any of " ++ show filenames ++
                       " on library path")

doesFileExistOnPath :: [FilePath] -> [FilePath] -> IO Bool
doesFileExistOnPath filenames paths = anyM doesFileExist fullFilenames
  where fullFilenames = [ path </> filename
                        | filename <- filenames
                        , path <- paths ]

-- | Perform validation checks (module file existence checks) on the
-- @hidden-modules@ field.
checkOtherModules :: InstalledPackageInfo -> Validate ()
checkOtherModules pkg = mapM_ (checkModuleFile pkg) (hiddenModules pkg)

-- | Perform validation checks (module file existence checks and module
-- reexport checks) on the @exposed-modules@ field.
checkExposedModules :: PackageDBStack -> InstalledPackageInfo -> Validate ()
checkExposedModules db_stack pkg =
  mapM_ checkExposedModule (exposedModules pkg)
  where
    checkExposedModule (ExposedModule modl reexport) = do
      let checkOriginal = checkModuleFile pkg modl
          checkReexport = checkModule "module reexport" db_stack pkg
      maybe checkOriginal checkReexport reexport

-- | Validates the existence of an appropriate @hi@ file associated with
-- a module.  Used for both @hidden-modules@ and @exposed-modules@ which
-- are not reexports.
checkModuleFile :: InstalledPackageInfo -> ModuleName -> Validate ()
checkModuleFile pkg modl =
      -- there's no interface file for GHC.Prim
      unless (modl == ModuleName.fromString "GHC.Prim") $ do
      let files = [ ModuleName.toFilePath modl <.> extension
                  | extension <- ["hi", "p_hi", "dyn_hi" ] ]
      b <- liftIO $ doesFileExistOnPath files (importDirs pkg)
      when (not b) $
         verror ForceFiles ("cannot find any of " ++ show files)

-- | Validates that @exposed-modules@ and @hidden-modules@ do not have duplicate
-- entries.
-- ToDo: this needs updating for signatures: signatures can validly show up
-- multiple times in the @exposed-modules@ list as long as their backing
-- implementations agree.
checkDuplicateModules :: InstalledPackageInfo -> Validate ()
checkDuplicateModules pkg
  | null dups = return ()
  | otherwise = verror ForceAll ("package has duplicate modules: " ++
                                     unwords (map display dups))
  where
    dups = [ m | (m:_:_) <- group (sort mods) ]
    mods = map exposedName (exposedModules pkg) ++ hiddenModules pkg

-- | Validates an original module entry, either the origin of a module reexport
-- or the backing implementation of a signature, by checking that it exists,
-- really is an original definition, and is accessible from the dependencies of
-- the package.
-- ToDo: If the original module in question is a backing signature
-- implementation, then we should also check that the original module in
-- question is NOT a signature (however, if it is a reexport, then it's fine
-- for the original module to be a signature.)
checkModule :: String
            -> PackageDBStack
            -> InstalledPackageInfo
            -> OpenModule
            -> Validate ()
checkModule _ _ _ (OpenModuleVar _) = error "Impermissible reexport"
checkModule field_name db_stack pkg
    (OpenModule (DefiniteUnitId def_uid) definingModule) =
  let definingPkgId = unDefUnitId def_uid
      mpkg = if definingPkgId == installedUnitId pkg
              then Just pkg
              else PackageIndex.lookupUnitId ipix definingPkgId
  in case mpkg of
      Nothing
           -> verror ForceAll (field_name ++ " refers to a non-existent " ++
                               "defining package: " ++
                                       display definingPkgId)

      Just definingPkg
        | not (isIndirectDependency definingPkgId)
           -> verror ForceAll (field_name ++ " refers to a defining " ++
                               "package that is not a direct (or indirect) " ++
                               "dependency of this package: " ++
                                       display definingPkgId)

        | otherwise
        -> case find ((==definingModule).exposedName)
                     (exposedModules definingPkg) of
            Nothing ->
              verror ForceAll (field_name ++ " refers to a module " ++
                               display definingModule ++ " " ++
                               "that is not exposed in the " ++
                               "defining package " ++ display definingPkgId)
            Just (ExposedModule {exposedReexport = Just _} ) ->
              verror ForceAll (field_name ++ " refers to a module " ++
                               display definingModule ++ " " ++
                               "that is reexported but not defined in the " ++
                               "defining package " ++ display definingPkgId)
            _ -> return ()
  where
    all_pkgs = allPackagesInStack db_stack
    ipix     = PackageIndex.fromList all_pkgs

    isIndirectDependency pkgid = fromMaybe False $ do
      thispkg  <- graphVertex (installedUnitId pkg)
      otherpkg <- graphVertex pkgid
      return (Graph.path depgraph thispkg otherpkg)
    (depgraph, _, graphVertex) =
      PackageIndex.dependencyGraph (PackageIndex.insert pkg ipix)

checkModule _ _ _ (OpenModule (IndefFullUnitId _ _) _) =
    -- TODO: add some checks here
    return ()


-- ---------------------------------------------------------------------------
-- expanding environment variables in the package configuration

expandEnvVars :: String -> Force -> IO String
expandEnvVars str0 force = go str0 ""
 where
   go "" acc = return $! reverse acc
   go ('$':'{':str) acc | (var, '}':rest) <- break close str
        = do value <- lookupEnvVar var
             go rest (reverse value ++ acc)
        where close c = c == '}' || c == '\n' -- don't span newlines
   go (c:str) acc
        = go str (c:acc)

   lookupEnvVar :: String -> IO String
   lookupEnvVar "pkgroot"    = return "${pkgroot}"    -- these two are special,
   lookupEnvVar "pkgrooturl" = return "${pkgrooturl}" -- we don't expand them
   lookupEnvVar nm =
        catchIO (System.Environment.getEnv nm)
           (\ _ -> do dieOrForceAll force ("Unable to expand variable " ++
                                        show nm)
                      return "")

-----------------------------------------------------------------------------

getProgramName :: IO String
getProgramName = liftM (`withoutSuffix` ".bin") getProgName
   where str `withoutSuffix` suff
            | suff `isSuffixOf` str = take (length str - length suff) str
            | otherwise             = str

bye :: String -> IO a
bye s = putStr s >> exitWith ExitSuccess

die :: String -> IO a
die = dieWith 1

dieWith :: Int -> String -> IO a
dieWith ec s = do
  prog <- getProgramName
  reportError (prog ++ ": " ++ s)
  exitWith (ExitFailure ec)

dieOrForceAll :: Force -> String -> IO ()
dieOrForceAll ForceAll s = ignoreError s
dieOrForceAll _other s   = dieForcible s

warn :: String -> IO ()
warn = reportError

-- send info messages to stdout
infoLn :: String -> IO ()
infoLn = putStrLn

info :: String -> IO ()
info = putStr

ignoreError :: String -> IO ()
ignoreError s = reportError (s ++ " (ignoring)")

reportError :: String -> IO ()
reportError s = do hFlush stdout; hPutStrLn stderr s

dieForcible :: String -> IO ()
dieForcible s = die (s ++ " (use --force to override)")

-----------------------------------------
-- Adapted from ghc/compiler/utils/Panic

installSignalHandlers :: IO ()
installSignalHandlers = do
  threadid <- myThreadId
  let
      interrupt = Exception.throwTo threadid
                                    (Exception.ErrorCall "interrupted")
  --
#if !defined(mingw32_HOST_OS)
  _ <- installHandler sigQUIT (Catch interrupt) Nothing
  _ <- installHandler sigINT  (Catch interrupt) Nothing
  return ()
#else
  -- GHC 6.3+ has support for console events on Windows
  -- NOTE: running GHCi under a bash shell for some reason requires
  -- you to press Ctrl-Break rather than Ctrl-C to provoke
  -- an interrupt.  Ctrl-C is getting blocked somewhere, I don't know
  -- why --SDM 17/12/2004
  let sig_handler ControlC = interrupt
      sig_handler Break    = interrupt
      sig_handler _        = return ()

  _ <- installHandler (Catch sig_handler)
  return ()
#endif

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = catchException

tryIO :: IO a -> IO (Either Exception.IOException a)
tryIO = Exception.try

-- removeFileSave doesn't throw an exceptions, if the file is already deleted
removeFileSafe :: FilePath -> IO ()
removeFileSafe fn =
  removeFile fn `catchIO` \ e ->
    when (not $ isDoesNotExistError e) $ ioError e

-- | Turn a path relative to the current directory into a (normalised)
-- absolute path.
absolutePath :: FilePath -> IO FilePath
absolutePath path = return . normalise . (</> path) =<< getCurrentDirectory
