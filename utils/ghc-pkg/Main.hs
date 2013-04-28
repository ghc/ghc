{-# LANGUAGE PatternGuards, CPP, ForeignFunctionInterface #-}
-----------------------------------------------------------------------------
--
-- (c) The University of Glasgow 2004-2009.
--
-- Package management tool
--
-----------------------------------------------------------------------------

module Main (main) where

import Version ( version, targetOS, targetARCH )
import Distribution.InstalledPackageInfo.Binary()
import qualified Distribution.Simple.PackageIndex as PackageIndex
import Distribution.ModuleName hiding (main)
import Distribution.InstalledPackageInfo
import Distribution.Compat.ReadP
import Distribution.ParseUtils
import Distribution.Package hiding (depends)
import Distribution.Text
import Distribution.Version
import System.FilePath as FilePath
import qualified System.FilePath.Posix as FilePath.Posix
import System.Process
import System.Directory ( getAppUserDataDirectory, createDirectoryIfMissing,
                          getModificationTime )
import Text.Printf

import Prelude

import System.Console.GetOpt
import qualified Control.Exception as Exception
import Data.Maybe

import Data.Char ( isSpace, toLower )
import Control.Monad
import System.Directory ( doesDirectoryExist, getDirectoryContents,
                          doesFileExist, renameFile, removeFile,
                          getCurrentDirectory )
import System.Exit ( exitWith, ExitCode(..) )
import System.Environment ( getArgs, getProgName, getEnv )
import System.IO
import System.IO.Error
import Data.List
import Control.Concurrent

import qualified Data.ByteString.Lazy as B
import qualified Data.Binary as Bin
import qualified Data.Binary.Get as Bin

#if defined(mingw32_HOST_OS)
-- mingw32 needs these for getExecDir
import Foreign
import Foreign.C
#endif

#ifdef mingw32_HOST_OS
import GHC.ConsoleHandler
#else
import System.Posix hiding (fdToHandle)
#endif

#if defined(GLOB)
import qualified System.Info(os)
#endif

#if !defined(mingw32_HOST_OS) && !defined(BOOTSTRAPPING)
import System.Console.Terminfo as Terminfo
#endif

#ifdef mingw32_HOST_OS
# if defined(i386_HOST_ARCH)
#  define WINDOWS_CCONV stdcall
# elif defined(x86_64_HOST_ARCH)
#  define WINDOWS_CCONV ccall
# else
#  error Unknown mingw32 arch
# endif
#endif

-- -----------------------------------------------------------------------------
-- Entry point

main :: IO ()
main = do
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
  | FlagForce
  | FlagForceFiles
  | FlagAutoGHCiLibs
  | FlagExpandEnvVars
  | FlagExpandPkgroot
  | FlagNoExpandPkgroot
  | FlagSimpleOutput
  | FlagNamesOnly
  | FlagIgnoreCase
  | FlagNoUserDb
  | FlagVerbosity (Maybe String)
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
  Option [] ["no-user-package-conf"] (NoArg FlagNoUserDb)
        "never read the user package database (DEPRECATED)",
  Option [] ["force"] (NoArg FlagForce)
         "ignore missing dependencies, directories, and libraries",
  Option [] ["force-files"] (NoArg FlagForceFiles)
         "ignore missing directories and libraries only",
  Option ['g'] ["auto-ghci-libs"] (NoArg FlagAutoGHCiLibs)
        "automatically build libs for GHCi (with register)",
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
  Option [] ["names-only"] (NoArg FlagNamesOnly)
        "only print package names, not versions; can only be used with list --simple-output",
  Option [] ["ignore-case"] (NoArg FlagIgnoreCase)
        "ignore case for substring matching",
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
ourCopyright = "GHC package manager version " ++ Version.version ++ "\n"

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
  "  $p unregister {pkg-id}\n" ++
  "    Unregister the specified package.\n" ++
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
  "    for input for the graphviz tools.  For example, to generate a PDF" ++
  "    of the dependency graph: ghc-pkg dot | tred | dot -Tpdf >pkgs.pdf" ++
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
  " open substring ends (prefix*, *suffix, *infix*).\n" ++
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

data PackageArg = Id PackageIdentifier | Substring String (String->Bool)

runit :: Verbosity -> [Flag] -> [String] -> IO ()
runit verbosity cli nonopts = do
  installSignalHandlers -- catch ^C and clean up
  prog <- getProgramName
  let
        force
          | FlagForce `elem` cli        = ForceAll
          | FlagForceFiles `elem` cli   = ForceFiles
          | otherwise                   = NoForce
        auto_ghci_libs = FlagAutoGHCiLibs `elem` cli
        expand_env_vars= FlagExpandEnvVars `elem` cli
        mexpand_pkgroot= foldl' accumExpandPkgroot Nothing cli
          where accumExpandPkgroot _ FlagExpandPkgroot   = Just True
                accumExpandPkgroot _ FlagNoExpandPkgroot = Just False
                accumExpandPkgroot x _                   = x
                
        splitFields fields = unfoldr splitComma (',':fields)
          where splitComma "" = Nothing
                splitComma fs = Just $ break (==',') (tail fs)

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
                        auto_ghci_libs expand_env_vars False force
    ["update", filename] ->
        registerPackage filename verbosity cli
                        auto_ghci_libs expand_env_vars True force
    ["unregister", pkgid_str] -> do
        pkgid <- readGlobPkgId pkgid_str
        unregisterPackage pkgid verbosity cli force
    ["expose", pkgid_str] -> do
        pkgid <- readGlobPkgId pkgid_str
        exposePackage pkgid verbosity cli force
    ["hide",   pkgid_str] -> do
        pkgid <- readGlobPkgId pkgid_str
        hidePackage pkgid verbosity cli force
    ["trust",    pkgid_str] -> do
        pkgid <- readGlobPkgId pkgid_str
        trustPackage pkgid verbosity cli force
    ["distrust", pkgid_str] -> do
        pkgid <- readGlobPkgId pkgid_str
        distrustPackage pkgid verbosity cli force
    ["list"] -> do
        listPackages verbosity cli Nothing Nothing
    ["list", pkgid_str] ->
        case substringCheck pkgid_str of
          Nothing -> do pkgid <- readGlobPkgId pkgid_str
                        listPackages verbosity cli (Just (Id pkgid)) Nothing
          Just m -> listPackages verbosity cli (Just (Substring pkgid_str m)) Nothing
    ["dot"] -> do
        showPackageDot verbosity cli
    ["find-module", moduleName] -> do
        let match = maybe (==moduleName) id (substringCheck moduleName)
        listPackages verbosity cli Nothing (Just match)
    ["latest", pkgid_str] -> do
        pkgid <- readGlobPkgId pkgid_str
        latestPackage verbosity cli pkgid
    ["describe", pkgid_str] -> do
        pkgarg <- case substringCheck pkgid_str of
          Nothing -> liftM Id (readGlobPkgId pkgid_str)
          Just m  -> return (Substring pkgid_str m)
        describePackage verbosity cli pkgarg (fromMaybe False mexpand_pkgroot)
        
    ["field", pkgid_str, fields] -> do
        pkgarg <- case substringCheck pkgid_str of
          Nothing -> liftM Id (readGlobPkgId pkgid_str)
          Just m  -> return (Substring pkgid_str m)
        describeField verbosity cli pkgarg
                      (splitFields fields) (fromMaybe True mexpand_pkgroot)

    ["check"] -> do
        checkConsistency verbosity cli

    ["dump"] -> do
        dumpPackages verbosity cli (fromMaybe False mexpand_pkgroot)

    ["recache"] -> do
        recache verbosity cli

    [] -> do
        die ("missing command\n" ++ shortUsage prog)
    (_cmd:_) -> do
        die ("command-line syntax error\n" ++ shortUsage prog)

parseCheck :: ReadP a a -> String -> String -> IO a
parseCheck parser str what =
  case [ x | (x,ys) <- readP_to_S parser str, all isSpace ys ] of
    [x] -> return x
    _ -> die ("cannot parse \'" ++ str ++ "\' as a " ++ what)

readGlobPkgId :: String -> IO PackageIdentifier
readGlobPkgId str = parseCheck parseGlobPackageId str "package identifier"

parseGlobPackageId :: ReadP r PackageIdentifier
parseGlobPackageId =
  parse
     +++
  (do n <- parse
      _ <- string "-*"
      return (PackageIdentifier{ pkgName = n, pkgVersion = globVersion }))

-- globVersion means "all versions"
globVersion :: Version
globVersion = Version{ versionBranch=[], versionTags=["*"] }

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

data PackageDB 
  = PackageDB {
      location, locationAbsolute :: !FilePath,
      -- We need both possibly-relative and definately-absolute package
      -- db locations. This is because the relative location is used as
      -- an identifier for the db, so it is important we do not modify it.
      -- On the other hand we need the absolute path in a few places
      -- particularly in relation to the ${pkgroot} stuff.
      
      packages :: [InstalledPackageInfo]
    }

type PackageDBStack = [PackageDB]
        -- A stack of package databases.  Convention: head is the topmost
        -- in the stack.

allPackagesInStack :: PackageDBStack -> [InstalledPackageInfo]
allPackagesInStack = concatMap packages

getPkgDatabases :: Verbosity
                -> Bool    -- we are modifying, not reading
                -> Bool    -- read caches, if available
                -> Bool    -- expand vars, like ${pkgroot} and $topdir
                -> [Flag]
                -> IO (PackageDBStack, 
                          -- the real package DB stack: [global,user] ++ 
                          -- DBs specified on the command line with -f.
                       Maybe FilePath,
                          -- which one to modify, if any
                       PackageDBStack)
                          -- the package DBs specified on the command
                          -- line, or [global,user] otherwise.  This
                          -- is used as the list of package DBs for
                          -- commands that just read the DB, such as 'list'.

getPkgDatabases verbosity modify use_cache expand_vars my_flags = do
  -- first we determine the location of the global package config.  On Windows,
  -- this is found relative to the ghc-pkg.exe binary, whereas on Unix the
  -- location is passed to the binary using the --global-package-db flag by the
  -- wrapper script.
  let err_msg = "missing --global-package-db option, location of global package database unknown\n"
  global_conf <-
     case [ f | FlagGlobalConfig f <- my_flags ] of
        [] -> do mb_dir <- getLibDir
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
  -- getAppUserDataDirectory can fail (e.g. if $HOME isn't set)
  e_appdir <- tryIO $ getAppUserDataDirectory "ghc"

  mb_user_conf <-
     if no_user_db then return Nothing else
     case e_appdir of
       Left _    -> return Nothing
       Right appdir -> do
         let subdir = targetARCH ++ '-':targetOS ++ '-':Version.version
             dir = appdir </> subdir
         r <- lookForPackageDBIn dir
         case r of
           Nothing -> return (Just (dir </> "package.conf.d", False))
           Just f  -> return (Just (f, True))

  -- If the user database doesn't exist, and this command isn't a
  -- "modify" command, then we won't attempt to create or use it.
  let sys_databases
        | Just (user_conf,user_exists) <- mb_user_conf,
          modify || user_exists = [user_conf, global_conf]
        | otherwise             = [global_conf]

  e_pkg_path <- tryIO (System.Environment.getEnv "GHC_PACKAGE_PATH")
  let env_stack =
        case e_pkg_path of
                Left  _ -> sys_databases
                Right path
                  | last cs == ""  -> init cs ++ sys_databases
                  | otherwise      -> cs
                  where cs = parseSearchPath path

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

  -- the database we actually modify is the one mentioned
  -- rightmost on the command-line.
  let to_modify
        | not modify    = Nothing
        | null db_flags = Just virt_global_conf
        | otherwise     = Just (last db_flags)

  db_stack  <- sequence
    [ do db <- readParseDatabase verbosity mb_user_conf use_cache db_path
         if expand_vars then return (mungePackageDBPaths top_dir db)
                        else return db
    | db_path <- final_stack ]

  let flag_db_stack = [ db | db_name <- flag_db_names,
                        db <- db_stack, location db == db_name ]

  return (db_stack, to_modify, flag_db_stack)


lookForPackageDBIn :: FilePath -> IO (Maybe FilePath)
lookForPackageDBIn dir = do
  let path_dir = dir </> "package.conf.d"
  exists_dir <- doesDirectoryExist path_dir
  if exists_dir then return (Just path_dir) else do
  let path_file = dir </> "package.conf"
  exists_file <- doesFileExist path_file
  if exists_file then return (Just path_file) else return Nothing

readParseDatabase :: Verbosity
                  -> Maybe (FilePath,Bool)
                  -> Bool -- use cache
                  -> FilePath
                  -> IO PackageDB

readParseDatabase verbosity mb_user_conf use_cache path
  -- the user database (only) is allowed to be non-existent
  | Just (user_conf,False) <- mb_user_conf, path == user_conf
  = mkPackageDB []
  | otherwise
  = do e <- tryIO $ getDirectoryContents path
       case e of
         Left _   -> do
              pkgs <- parseMultiPackageConf verbosity path
              mkPackageDB pkgs
         Right fs
           | not use_cache -> ignore_cache
           | otherwise -> do
              let cache = path </> cachefilename
              tdir     <- getModificationTime path
              e_tcache <- tryIO $ getModificationTime cache
              case e_tcache of
                Left ex -> do
                     when (verbosity > Normal) $
                        warn ("warning: cannot read cache file " ++ cache ++ ": " ++ show ex)
                     ignore_cache
                Right tcache
                  | tcache >= tdir -> do
                     when (verbosity > Normal) $
                        infoLn ("using cache: " ++ cache)
                     pkgs <- myReadBinPackageDB cache
                     let pkgs' = map convertPackageInfoIn pkgs
                     mkPackageDB pkgs'
                  | otherwise -> do
                     when (verbosity >= Normal) $ do
                        warn ("WARNING: cache is out of date: " ++ cache)
                        warn "  use 'ghc-pkg recache' to fix."
                     ignore_cache
            where
                 ignore_cache = do
                     let confs = filter (".conf" `isSuffixOf`) fs
                     pkgs <- mapM (parseSingletonPackageConf verbosity) $
                                   map (path </>) confs
                     mkPackageDB pkgs
  where
    mkPackageDB pkgs = do
      path_abs <- absolutePath path
      return PackageDB {
        location = path,
        locationAbsolute = path_abs,
        packages = pkgs
      }

-- read the package.cache file strictly, to work around a problem with
-- bytestring 0.9.0.x (fixed in 0.9.1.x) where the file wasn't closed
-- after it has been completely read, leading to a sharing violation
-- later.
myReadBinPackageDB :: FilePath -> IO [InstalledPackageInfoString]
myReadBinPackageDB filepath = do
  h <- openBinaryFile filepath ReadMode
  sz <- hFileSize h
  b <- B.hGet h (fromIntegral sz)
  hClose h
  return $ Bin.runGet Bin.get b

parseMultiPackageConf :: Verbosity -> FilePath -> IO [InstalledPackageInfo]
parseMultiPackageConf verbosity file = do
  when (verbosity > Normal) $ infoLn ("reading package database: " ++ file)
  str <- readUTF8File file
  let pkgs = map convertPackageInfoIn $ read str
  Exception.evaluate pkgs
    `catchError` \e->
       die ("error while parsing " ++ file ++ ": " ++ show e)
  
parseSingletonPackageConf :: Verbosity -> FilePath -> IO InstalledPackageInfo
parseSingletonPackageConf verbosity file = do
  when (verbosity > Normal) $ infoLn ("reading package config: " ++ file)
  readUTF8File file >>= fmap fst . parsePackageInfo

cachefilename :: FilePath
cachefilename = "package.cache"

mungePackageDBPaths :: FilePath -> PackageDB -> PackageDB
mungePackageDBPaths top_dir db@PackageDB { packages = pkgs } =
    db { packages = map (mungePackagePaths top_dir pkgroot) pkgs }
  where
    pkgroot = takeDirectory (locationAbsolute db)    
    -- It so happens that for both styles of package db ("package.conf"
    -- files and "package.conf.d" dirs) the pkgroot is the parent directory
    -- ${pkgroot}/package.conf  or  ${pkgroot}/package.conf.d/

-- TODO: This code is duplicated in compiler/main/Packages.lhs
mungePackagePaths :: FilePath -> FilePath
                  -> InstalledPackageInfo -> InstalledPackageInfo
-- Perform path/URL variable substitution as per the Cabal ${pkgroot} spec
-- (http://www.haskell.org/pipermail/libraries/2009-May/011772.html)
-- Paths/URLs can be relative to ${pkgroot} or ${pkgrooturl}.
-- The "pkgroot" is the directory containing the package database.
--
-- Also perform a similar substitution for the older GHC-specific
-- "$topdir" variable. The "topdir" is the location of the ghc
-- installation (obtained from the -B option).
mungePackagePaths top_dir pkgroot pkg =
    pkg {
      importDirs  = munge_paths (importDirs pkg),
      includeDirs = munge_paths (includeDirs pkg),
      libraryDirs = munge_paths (libraryDirs pkg),
      frameworkDirs = munge_paths (frameworkDirs pkg),
      haddockInterfaces = munge_paths (haddockInterfaces pkg),
                     -- haddock-html is allowed to be either a URL or a file
      haddockHTMLs = munge_paths (munge_urls (haddockHTMLs pkg))
    }
  where
    munge_paths = map munge_path
    munge_urls  = map munge_url

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
-- Creating a new package DB

initPackageDB :: FilePath -> Verbosity -> [Flag] -> IO ()
initPackageDB filename verbosity _flags = do
  let eexist = die ("cannot create: " ++ filename ++ " already exists")
  b1 <- doesFileExist filename
  when b1 eexist
  b2 <- doesDirectoryExist filename
  when b2 eexist
  filename_abs <- absolutePath filename
  changeDB verbosity [] PackageDB {
                          location = filename, locationAbsolute = filename_abs,
                          packages = []
                        }

-- -----------------------------------------------------------------------------
-- Registering

registerPackage :: FilePath
                -> Verbosity
                -> [Flag]
                -> Bool              -- auto_ghci_libs
                -> Bool              -- expand_env_vars
                -> Bool              -- update
                -> Force
                -> IO ()
registerPackage input verbosity my_flags auto_ghci_libs expand_env_vars update force = do
  (db_stack, Just to_modify, _flag_dbs) <- 
      getPkgDatabases verbosity True True False{-expand vars-} my_flags

  let
        db_to_operate_on = my_head "register" $
                           filter ((== to_modify).location) db_stack
  --
  when (auto_ghci_libs && verbosity >= Silent) $
    warn "Warning: --auto-ghci-libs is deprecated and will be removed in GHC 7.4"
  --
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

  (pkg, ws) <- parsePackageInfo expanded
  when (verbosity >= Normal) $
      infoLn "done."

  -- report any warnings from the parse phase
  _ <- reportValidateErrors [] ws
         (display (sourcePackageId pkg) ++ ": Warning: ") Nothing

  -- validate the expanded pkg, but register the unexpanded
  pkgroot <- absolutePath (takeDirectory to_modify)
  let top_dir = takeDirectory (location (last db_stack))
      pkg_expanded = mungePackagePaths top_dir pkgroot pkg

  let truncated_stack = dropWhile ((/= to_modify).location) db_stack
  -- truncate the stack for validation, because we don't allow
  -- packages lower in the stack to refer to those higher up.
  validatePackageConfig pkg_expanded verbosity truncated_stack auto_ghci_libs update force
  let 
     removes = [ RemovePackage p
               | p <- packages db_to_operate_on,
                 sourcePackageId p == sourcePackageId pkg ]
  --
  changeDB verbosity (removes ++ [AddPackage pkg]) db_to_operate_on

parsePackageInfo
        :: String
        -> IO (InstalledPackageInfo, [ValidateWarning])
parsePackageInfo str =
  case parseInstalledPackageInfo str of
    ParseOk warnings ok -> return (ok, ws)
      where
        ws = [ msg | PWarning msg <- warnings
                   , not ("Unrecognized field pkgroot" `isPrefixOf` msg) ]
    ParseFailed err -> case locatedErrorMsg err of
                           (Nothing, s) -> die s
                           (Just l, s) -> die (show l ++ ": " ++ s)

-- -----------------------------------------------------------------------------
-- Making changes to a package database

data DBOp = RemovePackage InstalledPackageInfo
          | AddPackage    InstalledPackageInfo
          | ModifyPackage InstalledPackageInfo

changeDB :: Verbosity -> [DBOp] -> PackageDB -> IO ()
changeDB verbosity cmds db = do
  let db' = updateInternalDB db cmds
  isfile <- doesFileExist (location db)
  if isfile
     then writeNewConfig verbosity (location db') (packages db')
     else do
       createDirectoryIfMissing True (location db)
       changeDBDir verbosity cmds db'

updateInternalDB :: PackageDB -> [DBOp] -> PackageDB
updateInternalDB db cmds = db{ packages = foldl do_cmd (packages db) cmds }
 where
  do_cmd pkgs (RemovePackage p) = 
    filter ((/= installedPackageId p) . installedPackageId) pkgs
  do_cmd pkgs (AddPackage p) = p : pkgs
  do_cmd pkgs (ModifyPackage p) = 
    do_cmd (do_cmd pkgs (RemovePackage p)) (AddPackage p)
    

changeDBDir :: Verbosity -> [DBOp] -> PackageDB -> IO ()
changeDBDir verbosity cmds db = do
  mapM_ do_cmd cmds
  updateDBCache verbosity db
 where
  do_cmd (RemovePackage p) = do
    let file = location db </> display (installedPackageId p) <.> "conf"
    when (verbosity > Normal) $ infoLn ("removing " ++ file)
    removeFileSafe file
  do_cmd (AddPackage p) = do
    let file = location db </> display (installedPackageId p) <.> "conf"
    when (verbosity > Normal) $ infoLn ("writing " ++ file)
    writeFileUtf8Atomic file (showInstalledPackageInfo p)
  do_cmd (ModifyPackage p) = 
    do_cmd (AddPackage p)

updateDBCache :: Verbosity -> PackageDB -> IO ()
updateDBCache verbosity db = do
  let filename = location db </> cachefilename
  when (verbosity > Normal) $
      infoLn ("writing cache " ++ filename)
  writeBinaryFileAtomic filename (map convertPackageInfoOut (packages db))
    `catchIO` \e ->
      if isPermissionError e
      then die (filename ++ ": you don't have permission to modify this file")
      else ioError e

-- -----------------------------------------------------------------------------
-- Exposing, Hiding, Trusting, Distrusting, Unregistering are all similar

exposePackage :: PackageIdentifier -> Verbosity -> [Flag] -> Force -> IO ()
exposePackage = modifyPackage (\p -> ModifyPackage p{exposed=True})

hidePackage :: PackageIdentifier -> Verbosity -> [Flag] -> Force -> IO ()
hidePackage = modifyPackage (\p -> ModifyPackage p{exposed=False})

trustPackage :: PackageIdentifier -> Verbosity -> [Flag] -> Force -> IO ()
trustPackage = modifyPackage (\p -> ModifyPackage p{trusted=True})

distrustPackage :: PackageIdentifier -> Verbosity -> [Flag] -> Force -> IO ()
distrustPackage = modifyPackage (\p -> ModifyPackage p{trusted=False})

unregisterPackage :: PackageIdentifier -> Verbosity -> [Flag] -> Force -> IO ()
unregisterPackage = modifyPackage RemovePackage

modifyPackage
  :: (InstalledPackageInfo -> DBOp)
  -> PackageIdentifier
  -> Verbosity
  -> [Flag]
  -> Force
  -> IO ()
modifyPackage fn pkgid verbosity my_flags force = do
  (db_stack, Just _to_modify, _flag_dbs) <- 
      getPkgDatabases verbosity True{-modify-} True{-use cache-} False{-expand vars-} my_flags

  (db, ps) <- fmap head $ findPackagesByDB db_stack (Id pkgid)
  let 
      db_name = location db
      pkgs    = packages db

      pids = map sourcePackageId ps

      cmds = [ fn pkg | pkg <- pkgs, sourcePackageId pkg `elem` pids ]
      new_db = updateInternalDB db cmds

      old_broken = brokenPackages (allPackagesInStack db_stack)
      rest_of_stack = filter ((/= db_name) . location) db_stack
      new_stack = new_db : rest_of_stack
      new_broken = map sourcePackageId (brokenPackages (allPackagesInStack new_stack))
      newly_broken = filter (`notElem` map sourcePackageId old_broken) new_broken
  --
  when (not (null newly_broken)) $
      dieOrForceAll force ("unregistering " ++ display pkgid ++
           " would break the following packages: "
              ++ unwords (map display newly_broken))

  changeDB verbosity cmds db

recache :: Verbosity -> [Flag] -> IO ()
recache verbosity my_flags = do
  (db_stack, Just to_modify, _flag_dbs) <- 
     getPkgDatabases verbosity True{-modify-} False{-no cache-} False{-expand vars-} my_flags
  let
        db_to_operate_on = my_head "recache" $
                           filter ((== to_modify).location) db_stack
  --
  changeDB verbosity [] db_to_operate_on

-- -----------------------------------------------------------------------------
-- Listing packages

listPackages ::  Verbosity -> [Flag] -> Maybe PackageArg
             -> Maybe (String->Bool)
             -> IO ()
listPackages verbosity my_flags mPackageName mModuleName = do
  let simple_output = FlagSimpleOutput `elem` my_flags
  (db_stack, _, flag_db_stack) <- 
     getPkgDatabases verbosity False True{-use cache-} False{-expand vars-} my_flags

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
                   case pkgName p1 `compare` pkgName p2 of
                        LT -> LT
                        GT -> GT
                        EQ -> pkgVersion p1 `compare` pkgVersion p2
                   where (p1,p2) = (sourcePackageId pkg1, sourcePackageId pkg2)

      stack = reverse db_stack_sorted

      match `exposedInPkg` pkg = any match (map display $ exposedModules pkg)

      pkg_map = allPackagesInStack db_stack
      broken = map sourcePackageId (brokenPackages pkg_map)

      show_normal PackageDB{ location = db_name, packages = pkg_confs } =
          do hPutStrLn stdout (db_name ++ ":")
             if null pp_pkgs
                 then hPutStrLn stdout "    (no packages)"
                 else hPutStrLn stdout $ unlines (map ("    " ++) pp_pkgs)
           where
                 pp_pkgs = map pp_pkg pkg_confs
                 pp_pkg p
                   | sourcePackageId p `elem` broken = printf "{%s}" doc
                   | exposed p = doc
                   | otherwise = printf "(%s)" doc
                   where doc | verbosity >= Verbose = printf "%s (%s)" pkg ipid
                             | otherwise            = pkg
                          where
                          InstalledPackageId ipid = installedPackageId p
                          pkg = display (sourcePackageId p)

      show_simple = simplePackageList my_flags . allPackagesInStack

  when (not (null broken) && not simple_output && verbosity /= Silent) $ do
     prog <- getProgramName
     warn ("WARNING: there are broken packages.  Run '" ++ prog ++ " check' for more details.")

  if simple_output then show_simple stack else do

#if defined(mingw32_HOST_OS) || defined(BOOTSTRAPPING)
  mapM_ show_normal stack
#else
  let
     show_colour withF db =
         mconcat $ map (<#> termText "\n") $
             (termText (location db) :
                map (termText "   " <#>) (map pp_pkg (packages db)))
        where
                 pp_pkg p
                   | sourcePackageId p `elem` broken = withF Red  doc
                   | exposed p                       = doc
                   | otherwise                       = withF Blue doc
                   where doc | verbosity >= Verbose
                             = termText (printf "%s (%s)" pkg ipid)
                             | otherwise
                             = termText pkg
                          where
                          InstalledPackageId ipid = installedPackageId p
                          pkg = display (sourcePackageId p)

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
   let showPkg = if FlagNamesOnly `elem` my_flags then display . pkgName
                                                  else display
       strs = map showPkg $ sortBy compPkgIdVer $ map sourcePackageId pkgs
   when (not (null pkgs)) $
      hPutStrLn stdout $ concat $ intersperse " " strs

showPackageDot :: Verbosity -> [Flag] -> IO ()
showPackageDot verbosity myflags = do
  (_, _, flag_db_stack) <- 
      getPkgDatabases verbosity False True{-use cache-} False{-expand vars-} myflags

  let all_pkgs = allPackagesInStack flag_db_stack
      ipix  = PackageIndex.fromList all_pkgs

  putStrLn "digraph {"
  let quote s = '"':s ++ "\""
  mapM_ putStrLn [ quote from ++ " -> " ++ quote to
                 | p <- all_pkgs,
                   let from = display (sourcePackageId p),
                   depid <- depends p,
                   Just dep <- [PackageIndex.lookupInstalledPackageId ipix depid],
                   let to = display (sourcePackageId dep)
                 ]
  putStrLn "}"

-- -----------------------------------------------------------------------------
-- Prints the highest (hidden or exposed) version of a package

latestPackage ::  Verbosity -> [Flag] -> PackageIdentifier -> IO ()
latestPackage verbosity my_flags pkgid = do
  (_, _, flag_db_stack) <- 
     getPkgDatabases verbosity False True{-use cache-} False{-expand vars-} my_flags

  ps <- findPackages flag_db_stack (Id pkgid)
  show_pkg (sortBy compPkgIdVer (map sourcePackageId ps))
  where
    show_pkg [] = die "no matches"
    show_pkg pids = hPutStrLn stdout (display (last pids))

-- -----------------------------------------------------------------------------
-- Describe

describePackage :: Verbosity -> [Flag] -> PackageArg -> Bool -> IO ()
describePackage verbosity my_flags pkgarg expand_pkgroot = do
  (_, _, flag_db_stack) <- 
      getPkgDatabases verbosity False True{-use cache-} expand_pkgroot my_flags
  dbs <- findPackagesByDB flag_db_stack pkgarg
  doDump expand_pkgroot [ (pkg, locationAbsolute db)
                        | (db, pkgs) <- dbs, pkg <- pkgs ]

dumpPackages :: Verbosity -> [Flag] -> Bool -> IO ()
dumpPackages verbosity my_flags expand_pkgroot = do
  (_, _, flag_db_stack) <- 
     getPkgDatabases verbosity False True{-use cache-} expand_pkgroot my_flags
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

findPackagesByDB :: PackageDBStack -> PackageArg
                 -> IO [(PackageDB, [InstalledPackageInfo])]
findPackagesByDB db_stack pkgarg
  = case [ (db, matched)
         | db <- db_stack,
           let matched = filter (pkgarg `matchesPkg`) (packages db),
           not (null matched) ] of
        [] -> die ("cannot find package " ++ pkg_msg pkgarg)
        ps -> return ps
  where
        pkg_msg (Id pkgid)           = display pkgid
        pkg_msg (Substring pkgpat _) = "matching " ++ pkgpat

matches :: PackageIdentifier -> PackageIdentifier -> Bool
pid `matches` pid'
  = (pkgName pid == pkgName pid')
    && (pkgVersion pid == pkgVersion pid' || not (realVersion pid))

realVersion :: PackageIdentifier -> Bool
realVersion pkgid = versionBranch (pkgVersion pkgid) /= []
  -- when versionBranch == [], this is a glob

matchesPkg :: PackageArg -> InstalledPackageInfo -> Bool
(Id pid)        `matchesPkg` pkg = pid `matches` sourcePackageId pkg
(Substring _ m) `matchesPkg` pkg = m (display (sourcePackageId pkg))

compPkgIdVer :: PackageIdentifier -> PackageIdentifier -> Ordering
compPkgIdVer p1 p2 = pkgVersion p1 `compare` pkgVersion p2

-- -----------------------------------------------------------------------------
-- Field

describeField :: Verbosity -> [Flag] -> PackageArg -> [String] -> Bool -> IO ()
describeField verbosity my_flags pkgarg fields expand_pkgroot = do
  (_, _, flag_db_stack) <- 
      getPkgDatabases verbosity False True{-use cache-} expand_pkgroot my_flags
  fns <- toFields fields
  ps <- findPackages flag_db_stack pkgarg
  mapM_ (selectFields fns) ps
  where showFun = if FlagSimpleOutput `elem` my_flags
                  then showSimpleInstalledPackageInfoField
                  else showInstalledPackageInfoField
        toFields [] = return []
        toFields (f:fs) = case showFun f of
            Nothing -> die ("unknown field: " ++ f)
            Just fn -> do fns <- toFields fs
                          return (fn:fns)
        selectFields fns pinfo = mapM_ (\fn->putStrLn (fn pinfo)) fns


-- -----------------------------------------------------------------------------
-- Check: Check consistency of installed packages

checkConsistency :: Verbosity -> [Flag] -> IO ()
checkConsistency verbosity my_flags = do
  (db_stack, _, _) <- 
         getPkgDatabases verbosity True True{-use cache-} True{-expand vars-} my_flags
         -- check behaves like modify for the purposes of deciding which
         -- databases to use, because ordering is important.

  let simple_output = FlagSimpleOutput `elem` my_flags

  let pkgs = allPackagesInStack db_stack

      checkPackage p = do
         (_,es,ws) <- runValidate $ checkPackageConfig p verbosity db_stack False True
         if null es
            then do when (not simple_output) $ do
                      _ <- reportValidateErrors [] ws "" Nothing
                      return ()
                    return []
            else do
              when (not simple_output) $ do
                  reportError ("There are problems in package " ++ display (sourcePackageId p) ++ ":")
                  _ <- reportValidateErrors es ws "  " Nothing
                  return ()
              return [p]

  broken_pkgs <- concat `fmap` mapM checkPackage pkgs

  let filterOut pkgs1 pkgs2 = filter not_in pkgs2
        where not_in p = sourcePackageId p `notElem` all_ps
              all_ps = map sourcePackageId pkgs1

  let not_broken_pkgs = filterOut broken_pkgs pkgs
      (_, trans_broken_pkgs) = closure [] not_broken_pkgs
      all_broken_pkgs = broken_pkgs ++ trans_broken_pkgs

  when (not (null all_broken_pkgs)) $ do
    if simple_output
      then simplePackageList my_flags all_broken_pkgs
      else do
       reportError ("\nThe following packages are broken, either because they have a problem\n"++
                "listed above, or because they depend on a broken package.")
       mapM_ (hPutStrLn stderr . display . sourcePackageId) all_broken_pkgs

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
              pids = map installedPackageId pkgs_ok

        -- we want mutually recursive groups of package to show up
        -- as broken. (#1750)

brokenPackages :: [InstalledPackageInfo] -> [InstalledPackageInfo]
brokenPackages pkgs = snd (closure [] pkgs)

-- -----------------------------------------------------------------------------
-- Manipulating package.conf files

type InstalledPackageInfoString = InstalledPackageInfo_ String

convertPackageInfoOut :: InstalledPackageInfo -> InstalledPackageInfoString
convertPackageInfoOut
    (pkgconf@(InstalledPackageInfo { exposedModules = e,
                                     hiddenModules = h })) =
        pkgconf{ exposedModules = map display e,
                 hiddenModules  = map display h }

convertPackageInfoIn :: InstalledPackageInfoString -> InstalledPackageInfo
convertPackageInfoIn
    (pkgconf@(InstalledPackageInfo { exposedModules = e,
                                     hiddenModules = h })) =
        pkgconf{ exposedModules = map convert e,
                 hiddenModules  = map convert h }
    where convert = fromJust . simpleParse

writeNewConfig :: Verbosity -> FilePath -> [InstalledPackageInfo] -> IO ()
writeNewConfig verbosity filename ipis = do
  when (verbosity >= Normal) $
      info "Writing new package config file... "
  createDirectoryIfMissing True $ takeDirectory filename
  let shown = concat $ intersperse ",\n "
                     $ map (show . convertPackageInfoOut) ipis
      fileContents = "[" ++ shown ++ "\n]"
  writeFileUtf8Atomic filename fileContents
    `catchIO` \e ->
      if isPermissionError e
      then die (filename ++ ": you don't have permission to modify this file")
      else ioError e
  when (verbosity >= Normal) $
      infoLn "done."

-----------------------------------------------------------------------------
-- Sanity-check a new package config, and automatically build GHCi libs
-- if requested.

type ValidateError   = (Force,String)
type ValidateWarning = String

newtype Validate a = V { runValidate :: IO (a, [ValidateError],[ValidateWarning]) }

instance Monad Validate where
   return a = V $ return (a, [], [])
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
reportValidateErrors :: [ValidateError] -> [ValidateWarning]
                     -> String -> Maybe Force -> IO Bool
reportValidateErrors es ws prefix mb_force = do
  mapM_ (warn . (prefix++)) ws
  oks <- mapM report es
  return (and oks)
  where
    report (f,s)
      | Just force <- mb_force
      = if (force >= f)
           then do reportError (prefix ++ s ++ " (ignoring)")
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
                      -> Bool   -- auto-ghc-libs
                      -> Bool   -- update, or check
                      -> Force
                      -> IO ()
validatePackageConfig pkg verbosity db_stack auto_ghci_libs update force = do
  (_,es,ws) <- runValidate $ checkPackageConfig pkg verbosity db_stack auto_ghci_libs update
  ok <- reportValidateErrors es ws (display (sourcePackageId pkg) ++ ": ") (Just force)
  when (not ok) $ exitWith (ExitFailure 1)

checkPackageConfig :: InstalledPackageInfo
                      -> Verbosity
                      -> PackageDBStack
                      -> Bool   -- auto-ghc-libs
                      -> Bool   -- update, or check
                      -> Validate ()
checkPackageConfig pkg verbosity db_stack auto_ghci_libs update = do
  checkInstalledPackageId pkg db_stack update
  checkPackageId pkg
  checkDuplicates db_stack pkg update
  mapM_ (checkDep db_stack) (depends pkg)
  checkDuplicateDepends (depends pkg)
  mapM_ (checkDir False "import-dirs")  (importDirs pkg)
  mapM_ (checkDir True  "library-dirs") (libraryDirs pkg)
  mapM_ (checkDir True  "include-dirs") (includeDirs pkg)
  mapM_ (checkDir True  "framework-dirs") (frameworkDirs pkg)
  mapM_ (checkFile   True "haddock-interfaces") (haddockInterfaces pkg)
  mapM_ (checkDirURL True "haddock-html")       (haddockHTMLs pkg)
  checkModules pkg
  mapM_ (checkHSLib verbosity (libraryDirs pkg) auto_ghci_libs) (hsLibraries pkg)
  -- ToDo: check these somehow?
  --    extra_libraries :: [String],
  --    c_includes      :: [String],

checkInstalledPackageId :: InstalledPackageInfo -> PackageDBStack -> Bool 
                        -> Validate ()
checkInstalledPackageId ipi db_stack update = do
  let ipid@(InstalledPackageId str) = installedPackageId ipi
  when (null str) $ verror CannotForce "missing id field"
  let dups = [ p | p <- allPackagesInStack db_stack, 
                   installedPackageId p == ipid ]
  when (not update && not (null dups)) $
    verror CannotForce $
        "package(s) with this id already exist: " ++ 
         unwords (map (display.packageId) dups)

-- When the package name and version are put together, sometimes we can
-- end up with a package id that cannot be parsed.  This will lead to
-- difficulties when the user wants to refer to the package later, so
-- we check that the package id can be parsed properly here.
checkPackageId :: InstalledPackageInfo -> Validate ()
checkPackageId ipi =
  let str = display (sourcePackageId ipi) in
  case [ x :: PackageIdentifier | (x,ys) <- readP_to_S parse str, all isSpace ys ] of
    [_] -> return ()
    []  -> verror CannotForce ("invalid package identifier: " ++ str)
    _   -> verror CannotForce ("ambiguous package identifier: " ++ str)

checkDuplicates :: PackageDBStack -> InstalledPackageInfo -> Bool -> Validate ()
checkDuplicates db_stack pkg update = do
  let
        pkgid = sourcePackageId pkg
        pkgs  = packages (head db_stack)
  --
  -- Check whether this package id already exists in this DB
  --
  when (not update && (pkgid `elem` map sourcePackageId pkgs)) $
       verror CannotForce $
          "package " ++ display pkgid ++ " is already installed"

  let
        uncasep = map toLower . display
        dups = filter ((== uncasep pkgid) . uncasep) (map sourcePackageId pkgs)

  when (not update && not (null dups)) $ verror ForceAll $
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

checkDep :: PackageDBStack -> InstalledPackageId -> Validate ()
checkDep db_stack pkgid
  | pkgid `elem` pkgids = return ()
  | otherwise = verror ForceAll ("dependency \"" ++ display pkgid
                                 ++ "\" doesn't exist")
  where
        all_pkgs = allPackagesInStack db_stack
        pkgids = map installedPackageId all_pkgs

checkDuplicateDepends :: [InstalledPackageId] -> Validate ()
checkDuplicateDepends deps
  | null dups = return ()
  | otherwise = verror ForceAll ("package has duplicate dependencies: " ++
                                     unwords (map display dups))
  where
       dups = [ p | (p:_:_) <- group (sort deps) ]

checkHSLib :: Verbosity -> [String] -> Bool -> String -> Validate ()
checkHSLib verbosity dirs auto_ghci_libs lib = do
  let batch_lib_file = "lib" ++ lib ++ ".a"
      filenames = ["lib" ++ lib ++ ".a",
                   "lib" ++ lib ++ ".p_a",
                   "lib" ++ lib ++ "-ghc" ++ Version.version ++ ".so",
                   "lib" ++ lib ++ "-ghc" ++ Version.version ++ ".dylib",
                            lib ++ "-ghc" ++ Version.version ++ ".dll"]
  m <- liftIO $ doesFileExistOnPath filenames dirs
  case m of
    Nothing -> verror ForceFiles ("cannot find any of " ++ show filenames ++
                                  " on library path")
    Just dir -> liftIO $ checkGHCiLib verbosity dir batch_lib_file lib auto_ghci_libs

doesFileExistOnPath :: [FilePath] -> [FilePath] -> IO (Maybe FilePath)
doesFileExistOnPath filenames paths = go fullFilenames
  where fullFilenames = [ (path, path </> filename)
                        | filename <- filenames
                        , path <- paths ]
        go []             = return Nothing
        go ((p, fp) : xs) = do b <- doesFileExist fp
                               if b then return (Just p) else go xs

checkModules :: InstalledPackageInfo -> Validate ()
checkModules pkg = do
  mapM_ findModule (exposedModules pkg ++ hiddenModules pkg)
  where
    findModule modl =
      -- there's no interface file for GHC.Prim
      unless (modl == fromString "GHC.Prim") $ do
      let files = [ toFilePath modl <.> extension
                  | extension <- ["hi", "p_hi", "dyn_hi" ] ]
      m <- liftIO $ doesFileExistOnPath files (importDirs pkg)
      when (isNothing m) $
         verror ForceFiles ("cannot find any of " ++ show files)

checkGHCiLib :: Verbosity -> String -> String -> String -> Bool -> IO ()
checkGHCiLib verbosity batch_lib_dir batch_lib_file lib auto_build
  | auto_build = autoBuildGHCiLib verbosity batch_lib_dir batch_lib_file ghci_lib_file
  | otherwise  = return ()
 where
    ghci_lib_file = lib <.> "o"

-- automatically build the GHCi version of a batch lib,
-- using ld --whole-archive.

autoBuildGHCiLib :: Verbosity -> String -> String -> String -> IO ()
autoBuildGHCiLib verbosity dir batch_file ghci_file = do
  let ghci_lib_file  = dir ++ '/':ghci_file
      batch_lib_file = dir ++ '/':batch_file
  when (verbosity >= Normal) $
    info ("building GHCi library " ++ ghci_lib_file ++ "...")
#if defined(darwin_HOST_OS)
  r <- rawSystem "ld" ["-r","-x","-o",ghci_lib_file,"-all_load",batch_lib_file]
#elif defined(mingw32_HOST_OS)
  execDir <- getLibDir
  r <- rawSystem (maybe "" (++"/gcc-lib/") execDir++"ld") ["-r","-x","-o",ghci_lib_file,"--whole-archive",batch_lib_file]
#else
  r <- rawSystem "ld" ["-r","-x","-o",ghci_lib_file,"--whole-archive",batch_lib_file]
#endif
  when (r /= ExitSuccess) $ exitWith r
  when (verbosity >= Normal) $
    infoLn (" done.")

-- -----------------------------------------------------------------------------
-- Searching for modules

#if not_yet

findModules :: [FilePath] -> IO [String]
findModules paths =
  mms <- mapM searchDir paths
  return (concat mms)

searchDir path prefix = do
  fs <- getDirectoryEntries path `catchIO` \_ -> return []
  searchEntries path prefix fs

searchEntries path prefix [] = return []
searchEntries path prefix (f:fs)
  | looks_like_a_module  =  do
        ms <- searchEntries path prefix fs
        return (prefix `joinModule` f : ms)
  | looks_like_a_component  =  do
        ms <- searchDir (path </> f) (prefix `joinModule` f)
        ms' <- searchEntries path prefix fs
        return (ms ++ ms')
  | otherwise
        searchEntries path prefix fs

  where
        (base,suffix) = splitFileExt f
        looks_like_a_module =
                suffix `elem` haskell_suffixes &&
                all okInModuleName base
        looks_like_a_component =
                null suffix && all okInModuleName base

okInModuleName c

#endif

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

my_head :: String -> [a] -> a
my_head s []      = error s
my_head _ (x : _) = x

-----------------------------------------
-- Cut and pasted from ghc/compiler/main/SysTools

#if defined(mingw32_HOST_OS)
subst :: Char -> Char -> String -> String
subst a b ls = map (\ x -> if x == a then b else x) ls

unDosifyPath :: FilePath -> FilePath
unDosifyPath xs = subst '\\' '/' xs

getLibDir :: IO (Maybe String)
getLibDir = fmap (fmap (</> "lib")) $ getExecDir "/bin/ghc-pkg.exe"

-- (getExecDir cmd) returns the directory in which the current
--                  executable, which should be called 'cmd', is running
-- So if the full path is /a/b/c/d/e, and you pass "d/e" as cmd,
-- you'll get "/a/b/c" back as the result
getExecDir :: String -> IO (Maybe String)
getExecDir cmd =
    getExecPath >>= maybe (return Nothing) removeCmdSuffix
    where initN n = reverse . drop n . reverse
          removeCmdSuffix = return . Just . initN (length cmd) . unDosifyPath

getExecPath :: IO (Maybe String)
getExecPath = try_size 2048 -- plenty, PATH_MAX is 512 under Win32.
  where
    try_size size = allocaArray (fromIntegral size) $ \buf -> do
        ret <- c_GetModuleFileName nullPtr buf size
        case ret of
          0 -> return Nothing
          _ | ret < size -> fmap Just $ peekCWString buf
            | otherwise  -> try_size (size * 2)

foreign import WINDOWS_CCONV unsafe "windows.h GetModuleFileNameW"
  c_GetModuleFileName :: Ptr () -> CWString -> Word32 -> IO Word32
#else
getLibDir :: IO (Maybe String)
getLibDir = return Nothing
#endif

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

#if mingw32_HOST_OS || mingw32_TARGET_OS
throwIOIO :: Exception.IOException -> IO a
throwIOIO = Exception.throwIO
#endif

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch

catchError :: IO a -> (String -> IO a) -> IO a
catchError io handler = io `Exception.catch` handler'
    where handler' (Exception.ErrorCall err) = handler err

tryIO :: IO a -> IO (Either Exception.IOException a)
tryIO = Exception.try

writeBinaryFileAtomic :: Bin.Binary a => FilePath -> a -> IO ()
writeBinaryFileAtomic targetFile obj =
  withFileAtomic targetFile $ \h -> do
     hSetBinaryMode h True
     B.hPutStr h (Bin.encode obj)

writeFileUtf8Atomic :: FilePath -> String -> IO ()
writeFileUtf8Atomic targetFile content =
  withFileAtomic targetFile $ \h -> do
     hSetEncoding h utf8
     hPutStr h content

-- copied from Cabal's Distribution.Simple.Utils, except that we want
-- to use text files here, rather than binary files.
withFileAtomic :: FilePath -> (Handle -> IO ()) -> IO ()
withFileAtomic targetFile write_content = do
  (newFile, newHandle) <- openNewFile targetDir template
  do  write_content newHandle
      hClose newHandle
#if mingw32_HOST_OS || mingw32_TARGET_OS
      renameFile newFile targetFile
        -- If the targetFile exists then renameFile will fail
        `catchIO` \err -> do
          exists <- doesFileExist targetFile
          if exists
            then do removeFileSafe targetFile
                    -- Big fat hairy race condition
                    renameFile newFile targetFile
                    -- If the removeFile succeeds and the renameFile fails
                    -- then we've lost the atomic property.
            else throwIOIO err
#else
      renameFile newFile targetFile
#endif
   `Exception.onException` do hClose newHandle
                              removeFileSafe newFile
  where
    template = targetName <.> "tmp"
    targetDir | null targetDir_ = "."
              | otherwise       = targetDir_
    --TODO: remove this when takeDirectory/splitFileName is fixed
    --      to always return a valid dir
    (targetDir_,targetName) = splitFileName targetFile

openNewFile :: FilePath -> String -> IO (FilePath, Handle)
openNewFile dir template = do
  -- this was added to System.IO in 6.12.1
  -- we must use this version because the version below opens the file
  -- in binary mode.
  openTempFileWithDefaultPermissions dir template

-- | The function splits the given string to substrings
-- using 'isSearchPathSeparator'.
parseSearchPath :: String -> [FilePath]
parseSearchPath path = split path
  where
    split :: String -> [String]
    split s =
      case rest' of
        []     -> [chunk]
        _:rest -> chunk : split rest
      where
        chunk =
          case chunk' of
#ifdef mingw32_HOST_OS
            ('\"':xs@(_:_)) | last xs == '\"' -> init xs
#endif
            _                                 -> chunk'

        (chunk', rest') = break isSearchPathSeparator s

readUTF8File :: FilePath -> IO String
readUTF8File file = do
  h <- openFile file ReadMode
  -- fix the encoding to UTF-8
  hSetEncoding h utf8
  hGetContents h

-- removeFileSave doesn't throw an exceptions, if the file is already deleted
removeFileSafe :: FilePath -> IO ()
removeFileSafe fn =
  removeFile fn `catchIO` \ e ->
    when (not $ isDoesNotExistError e) $ ioError e

absolutePath :: FilePath -> IO FilePath
absolutePath path = return . normalise . (</> path) =<< getCurrentDirectory
