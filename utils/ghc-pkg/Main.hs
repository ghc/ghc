{-# OPTIONS -fglasgow-exts -cpp #-}
-----------------------------------------------------------------------------
--
-- (c) The University of Glasgow 2004.
--
-- Package management tool
--
-----------------------------------------------------------------------------

-- TODO:
-- * validate modules
-- * expanding of variables in new-style package conf
-- * version manipulation (checking whether old version exists,
--   hiding old version?)

module Main (main) where

import Version ( version, targetOS, targetARCH )
import Distribution.InstalledPackageInfo hiding (depends)
import Distribution.Compat.ReadP
import Distribution.ParseUtils
import Distribution.Package
import Distribution.Text
import Distribution.Version
import System.FilePath
import System.Cmd       ( rawSystem )
import System.Directory ( getAppUserDataDirectory, createDirectoryIfMissing )

import Prelude

#include "../../includes/ghcconfig.h"

import System.Console.GetOpt
import Text.PrettyPrint
#if __GLASGOW_HASKELL__ >= 609
import qualified Control.Exception as Exception
#else
import qualified Control.Exception.Extensible as Exception
#endif
import Data.Maybe

import Data.Char ( isSpace, toLower )
import Control.Monad
import System.Directory ( doesDirectoryExist, getDirectoryContents,
                          doesFileExist, renameFile, removeFile )
import System.Exit ( exitWith, ExitCode(..) )
import System.Environment ( getArgs, getProgName, getEnv )
import System.IO
import System.IO.Error (try)
import Data.List
import Control.Concurrent

import Foreign
import Foreign.C
#ifdef mingw32_HOST_OS
import GHC.ConsoleHandler
#else
import System.Posix hiding (fdToHandle)
#endif

import IO ( isPermissionError )
import System.Posix.Internals
import GHC.Handle (fdToHandle)

#if defined(GLOB)
import System.Process(runInteractiveCommand)
import qualified System.Info(os)
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
           runit cli nonopts
        (_,_,errors) -> do
           prog <- getProgramName
           die (concat errors ++ usageInfo (usageHeader prog) flags)

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
  | FlagSimpleOutput
  | FlagNamesOnly
  | FlagIgnoreCase
  | FlagNoUserDb
  deriving Eq

flags :: [OptDescr Flag]
flags = [
  Option [] ["user"] (NoArg FlagUser)
        "use the current user's package database",
  Option [] ["global"] (NoArg FlagGlobal)
        "use the global package database",
  Option ['f'] ["package-conf"] (ReqArg FlagConfig "FILE")
        "use the specified package config file",
  Option [] ["global-conf"] (ReqArg FlagGlobalConfig "FILE")
        "location of the global package config",
  Option [] ["no-user-package-conf"] (NoArg FlagNoUserDb)
        "never read the user package database",
  Option [] ["force"] (NoArg FlagForce)
         "ignore missing dependencies, directories, and libraries",
  Option [] ["force-files"] (NoArg FlagForceFiles)
         "ignore missing directories and libraries only",
  Option ['g'] ["auto-ghci-libs"] (NoArg FlagAutoGHCiLibs)
        "automatically build libs for GHCi (with register)",
  Option ['?'] ["help"] (NoArg FlagHelp)
        "display this help and exit",
  Option ['V'] ["version"] (NoArg FlagVersion)
        "output version information and exit",
  Option [] ["simple-output"] (NoArg FlagSimpleOutput)
        "print output in easy-to-parse format for some commands",
  Option [] ["names-only"] (NoArg FlagNamesOnly)
        "only print package names, not versions; can only be used with list --simple-output",
  Option [] ["ignore-case"] (NoArg FlagIgnoreCase)
        "ignore case for substring matching"
  ]

deprecFlags :: [OptDescr Flag]
deprecFlags = [
        -- put deprecated flags here
  ]

ourCopyright :: String
ourCopyright = "GHC package manager version " ++ Version.version ++ "\n"

usageHeader :: String -> String
usageHeader prog = substProg prog $
  "Usage:\n" ++
  "  $p register {filename | -}\n" ++
  "    Register the package using the specified installed package\n" ++
  "    description. The syntax for the latter is given in the $p\n" ++
  "    documentation.\n" ++
  "\n" ++
  "  $p update {filename | -}\n" ++
  "    Register the package, overwriting any other package with the\n" ++
  "    same name.\n" ++
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
  "  $p list [pkg]\n" ++
  "    List registered packages in the global database, and also the\n" ++
  "    user database if --user is given. If a package name is given\n" ++
  "    all the registered versions will be listed in ascending order.\n" ++
  "    Accepts the --simple-output flag.\n" ++
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
  "    Check the consistency of package depenencies and list broken packages.\n" ++
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
  "    by tools that parse the results, rather than humans.\n" ++
  "\n" ++
  " Substring matching is supported for {module} in find-module and\n" ++
  " for {pkg} in list, describe, and field, where a '*' indicates\n" ++
  " open substring ends (prefix*, *suffix, *infix*).\n" ++
  "\n" ++
  "  When asked to modify a database (register, unregister, update,\n"++
  "  hide, expose, and also check), ghc-pkg modifies the global database by\n"++
  "  default.  Specifying --user causes it to act on the user database,\n"++
  "  or --package-conf can be used to act on another database\n"++
  "  entirely. When multiple of these options are given, the rightmost\n"++
  "  one is used as the database to act upon.\n"++
  "\n"++
  "  Commands that query the package database (list, latest, describe,\n"++
  "  field) operate on the list of databases specified by the flags\n"++
  "  --user, --global, and --package-conf.  If none of these flags are\n"++
  "  given, the default is --global --user.\n"++
  "\n" ++
  " The following optional flags are also accepted:\n"

substProg :: String -> String -> String
substProg _ [] = []
substProg prog ('$':'p':xs) = prog ++ substProg prog xs
substProg prog (c:xs) = c : substProg prog xs

-- -----------------------------------------------------------------------------
-- Do the business

data Force = ForceAll | ForceFiles | NoForce

data PackageArg = Id PackageIdentifier | Substring String (String->Bool)

runit :: [Flag] -> [String] -> IO ()
runit cli nonopts = do
  installSignalHandlers -- catch ^C and clean up
  prog <- getProgramName
  let
        force
          | FlagForce `elem` cli        = ForceAll
          | FlagForceFiles `elem` cli   = ForceFiles
          | otherwise                   = NoForce
        auto_ghci_libs = FlagAutoGHCiLibs `elem` cli
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
    ["register", filename] ->
        registerPackage filename cli auto_ghci_libs False force
    ["update", filename] ->
        registerPackage filename cli auto_ghci_libs True force
    ["unregister", pkgid_str] -> do
        pkgid <- readGlobPkgId pkgid_str
        unregisterPackage pkgid cli force
    ["expose", pkgid_str] -> do
        pkgid <- readGlobPkgId pkgid_str
        exposePackage pkgid cli force
    ["hide",   pkgid_str] -> do
        pkgid <- readGlobPkgId pkgid_str
        hidePackage pkgid cli force
    ["list"] -> do
        listPackages cli Nothing Nothing
    ["list", pkgid_str] ->
        case substringCheck pkgid_str of
          Nothing -> do pkgid <- readGlobPkgId pkgid_str
                        listPackages cli (Just (Id pkgid)) Nothing
          Just m -> listPackages cli (Just (Substring pkgid_str m)) Nothing
    ["find-module", moduleName] -> do
        let match = maybe (==moduleName) id (substringCheck moduleName)
        listPackages cli Nothing (Just match)
    ["latest", pkgid_str] -> do
        pkgid <- readGlobPkgId pkgid_str
        latestPackage cli pkgid
    ["describe", pkgid_str] ->
        case substringCheck pkgid_str of
          Nothing -> do pkgid <- readGlobPkgId pkgid_str
                        describePackage cli (Id pkgid)
          Just m -> describePackage cli (Substring pkgid_str m)
    ["field", pkgid_str, fields] ->
        case substringCheck pkgid_str of
          Nothing -> do pkgid <- readGlobPkgId pkgid_str
                        describeField cli (Id pkgid) (splitFields fields)
          Just m -> describeField cli (Substring pkgid_str m)
                                      (splitFields fields)
    ["check"] -> do
        checkConsistency cli

    ["dump"] -> do
        dumpPackages cli

    [] -> do
        die ("missing command\n" ++
                usageInfo (usageHeader prog) flags)
    (_cmd:_) -> do
        die ("command-line syntax error\n" ++
                usageInfo (usageHeader prog) flags)

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
      string "-*"
      return (PackageIdentifier{ pkgName = n, pkgVersion = globVersion }))

-- globVersion means "all versions"
globVersion :: Version
globVersion = Version{ versionBranch=[], versionTags=["*"] }

-- -----------------------------------------------------------------------------
-- Package databases

-- Some commands operate on a single database:
--      register, unregister, expose, hide
-- however these commands also check the union of the available databases
-- in order to check consistency.  For example, register will check that
-- dependencies exist before registering a package.
--
-- Some commands operate  on multiple databases, with overlapping semantics:
--      list, describe, field

type PackageDBName  = FilePath
type PackageDB      = [InstalledPackageInfo]

type NamedPackageDB = (PackageDBName, PackageDB)
type PackageDBStack = [NamedPackageDB]
        -- A stack of package databases.  Convention: head is the topmost
        -- in the stack.  Earlier entries override later one.

allPackagesInStack :: PackageDBStack -> [InstalledPackageInfo]
allPackagesInStack = concatMap snd

getPkgDatabases :: Bool -> [Flag] -> IO (PackageDBStack, Maybe PackageDBName)
getPkgDatabases modify my_flags = do
  -- first we determine the location of the global package config.  On Windows,
  -- this is found relative to the ghc-pkg.exe binary, whereas on Unix the
  -- location is passed to the binary using the --global-config flag by the
  -- wrapper script.
  let err_msg = "missing --global-conf option, location of global package.conf unknown\n"
  global_conf <-
     case [ f | FlagGlobalConfig f <- my_flags ] of
        [] -> do mb_dir <- getExecDir "/bin/ghc-pkg.exe"
                 case mb_dir of
                        Nothing  -> die err_msg
                        Just dir ->
                            do let path1 = dir </> "package.conf"
                                   path2 = dir </> ".." </> ".." </> ".."
                                               </> "inplace-datadir"
                                               </> "package.conf"
                               exists1 <- doesFileExist path1
                               exists2 <- doesFileExist path2
                               if exists1 then return path1
                                   else if exists2 then return path2
                                   else die "Can't find package.conf"
        fs -> return (last fs)

  let global_conf_dir = global_conf ++ ".d"
  global_conf_dir_exists <- doesDirectoryExist global_conf_dir
  global_confs <-
    if global_conf_dir_exists
      then do files <- getDirectoryContents global_conf_dir
              return [ global_conf_dir ++ '/' : file
                     | file <- files
                     , isSuffixOf ".conf" file]
      else return []

  let no_user_db = FlagNoUserDb `elem` my_flags

  -- get the location of the user package database, and create it if necessary
  -- getAppUserDataDirectory can fail (e.g. if $HOME isn't set)
  appdir <- try $ getAppUserDataDirectory "ghc"

  mb_user_conf <-
     if no_user_db then return Nothing else
     case appdir of
       Right dir -> do
               let subdir = targetARCH ++ '-':targetOS ++ '-':Version.version
                   user_conf = dir </> subdir </> "package.conf"
               user_exists <- doesFileExist user_conf
               return (Just (user_conf,user_exists))
       Left _ ->
               return Nothing

  -- If the user database doesn't exist, and this command isn't a
  -- "modify" command, then we won't attempt to create or use it.
  let sys_databases
        | Just (user_conf,user_exists) <- mb_user_conf,
          modify || user_exists = user_conf : global_confs ++ [global_conf]
        | otherwise             = global_confs ++ [global_conf]

  e_pkg_path <- try (System.Environment.getEnv "GHC_PACKAGE_PATH")
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

  (final_stack, to_modify) <-
     if not modify
        then    -- For a "read" command, we use all the databases
                -- specified on the command line.  If there are no
                -- command-line flags specifying databases, the default
                -- is to use all the ones we know about.
             if null db_flags then return (env_stack, Nothing)
                              else return (reverse (nub db_flags), Nothing)
        else let
                -- For a "modify" command, treat all the databases as
                -- a stack, where we are modifying the top one, but it
                -- can refer to packages in databases further down the
                -- stack.

                -- -f flags on the command line add to the database
                -- stack, unless any of them are present in the stack
                -- already.
                flag_stack = filter (`notElem` env_stack)
                                [ f | FlagConfig f <- reverse my_flags ]
                                ++ env_stack

                -- the database we actually modify is the one mentioned
                -- rightmost on the command-line.
                to_modify = if null db_flags 
                                then Just virt_global_conf
                                else Just (last db_flags)
             in
                return (flag_stack, to_modify)

  db_stack <- mapM readParseDatabase final_stack
  return (db_stack, to_modify)

readParseDatabase :: PackageDBName -> IO (PackageDBName,PackageDB)
readParseDatabase filename = do
  str <- readFile filename `catchIO` \_ -> return emptyPackageConfig
  let packages = map convertPackageInfoIn $ read str
  Exception.evaluate packages
    `catchError` \e->
        die ("error while parsing " ++ filename ++ ": " ++ show e)
  return (filename,packages)

emptyPackageConfig :: String
emptyPackageConfig = "[]"

-- -----------------------------------------------------------------------------
-- Registering

registerPackage :: FilePath
                -> [Flag]
                -> Bool              -- auto_ghci_libs
                -> Bool              -- update
                -> Force
                -> IO ()
registerPackage input my_flags auto_ghci_libs update force = do
  (db_stack, Just to_modify) <- getPkgDatabases True my_flags
  let
        db_to_operate_on = my_head "register" $
                           filter ((== to_modify).fst) db_stack
  --
  s <-
    case input of
      "-" -> do
        putStr "Reading package info from stdin ... "
        getContents
      f   -> do
        putStr ("Reading package info from " ++ show f ++ " ... ")
        readFile f

  expanded <- expandEnvVars s force

  pkg <- parsePackageInfo expanded
  putStrLn "done."

  let unversioned_deps = filter (not . realVersion) (depends pkg)
  unless (null unversioned_deps) $
      die ("Unversioned dependencies found: " ++
           unwords (map display unversioned_deps))

  let truncated_stack = dropWhile ((/= to_modify).fst) db_stack
  -- truncate the stack for validation, because we don't allow
  -- packages lower in the stack to refer to those higher up.
  validatePackageConfig pkg truncated_stack auto_ghci_libs update force
  let new_details = filter not_this (snd db_to_operate_on) ++ [pkg]
      not_this p = package p /= package pkg
  writeNewConfig to_modify new_details

parsePackageInfo
        :: String
        -> IO InstalledPackageInfo
parsePackageInfo str =
  case parseInstalledPackageInfo str of
    ParseOk _warns ok -> return ok
    ParseFailed err -> case locatedErrorMsg err of
                           (Nothing, s) -> die s
                           (Just l, s) -> die (show l ++ ": " ++ s)

-- -----------------------------------------------------------------------------
-- Exposing, Hiding, Unregistering are all similar

exposePackage :: PackageIdentifier ->  [Flag] -> Force -> IO ()
exposePackage = modifyPackage (\p -> [p{exposed=True}])

hidePackage :: PackageIdentifier ->  [Flag] -> Force -> IO ()
hidePackage = modifyPackage (\p -> [p{exposed=False}])

unregisterPackage :: PackageIdentifier ->  [Flag] -> Force -> IO ()
unregisterPackage = modifyPackage (\_ -> [])

modifyPackage
  :: (InstalledPackageInfo -> [InstalledPackageInfo])
  -> PackageIdentifier
  -> [Flag]
  -> Force
  -> IO ()
modifyPackage fn pkgid my_flags force = do
  (db_stack, Just _to_modify) <- getPkgDatabases True{-modify-} my_flags
  ((db_name, pkgs), ps) <- fmap head $ findPackagesByDB db_stack (Id pkgid)
--  let ((db_name, pkgs) : rest_of_stack) = db_stack
--  ps <- findPackages [(db_name,pkgs)] (Id pkgid)
  let 
      pids = map package ps
      modify pkg
          | package pkg `elem` pids = fn pkg
          | otherwise               = [pkg]
      new_config = concat (map modify pkgs)

  let
      old_broken = brokenPackages (allPackagesInStack db_stack)
      rest_of_stack = [ (nm, mypkgs)
                      | (nm, mypkgs) <- db_stack, nm /= db_name ]
      new_stack = (db_name,new_config) : rest_of_stack
      new_broken = map package (brokenPackages (allPackagesInStack new_stack))
      newly_broken = filter (`notElem` map package old_broken) new_broken
  --
  when (not (null newly_broken)) $
      dieOrForceAll force ("unregistering " ++ display pkgid ++
           " would break the following packages: "
              ++ unwords (map display newly_broken))

  writeNewConfig db_name new_config

-- -----------------------------------------------------------------------------
-- Listing packages

listPackages ::  [Flag] -> Maybe PackageArg -> Maybe (String->Bool) -> IO ()
listPackages my_flags mPackageName mModuleName = do
  let simple_output = FlagSimpleOutput `elem` my_flags
  (db_stack, _) <- getPkgDatabases False my_flags
  let db_stack_filtered -- if a package is given, filter out all other packages
        | Just this <- mPackageName =
            map (\(conf,pkgs) -> (conf, filter (this `matchesPkg`) pkgs))
                db_stack
        | Just match <- mModuleName = -- packages which expose mModuleName
            map (\(conf,pkgs) -> (conf, filter (match `exposedInPkg`) pkgs))
                db_stack
        | otherwise = db_stack

      db_stack_sorted
          = [ (db, sort_pkgs pkgs) | (db,pkgs) <- db_stack_filtered ]
          where sort_pkgs = sortBy cmpPkgIds
                cmpPkgIds pkg1 pkg2 =
                   case pkgName p1 `compare` pkgName p2 of
                        LT -> LT
                        GT -> GT
                        EQ -> pkgVersion p1 `compare` pkgVersion p2
                   where (p1,p2) = (package pkg1, package pkg2)

      match `exposedInPkg` pkg = any match (map display $ exposedModules pkg)

      pkg_map = allPackagesInStack db_stack
      show_func = if simple_output then show_simple else mapM_ (show_normal pkg_map)

  show_func (reverse db_stack_sorted)

  where show_normal pkg_map (db_name,pkg_confs) =
          hPutStrLn stdout (render $
                text db_name <> colon $$ nest 4 packages
                )
           where packages = fsep (punctuate comma (map pp_pkg pkg_confs))
                 broken = map package (brokenPackages pkg_map)
                 pp_pkg p
                   | package p `elem` broken = braces doc
                   | exposed p = doc
                   | otherwise = parens doc
                   where doc = text (display (package p))

        show_simple db_stack = do
          let showPkg = if FlagNamesOnly `elem` my_flags then display . pkgName
                                                         else display
              pkgs = map showPkg $ sortBy compPkgIdVer $
                          map package (allPackagesInStack db_stack)
          when (not (null pkgs)) $ 
             hPutStrLn stdout $ concat $ intersperse " " pkgs

-- -----------------------------------------------------------------------------
-- Prints the highest (hidden or exposed) version of a package

latestPackage ::  [Flag] -> PackageIdentifier -> IO ()
latestPackage my_flags pkgid = do
  (db_stack, _) <- getPkgDatabases False my_flags
  ps <- findPackages db_stack (Id pkgid)
  show_pkg (sortBy compPkgIdVer (map package ps))
  where
    show_pkg [] = die "no matches"
    show_pkg pids = hPutStrLn stdout (display (last pids))

-- -----------------------------------------------------------------------------
-- Describe

describePackage :: [Flag] -> PackageArg -> IO ()
describePackage my_flags pkgarg = do
  (db_stack, _) <- getPkgDatabases False my_flags
  ps <- findPackages db_stack pkgarg
  doDump ps

dumpPackages :: [Flag] -> IO ()
dumpPackages my_flags = do
  (db_stack, _) <- getPkgDatabases False my_flags
  doDump (allPackagesInStack db_stack)

doDump :: [InstalledPackageInfo] -> IO ()
doDump = mapM_ putStrLn . intersperse "---" . map showInstalledPackageInfo

-- PackageId is can have globVersion for the version
findPackages :: PackageDBStack -> PackageArg -> IO [InstalledPackageInfo]
findPackages db_stack pkgarg
  = fmap (concatMap snd) $ findPackagesByDB db_stack pkgarg

findPackagesByDB :: PackageDBStack -> PackageArg
                 -> IO [(NamedPackageDB, [InstalledPackageInfo])]
findPackagesByDB db_stack pkgarg
  = case [ (db, matched)
         | db@(_, pkgs) <- db_stack,
           let matched = filter (pkgarg `matchesPkg`) pkgs,
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

matchesPkg :: PackageArg -> InstalledPackageInfo -> Bool
(Id pid)        `matchesPkg` pkg = pid `matches` package pkg
(Substring _ m) `matchesPkg` pkg = m (display (package pkg))

compPkgIdVer :: PackageIdentifier -> PackageIdentifier -> Ordering
compPkgIdVer p1 p2 = pkgVersion p1 `compare` pkgVersion p2

-- -----------------------------------------------------------------------------
-- Field

describeField :: [Flag] -> PackageArg -> [String] -> IO ()
describeField my_flags pkgarg fields = do
  (db_stack, _) <- getPkgDatabases False my_flags
  fns <- toFields fields
  ps <- findPackages db_stack pkgarg
  let top_dir = takeDirectory (fst (last db_stack))
  mapM_ (selectFields fns) (mungePackagePaths top_dir ps)
  where toFields [] = return []
        toFields (f:fs) = case toField f of
            Nothing -> die ("unknown field: " ++ f)
            Just fn -> do fns <- toFields fs
                          return (fn:fns)
        selectFields fns info = mapM_ (\fn->putStrLn (fn info)) fns

mungePackagePaths :: String -> [InstalledPackageInfo] -> [InstalledPackageInfo]
-- Replace the strings "$topdir" and "$httptopdir" at the beginning of a path
-- with the current topdir (obtained from the -B option).
mungePackagePaths top_dir ps = map munge_pkg ps
  where
  munge_pkg p = p{ importDirs        = munge_paths (importDirs p),
                   includeDirs       = munge_paths (includeDirs p),
                   libraryDirs       = munge_paths (libraryDirs p),
                   frameworkDirs     = munge_paths (frameworkDirs p),
                   haddockInterfaces = munge_paths (haddockInterfaces p),
                   haddockHTMLs      = munge_paths (haddockHTMLs p)
                 }

  munge_paths = map munge_path

  munge_path p
   | Just p' <- maybePrefixMatch "$topdir"     p =            top_dir ++ p'
   | Just p' <- maybePrefixMatch "$httptopdir" p = toHttpPath top_dir ++ p'
   | otherwise                               = p

  toHttpPath p = "file:///" ++ p

maybePrefixMatch :: String -> String -> Maybe String
maybePrefixMatch []    rest = Just rest
maybePrefixMatch (_:_) []   = Nothing
maybePrefixMatch (p:pat) (r:rest)
  | p == r    = maybePrefixMatch pat rest
  | otherwise = Nothing

toField :: String -> Maybe (InstalledPackageInfo -> String)
-- backwards compatibility:
toField "import_dirs"     = Just $ strList . importDirs
toField "source_dirs"     = Just $ strList . importDirs
toField "library_dirs"    = Just $ strList . libraryDirs
toField "hs_libraries"    = Just $ strList . hsLibraries
toField "extra_libraries" = Just $ strList . extraLibraries
toField "include_dirs"    = Just $ strList . includeDirs
toField "c_includes"      = Just $ strList . includes
toField "package_deps"    = Just $ strList . map display. depends
toField "extra_cc_opts"   = Just $ strList . ccOptions
toField "extra_ld_opts"   = Just $ strList . ldOptions
toField "framework_dirs"  = Just $ strList . frameworkDirs
toField "extra_frameworks"= Just $ strList . frameworks
toField s                 = showInstalledPackageInfoField s

strList :: [String] -> String
strList = show


-- -----------------------------------------------------------------------------
-- Check: Check consistency of installed packages

checkConsistency :: [Flag] -> IO ()
checkConsistency my_flags = do
  (db_stack, _) <- getPkgDatabases True my_flags
         -- check behaves like modify for the purposes of deciding which
         -- databases to use, because ordering is important.
  let pkgs = allPackagesInStack db_stack
      broken_pkgs = brokenPackages pkgs
      broken_ids = map package broken_pkgs
      broken_why = [ (package p, filter (`elem` broken_ids) (depends p))
                   | p <- broken_pkgs ]
  mapM_ (putStrLn . render . show_func) broken_why
  where
  show_func | FlagSimpleOutput `elem` my_flags = show_simple
            | otherwise = show_normal
  show_simple (pid,deps) =
    text (display pid) <> colon
      <+> fsep (punctuate comma (map (text . display) deps))
  show_normal (pid,deps) =
    text "package" <+> text (display pid) <+> text "has missing dependencies:"
      $$ nest 4 (fsep (punctuate comma (map (text . display) deps)))


brokenPackages :: [InstalledPackageInfo] -> [InstalledPackageInfo]
brokenPackages pkgs = go [] pkgs
 where
   go avail not_avail =
     case partition (depsAvailable avail) not_avail of
        ([],        not_avail') -> not_avail'
        (new_avail, not_avail') -> go (new_avail ++ avail) not_avail'

   depsAvailable :: [InstalledPackageInfo] -> InstalledPackageInfo
                 -> Bool
   depsAvailable pkgs_ok pkg = null dangling
        where dangling = filter (`notElem` pids) (depends pkg)
              pids = map package pkgs_ok

        -- we want mutually recursive groups of package to show up
        -- as broken. (#1750)

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

writeNewConfig :: FilePath -> [InstalledPackageInfo] -> IO ()
writeNewConfig filename packages = do
  hPutStr stdout "Writing new package config file... "
  createDirectoryIfMissing True $ takeDirectory filename
  let shown = concat $ intersperse ",\n "
                     $ map (show . convertPackageInfoOut) packages
      fileContents = "[" ++ shown ++ "\n]"
  writeFileAtomic filename fileContents
    `catch` \e ->
      if isPermissionError e
      then die (filename ++ ": you don't have permission to modify this file")
      else ioError e
  hPutStrLn stdout "done."

-----------------------------------------------------------------------------
-- Sanity-check a new package config, and automatically build GHCi libs
-- if requested.

validatePackageConfig :: InstalledPackageInfo
                      -> PackageDBStack
                      -> Bool   -- auto-ghc-libs
                      -> Bool   -- update
                      -> Force
                      -> IO ()
validatePackageConfig pkg db_stack auto_ghci_libs update force = do
  checkPackageId pkg
  checkDuplicates db_stack pkg update force
  mapM_ (checkDep db_stack force) (depends pkg)
  checkDuplicateDepends force (depends pkg)
  mapM_ (checkDir force) (importDirs pkg)
  mapM_ (checkDir force) (libraryDirs pkg)
  mapM_ (checkDir force) (includeDirs pkg)
  mapM_ (checkHSLib (libraryDirs pkg) auto_ghci_libs force) (hsLibraries pkg)
  -- ToDo: check these somehow?
  --    extra_libraries :: [String],
  --    c_includes      :: [String],

-- When the package name and version are put together, sometimes we can
-- end up with a package id that cannot be parsed.  This will lead to
-- difficulties when the user wants to refer to the package later, so
-- we check that the package id can be parsed properly here.
checkPackageId :: InstalledPackageInfo -> IO ()
checkPackageId ipi =
  let str = display (package ipi) in
  case [ x :: PackageIdentifier | (x,ys) <- readP_to_S parse str, all isSpace ys ] of
    [_] -> return ()
    []  -> die ("invalid package identifier: " ++ str)
    _   -> die ("ambiguous package identifier: " ++ str)

checkDuplicates :: PackageDBStack -> InstalledPackageInfo -> Bool -> Force -> IO ()
checkDuplicates db_stack pkg update force = do
  let
        pkgid = package pkg
        (_top_db_name, pkgs) : _  = db_stack
  --
  -- Check whether this package id already exists in this DB
  --
  when (not update && (pkgid `elem` map package pkgs)) $
       die ("package " ++ display pkgid ++ " is already installed")

  let
        uncasep = map toLower . display
        dups = filter ((== uncasep pkgid) . uncasep) (map package pkgs)

  when (not update && not (null dups)) $ dieOrForceAll force $
        "Package names may be treated case-insensitively in the future.\n"++
        "Package " ++ display pkgid ++
        " overlaps with: " ++ unwords (map display dups)


checkDir :: Force -> String -> IO ()
checkDir force d
 | "$topdir"     `isPrefixOf` d = return ()
 | "$httptopdir" `isPrefixOf` d = return ()
        -- can't check these, because we don't know what $(http)topdir is
 | otherwise = do
   there <- doesDirectoryExist d
   when (not there)
       (dieOrForceFile force (d ++ " doesn't exist or isn't a directory"))

checkDep :: PackageDBStack -> Force -> PackageIdentifier -> IO ()
checkDep db_stack force pkgid
  | pkgid `elem` pkgids || (not real_version && name_exists) = return ()
  | otherwise = dieOrForceAll force ("dependency " ++ display pkgid
                                        ++ " doesn't exist")
  where
        -- for backwards compat, we treat 0.0 as a special version,
        -- and don't check that it actually exists.
        real_version = realVersion pkgid

        name_exists = any (\p -> pkgName (package p) == name) all_pkgs
        name = pkgName pkgid

        all_pkgs = allPackagesInStack db_stack
        pkgids = map package all_pkgs

checkDuplicateDepends :: Force -> [PackageIdentifier] -> IO ()
checkDuplicateDepends force deps
  | null dups = return ()
  | otherwise = dieOrForceAll force ("package has duplicate dependencies: " ++
                                     unwords (map display dups))
  where
       dups = [ p | (p:_:_) <- group (sort deps) ]

realVersion :: PackageIdentifier -> Bool
realVersion pkgid = versionBranch (pkgVersion pkgid) /= []

checkHSLib :: [String] -> Bool -> Force -> String -> IO ()
checkHSLib dirs auto_ghci_libs force lib = do
  let batch_lib_file = "lib" ++ lib ++ ".a"
  bs <- mapM (doesLibExistIn batch_lib_file) dirs
  case [ dir | (exists,dir) <- zip bs dirs, exists ] of
        [] -> dieOrForceFile force ("cannot find " ++ batch_lib_file ++
                                    " on library path")
        (dir:_) -> checkGHCiLib dirs dir batch_lib_file lib auto_ghci_libs

doesLibExistIn :: String -> String -> IO Bool
doesLibExistIn lib d
 | "$topdir"     `isPrefixOf` d = return True
 | "$httptopdir" `isPrefixOf` d = return True
 | otherwise                = doesFileExist (d ++ '/':lib)

checkGHCiLib :: [String] -> String -> String -> String -> Bool -> IO ()
checkGHCiLib dirs batch_lib_dir batch_lib_file lib auto_build
  | auto_build = autoBuildGHCiLib batch_lib_dir batch_lib_file ghci_lib_file
  | otherwise  = do
      bs <- mapM (doesLibExistIn ghci_lib_file) dirs
      case [dir | (exists,dir) <- zip bs dirs, exists] of
        []    -> hPutStrLn stderr ("warning: can't find GHCi lib " ++ ghci_lib_file)
        (_:_) -> return ()
  where
    ghci_lib_file = lib ++ ".o"

-- automatically build the GHCi version of a batch lib,
-- using ld --whole-archive.

autoBuildGHCiLib :: String -> String -> String -> IO ()
autoBuildGHCiLib dir batch_file ghci_file = do
  let ghci_lib_file  = dir ++ '/':ghci_file
      batch_lib_file = dir ++ '/':batch_file
  hPutStr stderr ("building GHCi library " ++ ghci_lib_file ++ "...")
#if defined(darwin_HOST_OS)
  r <- rawSystem "ld" ["-r","-x","-o",ghci_lib_file,"-all_load",batch_lib_file]
#elif defined(mingw32_HOST_OS)
  execDir <- getExecDir "/bin/ghc-pkg.exe"
  r <- rawSystem (maybe "" (++"/gcc-lib/") execDir++"ld") ["-r","-x","-o",ghci_lib_file,"--whole-archive",batch_lib_file]
#else
  r <- rawSystem "ld" ["-r","-x","-o",ghci_lib_file,"--whole-archive",batch_lib_file]
#endif
  when (r /= ExitSuccess) $ exitWith r
  hPutStrLn stderr (" done.")

-- -----------------------------------------------------------------------------
-- Searching for modules

#if not_yet

findModules :: [FilePath] -> IO [String]
findModules paths =
  mms <- mapM searchDir paths
  return (concat mms)

searchDir path prefix = do
  fs <- getDirectoryEntries path `catch` \_ -> return []
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
   lookupEnvVar nm =
        catch (System.Environment.getEnv nm)
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
  hFlush stdout
  prog <- getProgramName
  hPutStrLn stderr (prog ++ ": " ++ s)
  exitWith (ExitFailure ec)

dieOrForceAll :: Force -> String -> IO ()
dieOrForceAll ForceAll s = ignoreError s
dieOrForceAll _other s   = dieForcible s

dieOrForceFile :: Force -> String -> IO ()
dieOrForceFile ForceAll   s = ignoreError s
dieOrForceFile ForceFiles s = ignoreError s
dieOrForceFile _other     s = dieForcible s

ignoreError :: String -> IO ()
ignoreError s = do hFlush stdout; hPutStrLn stderr (s ++ " (ignoring)")

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

getExecDir :: String -> IO (Maybe String)
-- (getExecDir cmd) returns the directory in which the current
--                  executable, which should be called 'cmd', is running
-- So if the full path is /a/b/c/d/e, and you pass "d/e" as cmd,
-- you'll get "/a/b/c" back as the result
getExecDir cmd
  = allocaArray len $ \buf -> do
        ret <- getModuleFileName nullPtr buf len
        if ret == 0 then return Nothing
                    else do s <- peekCString buf
                            return (Just (reverse (drop (length cmd)
                                                        (reverse (unDosifyPath s)))))
  where
    len = 2048::Int -- Plenty, PATH_MAX is 512 under Win32.

foreign import stdcall unsafe  "GetModuleFileNameA"
  getModuleFileName :: Ptr () -> CString -> Int -> IO Int32
#else
getExecDir :: String -> IO (Maybe String)
getExecDir _ = return Nothing
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
  installHandler sigQUIT (Catch interrupt) Nothing
  installHandler sigINT  (Catch interrupt) Nothing
  return ()
#elif __GLASGOW_HASKELL__ >= 603
  -- GHC 6.3+ has support for console events on Windows
  -- NOTE: running GHCi under a bash shell for some reason requires
  -- you to press Ctrl-Break rather than Ctrl-C to provoke
  -- an interrupt.  Ctrl-C is getting blocked somewhere, I don't know
  -- why --SDM 17/12/2004
  let sig_handler ControlC = interrupt
      sig_handler Break    = interrupt
      sig_handler _        = return ()

  installHandler (Catch sig_handler)
  return ()
#else
  return () -- nothing
#endif

#if __GLASGOW_HASKELL__ <= 604
isInfixOf               :: (Eq a) => [a] -> [a] -> Bool
isInfixOf needle haystack = any (isPrefixOf needle) (tails haystack)
#endif

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch

#if mingw32_HOST_OS || mingw32_TARGET_OS
throwIOIO :: Exception.IOException -> IO a
throwIOIO = Exception.throwIO
#endif

catchError :: IO a -> (String -> IO a) -> IO a
catchError io handler = io `Exception.catch` handler'
    where handler' (Exception.ErrorCall err) = handler err


-- copied from Cabal's Distribution.Simple.Utils, except that we want
-- to use text files here, rather than binary files.
writeFileAtomic :: FilePath -> String -> IO ()
writeFileAtomic targetFile content = do
  (newFile, newHandle) <- openNewFile targetDir template
  do  hPutStr newHandle content
      hClose newHandle
#if mingw32_HOST_OS || mingw32_TARGET_OS
      renameFile newFile targetFile
        -- If the targetFile exists then renameFile will fail
        `catchIO` \err -> do
          exists <- doesFileExist targetFile
          if exists
            then do removeFile targetFile
                    -- Big fat hairy race condition
                    renameFile newFile targetFile
                    -- If the removeFile succeeds and the renameFile fails
                    -- then we've lost the atomic property.
            else throwIOIO err
#else
      renameFile newFile targetFile
#endif
   `Exception.onException` do hClose newHandle
                              removeFile newFile
  where
    template = targetName <.> "tmp"
    targetDir | null targetDir_ = "."
              | otherwise       = targetDir_
    --TODO: remove this when takeDirectory/splitFileName is fixed
    --      to always return a valid dir
    (targetDir_,targetName) = splitFileName targetFile

-- Ugh, this is a copy/paste of code from the base library, but
-- if uses 666 rather than 600 for the permissions.
openNewFile :: FilePath -> String -> IO (FilePath, Handle)
openNewFile dir template = do
  pid <- c_getpid
  findTempName pid
  where
    -- We split off the last extension, so we can use .foo.ext files
    -- for temporary files (hidden on Unix OSes). Unfortunately we're
    -- below filepath in the hierarchy here.
    (prefix,suffix) =
       case break (== '.') $ reverse template of
         -- First case: template contains no '.'s. Just re-reverse it.
         (rev_suffix, "")       -> (reverse rev_suffix, "")
         -- Second case: template contains at least one '.'. Strip the
         -- dot from the prefix and prepend it to the suffix (if we don't
         -- do this, the unique number will get added after the '.' and
         -- thus be part of the extension, which is wrong.)
         (rev_suffix, '.':rest) -> (reverse rest, '.':reverse rev_suffix)
         -- Otherwise, something is wrong, because (break (== '.')) should
         -- always return a pair with either the empty string or a string
         -- beginning with '.' as the second component.
         _                      -> error "bug in System.IO.openTempFile"

    oflags = rw_flags .|. o_EXCL

    findTempName x = do
      fd <- withCString filepath $ \ f ->
              c_open f oflags 0o666
      if fd < 0
       then do
         errno <- getErrno
         if errno == eEXIST
           then findTempName (x+1)
           else ioError (errnoToIOError "openNewBinaryFile" errno Nothing (Just dir))
       else do
         -- XXX We want to tell fdToHandle what the filepath is,
         -- as any exceptions etc will only be able to report the
         -- fd currently
         h <-
#if __GLASGOW_HASKELL__ >= 609
              fdToHandle fd
#else
              fdToHandle (fromIntegral fd)
#endif
              `Exception.onException` c_close fd
         return (filepath, h)
      where
        filename        = prefix ++ show x ++ suffix
        filepath        = dir `combine` filename

-- XXX Copied from GHC.Handle
std_flags, output_flags, rw_flags :: CInt
std_flags    = o_NONBLOCK   .|. o_NOCTTY
output_flags = std_flags    .|. o_CREAT
rw_flags     = output_flags .|. o_RDWR

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
