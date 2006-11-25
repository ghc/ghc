{-# OPTIONS -fglasgow-exts #-}
-----------------------------------------------------------------------------
--
-- (c) The University of Glasgow 2004.
--
-- Package management tool
--
-----------------------------------------------------------------------------

-- TODO:
--	- validate modules
--	- expanding of variables in new-style package conf
--	- version manipulation (checking whether old version exists,
--	  hiding old version?)

module Main (main) where

import Version	( version, targetOS, targetARCH )
import Distribution.InstalledPackageInfo
import Distribution.Compat.ReadP
import Distribution.ParseUtils	( showError )
import Distribution.Package
import Distribution.Version
import Compat.Directory 	( getAppUserDataDirectory, createDirectoryIfMissing )
import Compat.RawSystem 	( rawSystem )

import Prelude

#include "../../includes/ghcconfig.h"

#if __GLASGOW_HASKELL__ >= 504
import System.Console.GetOpt
import Text.PrettyPrint
import qualified Control.Exception as Exception
import Data.Maybe
#else
import GetOpt
import Pretty
import qualified Exception
import Maybe
#endif

import Data.Char	( isSpace )
import Monad
import Directory
import System	( getArgs, getProgName, getEnv,
		  exitWith, ExitCode(..)
		)
import System.IO
#if __GLASGOW_HASKELL__ >= 600
import System.IO.Error (try)
#else
import System.IO (try)
#endif
import Data.List ( isPrefixOf, isSuffixOf, intersperse, sortBy )

#ifdef mingw32_HOST_OS
import Foreign

#if __GLASGOW_HASKELL__ >= 504
import Foreign.C.String
#else
import CString
#endif
#endif

import IO ( isPermissionError, isDoesNotExistError )

-- -----------------------------------------------------------------------------
-- Entry point

main :: IO ()
main = do
  args <- getArgs

  case getOpt Permute flags args of
	(cli,_,[]) | FlagHelp `elem` cli -> do
	   prog <- getProgramName
	   bye (usageInfo (usageHeader prog) flags)
	(cli,_,[]) | FlagVersion `elem` cli ->
	   bye ourCopyright
	(cli,nonopts,[]) ->
	   runit cli nonopts
	(_,_,errors) -> tryOldCmdLine errors args

-- If the new command-line syntax fails, then we try the old.  If that
-- fails too, then we output the original errors and the new syntax
-- (so the old syntax is still available, but hidden).
tryOldCmdLine :: [String] -> [String] -> IO ()
tryOldCmdLine errors args = do
  case getOpt Permute oldFlags args of
	(cli@(_:_),[],[]) -> 
	   oldRunit cli
	_failed -> do
	   prog <- getProgramName
	   die (concat errors ++ usageInfo (usageHeader prog) flags)

-- -----------------------------------------------------------------------------
-- Command-line syntax

data Flag
  = FlagUser
  | FlagGlobal
  | FlagHelp
  | FlagVersion
  | FlagConfig	FilePath
  | FlagGlobalConfig FilePath
  | FlagForce
  | FlagForceFiles
  | FlagAutoGHCiLibs
  | FlagDefinedName String String
  | FlagSimpleOutput
  deriving Eq

flags :: [OptDescr Flag]
flags = [
  Option [] ["user"] (NoArg FlagUser)
	"use the current user's package database",
  Option [] ["global"] (NoArg FlagGlobal)
	"(default) use the global package database",
  Option ['f'] ["package-conf"] (ReqArg FlagConfig "FILE")
	"act upon specified package config file (only)",
  Option [] ["global-conf"] (ReqArg FlagGlobalConfig "FILE")
	"location of the global package config",
  Option [] ["force"] (NoArg FlagForce)
 	"ignore missing dependencies, directories, and libraries",
  Option [] ["force-files"] (NoArg FlagForceFiles)
 	"ignore missing directories and libraries only",
  Option ['g'] ["auto-ghci-libs"] (NoArg FlagAutoGHCiLibs)
	"automatically build libs for GHCi (with register)",
  Option ['?'] ["help"] (NoArg FlagHelp)
	"display this help and exit",
  Option ['D'] ["define-name"] (ReqArg toDefined "NAME=VALUE")
  	"define NAME as VALUE",
  Option ['V'] ["version"] (NoArg FlagVersion)
	"output version information and exit",
  Option [] ["simple-output"] (NoArg FlagSimpleOutput)
        "print output in easy-to-parse format for some commands"
  ]
 where
  toDefined str = 
    case break (=='=') str of
      (nm,[])    -> FlagDefinedName nm []
      (nm,_:val) -> FlagDefinedName nm val

ourCopyright :: String
ourCopyright = "GHC package manager version " ++ version ++ "\n"

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
  "  $p latest pkg\n" ++
  "    Prints the highest registered version of a package.\n" ++
  "\n" ++
  "  $p check\n" ++
  "    Check the consistency of package depenencies and list broken packages.\n" ++
  "    Accepts the --simple-output flag.\n" ++
  "\n" ++
  "  $p describe {pkg-id}\n" ++
  "    Give the registered description for the specified package. The\n" ++
  "    description is returned in precisely the syntax required by $p\n" ++
  "    register.\n" ++
  "\n" ++
  "  $p field {pkg-id} {field}\n" ++
  "    Extract the specified field of the package description for the\n" ++
  "    specified package.\n" ++
  "\n" ++
  " The following optional flags are also accepted:\n"

substProg :: String -> String -> String
substProg _ [] = []
substProg prog ('$':'p':xs) = prog ++ substProg prog xs
substProg prog (c:xs) = c : substProg prog xs

-- -----------------------------------------------------------------------------
-- Do the business

data Force = ForceAll | ForceFiles | NoForce

runit :: [Flag] -> [String] -> IO ()
runit cli nonopts = do
  prog <- getProgramName
  let
        force 
          | FlagForce `elem` cli        = ForceAll 
          | FlagForceFiles `elem` cli   = ForceFiles
          | otherwise                   = NoForce
	auto_ghci_libs = FlagAutoGHCiLibs `elem` cli
        defines = [ (nm,val) | FlagDefinedName nm val <- cli ]
  --
  -- first, parse the command
  case nonopts of
    ["register", filename] -> 
	registerPackage filename defines cli auto_ghci_libs False force
    ["update", filename] -> 
	registerPackage filename defines cli auto_ghci_libs True force
    ["unregister", pkgid_str] -> do
	pkgid <- readGlobPkgId pkgid_str
	unregisterPackage pkgid cli
    ["expose", pkgid_str] -> do
	pkgid <- readGlobPkgId pkgid_str
	exposePackage pkgid cli
    ["hide",   pkgid_str] -> do
	pkgid <- readGlobPkgId pkgid_str
	hidePackage pkgid cli
    ["list"] -> do
	listPackages cli Nothing
    ["list", pkgid_str] -> do
	pkgid <- readGlobPkgId pkgid_str
	listPackages cli (Just pkgid)
    ["latest", pkgid_str] -> do
	pkgid <- readGlobPkgId pkgid_str
	latestPackage cli pkgid
    ["describe", pkgid_str] -> do
	pkgid <- readGlobPkgId pkgid_str
	describePackage cli pkgid
    ["field", pkgid_str, field] -> do
	pkgid <- readGlobPkgId pkgid_str
	describeField cli pkgid field
    ["check"] -> do
	checkConsistency cli
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

readPkgId :: String -> IO PackageIdentifier
readPkgId str = parseCheck parsePackageId str "package identifier"

readGlobPkgId :: String -> IO PackageIdentifier
readGlobPkgId str = parseCheck parseGlobPackageId str "package identifier"

parseGlobPackageId :: ReadP r PackageIdentifier
parseGlobPackageId = 
  parsePackageId
     +++
  (do n <- parsePackageName; string "-*"
      return (PackageIdentifier{ pkgName = n, pkgVersion = globVersion }))

-- globVersion means "all versions"
globVersion :: Version
globVersion = Version{ versionBranch=[], versionTags=["*"] }

-- -----------------------------------------------------------------------------
-- Package databases

-- Some commands operate on a single database:
--	register, unregister, expose, hide
-- however these commands also check the union of the available databases
-- in order to check consistency.  For example, register will check that
-- dependencies exist before registering a package.
--
-- Some commands operate  on multiple databases, with overlapping semantics:
--	list, describe, field

type PackageDBName  = FilePath
type PackageDB      = [InstalledPackageInfo]

type PackageDBStack = [(PackageDBName,PackageDB)]
	-- A stack of package databases.  Convention: head is the topmost
	-- in the stack.  Earlier entries override later one.

getPkgDatabases :: Bool -> [Flag] -> IO PackageDBStack
getPkgDatabases modify flags = do
  -- first we determine the location of the global package config.  On Windows,
  -- this is found relative to the ghc-pkg.exe binary, whereas on Unix the
  -- location is passed to the binary using the --global-config flag by the
  -- wrapper script.
  let err_msg = "missing --global-conf option, location of global package.conf unknown\n"
  global_conf <- 
     case [ f | FlagGlobalConfig f <- flags ] of
	[] -> do mb_dir <- getExecDir "/bin/ghc-pkg.exe"
		 case mb_dir of
			Nothing  -> die err_msg
			Just dir -> return (dir `joinFileName` "package.conf")
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

  -- get the location of the user package database, and create it if necessary
  appdir <- getAppUserDataDirectory "ghc"

  let
	subdir = targetARCH ++ '-':targetOS ++ '-':version
	archdir   = appdir `joinFileName` subdir
	user_conf = archdir `joinFileName` "package.conf"
  user_exists <- doesFileExist user_conf

  -- If the user database doesn't exist, and this command isn't a
  -- "modify" command, then we won't attempt to create or use it.
  let sys_databases
	| modify || user_exists = user_conf : global_confs ++ [global_conf]
	| otherwise             = global_confs ++ [global_conf]

  e_pkg_path <- try (getEnv "GHC_PACKAGE_PATH")
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

  -- -f flags on the command line add to the database stack, unless any
  -- of them are present in the stack already.
  let flag_stack = filter (`notElem` env_stack) 
			[ f | FlagConfig f <- reverse flags ] ++ env_stack

  -- Now we have the full stack of databases.  Next, if the current
  -- command is a "modify" type command, then we truncate the stack
  -- so that the topmost element is the database being modified.
  final_stack <-
     if not modify 
        then return flag_stack
	else let
	      	go (FlagUser : fs)     = modifying user_conf
	      	go (FlagGlobal : fs)   = modifying virt_global_conf
	      	go (FlagConfig f : fs) = modifying f
	      	go (_ : fs)            = go fs
	      	go []                  = modifying virt_global_conf
		
		modifying f 
		  | f `elem` flag_stack = return (dropWhile (/= f) flag_stack)
		  | otherwise           = die ("requesting modification of database:\n\t" ++ f ++ "\n\twhich is not in the database stack.")
	     in
		go flags

  db_stack <- mapM readParseDatabase final_stack
  return db_stack

readParseDatabase :: PackageDBName -> IO (PackageDBName,PackageDB)
readParseDatabase filename = do
  str <- readFile filename `Exception.catch` \_ -> return emptyPackageConfig
  let packages = read str
  Exception.evaluate packages
    `Exception.catch` \_ -> 
	die (filename ++ ": parse error in package config file")
  return (filename,packages)

emptyPackageConfig :: String
emptyPackageConfig = "[]"

-- -----------------------------------------------------------------------------
-- Registering

registerPackage :: FilePath
		-> [(String,String)] -- defines
	        -> [Flag]
		-> Bool		-- auto_ghci_libs
		-> Bool		-- update
		-> Force
		-> IO ()
registerPackage input defines flags auto_ghci_libs update force = do
  db_stack <- getPkgDatabases True flags
  let
	db_to_operate_on = my_head "db" db_stack
	db_filename	 = fst db_to_operate_on
  --

  s <-
    case input of
      "-" -> do
	putStr "Reading package info from stdin ... "
        getContents
      f   -> do
        putStr ("Reading package info from " ++ show f ++ " ... ")
	readFile f

  expanded <- expandEnvVars s defines force

  pkg0 <- parsePackageInfo expanded defines
  putStrLn "done."

  let pkg = resolveDeps db_stack pkg0
  validatePackageConfig pkg db_stack auto_ghci_libs update force
  let new_details = filter not_this (snd db_to_operate_on) ++ [pkg]
      not_this p = package p /= package pkg
  savingOldConfig db_filename $
    writeNewConfig db_filename new_details

parsePackageInfo
	:: String
	-> [(String,String)]
	-> IO InstalledPackageInfo
parsePackageInfo str defines =
  case parseInstalledPackageInfo str of
    ParseOk _warns ok -> return ok
    ParseFailed err -> die (showError err)

-- -----------------------------------------------------------------------------
-- Exposing, Hiding, Unregistering are all similar

exposePackage :: PackageIdentifier ->  [Flag] -> IO ()
exposePackage = modifyPackage (\p -> [p{exposed=True}])

hidePackage :: PackageIdentifier ->  [Flag] -> IO ()
hidePackage = modifyPackage (\p -> [p{exposed=False}])

unregisterPackage :: PackageIdentifier ->  [Flag] -> IO ()
unregisterPackage = modifyPackage (\p -> [])

modifyPackage
  :: (InstalledPackageInfo -> [InstalledPackageInfo])
  -> PackageIdentifier
  -> [Flag]
  -> IO ()
modifyPackage fn pkgid flags  = do
  db_stack <- getPkgDatabases True{-modify-} flags
  let ((db_name, pkgs) : _) = db_stack
  ps <- findPackages [(db_name,pkgs)] pkgid
  let pids = map package ps
  let new_config = concat (map modify pkgs)
      modify pkg
  	  | package pkg `elem` pids = fn pkg
  	  | otherwise               = [pkg]
  savingOldConfig db_name $
      writeNewConfig db_name new_config

-- -----------------------------------------------------------------------------
-- Listing packages

listPackages ::  [Flag] -> Maybe PackageIdentifier -> IO ()
listPackages flags mPackageName = do
  let simple_output = FlagSimpleOutput `elem` flags
  db_stack <- getPkgDatabases False flags
  let db_stack_filtered -- if a package is given, filter out all other packages
        | Just this <- mPackageName =
            map (\(conf,pkgs) -> (conf, filter (this `matchesPkg`) pkgs)) 
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

      pkg_map = map (\p -> (package p, p)) $ concatMap snd db_stack
      show_func = if simple_output then show_simple else mapM_ (show_normal pkg_map)

  show_func (reverse db_stack_sorted)

  where show_normal pkg_map (db_name,pkg_confs) =
	  hPutStrLn stdout (render $
		text db_name <> colon $$ nest 4 packages
		)
	   where packages = fsep (punctuate comma (map pp_pkg pkg_confs))
		 pp_pkg p
                   | isBrokenPackage p pkg_map = braces doc
		   | exposed p = doc
		   | otherwise = parens doc
		   where doc = text (showPackageId (package p))

        show_simple db_stack = do
          let pkgs = map showPackageId $ sortBy compPkgIdVer $
                          map package (concatMap snd db_stack)
          when (null pkgs) $ die "no matches"
          hPutStrLn stdout $ concat $ intersperse " " pkgs

-- -----------------------------------------------------------------------------
-- Prints the highest (hidden or exposed) version of a package

latestPackage ::  [Flag] -> PackageIdentifier -> IO ()
latestPackage flags pkgid = do
  db_stack <- getPkgDatabases False flags
  ps <- findPackages db_stack pkgid
  show_pkg (sortBy compPkgIdVer (map package ps))
  where
    show_pkg [] = die "no matches"
    show_pkg pids = hPutStrLn stdout (showPackageId (last pids))

-- -----------------------------------------------------------------------------
-- Describe

describePackage :: [Flag] -> PackageIdentifier -> IO ()
describePackage flags pkgid = do
  db_stack <- getPkgDatabases False flags
  ps <- findPackages db_stack pkgid
  mapM_ (putStrLn . showInstalledPackageInfo) ps

-- PackageId is can have globVersion for the version
findPackages :: PackageDBStack -> PackageIdentifier -> IO [InstalledPackageInfo]
findPackages db_stack pkgid
  = case [ p | p <- all_pkgs, pkgid `matchesPkg` p ] of
	[]  -> die ("cannot find package " ++ showPackageId pkgid)
	ps -> return ps
  where
	all_pkgs = concat (map snd db_stack)

matches :: PackageIdentifier -> PackageIdentifier -> Bool
pid `matches` pid'
  = (pkgName pid == pkgName pid')
    && (pkgVersion pid == pkgVersion pid' || not (realVersion pid))

matchesPkg :: PackageIdentifier -> InstalledPackageInfo -> Bool
pid `matchesPkg` pkg = pid `matches` package pkg

compPkgIdVer :: PackageIdentifier -> PackageIdentifier -> Ordering
compPkgIdVer p1 p2 = pkgVersion p1 `compare` pkgVersion p2

-- -----------------------------------------------------------------------------
-- Field

describeField :: [Flag] -> PackageIdentifier -> String -> IO ()
describeField flags pkgid field = do
  db_stack <- getPkgDatabases False flags
  case toField field of
    Nothing -> die ("unknown field: " ++ field)
    Just fn -> do
	ps <- findPackages db_stack pkgid 
	mapM_ (putStrLn.fn) ps

toField :: String -> Maybe (InstalledPackageInfo -> String)
-- backwards compatibility:
toField "import_dirs"     = Just $ strList . importDirs
toField "source_dirs"     = Just $ strList . importDirs
toField "library_dirs"    = Just $ strList . libraryDirs
toField "hs_libraries"    = Just $ strList . hsLibraries
toField "extra_libraries" = Just $ strList . extraLibraries
toField "include_dirs"    = Just $ strList . includeDirs
toField "c_includes"      = Just $ strList . includes
toField "package_deps"    = Just $ strList . map showPackageId. depends
toField "extra_cc_opts"   = Just $ strList . ccOptions
toField "extra_ld_opts"   = Just $ strList . ldOptions
toField "framework_dirs"  = Just $ strList . frameworkDirs  
toField "extra_frameworks"= Just $ strList . frameworks  
toField s 	 	  = showInstalledPackageInfoField s

strList :: [String] -> String
strList = show


-- -----------------------------------------------------------------------------
-- Check: Check consistency of installed packages

checkConsistency :: [Flag] -> IO ()
checkConsistency flags = do
  db_stack <- getPkgDatabases False flags
  let pkgs = map (\p -> (package p, p)) $ concatMap snd db_stack
      broken_pkgs = do
        (pid, p) <- pkgs
        let broken_deps = missingPackageDeps p pkgs
        guard (not . null $ broken_deps)
        return (pid, broken_deps)
  mapM_ (putStrLn . render . show_func) broken_pkgs
  where
  show_func | FlagSimpleOutput `elem` flags = show_simple
            | otherwise = show_normal
  show_simple (pid,deps) =
    text (showPackageId pid) <> colon
      <+> fsep (punctuate comma (map (text . showPackageId) deps))
  show_normal (pid,deps) =
    text "package" <+> text (showPackageId pid) <+> text "has missing dependencies:"
      $$ nest 4 (fsep (punctuate comma (map (text . showPackageId) deps)))

missingPackageDeps :: InstalledPackageInfo
                   -> [(PackageIdentifier, InstalledPackageInfo)]
                   -> [PackageIdentifier]
missingPackageDeps pkg pkg_map =
  [ d | d <- depends pkg, isNothing (lookup d pkg_map)] ++
  [ d | d <- depends pkg, Just p <- return (lookup d pkg_map), isBrokenPackage p pkg_map]

isBrokenPackage :: InstalledPackageInfo -> [(PackageIdentifier, InstalledPackageInfo)] -> Bool
isBrokenPackage pkg pkg_map = not . null $ missingPackageDeps pkg pkg_map


-- -----------------------------------------------------------------------------
-- Manipulating package.conf files

writeNewConfig :: FilePath -> [InstalledPackageInfo] -> IO ()
writeNewConfig filename packages = do
  hPutStr stdout "Writing new package config file... "
  createDirectoryIfMissing True $ getFilenameDir filename
  h <- openFile filename WriteMode `catch` \e ->
      if isPermissionError e
      then die (filename ++ ": you don't have permission to modify this file")
      else ioError e
  hPutStrLn h (show packages)
  hClose h
  hPutStrLn stdout "done."

savingOldConfig :: FilePath -> IO () -> IO ()
savingOldConfig filename io = Exception.block $ do
  hPutStr stdout "Saving old package config file... "
    -- mv rather than cp because we've already done an hGetContents
    -- on this file so we won't be able to open it for writing
    -- unless we move the old one out of the way...
  let oldFile = filename ++ ".old"
  restore_on_error <- catch (renameFile filename oldFile >> return True) $
      \err -> do
          unless (isDoesNotExistError err) $ do
              hPutStrLn stderr (unwords ["Unable to rename", show filename,
                                         "to", show oldFile])
              ioError err
          return False
  hPutStrLn stdout "done."
  io `catch` \e -> do
      hPutStrLn stderr (show e)
      hPutStr stdout ("\nWARNING: an error was encountered while writing"
                   ++ "the new configuration.\n")
      when restore_on_error $ do
          hPutStr stdout "Attempting to restore the old configuration..."
          do renameFile oldFile filename
             hPutStrLn stdout "done."
           `catch` \err -> hPutStrLn stdout ("Failed: " ++ show err)
      ioError e

-----------------------------------------------------------------------------
-- Sanity-check a new package config, and automatically build GHCi libs
-- if requested.

validatePackageConfig :: InstalledPackageInfo
		      -> PackageDBStack
		      -> Bool	-- auto-ghc-libs
		      -> Bool	-- update
		      -> Force
		      -> IO ()
validatePackageConfig pkg db_stack auto_ghci_libs update force = do
  checkPackageId pkg
  checkDuplicates db_stack pkg update
  mapM_	(checkDep db_stack force) (depends pkg)
  mapM_	(checkDir force) (importDirs pkg)
  mapM_	(checkDir force) (libraryDirs pkg)
  mapM_	(checkDir force) (includeDirs pkg)
  mapM_ (checkHSLib (libraryDirs pkg) auto_ghci_libs force) (hsLibraries pkg)
  -- ToDo: check these somehow?
  --	extra_libraries :: [String],
  --	c_includes      :: [String],

-- When the package name and version are put together, sometimes we can
-- end up with a package id that cannot be parsed.  This will lead to 
-- difficulties when the user wants to refer to the package later, so
-- we check that the package id can be parsed properly here.
checkPackageId :: InstalledPackageInfo -> IO ()
checkPackageId ipi =
  let str = showPackageId (package ipi) in
  case [ x | (x,ys) <- readP_to_S parsePackageId str, all isSpace ys ] of
    [_] -> return ()
    []  -> die ("invalid package identifier: " ++ str)
    _   -> die ("ambiguous package identifier: " ++ str)

resolveDeps :: PackageDBStack -> InstalledPackageInfo -> InstalledPackageInfo
resolveDeps db_stack p = updateDeps p
  where
	-- The input package spec is allowed to give a package dependency
	-- without a version number; e.g.
	--	depends: base
	-- Here, we update these dependencies without version numbers to
	-- match the actual versions of the relevant packages installed.
	updateDeps p = p{depends = map resolveDep (depends p)}

	resolveDep dep_pkgid
	   | realVersion dep_pkgid  = dep_pkgid
	   | otherwise		    = lookupDep dep_pkgid

	lookupDep dep_pkgid
	   = let 
		name = pkgName dep_pkgid
	     in
	     case [ pid | p <- concat (map snd db_stack), 
			  let pid = package p,
			  pkgName pid == name ] of
		(pid:_) -> pid		-- Found installed package,
					-- replete with its version
		[]	-> dep_pkgid	-- No installed package; use 
					-- the version-less one

checkDuplicates :: PackageDBStack -> InstalledPackageInfo -> Bool -> IO ()
checkDuplicates db_stack pkg update = do
  let
	pkgid = package pkg
	(_top_db_name, pkgs) : _  = db_stack
  --
  -- Check whether this package id already exists in this DB
  --
  when (not update && (pkgid `elem` map package pkgs)) $
       die ("package " ++ showPackageId pkgid ++ " is already installed")



checkDir :: Force -> String -> IO ()
checkDir force d
 | "$topdir" `isPrefixOf` d = return ()
	-- can't check this, because we don't know what $topdir is
 | otherwise = do
   there <- doesDirectoryExist d
   when (not there)
       (dieOrForceFile force (d ++ " doesn't exist or isn't a directory"))

checkDep :: PackageDBStack -> Force -> PackageIdentifier -> IO ()
checkDep db_stack force pkgid
  | pkgid `elem` pkgids || (not real_version && name_exists) = return ()
  | otherwise = dieOrForceAll force ("dependency " ++ showPackageId pkgid
					++ " doesn't exist")
  where
	-- for backwards compat, we treat 0.0 as a special version,
	-- and don't check that it actually exists.
 	real_version = realVersion pkgid
	
        name_exists = any (\p -> pkgName (package p) == name) all_pkgs
        name = pkgName pkgid

	all_pkgs = concat (map snd db_stack)
	pkgids = map package all_pkgs

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
 | "$topdir" `isPrefixOf` d = return True
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
        ms <- searchDir (path `joinFilename` f) (prefix `joinModule` f)
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

-- -----------------------------------------------------------------------------
-- The old command-line syntax, supported for backwards compatibility

data OldFlag 
  = OF_Config FilePath
  | OF_Input FilePath
  | OF_List
  | OF_ListLocal
  | OF_Add Bool {- True => replace existing info -}
  | OF_Remove String | OF_Show String 
  | OF_Field String | OF_AutoGHCiLibs | OF_Force
  | OF_DefinedName String String
  | OF_GlobalConfig FilePath
  deriving (Eq)

isAction :: OldFlag -> Bool
isAction OF_Config{}        = False
isAction OF_Field{}         = False
isAction OF_Input{}         = False
isAction OF_AutoGHCiLibs{}  = False
isAction OF_Force{}	    = False
isAction OF_DefinedName{}   = False
isAction OF_GlobalConfig{}  = False
isAction _                  = True

oldFlags :: [OptDescr OldFlag]
oldFlags = [
  Option ['f'] ["config-file"] (ReqArg OF_Config "FILE")
	"use the specified package config file",
  Option ['l'] ["list-packages"] (NoArg OF_List)
 	"list packages in all config files",
  Option ['L'] ["list-local-packages"] (NoArg OF_ListLocal)
 	"list packages in the specified config file",
  Option ['a'] ["add-package"] (NoArg (OF_Add False))
 	"add a new package",
  Option ['u'] ["update-package"] (NoArg (OF_Add True))
 	"update package with new configuration",
  Option ['i'] ["input-file"] (ReqArg OF_Input "FILE")
	"read new package info from specified file",
  Option ['s'] ["show-package"] (ReqArg OF_Show "NAME")
 	"show the configuration for package NAME",
  Option [] ["field"] (ReqArg OF_Field "FIELD")
 	"(with --show-package) Show field FIELD only",
  Option [] ["force"] (NoArg OF_Force)
 	"ignore missing directories/libraries",
  Option ['r'] ["remove-package"] (ReqArg OF_Remove "NAME")
 	"remove an installed package",
  Option ['g'] ["auto-ghci-libs"] (NoArg OF_AutoGHCiLibs)
	"automatically build libs for GHCi (with -a)",
  Option ['D'] ["define-name"] (ReqArg toDefined "NAME=VALUE")
  	"define NAME as VALUE",
  Option [] ["global-conf"] (ReqArg OF_GlobalConfig "FILE")
	"location of the global package config"
  ]
 where
  toDefined str = 
    case break (=='=') str of
      (nm,[]) -> OF_DefinedName nm []
      (nm,_:val) -> OF_DefinedName nm val

oldRunit :: [OldFlag] -> IO ()
oldRunit clis = do
  let new_flags = [ f | Just f <- map conv clis ]

      conv (OF_GlobalConfig f) = Just (FlagGlobalConfig f)
      conv (OF_Config f)       = Just (FlagConfig f)
      conv _                   = Nothing

  

  let fields = [ f | OF_Field f <- clis ]

  let auto_ghci_libs = any isAuto clis 
	 where isAuto OF_AutoGHCiLibs = True; isAuto _ = False
      input_file = my_head "inp" ([ f | (OF_Input f) <- clis] ++ ["-"])

      force = if OF_Force `elem` clis then ForceAll else NoForce
      
      defines = [ (nm,val) | OF_DefinedName nm val <- clis ]

  case [ c | c <- clis, isAction c ] of
    [ OF_List ]      -> listPackages new_flags Nothing
    [ OF_ListLocal ] -> listPackages new_flags Nothing
    [ OF_Add upd ]   -> 
	registerPackage input_file defines new_flags auto_ghci_libs upd force
    [ OF_Remove pkgid_str ]  -> do
	pkgid <- readPkgId pkgid_str
	unregisterPackage pkgid new_flags
    [ OF_Show pkgid_str ]
	| null fields -> do
		pkgid <- readPkgId pkgid_str
		describePackage new_flags pkgid
	| otherwise   -> do
		pkgid <- readPkgId pkgid_str
		mapM_ (describeField new_flags pkgid) fields
    _ -> do 
	prog <- getProgramName
	die (usageInfo (usageHeader prog) flags)

my_head :: String -> [a] -> a
my_head s [] = error s
my_head s (x:xs) = x

-- ---------------------------------------------------------------------------
-- expanding environment variables in the package configuration

expandEnvVars :: String -> [(String, String)] -> Force -> IO String
expandEnvVars str defines force = go str ""
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
     case lookup nm defines of
       Just x | not (null x) -> return x
       _      -> 
	catch (System.getEnv nm)
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
die s = do 
  hFlush stdout
  prog <- getProgramName
  hPutStrLn stderr (prog ++ ": " ++ s)
  exitWith (ExitFailure 1)

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

-----------------------------------------
--	Cut and pasted from ghc/compiler/SysTools

#if defined(mingw32_HOST_OS)
subst a b ls = map (\ x -> if x == a then b else x) ls
unDosifyPath xs = subst '\\' '/' xs

getExecDir :: String -> IO (Maybe String)
-- (getExecDir cmd) returns the directory in which the current
--	  	    executable, which should be called 'cmd', is running
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

-- -----------------------------------------------------------------------------
-- FilePath utils

-- | The 'joinFileName' function is the opposite of 'splitFileName'. 
-- It joins directory and file names to form a complete file path.
--
-- The general rule is:
--
-- > dir `joinFileName` basename == path
-- >   where
-- >     (dir,basename) = splitFileName path
--
-- There might be an exceptions to the rule but in any case the
-- reconstructed path will refer to the same object (file or directory).
-- An example exception is that on Windows some slashes might be converted
-- to backslashes.
joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (last dir) = dir++fname
  | otherwise                  = dir++pathSeparator:fname

-- | Checks whether the character is a valid path separator for the host
-- platform. The valid character is a 'pathSeparator' but since the Windows
-- operating system also accepts a slash (\"\/\") since DOS 2, the function
-- checks for it on this platform, too.
isPathSeparator :: Char -> Bool
isPathSeparator ch = ch == pathSeparator || ch == '/'

-- | Provides a platform-specific character used to separate directory levels in
-- a path string that reflects a hierarchical file system organization. The
-- separator is a slash (@\"\/\"@) on Unix and Macintosh, and a backslash
-- (@\"\\\"@) on the Windows operating system.
pathSeparator :: Char
#ifdef mingw32_HOST_OS
pathSeparator = '\\'
#else
pathSeparator = '/'
#endif

getFilenameDir :: FilePath -> FilePath
getFilenameDir fn = case break isPathSeparator (reverse fn) of
                        (xs, "") -> "."
                        (_, sep:ys) -> reverse ys

-- | The function splits the given string to substrings
-- using the 'searchPathSeparator'.
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

        (chunk', rest') = break (==searchPathSeparator) s

-- | A platform-specific character used to separate search path strings in 
-- environment variables. The separator is a colon (\":\") on Unix and Macintosh, 
-- and a semicolon (\";\") on the Windows operating system.
searchPathSeparator :: Char
#if mingw32_HOST_OS || mingw32_TARGET_OS
searchPathSeparator = ';'
#else
searchPathSeparator = ':'
#endif

