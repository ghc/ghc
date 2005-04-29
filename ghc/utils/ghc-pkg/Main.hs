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
#else
import GetOpt
import Pretty
import qualified Exception
#endif

import Data.Char	( isSpace )
import Monad
import Directory
import System	( getArgs, getProgName, getEnv,
		  exitWith, ExitCode(..)
		)
import System.IO
import Data.List ( isPrefixOf, isSuffixOf, intersperse )

#ifdef mingw32_HOST_OS
import Foreign

#if __GLASGOW_HASKELL__ >= 504
import Foreign.C.String
#else
import CString
#endif
#endif

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
  | FlagAutoGHCiLibs
  | FlagDefinedName String String
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
  Option ['g'] ["auto-ghci-libs"] (NoArg FlagAutoGHCiLibs)
	"automatically build libs for GHCi (with register)",
  Option ['?'] ["help"] (NoArg FlagHelp)
	"display this help and exit",
  Option ['D'] ["define-name"] (ReqArg toDefined "NAME=VALUE")
  	"define NAME as VALUE",
  Option ['V'] ["version"] (NoArg FlagVersion)
	"output version information and exit"
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
  "  $p list\n" ++
  "    List registered packages in the global database, and also the" ++
  "    user database if --user is given.\n" ++
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

runit :: [Flag] -> [String] -> IO ()
runit cli nonopts = do
  prog <- getProgramName
  let
	force = FlagForce `elem` cli
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
	listPackages cli
    ["describe", pkgid_str] -> do
	pkgid <- readGlobPkgId pkgid_str
	describePackage cli pkgid
    ["field", pkgid_str, field] -> do
	pkgid <- readGlobPkgId pkgid_str
	describeField cli pkgid field
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

  -- get the location of the user package database, and create it if necessary
  appdir <- getAppUserDataDirectory "ghc"

  let
	subdir = targetARCH ++ '-':targetOS ++ '-':version
	archdir   = appdir `joinFileName` subdir
	user_conf = archdir `joinFileName` "package.conf"
  user_exists <- doesFileExist user_conf

  let
	-- The semantics here are slightly strange.  If we are
	-- *modifying* the database, then the default is to modify
	-- the global database by default, unless you say --user.
	-- If we are not modifying (eg. list, describe etc.) then
	-- the user database is included by default.
	databases
	  | modify          = foldl addDB [global_conf] flags
	  | not user_exists = foldl addDB [global_conf] flags
	  | otherwise       = foldl addDB [user_conf,global_conf] flags

	-- implement the following rules:
	-- 	--user means overlap with the user database
	-- 	--global means reset to just the global database
	--	-f <file> means overlap with <file>
	addDB dbs FlagUser
	   | user_conf `elem` dbs     = dbs
	   | modify || user_exists    = user_conf : dbs
	addDB dbs FlagGlobal     = [global_conf]
	addDB dbs (FlagConfig f) = f : dbs
	addDB dbs _		 = dbs

  -- we create the user database iff (a) we're modifying, and (b) the
  -- user asked to use it by giving the --user flag.
  when (not user_exists && user_conf `elem` databases) $ do
	putStrLn ("Creating user package database in " ++ user_conf)
	createDirectoryIfMissing True archdir
	writeFile user_conf emptyPackageConfig

  db_stack <- mapM readParseDatabase databases
  return db_stack

readParseDatabase :: PackageDBName -> IO (PackageDBName,PackageDB)
readParseDatabase filename = do
  str <- readFile filename
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
		-> Bool		-- force
		-> IO ()
registerPackage input defines flags auto_ghci_libs update force = do
  db_stack <- getPkgDatabases True flags
  let
	db_to_operate_on = my_head "db" db_stack
	db_filename	 = fst db_to_operate_on
  --
  checkConfigAccess db_filename

  s <-
    case input of
      "-" -> do
	putStr "Reading package info from stdin ... "
        getContents
      f   -> do
        putStr ("Reading package info from " ++ show f ++ " ... ")
	readFile f

  expanded <- expandEnvVars s defines force

  pkg <- parsePackageInfo expanded defines force
  putStrLn "done."

  validatePackageConfig pkg db_stack auto_ghci_libs update force
  new_details <- updatePackageDB db_stack (snd db_to_operate_on) pkg
  savePackageConfig db_filename
  maybeRestoreOldConfig db_filename $
    writeNewConfig db_filename new_details

parsePackageInfo
	:: String
	-> [(String,String)]
	-> Bool
	-> IO InstalledPackageInfo
parsePackageInfo str defines force =
  case parseInstalledPackageInfo str of
    ParseOk ok -> return ok
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
  checkConfigAccess db_name
  ps <- findPackages [(db_name,pkgs)] pkgid
  let pids = map package ps
  savePackageConfig db_name
  let new_config = concat (map modify pkgs)
      modify pkg
  	  | package pkg `elem` pids = fn pkg
  	  | otherwise               = [pkg]
  maybeRestoreOldConfig db_name $
      writeNewConfig db_name new_config

-- -----------------------------------------------------------------------------
-- Listing packages

listPackages ::  [Flag] -> IO ()
listPackages flags = do
  db_stack <- getPkgDatabases False flags
  mapM_ show_pkgconf (reverse db_stack)
  where show_pkgconf (db_name,pkg_confs) =
	  hPutStrLn stdout (render $
		text (db_name ++ ":") $$ nest 4 packages
		)
	   where packages = fsep (punctuate comma (map pp_pkg pkg_confs))
		 pp_pkg p
		   | exposed p = doc
		   | otherwise = parens doc
		   where doc = text (showPackageId (package p))

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
  = case [ p | p <- all_pkgs, pkgid `matches` p ] of
	[]  -> die ("cannot find package " ++ showPackageId pkgid)
	[p] -> return [p]
	 -- if the version is globVersion, then we are allowed to match
	 -- multiple packages.  So eg. "Cabal-*" matches all Cabal packages,
	 -- but "Cabal" matches just one Cabal package - if there are more,
	 -- you get an error.
	ps | versionTags (pkgVersion pkgid) == versionTags globVersion
	   -> return ps
	   | otherwise
	   -> die ("package " ++ showPackageId pkgid ++ 
			" matches multiple packages: " ++ 
			concat (intersperse ", " (
				 map (showPackageId.package) ps)))
  where
	pid `matches` pkg
	  = (pkgName pid == pkgName p)
	    && (pkgVersion pid == pkgVersion p || not (realVersion pid))
	  where p = package pkg

	all_pkgs = concat (map snd db_stack)

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
-- Manipulating package.conf files

checkConfigAccess :: FilePath -> IO ()
checkConfigAccess filename = do
  access <- getPermissions filename
  when (not (writable access))
      (die (filename ++ ": you don't have permission to modify this file"))

maybeRestoreOldConfig :: FilePath -> IO () -> IO ()
maybeRestoreOldConfig filename io
  = io `catch` \e -> do
	hPutStrLn stderr (show e)
        hPutStr stdout ("\nWARNING: an error was encountered while the new \n"++
        	          "configuration was being written.  Attempting to \n"++
        	          "restore the old configuration... ")
	renameFile (filename ++ ".old")  filename
        hPutStrLn stdout "done."
	ioError e

writeNewConfig :: FilePath -> [InstalledPackageInfo] -> IO ()
writeNewConfig filename packages = do
  hPutStr stdout "Writing new package config file... "
  h <- openFile filename WriteMode
  hPutStrLn h (show packages)
  hClose h
  hPutStrLn stdout "done."

savePackageConfig :: FilePath -> IO ()
savePackageConfig filename = do
  hPutStr stdout "Saving old package config file... "
    -- mv rather than cp because we've already done an hGetContents
    -- on this file so we won't be able to open it for writing
    -- unless we move the old one out of the way...
  let oldFile = filename ++ ".old"
  doesExist <- doesFileExist oldFile  `catch` (\ _ -> return False)
  when doesExist (removeFile oldFile `catch` (const $ return ()))
  catch (renameFile filename oldFile)
  	(\ err -> do
		hPutStrLn stderr (unwords [ "Unable to rename "
					  , show filename
					  , " to "
					  , show oldFile
					  ])
		ioError err)
  hPutStrLn stdout "done."

-----------------------------------------------------------------------------
-- Sanity-check a new package config, and automatically build GHCi libs
-- if requested.

validatePackageConfig :: InstalledPackageInfo
		      -> PackageDBStack
		      -> Bool	-- auto-ghc-libs
		      -> Bool	-- update
		      -> Bool	-- force
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

checkDuplicates :: PackageDBStack -> InstalledPackageInfo -> Bool -> IO ()
checkDuplicates db_stack pkg update = do
  let
	pkgid = package pkg

	(_top_db_name, pkgs) : _  = db_stack

	pkgs_with_same_name = 
		[ p | p <- pkgs, pkgName (package p) == pkgName pkgid]
	exposed_pkgs_with_same_name =
		filter exposed pkgs_with_same_name
  --
  -- Check whether this package id already exists in this DB
  --
  when (not update && (package pkg `elem` map package pkgs)) $
       die ("package " ++ showPackageId pkgid ++ " is already installed")
  --
  -- if we are exposing this new package, then check that
  -- there are no other exposed packages with the same name.
  --
  when (not update && exposed pkg && not (null exposed_pkgs_with_same_name)) $
	die ("trying to register " ++ showPackageId pkgid 
		  ++ " as exposed, but "
		  ++ showPackageId (package (my_head "when" exposed_pkgs_with_same_name))
		  ++ " is also exposed.")


checkDir :: Bool -> String -> IO ()
checkDir force d
 | "$topdir" `isPrefixOf` d = return ()
	-- can't check this, because we don't know what $topdir is
 | otherwise = do
   there <- doesDirectoryExist d
   when (not there)
       (dieOrForce force (d ++ " doesn't exist or isn't a directory"))

checkDep :: PackageDBStack -> Bool -> PackageIdentifier -> IO ()
checkDep db_stack force pkgid
  | real_version && pkgid `elem` pkgids = return ()
  | not real_version && pkgName pkgid `elem` pkg_names = return ()
  | otherwise = dieOrForce force ("dependency " ++ showPackageId pkgid
					++ " doesn't exist")
  where
	-- for backwards compat, we treat 0.0 as a special version,
	-- and don't check that it actually exists.
 	real_version = realVersion pkgid
	
	all_pkgs = concat (map snd db_stack)
	pkgids = map package all_pkgs
	pkg_names = map pkgName pkgids

realVersion :: PackageIdentifier -> Bool
realVersion pkgid = versionBranch (pkgVersion pkgid) /= []

checkHSLib :: [String] -> Bool -> Bool -> String -> IO ()
checkHSLib dirs auto_ghci_libs force lib = do
  let batch_lib_file = "lib" ++ lib ++ ".a"
  bs <- mapM (doesLibExistIn batch_lib_file) dirs
  case [ dir | (exists,dir) <- zip bs dirs, exists ] of
	[] -> dieOrForce force ("cannot find " ++ batch_lib_file ++
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
-- Updating the DB with the new package.

updatePackageDB
	:: PackageDBStack
	-> [InstalledPackageInfo]
	-> InstalledPackageInfo
	-> IO [InstalledPackageInfo]
updatePackageDB db_stack pkgs new_pkg = do
  let
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

	is_exposed = exposed new_pkg
	pkgid      = package new_pkg
	name       = pkgName pkgid

	pkgs' = [ maybe_hide p | p <- pkgs, package p /= pkgid ]
	
	-- When update is on, and we're exposing the new package,
	-- we hide any packages with the same name (different versions)
	-- in the current DB.  Earlier checks will have failed if
	-- update isn't on.
	maybe_hide p
	  | is_exposed && pkgName (package p) == name = p{ exposed = False }
	  | otherwise = p
  --
  return (pkgs'++[updateDeps new_pkg])

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

      force = OF_Force `elem` clis
      
      defines = [ (nm,val) | OF_DefinedName nm val <- clis ]

  case [ c | c <- clis, isAction c ] of
    [ OF_List ]      -> listPackages new_flags
    [ OF_ListLocal ] -> listPackages new_flags
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

expandEnvVars :: String -> [(String, String)] -> Bool -> IO String
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
	   (\ _ -> do dieOrForce force ("Unable to expand variable " ++ 
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

dieOrForce :: Bool -> String -> IO ()
dieOrForce force s 
  | force     = do hFlush stdout; hPutStrLn stderr (s ++ " (ignoring)")
  | otherwise = die s


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
