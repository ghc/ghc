-----------------------------------------------------------------------------
-- $Id: Main.hs,v 1.21 2002/02/12 15:17:24 simonmar Exp $
--
-- Package management tool
-----------------------------------------------------------------------------

module Main where

import Package

#ifdef __GLASGOW_HASKELL__
import qualified Exception
#endif
import GetOpt
import Pretty
import Monad
import Directory
import System	( getEnv, getArgs, 
		  system, exitWith,
		  ExitCode(..)
		)
import IO
import List ( isPrefixOf )

#include "../../includes/config.h"

#ifdef mingw32_TARGET_OS
import Win32DLL
#endif

main = do
  args <- getArgs

  case getOpt Permute flags args of
	(clis@(_:_),[],[]) -> runit clis
	(_,_,errors) -> die (concat errors ++ 
			     usageInfo usageHeader flags)

data Flag 
  = Config FilePath
  | Input FilePath
  | List | Add Bool {- True => replace existing info -}
  | Remove String | Show String 
  | Field String | AutoGHCiLibs

isAction (Config _)     = False
isAction (Field _)      = False
isAction (Input _)      = False
isAction (AutoGHCiLibs) = False
isAction _              = True

usageHeader = "ghc-pkg [OPTION...]"

flags = [
  Option ['f'] ["config-file"] (ReqArg Config "FILE")
	"Use the specified package config file",
  Option ['l'] ["list-packages"] (NoArg List)
 	"List the currently installed packages",
  Option ['a'] ["add-package"] (NoArg (Add False))
 	"Add a new package",
  Option ['u'] ["update-package"] (NoArg (Add True))
 	"Update package with new configuration",
  Option ['i'] ["input-file"] (ReqArg Input "FILE")
	"Read new package info from specified file",
  Option ['s'] ["show-package"] (ReqArg Show "NAME")
 	"Show the configuration for package NAME",
  Option [] ["field"] (ReqArg Field "FIELD")
 	"(with --show-package) Show field FIELD only",
  Option ['r'] ["remove-package"] (ReqArg Remove "NAME")
 	"Remove an installed package",
  Option ['g'] ["auto-ghci-libs"] (NoArg AutoGHCiLibs)
	"Automatically build libs for GHCi (with -a)"
  ]

#ifdef mingw32_TARGET_OS
subst a b ls = map (\ x -> if x == a then b else x) ls

unDosifyPath xs = subst '\\' '/' xs
#endif

runit clis = do
  conf_file <- 
     case [ f | Config f <- clis ] of
        fs@(_:_)  -> return (last fs)
#ifndef mingw32_TARGET_OS
	[] -> die "missing -f option, location of package.conf unknown"
#else
	[] -> do h <- getModuleHandle Nothing
	  	 n <- getModuleFileName h
		 return (reverse (drop (length "/bin/ghc-pkg.exe") (reverse (unDosifyPath n))) ++ "/package.conf")
#endif

  let toField "import_dirs"     = return import_dirs
      toField "source_dirs"     = return source_dirs
      toField "library_dirs"    = return library_dirs
      toField "hs_libraries"    = return hs_libraries
      toField "extra_libraries" = return extra_libraries
      toField "include_dirs"    = return include_dirs
      toField "c_includes"      = return c_includes
      toField "package_deps"    = return package_deps
      toField "extra_ghc_opts"  = return extra_ghc_opts
      toField "extra_cc_opts"   = return extra_cc_opts
      toField "extra_ld_opts"   = return extra_ld_opts  
      toField s			= die ("unknown field: `" ++ s ++ "'")

  fields <- mapM toField [ f | Field f <- clis ]

  s <- readFile conf_file
  let packages = read s :: [PackageConfig]
  eval_catch packages (\_ -> die "parse error in package config file")

  let auto_ghci_libs = any isAuto clis 
	 where isAuto AutoGHCiLibs = True; isAuto _ = False
      input_file = head ([ f | (Input f) <- clis] ++ ["-"])

  case [ c | c <- clis, isAction c ] of
    [ List ]     -> listPackages packages
    [ Add upd ]  -> addPackage packages conf_file input_file auto_ghci_libs upd
    [ Remove p ] -> removePackage packages conf_file p
    [ Show p ]   -> showPackage packages conf_file p fields
    _            -> die (usageInfo usageHeader flags)


listPackages :: [PackageConfig] -> IO ()
listPackages packages = hPutStrLn stdout (listPkgs packages)

showPackage :: [PackageConfig]
	    -> FilePath
	    -> String
	    -> [PackageConfig -> [String]]
	    -> IO ()
showPackage packages pkgconf pkg_name fields =
  case [ p | p <- packages, name p == pkg_name ] of
    []    -> die ("can't find package `" ++ pkg_name ++ "'")
    [pkg] | null fields -> hPutStrLn stdout (render (dumpPkgGuts pkg))
	  | otherwise   -> hPutStrLn stdout (render (vcat 
				(map (vcat . map text) (map ($pkg) fields))))
    _     -> die "showPackage: internal error"

addPackage :: [PackageConfig] -> FilePath -> FilePath -> Bool -> Bool -> IO ()
addPackage packages pkgconf inputFile auto_ghci_libs updatePkg = do
  checkConfigAccess pkgconf
  s <-
    case inputFile of
      "-" -> do
	hPutStr stdout "Reading package info from stdin... "
        getContents
      f   -> do
        hPutStr stdout ("Reading package info from " ++ show f)
	readFile f
  let new_pkg = read s :: PackageConfig
  eval_catch new_pkg (\_ -> die "parse error in package info")
  hPutStrLn stdout "done."
  hPutStr stdout "Expanding embedded variables..."
  new_exp_pkg <- expandEnvVars new_pkg
  hPutStrLn stdout "done."
  new_details <- validatePackageConfig new_exp_pkg packages auto_ghci_libs updatePkg
  savePackageConfig pkgconf
  maybeRestoreOldConfig pkgconf $
    writeNewConfig pkgconf new_details

removePackage :: [PackageConfig] -> FilePath -> String -> IO ()
removePackage packages pkgconf pkgName = do  
  checkConfigAccess pkgconf
  when (pkgName `notElem` map name packages)
       (die ("package `" ++ pkgName ++ "' not installed"))
  savePackageConfig pkgconf
  maybeRestoreOldConfig pkgconf $
    writeNewConfig pkgconf (filter ((/= pkgName) . name) packages)

checkConfigAccess :: FilePath -> IO ()
checkConfigAccess pkgconf = do
  access <- getPermissions pkgconf
  when (not (writable access))
      (die "you don't have permission to modify the package configuration file")

maybeRestoreOldConfig :: String -> IO () -> IO ()
maybeRestoreOldConfig conf_file io
  = my_catch io (\e -> do
        hPutStr stdout "\nWARNING: an error was encountered while the new \n\ 
        	       \configuration was being written.  Attempting to \n\ 
        	       \restore the old configuration... "
	renameFile (conf_file ++ ".old")  conf_file
        hPutStrLn stdout "done."
	my_throw e
    )

writeNewConfig :: String -> [PackageConfig] -> IO ()
writeNewConfig conf_file packages = do
  hPutStr stdout "Writing new package config file... "
  h <- openFile conf_file WriteMode
  hPutStrLn h (dumpPackages packages)
  hClose h
  hPutStrLn stdout "done."

savePackageConfig :: String -> IO ()
savePackageConfig conf_file = do
  hPutStr stdout "Saving old package config file... "
    -- mv rather than cp because we've already done an hGetContents
    -- on this file so we won't be able to open it for writing
    -- unless we move the old one out of the way...
  let oldFile = conf_file ++ ".old"
  doesExist <- doesFileExist oldFile  `catch` (\ _ -> return False)
  when doesExist (removeFile oldFile `catch` (const $ return ()))
  catch (renameFile conf_file oldFile)
  	(\ err -> do
		hPutStrLn stderr (unwords [ "Unable to rename "
					  , show conf_file
					  , " to "
					  , show oldFile
					  ])
		ioError err)
  hPutStrLn stdout "done."

-----------------------------------------------------------------------------
-- Sanity-check a new package config, and automatically build GHCi libs
-- if requested.

validatePackageConfig :: PackageConfig 
		      -> [PackageConfig]
		      -> Bool
		      -> Bool
		      -> IO [PackageConfig]
validatePackageConfig pkg pkgs auto_ghci_libs updatePkg = do
  when (not updatePkg && (name pkg `elem` map name pkgs))
       (die ("package `" ++ name pkg ++ "' is already installed"))
  mapM_	(checkDep pkgs) (package_deps pkg)
  mapM_	checkDir (import_dirs pkg)
  mapM_	checkDir (source_dirs pkg)
  mapM_	checkDir (library_dirs pkg)
  mapM_	checkDir (include_dirs pkg)
  mapM_ (checkHSLib (library_dirs pkg) auto_ghci_libs) (hs_libraries pkg)
  -- ToDo: check these somehow?
  --	extra_libraries :: [String],
  --	c_includes      :: [String],
  let existing_pkgs
       | updatePkg = filter ((/=(name pkg)).name) pkgs  
       | otherwise = pkgs
  return (existing_pkgs ++ [pkg])

checkDir d
 | "$libdir" `isPrefixOf` d = return ()
	-- can't check this, because we don't know what $libdir is
 | otherwise = do
   there <- doesDirectoryExist d
   when (not there)
       (die ("`" ++ d ++ "' doesn't exist or isn't a directory"))

checkDep :: [PackageConfig] -> String -> IO ()
checkDep pkgs n
  | n `elem` map name pkgs = return ()
  | otherwise = die ("dependency `" ++ n ++ "' doesn't exist")

checkHSLib :: [String] -> Bool -> String -> IO ()
checkHSLib dirs auto_ghci_libs lib = do
  let batch_lib_file = "lib" ++ lib ++ ".a"
  bs <- mapM (doesLibExistIn batch_lib_file) dirs
  case [ dir | (exists,dir) <- zip bs dirs, exists ] of
	[] -> die ("cannot find `" ++ batch_lib_file ++ "' on library path") 
	(dir:_) -> checkGHCiLib dirs dir batch_lib_file lib auto_ghci_libs

doesLibExistIn lib d
 | "$libdir" `isPrefixOf` d = return True
 | otherwise                = doesFileExist (d ++ '/':lib)

checkGHCiLib :: [String] -> String -> String -> String -> Bool -> IO ()
checkGHCiLib dirs batch_lib_dir batch_lib_file lib auto_build = do
  let ghci_lib_file = lib ++ ".o"
      ghci_lib_path = batch_lib_dir ++ '/':ghci_lib_file
  bs <- mapM (\d -> doesFileExist (d ++ '/':ghci_lib_file)) dirs
  case [ dir | (exists,dir) <- zip bs dirs, exists ] of
        [] | auto_build -> 
		autoBuildGHCiLib batch_lib_dir batch_lib_file ghci_lib_file
	   | otherwise  -> 
		hPutStrLn stderr ("warning: can't find GHCi lib `"
					 ++ ghci_lib_file ++ "'")
   	(dir:_) -> return ()

-- automatically build the GHCi version of a batch lib, 
-- using ld --whole-archive.

autoBuildGHCiLib dir batch_file ghci_file = do
  let ghci_lib_file  = dir ++ '/':ghci_file
      batch_lib_file = dir ++ '/':batch_file
  hPutStr stderr ("building GHCi library `" ++ ghci_lib_file ++ "'...")
  system("ld -r -x -o " ++ ghci_lib_file ++ 
	 " --whole-archive " ++ batch_lib_file)
  hPutStrLn stderr (" done.")

-----------------------------------------------------------------------------
expandEnvVars :: PackageConfig -> IO PackageConfig
expandEnvVars pkg = do
   -- permit _all_ strings to contain ${..} environment variable references,
   -- arguably too flexible.
  nm       <- expandString (name pkg)
  imp_dirs <- expandStrings (import_dirs pkg) 
  src_dirs <- expandStrings (source_dirs pkg) 
  lib_dirs <- expandStrings (library_dirs pkg) 
  hs_libs  <- expandStrings (hs_libraries pkg)
  ex_libs  <- expandStrings (extra_libraries pkg)
  inc_dirs <- expandStrings (include_dirs pkg)
  c_incs   <- expandStrings (c_includes pkg)
  p_deps   <- expandStrings (package_deps pkg)
  e_g_opts <- expandStrings (extra_ghc_opts pkg)
  e_c_opts <- expandStrings (extra_cc_opts pkg)
  e_l_opts <- expandStrings (extra_ld_opts pkg)
  return (pkg { name            = nm
  	      , import_dirs     = imp_dirs
	      , source_dirs     = src_dirs
	      , library_dirs    = lib_dirs
	      , hs_libraries    = hs_libs
	      , extra_libraries = ex_libs
	      , include_dirs    = inc_dirs
	      , c_includes      = c_incs
	      , package_deps    = p_deps
	      , extra_ghc_opts  = e_g_opts
	      , extra_cc_opts   = e_c_opts
	      , extra_ld_opts   = e_l_opts
	      })
  where
   expandStrings = mapM expandString
   
    -- Just for fun, keep this in the IO monad.
   expandString :: String -> IO String
   expandString str =
     case break (=='$') str of
       (xs, _:'{':rs) ->
         case span (/='}') rs of
	   (nm,_:remainder) -> do
	      nm'  <- lookupEnvVar nm
	      str' <- expandString remainder
	      return (nm' ++ str')
	   _ -> return str -- no closing '}'
       _ -> return str	   

   lookupEnvVar nm = 
	catch (System.getEnv nm)
	      (\ _ -> die ("Unable to expand variable " ++ show nm))

-----------------------------------------------------------------------------

die :: String -> IO a
die s = do { hFlush stdout ; hPutStrLn stderr s; exitWith (ExitFailure 1) }

-----------------------------------------------------------------------------
-- Exceptions

#ifndef __GLASGOW_HASKELL__

eval_catch a h = a `seq` return ()
my_catch = IO.catch
my_throw = IO.fail

#else /* GHC */

my_throw = Exception.throw
#if __GLASGOW_HASKELL__ > 408
eval_catch = Exception.catch . Exception.evaluate
my_catch = Exception.catch
#else
eval_catch = Exception.catchAll
my_catch = Exception.catchAllIO
#endif

#endif
