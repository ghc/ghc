-----------------------------------------------------------------------------
-- $Id: Main.hs,v 1.17 2001/10/10 23:17:14 sof Exp $
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
import System
import IO

#include "../../includes/config.h"

#ifdef mingw32_TARGET_OS
import Win32DLL
#endif

main = do
  args <- getArgs

  case getOpt Permute flags args of
	(clis,[],[]) -> runit clis
	(_,_,errors) -> die (concat errors ++ 
			     usageInfo usageHeader flags)

data Flag = Config String | List | Add | Remove String | Show String 
		| Field String | AutoGHCiLibs

isAction (Config _)     = False
isAction (Field _)      = False
isAction (AutoGHCiLibs) = False
isAction _              = True

usageHeader = "ghc-pkg [OPTION...]"

flags = [
  Option ['f'] ["config-file"] (ReqArg Config "FILE")
	"Use the specified package config file",
  Option ['l'] ["list-packages"] (NoArg List)
 	"List the currently installed packages",
  Option ['a'] ["add-package"] (NoArg Add)
 	"Add a new package",
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
  let details = read s :: [PackageConfig]
  eval_catch details (\_ -> die "parse error in package config file")

  let auto_ghci_libs = any isAuto clis 
	 where isAuto AutoGHCiLibs = True; isAuto _ = False

  case [ c | c <- clis, isAction c ] of
    [ List ]     -> listPackages details
    [ Add  ]     -> addPackage details conf_file auto_ghci_libs
    [ Remove p ] -> removePackage details conf_file p
    [ Show p ]   -> showPackage details conf_file p fields
    _            -> die (usageInfo usageHeader flags)


listPackages :: [PackageConfig] -> IO ()
listPackages details = do 
  hPutStr stdout (listPkgs details)
  hPutChar stdout '\n'

showPackage :: [PackageConfig] -> FilePath -> String
	 -> [PackageConfig->[String]] -> IO ()
showPackage details pkgconf pkg_name fields =
  case [ p | p <- details, name p == pkg_name ] of
    []    -> die ("can't find package `" ++ pkg_name ++ "'")
    [pkg] | null fields -> hPutStrLn stdout (render (dumpPkgGuts pkg))
	  | otherwise   -> hPutStrLn stdout (render (vcat 
				(map (vcat . map text) (map ($pkg) fields))))
    _     -> die "showPackage: internal error"

addPackage :: [PackageConfig] -> FilePath -> Bool -> IO ()
addPackage details pkgconf auto_ghci_libs = do
  checkConfigAccess pkgconf
  hPutStr stdout "Reading package info from stdin... "
  s <- getContents
  let new_pkg = read s :: PackageConfig
  eval_catch new_pkg (\_ -> die "parse error in package info")
  hPutStrLn stdout "done."
  checkPackageConfig new_pkg details auto_ghci_libs
  savePackageConfig pkgconf
  maybeRestoreOldConfig pkgconf $
    writeNewConfig pkgconf (details ++ [new_pkg])

removePackage :: [PackageConfig] -> FilePath -> String -> IO ()
removePackage details pkgconf pkg = do  
  checkConfigAccess pkgconf
  if (pkg `notElem` map name details)
	then die ("package `" ++ pkg ++ "' not installed")
	else do
  savePackageConfig pkgconf
  maybeRestoreOldConfig pkgconf $
    writeNewConfig pkgconf (filter ((/= pkg) . name) details)

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
writeNewConfig conf_file details = do
  hPutStr stdout "Writing new package config file... "
  h <- openFile conf_file WriteMode
  hPutStrLn h (dumpPackages details)
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
		hPutStrLn stderr (unwords [ "Unable to rename"
					  , show conf_file
					  , " to "
					  , show oldFile
					  ])
		ioError err)
  hPutStrLn stdout "done."

-----------------------------------------------------------------------------
-- Sanity-check a new package config, and automatically build GHCi libs
-- if requested.

checkPackageConfig :: PackageConfig -> [PackageConfig] -> Bool -> IO ()
checkPackageConfig pkg pkgs auto_ghci_libs = do
  if (name pkg `elem` map name pkgs)
	then die ("package `" ++ name pkg ++ "' is already installed")
	else do
  mapM_	(checkDep pkgs) (package_deps pkg)
  mapM_	checkDir (import_dirs pkg)
  mapM_	checkDir (source_dirs pkg)
  mapM_	checkDir (library_dirs pkg)
  mapM_	checkDir (include_dirs pkg)
  mapM_ (checkHSLib (library_dirs pkg) auto_ghci_libs) (hs_libraries pkg)
  -- ToDo: check these somehow?
  --	extra_libraries :: [String],
  --	c_includes      :: [String],

checkDir d = do
  b <- doesDirectoryExist d
  if b then return ()
       else die ("`" ++ d ++ "' doesn't exist or isn't a directory")

checkDep :: [PackageConfig] -> String -> IO ()
checkDep pkgs n
  | n `elem` map name pkgs = return ()
  | otherwise = die ("dependency `" ++ n ++ "' doesn't exist")

checkHSLib :: [String] -> Bool -> String -> IO ()
checkHSLib dirs auto_ghci_libs lib = do
  let batch_lib_file = "lib" ++ lib ++ ".a"
  bs <- mapM (\d -> doesFileExist (d ++ '/':batch_lib_file)) dirs
  case [ dir | (exists,dir) <- zip bs dirs, exists ] of
	[] -> die ("cannot find `" ++ batch_lib_file ++ "' on library path") 
	(dir:_) -> checkGHCiLib dirs dir batch_lib_file lib auto_ghci_libs

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

die :: String -> IO a
die s = do { hPutStrLn stderr s; exitWith (ExitFailure 1) }

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
