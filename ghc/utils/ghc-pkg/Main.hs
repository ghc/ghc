-----------------------------------------------------------------------------
-- $Id: Main.hs,v 1.10 2001/06/04 06:20:35 qrczak Exp $
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

main = do
  args <- getArgs

  case getOpt Permute flags args of
	(clis,[],[]) -> runit clis
	(_,_,errors) -> die (concat errors ++ 
			     usageInfo usageHeader flags)

data Flag = Config String | List | Add | Remove String | Show String | Field String
isConfigOrField (Config _) = True
isConfigOrField (Field _) = True
isConfigOrField _ = False

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
 	"Remove an installed package"
  ]

runit clis = do
  conf_file <- 
     case [ f | Config f <- clis ] of
        []  -> die "missing -f option, location of package.conf unknown"
        [f] -> return f
        _   -> die (usageInfo usageHeader flags)

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

  case [ c | c <- clis, not (isConfigOrField c) ] of
    [ List ]     -> listPackages details
    [ Add  ]     -> addPackage details conf_file
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

addPackage :: [PackageConfig] -> FilePath -> IO ()
addPackage details pkgconf = do
  checkConfigAccess pkgconf
  hPutStr stdout "Reading package info from stdin... "
  s <- getContents
  let new_pkg = read s :: PackageConfig
  eval_catch new_pkg (\_ -> die "parse error in package info")
  hPutStrLn stdout "done."
  if (name new_pkg `elem` map name details)
	then die ("package `" ++ name new_pkg ++ "' already installed")
	else do
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
  renameFile conf_file (conf_file ++ ".old") 
  hPutStrLn stdout "done."

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
