-----------------------------------------------------------------------------
-- $Id: Main.hs,v 1.4 2001/03/24 18:34:05 qrczak Exp $
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

default_pkgconf = clibdir ++ "/package.conf"

main = do
  args <- getArgs

  case getOpt Permute flags args of
	(clis,[],[]) -> runit clis
	(_,_,errors) -> die (concat errors ++ 
			     usageInfo usageHeader flags)

data Flag = Config String | List | Add | Remove String
isConfig (Config _) = True
isConfig _ = False

usageHeader = "ghc-pkg [OPTION...]"

flags = [
  Option ['f'] ["config-file"] (ReqArg Config "FILE")
	"Use the specified package config file",
  Option ['l'] ["list-packages"] (NoArg List)
 	"List the currently installed packages",
  Option ['a'] ["add-package"] (NoArg Add)
 	"Add a new package",
  Option ['r'] ["remove-package"] (ReqArg Remove "NAME")
 	"Remove an installed package"
  ]

runit clis = do
  conf_file <- 
     case [ f | Config f <- clis ] of
        []  -> return default_pkgconf
        [f] -> return f
        _   -> die (usageInfo usageHeader flags)

  s <- readFile conf_file
  let details = read s :: [PackageConfig]
  eval_catch details (\_ -> die "parse error in package config file")

  case [ c | c <- clis, not (isConfig c) ] of
    [ List ]     -> listPackages details
    [ Add  ]     -> addPackage details conf_file
    [ Remove p ] -> removePackage details conf_file p
    _            -> die (usageInfo usageHeader flags)


listPackages :: [PackageConfig] -> IO ()
listPackages details = do 
  hPutStr stdout (listPkgs details)
  hPutChar stdout '\n'
  exitWith ExitSuccess

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
  maybeRestoreOldConfig pkgconf $ do
  writeNewConfig pkgconf (details ++ [new_pkg])
  exitWith ExitSuccess

removePackage :: [PackageConfig] -> FilePath -> String -> IO ()
removePackage details pkgconf pkg = do  
  checkConfigAccess pkgconf
  if (pkg `notElem` map name details)
	then die ("package `" ++ pkg ++ "' not installed")
	else do
  savePackageConfig pkgconf
  maybeRestoreOldConfig pkgconf $ do
  writeNewConfig pkgconf (filter ((/= pkg) . name) details)
  exitWith ExitSuccess

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
  hPutStr h (dumpPackages details )
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
