-----------------------------------------------------------------------------
-- $Id: PackageMaintenance.hs,v 1.9 2001/03/08 11:44:16 simonmar Exp $
--
-- GHC Driver program
--
-- (c) Simon Marlow 2000
--
-----------------------------------------------------------------------------

module PackageMaintenance 
     ( listPackages, newPackage, deletePackage 
     ) where

import CmStaticInfo
import DriverState
import TmpFiles
import Panic

import Exception
import IOExts
import Pretty

import IO
import Directory
import System
import Monad

-----------------------------------------------------------------------------
-- Package maintenance

listPackages :: IO ()
listPackages = do 
  details <- readIORef v_Package_details
  hPutStr stdout (listPkgs details)
  hPutChar stdout '\n'
  exitWith ExitSuccess

newPackage :: IO ()
newPackage = do
  error "wibble" {-
  checkConfigAccess
  details <- readIORef v_Package_details
  hPutStr stdout "Reading package info from stdin... "
  stuff <- getContents
  let new_pkg = read stuff :: Package
  catchAll new_pkg
  	(\_ -> throwDyn (OtherError "parse error in package info"))
  hPutStrLn stdout "done."
  if (name new_pkg `elem` map name details)
	then throwDyn (OtherError ("package `" ++ name new_pkg ++ 
					"' already installed"))
	else do
  conf_file <- readIORef v_Path_package_config
  savePackageConfig conf_file
  maybeRestoreOldConfig conf_file $ do
  writeNewConfig conf_file ( ++ [new_pkg])
  exitWith ExitSuccess
-}

deletePackage :: String -> IO ()
deletePackage pkg = do  
  checkConfigAccess
  details <- readIORef v_Package_details
  if (pkg `notElem` map name details)
	then throwDyn (OtherError ("package `" ++ pkg ++ "' not installed"))
	else do
  conf_file <- readIORef v_Path_package_config
  savePackageConfig conf_file
  maybeRestoreOldConfig conf_file $ do
  writeNewConfig conf_file (filter ((/= pkg) . name))
  exitWith ExitSuccess

checkConfigAccess :: IO ()
checkConfigAccess = do
  conf_file <- readIORef v_Path_package_config
  access <- getPermissions conf_file
  unless (writable access)
	(throwDyn (OtherError "you don't have permission to modify the package configuration file"))

maybeRestoreOldConfig :: String -> IO () -> IO ()
maybeRestoreOldConfig conf_file io
  = catchAllIO io (\e -> do
        hPutStr stdout "\nWARNING: an error was encountered while the new \n\ 
        	       \configuration was being written.  Attempting to \n\ 
        	       \restore the old configuration... "
        kludgedSystem ("cp " ++ conf_file ++ ".old " ++ conf_file) "Restoring old configuration"
        hPutStrLn stdout "done."
	throw e
    )

writeNewConfig :: String -> ([Package] -> [Package]) -> IO ()
writeNewConfig conf_file fn = do
  hPutStr stdout "Writing new package config file... "
  old_details <- readIORef v_Package_details
  h <- openFile conf_file WriteMode
  hPutStr h (dumpPackages (fn old_details))
  hClose h
  hPutStrLn stdout "done."

savePackageConfig :: String -> IO ()
savePackageConfig conf_file = do
  hPutStr stdout "Saving old package config file... "
    -- mv rather than cp because we've already done an hGetContents
    -- on this file so we won't be able to open it for writing
    -- unless we move the old one out of the way...
  kludgedSystem ("mv " ++ conf_file ++ " " ++ conf_file ++ ".old") "Saving package configuration"
  hPutStrLn stdout "done."

-----------------------------------------------------------------------------
-- Pretty printing package info

listPkgs :: [Package] -> String
listPkgs pkgs = render (fsep (punctuate comma (map (text . name) pkgs)))

dumpPackages :: [Package] -> String
dumpPackages pkgs = 
   render (brackets (vcat (punctuate comma (map dumpPkgGuts pkgs))))

dumpPkgGuts :: Package -> Doc
dumpPkgGuts pkg =
   text "Package" $$ nest 3 (braces (
      sep (punctuate comma [
         text "name = " <> text (show (name pkg)),
         dumpField "import_dirs"     (import_dirs     pkg),
         dumpField "library_dirs"    (library_dirs    pkg),
         dumpField "hs_libraries"    (hs_libraries    pkg),
         dumpField "extra_libraries" (extra_libraries pkg),
         dumpField "include_dirs"    (include_dirs    pkg),
         dumpField "c_includes"      (c_includes      pkg),
         dumpField "package_deps"    (package_deps    pkg),
         dumpField "extra_ghc_opts"  (extra_ghc_opts  pkg),
         dumpField "extra_cc_opts"   (extra_cc_opts   pkg),
         dumpField "extra_ld_opts"   (extra_ld_opts   pkg)
      ])))

dumpField :: String -> [String] -> Doc
dumpField name val =
   hang (text name <+> equals) 2
        (brackets (sep (punctuate comma (map (text . show) val))))
