-----------------------------------------------------------------------------
-- $Id: DriverMkDepend.hs,v 1.2 2000/10/17 13:22:10 simonmar Exp $
--
-- GHC Driver
--
-- (c) Simon Marlow 2000
--
-----------------------------------------------------------------------------

module DriverMkDepend where

#include "HsVersions.h"

import CmSummarise -- for mkdependHS stuff
import DriverState
import DriverUtil
import DriverFlags
import TmpFiles
import Module
import Config
import Util

import IOExts
import Exception

import Directory
import IO
import Monad
import Maybe

-------------------------------------------------------------------------------
-- mkdependHS

	-- flags
GLOBAL_VAR(dep_makefile, 	"Makefile", String);
GLOBAL_VAR(dep_include_prelude, False, Bool);
GLOBAL_VAR(dep_ignore_dirs,	[], [String]);
GLOBAL_VAR(dep_suffixes,	[], [String]);
GLOBAL_VAR(dep_warnings,	True, Bool);

	-- global vars
GLOBAL_VAR(dep_makefile_hdl,   	error "dep_makefile_hdl", Maybe Handle);
GLOBAL_VAR(dep_tmp_file,       	error "dep_tmp_file", String);
GLOBAL_VAR(dep_tmp_hdl,        	error "dep_tmp_hdl", Handle);
GLOBAL_VAR(dep_dir_contents,   	error "dep_dir_contents", [(String,[String])]);

depStartMarker = "# DO NOT DELETE: Beginning of Haskell dependencies"
depEndMarker   = "# DO NOT DELETE: End of Haskell dependencies"

-- for compatibility with the old mkDependHS, we accept options of the form
-- -optdep-f -optdep.depend, etc.
dep_opts = [
   (  "s", 			SepArg (add dep_suffixes) ),
   (  "f", 			SepArg (writeIORef dep_makefile) ),
   (  "w", 			NoArg (writeIORef dep_warnings False) ),
   (  "-include-prelude",  	NoArg (writeIORef dep_include_prelude True) ),
   (  "X", 			Prefix (addToDirList dep_ignore_dirs) ),
   (  "-exclude-directory=",	Prefix (addToDirList dep_ignore_dirs) )
 ]

beginMkDependHS :: IO ()
beginMkDependHS = do

  	-- slurp in the mkdependHS-style options
  flags <- getStaticOpts opt_dep
  _ <- processArgs dep_opts flags []

     	-- open a new temp file in which to stuff the dependency info
     	-- as we go along.
  dep_file <- newTempName "dep"
  writeIORef dep_tmp_file dep_file
  tmp_hdl <- openFile dep_file WriteMode
  writeIORef dep_tmp_hdl tmp_hdl

  	-- open the makefile
  makefile <- readIORef dep_makefile
  exists <- doesFileExist makefile
  if not exists
	then do 
	   writeIORef dep_makefile_hdl Nothing
	   return ()

	else do
  	   makefile_hdl <- openFile makefile ReadMode
  	   writeIORef dep_makefile_hdl (Just makefile_hdl)

		-- slurp through until we get the magic start string,
		-- copying the contents into dep_makefile
  	   let slurp = do
		l <- hGetLine makefile_hdl
		if (l == depStartMarker)
			then return ()
			else do hPutStrLn tmp_hdl l; slurp
	 
		-- slurp through until we get the magic end marker,
		-- throwing away the contents
  	   let chuck = do
		l <- hGetLine makefile_hdl
		if (l == depEndMarker)
			then return ()
			else chuck
	 
	   catchJust ioErrors slurp 
		(\e -> if isEOFError e then return () else ioError e)
	   catchJust ioErrors chuck
		(\e -> if isEOFError e then return () else ioError e)


	-- write the magic marker into the tmp file
  hPutStrLn tmp_hdl depStartMarker

  	-- cache the contents of all the import directories, for future
	-- reference.
  import_dirs <- readIORef import_paths
  pkg_import_dirs <- getPackageImportPath
  import_dir_contents <- mapM getDirectoryContents import_dirs
  pkg_import_dir_contents <- mapM getDirectoryContents pkg_import_dirs
  writeIORef dep_dir_contents 
	(zip import_dirs import_dir_contents ++
  	 zip pkg_import_dirs pkg_import_dir_contents)

	-- ignore packages unless --include-prelude is on
  include_prelude <- readIORef dep_include_prelude
  when (not include_prelude) $
    mapM_ (add dep_ignore_dirs) pkg_import_dirs

  return ()


endMkDependHS :: IO ()
endMkDependHS = do
  makefile     <- readIORef dep_makefile
  makefile_hdl <- readIORef dep_makefile_hdl
  tmp_file     <- readIORef dep_tmp_file
  tmp_hdl      <- readIORef dep_tmp_hdl

	-- write the magic marker into the tmp file
  hPutStrLn tmp_hdl depEndMarker

  case makefile_hdl of
     Nothing  -> return ()
     Just hdl -> do

	  -- slurp the rest of the orignal makefile and copy it into the output
  	let slurp = do
		l <- hGetLine hdl
		hPutStrLn tmp_hdl l
		slurp
	 
  	catchJust ioErrors slurp 
		(\e -> if isEOFError e then return () else ioError e)

	hClose hdl

  hClose tmp_hdl  -- make sure it's flushed

	-- create a backup of the original makefile
  when (isJust makefile_hdl) $
     run_something ("Backing up " ++ makefile)
	(unwords [ "cp", makefile, makefile++".bak" ])

  	-- copy the new makefile in place
  run_something "Installing new makefile"
	(unwords [ "cp", tmp_file, makefile ])


findDependency :: String -> ModImport -> IO (Maybe (String, Bool))
findDependency mod imp = do
   dir_contents <- readIORef dep_dir_contents
   ignore_dirs  <- readIORef dep_ignore_dirs
   hisuf <- readIORef hi_suf

   let
     (imp_mod, is_source) = 
	case imp of
	   MINormal str -> (moduleNameString str, False)
	   MISource str -> (moduleNameString str, True )	

     imp_hi = imp_mod ++ '.':hisuf
     imp_hiboot = imp_mod ++ ".hi-boot"
     imp_hiboot_v = imp_mod ++ ".hi-boot-" ++ cHscIfaceFileVersion
     imp_hs = imp_mod ++ ".hs"
     imp_lhs = imp_mod ++ ".lhs"

     deps | is_source = [ imp_hiboot_v, imp_hiboot, imp_hs, imp_lhs ]
     	  | otherwise = [ imp_hi, imp_hs, imp_lhs ]

     search [] = throwDyn (OtherError ("can't find one of the following: " ++
				      unwords (map (\d -> '`': d ++ "'") deps) ++
				      " (imported from `" ++ mod ++ "')"))
     search ((dir, contents) : dirs)
	   | null present = search dirs
	   | otherwise = 
		if dir `elem` ignore_dirs 
			then return Nothing
			else if is_source
				then if dep /= imp_hiboot_v 
					then return (Just (dir++'/':imp_hiboot, False))	
					else return (Just (dir++'/':dep, False))	
				else return (Just (dir++'/':imp_hi, not is_source))
	   where
		present = filter (`elem` contents) deps
		dep     = head present
 
   -- in
   search dir_contents

