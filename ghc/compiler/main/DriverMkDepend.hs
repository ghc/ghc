-----------------------------------------------------------------------------
-- $Id: DriverMkDepend.hs,v 1.11 2001/05/28 03:31:19 sof Exp $
--
-- GHC Driver
--
-- (c) Simon Marlow 2000
--
-----------------------------------------------------------------------------

module DriverMkDepend where

#include "HsVersions.h"

import DriverState
import DriverUtil
import DriverFlags
import TmpFiles
import Module
import Config
import Util
import Panic

import IOExts
import Exception

import Directory
import IO
import Monad
import Maybe

-------------------------------------------------------------------------------
-- mkdependHS

	-- flags
GLOBAL_VAR(v_Dep_makefile, 		"Makefile", String);
GLOBAL_VAR(v_Dep_include_prelude, 	False, Bool);
GLOBAL_VAR(v_Dep_ignore_dirs,		[], [String]);
GLOBAL_VAR(v_Dep_exclude_mods,          [], [String]);
GLOBAL_VAR(v_Dep_suffixes,		[], [String]);
GLOBAL_VAR(v_Dep_warnings,		True, Bool);

	-- global vars
GLOBAL_VAR(v_Dep_makefile_hdl,  error "dep_makefile_hdl", Maybe Handle);
GLOBAL_VAR(v_Dep_tmp_file,      error "dep_tmp_file", String);
GLOBAL_VAR(v_Dep_tmp_hdl,       error "dep_tmp_hdl", Handle);
GLOBAL_VAR(v_Dep_dir_contents,  error "dep_dir_contents", [(String,[String])]);

depStartMarker = "# DO NOT DELETE: Beginning of Haskell dependencies"
depEndMarker   = "# DO NOT DELETE: End of Haskell dependencies"

-- for compatibility with the old mkDependHS, we accept options of the form
-- -optdep-f -optdep.depend, etc.
dep_opts = [
   (  "s", 			SepArg (add v_Dep_suffixes) ),
   (  "f", 			SepArg (writeIORef v_Dep_makefile) ),
   (  "w", 			NoArg (writeIORef v_Dep_warnings False) ),
   (  "-include-prelude",  	NoArg (writeIORef v_Dep_include_prelude True) ),
   (  "X", 			Prefix (addToDirList v_Dep_ignore_dirs) ),
   (  "-exclude-directory=",	Prefix (addToDirList v_Dep_ignore_dirs) )
--   (  "-exclude-module=",       Prefix (add v_Dep_exclude_mods) )
--   (  "x",                      Prefix (add v_Dep_exclude_mods) )
   
 ]

beginMkDependHS :: IO ()
beginMkDependHS = do

  	-- slurp in the mkdependHS-style options
  flags <- getStaticOpts v_Opt_dep
  _ <- processArgs dep_opts flags []

     	-- open a new temp file in which to stuff the dependency info
     	-- as we go along.
  dep_file <- newTempName "dep"
  writeIORef v_Dep_tmp_file dep_file
  tmp_hdl <- openFile dep_file WriteMode
  writeIORef v_Dep_tmp_hdl tmp_hdl

  	-- open the makefile
  makefile <- readIORef v_Dep_makefile
  exists <- doesFileExist makefile
  if not exists
	then do 
	   writeIORef v_Dep_makefile_hdl Nothing
	   return ()

	else do
  	   makefile_hdl <- openFile makefile ReadMode
  	   writeIORef v_Dep_makefile_hdl (Just makefile_hdl)

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
  import_dirs <- readIORef v_Import_paths
  pkg_import_dirs <- getPackageImportPath
  import_dir_contents <- mapM softGetDirectoryContents import_dirs
  pkg_import_dir_contents <- mapM softGetDirectoryContents pkg_import_dirs
  writeIORef v_Dep_dir_contents 
	(zip import_dirs import_dir_contents ++
  	 zip pkg_import_dirs pkg_import_dir_contents)

	-- ignore packages unless --include-prelude is on
  include_prelude <- readIORef v_Dep_include_prelude
  when (not include_prelude) $
    mapM_ (add v_Dep_ignore_dirs) pkg_import_dirs

  return ()


endMkDependHS :: IO ()
endMkDependHS = do
  makefile     <- readIORef v_Dep_makefile
  makefile_hdl <- readIORef v_Dep_makefile_hdl
  tmp_file     <- readIORef v_Dep_tmp_file
  tmp_hdl      <- readIORef v_Dep_tmp_hdl

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
     runSomething ("Backing up " ++ makefile)
	(unwords [ cCP, dosifyPath makefile, dosifyPath $ makefile++".bak" ])

  	-- copy the new makefile in place
  runSomething "Installing new makefile"
	(unwords [ cCP, dosifyPath tmp_file, dosifyPath makefile ])


findDependency :: Bool -> FilePath -> ModuleName -> IO (Maybe (String, Bool))
findDependency is_source src imp = do
   dir_contents <- readIORef v_Dep_dir_contents
   ignore_dirs  <- readIORef v_Dep_ignore_dirs
   excl_mods    <- readIORef v_Dep_exclude_mods
   hisuf <- readIORef v_Hi_suf

   let
     imp_mod      = moduleNameUserString imp
     imp_hi       = imp_mod ++ '.':hisuf
     imp_hiboot   = imp_mod ++ ".hi-boot"
     imp_hiboot_v = imp_mod ++ ".hi-boot-" ++ cHscIfaceFileVersion
     imp_hs       = imp_mod ++ ".hs"
     imp_lhs      = imp_mod ++ ".lhs"

     deps | is_source = [ imp_hiboot_v, imp_hiboot, imp_hs, imp_lhs ]
     	  | otherwise = [ imp_hi, imp_hs, imp_lhs ]

     search [] = throwDyn (ProgramError (src ++ ": " ++ "can't find one of the following: " ++
				      unwords (map (\d -> '`': d ++ "'") deps)))
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
   if imp_mod `elem` excl_mods then
      return Nothing
    else
      search dir_contents
