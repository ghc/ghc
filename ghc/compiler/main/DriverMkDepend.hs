-----------------------------------------------------------------------------
-- $Id: DriverMkDepend.hs,v 1.36 2005/01/18 12:18:28 simonpj Exp $
--
-- GHC Driver
--
-- (c) Simon Marlow 2000
--
-----------------------------------------------------------------------------

module DriverMkDepend (
	doMkDependHSPhase, beginMkDependHS, endMkDependHS
  ) where

#include "HsVersions.h"

import GetImports	( getImportsFromFile )
import CmdLineOpts	( DynFlags )
import DriverState      
import DriverUtil
import DriverFlags
import Packages		( PackageIdH(..) )
import SysTools		( newTempName )
import qualified SysTools
import Module		( Module, ModLocation(..), moduleUserString)
import Finder		( findModule, hiBootExt, hiBootVerExt,
			  mkHomeModLocation, FindResult(..) )
import Util             ( global, maybePrefixMatch )
import Panic

import DATA_IOREF	( IORef, readIORef, writeIORef )
import EXCEPTION

import Directory
import IO
import Monad            ( when )
import Maybe            ( isJust )

#if __GLASGOW_HASKELL__ <= 408
import Panic		( catchJust, ioErrors )
#endif

-------------------------------------------------------------------------------
-- mkdependHS

	-- flags
GLOBAL_VAR(v_Dep_makefile, 		"Makefile", String);
GLOBAL_VAR(v_Dep_include_prelude, 	False, Bool);
GLOBAL_VAR(v_Dep_exclude_mods,          ["GHC.Prim"], [String]);
GLOBAL_VAR(v_Dep_suffixes,		[], [String]);
GLOBAL_VAR(v_Dep_warnings,		True, Bool);

	-- global vars
GLOBAL_VAR(v_Dep_makefile_hdl,  error "dep_makefile_hdl", Maybe Handle);
GLOBAL_VAR(v_Dep_tmp_file,      error "dep_tmp_file", String);
GLOBAL_VAR(v_Dep_tmp_hdl,       error "dep_tmp_hdl", Handle);

depStartMarker = "# DO NOT DELETE: Beginning of Haskell dependencies"
depEndMarker   = "# DO NOT DELETE: End of Haskell dependencies"

-- for compatibility with the old mkDependHS, we accept options of the form
-- -optdep-f -optdep.depend, etc.
dep_opts = 
   [ (  "s", 			SepArg (add v_Dep_suffixes) )
   , (  "f", 			SepArg (writeIORef v_Dep_makefile) )
   , (  "w", 			NoArg (writeIORef v_Dep_warnings False) )
   , (  "-include-prelude",  	NoArg (writeIORef v_Dep_include_prelude True) )
   , (  "-exclude-module=",       Prefix (add v_Dep_exclude_mods) )
   , (  "x",                      Prefix (add v_Dep_exclude_mods) )
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

  return ()


doMkDependHSPhase dflags basename suff input_fn
 = do (import_sources, import_normals, mod_name) 
		<- getImportsFromFile dflags input_fn
      let orig_fn = basename ++ '.':suff
      location' <- mkHomeModLocation mod_name orig_fn

      -- take -ohi into account if present
      ohi <- readIORef v_Output_hi
      let location | Just fn <- ohi = location'{ ml_hi_file = fn }
		   | otherwise      = location'

      deps_sources <- mapM (findDependency dflags True  orig_fn) import_sources
      deps_normals <- mapM (findDependency dflags False orig_fn) import_normals
      let deps = deps_sources ++ deps_normals

      osuf <- readIORef v_Object_suf
      extra_suffixes <- readIORef v_Dep_suffixes
      let suffixes = map (++ ('_':osuf)) extra_suffixes
	  obj_file = ml_obj_file location
          objs = obj_file : map (replaceFilenameSuffix obj_file) suffixes

	-- Handle for file that accumulates dependencies 
      hdl <- readIORef v_Dep_tmp_hdl

	-- std dependency of the object(s) on the source file
      hPutStrLn hdl (unwords (map escapeSpaces objs) ++ " : " ++
		     escapeSpaces (basename ++ '.':suff))

      let genDep (dep, False {- not an hi file -}) = 
	     hPutStrLn hdl (unwords (map escapeSpaces objs) ++ " : " ++
			    escapeSpaces dep)
          genDep (dep, True  {- is an hi file -}) = do
	     hisuf <- readIORef v_Hi_suf
	     let 
		-- In order to construct hi files with alternate suffixes, we
		-- now have to find the "basename" of the hi file.  This is
		-- difficult because we can't just split the hi filename
		-- at the last dot - the hisuf might have dots in it.  So we
		-- check whether the hi filename ends in hisuf, and if it does,
		-- we strip off hisuf, otherwise we strip everything after the
		-- last dot.
		dep_base 
		   | Just rest <- maybePrefixMatch rev_hisuf rev_dep
		   = reverse rest
		   | otherwise
		   = remove_suffix '.' dep
		  where
			rev_hisuf = reverse hisuf
			rev_dep   = reverse dep

	        deps = dep : map (\suf -> dep_base ++ suf ++ '_':hisuf) 
				extra_suffixes
		  -- length objs should be == length deps
	     sequence_ (zipWith (\o d -> hPutStrLn hdl (escapeSpaces o ++ " : " ++ escapeSpaces d)) objs deps)

      sequence_ (map genDep [ d | Just d <- deps ])
      return location

-- add the lines to dep_makefile:
	   -- always:
		   -- this.o : this.hs

  	   -- if the dependency is on something other than a .hi file:
   		   -- this.o this.p_o ... : dep
   	   -- otherwise
   		   -- if the import is {-# SOURCE #-}
   			   -- this.o this.p_o ... : dep.hi-boot[-$vers]
   			   
   		   -- else
   			   -- this.o ...   : dep.hi
   			   -- this.p_o ... : dep.p_hi
   			   -- ...
   
   	   -- (where .o is $osuf, and the other suffixes come from
   	   -- the cmdline -s options).
   


endMkDependHS :: DynFlags -> IO ()
endMkDependHS dflags = do
  makefile     <- readIORef v_Dep_makefile
  makefile_hdl <- readIORef v_Dep_makefile_hdl
  tmp_file     <- readIORef v_Dep_tmp_file
  tmp_hdl      <- readIORef v_Dep_tmp_hdl

	-- write the magic marker into the tmp file
  hPutStrLn tmp_hdl depEndMarker

  case makefile_hdl of
     Nothing  -> return ()
     Just hdl -> do

	  -- slurp the rest of the original makefile and copy it into the output
  	let slurp = do
		l <- hGetLine hdl
		hPutStrLn tmp_hdl l
		slurp
	 
  	catchJust ioErrors slurp 
		(\e -> if isEOFError e then return () else ioError e)

	hClose hdl

  hClose tmp_hdl  -- make sure it's flushed

	-- Create a backup of the original makefile
  when (isJust makefile_hdl)
       (SysTools.copy dflags ("Backing up " ++ makefile) 
		makefile (makefile++".bak"))

  	-- Copy the new makefile in place
  SysTools.copy dflags "Installing new makefile" tmp_file makefile


findDependency :: DynFlags -> Bool -> FilePath -> Module -> IO (Maybe (String, Bool))
findDependency dflags is_source src imp = do
   excl_mods <- readIORef v_Dep_exclude_mods
   include_prelude <- readIORef v_Dep_include_prelude
   let imp_mod = moduleUserString imp
   if imp_mod `elem` excl_mods 
      then return Nothing
      else do
	r <- findModule dflags imp True{-explicit-}
	case r of 
	   Found loc pkg
		-- not in this package: we don't need a dependency
		| ExtPackage _ <- pkg, not include_prelude
		-> return Nothing

		-- normal import: just depend on the .hi file
		| not is_source
		-> return (Just (ml_hi_file loc, not is_source))

		-- if it's a source import, we want to generate a dependency
		-- on the .hi-boot file, not the .hi file
		| otherwise
		-> let hi_file = ml_hi_file loc
		       boot_hi_file = replaceFilenameSuffix hi_file hiBootExt 
		       boot_ver_hi_file = replaceFilenameSuffix hi_file hiBootVerExt 
		   in do
		   b <- doesFileExist boot_ver_hi_file
		   if b 
		     then return (Just (boot_ver_hi_file, not is_source))
		     else do
		        b <- doesFileExist boot_hi_file
		   	if b 
			   then return (Just (boot_hi_file, not is_source))
		   	   else return (Just (hi_file, not is_source))

	   _ -> throwDyn (ProgramError 
		(src ++ ": " ++ "can't locate import `" ++ imp_mod ++ "'" ++
		 if is_source then " (SOURCE import)" else ""))
