%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[Module]{The @Module@ module.}

Representing modules and their flavours.

\begin{code}
module Module 
    (
      Module		    -- abstract, instance of Eq, Ord, Outputable
    , ModuleName

    , moduleNameString		-- :: ModuleName -> EncodedString
    , moduleNameUserString	-- :: ModuleName -> UserString

    , moduleString          -- :: Module -> EncodedString
    , moduleUserString      -- :: Module -> UserString
    , moduleName	    -- :: Module -> ModuleName

    , mkVanillaModule	    -- :: ModuleName -> Module
    , mkThisModule	    -- :: ModuleName -> Module
    , mkPrelModule          -- :: UserString -> Module
    , mkModule		    -- :: ModuleName -> PackageName -> Module
    
    , isLocalModule       -- :: Module -> Bool

    , mkSrcModule

    , mkSrcModuleFS         -- :: UserFS    -> ModuleName
    , mkSysModuleFS         -- :: EncodedFS -> ModuleName

    , pprModule, pprModuleName
 
    , PackageName

	-- Where to find a .hi file
    , WhereFrom(..), SearchPath, mkSearchPath
    , ModuleHiMap, mkModuleHiMaps

    ) where

#include "HsVersions.h"
import OccName
import Outputable
import FiniteMap
import CmdLineOpts	( opt_Static, opt_InPackage, opt_WarnHiShadows, opt_HiMapSep )
import Constants	( interfaceFileFormatVersion )
import Maybes		( seqMaybe )
import Maybe		( fromMaybe )
import Directory	( doesFileExist )
import DirUtils		( getDirectoryContents )
import List		( intersperse )
import Monad		( foldM )
import IO		( hPutStrLn, stderr, isDoesNotExistError )
import FastString	( FastString )
\end{code}


%************************************************************************
%*									*
\subsection{Interface file flavour}
%*									*
%************************************************************************

A further twist to the tale is the support for dynamically linked libraries under
Win32. Here, dealing with the use of global variables that's residing in a DLL
requires special handling at the point of use (there's an extra level of indirection,
i.e., (**v) to get at v's value, rather than just (*v) .) When slurping in an
interface file we then record whether it's coming from a .hi corresponding to a
module that's packaged up in a DLL or not, so that we later can emit the
appropriate code.

The logic for how an interface file is marked as corresponding to a module that's
hiding in a DLL is explained elsewhere (ToDo: give renamer href here.)

\begin{code}
data PackageInfo = ThisPackage 			-- A module from the same package 
						-- as the one being compiled
		 | AnotherPackage PackageName	-- A module from a different package

type PackageName = FastString		-- No encoding at all

preludePackage :: PackageName
preludePackage = SLIT("std")

instance Show PackageInfo where	-- Just used in debug prints of lex tokens
  showsPrec n ThisPackage s = s
  showsPrec n (AnotherPackage p) s = (_UNPK_ p) ++ s
\end{code}


%************************************************************************
%*									*
\subsection{System/user module}
%*									*
%************************************************************************

We also track whether an imported module is from a 'system-ish' place.  In this case
we don't record the fact that this module depends on it, nor usages of things
inside it.  

Apr 00: We want to record dependencies on all modules other than
prelude modules else STG Hugs gets confused because it uses this
info to know what modules to link.  (Compiled GHC uses command line
options to specify this.)

\begin{code}
data ModFlavour = PrelMod	-- A Prelude module
		| UserMod	-- Not library-ish
\end{code}


%************************************************************************
%*									*
\subsection{Where from}
%*									*
%************************************************************************

The @WhereFrom@ type controls where the renamer looks for an interface file

\begin{code}
data WhereFrom = ImportByUser		-- Ordinary user import: look for M.hi
	       | ImportByUserSource	-- User {- SOURCE -}: look for M.hi-boot
	       | ImportBySystem		-- Non user import.  Look for M.hi if M is in
					-- the module this module depends on, or is a system-ish module; 
					-- M.hi-boot otherwise

instance Outputable WhereFrom where
  ppr ImportByUser       = empty
  ppr ImportByUserSource = ptext SLIT("{- SOURCE -}")
  ppr ImportBySystem     = ptext SLIT("{- SYSTEM IMPORT -}")
\end{code}


%************************************************************************
%*									*
\subsection{The name of a module}
%*									*
%************************************************************************

\begin{code}
type ModuleName = EncodedFS
	-- Haskell module names can include the quote character ',
	-- so the module names have the z-encoding applied to them

pprModuleName :: ModuleName -> SDoc
pprModuleName nm = pprEncodedFS nm

moduleNameString :: ModuleName -> EncodedString
moduleNameString mod = _UNPK_ mod

moduleNameUserString :: ModuleName -> UserString
moduleNameUserString mod = decode (_UNPK_ mod)

mkSrcModule :: UserString -> ModuleName
mkSrcModule s = _PK_ (encode s)

mkSrcModuleFS :: UserFS -> ModuleName
mkSrcModuleFS s = encodeFS s

mkSysModuleFS :: EncodedFS -> ModuleName
mkSysModuleFS s = s 
\end{code}

\begin{code}
data Module = Module ModuleName PackageInfo
\end{code}

\begin{code}
instance Outputable Module where
  ppr = pprModule

instance Eq Module where
  (Module m1 _) == (Module m2 _) = m1 == m2

instance Ord Module where
  (Module m1 _) `compare` (Module m2 _) = m1 `compare` m2
\end{code}


\begin{code}
pprModule :: Module -> SDoc
pprModule (Module mod _) = getPprStyle $ \ sty ->
			   if userStyle sty then
				text (moduleNameUserString mod)				
			   else
				pprModuleName mod
\end{code}


\begin{code}
mkModule :: ModuleName	-- Name of the module
	 -> PackageName
	 -> Module
mkModule mod_nm pack_name
  = Module mod_nm pack_info
  where
    pack_info | pack_name == opt_InPackage = ThisPackage
	      | otherwise		   = AnotherPackage pack_name

mkVanillaModule :: ModuleName -> Module
mkVanillaModule name = Module name (pprTrace "mkVanillaModule" (ppr name) ThisPackage)
	-- Used temporarily when we first come across Foo.x in an interface
	-- file, but before we've opened Foo.hi.
	-- (Until we've opened Foo.hi we don't know what the PackageInfo is.)

mkThisModule :: ModuleName -> Module	-- The module being comiled
mkThisModule name = Module name ThisPackage

mkPrelModule :: ModuleName -> Module
mkPrelModule name = mkModule name preludePackage

moduleString :: Module -> EncodedString
moduleString (Module mod _) = _UNPK_ mod

moduleName :: Module -> ModuleName
moduleName (Module mod _) = mod

moduleUserString :: Module -> UserString
moduleUserString (Module mod _) = moduleNameUserString mod
\end{code}

\begin{code}
isLocalModule :: Module -> Bool
isLocalModule (Module _ ThisPackage) = True
isLocalModule _		      	     = False
\end{code}


%************************************************************************
%*									*
\subsection{Finding modules in the file system
%*									*
%************************************************************************

\begin{code}
type ModuleHiMap = FiniteMap ModuleName String
  -- Mapping from module name to 
  -- 	* the file path of its corresponding interface file, 
  --	* the ModuleName
\end{code}

(We allege that) it is quicker to build up a mapping from module names
to the paths to their corresponding interface files once, than to search
along the import part every time we slurp in a new module (which we 
do quite a lot of.)

\begin{code}
type SearchPath = [(String,String)]	-- List of (directory,suffix) pairs to search 
                                        -- for interface files.

mkModuleHiMaps :: SearchPath -> IO (ModuleHiMap, ModuleHiMap)
mkModuleHiMaps dirs = foldM (getAllFilesMatching dirs) (env,env) dirs
 where
  env = emptyFM

getAllFilesMatching :: SearchPath
		    -> (ModuleHiMap, ModuleHiMap)
		    -> (FilePath, String) 
		    -> IO (ModuleHiMap, ModuleHiMap)
getAllFilesMatching dirs hims (dir_path, suffix) = ( do
    -- fpaths entries do not have dir_path prepended
  fpaths  <- getDirectoryContents dir_path
  return (foldl addModules hims fpaths))
  -- soft failure
      `catch` 
        (\ err -> do
	      hPutStrLn stderr
		     ("Import path element `" ++ dir_path ++ 
		      if (isDoesNotExistError err) then
	                 "' does not exist, ignoring."
		      else
	                "' couldn't read, ignoring.")
	       
              return hims
	)
 where
   xiffus	 = reverse dotted_suffix 
   dotted_suffix = case suffix of
		      []       -> []
		      ('.':xs) -> suffix
		      ls       -> '.':ls

   hi_boot_version_xiffus = 
      reverse (show interfaceFileFormatVersion) ++ '-':hi_boot_xiffus
   hi_boot_xiffus = "toob-ih." -- .hi-boot reversed!

   addModules his@(hi_env, hib_env) filename = fromMaybe his $ 
        FMAP add_hi   (go xiffus		 rev_fname)	`seqMaybe`

        FMAP add_vhib (go hi_boot_version_xiffus rev_fname)	`seqMaybe`
		-- If there's a Foo.hi-boot-N file then override any Foo.hi-boot

	FMAP add_hib  (go hi_boot_xiffus	 rev_fname)
     where
	rev_fname = reverse filename
	path      = dir_path ++ '/':filename

	  -- In these functions file_nm is the base of the filename,
	  -- with the path and suffix both stripped off.  The filename
	  -- is the *unencoded* module name (else 'make' gets confused).
	  -- But the domain of the HiMaps is ModuleName which is encoded.
	add_hi    file_nm = (add_to_map addNewOne hi_env file_nm,   hib_env)
	add_vhib  file_nm = (hi_env, add_to_map overrideNew hib_env file_nm)
	add_hib   file_nm = (hi_env, add_to_map addNewOne   hib_env file_nm)

	add_to_map combiner env file_nm 
	  = addToFM_C combiner env mod_nm path
	  where
     	    mod_nm = mkSrcModuleFS file_nm

   -- go prefix (prefix ++ stuff) == Just (reverse stuff)
   go [] xs        		= Just (_PK_ (reverse xs))
   go _  []         		= Nothing
   go (x:xs) (y:ys) | x == y    = go xs ys 
		    | otherwise = Nothing

   addNewOne | opt_WarnHiShadows = conflict
	     | otherwise         = stickWithOld

   stickWithOld old new = old
   overrideNew  old new = new

   conflict old_path new_path
    | old_path /= new_path = 
        pprTrace "Warning: " (text "Identically named interface files present on the import path, " $$
			      text (show old_path) <+> text "shadows" $$
			      text (show new_path) $$
			      text "on the import path: " <+> 
			      text (concat (intersperse ":" (map fst dirs))))
        old_path
    | otherwise = old_path	-- don't warn about innocous shadowings.
\end{code}


%*********************************************************
%*						 	 *
\subsection{Making a search path}
%*							 *
%*********************************************************

@mkSearchPath@ takes a string consisting of a colon-separated list
of directories and corresponding suffixes, and turns it into a list
of (directory, suffix) pairs.  For example:

\begin{verbatim}
 mkSearchPath "foo%.hi:.%.p_hi:baz%.mc_hi"
   = [("foo",".hi"),( ".", ".p_hi"), ("baz",".mc_hi")]
\begin{verbatim}

\begin{code}
mkSearchPath :: Maybe String -> SearchPath
mkSearchPath Nothing = [(".",".hi")]  -- ToDo: default should be to look in
				      -- the directory the module we're compiling
				      -- lives.
mkSearchPath (Just s) = go s
  where
    go "" = []
    go s  = 
      case span (/= '%') s of
       (dir,'%':rs) ->
         case span (/= opt_HiMapSep) rs of
          (hisuf,_:rest) -> (dir,hisuf):go rest
          (hisuf,[])     -> [(dir,hisuf)]
\end{code}

