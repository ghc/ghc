%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[RnMonad]{The monad used by the renamer}

\begin{code}
module RnMonad(
	module RnMonad,
	Module,
	FiniteMap,
	Bag,
	Name,
	RdrNameHsDecl,
	RdrNameInstDecl,
	Version,
	NameSet,
	OccName,
	Fixity
    ) where

#include "HsVersions.h"

import SST
import GlaExts		( RealWorld, stToIO )
import List		( intersperse )

import HsSyn		
import RdrHsSyn
import RnHsSyn		( RenamedFixitySig )
import BasicTypes	( Version )
import SrcLoc		( noSrcLoc )
import ErrUtils		( addShortErrLocLine, addShortWarnLocLine,
			  pprBagOfErrors, ErrMsg, WarnMsg, Message
			)
import Name		( Name, OccName, NamedThing(..),
			  isLocallyDefinedName, nameModule, nameOccName,
			  decode
			)
import Module		( Module, IfaceFlavour, setModuleFlavour, mkSysModuleFS,
			  bootFlavour, moduleString, moduleIfaceFlavour, mkDynFlavour
			)
import NameSet		
import RdrName		( RdrName )
import CmdLineOpts	( opt_D_show_rn_trace, opt_IgnoreIfacePragmas, 
			  opt_WarnHiShadows, opt_Static
			)
import PrelInfo		( builtinNames )
import TysWiredIn	( boolTyCon )
import SrcLoc		( SrcLoc, mkGeneratedSrcLoc )
import Unique		( Unique )
import UniqFM		( UniqFM )
import FiniteMap	( FiniteMap, emptyFM, bagToFM, lookupFM, addToFM, addListToFM, 
			  addListToFM_C, addToFM_C, eltsFM
			)
import Bag		( Bag, mapBag, emptyBag, isEmptyBag, snocBag )
import Maybes		( seqMaybe, mapMaybe )
import UniqSet
import UniqFM
import UniqSupply
import Util
import Outputable
import DirUtils		( getDirectoryContents )
import Directory	( doesFileExist )
import IO		( hPutStrLn, stderr, isDoesNotExistError )
import Monad		( foldM )
import Maybe		( fromMaybe )
import Constants	( interfaceFileFormatVersion )

infixr 9 `thenRn`, `thenRn_`
\end{code}


%************************************************************************
%*									*
\subsection{Somewhat magical interface to other monads}
%*									*
%************************************************************************

\begin{code}
sstToIO :: SST RealWorld r -> IO r
sstToIO sst = stToIO (sstToST sst)

ioToRnMG :: IO r -> RnMG (Either IOError r)
ioToRnMG io rn_down g_down = ioToSST io
	    
traceRn :: SDoc -> RnMG ()
traceRn msg | opt_D_show_rn_trace = putDocRn msg
	    | otherwise		  = returnRn ()

putDocRn :: SDoc -> RnMG ()
putDocRn msg = ioToRnMG (printErrs msg)	`thenRn_`
	       returnRn ()
\end{code}


%************************************************************************
%*									*
\subsection{Data types}
%*									*
%************************************************************************

===================================================
		MONAD TYPES
===================================================

\begin{code}
type RnM s d r = RnDown s -> d -> SST s r
type RnMS s r   = RnM s         (SDown s) r		-- Renaming source
type RnMG r     = RnM RealWorld GDown     r		-- Getting global names etc
type SSTRWRef a = SSTRef RealWorld a		-- ToDo: there ought to be a standard defn of this

	-- Common part
data RnDown s = RnDown {
		  rn_loc  :: SrcLoc,
		  rn_omit :: Name -> Bool, 			-- True <=> omit qualifier when printing
		  rn_ns   :: SSTRef s RnNameSupply,
		  rn_errs :: SSTRef s (Bag WarnMsg, Bag ErrMsg),
		  rn_occs :: SSTRef s ([Occurrence],[Occurrence]),	-- Occurrences: compulsory and optional resp
		  rn_hi_map     :: ModuleHiMap,   -- for .hi files
		  rn_hiboot_map :: ModuleHiMap,   -- for .hi-boot files
		  rn_mod        :: Module
		}

type Occurrence = (Name, SrcLoc)		-- The srcloc is the occurrence site

data Necessity = Compulsory | Optional		-- We *must* find definitions for
						-- compulsory occurrences; we *may* find them
						-- for optional ones.

	-- For getting global names
data GDown = GDown {
		rn_ifaces     :: SSTRWRef Ifaces
	     }

	-- For renaming source code
data SDown s = SDown {
		  rn_mode :: RnMode,
		  rn_genv :: RnEnv,	-- Global envt; the fixity component gets extended
					--   with local fixity decls
		  rn_lenv :: LocalRdrEnv	-- Local name envt
					--   Does *not* includes global name envt; may shadow it
					--   Includes both ordinary variables and type variables;
					--   they are kept distinct because tyvar have a different
					--   occurrence contructor (Name.TvOcc)
					-- We still need the unsullied global name env so that
					--   we can look up record field names
		}

data RnMode	= SourceMode			-- Renaming source code
		| InterfaceMode			-- Renaming interface declarations.  
			Necessity		-- The "necessity"
						-- flag says free variables *must* be found and slurped
						-- or whether they need not be.  For value signatures of
						-- things that are themselves compulsorily imported
						-- we arrange that the type signature is read 
						-- in compulsory mode,
						-- but the pragmas in optional mode.

type SearchPath = [(String,String)]	-- List of (directory,suffix) pairs to search 
                                        -- for interface files.

type ModuleHiMap = FiniteMap String (String, Bool)
   -- mapping from module name to the file path of its corresponding
   -- interface file.
\end{code}

===================================================
		ENVIRONMENTS
===================================================

\begin{code}
--------------------------------
type RdrNameEnv a = FiniteMap RdrName a
type GlobalRdrEnv = RdrNameEnv [Name]	-- The list is because there may be name clashes
					-- These only get reported on lookup,
					-- not on construction
type LocalRdrEnv  = RdrNameEnv Name

emptyRdrEnv  :: RdrNameEnv a
lookupRdrEnv :: RdrNameEnv a -> RdrName -> Maybe a
addListToRdrEnv :: RdrNameEnv a -> [(RdrName,a)] -> RdrNameEnv a

emptyRdrEnv  = emptyFM
lookupRdrEnv = lookupFM
addListToRdrEnv = addListToFM
rdrEnvElts	= eltsFM

--------------------------------
type NameEnv a = UniqFM a	-- Domain is Name

emptyNameEnv   :: NameEnv a
nameEnvElts    :: NameEnv a -> [a]
addToNameEnv_C :: (a->a->a) -> NameEnv a -> Name -> a -> NameEnv a
addToNameEnv   :: NameEnv a -> Name -> a -> NameEnv a
plusNameEnv    :: NameEnv a -> NameEnv a -> NameEnv a
extendNameEnv  :: NameEnv a -> [(Name,a)] -> NameEnv a
lookupNameEnv  :: NameEnv a -> Name -> Maybe a
delFromNameEnv :: NameEnv a -> Name -> NameEnv a
elemNameEnv    :: Name -> NameEnv a -> Bool

emptyNameEnv   = emptyUFM
nameEnvElts    = eltsUFM
addToNameEnv_C = addToUFM_C
addToNameEnv   = addToUFM
plusNameEnv    = plusUFM
extendNameEnv  = addListToUFM
lookupNameEnv  = lookupUFM
delFromNameEnv = delFromUFM
elemNameEnv    = elemUFM

--------------------------------
type FixityEnv = NameEnv RenamedFixitySig

--------------------------------
data RnEnv     	= RnEnv GlobalRdrEnv FixityEnv
emptyRnEnv	= RnEnv emptyRdrEnv  emptyNameEnv
\end{code}

\begin{code}
--------------------------------
type RnNameSupply
 = ( UniqSupply

   , FiniteMap (OccName, OccName) Int
	-- This is used as a name supply for dictionary functions
	-- From the inst decl we derive a (class, tycon) pair;
	-- this map then gives a unique int for each inst decl with that
	-- (class, tycon) pair.  (In Haskell 98 there can only be one,
	-- but not so in more extended versions.)
	--	
	-- We could just use one Int for all the instance decls, but this
	-- way the uniques change less when you add an instance decl,	
	-- hence less recompilation

   , FiniteMap (Module,OccName) Name
	-- Ensures that one (module,occname) pair gets one unique
   )


--------------------------------
data ExportEnv	  = ExportEnv Avails Fixities
type Avails	  = [AvailInfo]
type Fixities	  = [(Name, Fixity)]

type ExportAvails = (FiniteMap Module Avails,	-- Used to figure out "module M" export specifiers
						-- Includes avails only from *unqualified* imports
						-- (see 1.4 Report Section 5.1.1)

		     NameEnv AvailInfo)		-- Used to figure out all other export specifiers.
						-- Maps a Name to the AvailInfo that contains it


data GenAvailInfo name	= Avail name		-- An ordinary identifier
			| AvailTC name 		-- The name of the type or class
				  [name]	-- The available pieces of type/class. NB: If the type or
						-- class is itself to be in scope, it must be in this list.
						-- Thus, typically: AvailTC Eq [Eq, ==, /=]
type AvailInfo    = GenAvailInfo Name
type RdrAvailInfo = GenAvailInfo OccName
\end{code}

===================================================
		INTERFACE FILE STUFF
===================================================

\begin{code}
type ExportItem		 = (Module, [RdrAvailInfo])
type VersionInfo name    = [ImportVersion name]

type ImportVersion name  = (Module, Version, WhatsImported name)
data WhatsImported name  = Everything 
			 | Specifically [LocalVersion name]	-- List guaranteed non-empty

    -- ("M", hif, ver, Everything) means there was a "module M" in 
    -- this module's export list, so we just have to go by M's version, "ver",
    -- not the list of LocalVersions.


type LocalVersion name   = (name, Version)

data ParsedIface
  = ParsedIface
      Version		 		-- Module version number
      [ImportVersion OccName]		-- Usages
      [ExportItem]			-- Exports
      [Module]				-- Special instance modules
      [(Version, RdrNameHsDecl)]	-- Local definitions
      [RdrNameInstDecl]			-- Local instance declarations

type InterfaceDetails = (VersionInfo Name,	-- Version information for what this module imports
			 ExportEnv, 		-- What this module exports
			 [Module])		-- Instance modules


-- needed by Main to fish out the fixities assoc list.
getIfaceFixities :: InterfaceDetails -> Fixities
getIfaceFixities (_, ExportEnv _ fs, _) = fs


type RdrNamePragma = ()				-- Fudge for now
-------------------

data Ifaces = Ifaces {
		iMod :: Module,				-- Name of the module being compiled

		iModMap :: FiniteMap Module (IfaceFlavour, 		-- Exports
					     Version, 
					     Avails),

		iDecls :: DeclsMap,	-- A single, global map of Names to decls

		iFixes :: FixityEnv,	-- A single, global map of Names to fixities

		iSlurp :: NameSet,	-- All the names (whether "big" or "small", whether wired-in or not,
					-- whether locally defined or not) that have been slurped in so far.

		iVSlurp :: [(Name,Version)],	-- All the (a) non-wired-in (b) "big" (c) non-locally-defined names that 
						-- have been slurped in so far, with their versions. 
						-- This is used to generate the "usage" information for this module.
						-- Subset of the previous field.

		iDefInsts :: (Bag IfaceInst, NameSet),
					 -- The as-yet un-slurped instance decls; this bag is depleted when we
					 -- slurp an instance decl so that we don't slurp the same one twice.
					 -- Together with them is the set of tycons/classes that may allow 
					 -- the instance decls in.

		iDefData :: NameEnv (Module, RdrNameTyClDecl),
					-- Deferred data type declarations; each has the following properties
					--	* it's a data type decl
					--	* its TyCon is needed
					--	* the decl may or may not have been slurped, depending on whether any
					--	  of the constrs are needed.

		iInstMods :: [Module]	-- Set of modules with "special" instance declarations
					-- Excludes this module
	}


type DeclsMap = NameEnv (Version, AvailInfo, RdrNameHsDecl, Bool)
		-- A DeclsMap contains a binding for each Name in the declaration
		-- including the constructors of a type decl etc.
		-- The Bool is True just for the 'main' Name.

type IfaceInst = ((Module, RdrNameInstDecl),	-- Instance decl
		  NameSet)			-- "Gate" names.  Slurp this instance decl when this
						-- set becomes empty.  It's depleted whenever we
						-- slurp another type or class decl.
\end{code}


%************************************************************************
%*									*
\subsection{Main monad code}
%*									*
%************************************************************************

\begin{code}
initRn :: Module -> UniqSupply -> SearchPath -> SrcLoc
       -> RnMG r
       -> IO (r, Bag ErrMsg, Bag WarnMsg)

initRn mod us dirs loc do_rn = do
  names_var <- sstToIO (newMutVarSST (us, emptyFM, builtins))
  errs_var  <- sstToIO (newMutVarSST (emptyBag,emptyBag))
  iface_var <- sstToIO (newMutVarSST (emptyIfaces mod))
  occs_var  <- sstToIO (newMutVarSST initOccs)
  (himap, hibmap) <- mkModuleHiMaps dirs
  let
        rn_down = RnDown { rn_loc = loc, rn_omit = \n -> False, rn_ns = names_var, 
			   rn_errs = errs_var, rn_occs = occs_var,
			   rn_hi_map = himap, rn_hiboot_map = hibmap,
			   rn_mod = mod }
	g_down  = GDown {rn_ifaces = iface_var }

	-- do the business
  res <- sstToIO (do_rn rn_down g_down)

	-- grab errors and return
  (warns, errs) <- sstToIO (readMutVarSST errs_var)
  return (res, errs, warns)


initRnMS :: RnEnv -> RnMode -> RnMS RealWorld r -> RnMG r
initRnMS rn_env mode m rn_down g_down
  = let
	s_down = SDown { rn_genv = rn_env, rn_lenv = emptyRdrEnv, rn_mode = mode }
    in
    m rn_down s_down


emptyIfaces :: Module -> Ifaces
emptyIfaces mod = Ifaces { iMod = mod,
			   iModMap = emptyFM,
			   iDecls = emptyNameEnv,
			   iFixes = emptyNameEnv,
			   iSlurp = emptyNameSet,
			   iVSlurp = [],
			   iDefInsts = (emptyBag, emptyNameSet),
			   iDefData = emptyNameEnv, 
			   iInstMods = []
		  }

builtins :: FiniteMap (Module,OccName) Name
builtins = bagToFM $
	   mapBag (\ name -> ((nameModule name, nameOccName name), name)) 
		  builtinNames

	-- Initial value for the occurrence pool.
initOccs :: ([Occurrence],[Occurrence])	-- Compulsory and optional respectively
initOccs = ([(getName boolTyCon, noSrcLoc)], [])
	-- Booleans occur implicitly a lot, so it's tiresome to keep recording the fact, and
	-- rather implausible that not one will be used in the module.
	-- We could add some other common types, notably lists, but the general idea is
	-- to do as much as possible explicitly.
\end{code}

We (allege) that it is quicker to build up a mapping from module names
to the paths to their corresponding interface files once, than to search
along the import part every time we slurp in a new module (which we 
do quite a lot of.)

\begin{code}
mkModuleHiMaps :: SearchPath -> IO (ModuleHiMap, ModuleHiMap)
mkModuleHiMaps dirs = foldM (getAllFilesMatching dirs) (env,env) dirs
 where
  env = emptyFM

{- a pseudo file which signals that the interface files
   contained in a particular directory have got their
   corresponding object codes stashed away in a DLL
   
   This stuff is only needed to deal with Win32 DLLs,
   and conceivably we conditionally compile in support
   for handling it. (ToDo?)
-}
dir_contain_dll_his = "dLL_ifs.hi"

getAllFilesMatching :: SearchPath
		    -> (ModuleHiMap, ModuleHiMap)
		    -> (FilePath, String) 
		    -> IO (ModuleHiMap, ModuleHiMap)
getAllFilesMatching dirs hims (dir_path, suffix) = ( do
    -- fpaths entries do not have dir_path prepended
  fpaths  <- getDirectoryContents dir_path
  is_dyns <- catch
		(if opt_Static || dir_path == "." then
		     return False
		 else
		     doesFileExist (dir_path ++ '/': dir_contain_dll_his))
		(\ _ {-don't care-} -> return False)
  return (foldl (addModules is_dyns) hims fpaths)
   )  -- soft failure
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
   xiffus = reverse dotted_suffix 
  
   dotted_suffix =
    case suffix of
      [] -> []
      ('.':xs) -> suffix
      ls -> '.':ls

   hi_boot_version_xiffus = 
      reverse (show interfaceFileFormatVersion) ++ '-':hi_boot_xiffus
   hi_boot_xiffus = "toob-ih." -- .hi-boot reversed.

   addModules is_dll his@(hi_env, hib_env) nm = fromMaybe his $ 
        FMAP (\ (mod_nm,v) -> (addToFM_C addNewOne hi_env mod_nm (v, is_dll), hib_env))
	    (go xiffus rev_nm)		       `seqMaybe`

        FMAP (\ (mod_nm,v) -> (hi_env, addToFM_C overrideNew hib_env mod_nm (v,is_dll)))
	    (go hi_boot_version_xiffus rev_nm) `seqMaybe`

	FMAP (\ (mod_nm,v) -> (hi_env, addToFM_C addNewOne hib_env mod_nm (v,is_dll)))
	    (go hi_boot_xiffus rev_nm)
    where
     rev_nm  = reverse nm

     go [] xs         = Just (reverse xs, dir_path ++'/':nm)
     go _  []         = Nothing
     go (x:xs) (y:ys) 
       | x == y       = go xs ys 
       | otherwise    = Nothing

   addNewOne
    | opt_WarnHiShadows = conflict
    | otherwise         = stickWithOld

   stickWithOld old new = old
   overrideNew old new  = new

   conflict old_path new_path
    | old_path /= new_path = 
        pprTrace "Warning: " (text "Identically named interface files present on the import path, " $$
			      text (show old_path) <+> text "shadows" $$
			      text (show new_path) $$
			      text "on the import path: " <+> 
			      text (concat (intersperse ":" (map fst dirs))))
        old_path
    | otherwise = old_path  -- don't warn about innocous shadowings.

\end{code}


@renameSourceCode@ is used to rename stuff "out-of-line"; that is, not as part of
the main renamer.  Examples: pragmas (which we don't want to rename unless
we actually explore them); and derived definitions, which are only generated
in the type checker.

The @RnNameSupply@ includes a @UniqueSupply@, so if you call it more than
once you must either split it, or install a fresh unique supply.

\begin{code}
renameSourceCode :: Module 
		 -> RnNameSupply
	         -> RnMS RealWorld r
	         -> r

-- Alas, we can't use the real runST, with the desired signature:
--	renameSourceCode :: RnNameSupply -> RnMS s r -> r
-- because we can't manufacture "new versions of runST".

renameSourceCode mod_name name_supply m
  = runSST (
	newMutVarSST name_supply		`thenSST` \ names_var ->
	newMutVarSST (emptyBag,emptyBag)	`thenSST` \ errs_var ->
	newMutVarSST ([],[])			`thenSST` \ occs_var ->
    	let
	    rn_down = RnDown { rn_loc = mkGeneratedSrcLoc, rn_ns = names_var,
			       rn_errs = errs_var, rn_occs = occs_var,
			       rn_mod = mod_name }
	    s_down = SDown { rn_mode = InterfaceMode Compulsory,
			     rn_genv = emptyRnEnv, rn_lenv = emptyRdrEnv }
	in
	m rn_down s_down			`thenSST` \ result ->
	
	readMutVarSST errs_var			`thenSST` \ (warns,errs) ->

	(if not (isEmptyBag errs) then
		pprTrace "Urk! renameSourceCode found errors" (display errs) 
#ifdef DEBUG
	 else if not (isEmptyBag warns) then
		pprTrace "Note: renameSourceCode found warnings" (display warns)
#endif
	 else
		id) $

	returnSST result
    )
  where
    display errs = pprBagOfErrors errs

{-# INLINE thenRn #-}
{-# INLINE thenRn_ #-}
{-# INLINE returnRn #-}
{-# INLINE andRn #-}

returnRn :: a -> RnM s d a
thenRn   :: RnM s d a -> (a -> RnM s d b) -> RnM s d b
thenRn_  :: RnM s d a -> RnM s d b -> RnM s d b
andRn    :: (a -> a -> a) -> RnM s d a -> RnM s d a -> RnM s d a
mapRn    :: (a -> RnM s d b) -> [a] -> RnM s d [b]
mapMaybeRn :: (a -> RnM s d (Maybe b)) -> [a] -> RnM s d [b]
sequenceRn :: [RnM s d a] -> RnM s d [a]
foldlRn :: (b  -> a -> RnM s d b) -> b -> [a] -> RnM s d b
mapAndUnzipRn :: (a -> RnM s d (b,c)) -> [a] -> RnM s d ([b],[c])
fixRn    :: (a -> RnM s d a) -> RnM s d a

returnRn v gdown ldown  = returnSST v
thenRn m k gdown ldown  = m gdown ldown `thenSST` \ r -> k r gdown ldown
thenRn_ m k gdown ldown = m gdown ldown `thenSST_` k gdown ldown
fixRn m gdown ldown = fixSST (\r -> m r gdown ldown)
andRn combiner m1 m2 gdown ldown
  = m1 gdown ldown `thenSST` \ res1 ->
    m2 gdown ldown `thenSST` \ res2 ->
    returnSST (combiner res1 res2)

sequenceRn []     = returnRn []
sequenceRn (m:ms) =  m			`thenRn` \ r ->
		     sequenceRn ms 	`thenRn` \ rs ->
		     returnRn (r:rs)

mapRn f []     = returnRn []
mapRn f (x:xs)
  = f x		`thenRn` \ r ->
    mapRn f xs 	`thenRn` \ rs ->
    returnRn (r:rs)

foldlRn k z [] = returnRn z
foldlRn k z (x:xs) = k z x	`thenRn` \ z' ->
		     foldlRn k z' xs

mapAndUnzipRn f [] = returnRn ([],[])
mapAndUnzipRn f (x:xs)
  = f x		    	`thenRn` \ (r1,  r2)  ->
    mapAndUnzipRn f xs	`thenRn` \ (rs1, rs2) ->
    returnRn (r1:rs1, r2:rs2)

mapAndUnzip3Rn f [] = returnRn ([],[],[])
mapAndUnzip3Rn f (x:xs)
  = f x		    	`thenRn` \ (r1,  r2,  r3)  ->
    mapAndUnzip3Rn f xs	`thenRn` \ (rs1, rs2, rs3) ->
    returnRn (r1:rs1, r2:rs2, r3:rs3)

mapMaybeRn f []     = returnRn []
mapMaybeRn f (x:xs) = f x		`thenRn` \ maybe_r ->
		      mapMaybeRn f xs 	`thenRn` \ rs ->
		      case maybe_r of
			Nothing -> returnRn rs
			Just r  -> returnRn (r:rs)
\end{code}



%************************************************************************
%*									*
\subsection{Boring plumbing for common part}
%*									*
%************************************************************************


================  Errors and warnings =====================

\begin{code}
failWithRn :: a -> Message -> RnM s d a
failWithRn res msg (RnDown {rn_errs = errs_var, rn_loc = loc}) l_down
  = readMutVarSST  errs_var  					`thenSST`  \ (warns,errs) ->
    writeMutVarSST errs_var (warns, errs `snocBag` err)		`thenSST_` 
    returnSST res
  where
    err = addShortErrLocLine loc msg

warnWithRn :: a -> Message -> RnM s d a
warnWithRn res msg (RnDown {rn_errs = errs_var, rn_loc = loc}) l_down
  = readMutVarSST  errs_var  				 	`thenSST`  \ (warns,errs) ->
    writeMutVarSST errs_var (warns `snocBag` warn, errs)	`thenSST_` 
    returnSST res
  where
    warn = addShortWarnLocLine loc msg

addErrRn :: Message -> RnM s d ()
addErrRn err = failWithRn () err

checkRn :: Bool -> Message -> RnM s d ()	-- Check that a condition is true
checkRn False err = addErrRn err
checkRn True  err = returnRn ()

warnCheckRn :: Bool -> Message -> RnM s d ()	-- Check that a condition is true
warnCheckRn False err = addWarnRn err
warnCheckRn True  err = returnRn ()

addWarnRn :: Message -> RnM s d ()
addWarnRn warn = warnWithRn () warn

checkErrsRn :: RnM s d Bool		-- True <=> no errors so far
checkErrsRn (RnDown {rn_errs = errs_var}) l_down
  = readMutVarSST  errs_var  				 	`thenSST`  \ (warns,errs) ->
    returnSST (isEmptyBag errs)
\end{code}


================  Source location =====================

\begin{code}
pushSrcLocRn :: SrcLoc -> RnM s d a -> RnM s d a
pushSrcLocRn loc' m down l_down
  = m (down {rn_loc = loc'}) l_down

getSrcLocRn :: RnM s d SrcLoc
getSrcLocRn down l_down
  = returnSST (rn_loc down)
\end{code}

================  Name supply =====================

\begin{code}
getNameSupplyRn :: RnM s d RnNameSupply
getNameSupplyRn rn_down l_down
  = readMutVarSST (rn_ns rn_down)

setNameSupplyRn :: RnNameSupply -> RnM s d ()
setNameSupplyRn names' (RnDown {rn_ns = names_var}) l_down
  = writeMutVarSST names_var names'

-- See comments with RnNameSupply above.
newInstUniq :: (OccName, OccName) -> RnM s d Int
newInstUniq key (RnDown {rn_ns = names_var}) l_down
  = readMutVarSST names_var				`thenSST` \ (us, mapInst, cache) ->
    let
	uniq = case lookupFM mapInst key of
		   Just x  -> x+1
		   Nothing -> 0
	mapInst' = addToFM mapInst key uniq
    in
    writeMutVarSST names_var (us, mapInst', cache)	`thenSST_`
    returnSST uniq

getUniqRn :: RnM s d Unique
getUniqRn (RnDown {rn_ns = names_var}) l_down
 = readMutVarSST names_var `thenSST` \ (us, mapInst, cache) ->
   let
     (us1,us') = splitUniqSupply us
   in
   writeMutVarSST names_var (us', mapInst, cache)  `thenSST_`
   returnSST (uniqFromSupply us1)
\end{code}

================  Occurrences =====================

Every time we get an occurrence of a name we put it in one of two lists:
	one for "compulsory" occurrences
	one for "optional" occurrences

The significance of "compulsory" is
	(a) we *must* find the declaration
	(b) in the case of type or class names, the name is part of the
	    source level program, and we must slurp in any instance decls
	    involving it.  

We don't need instance decls "optional" names, because the type inference
process will never come across them.  Optional names are buried inside
type checked (but not renamed) cross-module unfoldings and such.

The pair of lists is held in a mutable variable in RnDown.  

The lists are kept separate so that we can process all the compulsory occurrences 
before any of the optional ones.  Why?  Because suppose we processed an optional 
"g", and slurped an interface decl of g::T->T.  Then we'd rename the type T->T in
optional mode.  But if we later need g compulsorily we'll find that it's already
been slurped and will do nothing.  We could, I suppose, rename it a second time,
but it seems simpler just to do all the compulsory ones first.

\begin{code}
addOccurrenceName :: Name -> RnMS s Name	-- Same name returned as passed
addOccurrenceName name (RnDown {rn_loc = loc, rn_occs = occs_var})
		       (SDown {rn_mode = mode})
  | isLocallyDefinedName name ||
    not_necessary necessity
  = returnSST name

  | otherwise
  = readMutVarSST occs_var			`thenSST` \ (comp_occs, opt_occs) ->
    let
	new_occ_pair = case necessity of
			 Optional   -> (comp_occs, (name,loc):opt_occs)
			 Compulsory -> ((name,loc):comp_occs, opt_occs)
    in
    writeMutVarSST occs_var new_occ_pair	`thenSST_`
    returnSST name
  where
    necessity = modeToNecessity mode


addOccurrenceNames :: [Name] -> RnMS s ()
addOccurrenceNames names (RnDown {rn_loc = loc, rn_occs = occs_var})
		         (SDown {rn_mode = mode})
  | not_necessary necessity 
  = returnSST ()

  | otherwise
  = readMutVarSST occs_var			`thenSST` \ (comp_occs, opt_occs) ->
    let
	new_occ_pair = case necessity of
			 Optional   -> (comp_occs, non_local_occs ++ opt_occs)
			 Compulsory -> (non_local_occs ++ comp_occs, opt_occs)
    in
    writeMutVarSST occs_var new_occ_pair
  where
    non_local_occs = [(name, loc) | name <- names, not (isLocallyDefinedName name)]
    necessity = modeToNecessity mode

	-- Never look for optional things if we're
	-- ignoring optional input interface information
not_necessary Compulsory = False
not_necessary Optional   = opt_IgnoreIfacePragmas

popOccurrenceName :: RnMode -> RnM s d (Maybe Occurrence)
popOccurrenceName mode (RnDown {rn_occs = occs_var}) l_down
  = readMutVarSST occs_var			`thenSST` \ occs ->
    case (mode, occs) of
		-- Find a compulsory occurrence
	(InterfaceMode Compulsory, (comp:comps, opts))
		-> writeMutVarSST occs_var (comps, opts)	`thenSST_`
		   returnSST (Just comp)

		-- Find an optional occurrence
		-- We shouldn't be looking unless we've done all the compulsories
	(InterfaceMode Optional, (comps, opt:opts))
		-> ASSERT2( null comps, ppr comps )
		   writeMutVarSST occs_var (comps, opts)	`thenSST_`
		   returnSST (Just opt)

		-- No suitable occurrence
	other -> returnSST Nothing

-- discardOccurrencesRn does the enclosed thing with a *fresh* occurrences
-- variable, and discards the list of occurrences thus found.  It's useful
-- when loading instance decls and specialisation signatures, when we want to
-- know the names of the things in the types, but we don't want to treat them
-- as occurrences.

discardOccurrencesRn :: RnM s d a -> RnM s d a
discardOccurrencesRn enclosed_thing rn_down l_down
  = newMutVarSST ([],[])						`thenSST` \ new_occs_var ->
    enclosed_thing (rn_down {rn_occs = new_occs_var}) l_down
\end{code}


================  Module =====================

\begin{code}
getModuleRn :: RnM s d Module
getModuleRn (RnDown {rn_mod = mod_name}) l_down
  = returnSST mod_name

setModuleRn :: Module -> RnM s d a -> RnM s d a
setModuleRn new_mod enclosed_thing rn_down l_down
  = enclosed_thing (rn_down {rn_mod = new_mod}) l_down
\end{code}

\begin{code}
setOmitQualFn :: (Name -> Bool) -> RnM s d a -> RnM s d a
setOmitQualFn fn m g_down l_down = m (g_down { rn_omit = fn }) l_down

getOmitQualFn :: RnM s d (Name -> Bool)
getOmitQualFn (RnDown {rn_omit = omit_fn}) l_down
  = returnSST omit_fn
\end{code}

%************************************************************************
%*									*
\subsection{Plumbing for rename-source part}
%*									*
%************************************************************************

================  RnEnv  =====================

\begin{code}
getNameEnvs :: RnMS s (GlobalRdrEnv, LocalRdrEnv)
getNameEnvs rn_down (SDown {rn_genv = RnEnv global_env fixity_env, rn_lenv = local_env})
  = returnSST (global_env, local_env)

getLocalNameEnv :: RnMS s LocalRdrEnv
getLocalNameEnv rn_down (SDown {rn_lenv = local_env})
  = returnSST local_env

setLocalNameEnv :: LocalRdrEnv -> RnMS s a -> RnMS s a
setLocalNameEnv local_env' m rn_down l_down
  = m rn_down (l_down {rn_lenv = local_env'})

getFixityEnv :: RnMS s FixityEnv
getFixityEnv rn_down (SDown {rn_genv = RnEnv name_env fixity_env})
  = returnSST fixity_env

extendFixityEnv :: [(Name, RenamedFixitySig)] -> RnMS s a -> RnMS s a
extendFixityEnv fixes enclosed_scope
	        rn_down l_down@(SDown {rn_genv = RnEnv name_env fixity_env})
  = let
	new_fixity_env = extendNameEnv fixity_env fixes
    in
    enclosed_scope rn_down (l_down {rn_genv = RnEnv name_env new_fixity_env})
\end{code}

================  Mode  =====================

\begin{code}
getModeRn :: RnMS s RnMode
getModeRn rn_down (SDown {rn_mode = mode})
  = returnSST mode

setModeRn :: RnMode -> RnMS s a -> RnMS s a
setModeRn new_mode thing_inside rn_down l_down
  = thing_inside rn_down (l_down {rn_mode = new_mode})
\end{code}


%************************************************************************
%*									*
\subsection{Plumbing for rename-globals part}
%*									*
%************************************************************************

\begin{code}
getIfacesRn :: RnMG Ifaces
getIfacesRn rn_down (GDown {rn_ifaces = iface_var})
  = readMutVarSST iface_var

setIfacesRn :: Ifaces -> RnMG ()
setIfacesRn ifaces rn_down (GDown {rn_ifaces = iface_var})
  = writeMutVarSST iface_var ifaces

getModuleHiMap :: Bool -> RnM s d ModuleHiMap
getModuleHiMap want_hi_boot (RnDown {rn_hi_map = himap, rn_hiboot_map = hibmap}) _ 
  | want_hi_boot = returnSST hibmap
  | otherwise    = returnSST himap
\end{code}

The interface file format is capable of distinguishing
between normal imports/exports of names from other modules
and 'hi-boot' mentions of names, with the flavour in the
being encoded inside a @Module@.

@setModuleFlavourRn@ fixes up @Module@ values containing
normal flavours, checking to see whether 

\begin{code}
setModuleFlavourRn :: Module -> RnM s d Module
setModuleFlavourRn mod
  | bootFlavour hif = returnRn mod
  | otherwise       =
     getModuleHiMap (bootFlavour hif) `thenRn` \ himap ->
     let mod_pstr = moduleString mod in
     case (lookupFM himap mod_pstr) of
       Nothing -> returnRn mod
       Just (_,is_in_a_dll) ->
            returnRn (setModuleFlavour (mkDynFlavour is_in_a_dll hif) mod)
  where
    hif = moduleIfaceFlavour mod

\end{code}

%************************************************************************
%*									*
\subsection{HowInScope}
%*									*
%************************************************************************

\begin{code}
modeToNecessity SourceMode		  = Compulsory
modeToNecessity (InterfaceMode necessity) = necessity
\end{code}
