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

#if   defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 405
import IOExts		( fixIO )
#elif defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 302
import PrelIOBase	( fixIO )	-- Should be in GlaExts
#else
import IOBase		( fixIO )
#endif
import IOExts		( IORef, newIORef, readIORef, writeIORef, unsafePerformIO )
	
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
			  decode, mkLocalName
			)
import Module		( Module, ModuleName, ModuleHiMap, SearchPath, WhereFrom,
			  mkModuleHiMaps, moduleName, mkVanillaModule, mkSearchPath
			)
import NameSet		
import RdrName		( RdrName, dummyRdrVarName, rdrNameOcc )
import CmdLineOpts	( opt_D_dump_rn_trace, opt_HiMap )
import PrelInfo		( builtinNames )
import TysWiredIn	( boolTyCon )
import SrcLoc		( SrcLoc, mkGeneratedSrcLoc )
import Unique		( Unique, getUnique, unboundKey )
import UniqFM		( UniqFM )
import FiniteMap	( FiniteMap, emptyFM, bagToFM, lookupFM, addToFM, addListToFM, 
			  addListToFM_C, addToFM_C, eltsFM, fmToList
			)
import Bag		( Bag, mapBag, emptyBag, isEmptyBag, snocBag )
import Maybes		( mapMaybe )
import UniqSet
import UniqFM
import UniqSupply
import Util
import Outputable

infixr 9 `thenRn`, `thenRn_`
\end{code}


%************************************************************************
%*									*
\subsection{Somewhat magical interface to other monads}
%*									*
%************************************************************************

\begin{code}
ioToRnM :: IO r -> RnM d (Either IOError r)
ioToRnM io rn_down g_down = (io >>= \ ok -> return (Right ok)) 
			    `catch` 
			    (\ err -> return (Left err))
	    
traceRn :: SDoc -> RnM d ()
traceRn msg | opt_D_dump_rn_trace = putDocRn msg
	    | otherwise		  = returnRn ()

putDocRn :: SDoc -> RnM d ()
putDocRn msg = ioToRnM (printErrs msg)	`thenRn_`
	       returnRn ()
\end{code}


%************************************************************************
%*									*
\subsection{Data types}
%*									*
%************************************************************************

%===================================================
\subsubsection{		MONAD TYPES}
%===================================================

\begin{code}
type RnM d r = RnDown -> d -> IO r
type RnMS r  = RnM SDown r		-- Renaming source
type RnMG r  = RnM ()    r		-- Getting global names etc

	-- Common part
data RnDown = RnDown {
		  rn_mod     :: ModuleName,
		  rn_loc     :: SrcLoc,
		  rn_ns      :: IORef RnNameSupply,
		  rn_errs    :: IORef (Bag WarnMsg, Bag ErrMsg),
	  	  rn_ifaces  :: IORef Ifaces,
		  rn_hi_maps :: (ModuleHiMap,	-- for .hi files
				 ModuleHiMap)	-- for .hi-boot files
		}

	-- For renaming source code
data SDown = SDown {
		  rn_mode :: RnMode,

		  rn_genv :: GlobalRdrEnv,
		    	--   Global envt; the fixity component gets extended
		    	--   with local fixity decls

		  rn_lenv :: LocalRdrEnv,	-- Local name envt
			--   Does *not* include global name envt; may shadow it
			--   Includes both ordinary variables and type variables;
			--   they are kept distinct because tyvar have a different
			--   occurrence contructor (Name.TvOcc)
			-- We still need the unsullied global name env so that
			--   we can look up record field names

		  rn_fixenv :: FixityEnv	-- Local fixities
						-- The global ones are held in the
						-- rn_ifaces field
		}

data RnMode	= SourceMode			-- Renaming source code
		| InterfaceMode			-- Renaming interface declarations.  
\end{code}

%===================================================
\subsubsection{		ENVIRONMENTS}
%===================================================

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
extendRdrEnv	:: RdrNameEnv a -> RdrName -> a -> RdrNameEnv a

emptyRdrEnv  = emptyFM
lookupRdrEnv = lookupFM
addListToRdrEnv = addListToFM
rdrEnvElts	= eltsFM
extendRdrEnv    = addToFM
rdrEnvToList    = fmToList

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
	-- We keep the whole fixity sig so that we
	-- can report line-number info when there is a duplicate
	-- fixity declaration
\end{code}

\begin{code}
--------------------------------
type RnNameSupply
 = ( UniqSupply

   , FiniteMap String Int
	-- This is used as a name supply for dictionary functions
	-- From the inst decl we derive a string, usually by glomming together
	-- the class and tycon name -- but it doesn't matter exactly how;
	-- this map then gives a unique int for each inst decl with that
	-- string.  (In Haskell 98 there can only be one,
	-- but not so in more extended versions; also class CC type T
	-- and class C type TT might both give the string CCT
	--	
	-- We could just use one Int for all the instance decls, but this
	-- way the uniques change less when you add an instance decl,	
	-- hence less recompilation

   , FiniteMap (ModuleName, OccName) Name
	-- Ensures that one (module,occname) pair gets one unique
   )


--------------------------------
data ExportEnv	  = ExportEnv Avails Fixities
type Avails	  = [AvailInfo]
type Fixities	  = [(Name, Fixity)]

type ExportAvails = (FiniteMap ModuleName Avails,
	-- Used to figure out "module M" export specifiers
	-- Includes avails only from *unqualified* imports
	-- (see 1.4 Report Section 5.1.1)

	NameEnv AvailInfo)	-- Used to figure out all other export specifiers.
				-- Maps a Name to the AvailInfo that contains it


data GenAvailInfo name	= Avail name	 -- An ordinary identifier
			| AvailTC name 	 -- The name of the type or class
				  [name] -- The available pieces of type/class.
					 -- NB: If the type or class is itself
					 -- to be in scope, it must be in this list.
					 -- Thus, typically: AvailTC Eq [Eq, ==, /=]

type AvailInfo    = GenAvailInfo Name
type RdrAvailInfo = GenAvailInfo OccName
\end{code}

%===================================================
\subsubsection{		INTERFACE FILE STUFF}
%===================================================

\begin{code}
type ExportItem		 = (ModuleName, [RdrAvailInfo])
type VersionInfo name    = [ImportVersion name]

type ImportVersion name  = (ModuleName, Version, WhetherHasOrphans, WhatsImported name)

type WhetherHasOrphans   = Bool
	-- An "orphan" is 
	-- 	* an instance decl in a module other than the defn module for 
	--		one of the tycons or classes in the instance head
	--	* a transformation rule in a module other than the one defining
	--		the function in the head of the rule.

data WhatsImported name  = Everything 
			 | Specifically [LocalVersion name] -- List guaranteed non-empty

    -- ("M", hif, ver, Everything) means there was a "module M" in 
    -- this module's export list, so we just have to go by M's version, "ver",
    -- not the list of LocalVersions.


type LocalVersion name   = (name, Version)

data ParsedIface
  = ParsedIface {
      pi_mod	   :: Version,		 		-- Module version number
      pi_orphan    :: WhetherHasOrphans,		-- Whether this module has orphans
      pi_usages	   :: [ImportVersion OccName],		-- Usages
      pi_exports   :: [ExportItem],			-- Exports
      pi_decls	   :: [(Version, RdrNameHsDecl)],	-- Local definitions
      pi_insts	   :: [RdrNameInstDecl],		-- Local instance declarations
      pi_rules	   :: [RdrNameRuleDecl]			-- Rules
    }

type InterfaceDetails = (WhetherHasOrphans,
			 VersionInfo Name, -- Version information for what this module imports
			 ExportEnv)	   -- What modules this one depends on


-- needed by Main to fish out the fixities assoc list.
getIfaceFixities :: InterfaceDetails -> Fixities
getIfaceFixities (_, _, ExportEnv _ fs) = fs


type RdrNamePragma = ()				-- Fudge for now
-------------------

data Ifaces = Ifaces {
		iImpModInfo :: ImportedModuleInfo,
				-- Modules this one depends on: that is, the union 
				-- of the modules its *direct* imports depend on.
				-- NB: The direct imports have .hi files that enumerate *all* the
				-- dependencies (direct or not) of the imported module.

		iDecls :: DeclsMap,	-- A single, global map of Names to decls

		iFixes :: FixityEnv,	-- A single, global map of Names to fixities

		iSlurp :: NameSet,
		-- All the names (whether "big" or "small", whether wired-in or not,
		-- whether locally defined or not) that have been slurped in so far.

		iVSlurp :: [(Name,Version)],
		-- All the (a) non-wired-in (b) "big" (c) non-locally-defined 
		-- names that have been slurped in so far, with their versions.
		-- This is used to generate the "usage" information for this module.
		-- Subset of the previous field.

		iInsts :: Bag GatedDecl,
		-- The as-yet un-slurped instance decls; this bag is depleted when we
		-- slurp an instance decl so that we don't slurp the same one twice.
		-- Each is 'gated' by the names that must be available before
		-- this instance decl is needed.

		iRules :: Bag GatedDecl
			-- Ditto transformation rules
	}

type GatedDecl = (NameSet, (Module, RdrNameHsDecl))

type ImportedModuleInfo 
     = FiniteMap ModuleName (Version, Bool, Maybe (Module, Bool, Avails))
		-- Suppose the domain element is module 'A'
		--
		-- The first Bool is True if A contains 
		-- 'orphan' rules or instance decls

		-- The second Bool is true if the interface file actually
		-- read was an .hi-boot file

		-- Nothing => A's interface not yet read, but this module has
		-- 	      imported a module, B, that itself depends on A
		--
		-- Just xx => A's interface has been read.  The Module in 
		--		the Just has the correct Dll flag

		-- This set is used to decide whether to look for
		-- A.hi or A.hi-boot when importing A.f.
		-- Basically, we look for A.hi if A is in the map, and A.hi-boot
		-- otherwise

type DeclsMap = NameEnv (Version, AvailInfo, Bool, (Module, RdrNameHsDecl))
		-- A DeclsMap contains a binding for each Name in the declaration
		-- including the constructors of a type decl etc.
		-- The Bool is True just for the 'main' Name.
\end{code}


%************************************************************************
%*									*
\subsection{Main monad code}
%*									*
%************************************************************************

\begin{code}
initRn :: ModuleName -> UniqSupply -> SearchPath -> SrcLoc
       -> RnMG r
       -> IO (r, Bag ErrMsg, Bag WarnMsg)

initRn mod us dirs loc do_rn = do
  himaps    <- mkModuleHiMaps dirs
  names_var <- newIORef (us, emptyFM, builtins)
  errs_var  <- newIORef (emptyBag,emptyBag)
  iface_var <- newIORef emptyIfaces 
  let
        rn_down = RnDown { rn_loc = loc, rn_ns = names_var, 
			   rn_errs = errs_var, 
			   rn_hi_maps = himaps, 
		  	   rn_ifaces = iface_var,
			   rn_mod = mod }

	-- do the business
  res <- do_rn rn_down ()

	-- grab errors and return
  (warns, errs) <- readIORef errs_var

  return (res, errs, warns)


initRnMS :: GlobalRdrEnv -> FixityEnv -> RnMode -> RnMS r -> RnM d r
initRnMS rn_env fixity_env mode thing_inside rn_down g_down
  = let
	s_down = SDown { rn_genv = rn_env, rn_lenv = emptyRdrEnv, 
			 rn_fixenv = fixity_env, rn_mode = mode }
    in
    thing_inside rn_down s_down

initIfaceRnMS :: Module -> RnMS r -> RnM d r
initIfaceRnMS mod thing_inside 
  = initRnMS emptyRdrEnv emptyNameEnv InterfaceMode $
    setModuleRn (moduleName mod) thing_inside

emptyIfaces :: Ifaces
emptyIfaces = Ifaces { iImpModInfo = emptyFM,
		       iDecls = emptyNameEnv,
		       iFixes = emptyNameEnv,
		       iSlurp = unitNameSet (mkUnboundName dummyRdrVarName),
			-- Pretend that the dummy unbound name has already been
			-- slurped.  This is what's returned for an out-of-scope name,
			-- and we don't want thereby to try to suck it in!
		       iVSlurp = [],
		       iInsts = emptyBag,
		       iRules = emptyBag
	      }

-- mkUnboundName makes a place-holder Name; it shouldn't be looked at except possibly
-- during compiler debugging.
mkUnboundName :: RdrName -> Name
mkUnboundName rdr_name = mkLocalName unboundKey (rdrNameOcc rdr_name) noSrcLoc

isUnboundName :: Name -> Bool
isUnboundName name = getUnique name == unboundKey

builtins :: FiniteMap (ModuleName,OccName) Name
builtins = 
   bagToFM (
   mapBag (\ name ->  ((moduleName (nameModule name), nameOccName name), name))
 	  builtinNames)
\end{code}

@renameSourceCode@ is used to rename stuff ``out-of-line'';
that is, not as part of the main renamer.
Sole examples: derived definitions,
which are only generated in the type checker.

The @RnNameSupply@ includes a @UniqueSupply@, so if you call it more than
once you must either split it, or install a fresh unique supply.

\begin{code}
renameSourceCode :: ModuleName
		 -> RnNameSupply
	         -> RnMS r
	         -> r

renameSourceCode mod_name name_supply m
  = unsafePerformIO (
	-- It's not really unsafe!  When renaming source code we
	-- only do any I/O if we need to read in a fixity declaration;
	-- and that doesn't happen in pragmas etc

        mkModuleHiMaps (mkSearchPath opt_HiMap) >>= \ himaps ->
	newIORef name_supply		>>= \ names_var ->
	newIORef (emptyBag,emptyBag)	>>= \ errs_var ->
    	let
	    rn_down = RnDown { rn_loc = mkGeneratedSrcLoc, rn_ns = names_var,
			       rn_errs = errs_var, rn_hi_maps = himaps,
			       rn_mod = mod_name }
	    s_down = SDown { rn_mode = InterfaceMode,
			       -- So that we can refer to PrelBase.True etc
			     rn_genv = emptyRdrEnv, rn_lenv = emptyRdrEnv,
			     rn_fixenv = emptyNameEnv }
	in
	m rn_down s_down			>>= \ result ->
	
	readIORef errs_var			>>= \ (warns,errs) ->

	(if not (isEmptyBag errs) then
		pprTrace "Urk! renameSourceCode found errors" (display errs) 
#ifdef DEBUG
	 else if not (isEmptyBag warns) then
		pprTrace "Note: renameSourceCode found warnings" (display warns)
#endif
	 else
		id) $

	return result
    )
  where
    display errs = pprBagOfErrors errs

{-# INLINE thenRn #-}
{-# INLINE thenRn_ #-}
{-# INLINE returnRn #-}
{-# INLINE andRn #-}

returnRn :: a -> RnM d a
thenRn   :: RnM d a -> (a -> RnM d b) -> RnM d b
thenRn_  :: RnM d a -> RnM d b -> RnM d b
andRn    :: (a -> a -> a) -> RnM d a -> RnM d a -> RnM d a
mapRn    :: (a -> RnM d b) -> [a] -> RnM d [b]
mapRn_   :: (a -> RnM d b) -> [a] -> RnM d ()
mapMaybeRn :: (a -> RnM d (Maybe b)) -> [a] -> RnM d [b]
sequenceRn :: [RnM d a] -> RnM d [a]
foldlRn :: (b  -> a -> RnM d b) -> b -> [a] -> RnM d b
mapAndUnzipRn :: (a -> RnM d (b,c)) -> [a] -> RnM d ([b],[c])
fixRn    :: (a -> RnM d a) -> RnM d a

returnRn v gdown ldown  = return v
thenRn m k gdown ldown  = m gdown ldown >>= \ r -> k r gdown ldown
thenRn_ m k gdown ldown = m gdown ldown >> k gdown ldown
fixRn m gdown ldown = fixIO (\r -> m r gdown ldown)
andRn combiner m1 m2 gdown ldown
  = m1 gdown ldown >>= \ res1 ->
    m2 gdown ldown >>= \ res2 ->
    return (combiner res1 res2)

sequenceRn []     = returnRn []
sequenceRn (m:ms) =  m			`thenRn` \ r ->
		     sequenceRn ms 	`thenRn` \ rs ->
		     returnRn (r:rs)

mapRn f []     = returnRn []
mapRn f (x:xs)
  = f x		`thenRn` \ r ->
    mapRn f xs 	`thenRn` \ rs ->
    returnRn (r:rs)

mapRn_ f []     = returnRn ()
mapRn_ f (x:xs) = 
    f x		`thenRn_`
    mapRn_ f xs

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


%================
\subsubsection{  Errors and warnings}
%=====================

\begin{code}
failWithRn :: a -> Message -> RnM d a
failWithRn res msg (RnDown {rn_errs = errs_var, rn_loc = loc}) l_down
  = readIORef  errs_var  					>>=  \ (warns,errs) ->
    writeIORef errs_var (warns, errs `snocBag` err)		>> 
    return res
  where
    err = addShortErrLocLine loc msg

warnWithRn :: a -> Message -> RnM d a
warnWithRn res msg (RnDown {rn_errs = errs_var, rn_loc = loc}) l_down
  = readIORef  errs_var  				 	>>=  \ (warns,errs) ->
    writeIORef errs_var (warns `snocBag` warn, errs)	>> 
    return res
  where
    warn = addShortWarnLocLine loc msg

addErrRn :: Message -> RnM d ()
addErrRn err = failWithRn () err

checkRn :: Bool -> Message -> RnM d ()	-- Check that a condition is true
checkRn False err = addErrRn err
checkRn True  err = returnRn ()

warnCheckRn :: Bool -> Message -> RnM d ()	-- Check that a condition is true
warnCheckRn False err = addWarnRn err
warnCheckRn True  err = returnRn ()

addWarnRn :: Message -> RnM d ()
addWarnRn warn = warnWithRn () warn

checkErrsRn :: RnM d Bool		-- True <=> no errors so far
checkErrsRn (RnDown {rn_errs = errs_var}) l_down
  = readIORef  errs_var  				 	>>=  \ (warns,errs) ->
    return (isEmptyBag errs)
\end{code}


%================
\subsubsection{  Source location}
%=====================

\begin{code}
pushSrcLocRn :: SrcLoc -> RnM d a -> RnM d a
pushSrcLocRn loc' m down l_down
  = m (down {rn_loc = loc'}) l_down

getSrcLocRn :: RnM d SrcLoc
getSrcLocRn down l_down
  = return (rn_loc down)
\end{code}

%================
\subsubsection{  Name supply}
%=====================

\begin{code}
getNameSupplyRn :: RnM d RnNameSupply
getNameSupplyRn rn_down l_down
  = readIORef (rn_ns rn_down)

setNameSupplyRn :: RnNameSupply -> RnM d ()
setNameSupplyRn names' (RnDown {rn_ns = names_var}) l_down
  = writeIORef names_var names'

-- See comments with RnNameSupply above.
newInstUniq :: String -> RnM d Int
newInstUniq key (RnDown {rn_ns = names_var}) l_down
  = readIORef names_var				>>= \ (us, mapInst, cache) ->
    let
	uniq = case lookupFM mapInst key of
		   Just x  -> x+1
		   Nothing -> 0
	mapInst' = addToFM mapInst key uniq
    in
    writeIORef names_var (us, mapInst', cache)	>>
    return uniq

getUniqRn :: RnM d Unique
getUniqRn (RnDown {rn_ns = names_var}) l_down
 = readIORef names_var >>= \ (us, mapInst, cache) ->
   let
     (us1,us') = splitUniqSupply us
   in
   writeIORef names_var (us', mapInst, cache)  >>
   return (uniqFromSupply us1)
\end{code}

%================
\subsubsection{  Module}
%=====================

\begin{code}
getModuleRn :: RnM d ModuleName
getModuleRn (RnDown {rn_mod = mod_name}) l_down
  = return mod_name

setModuleRn :: ModuleName -> RnM d a -> RnM d a
setModuleRn new_mod enclosed_thing rn_down l_down
  = enclosed_thing (rn_down {rn_mod = new_mod}) l_down
\end{code}


%************************************************************************
%*									*
\subsection{Plumbing for rename-source part}
%*									*
%************************************************************************

%================
\subsubsection{  RnEnv}
%=====================

\begin{code}
getNameEnvs :: RnMS (GlobalRdrEnv, LocalRdrEnv)
getNameEnvs rn_down (SDown {rn_genv = global_env, rn_lenv = local_env})
  = return (global_env, local_env)

getLocalNameEnv :: RnMS LocalRdrEnv
getLocalNameEnv rn_down (SDown {rn_lenv = local_env})
  = return local_env

setLocalNameEnv :: LocalRdrEnv -> RnMS a -> RnMS a
setLocalNameEnv local_env' m rn_down l_down
  = m rn_down (l_down {rn_lenv = local_env'})

getFixityEnv :: RnMS FixityEnv
getFixityEnv rn_down (SDown {rn_fixenv = fixity_env})
  = return fixity_env

extendFixityEnv :: [(Name, RenamedFixitySig)] -> RnMS a -> RnMS a
extendFixityEnv fixes enclosed_scope
	        rn_down l_down@(SDown {rn_fixenv = fixity_env})
  = let
	new_fixity_env = extendNameEnv fixity_env fixes
    in
    enclosed_scope rn_down (l_down {rn_fixenv = new_fixity_env})
\end{code}

%================
\subsubsection{  Mode}
%=====================

\begin{code}
getModeRn :: RnMS RnMode
getModeRn rn_down (SDown {rn_mode = mode})
  = return mode

setModeRn :: RnMode -> RnMS a -> RnMS a
setModeRn new_mode thing_inside rn_down l_down
  = thing_inside rn_down (l_down {rn_mode = new_mode})
\end{code}


%************************************************************************
%*									*
\subsection{Plumbing for rename-globals part}
%*									*
%************************************************************************

\begin{code}
getIfacesRn :: RnM d Ifaces
getIfacesRn (RnDown {rn_ifaces = iface_var}) _
  = readIORef iface_var

setIfacesRn :: Ifaces -> RnM d ()
setIfacesRn ifaces (RnDown {rn_ifaces = iface_var}) _
  = writeIORef iface_var ifaces

getHiMaps :: RnM d (ModuleHiMap, ModuleHiMap)
getHiMaps (RnDown {rn_hi_maps = himaps}) _ 
  = return himaps
\end{code}

\begin{code}
lookupModuleRn :: ModuleName -> RnM d Module
lookupModuleRn x = 
  getHiMaps `thenRn` \ (himap, _) ->
  case lookupFM himap x of
    Nothing    -> returnRn (mkVanillaModule x)
    Just (_,x) -> returnRn x

\end{code}
