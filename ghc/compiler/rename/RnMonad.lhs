%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[RnMonad]{The monad used by the renamer}

\begin{code}
module RnMonad(
	module RnMonad,

	module RdrName,		-- Re-exports
	module Name,		-- from these two

	Module,
	FiniteMap,
	Bag,
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
import BasicTypes	( Version, defaultFixity )
import ErrUtils		( addShortErrLocLine, addShortWarnLocLine,
			  pprBagOfErrors, ErrMsg, WarnMsg, Message
			)
import RdrName		( RdrName, dummyRdrVarName, rdrNameModule, rdrNameOcc,
			  RdrNameEnv, emptyRdrEnv, extendRdrEnv, 
			  lookupRdrEnv, addListToRdrEnv, rdrEnvToList, rdrEnvElts
			)
import Name		( Name, OccName, NamedThing(..), getSrcLoc,
			  isLocallyDefinedName, nameModule, nameOccName,
			  decode, mkLocalName, mkKnownKeyGlobal,
			  NameEnv, lookupNameEnv, emptyNameEnv, unitNameEnv, 
			  extendNameEnvList
			)
import Module		( Module, ModuleName, WhereFrom, moduleName )
import NameSet		
import CmdLineOpts	( DynFlags, dopt_D_dump_rn_trace )
import SrcLoc		( SrcLoc, mkGeneratedSrcLoc )
import Unique		( Unique )
import FiniteMap	( FiniteMap, emptyFM, listToFM, plusFM )
import Bag		( Bag, mapBag, emptyBag, isEmptyBag, snocBag )
import UniqSupply
import Outputable
import CmFind		( Finder )
import PrelNames	( mkUnboundName )
import HscTypes		( GlobalSymbolTable, OrigNameEnv, AvailEnv, 
			  WhetherHasOrphans, ImportVersion, ExportItem,
			  PersistentRenamerState(..), IsBootInterface, Avails,
			  DeclsMap, IfaceInsts, IfaceRules, DeprecationEnv )

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
traceRn msg
   = doptsRn dopt_D_dump_rn_trace `thenRn` \b ->
     if b then putDocRn msg else returnRn ()

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
data RnDown
  = RnDown {
	rn_mod     :: Module,			-- This module
	rn_loc     :: SrcLoc,			-- Current locn

	rn_finder  :: Finder,
	rn_dflags  :: DynFlags,
	rn_gst     :: GlobalSymbolTable,	-- Both home modules and packages,
						-- at the moment we started compiling 
						-- this module

	rn_errs    :: IORef (Bag WarnMsg, Bag ErrMsg),
	rn_ns      :: IORef (UniqSupply, OrigNameEnv),
	rn_ifaces  :: IORef Ifaces
    }

	-- For renaming source code
data SDown = SDown {
		  rn_mode :: RnMode,

		  rn_genv :: GlobalRdrEnv,    	-- Global envt

		  rn_lenv :: LocalRdrEnv,	-- Local name envt
			--   Does *not* include global name envt; may shadow it
			--   Includes both ordinary variables and type variables;
			--   they are kept distinct because tyvar have a different
			--   occurrence contructor (Name.TvOcc)
			-- We still need the unsullied global name env so that
			--   we can look up record field names

		  rn_fixenv :: FixityEnv	-- Local fixities
			-- The global fixities are held in the
			-- rn_ifaces field.  Why?  See the comments
			-- with RnIfaces.lookupFixity
		}

data RnMode	= SourceMode			-- Renaming source code
		| InterfaceMode			-- Renaming interface declarations.  
\end{code}

%===================================================
\subsubsection{		ENVIRONMENTS}
%===================================================

\begin{code}
--------------------------------
type GlobalRdrEnv = RdrNameEnv [Name]	-- The list is because there may be name clashes
					-- These only get reported on lookup,
					-- not on construction
type LocalRdrEnv  = RdrNameEnv Name

--------------------------------
type FixityEnv = NameEnv RenamedFixitySig
	-- We keep the whole fixity sig so that we
	-- can report line-number info when there is a duplicate
	-- fixity declaration

lookupFixity :: FixityEnv -> Name -> Fixity
lookupFixity env name
  = case lookupNameEnv env name of 
	Just (FixitySig _ fix _) -> fix
	Nothing		  	 -> defaultFixity
\end{code}

\begin{code}
type ExportAvails = (FiniteMap ModuleName Avails,
	-- Used to figure out "module M" export specifiers
	-- Includes avails only from *unqualified* imports
	-- (see 1.4 Report Section 5.1.1)

		     AvailEnv)	-- Used to figure out all other export specifiers.
\end{code}

%===================================================
\subsubsection{		INTERFACE FILE STUFF}
%===================================================

\begin{code}

data ParsedIface
  = ParsedIface {
      pi_mod	   :: Module,				-- Complete with package info
      pi_vers	   :: Version,		 		-- Module version number
      pi_orphan    :: WhetherHasOrphans,		-- Whether this module has orphans
      pi_usages	   :: [ImportVersion OccName],		-- Usages
      pi_exports   :: [ExportItem],			-- Exports
      pi_insts	   :: [RdrNameInstDecl],		-- Local instance declarations
      pi_decls	   :: [(Version, RdrNameHsDecl)],	-- Local definitions
      pi_fixity	   :: (Version, [RdrNameFixitySig]),	-- Local fixity declarations, with their version
      pi_rules	   :: (Version, [RdrNameRuleDecl]),	-- Rules, with their version
      pi_deprecs   :: [RdrNameDeprecation]		-- Deprecations
    }


type RdrNamePragma = ()				-- Fudge for now
-------------------

\end{code}

%************************************************************************
%*									*
\subsection{The renamer state}
%*									*
%************************************************************************

\begin{code}
data Ifaces = Ifaces {

	-- PERSISTENT FIELDS
		iImpModInfo :: ImportedModuleInfo,
				-- Modules this one depends on: that is, the union 
				-- of the modules its *direct* imports depend on.
				-- NB: The direct imports have .hi files that enumerate *all* the
				-- dependencies (direct or not) of the imported module.

		iDecls :: DeclsMap,	-- A single, global map of Names to decls
					-- we can get away with importing them abstractly

		iInsts :: IfaceInsts,
		-- The as-yet un-slurped instance decls; this bag is depleted when we
		-- slurp an instance decl so that we don't slurp the same one twice.
		-- Each is 'gated' by the names that must be available before
		-- this instance decl is needed.

		iRules :: IfaceRules,
		-- Similar to instance decls, only for rules

	-- SEMI-EPHEMERAL FIELDS
		-- iFixes and iDeprecs are accumulated here while one module
		-- is compiled, but are transferred to the package symbol table
		-- at the end.  We don't add them to the table as we encounter them
		-- because doing so would require us to have a mutable symbol table
		-- which is yukky.

		iFixes :: FixityEnv,		-- A single, global map of Names to fixities
						-- See comments with RnIfaces.lookupFixity
		iDeprecs :: DeprecationEnv,

	-- EPHEMERAL FIELDS
	-- These fields persist during the compilation of a single module only

		iSlurp :: NameSet,
		-- All the names (whether "big" or "small", whether wired-in or not,
		-- whether locally defined or not) that have been slurped in so far.

		iVSlurp :: [(Name,Version)]
		-- All the (a) non-wired-in (b) "big" (c) non-locally-defined 
		-- names that have been slurped in so far, with their versions.
		-- This is used to generate the "usage" information for this module.
		-- Subset of the previous field.
	}

type ImportedModuleInfo 
     = FiniteMap ModuleName (WhetherHasOrphans, IsBootInterface, 
			     Maybe (Module, Version, Version, Version, WhereFrom, Avails))
				-- The three Versions are module version, fixity version, rules version

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
\end{code}


%************************************************************************
%*									*
\subsection{Main monad code}
%*									*
%************************************************************************

\begin{code}
initRn :: DynFlags -> Finder -> GlobalSymbolTable
       -> PersistentRenamerState
       -> Module -> SrcLoc
       -> RnMG t
       -> IO (t, (Bag WarnMsg, Bag ErrMsg))

initRn dflags finder gst prs mod loc do_rn
  = do uniqs     <- mkSplitUniqSupply 'r'
       names_var <- newIORef (uniqs, prsOrig prs)
       errs_var  <- newIORef (emptyBag,emptyBag)
       iface_var <- newIORef (initIfaces prs)
       let rn_down = RnDown { rn_mod = mod,
	   		      rn_loc = loc, 
    
		   	      rn_finder = finder,
		   	      rn_dflags = dflags,
		   	      rn_gst    = gst,
			 	     
		   	      rn_ns     = names_var, 
		   	      rn_errs   = errs_var, 
	  	   	      rn_ifaces = iface_var,
	             }

       -- do the business
       res <- do_rn rn_down ()

       -- grab errors and return
       (warns, errs) <- readIORef errs_var

       return (res, (warns, errs))


initIfaces :: PersistentRenamerState -> Ifaces
initIfaces prs
  = Ifaces { iDecls = prsDecls prs,
	     iInsts = prsInsts prs,
	     iRules = prsRules prs,

	     iFixes   = emptyNameEnv,
	     iDeprecs = emptyNameEnv,

	     iImpModInfo = emptyFM,
	     --iDeferred   = emptyNameSet,
	     iSlurp      = unitNameSet (mkUnboundName dummyRdrVarName),
			-- Pretend that the dummy unbound name has already been
			-- slurped.  This is what's returned for an out-of-scope name,
			-- and we don't want thereby to try to suck it in!
	     iVSlurp = []
      }


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
    setModuleRn mod thing_inside

\end{code}

@renameSourceCode@ is used to rename stuff ``out-of-line'';
that is, not as part of the main renamer.
Sole examples: derived definitions,
which are only generated in the type checker.

The @NameSupply@ includes a @UniqueSupply@, so if you call it more than
once you must either split it, or install a fresh unique supply.

\begin{code}
renameSourceCode :: DynFlags 
		 -> Module
		 -> PersistentRenamerState
	         -> RnMS r
	         -> r

renameSourceCode dflags mod prs m
  = unsafePerformIO (
	-- It's not really unsafe!  When renaming source code we
	-- only do any I/O if we need to read in a fixity declaration;
	-- and that doesn't happen in pragmas etc

        mkSplitUniqSupply 'r'			>>= \ new_us ->
	newIORef (new_us, prsOrig prs)		>>= \ names_var ->
	newIORef (emptyBag,emptyBag)		>>= \ errs_var ->
    	let
	    rn_down = RnDown { rn_dflags = dflags,
			       rn_loc = mkGeneratedSrcLoc, rn_ns = names_var,
			       rn_errs = errs_var, 
			       rn_mod = mod, 
			       rn_ifaces = panic "rnameSourceCode: rn_ifaces"  -- Not required
			     }
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
flatMapRn  :: (a -> RnM d [b])       -> [a] -> RnM d [b]
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

flatMapRn f []     = returnRn []
flatMapRn f (x:xs) = f x 		`thenRn` \ r ->
		     flatMapRn f xs	`thenRn` \ rs ->
		     returnRn (r ++ rs)
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

doptsRn :: (DynFlags -> Bool) -> RnM d Bool
doptsRn dopt (RnDown { rn_dflags = dflags}) l_down
   = return (dopt dflags)
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
getNameSupplyRn :: RnM d (UniqSupply, OrigNameEnv)
getNameSupplyRn rn_down l_down
  = readIORef (rn_ns rn_down)

setNameSupplyRn :: (UniqSupply, OrigNameEnv) -> RnM d ()
setNameSupplyRn names' (RnDown {rn_ns = names_var}) l_down
  = writeIORef names_var names'

getUniqRn :: RnM d Unique
getUniqRn (RnDown {rn_ns = names_var}) l_down
 = readIORef names_var >>= \ (us, {-cache,-} ipcache) ->
   let
     (us1,us') = splitUniqSupply us
   in
   writeIORef names_var (us', {-cache,-} ipcache)  >>
   return (uniqFromSupply us1)
\end{code}

%================
\subsubsection{  Module}
%=====================

\begin{code}
getModuleRn :: RnM d Module
getModuleRn (RnDown {rn_mod = mod}) l_down
  = return mod

setModuleRn :: Module -> RnM d a -> RnM d a
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

getGlobalNameEnv :: RnMS GlobalRdrEnv
getGlobalNameEnv rn_down (SDown {rn_genv = global_env})
  = return global_env

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
	new_fixity_env = extendNameEnvList fixity_env fixes
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
\end{code}
