%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[RnMonad]{The monad used by the renamer}

\begin{code}
#include "HsVersions.h"

module RnMonad(
	RnMonad..,
	SST_R
    ) where

IMP_Ubiq(){-uitous-}

import SST
import PreludeGlaST	( SYN_IE(ST), thenST, returnST )

import HsSyn		
import RdrHsSyn
import ErrUtils		( addErrLoc, addShortErrLocLine, addShortWarnLocLine,
			  pprBagOfErrors, SYN_IE(Error), SYN_IE(Warning)
			)
import Name		( SYN_IE(Module), Name, OccName, Provenance, SYN_IE(NameSet),
			  modAndOcc, NamedThing(..)
			)
import CmdLineOpts	( opt_D_show_rn_trace )
import PrelInfo		( builtinNames )
import TyCon		( TyCon {- instance NamedThing -} )
import TysWiredIn	( boolTyCon )
import Pretty
import PprStyle		( PprStyle(..) )
import SrcLoc		( SrcLoc, mkGeneratedSrcLoc )
import Unique		( Unique )
import FiniteMap	( FiniteMap, emptyFM, bagToFM )
import Bag		( Bag, mapBag, emptyBag, isEmptyBag, snocBag )
import UniqSet
import Util

infixr 9 `thenRn`, `thenRn_`
\end{code}


%************************************************************************
%*									*
\subsection{Somewhat magical interface to other monads}
%*									*
%************************************************************************

\begin{code}
#if __GLASGOW_HASKELL__ >= 200
# define REAL_WORLD RealWorld
#else
# define REAL_WORLD _RealWorld
#endif
\end{code}

\begin{code}
sstToIO :: SST REAL_WORLD r -> IO r
sstToIO sst 
  = sstToST sst 	`thenST` \ r -> 
    returnST (Right r)

ioToRnMG :: IO r -> RnMG (Either IOError13 r)
ioToRnMG io rn_down g_down = stToSST io

traceRn :: Pretty -> RnMG ()
traceRn msg | opt_D_show_rn_trace = ioToRnMG (hPutStr stderr (ppShow 80 msg) >> 
					      hPutStr stderr "\n")	`thenRn_`
				    returnRn ()
	    | otherwise		  = returnRn ()
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
type RnMS s r   = RnM s          (SDown s) r		-- Renaming source
type RnMG r     = RnM REAL_WORLD GDown     r		-- Getting global names etc
type MutVar a  = MutableVar REAL_WORLD a		-- ToDo: there ought to be a standard defn of this

	-- Common part
data RnDown s = RnDown
		  SrcLoc
		  (MutableVar s RnNameSupply)
		  (MutableVar s (Bag Warning, Bag Error))
		  (MutableVar s [(Name,Necessity)])		-- Occurrences

data Necessity = Compulsory | Optional		-- We *must* find definitions for
						-- compulsory occurrences; we *may* find them
						-- for optional ones.

	-- For getting global names
data GDown = GDown
		SearchPath
		(MutVar Ifaces)

	-- For renaming source code
data SDown s = SDown
		  RnEnv	
		  Module
		  RnSMode


data RnSMode	= SourceMode
		| InterfaceMode

type SearchPath = [String]		-- List of directories to seach for interface files
type FreeVars	= NameSet
\end{code}

===================================================
		ENVIRONMENTS
===================================================

\begin{code}
type RnNameSupply = (UniqSupply, Int, FiniteMap (Module,OccName) Name)
	-- Ensures that one (m,n) pair gets one unique
	-- The Int is used to give a number to each instance declaration;
	-- it's really a separate name supply.

data RnEnv     	= RnEnv NameEnv FixityEnv
emptyRnEnv	= RnEnv emptyNameEnv emptyFixityEnv

type NameEnv	= FiniteMap RdrName Name
emptyNameEnv	= emptyFM

type FixityEnv		= FiniteMap RdrName (Fixity, Provenance)
emptyFixityEnv	        = emptyFM
	-- It's possible to have a different fixity for B.op than for op:
	--
	--	module A( op ) where		module B where
	--	import qualified B( op )	infixr 2 op
	--	infixl 9 `op`			op = ...
	--	op a b = a `B.op` b

data ExportEnv		= ExportEnv Avails Fixities
type Avails		= [AvailInfo]
type Fixities		= [(OccName, Fixity, Provenance)]
	-- Can contain duplicates, if one module defines the same fixity,
	-- or the same type/class/id, more than once.   Hence a boring old list.
	-- This allows us to report duplicates in just one place, namely plusRnEnv.
	
type ModuleAvails	= FiniteMap Module Avails

data AvailInfo		= NotAvailable | Avail Name [Name]
\end{code}

===================================================
		INTERFACE FILE STUFF
===================================================

\begin{code}
type ExportItem		 = (Module, OccName, [OccName])
type VersionInfo name    = [ImportVersion name]
type ImportVersion name  = (Module, Version, [LocalVersion name])
type LocalVersion name   = (name, Version)

data ParsedIface
  = ParsedIface
      Module		 	-- Module name
      Version		 	-- Module version number
      [ImportVersion OccName]		-- Usages
      [ExportItem]			-- Exports
      [Module]				-- Special instance modules
      [(OccName,Fixity)]		-- Fixities
      [(Version, RdrNameHsDecl)]	-- Local definitions
      [RdrNameInstDecl]			-- Local instance declarations

type InterfaceDetails = (VersionInfo Name,	-- Version information
			 ExportEnv, 		-- What this module exports
			 [Module])		-- Instance modules

type RdrNamePragma = ()				-- Fudge for now
-------------------

data Ifaces = Ifaces
		Module							-- Name of this module
		(FiniteMap Module Version)
		(FiniteMap Module (Avails, [(OccName,Fixity)]))		-- Exports
		VersionMap
		DeclsMap
		(Bag IfaceInst)
		[Module]		-- Set of modules with "special" instance declarations
					-- Excludes this module

type DeclsMap    = FiniteMap Name (AvailInfo, RdrNameHsDecl)
type VersionMap  = FiniteMap Name Version
type IfaceInst   = ([Name], Module, RdrNameInstDecl)	-- The Names are those tycons and
							-- classes mentioned by the instance type
\end{code}


%************************************************************************
%*									*
\subsection{Main monad code}
%*									*
%************************************************************************

\begin{code}
initRn :: Module -> UniqSupply -> SearchPath -> SrcLoc
       -> RnMG r
       -> IO (r, Bag Error, Bag Warning)

initRn mod us dirs loc do_rn
  = sstToIO $
    newMutVarSST (us, 1, builtins)	`thenSST` \ names_var ->
    newMutVarSST (emptyBag,emptyBag)	`thenSST` \ errs_var ->
    newMutVarSST (emptyIfaces mod)	`thenSST` \ iface_var -> 
    newMutVarSST initOccs		`thenSST` \ occs_var ->
    let
	rn_down = RnDown loc names_var errs_var occs_var
	g_down  = GDown dirs iface_var
    in
	-- do the buisness
    do_rn rn_down g_down		`thenSST` \ res ->

	-- grab errors and return
    readMutVarSST errs_var			`thenSST` \ (warns,errs) ->
    returnSST (res, errs, warns)


initRnMS :: RnEnv -> Module -> RnSMode -> RnMS REAL_WORLD r -> RnMG r
initRnMS env mod_name mode m rn_down g_down
  = let
	s_down = SDown env mod_name mode
    in
    m rn_down s_down


emptyIfaces :: Module -> Ifaces
emptyIfaces mod = Ifaces mod emptyFM emptyFM emptyFM emptyFM emptyBag []

builtins :: FiniteMap (Module,OccName) Name
builtins = bagToFM (mapBag (\ name -> (modAndOcc name, name)) builtinNames)

	-- Initial value for the occurrence pool.
initOccs :: [(Name,Necessity)]
initOccs = [(getName boolTyCon, Compulsory)]
	-- Booleans occur implicitly a lot, so it's tiresome to keep recording the fact, and
	-- rather implausible that not one will be used in the module.
	-- We could add some other common types, notably lists, but the general idea is
	-- to do as much as possible explicitly.
\end{code}

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
	         -> RnMS REAL_WORLD r
	         -> r

-- Alas, we can't use the real runST, with the desired signature:
--	renameSourceCode :: RnNameSupply -> RnMS s r -> r
-- because we can't manufacture "new versions of runST".

renameSourceCode mod_name name_supply m
  = runSST (
	newMutVarSST name_supply		`thenSST` \ names_var ->
	newMutVarSST (emptyBag,emptyBag)	`thenSST` \ errs_var ->
	newMutVarSST []				`thenSST` \ occs_var ->
    	let
	    rn_down = RnDown mkGeneratedSrcLoc names_var errs_var occs_var
	    s_down = SDown emptyRnEnv mod_name InterfaceMode
	in
	m rn_down s_down			`thenSST` \ result ->
	
	readMutVarSST errs_var			`thenSST` \ (warns,errs) ->

	(if not (isEmptyBag errs) then
		trace ("Urk! renameSourceCode found errors" ++ display errs) 
	 else if not (isEmptyBag warns) then
		trace ("Urk! renameSourceCode found warnings" ++ display warns)
	 else
		id) $

	returnSST result
    )
  where
    display errs = ppShow 80 (pprBagOfErrors PprDebug errs)

{-# INLINE thenRn #-}
{-# INLINE thenRn_ #-}
{-# INLINE returnRn #-}
{-# INLINE andRn #-}

returnRn :: a -> RnM s d a
thenRn   :: RnM s d a -> (a -> RnM s d b) -> RnM s d b
thenRn_  :: RnM s d a -> RnM s d b -> RnM s d b
andRn    :: (a -> a -> a) -> RnM s d a -> RnM s d a -> RnM s d a
mapRn    :: (a -> RnM s d b) -> [a] -> RnM s d [b]
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
\end{code}



%************************************************************************
%*									*
\subsection{Boring plumbing for common part}
%*									*
%************************************************************************


================  Errors and warnings =====================

\begin{code}
failWithRn :: a -> Error -> RnM s d a
failWithRn res msg (RnDown loc names_var errs_var occs_var) l_down
  = readMutVarSST  errs_var  					`thenSST`  \ (warns,errs) ->
    writeMutVarSST errs_var (warns, errs `snocBag` err)		`thenSST_` 
    returnSST res
  where
    err = addShortErrLocLine loc msg

warnWithRn :: a -> Warning -> RnM s d a
warnWithRn res msg (RnDown loc names_var errs_var occs_var) l_down
  = readMutVarSST  errs_var  				 	`thenSST`  \ (warns,errs) ->
    writeMutVarSST errs_var (warns `snocBag` warn, errs)	`thenSST_` 
    returnSST res
  where
    warn = addShortWarnLocLine loc msg

addErrRn :: Error -> RnM s d ()
addErrRn err = failWithRn () err

checkRn :: Bool -> Error -> RnM s d ()	-- Check that a condition is true
checkRn False err  = addErrRn err
checkRn True err = returnRn ()

addWarnRn :: Warning -> RnM s d ()
addWarnRn warn = warnWithRn () warn

checkErrsRn :: RnM s d Bool		-- True <=> no errors so far
checkErrsRn  (RnDown loc names_var errs_var occs_var) l_down
  = readMutVarSST  errs_var  				 	`thenSST`  \ (warns,errs) ->
    returnSST (isEmptyBag errs)
\end{code}


================  Source location =====================

\begin{code}
pushSrcLocRn :: SrcLoc -> RnM s d a -> RnM s d a
pushSrcLocRn loc' m (RnDown loc names_var errs_var occs_var) l_down
  = m (RnDown loc' names_var errs_var occs_var) l_down

getSrcLocRn :: RnM s d SrcLoc
getSrcLocRn (RnDown loc names_var errs_var occs_var) l_down
  = returnSST loc
\end{code}

================  Name supply =====================

\begin{code}
getNameSupplyRn :: RnM s d RnNameSupply
getNameSupplyRn (RnDown loc names_var errs_var occs_var) l_down
  = readMutVarSST names_var

setNameSupplyRn :: RnNameSupply -> RnM s d ()
setNameSupplyRn names' (RnDown loc names_var errs_var occs_var) l_down
  = writeMutVarSST names_var names'
\end{code}

================  Occurrences =====================

\begin{code}
addOccurrenceName :: Necessity -> Name -> RnM s d ()
addOccurrenceName necessity name (RnDown loc names_var errs_var occs_var) l_down
  = readMutVarSST occs_var			`thenSST` \ occs ->
    writeMutVarSST occs_var ((name,necessity) : occs)

addOccurrenceNames :: Necessity -> [Name] -> RnM s d ()
addOccurrenceNames necessity names (RnDown loc names_var errs_var occs_var) l_down
  = readMutVarSST occs_var			`thenSST` \ occs ->
    writeMutVarSST occs_var ([(name,necessity) | name <- names] ++ occs)

popOccurrenceName :: RnM s d (Maybe (Name,Necessity))
popOccurrenceName (RnDown loc names_var errs_var occs_var) l_down
  = readMutVarSST occs_var			`thenSST` \ occs ->
    case occs of
	[] 	   -> returnSST Nothing
	(occ:occs) -> writeMutVarSST occs_var occs	`thenSST_`
		      returnSST (Just occ)

-- findOccurrencesRn does the enclosed thing with a *fresh* occurrences
-- variable, and returns the list of occurrences thus found.  It's useful
-- when loading instance decls and specialisation signatures, when we want to
-- know the names of the things in the types, but we don't want to treat them
-- as occurrences.

findOccurrencesRn :: RnM s d a -> RnM s d [Name]
findOccurrencesRn enclosed_thing (RnDown loc names_var errs_var occs_var) l_down
  = newMutVarSST []							`thenSST` \ new_occs_var ->
    enclosed_thing (RnDown loc names_var errs_var new_occs_var) l_down	`thenSST_`
    readMutVarSST new_occs_var						`thenSST` \ occs ->
    returnSST (map fst occs)
\end{code}


%************************************************************************
%*									*
\subsection{Plumbing for rename-source part}
%*									*
%************************************************************************

================  RnEnv  =====================

\begin{code}
getNameEnv :: RnMS s NameEnv
getNameEnv rn_down (SDown (RnEnv name_env fixity_env) mod_name mode)
  = returnSST name_env

setNameEnv :: NameEnv -> RnMS s a -> RnMS s a
setNameEnv name_env' m rn_down (SDown (RnEnv name_env fixity_env) mod_name mode)
  = m rn_down (SDown (RnEnv name_env' fixity_env) mod_name mode)

getFixityEnv :: RnMS s FixityEnv
getFixityEnv rn_down (SDown (RnEnv name_env fixity_env) mod_name mode)
  = returnSST fixity_env

setRnEnv :: RnEnv -> RnMS s a -> RnMS s a 
setRnEnv rn_env' m rn_down (SDown rn_env mod_name mode)
  = m rn_down (SDown rn_env' mod_name mode)
\end{code}

================  Module and Mode =====================

\begin{code}
getModuleRn :: RnMS s Module
getModuleRn rn_down (SDown rn_env mod_name mode)
  = returnSST mod_name
\end{code}

\begin{code}
getModeRn :: RnMS s RnSMode
getModeRn rn_down (SDown rn_env mod_name mode)
  = returnSST mode
\end{code}


%************************************************************************
%*									*
\subsection{Plumbing for rename-globals part}
%*									*
%************************************************************************

\begin{code}
getIfacesRn :: RnMG Ifaces
getIfacesRn rn_down (GDown dirs iface_var)
  = readMutVarSST iface_var

setIfacesRn :: Ifaces -> RnMG ()
setIfacesRn ifaces rn_down (GDown dirs iface_var)
  = writeMutVarSST iface_var ifaces

getSearchPathRn :: RnMG SearchPath
getSearchPathRn rn_down (GDown dirs iface_var)
  = returnSST dirs
\end{code}
