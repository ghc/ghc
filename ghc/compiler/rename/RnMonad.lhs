%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1997
%
\section[RnMonad]{The monad used by the renamer}

\begin{code}
#include "HsVersions.h"

module RnMonad(
	EXP_MODULE(RnMonad),
	 -- close it up (partly done to allow unfoldings)
	EXP_MODULE(SST),
	SYN_IE(Module),
	FiniteMap,
	Bag,
	Name,
	SYN_IE(RdrNameHsDecl),
	SYN_IE(RdrNameInstDecl),
	SYN_IE(Version),
	SYN_IE(NameSet),
	OccName,
	Fixity
    ) where

IMP_Ubiq(){-uitous-}

import SST
#if __GLASGOW_HASKELL__ <= 201
import PreludeGlaST	( SYN_IE(ST), thenStrictlyST, returnStrictlyST )
#define MkIO
#else
import GlaExts
import IO
import ST
import IOBase
#define IOError13 IOError
#define MkIO IO
#endif

import HsSyn		
import RdrHsSyn
import BasicTypes	( SYN_IE(Version), NewOrData )
import ErrUtils		( addErrLoc, addShortErrLocLine, addShortWarnLocLine,
			  pprBagOfErrors, SYN_IE(Error), SYN_IE(Warning)
			)
import Name		( SYN_IE(Module), Name, OccName, Provenance, SYN_IE(NameSet), emptyNameSet,
			  isLocallyDefinedName,
			  modAndOcc, NamedThing(..)
			)
import CmdLineOpts	( opt_D_show_rn_trace, opt_IgnoreIfacePragmas )
import PrelInfo		( builtinNames )
import TyCon		( TyCon {- instance NamedThing -} )
import TysWiredIn	( boolTyCon )
import Pretty
import Outputable	( PprStyle(..), printErrs )
import SrcLoc		( SrcLoc, mkGeneratedSrcLoc )
import Unique		( Unique )
import UniqFM		( UniqFM )
import FiniteMap	( FiniteMap, emptyFM, bagToFM )
import Bag		( Bag, mapBag, emptyBag, isEmptyBag, snocBag )
import UniqSet
import Util
#if __GLASGOW_HASKELL__ >= 202
import UniqSupply
#endif

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
sstToIO sst =
    MkIO (
    sstToST sst 	`thenStrictlyST` \ r -> 
    returnStrictlyST (Right r))

ioToRnMG :: IO r -> RnMG (Either IOError13 r)
ioToRnMG (MkIO io) rn_down g_down = stToSST io

traceRn :: Doc -> RnMG ()
traceRn msg | opt_D_show_rn_trace = putDocRn msg
	    | otherwise		  = returnRn ()

putDocRn :: Doc -> RnMG ()
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
type RnMS s r   = RnM s          (SDown s) r		-- Renaming source
type RnMG r     = RnM REAL_WORLD GDown     r		-- Getting global names etc
type MutVar a  = MutableVar REAL_WORLD a		-- ToDo: there ought to be a standard defn of this

	-- Common part
data RnDown s = RnDown
		  SrcLoc
		  (MutableVar s RnNameSupply)
		  (MutableVar s (Bag Warning, Bag Error))
		  (MutableVar s ([Name],[Name]))	-- Occurrences: compulsory and optional resp

data Necessity = Compulsory | Optional		-- We *must* find definitions for
						-- compulsory occurrences; we *may* find them
						-- for optional ones.

	-- For getting global names
data GDown = GDown
		SearchPath
		(MutVar Ifaces)

	-- For renaming source code
data SDown s = SDown
		  RnEnv			-- Global envt
		  NameEnv		-- Local name envt (includes global name envt, 
					-- but may shadow it)
		  Module
		  RnSMode


data RnSMode	= SourceMode			-- Renaming source code
		| InterfaceMode Necessity	-- Renaming interface declarations.  The "necessity"
						-- flag says free variables *must* be found and slurped
						-- or whether they need not be.  For value signatures of
						-- things that are themselves compulsorily imported
						-- we arrange that the type signature is read in compulsory mode,
						-- but the pragmas in optional mode.

type SearchPath = [(String,String)]	-- List of (directory,suffix) pairs to search 
                                        -- for interface files.
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
type Fixities		= [(OccName, (Fixity, Provenance))]
	-- Can contain duplicates, if one module defines the same fixity,
	-- or the same type/class/id, more than once.   Hence a boring old list.
	-- This allows us to report duplicates in just one place, namely plusRnEnv.
	
type ExportAvails	= (FiniteMap Module Avails,	-- Used to figure out "module M" export specifiers
							-- Includes avails only from *unqualified* imports
							-- (see 1.4 Report Section 5.1.1)

			   UniqFM AvailInfo)		-- Used to figure out all other export specifiers.
							-- Maps a Name to the AvailInfo that contains it
							-- NB: Contain bindings for class ops but 
							-- not constructors (see defn of availEntityNames)


data GenAvailInfo name	= NotAvailable 
			| Avail name		-- An ordinary identifier
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
type ExportItem		 = (Module, IfaceFlavour, [RdrAvailInfo])
type VersionInfo name    = [ImportVersion name]
type ImportVersion name  = (Module, IfaceFlavour, Version, [LocalVersion name])
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
		Module						-- Name of this module
		(FiniteMap Module (IfaceFlavour, 		-- Exports
				   Version, 
				   Avails, 
				   [(OccName,Fixity)]))
		DeclsMap

		NameSet			-- All the names (whether "big" or "small", whether wired-in or not,
					-- whether locally defined or not) that have been slurped in so far.

		[(Name,Version)]	-- All the (a) non-wired-in (b) "big" (c) non-locally-defined names that 
					-- have been slurped in so far, with their versions. 
					-- This is used to generate the "usage" information for this module.
					-- Subset of the previous field.

		(Bag IfaceInst, NameSet) -- The as-yet un-slurped instance decls; this bag is depleted when we
					 -- slurp an instance decl so that we don't slurp the same one twice.
					 -- Together with them is the set of tycons/classes that may allow 
					 -- the instance decls in.

		(FiniteMap Name RdrNameTyDecl)
					-- Deferred data type declarations; each has the following properties
					--	* it's a data type decl
					--	* its TyCon is needed
					--	* the decl may or may not have been slurped, depending on whether any
					--	  of the constrs are needed.

		[Module]		-- Set of modules with "special" instance declarations
					-- Excludes this module


type DeclsMap    = FiniteMap Name (Version, AvailInfo, RdrNameHsDecl)
type IfaceInst   = ((Module, RdrNameInstDecl),	-- Instance decl
		    [Name])			-- "Gate" names.  Slurp this instance decl when this
						-- list becomes empty.  It's depleted whenever we
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
initRnMS rn_env@(RnEnv name_env _) mod_name mode m rn_down g_down
  = let
	s_down = SDown rn_env name_env mod_name mode
    in
    m rn_down s_down


emptyIfaces :: Module -> Ifaces
emptyIfaces mod = Ifaces mod emptyFM emptyFM emptyNameSet [] (emptyBag, emptyNameSet) emptyFM []

builtins :: FiniteMap (Module,OccName) Name
builtins = bagToFM (mapBag (\ name -> (modAndOcc name, name)) builtinNames)

	-- Initial value for the occurrence pool.
initOccs :: ([Name],[Name])	-- Compulsory and optional respectively
initOccs = ([getName boolTyCon], [])
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
	newMutVarSST ([],[])			`thenSST` \ occs_var ->
    	let
	    rn_down = RnDown mkGeneratedSrcLoc names_var errs_var occs_var
	    s_down = SDown emptyRnEnv emptyNameEnv mod_name (InterfaceMode Compulsory)
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
    display errs = show (pprBagOfErrors PprDebug errs)

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

-- The "instance-decl unique supply", inst, is just an integer that's used to
-- give a unique number for each instance declaration.
newInstUniq :: RnM s d Int
newInstUniq (RnDown loc names_var errs_var occs_var) l_down
  = readMutVarSST names_var				`thenSST` \ (us, inst, cache) ->
    writeMutVarSST names_var (us, inst+1, cache)	`thenSST_` 
    returnSST inst
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
addOccurrenceName name (RnDown loc names_var errs_var occs_var)
		       (SDown rn_env local_env mod_name mode)
  | isLocallyDefinedName name ||
    not_necessary necessity
  = returnSST name

  | otherwise
  = readMutVarSST occs_var			`thenSST` \ (comp_occs, opt_occs) ->
    let
	new_occ_pair = case necessity of
			 Optional   -> (comp_occs, name:opt_occs)
			 Compulsory -> (name:comp_occs, opt_occs)
    in
    writeMutVarSST occs_var new_occ_pair	`thenSST_`
    returnSST name
  where
    necessity = case mode of 
		  SourceMode	          -> Compulsory
		  InterfaceMode necessity -> necessity


addOccurrenceNames :: [Name] -> RnMS s ()
addOccurrenceNames names (RnDown loc names_var errs_var occs_var)
		         (SDown rn_env local_env mod_name mode)
  | not_necessary necessity 
  = returnSST ()

  | otherwise
  = readMutVarSST occs_var			`thenSST` \ (comp_occs, opt_occs) ->
    let
	new_occ_pair = case necessity of
			 Optional   -> (comp_occs, non_local_names ++ opt_occs)
			 Compulsory -> (non_local_names ++ comp_occs, opt_occs)
    in
    writeMutVarSST occs_var new_occ_pair
  where
    non_local_names = filter (not . isLocallyDefinedName) names
    necessity = case mode of 
		  SourceMode	          -> Compulsory
		  InterfaceMode necessity -> necessity

	-- Never look for optional things if we're
	-- ignoring optional input interface information
not_necessary Compulsory = False
not_necessary Optional   = opt_IgnoreIfacePragmas

popOccurrenceName :: Necessity -> RnM s d (Maybe Name)
popOccurrenceName necessity (RnDown loc names_var errs_var occs_var) l_down
  = readMutVarSST occs_var			`thenSST` \ occs ->
    case (necessity, occs) of
		-- Find a compulsory occurrence
	(Compulsory, (comp:comps, opts)) -> writeMutVarSST occs_var (comps, opts)	`thenSST_`
					    returnSST (Just comp)

		-- Find an optional occurrence
		-- We shouldn't be looking unless we've done all the compulsories
	(Optional, (comps, opt:opts)) -> ASSERT( null comps )
					 writeMutVarSST occs_var (comps, opts)	`thenSST_`
					 returnSST (Just opt)

		-- No suitable occurrence
	other -> returnSST Nothing

-- findOccurrencesRn does the enclosed thing with a *fresh* occurrences
-- variable, and returns the list of occurrences thus found.  It's useful
-- when loading instance decls and specialisation signatures, when we want to
-- know the names of the things in the types, but we don't want to treat them
-- as occurrences.

findOccurrencesRn :: RnM s d a -> RnM s d [Name]
findOccurrencesRn enclosed_thing (RnDown loc names_var errs_var occs_var) l_down
  = newMutVarSST ([],[])						`thenSST` \ new_occs_var ->
    enclosed_thing (RnDown loc names_var errs_var new_occs_var) l_down	`thenSST_`
    readMutVarSST new_occs_var						`thenSST` \ (occs,_) ->
    returnSST occs
\end{code}


%************************************************************************
%*									*
\subsection{Plumbing for rename-source part}
%*									*
%************************************************************************

================  RnEnv  =====================

\begin{code}
getGlobalNameEnv :: RnMS s NameEnv
getGlobalNameEnv rn_down (SDown (RnEnv global_env fixity_env) local_env mod_name mode)
  = returnSST global_env

getNameEnv :: RnMS s NameEnv
getNameEnv rn_down (SDown rn_env local_env mod_name mode)
  = returnSST local_env

setNameEnv :: NameEnv -> RnMS s a -> RnMS s a
setNameEnv local_env' m rn_down (SDown rn_env local_env mod_name mode)
  = m rn_down (SDown rn_env local_env' mod_name mode)

getFixityEnv :: RnMS s FixityEnv
getFixityEnv rn_down (SDown (RnEnv name_env fixity_env) local_env mod_name mode)
  = returnSST fixity_env
\end{code}

================  Module and Mode =====================

\begin{code}
getModuleRn :: RnMS s Module
getModuleRn rn_down (SDown rn_env local_env mod_name mode)
  = returnSST mod_name
\end{code}

\begin{code}
getModeRn :: RnMS s RnSMode
getModeRn rn_down (SDown rn_env local_env mod_name mode)
  = returnSST mode

setModeRn :: RnSMode -> RnMS s a -> RnMS s a
setModeRn new_mode thing_inside rn_down (SDown rn_env local_env mod_name mode)
  = thing_inside rn_down (SDown rn_env local_env mod_name new_mode)
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
