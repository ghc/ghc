%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1997
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
import BasicTypes	( Version, NewOrData, pprModule )
import SrcLoc		( noSrcLoc )
import ErrUtils		( addShortErrLocLine, addShortWarnLocLine,
			  pprBagOfErrors, ErrMsg, WarnMsg
			)
import Maybes	        ( seqMaybe, mapMaybe )			
import Name		( Module, Name, OccName, PrintUnqualified, NameSet, emptyNameSet,
			  isLocallyDefinedName,
			  modAndOcc, NamedThing(..)
			)
import CmdLineOpts	( opt_D_show_rn_trace, opt_IgnoreIfacePragmas, opt_WarnHiShadows )
import PrelInfo		( builtinNames )
import TysWiredIn	( boolTyCon )
import SrcLoc		( SrcLoc, mkGeneratedSrcLoc )
import Unique		( Unique )
import UniqFM		( UniqFM )
import FiniteMap	( FiniteMap, emptyFM, bagToFM, lookupFM, addToFM, addListToFM_C )
import Bag		( Bag, mapBag, emptyBag, isEmptyBag, snocBag )
import UniqSet
import UniqSupply
import Util
import Outputable
import DirUtils		( getDirectoryContents )

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
data RnDown s = RnDown
		  SrcLoc
		  (SSTRef s (GenRnNameSupply s))
		  (SSTRef s (Bag WarnMsg, Bag ErrMsg))
		  (SSTRef s ([Occurrence],[Occurrence]))	-- Occurrences: compulsory and optional resp

type Occurrence = (Name, SrcLoc)		-- The srcloc is the occurrence site

data Necessity = Compulsory | Optional		-- We *must* find definitions for
						-- compulsory occurrences; we *may* find them
						-- for optional ones.

	-- For getting global names
data GDown = GDown
		ModuleHiMap
		(SSTRWRef Ifaces)

	-- For renaming source code
data SDown s = SDown
		  RnEnv			-- Global envt
		  NameEnv		-- Local name envt (includes global name envt, 
					-- but may shadow it)
		  Module
		  RnSMode


data RnSMode	= SourceMode			-- Renaming source code
		| InterfaceMode			-- Renaming interface declarations.  
			Necessity		-- The "necessity"
						-- flag says free variables *must* be found and slurped
						-- or whether they need not be.  For value signatures of
						-- things that are themselves compulsorily imported
						-- we arrange that the type signature is read 
						-- in compulsory mode,
						-- but the pragmas in optional mode.
			(Name -> PrintUnqualified)	-- Tells whether the thing can be printed unqualified

type SearchPath = [(String,String)]	-- List of (directory,suffix) pairs to search 
                                        -- for interface files.

type ModuleHiMap = FiniteMap String String 
   -- mapping from module name to the file path of its corresponding
   -- interface file.

type FreeVars	= NameSet
\end{code}

===================================================
		ENVIRONMENTS
===================================================

\begin{code}
type RnNameSupply = GenRnNameSupply RealWorld

type GenRnNameSupply s
 = ( UniqSupply
   , FiniteMap FAST_STRING (SSTRef s Int)
   , FiniteMap (Module,OccName) Name
   )
	-- Ensures that one (m,n) pair gets one unique
	-- The finite map on FAST_STRINGS is used to give a per-class unique to each
	-- instance declaration; it's really a separate name supply.

data RnEnv     	= RnEnv GlobalNameEnv FixityEnv
emptyRnEnv	= RnEnv emptyNameEnv  emptyFixityEnv

type GlobalNameEnv = FiniteMap RdrName (Name, HowInScope)
emptyGlobalNameEnv = emptyFM

data HowInScope		-- Used for error messages only
   = FromLocalDefn SrcLoc
   | FromImportDecl Module SrcLoc

type NameEnv	= FiniteMap RdrName Name
emptyNameEnv	= emptyFM

type FixityEnv		= FiniteMap RdrName (Fixity, HowInScope)
emptyFixityEnv	        = emptyFM
	-- It's possible to have a different fixity for B.op than for op:
	--
	--	module A( op ) where		module B where
	--	import qualified B( op )	infixr 2 op
	--	infixl 9 `op`			op = ...
	--	op a b = a `B.op` b

data ExportEnv		= ExportEnv Avails Fixities
type Avails		= [AvailInfo]
type Fixities		= [(OccName, Fixity)]

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

type ImportVersion name  = (Module, IfaceFlavour, Version, WhatsImported name)
data WhatsImported name  = Everything 
			 | Specifically [LocalVersion name]	-- List guaranteed non-empty

    -- ("M", hif, ver, Everything) means there was a "module M" in 
    -- this module's export list, so we just have to go by M's version, "ver",
    -- not the list of LocalVersions.


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

type InterfaceDetails = (VersionInfo Name,	-- Version information for what this module imports
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
       -> IO (r, Bag ErrMsg, Bag WarnMsg)

initRn mod us dirs loc do_rn = do
  names_var <- sstToIO (newMutVarSST (us, emptyFM, builtins))
  errs_var  <- sstToIO (newMutVarSST (emptyBag,emptyBag))
  iface_var <- sstToIO (newMutVarSST (emptyIfaces mod))
  occs_var  <- sstToIO (newMutVarSST initOccs)
  himap     <- mkModuleHiMap dirs
  let
        rn_down = RnDown loc names_var errs_var occs_var
	g_down  = GDown himap iface_var

	-- do the buisness
  res <- sstToIO (do_rn rn_down g_down)

	-- grab errors and return
  (warns, errs) <- sstToIO (readMutVarSST errs_var)
  return (res, errs, warns)


initRnMS :: RnEnv -> Module -> RnSMode -> RnMS RealWorld r -> RnMG r
initRnMS rn_env@(RnEnv name_env _) mod_name mode m rn_down g_down
  = let
	s_down = SDown rn_env emptyNameEnv mod_name mode
    in
    m rn_down s_down


emptyIfaces :: Module -> Ifaces
emptyIfaces mod = Ifaces mod emptyFM emptyFM emptyNameSet [] (emptyBag, emptyNameSet) emptyFM []

builtins :: FiniteMap (Module,OccName) Name
builtins = bagToFM (mapBag (\ name -> (modAndOcc name, name)) builtinNames)

	-- Initial value for the occurrence pool.
initOccs :: ([Occurrence],[Occurrence])	-- Compulsory and optional respectively
initOccs = ([(getName boolTyCon, noSrcLoc)], [])
	-- Booleans occur implicitly a lot, so it's tiresome to keep recording the fact, and
	-- rather implausible that not one will be used in the module.
	-- We could add some other common types, notably lists, but the general idea is
	-- to do as much as possible explicitly.
\end{code}

\begin{code}
mkModuleHiMap :: SearchPath -> IO ModuleHiMap
mkModuleHiMap dirs = do
  lss <- mapM (uncurry getAllFilesMatching) dirs
  let ls = concat lss
  if opt_WarnHiShadows
   then return (addListToFM_C conflict env ls)
   else return (addListToFM_C (\ old new -> old) env ls)
 where
  env = emptyFM

  conflict old_path new_path
    | old_path /= new_path = 
        pprTrace "Warning: " (text "Identically named interface files present on import path, " $$
			      text (show old_path) <+> text "shadows" $$
			      text (show new_path) $$
			      text "on the import path: " <+> 
			      text (concat (intersperse ":" (map fst dirs))))
        old_path
    | otherwise = old_path  -- don't warn about innocous shadowings.

getAllFilesMatching :: FilePath -> String -> IO [(String, FilePath)]
getAllFilesMatching dir_path suffix = do
  fpaths <- getDirectoryContents dir_path
  -- fpaths entries do not have dir_path prepended
  return (mapMaybe withSuffix fpaths)
 where
   xiffus = reverse dotted_suffix 
  
   dotted_suffix =
    case suffix of
      [] -> []
      ('.':xs) -> suffix
      ls -> '.':ls

    -- filter out files that have the desired suffix
   withSuffix nm = go ""  xiffus rev_nm     `seqMaybe` 
                   go "b" "toob-ih." rev_nm
    where
     rev_nm  = reverse nm

     -- the prefix is needed to distinguish between a .hi-boot
     -- file and a normal interface file, i.e., I'm not willing
     -- to guarantee that the presence of the SOURCE pragma
     --
     --   import {-# SOURCE #-} Foo (x)
     --   import Bar
     --
     -- will not cause Foo.hi to somehow be looked at when
     -- slurping in Bar.
     -- 
     go pre [] xs     = Just (pre ++ reverse xs, dir_path ++'/':nm)
     go _ _  []       = Nothing
     go pre (x:xs) (y:ys) 
       | x == y       = go pre xs ys 
       | otherwise    = Nothing
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
	    rn_down = RnDown mkGeneratedSrcLoc names_var errs_var occs_var
	    s_down = SDown emptyRnEnv emptyNameEnv mod_name (InterfaceMode Compulsory (\_ -> False))
	in
	m rn_down s_down			`thenSST` \ result ->
	
	readMutVarSST errs_var			`thenSST` \ (warns,errs) ->

	(if not (isEmptyBag errs) then
		pprTrace "Urk! renameSourceCode found errors" (display errs) 
#ifdef DEBUG
	 else if not (isEmptyBag warns) then
		pprTrace "Urk! renameSourceCode found warnings" (display warns)
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
mapMaybeRn :: (a -> RnM s d b) -> b -> Maybe a -> RnM s d b
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

mapMaybeRn f def Nothing  = returnRn def
mapMaybeRn f def (Just v) = f v
\end{code}



%************************************************************************
%*									*
\subsection{Boring plumbing for common part}
%*									*
%************************************************************************


================  Errors and warnings =====================

\begin{code}
failWithRn :: a -> ErrMsg -> RnM s d a
failWithRn res msg (RnDown loc names_var errs_var occs_var) l_down
  = readMutVarSST  errs_var  					`thenSST`  \ (warns,errs) ->
    writeMutVarSST errs_var (warns, errs `snocBag` err)		`thenSST_` 
    returnSST res
  where
    err = addShortErrLocLine loc msg

warnWithRn :: a -> WarnMsg -> RnM s d a
warnWithRn res msg (RnDown loc names_var errs_var occs_var) l_down
  = readMutVarSST  errs_var  				 	`thenSST`  \ (warns,errs) ->
    writeMutVarSST errs_var (warns `snocBag` warn, errs)	`thenSST_` 
    returnSST res
  where
    warn = addShortWarnLocLine loc msg

addErrRn :: ErrMsg -> RnM s d ()
addErrRn err = failWithRn () err

checkRn :: Bool -> ErrMsg -> RnM s d ()	-- Check that a condition is true
checkRn False err  = addErrRn err
checkRn True err = returnRn ()

addWarnRn :: WarnMsg -> RnM s d ()
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
getNameSupplyRn :: RnM s d (GenRnNameSupply s)
getNameSupplyRn (RnDown loc names_var errs_var occs_var) l_down
  = readMutVarSST names_var

setNameSupplyRn :: GenRnNameSupply s -> RnM s d ()
setNameSupplyRn names' (RnDown loc names_var errs_var occs_var) l_down
  = writeMutVarSST names_var names'

-- The "instance-decl unique supply", inst, is really a map from class names
-- to unique supplies. Having per-class unique numbers for instance decls helps
-- the recompilation checker.
newInstUniq :: FAST_STRING -> RnM s d Int
newInstUniq cname (RnDown loc names_var errs_var occs_var) l_down
  = readMutVarSST names_var				`thenSST` \ (us, mapInst, cache) ->
    case lookupFM mapInst cname of
      Just class_us ->
         readMutVarSST  class_us       `thenSST`  \ v ->
	 writeMutVarSST class_us (v+1) `thenSST_`
         returnSST v
      Nothing -> -- first time caller gets to add a unique supply
                 -- to the finite map for that class.
        newMutVarSST 1 `thenSST` \ class_us ->
	let 
	  mapInst' = addToFM mapInst cname class_us
	in
	writeMutVarSST names_var (us, mapInst', cache)	`thenSST_` 
        returnSST 0

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
			 Optional   -> (comp_occs, (name,loc):opt_occs)
			 Compulsory -> ((name,loc):comp_occs, opt_occs)
    in
    writeMutVarSST occs_var new_occ_pair	`thenSST_`
    returnSST name
  where
    necessity = modeToNecessity mode


addOccurrenceNames :: [Name] -> RnMS s ()
addOccurrenceNames names (RnDown loc names_var errs_var occs_var)
		         (SDown rn_env local_env mod_name mode)
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

popOccurrenceName :: RnSMode -> RnM s d (Maybe Occurrence)
popOccurrenceName mode (RnDown loc names_var errs_var occs_var) l_down
  = readMutVarSST occs_var			`thenSST` \ occs ->
    case (mode, occs) of
		-- Find a compulsory occurrence
	(InterfaceMode Compulsory _, (comp:comps, opts))
		-> writeMutVarSST occs_var (comps, opts)	`thenSST_`
		   returnSST (Just comp)

		-- Find an optional occurrence
		-- We shouldn't be looking unless we've done all the compulsories
	(InterfaceMode Optional _, (comps, opt:opts))
		-> ASSERT( null comps )
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
    returnSST (map fst occs)
\end{code}


%************************************************************************
%*									*
\subsection{Plumbing for rename-source part}
%*									*
%************************************************************************

================  RnEnv  =====================

\begin{code}
-- Look in global env only
lookupGlobalNameRn :: RdrName -> RnMS s (Maybe Name)
lookupGlobalNameRn rdr_name rn_down (SDown (RnEnv global_env fixity_env) local_env mod_name mode)
  = case lookupFM global_env rdr_name of
	  Just (name, _) -> returnSST (Just name)
	  Nothing 	 -> returnSST Nothing
  
-- Look in both local and global env
lookupNameRn :: RdrName -> RnMS s (Maybe Name)
lookupNameRn rdr_name rn_down (SDown (RnEnv global_env fixity_env) local_env mod_name mode)
  = case lookupFM local_env rdr_name of
	  Just name -> returnSST (Just name)
	  Nothing   -> case lookupFM global_env rdr_name of
			  Just (name, _) -> returnSST (Just name)
			  Nothing        -> returnSST Nothing

getNameEnvs :: RnMS s (GlobalNameEnv, NameEnv)
getNameEnvs rn_down (SDown (RnEnv global_env fixity_env) local_env mod_name mode)
  = returnSST (global_env, local_env)

getLocalNameEnv :: RnMS s NameEnv
getLocalNameEnv rn_down (SDown rn_env local_env mod_name mode)
  = returnSST local_env

setLocalNameEnv :: NameEnv -> RnMS s a -> RnMS s a
setLocalNameEnv local_env' m rn_down (SDown rn_env local_env mod_name mode)
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

{-
getSearchPathRn :: RnMG SearchPath
getSearchPathRn rn_down (GDown dirs iface_var)
  = returnSST dirs
-}

getModuleHiMap :: RnMG ModuleHiMap
getModuleHiMap rn_down (GDown himap iface_var)
  = returnSST himap

\end{code}

%************************************************************************
%*									*
\subsection{HowInScope}
%*									*
%************************************************************************

\begin{code}
instance Outputable HowInScope where
  ppr (FromLocalDefn loc)      = ptext SLIT("Defined at") <+> ppr loc
  ppr (FromImportDecl mod loc) = ptext SLIT("Imported from") <+> quotes (pprModule mod) <+>
		  	         ptext SLIT("at") <+> ppr loc
\end{code}


\begin{code}
modeToNecessity SourceMode		    = Compulsory
modeToNecessity (InterfaceMode necessity _) = necessity
\end{code}
