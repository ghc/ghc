%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[RnEnv]{Environment manipulation for the renamer monad}

\begin{code}
module RnEnv where		-- Export everything

#include "HsVersions.h"

import CmdLineOpts	( opt_WarnNameShadowing, opt_WarnUnusedMatches,
			  opt_WarnUnusedBinds, opt_WarnUnusedImports )
import HsSyn
import RdrHsSyn		( RdrNameIE )
import RdrName		( RdrName, rdrNameModule, rdrNameOcc, isQual, isUnqual,
			  mkRdrUnqual, qualifyRdrName
			)
import HsTypes		( hsTyVarName, hsTyVarNames, replaceTyVarName )
import HscTypes		( pprNameProvenance )
import RnMonad
import Name		( Name, Provenance(..), ExportFlag(..), NamedThing(..),
			  ImportReason(..), getSrcLoc, 
			  mkLocalName, mkImportedLocalName, mkGlobalName,
			  mkIPName, hasBetterProv, isLocallyDefined, 
			  nameOccName, setNameModule, nameModule,
			  extendNameEnv_C, plusNameEnv_C, nameEnvElts
			)
import NameSet
import OccName		( OccName, occNameUserString, occNameFlavour )
import Module		( ModuleName, moduleName, mkVanillaModule )
import FiniteMap
import Unique		( Unique )
import UniqSupply
import SrcLoc		( SrcLoc )
import Outputable
import ListSetOps	( removeDups, equivClasses )
import Util		( thenCmp, sortLt )
import List		( nub )
import PrelNames	( mkUnboundName )
\end{code}



%*********************************************************
%*							*
\subsection{Making new names}
%*							*
%*********************************************************

\begin{code}
implicitImportProvenance = NonLocalDef ImplicitImport False

newTopBinder :: Module -> RdrName -> SrcLoc -> RnM d Name
newTopBinder mod rdr_name loc
  = 	-- First check the cache
    traceRn (text "newTopBinder" <+> ppr mod <+> ppr occ) `thenRn_`

    getNameSupplyRn		`thenRn` \ (us, cache, ipcache) ->
    let 
	occ = rdrNameOcc rdr_name
	key = (moduleName mod, occ)
    in
    case lookupFM cache key of

	-- A hit in the cache!  We are at the binding site of the name, and
	-- this is the moment when we know all about 
	--	a) the Name's host Module (in particular, which
	-- 	   package it comes from)
	--	b) its defining SrcLoc
	-- So we update this info

	Just name -> let 
			new_name  = setNameModuleAndLoc name mod loc
			new_cache = addToFM cache key new_name
		     in
		     setNameSupplyRn (us, new_cache, ipcache)	`thenRn_`
		     traceRn (text "newTopBinder: overwrite" <+> ppr new_name) `thenRn_`
		     returnRn new_name
		     
	-- Miss in the cache!
	-- Build a completely new Name, and put it in the cache
	-- Even for locally-defined names we use implicitImportProvenance; 
	-- updateProvenances will set it to rights
	Nothing -> let
			(us', us1) = splitUniqSupply us
			uniq   	   = uniqFromSupply us1
			new_name   = mkGlobalName uniq mod occ loc
			new_cache  = addToFM cache key new_name
		   in
		   setNameSupplyRn (us', new_cache, ipcache)	`thenRn_`
		   traceRn (text "newTopBinder: new" <+> ppr new_name) `thenRn_`
		   returnRn new_name


newGlobalName :: ModuleName -> OccName -> RnM d Name
  -- Used for *occurrences*.  We make a place-holder Name, really just
  -- to agree on its unique, which gets overwritten when we read in
  -- the binding occurence later (newTopBinder)
  -- The place-holder Name doesn't have the right SrcLoc, and its
  -- Module won't have the right Package either.
  --
  -- (We have to pass a ModuleName, not a Module, because we may be
  -- simply looking at an occurrence M.x in an interface file.)
  --
  -- This means that a renamed program may have incorrect info
  -- on implicitly-imported occurrences, but the correct info on the 
  -- *binding* declaration. It's the type checker that propagates the 
  -- correct information to all the occurrences.
  -- Since implicitly-imported names never occur in error messages,
  -- it doesn't matter that we get the correct info in place till later,
  -- (but since it affects DLL-ery it does matter that we get it right
  --  in the end).
newGlobalName mod_name occ
  = getNameSupplyRn		`thenRn` \ (us, cache, ipcache) ->
    let
	key = (mod_name, occ)
    in
    case lookupFM cache key of
	Just name -> traceRn (text "newGlobalName: hit" <+> ppr name) `thenRn_`
		     returnRn name

	Nothing   -> setNameSupplyRn (us', new_cache, ipcache)		`thenRn_`
		     traceRn (text "newGlobalName: new" <+> ppr name)	`thenRn_`
		     returnRn name
		  where
		     (us', us1) = splitUniqSupply us
		     uniq   	= uniqFromSupply us1
		     mod        = mkVanillaModule mod_name
		     name       = mkGlobalName uniq mod occ noSrcLoc
		     new_cache  = addToFM cache key name

newIPName rdr_name
  = getNameSupplyRn		`thenRn` \ (us, cache, ipcache) ->
    case lookupFM ipcache key of
	Just name -> returnRn name
	Nothing   -> setNameSupplyRn (us', cache, new_ipcache)	`thenRn_`
		     returnRn name
		  where
		     (us', us1)  = splitUniqSupply us
		     uniq   	 = uniqFromSupply us1
		     name        = mkIPName uniq key
		     new_ipcache = addToFM ipcache key name
    where key = (rdrNameOcc rdr_name)
\end{code}

%*********************************************************
%*							*
\subsection{Looking up names}
%*							*
%*********************************************************

Looking up a name in the RnEnv.

\begin{code}
lookupBndrRn rdr_name
  = getLocalNameEnv		`thenRn` \ local_env ->
    case lookupRdrEnv local_env rdr_name of 
	  Just name -> returnRn name
	  Nothing   -> lookupTopBndrRn rdr_name

lookupTopBndrRn rdr_name
  = getModeRn	`thenRn` \ mode ->
    case mode of 
	InterfaceMode -> 	-- Look in the global name cache
			    lookupOrigName rdr_name	

	SourceMode    -> -- Source mode, so look up a *qualified* version
			 -- of the name, so that we get the right one even
			 -- if there are many with the same occ name
			 -- There must *be* a binding
		getModuleRn		`thenRn` \ mod ->
		getGlobalNameEnv	`thenRn` \ global_env ->
		case lookupRdrEnv global_env (qualifyRdrName (moduleName mod) rdr_name) of
		  Just (name:rest) -> ASSERT( null rest )
				      returnRn name 
		  Nothing	   -> 	-- Almost always this case is a compiler bug.
					-- But consider a type signature that doesn't have 
					-- a corresponding binder: 
					--	module M where { f :: Int->Int }
					-- We use lookupSigOccRn, which uses lookupBndrRn (for good reasons)
					-- and we don't want to panic.  So we report an out-of-scope error
					failWithRn (mkUnboundName rdr_name)
						   (unknownNameErr rdr_name)

-- lookupSigOccRn is used for type signatures and pragmas
-- Is this valid?
--   module A
--	import M( f )
--	f :: Int -> Int
--	f x = x
-- It's clear that the 'f' in the signature must refer to A.f
-- The Haskell98 report does not stipulate this, but it will!
-- So we must treat the 'f' in the signature in the same way
-- as the binding occurrence of 'f', using lookupBndrRn
lookupSigOccRn :: RdrName -> RnMS Name
lookupSigOccRn = lookupBndrRn

-- lookupOccRn looks up an occurrence of a RdrName
lookupOccRn :: RdrName -> RnMS Name
lookupOccRn rdr_name
  = getLocalNameEnv			`thenRn` \ local_env ->
    case lookupRdrEnv local_env rdr_name of
	  Just name -> returnRn name
	  Nothing   -> lookupGlobalOccRn rdr_name

-- lookupGlobalOccRn is like lookupOccRn, except that it looks in the global 
-- environment.  It's used only for
--	record field names
--	class op names in class and instance decls
lookupGlobalOccRn rdr_name
  = getModeRn	`thenRn` \ mode ->
    case mode of {
		-- When processing interface files, the global env 
		-- is always empty, so go straight to the name cache
	InterfaceMode -> lookupOrigName rdr_name ;

	SourceMode ->

    getGlobalNameEnv	`thenRn` \ global_env ->
    case lookupRdrEnv global_env rdr_name of
	Just [(name,_)]	 -> returnRn name
	Just stuff@(_:_) -> addNameClashErrRn rdr_name stuff	`thenRn_`
			    returnRn name
	Nothing -> 	-- Not found when processing source code; so fail
			failWithRn (mkUnboundName rdr_name)
				   (unknownNameErr rdr_name)
    }
\end{code}
%

@lookupOrigName@ takes an RdrName representing an {\em original}
name, and adds it to the occurrence pool so that it'll be loaded
later.  This is used when language constructs (such as monad
comprehensions, overloaded literals, or deriving clauses) require some
stuff to be loaded that isn't explicitly mentioned in the code.

This doesn't apply in interface mode, where everything is explicit,
but we don't check for this case: it does no harm to record an
``extra'' occurrence and @lookupOrigNames@ isn't used much in
interface mode (it's only the @Nothing@ clause of @rnDerivs@ that
calls it at all I think).

  \fbox{{\em Jan 98: this comment is wrong: @rnHsType@ uses it quite a bit.}}

For List and Tuple types it's important to get the correct
@isLocallyDefined@ flag, which is used in turn when deciding
whether there are any instance decls in this module are ``special''.
The name cache should have the correct provenance, though.

\begin{code}
lookupOrigName :: RdrName -> RnM d Name 
lookupOrigName rdr_name
  | isQual rdr_name
  = newGlobalName (rdrNameModule rdr_name) (rdrNameOcc rdr_name)

  | otherwise
  =	-- An Unqual is allowed; interface files contain 
	-- unqualified names for locally-defined things, such as
	-- constructors of a data type.
    getModuleRn 			`thenRn ` \ mod ->
    newGlobalName (moduleName mod) (rdrNameOcc rdr_name)

lookupOrigNames :: [RdrName] -> RnM d NameSet
lookupOrigNames rdr_names
  = mapRn lookupOrigName rdr_names	`thenRn` \ names ->
    returnRn (mkNameSet names)
\end{code}

lookupSysBinder is used for the "system binders" of a type, class, or instance decl.
It ensures that the module is set correctly in the name cache, and sets the provenance
on the returned name too.  The returned name will end up actually in the type, class,
or instance.

\begin{code}
lookupSysBinder rdr_name
  = ASSERT( isUnqual rdr_name )
    getModuleRn				`thenRn` \ mod ->
    getSrcLocRn				`thenRn` \ loc ->
    newTopBinder mod rdr_name loc
\end{code}



%*********************************************************
%*							*
\subsection{Binding}
%*							*
%*********************************************************

\begin{code}
newLocalsRn :: (Unique -> OccName -> SrcLoc -> Name)
	    -> [(RdrName,SrcLoc)]
	    -> RnMS [Name]
newLocalsRn mk_name rdr_names_w_loc
 =  getNameSupplyRn		`thenRn` \ (us, cache, ipcache) ->
    let
	n	   = length rdr_names_w_loc
	(us', us1) = splitUniqSupply us
	uniqs	   = uniqsFromSupply n us1
	names	   = [ mk_name uniq (rdrNameOcc rdr_name) loc
		     | ((rdr_name,loc), uniq) <- rdr_names_w_loc `zip` uniqs
		     ]
    in
    setNameSupplyRn (us', cache, ipcache)	`thenRn_`
    returnRn names


bindLocatedLocalsRn :: SDoc	-- Documentation string for error message
	   	    -> [(RdrName,SrcLoc)]
	    	    -> ([Name] -> RnMS a)
	    	    -> RnMS a
bindLocatedLocalsRn doc_str rdr_names_w_loc enclosed_scope
  = getModeRn 				`thenRn` \ mode ->
    getLocalNameEnv			`thenRn` \ name_env ->

	-- Check for duplicate names
    checkDupOrQualNames doc_str rdr_names_w_loc	`thenRn_`

    	-- Warn about shadowing, but only in source modules
    (case mode of
	SourceMode | opt_WarnNameShadowing -> mapRn_ (check_shadow name_env) rdr_names_w_loc
	other				   -> returnRn ()
    )					`thenRn_`
	
    let
	mk_name    = case mode of
			SourceMode    -> mkLocalName 
			InterfaceMode -> mkImportedLocalName 
		     -- Keep track of whether the name originally came from 
		     -- an interface file.
    in
    newLocalsRn mk_name rdr_names_w_loc		`thenRn` \ names ->
    let
	new_local_env = addListToRdrEnv name_env (map fst rdr_names_w_loc `zip` names)
    in
    setLocalNameEnv new_local_env (enclosed_scope names)

  where
    check_shadow name_env (rdr_name,loc)
	= case lookupRdrEnv name_env rdr_name of
		Nothing   -> returnRn ()
		Just name -> pushSrcLocRn loc $
			     addWarnRn (shadowedNameWarn rdr_name)

bindCoreLocalFVRn :: RdrName -> (Name -> RnMS (a, FreeVars))
	  	  -> RnMS (a, FreeVars)
  -- A specialised variant when renaming stuff from interface
  -- files (of which there is a lot)
  --	* one at a time
  --	* no checks for shadowing
  -- 	* always imported
  -- 	* deal with free vars
bindCoreLocalFVRn rdr_name enclosed_scope
  = getSrcLocRn 		`thenRn` \ loc ->
    getLocalNameEnv		`thenRn` \ name_env ->
    getNameSupplyRn		`thenRn` \ (us, cache, ipcache) ->
    let
	(us', us1) = splitUniqSupply us
	uniq	   = uniqFromSupply us1
	name	   = mkImportedLocalName uniq (rdrNameOcc rdr_name) loc
    in
    setNameSupplyRn (us', cache, ipcache)	`thenRn_`
    let
	new_name_env = extendRdrEnv name_env rdr_name name
    in
    setLocalNameEnv new_name_env (enclosed_scope name)	`thenRn` \ (result, fvs) ->
    returnRn (result, delFromNameSet fvs name)

bindCoreLocalsFVRn []     thing_inside = thing_inside []
bindCoreLocalsFVRn (b:bs) thing_inside = bindCoreLocalFVRn b	$ \ name' ->
					 bindCoreLocalsFVRn bs	$ \ names' ->
					 thing_inside (name':names')

bindLocalNames names enclosed_scope
  = getLocalNameEnv 		`thenRn` \ name_env ->
    setLocalNameEnv (addListToRdrEnv name_env pairs)
		    enclosed_scope
  where
    pairs = [(mkRdrUnqual (nameOccName n), n) | n <- names]

-------------------------------------
bindLocalRn doc rdr_name enclosed_scope
  = getSrcLocRn 				`thenRn` \ loc ->
    bindLocatedLocalsRn doc [(rdr_name,loc)]	$ \ (n:ns) ->
    ASSERT( null ns )
    enclosed_scope n

bindLocalsRn doc rdr_names enclosed_scope
  = getSrcLocRn		`thenRn` \ loc ->
    bindLocatedLocalsRn doc
			(rdr_names `zip` repeat loc)
		 	enclosed_scope

	-- binLocalsFVRn is the same as bindLocalsRn
	-- except that it deals with free vars
bindLocalsFVRn doc rdr_names enclosed_scope
  = bindLocalsRn doc rdr_names		$ \ names ->
    enclosed_scope names		`thenRn` \ (thing, fvs) ->
    returnRn (thing, delListFromNameSet fvs names)

-------------------------------------
bindUVarRn :: SDoc -> RdrName -> (Name -> RnMS (a, FreeVars)) -> RnMS (a, FreeVars)
bindUVarRn = bindLocalRn

-------------------------------------
extendTyVarEnvFVRn :: [Name] -> RnMS (a, FreeVars) -> RnMS (a, FreeVars)
	-- This tiresome function is used only in rnDecl on InstDecl
extendTyVarEnvFVRn tyvars enclosed_scope
  = bindLocalNames tyvars enclosed_scope 	`thenRn` \ (thing, fvs) -> 
    returnRn (thing, delListFromNameSet fvs tyvars)

bindTyVarsRn :: SDoc -> [HsTyVarBndr RdrName]
	      -> ([HsTyVarBndr Name] -> RnMS a)
	      -> RnMS a
bindTyVarsRn doc_str tyvar_names enclosed_scope
  = bindTyVars2Rn doc_str tyvar_names 	$ \ names tyvars ->
    enclosed_scope tyvars

-- Gruesome name: return Names as well as HsTyVars
bindTyVars2Rn :: SDoc -> [HsTyVarBndr RdrName]
	      -> ([Name] -> [HsTyVarBndr Name] -> RnMS a)
	      -> RnMS a
bindTyVars2Rn doc_str tyvar_names enclosed_scope
  = getSrcLocRn					`thenRn` \ loc ->
    let
	located_tyvars = [(hsTyVarName tv, loc) | tv <- tyvar_names] 
    in
    bindLocatedLocalsRn doc_str located_tyvars	$ \ names ->
    enclosed_scope names (zipWith replaceTyVarName tyvar_names names)

bindTyVarsFVRn :: SDoc -> [HsTyVarBndr RdrName]
	      -> ([HsTyVarBndr Name] -> RnMS (a, FreeVars))
	      -> RnMS (a, FreeVars)
bindTyVarsFVRn doc_str rdr_names enclosed_scope
  = bindTyVars2Rn doc_str rdr_names	$ \ names tyvars ->
    enclosed_scope tyvars		`thenRn` \ (thing, fvs) ->
    returnRn (thing, delListFromNameSet fvs names)

bindTyVarsFV2Rn :: SDoc -> [HsTyVarBndr RdrName]
	      -> ([Name] -> [HsTyVarBndr Name] -> RnMS (a, FreeVars))
	      -> RnMS (a, FreeVars)
bindTyVarsFV2Rn doc_str rdr_names enclosed_scope
  = bindTyVars2Rn doc_str rdr_names	$ \ names tyvars ->
    enclosed_scope names tyvars		`thenRn` \ (thing, fvs) ->
    returnRn (thing, delListFromNameSet fvs names)

bindNakedTyVarsFVRn :: SDoc -> [RdrName]
	            -> ([Name] -> RnMS (a, FreeVars))
		    -> RnMS (a, FreeVars)
bindNakedTyVarsFVRn doc_str tyvar_names enclosed_scope
  = getSrcLocRn					`thenRn` \ loc ->
    let
	located_tyvars = [(tv, loc) | tv <- tyvar_names] 
    in
    bindLocatedLocalsRn doc_str located_tyvars	$ \ names ->
    enclosed_scope names			`thenRn` \ (thing, fvs) ->
    returnRn (thing, delListFromNameSet fvs names)


-------------------------------------
checkDupOrQualNames, checkDupNames :: SDoc
				   -> [(RdrName, SrcLoc)]
				   -> RnM d ()
	-- Works in any variant of the renamer monad

checkDupOrQualNames doc_str rdr_names_w_loc
  =	-- Check for use of qualified names
    mapRn_ (qualNameErr doc_str) quals 	`thenRn_`
    checkDupNames doc_str rdr_names_w_loc
  where
    quals = filter (isQual.fst) rdr_names_w_loc
    
checkDupNames doc_str rdr_names_w_loc
  = 	-- Check for duplicated names in a binding group
    mapRn_ (dupNamesErr doc_str) dups
  where
    (_, dups) = removeDups (\(n1,l1) (n2,l2) -> n1 `compare` n2) rdr_names_w_loc
\end{code}


%************************************************************************
%*									*
\subsection{GlobalRdrEnv}
%*									*
%************************************************************************

\begin{code}
plusGlobalRdrEnv :: GlobalRdrEnv -> GlobalRdrEnv -> GlobalRdrEnv
plusGlobalRdrEnv env1 env2 = plusFM_C combine_globals env1 env2

addOneToGlobalRdrEnv :: GlobalRdrEnv -> RdrName -> (Name,Provenance) -> GlobalRdrEnv
addOneToGlobalRdrEnv env rdr_name name = addToFM_C combine_globals env rdr_name [name]

delOneFromGlobalRdrEnv :: GlobalRdrEnv -> RdrName -> GlobalRdrEnv 
delOneFromGlobalRdrEnv env rdr_name = delFromFM env rdr_name

combine_globals :: [(Name,Provenance)] 	-- Old
		-> [(Name,Provenance)]	-- New
		-> [(Name,Provenance)]
combine_globals ns_old ns_new	-- ns_new is often short
  = foldr add ns_old ns_new
  where
    add n ns | any (is_duplicate n) ns_old = map (choose n) ns	-- Eliminate duplicates
	     | otherwise	           = n:ns

    choose n m | n `beats` m = n
	       | otherwise   = m

    (n,pn) `beats` (m,pm) = n==m && pn `hasBetterProv` pm

    is_duplicate :: Provenance -> (Name,Provenance) -> Bool
    is_duplicate (n1,LocalDef _) (n2,LocalDef _) = False
    is_duplicate _		 _		 = n1 == n2
\end{code}

We treat two bindings of a locally-defined name as a duplicate,
because they might be two separate, local defns and we want to report
and error for that, {\em not} eliminate a duplicate.

On the other hand, if you import the same name from two different
import statements, we {\em do} want to eliminate the duplicate, not report
an error.

If a module imports itself then there might be a local defn and an imported
defn of the same name; in this case the names will compare as equal, but
will still have different provenances.


@unQualInScope@ returns a function that takes a @Name@ and tells whether
its unqualified name is in scope.  This is put as a boolean flag in
the @Name@'s provenance to guide whether or not to print the name qualified
in error messages.

\begin{code}
unQualInScope :: GlobalRdrEnv -> Name -> Bool
unQualInScope env
  = lookup
  where
    lookup name = case lookupRdrEnv env (mkRdrUnqual (nameOccName name)) of
			   Just [(name',_)] -> name == name'
			   other            -> False
\end{code}


%************************************************************************
%*									*
\subsection{Avails}
%*									*
%************************************************************************

\begin{code}
plusAvail (Avail n1)	   (Avail n2)	    = Avail n1
plusAvail (AvailTC n1 ns1) (AvailTC n2 ns2) = AvailTC n2 (nub (ns1 ++ ns2))
-- Added SOF 4/97
#ifdef DEBUG
plusAvail a1 a2 = pprPanic "RnEnv.plusAvail" (hsep [pprAvail a1,pprAvail a2])
#endif

addAvail :: AvailEnv -> AvailInfo -> AvailEnv
addAvail avails avail = extendNameEnv_C plusAvail avails (availName avail) avail

emptyAvailEnv = emptyNameEnv
unitAvailEnv :: AvailInfo -> AvailEnv
unitAvailEnv a = unitNameEnv (availName a) a

plusAvailEnv :: AvailEnv -> AvailEnv -> AvailEnv
plusAvailEnv = plusNameEnv_C plusAvail

availEnvElts = nameEnvElts

addAvailToNameSet :: NameSet -> AvailInfo -> NameSet
addAvailToNameSet names avail = addListToNameSet names (availNames avail)

availsToNameSet :: [AvailInfo] -> NameSet
availsToNameSet avails = foldl addAvailToNameSet emptyNameSet avails

availName :: GenAvailInfo name -> name
availName (Avail n)     = n
availName (AvailTC n _) = n

availNames :: GenAvailInfo name -> [name]
availNames (Avail n)      = [n]
availNames (AvailTC n ns) = ns

addSysAvails :: AvailInfo -> [Name] -> AvailInfo
addSysAvails avail          []  = avail
addSysAvails (AvailTC n ns) sys = AvailTC n (sys ++ ns)

rdrAvailInfo :: AvailInfo -> RdrAvailInfo
-- Used when building the avails we are going to put in an interface file
-- We sort the components to reduce needless wobbling of interfaces
rdrAvailInfo (Avail n)	    = Avail   (nameOccName n)
rdrAvailInfo (AvailTC n ns) = AvailTC (nameOccName n) (sortLt (<) (map nameOccName ns))

filterAvail :: RdrNameIE	-- Wanted
	    -> AvailInfo	-- Available
	    -> Maybe AvailInfo	-- Resulting available; 
				-- Nothing if (any of the) wanted stuff isn't there

filterAvail ie@(IEThingWith want wants) avail@(AvailTC n ns)
  | sub_names_ok = Just (AvailTC n (filter is_wanted ns))
  | otherwise    = Nothing
  where
    is_wanted name = nameOccName name `elem` wanted_occs
    sub_names_ok   = all (`elem` avail_occs) wanted_occs
    avail_occs	   = map nameOccName ns
    wanted_occs    = map rdrNameOcc (want:wants)

filterAvail (IEThingAbs _) (AvailTC n ns)       = ASSERT( n `elem` ns ) 
						  Just (AvailTC n [n])

filterAvail (IEThingAbs _) avail@(Avail n)      = Just avail		-- Type synonyms

filterAvail (IEVar _)      avail@(Avail n)      = Just avail
filterAvail (IEVar v)      avail@(AvailTC n ns) = Just (AvailTC n (filter wanted ns))
						where
						  wanted n = nameOccName n == occ
						  occ      = rdrNameOcc v
	-- The second equation happens if we import a class op, thus
	-- 	import A( op ) 
	-- where op is a class operation

filterAvail (IEThingAll _) avail@(AvailTC _ _)   = Just avail
	-- We don't complain even if the IE says T(..), but
	-- no constrs/class ops of T are available
	-- Instead that's caught with a warning by the caller

filterAvail ie avail = Nothing

pprAvail :: AvailInfo -> SDoc
pprAvail (AvailTC n ns) = ppr n <> case filter (/= n) ns of
					[]  -> empty
					ns' -> parens (hsep (punctuate comma (map ppr ns')))

pprAvail (Avail n) = ppr n
\end{code}


%************************************************************************
%*									*
\subsection{Free variable manipulation}
%*									*
%************************************************************************

\begin{code}
type FreeVars	= NameSet

plusFV   :: FreeVars -> FreeVars -> FreeVars
addOneFV :: FreeVars -> Name -> FreeVars
unitFV   :: Name -> FreeVars
emptyFVs :: FreeVars
plusFVs  :: [FreeVars] -> FreeVars
mkFVs	 :: [Name] -> FreeVars

isEmptyFVs = isEmptyNameSet
emptyFVs   = emptyNameSet
plusFVs    = unionManyNameSets
plusFV     = unionNameSets
mkFVs	   = mkNameSet

-- No point in adding implicitly imported names to the free-var set
addOneFV s n = addOneToNameSet s n
unitFV     n = unitNameSet n

-- A useful utility
mapFvRn f xs = mapRn f xs	`thenRn` \ stuff ->
	       let
		  (ys, fvs_s) = unzip stuff
	       in
	       returnRn (ys, plusFVs fvs_s)
\end{code}


%************************************************************************
%*									*
\subsection{Envt utility functions}
%*									*
%************************************************************************

\begin{code}
warnUnusedModules :: [Module] -> RnM d ()
warnUnusedModules mods
  | not opt_WarnUnusedImports = returnRn ()
  | otherwise 		      = mapRn_ (addWarnRn . unused_mod . moduleName) mods
  where
    unused_mod m = vcat [ptext SLIT("Module") <+> quotes (ppr m) <+> 
			   text "is imported, but nothing from it is used",
			 parens (ptext SLIT("except perhaps to re-export instances visible in") <+>
				   quotes (pprModuleName m))]

warnUnusedImports :: [(Name,Provenance)] -> RnM d ()
warnUnusedImports names
  | not opt_WarnUnusedImports
  = returnRn () 	-- Don't force names unless necessary
  | otherwise
  = warnUnusedBinds names

warnUnusedLocalBinds, warnUnusedMatches :: [Name] -> RnM d ()
warnUnusedLocalBinds ns
  | not opt_WarnUnusedBinds = returnRn ()
  | otherwise		    = warnUnusedBinds [(n,LocalDef) | n<-ns]

warnUnusedMatches names
  | opt_WarnUnusedMatches = warnUnusedGroup [(n,LocalDef) | n<-ns]
  | otherwise 		  = returnRn ()

-------------------------

warnUnusedBinds :: [(Name,Provenance)] -> RnM d ()
warnUnusedBinds names
  = mapRn_ warnUnusedGroup  groups
  where
	-- Group by provenance
   groups = equivClasses cmp names
   (_,prov1) `cmp` (_,prov2) = prov1 `cmp_prov` prov2
 
   cmp_prov (LocalDef _ _) (NonLocalDef _ _)       = LT
   cmp_prov (LocalDef loc1 _) (LocalDef loc2 _)    = loc1 `compare` loc2
   cmp_prov (NonLocalDef (UserImport m1 loc1 _) _)
            (NonLocalDef (UserImport m2 loc2 _) _) =
	 (m1 `compare` m2) `thenCmp` (loc1 `compare` loc2)
   cmp_prov (NonLocalDef _ _) (LocalDef _ _)       = GT
			-- In-scope NonLocalDefs must have UserImport info on them

-------------------------

warnUnusedGroup :: [(Name,Provenance)] -> RnM d ()
warnUnusedGroup names
  | null filtered_names  = returnRn ()
  | not is_local	 = returnRn ()
  | otherwise
  = pushSrcLocRn def_loc	$
    addWarnRn			$
    sep [msg <> colon, nest 4 (fsep (punctuate comma (map ppr filtered_names)))]
  where
    filtered_names = filter reportable names
    (name1, prov1) = head filtered_names
    (is_local, def_loc, msg)
	= case prov1 of
		LocalDef loc _  -> (True, loc, text "Defined but not used")

		NonLocalDef (UserImport mod loc _) _ 
			-> (True, loc, text "Imported from" <+> quotes (ppr mod) <+> text "but not used")

    reportable (name,_) = case occNameUserString (nameOccName name) of
				('_' : _) -> False
				zz_other  -> True
	-- Haskell 98 encourages compilers to suppress warnings about
	-- unused names in a pattern if they start with "_".
\end{code}

\begin{code}
addNameClashErrRn rdr_name (np1:nps)
  = addErrRn (vcat [ptext SLIT("Ambiguous occurrence") <+> quotes (ppr rdr_name),
		    ptext SLIT("It could refer to") <+> vcat (msg1 : msgs)])
  where
    msg1 = ptext  SLIT("either") <+> mk_ref np1
    msgs = [ptext SLIT("    or") <+> mk_ref np | np <- nps]
    mk_ref (name,prov) = quotes (ppr name) <> comma <+> pprNameProvenance name prov

fixityClashErr (rdr_name, ((_,how_in_scope1), (_, how_in_scope2)))
  = hang (hsep [ptext SLIT("Conflicting fixities for"), quotes (ppr rdr_name)])
	4 (vcat [ppr how_in_scope1,
		 ppr how_in_scope2])

shadowedNameWarn shadow
  = hsep [ptext SLIT("This binding for"), 
	       quotes (ppr shadow),
	       ptext SLIT("shadows an existing binding")]

unknownNameErr name
  = sep [text flavour, ptext SLIT("not in scope:"), quotes (ppr name)]
  where
    flavour = occNameFlavour (rdrNameOcc name)

qualNameErr descriptor (name,loc)
  = pushSrcLocRn loc $
    addErrRn (hsep [ ptext SLIT("Invalid use of qualified name"), 
		     quotes (ppr name),
		     ptext SLIT("in"),
		     descriptor])

dupNamesErr descriptor ((name,loc) : dup_things)
  = pushSrcLocRn loc $
    addErrRn ((ptext SLIT("Conflicting definitions for") <+> quotes (ppr name))
	      $$ 
	      (ptext SLIT("in") <+> descriptor))
\end{code}
