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
import RdrName		( RdrName, rdrNameModule, rdrNameOcc, isQual, mkRdrUnqual, qualifyRdrName )
import HsTypes		( getTyVarName, replaceTyVarName )
import BasicTypes	( Fixity(..), FixityDirection(..) )
import RnMonad
import Name		( Name, Provenance(..), ExportFlag(..), NamedThing(..),
			  ImportReason(..), getSrcLoc, 
			  mkLocalName, mkGlobalName, isSystemName,
			  nameOccName, nameModule, setNameModule,
			  pprOccName, isLocallyDefined, nameUnique, nameOccName,
			  setNameProvenance, getNameProvenance, pprNameProvenance
			)
import NameSet
import OccName		( OccName,
			  mkDFunOcc, 
			  occNameFlavour, moduleIfaceFlavour
			)
import TyCon		( TyCon )
import FiniteMap
import Unique		( Unique, Uniquable(..), unboundKey )
import UniqFM           ( emptyUFM, listToUFM, plusUFM_C )
import UniqSupply
import SrcLoc		( SrcLoc, noSrcLoc )
import Outputable
import Util		( removeDups, equivClasses, thenCmp )
import List		( nub )
import Maybes		( mapMaybe )
\end{code}



%*********************************************************
%*							*
\subsection{Making new names}
%*							*
%*********************************************************

\begin{code}
newImportedGlobalName :: Module -> OccName
	      	      -> RnM s d Name
newImportedGlobalName mod occ
  = 	-- First check the cache
    getNameSupplyRn		`thenRn` \ (us, inst_ns, cache) ->
    let 
	key     = (mod,occ)
	mod_hif = moduleIfaceFlavour mod
    in
    case lookupFM cache key of
	
	-- A hit in the cache!
	-- Make sure that the module in the name has the same IfaceFlavour as
	-- the module we are looking for; if not, make it so
	-- so that it has the right HiFlag component.
	-- (This is necessary for known-key things.  
	-- 	For example, GHCmain.lhs imports as SOURCE
	-- 	Main; but Main.main is a known-key thing.)  
	Just name | isSystemName name	-- A known-key name; fix the provenance and module
		  -> getOmitQualFn			`thenRn` \ omit_fn ->
		     let
			  new_name = setNameProvenance (setNameModule name mod)
						       (NonLocalDef ImplicitImport (omit_fn name))
			  new_cache = addToFM cache key new_name
		     in
		     setNameSupplyRn (us, inst_ns, new_cache)	`thenRn_`
		     returnRn new_name

		  | otherwise
		  -> returnRn name
		     
	Nothing -> 	-- Miss in the cache!
			-- Build a new original name, and put it in the cache
		   getOmitQualFn			`thenRn` \ omit_fn ->
		   let
			(us', us1) = splitUniqSupply us
			uniq   	   = uniqFromSupply us1
			name       = mkGlobalName uniq mod occ (NonLocalDef ImplicitImport (omit_fn name))
					-- For in-scope things we improve the provenance
					-- in RnNames.importsFromImportDecl
			new_cache  = addToFM cache key name
		   in
		   setNameSupplyRn (us', inst_ns, new_cache)		`thenRn_`
		   returnRn name


newImportedGlobalFromRdrName rdr_name
  | isQual rdr_name
  = newImportedGlobalName (rdrNameModule rdr_name) (rdrNameOcc rdr_name)

  | otherwise
  =	-- An Unqual is allowed; interface files contain 
	-- unqualified names for locally-defined things, such as
	-- constructors of a data type.
    getModuleRn 	`thenRn ` \ mod_name ->
    newImportedGlobalName mod_name (rdrNameOcc rdr_name)


newLocallyDefinedGlobalName :: Module -> OccName 
			    -> (Name -> ExportFlag) -> SrcLoc
			    -> RnM s d Name
newLocallyDefinedGlobalName mod occ rec_exp_fn loc
  = 	-- First check the cache
    getNameSupplyRn		`thenRn` \ (us, inst_ns, cache) ->
    let 
	key          = (mod,occ)
	mk_prov name = LocalDef loc (rec_exp_fn name)
	-- We must set the provenance of the thing in the cache
	-- correctly, particularly whether or not it is locally defined.
	--
	-- Since newLocallyDefinedGlobalName is used only
	-- at binding occurrences, we may as well get the provenance
	-- dead right first time; hence the rec_exp_fn passed in
    in
    case lookupFM cache key of

	-- A hit in the cache!
	-- Overwrite whatever provenance is in the cache already; 
	-- this updates WiredIn things and known-key things, 
	-- which are there from the start, to LocalDef.
	--
	-- It also means that if there are two defns for the same thing
	-- in a module, then each gets a separate SrcLoc
	Just name -> let 
			new_name = setNameProvenance name (mk_prov new_name)
			new_cache = addToFM cache key new_name
		     in
		     setNameSupplyRn (us, inst_ns, new_cache)		`thenRn_`
		     returnRn new_name
		     
	-- Miss in the cache!
	-- Build a new original name, and put it in the cache
	Nothing -> let
			(us', us1) = splitUniqSupply us
			uniq   	   = uniqFromSupply us1
			new_name   = mkGlobalName uniq mod occ (mk_prov new_name)
			new_cache  = addToFM cache key new_name
		   in
		   setNameSupplyRn (us', inst_ns, new_cache)		`thenRn_`
		   returnRn new_name


newLocalNames :: [(RdrName,SrcLoc)] -> RnM s d [Name]
newLocalNames rdr_names
  = getNameSupplyRn		`thenRn` \ (us, inst_ns, cache) ->
    let
	n	   = length rdr_names
	(us', us1) = splitUniqSupply us
	uniqs	   = uniqsFromSupply n us1
	locals	   = [ mkLocalName uniq (rdrNameOcc rdr_name) loc
		     | ((rdr_name,loc), uniq) <- rdr_names `zip` uniqs
		     ]
    in
    setNameSupplyRn (us', inst_ns, cache)	`thenRn_`
    returnRn locals

newDFunName cl_occ tycon_occ (Just n) src_loc		-- Imported ones have "Just n"
  = newImportedGlobalFromRdrName n

newDFunName cl_occ tycon_occ Nothing src_loc		-- Local instance decls have a "Nothing"
  = getModuleRn				`thenRn` \ mod_name ->
    newInstUniq (cl_occ, tycon_occ)	`thenRn` \ inst_uniq ->
    let
     	dfun_occ = mkDFunOcc cl_occ tycon_occ inst_uniq
    in
    newLocallyDefinedGlobalName mod_name dfun_occ (\_ -> Exported) src_loc


-- mkUnboundName makes a place-holder Name; it shouldn't be looked at except possibly
-- during compiler debugging.
mkUnboundName :: RdrName -> Name
mkUnboundName rdr_name = mkLocalName unboundKey (rdrNameOcc rdr_name) noSrcLoc

isUnboundName :: Name -> Bool
isUnboundName name = getUnique name == unboundKey
\end{code}

\begin{code}
-------------------------------------
bindLocatedLocalsRn :: SDoc			-- Documentation string for error message
	   	    -> [(RdrName,SrcLoc)]
	    	    -> ([Name] -> RnMS s a)
	    	    -> RnMS s a
bindLocatedLocalsRn doc_str rdr_names_w_loc enclosed_scope
  = checkDupOrQualNames doc_str rdr_names_w_loc	`thenRn_`

    getLocalNameEnv			`thenRn` \ name_env ->
    (if opt_WarnNameShadowing
     then
	mapRn (check_shadow name_env) rdr_names_w_loc
     else
	returnRn []
    )					`thenRn_`
	
    newLocalNames rdr_names_w_loc	`thenRn` \ names ->
    let
	new_name_env = addListToRdrEnv name_env (map fst rdr_names_w_loc `zip` names)
    in
    setLocalNameEnv new_name_env (enclosed_scope names)
  where
    check_shadow name_env (rdr_name,loc)
	= case lookupRdrEnv name_env rdr_name of
		Nothing   -> returnRn ()
		Just name -> pushSrcLocRn loc $
			     addWarnRn (shadowedNameWarn rdr_name)


-------------------------------------
bindLocalsRn doc_str rdr_names enclosed_scope
  = getSrcLocRn		`thenRn` \ loc ->
    bindLocatedLocalsRn (text doc_str)
			(rdr_names `zip` repeat loc)
		 	enclosed_scope

	-- binLocalsFVRn is the same as bindLocalsRn
	-- except that it deals with free vars
bindLocalsFVRn doc_str rdr_names enclosed_scope
  = bindLocalsRn doc_str rdr_names	$ \ names ->
    enclosed_scope names		`thenRn` \ (thing, fvs) ->
    returnRn (thing, delListFromNameSet fvs names)

-------------------------------------
extendTyVarEnvFVRn :: [HsTyVar Name] -> RnMS s (a, FreeVars) -> RnMS s (a, FreeVars)
	-- This tiresome function is used only in rnDecl on InstDecl
extendTyVarEnvFVRn tyvars enclosed_scope
  = getLocalNameEnv		`thenRn` \ env ->
    let
	tyvar_names = map getTyVarName tyvars
	new_env = addListToRdrEnv env [ (mkRdrUnqual (getOccName name), name) 
				      | name <- tyvar_names
				      ]
    in
    setLocalNameEnv new_env enclosed_scope	`thenRn` \ (thing, fvs) -> 
    returnRn (thing, delListFromNameSet fvs tyvar_names)

bindTyVarsRn :: SDoc -> [HsTyVar RdrName]
	      -> ([HsTyVar Name] -> RnMS s a)
	      -> RnMS s a
bindTyVarsRn doc_str tyvar_names enclosed_scope
  = bindTyVars2Rn doc_str tyvar_names 	$ \ names tyvars ->
    enclosed_scope tyvars

-- Gruesome name: return Names as well as HsTyVars
bindTyVars2Rn :: SDoc -> [HsTyVar RdrName]
	      -> ([Name] -> [HsTyVar Name] -> RnMS s a)
	      -> RnMS s a
bindTyVars2Rn doc_str tyvar_names enclosed_scope
  = getSrcLocRn					`thenRn` \ loc ->
    let
	located_tyvars = [(getTyVarName tv, loc) | tv <- tyvar_names] 
    in
    bindLocatedLocalsRn doc_str located_tyvars	$ \ names ->
    enclosed_scope names (zipWith replaceTyVarName tyvar_names names)

bindTyVarsFVRn :: SDoc -> [HsTyVar RdrName]
	      -> ([HsTyVar Name] -> RnMS s (a, FreeVars))
	      -> RnMS s (a, FreeVars)
bindTyVarsFVRn doc_str rdr_names enclosed_scope
  = bindTyVars2Rn doc_str rdr_names	$ \ names tyvars ->
    enclosed_scope tyvars		`thenRn` \ (thing, fvs) ->
    returnRn (thing, delListFromNameSet fvs names)

bindTyVarsFV2Rn :: SDoc -> [HsTyVar RdrName]
	      -> ([Name] -> [HsTyVar Name] -> RnMS s (a, FreeVars))
	      -> RnMS s (a, FreeVars)
bindTyVarsFV2Rn doc_str rdr_names enclosed_scope
  = bindTyVars2Rn doc_str rdr_names	$ \ names tyvars ->
    enclosed_scope names tyvars		`thenRn` \ (thing, fvs) ->
    returnRn (thing, delListFromNameSet fvs names)


-------------------------------------
checkDupOrQualNames, checkDupNames :: SDoc
				   -> [(RdrName, SrcLoc)]
				   -> RnM s d ()
	-- Works in any variant of the renamer monad

checkDupOrQualNames doc_str rdr_names_w_loc
  =	-- Check for use of qualified names
    mapRn (qualNameErr doc_str) quals 	`thenRn_`
    checkDupNames doc_str rdr_names_w_loc
  where
    quals = filter (isQual.fst) rdr_names_w_loc
    
checkDupNames doc_str rdr_names_w_loc
  = 	-- Check for dupicated names in a binding group
    mapRn (dupNamesErr doc_str) dups	`thenRn_`
    returnRn ()
  where
    (_, dups) = removeDups (\(n1,l1) (n2,l2) -> n1 `compare` n2) rdr_names_w_loc
\end{code}


%*********************************************************
%*							*
\subsection{Looking up names}
%*							*
%*********************************************************

Looking up a name in the RnEnv.

\begin{code}
lookupBndrRn rdr_name
  = getNameEnvs		`thenRn` \ (global_env, local_env) ->

	-- Try local env
    case lookupRdrEnv local_env rdr_name of {
	  Just name -> returnRn name ;
	  Nothing   ->

    getModeRn	`thenRn` \ mode ->
    case mode of 
	InterfaceMode _ -> 	-- Look in the global name cache
			    newImportedGlobalFromRdrName rdr_name

	SourceMode      -> 	-- Source mode, so look up a *qualified* version
				-- of the name, so that we get the right one even
				-- if there are many with the same occ name
				-- There must *be* a binding
			    getModuleRn		`thenRn` \ mod ->
			    case lookupRdrEnv global_env (qualifyRdrName mod rdr_name) of
				Just (name:rest) -> ASSERT( null rest )
						    returnRn name 
				Nothing		 -> pprPanic "lookupBndrRn" (ppr mod <+> ppr rdr_name)
    }

-- Just like lookupRn except that we record the occurrence too
-- Perhaps surprisingly, even wired-in names are recorded.
-- Why?  So that we know which wired-in names are referred to when
-- deciding which instance declarations to import.
lookupOccRn :: RdrName -> RnMS s Name
lookupOccRn rdr_name
  = getNameEnvs					`thenRn` \ (global_env, local_env) ->
    lookup_occ global_env local_env rdr_name	`thenRn` \ name ->
    addOccurrenceName name

-- lookupGlobalOccRn is like lookupOccRn, except that it looks in the global 
-- environment.  It's used only for
--	record field names
--	class op names in class and instance decls
lookupGlobalOccRn :: RdrName -> RnMS s Name
lookupGlobalOccRn rdr_name
  = getNameEnvs					`thenRn` \ (global_env, local_env) ->
    lookup_global_occ global_env rdr_name	`thenRn` \ name ->
    addOccurrenceName name

-- Look in both local and global env
lookup_occ global_env local_env rdr_name
  = case lookupRdrEnv local_env rdr_name of
	  Just name -> returnRn name
	  Nothing   -> lookup_global_occ global_env rdr_name

-- Look in global env only
lookup_global_occ global_env rdr_name
  = case lookupRdrEnv global_env rdr_name of
	Just [name]	    -> returnRn name
	Just stuff@(name:_) -> addNameClashErrRn rdr_name stuff	`thenRn_`
			       returnRn name
	Nothing -> getModeRn	`thenRn` \ mode ->
		   case mode of 
			-- Not found when processing source code; so fail
			SourceMode    -> failWithRn (mkUnboundName rdr_name)
					            (unknownNameErr rdr_name)
		
			-- Not found when processing an imported declaration,
			-- so we create a new name for the purpose
			InterfaceMode _ -> newImportedGlobalFromRdrName rdr_name

  
-- lookupImplicitOccRn takes an RdrName representing an *original* name, and
-- adds it to the occurrence pool so that it'll be loaded later.  This is
-- used when language constructs (such as monad comprehensions, overloaded literals,
-- or deriving clauses) require some stuff to be loaded that isn't explicitly
-- mentioned in the code.
--
-- This doesn't apply in interface mode, where everything is explicit, but
-- we don't check for this case: it does no harm to record an "extra" occurrence
-- and lookupImplicitOccRn isn't used much in interface mode (it's only the
-- Nothing clause of rnDerivs that calls it at all I think).
--	[Jan 98: this comment is wrong: rnHsType uses it quite a bit.]
--
-- For List and Tuple types it's important to get the correct
-- isLocallyDefined flag, which is used in turn when deciding
-- whether there are any instance decls in this module are "special".
-- The name cache should have the correct provenance, though.

lookupImplicitOccRn :: RdrName -> RnMS s Name 
lookupImplicitOccRn rdr_name
 = newImportedGlobalFromRdrName rdr_name	`thenRn` \ name ->
   addOccurrenceName name

addImplicitOccRn :: Name -> RnMS s Name
addImplicitOccRn name = addOccurrenceName name

addImplicitOccsRn :: [Name] -> RnMS s ()
addImplicitOccsRn names = addOccurrenceNames names
\end{code}

\begin{code}
lookupFixity :: Name -> RnMS s Fixity
lookupFixity name
  = getFixityEnv	`thenRn` \ fixity_env ->
    case lookupNameEnv fixity_env name of
	Just (FixitySig _ fixity _) -> returnRn fixity
	Nothing	        	    -> returnRn (Fixity 9 InfixL)	-- Default case
\end{code}

unQualInScope returns a function that takes a Name and tells whether
its unqualified name is in scope.  This is put as a boolean flag in
the Name's provenance to guide whether or not to print the name qualified
in error messages.

\begin{code}
unQualInScope :: GlobalRdrEnv -> Name -> Bool
unQualInScope env
  = lookup
  where
    lookup name = case lookupRdrEnv env (mkRdrUnqual (nameOccName name)) of
			   Just [name'] -> name == name'
			   other        -> False
\end{code}

%************************************************************************
%*									*
\subsection{Envt utility functions}
%*									*
%************************************************************************

===============  RnEnv  ================
\begin{code}
plusRnEnv (RnEnv n1 f1) (RnEnv n2 f2) 
  = RnEnv (n1 `plusGlobalRdrEnv` n2)
	  (f1 `plusNameEnv`     f2)
\end{code}


===============  NameEnv  ================
\begin{code}
plusGlobalRdrEnv :: GlobalRdrEnv -> GlobalRdrEnv -> GlobalRdrEnv
plusGlobalRdrEnv env1 env2 = plusFM_C combine_globals env1 env2

addOneToGlobalRdrEnv :: GlobalRdrEnv -> RdrName -> Name -> GlobalRdrEnv
addOneToGlobalRdrEnv env rdr_name name = addToFM_C combine_globals env rdr_name [name]

delOneFromGlobalRdrEnv :: GlobalRdrEnv -> RdrName -> GlobalRdrEnv 
delOneFromGlobalRdrEnv env rdr_name = delFromFM env rdr_name

combine_globals :: [Name] 	-- Old
		-> [Name]	-- New
		-> [Name]
combine_globals ns_old ns_new	-- ns_new is often short
  = foldr add ns_old ns_new
  where
    add n ns | all (no_conflict n) ns_old = map choose ns	-- Eliminate duplicates
	     | otherwise	          = n:ns
	     where
	       choose n' | n==n' && better_provenance n n' = n
			 | otherwise			   = n'

-- Choose 
--	a local thing		      over an	imported thing
--	a user-imported thing	      over a	non-user-imported thing
-- 	an explicitly-imported thing  over an	implicitly imported thing
better_provenance n1 n2
  = case (getNameProvenance n1, getNameProvenance n2) of
	(LocalDef _ _,			      _				  ) -> True
	(NonLocalDef (UserImport _ _ True) _, _				  ) -> True
	(NonLocalDef (UserImport _ _ _   ) _, NonLocalDef ImplicitImport _) -> True
	other								    -> False

no_conflict :: Name -> Name -> Bool
no_conflict n1 n2 | isLocallyDefined n1 && isLocallyDefined n2 = False
		  | otherwise 		                       = n1 == n2
	-- We complain of a conflict if one RdrName maps to two different Names,
	-- OR if one RdrName maps to the same *locally-defined* Name.  The latter
	-- case is to catch two separate, local definitions of the same thing.
	--
	-- If a module imports itself then there might be a local defn and an imported
	-- defn of the same name; in this case the names will compare as equal, but
	-- will still have different provenances
\end{code}



===============  ExportAvails  ================
\begin{code}
mkEmptyExportAvails :: Module -> ExportAvails
mkEmptyExportAvails mod_name = (unitFM mod_name [], emptyUFM)

mkExportAvails :: Module -> Bool -> GlobalRdrEnv -> [AvailInfo] -> ExportAvails
mkExportAvails mod_name unqual_imp name_env avails
  = (mod_avail_env, entity_avail_env)
  where
    mod_avail_env = unitFM mod_name unqual_avails 

	-- unqual_avails is the Avails that are visible in *unqualfied* form
	-- (1.4 Report, Section 5.1.1)
	-- For example, in 
	--	import T hiding( f )
	-- we delete f from avails

    unqual_avails | not unqual_imp = []	-- Short cut when no unqualified imports
		  | otherwise      = mapMaybe prune avails

    prune (Avail n) | unqual_in_scope n = Just (Avail n)
    prune (Avail n) | otherwise		= Nothing
    prune (AvailTC n ns) | null uqs     = Nothing
			 | otherwise    = Just (AvailTC n uqs)
			 where
			   uqs = filter unqual_in_scope ns

    unqual_in_scope n = unQualInScope name_env n

    entity_avail_env = listToUFM [ (name,avail) | avail <- avails, 
			  	   		  name  <- availNames avail]

plusExportAvails ::  ExportAvails ->  ExportAvails ->  ExportAvails
plusExportAvails (m1, e1) (m2, e2)
  = (plusFM_C (++) m1 m2, plusUFM_C plusAvail e1 e2)
	-- ToDo: wasteful: we do this once for each constructor!
\end{code}


===============  AvailInfo  ================
\begin{code}
plusAvail (Avail n1)	   (Avail n2)	    = Avail n1
plusAvail (AvailTC n1 ns1) (AvailTC n2 ns2) = AvailTC n1 (nub (ns1 ++ ns2))
-- Added SOF 4/97
#ifdef DEBUG
plusAvail a1 a2 = pprPanic "RnEnv.plusAvail" (hsep [pprAvail a1,pprAvail a2])
#endif

addAvailToNameSet :: NameSet -> AvailInfo -> NameSet
addAvailToNameSet names avail = addListToNameSet names (availNames avail)

availsToNameSet :: [AvailInfo] -> NameSet
availsToNameSet avails = foldl addAvailToNameSet emptyNameSet avails

availName :: AvailInfo -> Name
availName (Avail n)     = n
availName (AvailTC n _) = n

availNames :: AvailInfo -> [Name]
availNames (Avail n)      = [n]
availNames (AvailTC n ns) = ns

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

filterAvail (IEThingAll _) avail@(AvailTC _ _)  = Just avail

filterAvail ie avail = Nothing


-- In interfaces, pprAvail gets given the OccName of the "host" thing
pprAvail avail = getPprStyle $ \ sty ->
	         if ifaceStyle sty then
		    ppr_avail (pprOccName . nameOccName) avail
		 else
		    ppr_avail ppr avail

ppr_avail pp_name (AvailTC n ns) = hsep [
				     pp_name n,
				     parens  $ hsep $ punctuate comma $
				     map pp_name ns
				   ]
ppr_avail pp_name (Avail n) = pp_name n
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

emptyFVs  = emptyNameSet
plusFVs   = unionManyNameSets
plusFV    = unionNameSets

-- No point in adding implicitly imported names to the free-var set
addOneFV s n = addOneToNameSet s n
unitFV     n = unitNameSet n
\end{code}


%************************************************************************
%*									*
\subsection{Envt utility functions}
%*									*
%************************************************************************


\begin{code}
warnUnusedLocalBinds, warnUnusedTopNames, warnUnusedMatches :: [Name] -> RnM s d ()

warnUnusedTopNames names
  | not opt_WarnUnusedBinds && not opt_WarnUnusedImports = returnRn ()	-- Don't force ns unless necessary
  | otherwise						 = warnUnusedBinds names

warnUnusedLocalBinds ns
  | not opt_WarnUnusedBinds = returnRn ()
  | otherwise		    = warnUnusedBinds ns

warnUnusedMatches names
  | opt_WarnUnusedMatches = warnUnusedGroup names
  | otherwise 		  = returnRn ()

-------------------------

warnUnusedBinds :: [Name] -> RnM s d ()
warnUnusedBinds names
  = mapRn warnUnusedGroup groups	`thenRn_`
    returnRn ()
  where
	-- Group by provenance
   groups = equivClasses cmp names
   name1 `cmp` name2 = getNameProvenance name1 `cmp_prov` getNameProvenance name2
 
   cmp_prov (LocalDef _ _) (NonLocalDef _ _)       = LT
   cmp_prov (LocalDef loc1 _) (LocalDef loc2 _)    = loc1 `compare` loc2
   cmp_prov (NonLocalDef (UserImport m1 loc1 _) _)
            (NonLocalDef (UserImport m2 loc2 _) _) = (m1 `compare` m2) `thenCmp` (loc1 `compare` loc2)
   cmp_prov (NonLocalDef _ _) (LocalDef _ _)       = GT
			-- In-scope NonLocalDefs must have UserImport info on them

-------------------------

warnUnusedGroup :: [Name] -> RnM s d ()
warnUnusedGroup []
  = returnRn ()

warnUnusedGroup names
  | is_local     && not opt_WarnUnusedBinds   = returnRn ()
  | not is_local && not opt_WarnUnusedImports = returnRn ()
  | otherwise
  = pushSrcLocRn def_loc	$
    addWarnRn			$
    sep [msg <> colon, nest 4 (fsep (punctuate comma (map ppr names)))]
  where
    name1 = head names
    (is_local, def_loc, msg)
	= case getNameProvenance name1 of
		LocalDef loc _ 			     -> (True, loc, text "Defined but not used")
		NonLocalDef (UserImport mod loc _) _ -> (True, loc, text "Imported from" <+> quotes (ppr mod) <+> 
								     text "but but not used")
		other -> (False, getSrcLoc name1, text "Strangely defined but not used")
\end{code}

\begin{code}
addNameClashErrRn rdr_name (name1:names)
  = addErrRn (vcat [ptext SLIT("Ambiguous occurrence") <+> quotes (ppr rdr_name),
		    ptext SLIT("It could refer to") <+> vcat (msg1 : msgs)])
  where
    msg1 = ptext  SLIT("either") <+> mk_ref name1
    msgs = [ptext SLIT("    or") <+> mk_ref name | name <- names]
    mk_ref name = quotes (ppr name) <> comma <+> pprNameProvenance name

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

