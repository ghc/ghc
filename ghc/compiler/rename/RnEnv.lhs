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
import RdrHsSyn		( RdrName(..), RdrNameIE,
			  rdrNameOcc, isQual, qual
			)
import HsTypes		( getTyVarName, replaceTyVarName )
import BasicTypes	( Fixity(..), FixityDirection(..), IfaceFlavour(..) )
import RnMonad
import Name		( Name, Provenance(..), ExportFlag(..), NamedThing(..),
			  ImportReason(..), getSrcLoc, 
			  mkLocalName, mkGlobalName, 
			  nameOccName, 
			  pprOccName, isLocalName, isLocallyDefined, isAnonOcc,
			  setNameProvenance, getNameProvenance, pprNameProvenance
			)
import NameSet
import OccName		( OccName, mkModuleFS, 
			  mkDFunOcc, tcOcc, varOcc, tvOcc,
			  isVarOcc, occNameFlavour, occNameString
			)
import TyCon		( TyCon )
import FiniteMap
import Unique		( Unique, Uniquable(..), unboundKey )
import UniqFM           ( emptyUFM, listToUFM, plusUFM_C )
import UniqSupply
import SrcLoc		( SrcLoc, noSrcLoc )
import Outputable
import Util		( removeDups )
import List		( nub )
\end{code}



%*********************************************************
%*							*
\subsection{Making new rdr names}
%*							*
%*********************************************************

These functions make new RdrNames from stuff read from an interface file

\begin{code}
ifaceQualTC  (m,n,hif) = Qual (mkModuleFS m) (tcOcc n) hif
ifaceQualVar (m,n,hif) = Qual (mkModuleFS m) (varOcc n) hif

ifaceUnqualTC  n = Unqual (tcOcc n)
ifaceUnqualVar n = Unqual (varOcc n)
ifaceUnqualTv  n = Unqual (tvOcc n)
\end{code}

%*********************************************************
%*							*
\subsection{Making new names}
%*							*
%*********************************************************

\begin{code}
newImportedGlobalName :: Module -> OccName -> IfaceFlavour
	      	      -> RnM s d Name
newImportedGlobalName mod occ hif
  = 	-- First check the cache
    getNameSupplyRn		`thenRn` \ (us, inst_ns, cache) ->
    let 
	key = (mod,occ)
	prov = NonLocalDef ImplicitImport hif False
	-- For in-scope things we improve the provenance in RnNames.qualifyImports
    in
    case lookupFM cache key of
	
	-- A hit in the cache!
	-- If it has no provenance at the moment then set its provenance
	-- so that it has the right HiFlag component.
	-- (This is necessary for known-key things.  
	-- 	For example, GHCmain.lhs imports as SOURCE
	-- 	Main; but Main.main is a known-key thing.)  
	-- Don't fiddle with the provenance if it already has one
	Just name -> case getNameProvenance name of
			NoProvenance -> let
					  new_name = setNameProvenance name prov
					  new_cache = addToFM cache key new_name
					in
					setNameSupplyRn (us, inst_ns, new_cache)	`thenRn_`
					returnRn new_name
			other	     -> returnRn name
		     
	Nothing -> 	-- Miss in the cache!
			-- Build a new original name, and put it in the cache
		   let
			(us', us1) = splitUniqSupply us
			uniq   	   = uniqFromSupply us1
			name       = mkGlobalName uniq mod occ prov
			new_cache  = addToFM cache key name
		   in
		   setNameSupplyRn (us', inst_ns, new_cache)		`thenRn_`
		   returnRn name


newImportedGlobalFromRdrName (Qual mod_name occ hif)
  = newImportedGlobalName mod_name occ hif

newImportedGlobalFromRdrName (Unqual occ)
  =	-- An Unqual is allowed; interface files contain 
	-- unqualified names for locally-defined things, such as
	-- constructors of a data type.
    getModuleRn 	`thenRn ` \ mod_name ->
    newImportedGlobalName mod_name occ HiFile


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
	  -- Note: we're not making use of the source location. Not good.
	locals	   = [ mkLocalName uniq (rdrNameOcc rdr_name)
		     | ((rdr_name,loc), uniq) <- rdr_names `zip` uniqs
		     ]
    in
    setNameSupplyRn (us', inst_ns, cache)	`thenRn_`
    returnRn locals

newDFunName cl_occ tycon_occ (Just n) src_loc		-- Imported ones have "Just n"
  = getModuleRn		`thenRn` \ mod_name ->
    newImportedGlobalName mod_name (rdrNameOcc n) HiFile {- Correct? -} 

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
mkUnboundName rdr_name = mkLocalName unboundKey (rdrNameOcc rdr_name)

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
extendTyVarEnvRn :: [HsTyVar Name] -> RnMS s a -> RnMS s a
	-- This tiresome function is used only in rnDecl on InstDecl
extendTyVarEnvRn tyvars enclosed_scope
  = getLocalNameEnv		`thenRn` \ env ->
    let
	new_env = addListToRdrEnv env [ (Unqual (getOccName name), name) 
				      | tyvar <- tyvars,
					let name = getTyVarName tyvar 
				      ]
    in
    setLocalNameEnv new_env enclosed_scope

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


-- Yuk!
ifaceFlavour name = case getNameProvenance name of
			NonLocalDef _ hif _ -> hif
			other		    -> HiFile	-- Shouldn't happen
\end{code}


%*********************************************************
%*							*
\subsection{Looking up names}
%*							*
%*********************************************************

Looking up a name in the RnEnv.

\begin{code}
checkUnboundRn :: RdrName -> Maybe Name -> RnMS s Name
checkUnboundRn rdr_name (Just name) 
  = 	-- Found it!
     returnRn name

checkUnboundRn rdr_name Nothing
  =	-- Not found by lookup
    getModeRn	`thenRn` \ mode ->
    case mode of 
	-- Not found when processing source code; so fail
	SourceMode    -> failWithRn (mkUnboundName rdr_name)
			            (unknownNameErr rdr_name)
		
	-- Not found when processing an imported declaration,
	-- so we create a new name for the purpose
	InterfaceMode _ -> newImportedGlobalFromRdrName rdr_name

lookupBndrRn rdr_name
  = lookupNameRn rdr_name		`thenRn` \ maybe_name ->
    checkUnboundRn rdr_name maybe_name

-- Just like lookupRn except that we record the occurrence too
-- Perhaps surprisingly, even wired-in names are recorded.
-- Why?  So that we know which wired-in names are referred to when
-- deciding which instance declarations to import.
lookupOccRn :: RdrName -> RnMS s Name
lookupOccRn rdr_name
  = lookupNameRn rdr_name		`thenRn` \ maybe_name ->
    checkUnboundRn rdr_name maybe_name	`thenRn` \ name ->
    let
	name' = mungePrintUnqual rdr_name name
    in
    addOccurrenceName name'

-- lookupGlobalOccRn is like lookupOccRn, except that it looks in the global 
-- environment.  It's used only for
--	record field names
--	class op names in class and instance decls
lookupGlobalOccRn :: RdrName -> RnMS s Name
lookupGlobalOccRn rdr_name
  = lookupGlobalNameRn rdr_name		`thenRn` \ maybe_name ->
    checkUnboundRn rdr_name maybe_name	`thenRn` \ name ->
    let
	name' = mungePrintUnqual rdr_name name
    in
    addOccurrenceName name'


-- mungePrintUnqual is used to make *imported* *occurrences* print unqualified
-- if they were mentioned unqualified in the source code.
-- This improves error messages from the type checker.
-- NB: the binding site is treated differently; see lookupBndrRn
--     After the type checker all occurrences are replaced by the one
--     at the binding site.
mungePrintUnqual (Qual _ _ _) name = name
mungePrintUnqual (Unqual _)   name 
  = case getNameProvenance name of
	NonLocalDef imp hif False -> setNameProvenance name (NonLocalDef imp hif True)
	other			  -> name

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
lookupImplicitOccRn (Qual mod occ hif)
 = newImportedGlobalName mod occ hif	`thenRn` \ name ->
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

mkPrintUnqualFn returns a function that takes a Name and tells whether
its unqualified name is in scope.  This is put as a boolean flag in
the Name's provenance to guide whether or not to print the name qualified
in error messages.

\begin{code}
mkPrintUnqualFn :: GlobalRdrEnv -> Name -> Bool
mkPrintUnqualFn env
  = lookup
  where
    lookup name = case lookupRdrEnv env (Unqual (nameOccName name)) of
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
-- Look in global env only
lookupGlobalNameRn :: RdrName -> RnMS s (Maybe Name)
lookupGlobalNameRn rdr_name
  = getNameEnvs		`thenRn` \ (global_env, local_env) ->
    lookup_global global_env rdr_name

-- Look in both local and global env
lookupNameRn :: RdrName -> RnMS s (Maybe Name)
lookupNameRn rdr_name
  = getNameEnvs		`thenRn` \ (global_env, local_env) ->
    case lookupRdrEnv local_env rdr_name of
	  Just name -> returnRn (Just name)
	  Nothing   -> lookup_global global_env rdr_name

lookup_global global_env rdr_name
  = case lookupRdrEnv global_env rdr_name of
	Just [name]	    -> returnRn (Just name)
	Just stuff@(name:_) -> addNameClashErrRn rdr_name stuff	`thenRn_`
			       returnRn (Just name)
	Nothing -> returnRn Nothing
  
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
	(LocalDef _ _,				_			      ) -> True
	(NonLocalDef (UserImport _ _ True) _ _, _			      ) -> True
	(NonLocalDef (UserImport _ _ _   ) _ _, NonLocalDef ImplicitImport _ _) -> True
	other									-> False

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
		  | otherwise      = [ avail' | avail  <- avails 
					      , let avail' = prune avail
					      , case avail' of
					          NotAvailable -> False
						  _            -> True
					      ]

    prune (Avail n) | unqual_in_scope n = Avail n
    prune (Avail n) | otherwise		= NotAvailable
    prune (AvailTC n ns) 		= AvailTC n (filter unqual_in_scope ns)

    unqual_in_scope n = Unqual (nameOccName n) `elemFM` name_env

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
plusAvail a NotAvailable = a
plusAvail NotAvailable a = a
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
availNames NotAvailable   = []
availNames (Avail n)      = [n]
availNames (AvailTC n ns) = ns

filterAvail :: RdrNameIE	-- Wanted
	    -> AvailInfo	-- Available
	    -> AvailInfo	-- Resulting available; 
				-- NotAvailable if (any of the) wanted stuff isn't there

filterAvail ie@(IEThingWith want wants) avail@(AvailTC n ns)
  | sub_names_ok = AvailTC n (filter is_wanted ns)
  | otherwise    = 
#ifdef DEBUG
		   pprTrace "filterAvail" (hsep [ppr ie, pprAvail avail]) $
#endif
		   NotAvailable
  where
    is_wanted name = nameOccName name `elem` wanted_occs
    sub_names_ok   = all (`elem` avail_occs) wanted_occs
    avail_occs	   = map nameOccName ns
    wanted_occs    = map rdrNameOcc (want:wants)

filterAvail (IEThingAbs _) (AvailTC n ns)       = ASSERT( n `elem` ns ) 
						  AvailTC n [n]
filterAvail (IEThingAll _) avail@(AvailTC _ _)  = avail

filterAvail (IEVar _)      avail@(Avail n)      = avail
filterAvail (IEVar v)      avail@(AvailTC n ns) = AvailTC n (filter wanted ns)
						where
						  wanted n = nameOccName n == occ
						  occ      = rdrNameOcc v
	-- The second equation happens if we import a class op, thus
	-- 	import A( op ) 
	-- where op is a class operation


#ifdef DEBUG
filterAvail ie avail = pprPanic "filterAvail" (ppr ie $$ pprAvail avail)
#endif


-- In interfaces, pprAvail gets given the OccName of the "host" thing
pprAvail avail = getPprStyle $ \ sty ->
	         if ifaceStyle sty then
		    ppr_avail (pprOccName . nameOccName) avail
		 else
		    ppr_avail ppr avail

ppr_avail pp_name NotAvailable = ptext SLIT("NotAvailable")
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

plusFV    = unionNameSets
addOneFV  = addOneToNameSet
unitFV    = unitNameSet
emptyFVs  = emptyNameSet
plusFVs   = unionManyNameSets
\end{code}


%************************************************************************
%*									*
\subsection{Envt utility functions}
%*									*
%************************************************************************


\begin{code}
warnUnusedBinds, warnUnusedMatches :: [Name] -> RnM s d ()

warnUnusedTopNames ns
  | not opt_WarnUnusedBinds && not opt_WarnUnusedImports
  = returnRn ()	-- Don't force ns unless necessary

warnUnusedTopNames (n:ns)
  | is_local     && opt_WarnUnusedBinds   = warnUnusedNames False{-include name's provenance-} ns
  | not is_local && opt_WarnUnusedImports = warnUnusedNames False ns
  where
    is_local = isLocallyDefined n

warnUnusedTopName other = returnRn ()

warnUnusedBinds ns
  | not opt_WarnUnusedBinds = returnRn ()
  | otherwise		    = warnUnusedNames False ns

{-
 Haskell 98 encourages compilers to suppress warnings about
 unused names in a pattern if they start with "_". Which
 we do here.

 Note: omit the inclusion of the names' provenance in the
 generated warning -- it's already given in the header
 of the warning (+ the local names we've been given have
 a provenance that's ultra low in content.)

-}
warnUnusedMatches names
  | opt_WarnUnusedMatches = warnUnusedNames True (filter (not.isAnonOcc.getOccName) names)
  | otherwise 		  = returnRn ()

warnUnusedNames :: Bool{-display provenance-} -> [Name] -> RnM s d ()
warnUnusedNames _ []
  = returnRn ()

warnUnusedNames short_msg names 
  = addWarnRn $
    sep [text "The following names are unused:",
	 nest 4 ((if short_msg then hsep else vcat) (map pp names))]
  where
    pp n 
     | short_msg = ppr n
     | otherwise = ppr n <> comma <+> pprNameProvenance n

addNameClashErrRn rdr_name names
{-	NO LONGER NEEDED WITH LAZY NAME-CLASH REPORTING
  | isClassDataConRdrName rdr_name 
	-- Nasty hack to prevent error messages complain about conflicts for ":C",
	-- where "C" is a class.  There'll be a message about C, and :C isn't 
	-- the programmer's business.  There may be a better way to filter this
	-- out, but I couldn't get up the energy to find it.
  = returnRn ()

  | otherwise
-}

  = addErrRn (vcat [ptext SLIT("Ambiguous occurrence") <+> quotes (ppr rdr_name),
		    ptext SLIT("It could refer to:") <+> vcat (map mk_ref names)])
  where
    mk_ref name = ppr name <> colon <+> pprNameProvenance name

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
    addErrRn (hsep [ptext SLIT("Conflicting definitions for"), 
		    quotes (ppr name), 
		    ptext SLIT("in"), descriptor])
\end{code}

