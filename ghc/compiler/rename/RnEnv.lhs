%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
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
import Name		( Name, OccName(..), Provenance(..), ExportFlag(..), NamedThing(..),
			  occNameFlavour, getSrcLoc,
			  NameSet, emptyNameSet, addListToNameSet, nameSetToList,
			  mkLocalName, mkGlobalName, modAndOcc,
			  nameOccName, setNameProvenance, isVarOcc, getNameProvenance,
			  pprOccName, isLocalName
			)
import TyCon		( TyCon )
import TysWiredIn	( tupleTyCon, listTyCon, charTyCon )
import FiniteMap
import Unique		( Unique, Uniquable(..), unboundKey )
import UniqFM           ( listToUFM, plusUFM_C )
import UniqSupply
import SrcLoc		( SrcLoc, noSrcLoc )
import Outputable
import Util		( removeDups )
import List		( nub )
\end{code}



%*********************************************************
%*							*
\subsection{Making new names}
%*							*
%*********************************************************

\begin{code}
newImportedGlobalName :: Module -> OccName 
	  	      -> IfaceFlavour
	      	      -> RnM s d Name
newImportedGlobalName mod occ hif
  = 	-- First check the cache
    getNameSupplyRn		`thenRn` \ (us, inst_ns, cache) ->
    let 
	key = (mod,occ)
	prov = NonLocalDef noSrcLoc hif False
    in
    case lookupFM cache key of

	-- A hit in the cache!
	-- If it has no provenance at the moment then set its provenance
	-- so that it has the right HiFlag component.
	-- (This is necessary
	-- for known-key things.  For example, GHCmain.lhs imports as SOURCE
	-- Main; but Main.main is a known-key thing.)  
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
			uniq   	   = getUnique us1
			name       = mkGlobalName uniq mod occ prov
			new_cache  = addToFM cache key name
		   in
		   setNameSupplyRn (us', inst_ns, new_cache)		`thenRn_`
		   returnRn name

{-
	    let
	      pprC ((mod,occ),name) = pprModule mod <> text "." <> pprOccName occ <+> text "--->" 
				     <+> ppr name
	    in
            pprTrace "ng" (vcat [text "newGlobalName miss" <+> pprModule mod <+> pprOccName occ,
			   brackets (sep (map pprC (fmToList cache))),
			   text ""
			  ])		$
-}


newLocallyDefinedGlobalName :: Module -> OccName 
			    -> (Name -> ExportFlag) -> SrcLoc
			    -> RnM s d Name
newLocallyDefinedGlobalName mod occ rec_exp_fn loc
  = 	-- First check the cache
    getNameSupplyRn		`thenRn` \ (us, inst_ns, cache) ->
    let 
	key = (mod,occ)
    in
    case lookupFM cache key of

	-- A hit in the cache!
	-- Overwrite whatever provenance is in the cache already; 
	-- this updates WiredIn things and known-key things, 
	-- which are there from the start, to LocalDef.
	Just name -> let 
			new_name = setNameProvenance name (LocalDef loc (rec_exp_fn new_name))
			new_cache = addToFM cache key new_name
		     in
		     setNameSupplyRn (us, inst_ns, new_cache)		`thenRn_`
		     returnRn new_name
		     
	-- Miss in the cache!
	-- Build a new original name, and put it in the cache
	Nothing -> let
			provenance = LocalDef loc (rec_exp_fn new_name)
			(us', us1) = splitUniqSupply us
			uniq   	   = getUnique us1
			new_name   = mkGlobalName uniq mod occ provenance
			new_cache  = addToFM cache key new_name
		   in
		   setNameSupplyRn (us', inst_ns, new_cache)		`thenRn_`
		   returnRn new_name


-- newDfunName is a variant, specially for dfuns.  
-- When renaming derived definitions we are in *interface* mode (because we can trip
-- over original names), but we still want to make the Dfun locally-defined.
-- So we can't use whether or not we're in source mode to decide the locally-defined question.
newDfunName :: Maybe RdrName -> SrcLoc -> RnMS s Name
newDfunName Nothing src_loc			-- Local instance decls have a "Nothing"
  = getModuleRn		`thenRn` \ mod_name ->
    newInstUniq		`thenRn` \ inst_uniq ->
    let
	dfun_occ = VarOcc (_PK_ ("$d" ++ show inst_uniq))
    in
    newLocallyDefinedGlobalName mod_name dfun_occ 
				(\_ -> Exported) src_loc

newDfunName (Just n) src_loc			-- Imported ones have "Just n"
  = getModuleRn		`thenRn` \ mod_name ->
    newImportedGlobalName mod_name (rdrNameOcc n) HiFile {- Correct? -} 


newLocalNames :: [(RdrName,SrcLoc)] -> RnM s d [Name]
newLocalNames rdr_names
  = getNameSupplyRn		`thenRn` \ (us, inst_ns, cache) ->
    let
	n	   = length rdr_names
	(us', us1) = splitUniqSupply us
	uniqs	   = getUniques n us1
	locals	   = [ mkLocalName uniq (rdrNameOcc rdr_name) loc
		     | ((rdr_name,loc), uniq) <- rdr_names `zip` uniqs
		     ]
    in
    setNameSupplyRn (us', inst_ns, cache)	`thenRn_`
    returnRn locals

-- mkUnboundName makes a place-holder Name; it shouldn't be looked at except possibly
-- during compiler debugging.
mkUnboundName :: RdrName -> Name
mkUnboundName rdr_name = mkLocalName unboundKey (rdrNameOcc rdr_name) noSrcLoc

isUnboundName :: Name -> Bool
isUnboundName name = uniqueOf name == unboundKey
\end{code}

\begin{code}
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
	new_name_env = addListToFM name_env (map fst rdr_names_w_loc `zip` names)
    in
    setLocalNameEnv new_name_env (enclosed_scope names)
  where
    check_shadow name_env (rdr_name,loc)
	= case lookupFM name_env rdr_name of
		Nothing   -> returnRn ()
		Just name -> pushSrcLocRn loc $
			     addWarnRn (shadowedNameWarn rdr_name)

bindLocalsRn doc_str rdr_names enclosed_scope
  = getSrcLocRn		`thenRn` \ loc ->
    bindLocatedLocalsRn (text doc_str)
			(rdr_names `zip` repeat loc)
		 	enclosed_scope

bindTyVarsRn doc_str tyvar_names enclosed_scope
  = getSrcLocRn					`thenRn` \ loc ->
    let
	located_tyvars = [(getTyVarName tv, loc) | tv <- tyvar_names] 
    in
    bindLocatedLocalsRn doc_str located_tyvars	$ \ names ->
    enclosed_scope (zipWith replaceTyVarName tyvar_names names)

	-- Works in any variant of the renamer monad
checkDupOrQualNames, checkDupNames :: SDoc
				   -> [(RdrName, SrcLoc)]
				   -> RnM s d ()

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
lookupRn :: RdrName
	 -> Maybe Name		-- Result of environment lookup
	 -> RnMS s Name

lookupRn rdr_name (Just name) 
  = 	-- Found the name in the envt
    returnRn name	-- In interface mode the only things in 
			-- the environment are things in local (nested) scopes

lookupRn rdr_name Nothing
  =	-- We didn't find the name in the environment
    getModeRn 		`thenRn` \ mode ->
    case mode of {
	SourceMode -> failWithRn (mkUnboundName rdr_name)
				 (unknownNameErr rdr_name) ;
		-- Souurce mode; lookup failure is an error

        InterfaceMode _ _ ->


	----------------------------------------------------
	-- OK, so we're in interface mode
	-- An Unqual is allowed; interface files contain 
	-- unqualified names for locally-defined things, such as
	-- constructors of a data type.
	-- So, qualify the unqualified name with the 
	-- module of the interface file, and try again
    case rdr_name of 
	Unqual occ       -> getModuleRn		`thenRn` \ mod ->
		            newImportedGlobalName mod occ HiFile
	Qual mod occ hif -> newImportedGlobalName mod occ hif

    }

lookupBndrRn rdr_name
  = lookupNameRn rdr_name		`thenRn` \ maybe_name ->
    lookupRn rdr_name maybe_name	`thenRn` \ name ->

    if isLocalName name then
	returnRn name
    else

	----------------------------------------------------
	-- OK, so we're at the binding site of a top-level defn
	-- Check to see whether its an imported decl
    getModeRn		`thenRn` \ mode ->
    case mode of {
	  SourceMode -> returnRn name ;

	  InterfaceMode _ print_unqual_fn -> 

	----------------------------------------------------
	-- OK, the binding site of an *imported* defn
	-- so we can make the provenance more informative
    getSrcLocRn		`thenRn` \ src_loc ->
    let
	name' = case getNameProvenance name of
		    NonLocalDef _ hif _ -> setNameProvenance name 
						(NonLocalDef src_loc hif (print_unqual_fn name'))
		    other		-> name
    in
    returnRn name'
    }

-- Just like lookupRn except that we record the occurrence too
-- Perhaps surprisingly, even wired-in names are recorded.
-- Why?  So that we know which wired-in names are referred to when
-- deciding which instance declarations to import.
lookupOccRn :: RdrName -> RnMS s Name
lookupOccRn rdr_name
  = lookupNameRn rdr_name		`thenRn` \ maybe_name ->
    lookupRn rdr_name maybe_name	`thenRn` \ name ->
    let
	name' = mungePrintUnqual rdr_name name
    in
    addOccurrenceName name'

-- lookupGlobalOccRn is like lookupOccRn, except that it looks in the global 
-- environment only.  It's used for record field names only.
lookupGlobalOccRn :: RdrName -> RnMS s Name
lookupGlobalOccRn rdr_name
  = lookupGlobalNameRn rdr_name		`thenRn` \ maybe_name ->
    lookupRn rdr_name maybe_name	`thenRn` \ name ->
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
mungePrintUnqual (Unqual _)   name = case new_prov of
					Nothing    -> name
					Just prov' -> setNameProvenance name prov'
				   where
				     new_prov = case getNameProvenance name of
						   NonLocalDef loc hif False -> Just (NonLocalDef loc hif True)
						   other		     -> Nothing

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

listType_RDR	= qual (modAndOcc listType_name)
tupleType_RDR n	= qual (modAndOcc (tupleType_name n))

charType_name    = getName charTyCon
listType_name    = getName listTyCon
tupleType_name n = getName (tupleTyCon n)
\end{code}

\begin{code}
lookupFixity :: RdrName -> RnMS s Fixity
lookupFixity rdr_name
  = getFixityEnv	`thenRn` \ fixity_env ->
    returnRn (lookupFixityEnv fixity_env rdr_name)
\end{code}

mkImportFn returns a function that takes a Name and tells whether
its unqualified name is in scope.  This is put as a boolean flag in
the Name's provenance to guide whether or not to print the name qualified
in error messages.

\begin{code}
mkImportFn :: RnEnv -> Name -> Bool
mkImportFn (RnEnv env _)
  = lookup
  where
    lookup name = case lookupFM env (Unqual (nameOccName name)) of
			   Just (name', _) -> name == name'
			   Nothing         -> False
\end{code}

%************************************************************************
%*									*
\subsection{Envt utility functions}
%*									*
%************************************************************************

===============  RnEnv  ================
\begin{code}
plusRnEnv (RnEnv n1 f1) (RnEnv n2 f2) 
  = plusGlobalNameEnvRn n1 n2		`thenRn` \ n ->
    plusFixityEnvRn f1 f2		`thenRn` \ f -> 
    returnRn (RnEnv n f)
\end{code}


===============  NameEnv  ================
\begin{code}
plusGlobalNameEnvRn :: GlobalNameEnv -> GlobalNameEnv -> RnM s d GlobalNameEnv
plusGlobalNameEnvRn env1 env2
  = mapRn (addErrRn.nameClashErr) (conflictsFM conflicting_name env1 env2)		`thenRn_`
    returnRn (env1 `plusFM` env2)

addOneToGlobalNameEnv :: GlobalNameEnv -> RdrName -> (Name, HowInScope) -> RnM s d GlobalNameEnv
addOneToGlobalNameEnv env rdr_name name
 = case lookupFM env rdr_name of
	Just name2 | conflicting_name name name2
		   -> addErrRn (nameClashErr (rdr_name, (name, name2)))	`thenRn_`
		      returnRn env

	other      -> returnRn (addToFM env rdr_name name)

delOneFromGlobalNameEnv :: GlobalNameEnv -> RdrName -> GlobalNameEnv 
delOneFromGlobalNameEnv env rdr_name = delFromFM env rdr_name

conflicting_name :: (Name, HowInScope) -> (Name, HowInScope) -> Bool
conflicting_name (n1, FromLocalDefn _) (n2, FromLocalDefn _) = True
conflicting_name (n1,h1) 	       (n2,h2) 		     = n1 /= n2
	-- We complain of a conflict if one RdrName maps to two different Names,
	-- OR if one RdrName maps to the same *locally-defined* Name.  The latter
	-- case is to catch two separate, local definitions of the same thing.
	--
	-- If a module imports itself then there might be a local defn and an imported
	-- defn of the same name; in this case the names will compare as equal, but
	-- will still have different HowInScope fields

lookupNameEnv :: NameEnv -> RdrName -> Maybe Name
lookupNameEnv = lookupFM
\end{code}

===============  FixityEnv  ================
\begin{code}
plusFixityEnvRn f1 f2
  = mapRn (addErrRn.fixityClashErr) (conflictsFM bad_fix f1 f2)		`thenRn_`
    returnRn (f1 `plusFM` f2)

addOneToFixityEnv env rdr_name fixity = addToFM env rdr_name fixity

lookupFixityEnv env rdr_name 
  = case lookupFM env rdr_name of
	Just (fixity,_) -> fixity
	Nothing	        -> Fixity 9 InfixL 		-- Default case

bad_fix :: (Fixity, HowInScope) -> (Fixity, HowInScope) -> Bool
bad_fix (f1,_) (f2,_) = f1 /= f2

pprFixityProvenance :: (Fixity, HowInScope) -> SDoc
pprFixityProvenance (fixity, how_in_scope) = ppr how_in_scope
\end{code}



===============  ExportAvails  ================
\begin{code}
mkExportAvails :: Module -> Bool -> GlobalNameEnv -> [AvailInfo] -> ExportAvails
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
		  | otherwise      = [prune avail | avail <- avails]

    prune (Avail n) | unqual_in_scope n = Avail n
    prune (Avail n) | otherwise		= NotAvailable
    prune (AvailTC n ns) 		= AvailTC n (filter unqual_in_scope ns)

    unqual_in_scope n = Unqual (nameOccName n) `elemFM` name_env

    entity_avail_env = listToUFM [ (name,avail) | avail <- avails, 
			  	   		  name  <- availEntityNames avail]

plusExportAvails ::  ExportAvails ->  ExportAvails ->  ExportAvails
plusExportAvails (m1, e1) (m2, e2)
  = (plusFM_C (++) m1 m2, plusUFM_C plusAvail e1 e2)
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

-- availEntityNames is used to extract the names that can appear on their own in
-- an export or import list.  For class decls, class methods can appear on their
-- own, thus 	import A( op )
-- but constructors cannot; thus
--		import B( T )
-- means import type T from B, not constructor T.

availEntityNames :: AvailInfo -> [Name]
availEntityNames NotAvailable   = []
availEntityNames (Avail n)      = [n]
availEntityNames (AvailTC n ns) = n : filter (isVarOcc . nameOccName) ns

filterAvail :: RdrNameIE	-- Wanted
	    -> AvailInfo	-- Available
	    -> AvailInfo	-- Resulting available; 
				-- NotAvailable if wanted stuff isn't there

filterAvail ie@(IEThingWith want wants) avail@(AvailTC n ns)
  | sub_names_ok = AvailTC n (filter is_wanted ns)
  | otherwise    = pprTrace "filterAvail" (hsep [ppr ie, pprAvail avail]) $
		   NotAvailable
  where
    is_wanted name = nameOccName name `elem` wanted_occs
    sub_names_ok   = all (`elem` avail_occs) wanted_occs
    avail_occs	   = map nameOccName ns
    wanted_occs    = map rdrNameOcc (want:wants)

filterAvail (IEThingAbs _) (AvailTC n ns)       = ASSERT( n `elem` ns ) 
						  AvailTC n [n]

filterAvail (IEThingAbs _) avail@(Avail n)      = avail		-- Type synonyms

filterAvail (IEVar _)      avail@(Avail n)      = avail
filterAvail (IEVar v)      avail@(AvailTC n ns) = AvailTC n (filter wanted ns)
						where
						  wanted n = nameOccName n == occ
						  occ      = rdrNameOcc v
	-- The second equation happens if we import a class op, thus
	-- 	import A( op ) 
	-- where op is a class operation

filterAvail (IEThingAll _) avail@(AvailTC _ _)  = avail

filterAvail ie avail = NotAvailable 


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
\subsection{Finite map utilities}
%*									*
%************************************************************************


Generally useful function on finite maps to check for overlap.

\begin{code}
conflictsFM :: Ord a 
	    => (b->b->Bool)		-- False <=> no conflict; you can pick either
	    -> FiniteMap a b -> FiniteMap a b
	    -> [(a,(b,b))]
conflictsFM bad fm1 fm2 
  = filter (\(a,(b1,b2)) -> bad b1 b2)
	   (fmToList (intersectFM_C (\b1 b2 -> (b1,b2)) fm1 fm2))

conflictFM :: Ord a 
	   => (b->b->Bool)
	   -> FiniteMap a b -> a -> b
	   -> Maybe (a,(b,b))
conflictFM bad fm key elt
  = case lookupFM fm key of
	Just elt' | bad elt elt' -> Just (key,(elt,elt'))
	other			 -> Nothing
\end{code}


%************************************************************************
%*									*
\subsection{Envt utility functions}
%*									*
%************************************************************************


\begin{code}
warnUnusedBinds, warnUnusedMatches, warnUnusedImports :: NameSet -> RnM s d ()

warnUnusedBinds names
  | opt_WarnUnusedBinds = warnUnusedNames names
  | otherwise           = returnRn ()

warnUnusedMatches names
  | opt_WarnUnusedMatches = warnUnusedNames names
  | otherwise           = returnRn ()

warnUnusedImports names
  | opt_WarnUnusedImports = warnUnusedNames names
  | otherwise           = returnRn ()

warnUnusedNames :: NameSet -> RnM s d ()
warnUnusedNames names 
  = mapRn warn (nameSetToList names)	`thenRn_`
    returnRn ()
  where
    warn name = pushSrcLocRn (getSrcLoc name) $
		addWarnRn (unusedNameWarn name)

unusedNameWarn name = quotes (ppr name) <+> ptext SLIT("is bound but not used")

nameClashErr (rdr_name, ((_,how_in_scope1), (_, how_in_scope2)))
  = hang (hsep [ptext SLIT("Conflicting definitions for"), quotes (ppr rdr_name)])
	4 (vcat [ppr how_in_scope1,
		 ppr how_in_scope2])

fixityClashErr (rdr_name, ((_,how_in_scope1), (_, how_in_scope2)))
  = hang (hsep [ptext SLIT("Conflicting fixities for"), quotes (ppr rdr_name)])
	4 (vcat [ppr how_in_scope1,
		 ppr how_in_scope2])

shadowedNameWarn shadow
  = hcat [ptext SLIT("This binding for"), 
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

