%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[RnEnv]{Environment manipulation for the renamer monad}

\begin{code}
#include "HsVersions.h"

module RnEnv where		-- Export everything

IMPORT_1_3(List (nub))
IMP_Ubiq()

import CmdLineOpts	( opt_WarnNameShadowing )
import HsSyn
import RdrHsSyn		( RdrName(..), SYN_IE(RdrNameIE),
			  rdrNameOcc, ieOcc, isQual, qual
			)
import HsTypes		( getTyVarName, replaceTyVarName )
import BasicTypes	( Fixity(..), FixityDirection(..), IfaceFlavour(..), pprModule )
import RnMonad
import Name		( Name, OccName(..), Provenance(..), ExportFlag(..), NamedThing(..),
			  occNameString, occNameFlavour,
			  SYN_IE(NameSet), emptyNameSet, addListToNameSet,
			  mkLocalName, mkGlobalName, modAndOcc, isLocallyDefinedName,
			  isWiredInName, nameOccName, setNameProvenance, isVarOcc, getNameProvenance,
			  pprProvenance, pprOccName, pprModule, pprNameProvenance
			)
import TyCon		( TyCon )
import TysWiredIn	( tupleTyCon, listTyCon, charTyCon, intTyCon )
import FiniteMap
import Unique		( Unique, Uniquable(..), unboundKey )
import UniqFM           ( listToUFM, plusUFM_C )
import Maybes		( maybeToBool )
import UniqSupply
import SrcLoc		( SrcLoc, noSrcLoc )
import Pretty
import Outputable	( Outputable(..), PprStyle(..) )
import Util		( Ord3(..), panic, removeDups, pprTrace, assertPanic )

\end{code}



%*********************************************************
%*							*
\subsection{Making new names}
%*							*
%*********************************************************

\begin{code}
newGlobalName :: Module -> OccName -> IfaceFlavour -> RnM s d Name
newGlobalName mod occ iface_flavour
  = 	-- First check the cache
    getNameSupplyRn		`thenRn` \ (us, inst_ns, cache) ->
    let key = (mod,occ)         in
    case lookupFM cache key of

	-- A hit in the cache!  Return it, but change the src loc
	-- of the thing we've found if this is a second definition site
	-- (that is, if loc /= NoSrcLoc)
	Just name ->  returnRn name

	-- Miss in the cache, so build a new original name,
	-- And put it in the cache
	Nothing        -> 
	    let
		(us', us1) = splitUniqSupply us
		uniq   	   = getUnique us1
		name       = mkGlobalName uniq mod occ (Implicit iface_flavour)
		cache'     = addToFM cache key name
	    in
	    setNameSupplyRn (us', inst_ns, cache')		`thenRn_`
	    returnRn name

newLocallyDefinedGlobalName :: Module -> OccName 
			    -> (Name -> ExportFlag) -> SrcLoc
			    -> RnM s d Name
newLocallyDefinedGlobalName mod occ rec_exp_fn loc
  = 	-- First check the cache
    getNameSupplyRn		`thenRn` \ (us, inst_ns, cache) ->

	-- We are at the binding site for a locally-defined thing, so
	-- you might think it can't be in the cache, but it can if it's a
	-- wired in thing. In that case we need to use the correct unique etc...
	-- so all we do is replace its provenance.  
	-- If it's not in the cache we put it there with the correct provenance.
	-- The idea is that, after all this, the cache
	-- will contain a Name with the correct Provenance (i.e. Local)

	-- OLD (now wrong) COMMENT:
	--   "Actually, there's a catch.  If this is the *second* binding for something
	--    we want to allocate a *fresh* unique, rather than using the same Name as before.
	--    Otherwise we don't detect conflicting definitions of the same top-level name!
	--    So the only time we re-use a Name already in the cache is when it's one of
	--    the Implicit magic-unique ones mentioned in the previous para"

	-- This (incorrect) patch doesn't work for record decls, when we have
	-- the same field declared in multiple constructors.   With the above patch,
	-- each occurrence got a new Name --- aargh!
	--
	-- So I reverted to the simple caching method (no "second-binding" thing)
	-- The multiple-local-binding case is now handled by improving the conflict
	-- detection in plusNameEnv.
    let
	provenance = LocalDef (rec_exp_fn new_name) loc
	(us', us1) = splitUniqSupply us
	uniq   	   = getUnique us1
        key        = (mod,occ)
	new_name   = case lookupFM cache key of
		         Just name -> setNameProvenance name provenance
		         other     -> mkGlobalName uniq mod occ provenance
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
    newGlobalName mod_name (rdrNameOcc n) HiFile {- Correct? -} 


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
bindLocatedLocalsRn :: (PprStyle -> Doc)		-- Documentation string for error message
	   	    -> [(RdrName,SrcLoc)]
	    	    -> ([Name] -> RnMS s a)
	    	    -> RnMS s a
bindLocatedLocalsRn doc_str rdr_names_w_loc enclosed_scope
  = checkDupOrQualNames doc_str rdr_names_w_loc	`thenRn_`

    getNameEnv			`thenRn` \ name_env ->
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
    setNameEnv new_name_env (enclosed_scope names)
  where
    check_shadow name_env (rdr_name,loc)
	= case lookupFM name_env rdr_name of
		Nothing   -> returnRn ()
		Just name -> pushSrcLocRn loc $
			     addWarnRn (shadowedNameWarn rdr_name)

bindLocalsRn doc_str rdr_names enclosed_scope
  = getSrcLocRn		`thenRn` \ loc ->
    bindLocatedLocalsRn (\_ -> text doc_str)
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
checkDupOrQualNames, checkDupNames :: (PprStyle -> Doc)
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
    (_, dups) = removeDups (\(n1,l1) (n2,l2) -> n1 `cmp` n2) rdr_names_w_loc


-- Yuk!
ifaceFlavour name = case getNameProvenance name of
			Imported _ _ hif -> hif
			Implicit hif     -> hif
			other		 -> HiFile	-- Shouldn't happen
\end{code}


%*********************************************************
%*							*
\subsection{Looking up names}
%*							*
%*********************************************************

Looking up a name in the RnEnv.

\begin{code}
lookupRn :: NameEnv -> RdrName -> RnMS s Name
lookupRn name_env rdr_name
  = case lookupFM name_env rdr_name of

	-- Found it!
	Just name -> returnRn name

	-- Not found
	Nothing -> getModeRn	`thenRn` \ mode ->
		   case mode of 
			-- Not found when processing source code; so fail
			SourceMode    -> failWithRn (mkUnboundName rdr_name)
					            (unknownNameErr rdr_name)
		
			-- Not found when processing an imported declaration,
			-- so we create a new name for the purpose
			InterfaceMode _ -> 
			    case rdr_name of

				Qual mod_name occ hif -> newGlobalName mod_name occ hif

				-- An Unqual is allowed; interface files contain 
				-- unqualified names for locally-defined things, such as
				-- constructors of a data type.
				Unqual occ -> getModuleRn 	`thenRn ` \ mod_name ->
					      newGlobalName mod_name occ HiFile


lookupBndrRn rdr_name
  = getNameEnv 			`thenRn` \ name_env ->
    lookupRn name_env rdr_name

-- Just like lookupRn except that we record the occurrence too
-- Perhaps surprisingly, even wired-in names are recorded.
-- Why?  So that we know which wired-in names are referred to when
-- deciding which instance declarations to import.
lookupOccRn :: RdrName -> RnMS s Name
lookupOccRn rdr_name
  = getNameEnv 			`thenRn` \ name_env ->
    lookupRn name_env rdr_name	`thenRn` \ name ->
    addOccurrenceName name

-- lookupGlobalOccRn is like lookupOccRn, except that it looks in the global 
-- environment.  It's used for record field names only.
lookupGlobalOccRn :: RdrName -> RnMS s Name
lookupGlobalOccRn rdr_name
  = getGlobalNameEnv		`thenRn` \ name_env ->
    lookupRn name_env rdr_name	`thenRn` \ name ->
    addOccurrenceName name

   

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
--
-- For List and Tuple types it's important to get the correct
-- isLocallyDefined flag, which is used in turn when deciding
-- whether there are any instance decls in this module are "special".
-- The name cache should have the correct provenance, though.

lookupImplicitOccRn :: RdrName -> RnMS s Name 
lookupImplicitOccRn (Qual mod occ hif)
 = newGlobalName mod occ hif		`thenRn` \ name ->
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



%************************************************************************
%*									*
\subsection{Envt utility functions}
%*									*
%************************************************************************

===============  RnEnv  ================
\begin{code}
plusRnEnv (RnEnv n1 f1) (RnEnv n2 f2) 
  = plusNameEnvRn n1 n2		`thenRn` \ n ->
    plusFixityEnvRn f1 f2	`thenRn` \ f -> 
    returnRn (RnEnv n f)
\end{code}

===============  NameEnv  ================
\begin{code}
plusNameEnvRn :: NameEnv -> NameEnv -> RnM s d NameEnv
plusNameEnvRn env1 env2
  = mapRn (addErrRn.nameClashErr) (conflictsFM conflicting_name env1 env2)		`thenRn_`
    returnRn (env1 `plusFM` env2)

addOneToNameEnv :: NameEnv -> RdrName -> Name -> RnM s d NameEnv
addOneToNameEnv env rdr_name name
 = case lookupFM env rdr_name of
	Just name2 | conflicting_name name name2
		   -> addErrRn (nameClashErr (rdr_name, (name, name2)))	`thenRn_`
		      returnRn env

	other      -> returnRn (addToFM env rdr_name name)

conflicting_name n1 n2 = (n1 /= n2) || 
			 (isLocallyDefinedName n1 && isLocallyDefinedName n2)
	-- We complain of a conflict if one RdrName maps to two different Names,
	-- OR if one RdrName maps to the same *locally-defined* Name.  The latter
	-- case is to catch two separate, local definitions of the same thing.
	--
	-- If a module imports itself then there might be a local defn and an imported
	-- defn of the same name; in this case the names will compare as equal, but
	-- will still have different provenances.

lookupNameEnv :: NameEnv -> RdrName -> Maybe Name
lookupNameEnv = lookupFM

delOneFromNameEnv :: NameEnv -> RdrName -> NameEnv 
delOneFromNameEnv env rdr_name = delFromFM env rdr_name
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

bad_fix :: (Fixity, Provenance) -> (Fixity, Provenance) -> Bool
bad_fix (f1,_) (f2,_) = f1 /= f2

pprFixityProvenance :: PprStyle -> (Fixity,Provenance) -> Doc
pprFixityProvenance sty (fixity, prov) = pprProvenance sty prov
\end{code}



===============  Avails  ================
\begin{code}
mkExportAvails :: Bool -> Module -> [AvailInfo] -> ExportAvails
mkExportAvails unqualified_import mod_name avails
  = (mod_avail_env, entity_avail_env)
  where
	-- The "module M" syntax only applies to *unqualified* imports (1.4 Report, Section 5.1.1)
    mod_avail_env | unqualified_import = unitFM mod_name avails 
		  | otherwise	       = emptyFM
   
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
plusAvail a1 a2 = panic ("RnEnv.plusAvail " ++ (show (hsep [pprAvail PprDebug a1,pprAvail PprDebug a2])))
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
  | otherwise    = pprTrace "filterAvail" (hsep [ppr PprDebug ie, pprAvail PprDebug avail]) $
		   NotAvailable
  where
    is_wanted name = nameOccName name `elem` wanted_occs
    sub_names_ok   = all (`elem` avail_occs) wanted_occs
    avail_occs	   = map nameOccName ns
    wanted_occs    = map rdrNameOcc (want:wants)

filterAvail (IEThingAbs _) (AvailTC n ns)      
  | n `elem` ns = AvailTC n [n]

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
pprAvail PprInterface avail = ppr_avail (pprOccName PprInterface . nameOccName) avail
pprAvail sty          avail = ppr_avail (ppr sty) avail

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
	   -> [(a,(b,b))]
conflictFM bad fm key elt
  = case lookupFM fm key of
	Just elt' | bad elt elt' -> [(key,(elt,elt'))]
	other			 -> []
\end{code}


%************************************************************************
%*									*
\subsection{Envt utility functions}
%*									*
%************************************************************************


\begin{code}
nameClashErr (rdr_name, (name1,name2)) sty
  = hang (hsep [ptext SLIT("Conflicting definitions for:"), ppr sty rdr_name])
	4 (vcat [pprNameProvenance sty name1,
		 pprNameProvenance sty name2])

fixityClashErr (rdr_name, (fp1,fp2)) sty
  = hang (hsep [ptext SLIT("Conflicting fixities for:"), ppr sty rdr_name])
	4 (vcat [pprFixityProvenance sty fp1,
		 pprFixityProvenance sty fp2])

shadowedNameWarn shadow sty
  = hcat [ptext SLIT("This binding for"), 
	       ppr sty shadow,
	       ptext SLIT("shadows an existing binding")]

unknownNameErr name sty
  = sep [text flavour, ptext SLIT("not in scope:"), ppr sty name]
  where
    flavour = occNameFlavour (rdrNameOcc name)

qualNameErr descriptor (name,loc)
  = pushSrcLocRn loc $
    addErrRn (\sty -> hsep [ ptext SLIT("Invalid use of qualified name"), 
			     ppr sty name,
			     ptext SLIT("in"),
			     descriptor sty])

dupNamesErr descriptor ((name,loc) : dup_things)
  = pushSrcLocRn loc $
    addErrRn (\sty -> hsep [ptext SLIT("Conflicting definitions for"), 
			    ppr sty name, 
			    ptext SLIT("in"), descriptor sty])
\end{code}

