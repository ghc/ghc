%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[RnEnv]{Environment manipulation for the renamer monad}

\begin{code}
#include "HsVersions.h"

module RnEnv where		-- Export everything

IMP_Ubiq()

import CmdLineOpts	( opt_WarnNameShadowing, opt_IgnoreIfacePragmas )
import HsSyn
import RdrHsSyn		( RdrName(..), SYN_IE(RdrNameIE),
			  rdrNameOcc, isQual, qual
			)
import HsTypes		( getTyVarName, replaceTyVarName )
import RnMonad
import Name		( Name, OccName(..), Provenance(..), DefnInfo(..), ExportFlag(..),
			  occNameString, occNameFlavour,
			  SYN_IE(NameSet), emptyNameSet, addListToNameSet,
			  mkLocalName, mkGlobalName, modAndOcc,
			  isLocalName, isWiredInName, nameOccName, setNameProvenance,
			  pprProvenance, pprOccName, pprModule, pprNonSymOcc, pprNameProvenance
			)
import TyCon		( TyCon )
import TysWiredIn	( tupleTyCon, listTyCon, charTyCon, intTyCon )
import FiniteMap
import Unique		( Unique, unboundKey )
import Maybes		( maybeToBool )
import UniqSupply
import SrcLoc		( SrcLoc, noSrcLoc )
import Pretty
import PprStyle		( PprStyle(..) )
import Util		( panic, removeDups, pprTrace, assertPanic )
\end{code}



%*********************************************************
%*							*
\subsection{Making new names}
%*							*
%*********************************************************

\begin{code}
newGlobalName :: Module -> OccName -> RnM s d Name
newGlobalName mod occ
  = 	-- First check the cache
    getNameSupplyRn		`thenRn` \ (us, inst_ns, cache) ->
    case lookupFM cache (mod,occ) of

	-- A hit in the cache!  Return it, but change the src loc
	-- of the thing we've found if this is a second definition site
	-- (that is, if loc /= NoSrcLoc)
	Just name ->  returnRn name

	-- Miss in the cache, so build a new original name,
	-- and put it in the cache
	Nothing        -> 
	    let
		(us', us1) = splitUniqSupply us
		uniq   	   = getUnique us1
		name       = mkGlobalName uniq mod occ VanillaDefn Implicit
		cache'     = addToFM cache (mod,occ) name
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
    let
	provenance = LocalDef (rec_exp_fn new_name) loc
	(us', us1) = splitUniqSupply us
	uniq   	   = getUnique us1
	new_name   = case lookupFM cache (mod,occ) of
			Just name -> setNameProvenance name provenance
			Nothing   -> mkGlobalName uniq mod occ VanillaDefn provenance
	cache'     = addToFM cache (mod,occ) new_name
    in
    setNameSupplyRn (us', inst_ns, cache')		`thenRn_`
    returnRn new_name

-- newDfunName is used to allocate a name for the dictionary function for
-- a local instance declaration.  No need to put it in the cache (I think!).
newDfunName ::  SrcLoc -> RnMS s Name
newDfunName src_loc
  = getNameSupplyRn		`thenRn` \ (us, inst_ns, cache) ->
    getModuleRn			`thenRn` \ mod_name ->
    let
	(us', us1) = splitUniqSupply us
	uniq	   = getUnique us1
	dfun_name  = mkGlobalName uniq mod_name (VarOcc (_PK_ ("df" ++ show inst_ns)))
				  VanillaDefn (LocalDef Exported src_loc)
   in
   setNameSupplyRn (us', inst_ns+1, cache)	`thenRn_`
   returnRn dfun_name


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
bindLocatedLocalsRn :: String		-- Documentation string for error message
	   	    -> [(RdrName,SrcLoc)]
	    	    -> ([Name] -> RnMS s a)
	    	    -> RnMS s a
bindLocatedLocalsRn doc_str rdr_names_w_loc enclosed_scope
  = 	-- Check for use of qualified names
    mapRn (qualNameErr doc_str) quals 	`thenRn_`
	-- Check for dupicated names in a binding group
    mapRn (dupNamesErr doc_str) dups  	`thenRn_`

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
    quals	  = filter (isQual.fst) rdr_names_w_loc
    (these, dups) = removeDups (\(n1,l1) (n2,l2) -> n1 `cmp` n2) rdr_names_w_loc
    check_shadow name_env (rdr_name,loc)
	= case lookupFM name_env rdr_name of
		Nothing   -> returnRn ()
		Just name -> pushSrcLocRn loc $
			     addWarnRn (shadowedNameWarn rdr_name)

bindLocalsRn doc_str rdr_names enclosed_scope
  = getSrcLocRn		`thenRn` \ loc ->
    bindLocatedLocalsRn doc_str (rdr_names `zip` repeat loc) enclosed_scope

bindTyVarsRn doc_str tyvar_names enclosed_scope
  = getSrcLocRn					`thenRn` \ loc ->
    let
	located_tyvars = [(getTyVarName tv, loc) | tv <- tyvar_names] 
    in
    bindLocatedLocalsRn doc_str located_tyvars	$ \ names ->
    enclosed_scope (zipWith replaceTyVarName tyvar_names names)
\end{code}


%*********************************************************
%*							*
\subsection{Looking up names}
%*							*
%*********************************************************

Looking up a name in the RnEnv.

\begin{code}
lookupRn :: RdrName -> RnMS s Name
lookupRn rdr_name
  = getNameEnv 		`thenRn` \ name_env ->
    case lookupFM name_env rdr_name of

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
			InterfaceMode -> 
			    case rdr_name of

				Qual mod_name occ -> newGlobalName mod_name occ

				-- An Unqual is allowed; interface files contain 
				-- unqualified names for locally-defined things, such as
				-- constructors of a data type.
				Unqual occ -> getModuleRn 	`thenRn ` \ mod_name ->
					      newGlobalName mod_name occ


-- Just like lookupRn except that we record the occurrence too
-- Perhaps surprisingly, even wired-in names are recorded.
-- Why?  So that we know which wired-in names are referred to when
-- deciding which instance declarations to import.
lookupOccRn :: RdrName -> RnMS s Name
lookupOccRn rdr_name
  = lookupRn rdr_name	`thenRn` \ name ->
    if isLocalName name then
	returnRn name
    else
	addOccurrenceName Compulsory name		`thenRn_`
 	returnRn name

-- lookupOptionalOccRn is similar, but it's used in places where
-- we don't *have* to find a definition for the thing.
lookupOptionalOccRn :: RdrName -> RnMS s Name
lookupOptionalOccRn rdr_name
  = lookupRn rdr_name	`thenRn` \ name ->
    if opt_IgnoreIfacePragmas || isLocalName name then
    		-- Never look for optional things if we're
		-- ignoring optional input interface information
	returnRn name
    else
	addOccurrenceName Optional name		`thenRn_`
 	returnRn name

-- lookupImplicitOccRn takes an RdrName representing an *original* name, and
-- adds it to the occurrence pool so that it'll be loaded later.  This is
-- used when language constructs (such as monad comprehensions, overloaded literals,
-- or deriving clauses) require some stuff to be loaded that isn't explicitly
-- mentioned in the code.
--
-- This doesn't apply in interface mode, where everything is explicit, but
-- we don't check for this case: it does no harm to record an "extra" occurrence
-- and lookupImplicitOccRn isn't used much in interface mode (it's only the
-- Nothing clause of rnDerivs that calls it at all I think.
--
-- For List and Tuple types it's important to get the correct
-- isLocallyDefined flag, which is used in turn when deciding
-- whether there are any instance decls in this module are "special".
-- The name cache should have the correct provenance, though.

lookupImplicitOccRn :: RdrName -> RnMS s Name 
lookupImplicitOccRn (Qual mod occ)
 = newGlobalName mod occ		`thenRn` \ name ->
   addOccurrenceName Compulsory name	`thenRn_`
   returnRn name

addImplicitOccRn :: Name -> RnM s d ()
addImplicitOccRn name = addOccurrenceName Compulsory name

addImplicitOccsRn :: [Name] -> RnM s d ()
addImplicitOccsRn names = addOccurrenceNames Compulsory names

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
plusNameEnvRn n1 n2
  = mapRn (addErrRn.nameClashErr) (conflictsFM (/=) n1 n2)		`thenRn_`
    returnRn (n1 `plusFM` n2)

addOneToNameEnvRn :: NameEnv -> RdrName -> Name -> RnM s d NameEnv
addOneToNameEnvRn env rdr_name name
  = mapRn (addErrRn.nameClashErr) (conflictFM (/=) env rdr_name name)	`thenRn_`
    returnRn (addToFM env rdr_name name)

lookupNameEnv :: NameEnv -> RdrName -> Maybe Name
lookupNameEnv = lookupFM
\end{code}

===============  FixityEnv  ================
\begin{code}
plusFixityEnvRn f1 f2
  = mapRn (addErrRn.fixityClashErr) (conflictsFM bad_fix f1 f2)		`thenRn_`
    returnRn (f1 `plusFM` f2)

addOneToFixityEnvRn env rdr_name fixity
  = mapRn (addErrRn.fixityClashErr) (conflictFM bad_fix env rdr_name fixity)	`thenRn_`
    returnRn (addToFM env rdr_name fixity)

lookupFixityEnv env rdr_name 
  = case lookupFM env rdr_name of
	Just (fixity,_) -> fixity
	Nothing	        -> Fixity 9 InfixL 		-- Default case

bad_fix :: (Fixity, Provenance) -> (Fixity, Provenance) -> Bool
bad_fix (f1,_) (f2,_) = f1 /= f2

pprFixityProvenance :: PprStyle -> (Fixity,Provenance) -> Pretty
pprFixityProvenance sty (fixity, prov) = pprProvenance sty prov
\end{code}



===============  Avails  ================
\begin{code}
emptyModuleAvails :: ModuleAvails
plusModuleAvails ::  ModuleAvails ->  ModuleAvails ->  ModuleAvails
lookupModuleAvails :: ModuleAvails -> Module -> Maybe [AvailInfo]

emptyModuleAvails = emptyFM
plusModuleAvails  = plusFM_C (++)
lookupModuleAvails = lookupFM
\end{code}


===============  AvailInfo  ================
\begin{code}
plusAvail (Avail n1 ns1) (Avail n2 ns2) = Avail n1 (nub (ns1 ++ ns2))
plusAvail a NotAvailable = a
plusAvail NotAvailable a = a

addAvailToNameSet :: NameSet -> AvailInfo -> NameSet
addAvailToNameSet names NotAvailable = names
addAvailToNameSet names (Avail n ns) = addListToNameSet names (n:ns)

availsToNameSet :: [AvailInfo] -> NameSet
availsToNameSet avails = foldl addAvailToNameSet emptyNameSet avails

availNames :: AvailInfo -> [Name]
availNames NotAvailable      = []
availNames (Avail n ns) = n:ns

filterAvail :: RdrNameIE -> AvailInfo -> AvailInfo
filterAvail (IEThingWith _ wanted) NotAvailable = NotAvailable
filterAvail (IEThingWith _ wanted) (Avail n ns)
  | sub_names_ok = Avail n (filter is_wanted ns)
  | otherwise	 = NotAvailable
  where
    is_wanted name = nameOccName name `elem` wanted_occs
    sub_names_ok   = all (`elem` avail_occs) wanted_occs
    wanted_occs    = map rdrNameOcc wanted
    avail_occs	   = map nameOccName ns


filterAvail (IEThingAll _) avail        = avail
filterAvail ie		   (Avail n ns) = Avail n []		-- IEThingAbs and IEVar

-- pprAvail gets given the OccName of the "host" thing
pprAvail sty NotAvailable = ppStr "NotAvailable"
pprAvail sty (Avail n ns) = ppCat [pprOccName sty (nameOccName n),
				   ppStr "(",
				   ppInterleave ppComma (map (pprOccName sty.nameOccName) ns),
				   ppStr ")"]
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
  = ppHang (ppCat [ppStr "Conflicting definitions for: ", ppr sty rdr_name])
	4 (ppAboves [pprNameProvenance sty name1,
		     pprNameProvenance sty name2])

fixityClashErr (rdr_name, (fp1,fp2)) sty
  = ppHang (ppCat [ppStr "Conflicting fixities for: ", ppr sty rdr_name])
	4 (ppAboves [pprFixityProvenance sty fp1,
		     pprFixityProvenance sty fp2])

shadowedNameWarn shadow sty
  = ppBesides [ppStr "More than one value with the same name (shadowing): ", ppr sty shadow]

unknownNameErr name sty
  = ppSep [ppStr flavour, ppStr "not in scope:", ppr sty name]
  where
    flavour = occNameFlavour (rdrNameOcc name)

qualNameErr descriptor (name,loc)
  = pushSrcLocRn loc $
    addErrRn (\sty -> ppBesides [ppStr "invalid use of qualified ", 
				 ppStr descriptor, ppStr ": ", 
				 pprNonSymOcc sty (rdrNameOcc name) ])

dupNamesErr descriptor ((name,loc) : dup_things)
  = pushSrcLocRn loc $
    addErrRn (\sty -> ppBesides [ppStr "duplicate bindings of `", 
				 ppr sty name, ppStr "' in ", 
				 ppStr descriptor])
\end{code}

