%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[RnIfaces]{Cacheing and Renaming of Interfaces}

\begin{code}
#include "HsVersions.h"

module RnIfaces (
	getInterfaceExports,
	getImportedInstDecls,
	getSpecialInstModules,
	importDecl, recordSlurp,
	getImportVersions, 

	checkUpToDate,

	getDeclBinders,
	mkSearchPath
    ) where

IMP_Ubiq()


import CmdLineOpts	( opt_HiSuffix, opt_HiSuffixPrelude )
import HsSyn		( HsDecl(..), TyDecl(..), ClassDecl(..), HsTyVar, Bind, HsExpr, Sig(..), HsType(..),
			  HsBinds(..), MonoBinds, DefaultDecl, ConDecl(..), BangType, IfaceSig(..),
			  FixityDecl(..), Fixity, Fake, InPat, InstDecl(..), SYN_IE(Version), HsIdInfo,
			  IE(..)
			)
import HsPragmas	( noGenPragmas )
import RdrHsSyn		( SYN_IE(RdrNameHsDecl), SYN_IE(RdrNameInstDecl), 
			  RdrName, rdrNameOcc
			)
import RnEnv		( newGlobalName, lookupRn, addImplicitOccsRn, 
			  availName, availNames, addAvailToNameSet, pprAvail
			)
import RnSource		( rnHsType )
import RnMonad
import ParseIface	( parseIface )

import ErrUtils		( SYN_IE(Error), SYN_IE(Warning) )
import FiniteMap	( FiniteMap, emptyFM, unitFM, lookupFM, addToFM, addToFM_C, addListToFM, fmToList )
import Name		( Name {-instance NamedThing-}, Provenance, OccName(..),
			  modAndOcc, occNameString, moduleString, pprModule,
			  NameSet(..), emptyNameSet, unionNameSets, nameSetToList,
			  minusNameSet, mkNameSet, elemNameSet,
			  isWiredInName, maybeWiredInTyConName, maybeWiredInIdName
			 )
import Id		( GenId, Id(..), idType, dataConTyCon, isDataCon )
import TyCon		( TyCon, tyConDataCons, isSynTyCon, getSynTyConDefn )
import Type		( namesOfType )
import TyVar		( GenTyVar )
import SrcLoc		( mkIfaceSrcLoc )
import PrelMods		( gHC__, isPreludeModule )
import Bag
import Maybes		( MaybeErr(..), expectJust, maybeToBool )
import ListSetOps	( unionLists )
import Pretty
import PprStyle		( PprStyle(..) )
import Util		( pprPanic, pprTrace )
import StringBuffer     ( StringBuffer, hGetStringBuffer, freeStringBuffer )

\end{code}



%*********************************************************
%*							*
\subsection{Loading a new interface file}
%*							*
%*********************************************************

\begin{code}
loadInterface :: Pretty -> Module -> RnMG Ifaces
loadInterface doc_str load_mod 
  = getIfacesRn 		`thenRn` \ ifaces ->
    let
	Ifaces this_mod mod_vers_map export_envs decls all_names imp_names insts inst_mods = ifaces
    in
	-- CHECK WHETHER WE HAVE IT ALREADY
    if maybeToBool (lookupFM export_envs load_mod) 
    then
	returnRn ifaces		-- Already in the cache; don't re-read it
    else

	-- READ THE MODULE IN
    findAndReadIface doc_str load_mod		`thenRn` \ read_result ->
    case read_result of {
	-- Check for not found
	Nothing -> 	-- Not found, so add an empty export env to the Ifaces map
			-- so that we don't look again
		   let
			new_export_envs = addToFM export_envs load_mod ([],[])
			new_ifaces = Ifaces this_mod mod_vers_map
					    new_export_envs
					    decls all_names imp_names insts inst_mods
		   in
		   setIfacesRn new_ifaces		`thenRn_`
		   failWithRn new_ifaces (noIfaceErr load_mod) ;

	-- Found and parsed!
	Just (ParsedIface _ mod_vers usages exports rd_inst_mods fixs rd_decls rd_insts) ->

	-- LOAD IT INTO Ifaces
    mapRn loadExport exports				`thenRn` \ avails_s ->
    foldlRn (loadDecl load_mod) decls rd_decls		`thenRn` \ new_decls ->
    foldlRn (loadInstDecl load_mod) insts rd_insts	`thenRn` \ new_insts ->
    let
	 export_env = (concat avails_s, fixs)

			-- Exclude this module from the "special-inst" modules
	 new_inst_mods = inst_mods `unionLists` (filter (/= this_mod) rd_inst_mods)

	 new_ifaces = Ifaces this_mod
			     (addToFM mod_vers_map load_mod mod_vers)
			     (addToFM export_envs load_mod export_env)
			     new_decls
			     all_names imp_names
			     new_insts
			     new_inst_mods 
    in
    setIfacesRn new_ifaces		`thenRn_`
    returnRn new_ifaces
    }

loadExport :: ExportItem -> RnMG [AvailInfo]
loadExport (mod, entities)
  = mapRn load_entity entities
  where
    new_name occ = newGlobalName mod occ

-- The communcation between this little code fragment and the "entity" rule
-- in ParseIface.y is a bit gruesome.  The idea is that things which are
-- destined to be AvailTCs show up as (occ, [non-empty-list]), whereas
-- things destined to be Avails show up as (occ, [])

    load_entity (occ, occs)
      =	new_name occ 		`thenRn` \ name ->
	if null occs then
		returnRn (Avail name)
	else
	        mapRn new_name occs 	`thenRn` \ names ->
	        returnRn (AvailTC name names)

loadDecl :: Module -> DeclsMap
	 -> (Version, RdrNameHsDecl)
	 -> RnMG DeclsMap
loadDecl mod decls_map (version, decl)
  = getDeclBinders new_implicit_name decl	`thenRn` \ avail ->
    returnRn (addListToFM decls_map
			  [(name,(version,avail,decl)) | name <- availNames avail]
    )
  where
    new_implicit_name rdr_name loc = newGlobalName mod (rdrNameOcc rdr_name)

loadInstDecl :: Module
	     -> Bag IfaceInst
	     -> RdrNameInstDecl
	     -> RnMG (Bag IfaceInst)
loadInstDecl mod_name insts decl@(InstDecl inst_ty binds uprags dfun_name src_loc)
  = 
	-- Find out what type constructors and classes are "gates" for the
	-- instance declaration.  If all these "gates" are slurped in then
	-- we should slurp the instance decl too.
	-- 
	-- We *don't* want to count names in the context part as gates, though.
	-- For example:
	--		instance Foo a => Baz (T a) where ...
	--
	-- Here the gates are Baz and T, but *not* Foo.
    let 
	munged_inst_ty = case inst_ty of
				HsForAllTy tvs cxt ty -> HsForAllTy tvs [] ty
				HsPreForAllTy cxt ty  -> HsPreForAllTy [] ty
				other		      -> inst_ty
    in
	-- We find the gates by renaming the instance type with in a 
	-- and returning the occurrence pool.
    initRnMS emptyRnEnv mod_name InterfaceMode (
        findOccurrencesRn (rnHsType munged_inst_ty)	
    )						`thenRn` \ gate_names ->
    returnRn (((mod_name, decl), gate_names) `consBag` insts)
\end{code}


%********************************************************
%*							*
\subsection{Loading usage information}
%*							*
%********************************************************

\begin{code}
checkUpToDate :: Module -> RnMG Bool		-- True <=> no need to recompile
checkUpToDate mod_name
  = findAndReadIface doc_str mod_name		`thenRn` \ read_result ->
    case read_result of
	Nothing -> 	-- Old interface file not found, so we'd better bail out
		    traceRn (ppSep [ppPStr SLIT("Didnt find old iface"), 
				    pprModule PprDebug mod_name])	`thenRn_`
		    returnRn False

	Just (ParsedIface _ _ usages _ _ _ _ _) 
		-> 	-- Found it, so now check it
		    checkModUsage usages
  where
	-- Only look in current directory, with suffix .hi
    doc_str = ppSep [ppPStr SLIT("Need usage info from"), pprModule PprDebug mod_name]


checkModUsage [] = returnRn True		-- Yes!  Everything is up to date!

checkModUsage ((mod, old_mod_vers, old_local_vers) : rest)
  = loadInterface doc_str mod		`thenRn` \ ifaces ->
    let
	Ifaces _ mod_vers _ decls _ _ _ _ = ifaces
	maybe_new_mod_vers = lookupFM mod_vers mod
	Just new_mod_vers  = maybe_new_mod_vers
    in
	-- If we can't find a version number for the old module then
	-- bail out saying things aren't up to date
    if not (maybeToBool maybe_new_mod_vers) then
	returnRn False
    else

	-- If the module version hasn't changed, just move on
    if new_mod_vers == old_mod_vers then
	traceRn (ppSep [ppPStr SLIT("Module version unchanged:"), pprModule PprDebug mod])	`thenRn_`
	checkModUsage rest
    else
    traceRn (ppSep [ppPStr SLIT("Module version has changed:"), pprModule PprDebug mod])	`thenRn_`

	-- New module version, so check entities inside
    checkEntityUsage mod decls old_local_vers	`thenRn` \ up_to_date ->
    if up_to_date then
	traceRn (ppPStr SLIT("...but the bits I use haven't."))	`thenRn_`
	checkModUsage rest	-- This one's ok, so check the rest
    else
	returnRn False		-- This one failed, so just bail out now
  where
    doc_str = ppSep [ppPStr SLIT("need version info for"), pprModule PprDebug mod]


checkEntityUsage mod decls [] 
  = returnRn True	-- Yes!  All up to date!

checkEntityUsage mod decls ((occ_name,old_vers) : rest)
  = newGlobalName mod occ_name		`thenRn` \ name ->
    case lookupFM decls name of

	Nothing       -> 	-- We used it before, but it ain't there now
			  traceRn (ppSep [ppPStr SLIT("...and this no longer exported:"), ppr PprDebug name])	`thenRn_`
			  returnRn False

	Just (new_vers,_,_) 	-- It's there, but is it up to date?
		| new_vers == old_vers
			-- Up to date, so check the rest
		-> checkEntityUsage mod decls rest

		| otherwise
			-- Out of date, so bale out
		-> traceRn (ppSep [ppPStr SLIT("...and this is out of date:"), ppr PprDebug name])  `thenRn_`
		   returnRn False
\end{code}


%*********************************************************
%*							*
\subsection{Getting in a declaration}
%*							*
%*********************************************************

\begin{code}
importDecl :: Name -> Necessity -> RnMG (Maybe RdrNameHsDecl)
	-- Returns Nothing for a wired-in or already-slurped decl

importDecl name necessity
  = checkSlurped name			`thenRn` \ already_slurped ->
    if already_slurped then
	-- traceRn (ppSep [ppStr "Already slurped:", ppr PprDebug name])	`thenRn_`
	returnRn Nothing	-- Already dealt with
    else
    if isWiredInName name then
	getWiredInDecl name
    else 
       getIfacesRn 		`thenRn` \ ifaces ->
       let
         Ifaces this_mod _ _ _ _ _ _ _ = ifaces
         (mod,_) = modAndOcc name
       in
       if mod == this_mod  then    -- Don't bring in decls from
	  pprTrace "importDecl wierdness:" (ppr PprDebug name) $
	  returnRn Nothing         -- the renamed module's own interface file
			           -- 
       else
	getNonWiredInDecl name necessity

\end{code}

\begin{code}
getNonWiredInDecl :: Name -> Necessity -> RnMG (Maybe RdrNameHsDecl)
getNonWiredInDecl name necessity
  = traceRn doc_str 			`thenRn_`
    loadInterface doc_str mod		`thenRn` \ (Ifaces _ _ _ decls _ _ _ _) ->
    case lookupFM decls name of

      Just (version,avail,decl) -> recordSlurp (Just version) avail	`thenRn_`
				   returnRn (Just decl)

      Nothing -> 	-- Can happen legitimately for "Optional" occurrences
		   case necessity of { 
				Optional -> addWarnRn (getDeclWarn name);
				other	 -> addErrRn  (getDeclErr  name)
		   }						`thenRn_` 
		   returnRn Nothing
  where
     doc_str = ppSep [ppPStr SLIT("Need decl for"), ppr PprDebug name]
     (mod,_) = modAndOcc name
\end{code}

@getWiredInDecl@ maps a wired-in @Name@ to what it makes available.
It behaves exactly as if the wired in decl were actually in an interface file.
Specifically,

  *	if the wired-in name is a data type constructor or a data constructor, 
	it brings in the type constructor and all the data constructors; and
	marks as "occurrences" any free vars of the data con.

  * 	similarly for synonum type constructor

  * 	if the wired-in name is another wired-in Id, it marks as "occurrences"
	the free vars of the Id's type.

  *	it loads the interface file for the wired-in thing for the
	sole purpose of making sure that its instance declarations are available

All this is necessary so that we know all types that are "in play", so
that we know just what instances to bring into scope.
	
\begin{code}
getWiredInDecl name
  = get_wired				`thenRn` \ avail ->
    recordSlurp Nothing avail		`thenRn_`

   	-- Force in the home module in case it has instance decls for
	-- the thing we are interested in.
	--
	-- Mini hack 1: no point for non-tycons/class; and if we
	-- do this we find PrelNum trying to import PackedString,
	-- because PrelBase's .hi file mentions PackedString.unpackString
	-- But PackedString.hi isn't built by that point!
	--
	-- Mini hack 2; GHC is guaranteed not to have
	-- instance decls, so it's a waste of time to read it
	--
	-- NB: We *must* look at the availName of the slurped avail, 
	-- not the name passed to getWiredInDecl!  Why?  Because if a data constructor 
	-- or class op is passed to getWiredInDecl we'll pull in the whole data/class
	-- decl, and recordSlurp will record that fact.  But since the data constructor
	-- isn't a tycon/class we won't force in the home module.  And even if the
	-- type constructor/class comes along later, loadDecl will say that it's already
	-- been slurped, so getWiredInDecl won't even be called.  Pretty obscure bug, this was.
    let
	main_name  = availName avail
	main_is_tc = case avail of { AvailTC _ _ -> True; Avail _ -> False }
	(mod,_)    = modAndOcc main_name
	doc_str    = ppSep [ppPStr SLIT("Need home module for wired in thing"), ppr PprDebug name]
    in
    (if not main_is_tc || mod == gHC__ then
	returnRn ()		
    else
	loadInterface doc_str mod	`thenRn_`
	returnRn ()
    )				 	`thenRn_`

    returnRn Nothing		-- No declaration to process further
  where

    get_wired | is_tycon			-- ... a type constructor
	      = get_wired_tycon the_tycon

	      | (isDataCon the_id) 		-- ... a wired-in data constructor
	      = get_wired_tycon (dataConTyCon the_id)

	      | otherwise			-- ... a wired-in non data-constructor
	      = get_wired_id the_id

    maybe_wired_in_tycon = maybeWiredInTyConName name
    is_tycon		 = maybeToBool maybe_wired_in_tycon
    maybe_wired_in_id    = maybeWiredInIdName    name
    Just the_tycon	 = maybe_wired_in_tycon
    Just the_id 	 = maybe_wired_in_id


get_wired_id id
  = addImplicitOccsRn (nameSetToList id_mentioned)	`thenRn_`
    returnRn (Avail (getName id))
  where
    id_mentioned = namesOfType (idType id)

get_wired_tycon tycon 
  | isSynTyCon tycon
  = addImplicitOccsRn (nameSetToList mentioned)		`thenRn_`
    returnRn (Avail (getName tycon))
  where
    (tyvars,ty) = getSynTyConDefn tycon
    mentioned = namesOfType ty `minusNameSet` mkNameSet (map getName tyvars)

get_wired_tycon tycon 
  | otherwise		-- data or newtype
  = addImplicitOccsRn (nameSetToList mentioned)		`thenRn_`
    returnRn (AvailTC tycon_name (tycon_name : map getName data_cons))
  where
    tycon_name = getName tycon
    data_cons  = tyConDataCons tycon
    mentioned  = foldr (unionNameSets . namesOfType . idType) emptyNameSet data_cons
\end{code}


\begin{code}
checkSlurped name
  = getIfacesRn 	`thenRn` \ (Ifaces _ _ _ _ slurped_names _ _ _) ->
    returnRn (name `elemNameSet` slurped_names)

recordSlurp maybe_version avail
  = -- traceRn (ppSep [ppStr "Record slurp:", pprAvail PprDebug avail])	`thenRn_`
    getIfacesRn 	`thenRn` \ ifaces ->
    let
	Ifaces this_mod mod_vers export_envs decls slurped_names imp_names insts inst_mods = ifaces
	new_slurped_names = addAvailToNameSet slurped_names avail

	new_imp_names = case maybe_version of
			   Just version -> (availName avail, version) : imp_names
			   Nothing      -> imp_names

	new_ifaces = Ifaces this_mod mod_vers export_envs decls 
			    new_slurped_names 
			    new_imp_names
			    insts
			    inst_mods
    in
    setIfacesRn new_ifaces
\end{code}
    
%*********************************************************
%*							*
\subsection{Getting other stuff}
%*							*
%*********************************************************

\begin{code}
getInterfaceExports :: Module -> RnMG (Avails, [(OccName,Fixity)])
getInterfaceExports mod
  = loadInterface doc_str mod		`thenRn` \ (Ifaces _ _ export_envs _ _ _ _ _) ->
    case lookupFM export_envs mod of
	Nothing ->	-- Not there; it must be that the interface file wasn't found;
			-- the error will have been reported already.
			-- (Actually loadInterface should put the empty export env in there
			--  anyway, but this does no harm.)
		      returnRn ([],[])

	Just stuff -> returnRn stuff
  where
    doc_str = ppSep [pprModule PprDebug mod, ppPStr SLIT("is directly imported")]


getImportedInstDecls :: RnMG [(Module,RdrNameInstDecl)]
getImportedInstDecls
  = 	-- First load any special-instance modules that aren't aready loaded
    getSpecialInstModules 			`thenRn` \ inst_mods ->
    mapRn load_it inst_mods			`thenRn_`

	-- Now we're ready to grab the instance declarations
	-- Find the un-gated ones and return them, 
	-- removing them from the bag kept in Ifaces
    getIfacesRn 	`thenRn` \ ifaces ->
    let
	Ifaces this_mod mod_vers export_envs decls slurped_names imp_names insts inst_mods = ifaces

		-- An instance decl is ungated if all its gates have been slurped
        select_ungated :: IfaceInst					-- A gated inst decl

		       -> ([(Module, RdrNameInstDecl)], [IfaceInst])	-- Accumulator

		       -> ([(Module, RdrNameInstDecl)], 		-- The ungated ones
			   [IfaceInst]) 				-- Still gated, but with
									-- depeleted gates
	select_ungated (decl,gates) (ungated_decls, gated_decls)
	  | null remaining_gates
	  = (decl : ungated_decls, gated_decls)
	  | otherwise
	  = (ungated_decls, (decl, remaining_gates) : gated_decls)
	  where
	    remaining_gates = filter (not . (`elemNameSet` slurped_names)) gates

	(un_gated_insts, still_gated_insts) = foldrBag select_ungated ([], []) insts
	
	new_ifaces = Ifaces this_mod mod_vers export_envs decls slurped_names imp_names
			    (listToBag still_gated_insts)
			    inst_mods
    in
    setIfacesRn new_ifaces	`thenRn_`
    returnRn un_gated_insts
  where
    load_it mod = loadInterface (doc_str mod) mod
    doc_str mod = ppSep [pprModule PprDebug mod, ppPStr SLIT("is a special-instance module")]


getSpecialInstModules :: RnMG [Module]
getSpecialInstModules 
  = getIfacesRn						`thenRn` \ ifaces ->
    let
	 Ifaces _ _ _ _ _ _ _ inst_mods = ifaces
    in
    returnRn inst_mods
\end{code}

getImportVersions figures out what the "usage information" for this moudule is;
that is, what it must record in its interface file as the things it uses.
It records:
	- anything reachable from its body code
	- any module exported with a "module Foo".

Why the latter?  Because if Foo changes then this module's export list
will change, so we must recompile this module at least as far as
making a new interface file --- but in practice that means complete
recompilation.

What about this? 
	module A( f, g ) where		module B( f ) where
	  import B( f )			  f = h 3
	  g = ...			  h = ...

Should we record B.f in A's usages?  In fact we don't.  Certainly, if
anything about B.f changes than anyone who imports A should be recompiled;
they'll get an early exit if they don't use B.f.  However, even if B.f
doesn't change at all, B.h may do so, and this change may not be reflected
in f's version number.  So there are two things going on when compiling module A:

1.  Are A.o and A.hi correct?  Then we can bale out early.
2.  Should modules that import A be recompiled?

For (1) it is slightly harmful to record B.f in A's usages, because a change in
B.f's version will provoke full recompilation of A, producing an identical A.o,
and A.hi differing only in its usage-version of B.f (which isn't used by any importer).

For (2), because of the tricky B.h question above, we ensure that A.hi is touched
(even if identical to its previous version) if A's recompilation was triggered by
an imported .hi file date change.  Given that, there's no need to record B.f in
A's usages.

On the other hand, if A exports "module B" then we *do* count module B among
A's usages, because we must recompile A to ensure that A.hi changes appropriately.

\begin{code}
getImportVersions :: Module			-- Name of this module
		  -> Maybe [IE any]		-- Export list for this module
		  -> RnMG (VersionInfo Name)	-- Version info for these names

getImportVersions this_mod exports
  = getIfacesRn					`thenRn` \ ifaces ->
    let
	 Ifaces _ mod_versions_map _ _ _ imp_names _ _ = ifaces
	 mod_version mod = expectJust "import_versions" (lookupFM mod_versions_map mod)

	 -- mv_map groups together all the things imported from a particular module.
	 mv_map, mv_map_mod :: FiniteMap Module [LocalVersion Name]

	 mv_map_mod = foldl add_mod emptyFM export_mods
		-- mv_map_mod records all the modules that have a "module M"
		-- in this module's export list

	 mv_map = foldl add_mv mv_map_mod imp_names
		-- mv_map adds the version numbers of things exported individually
    in
    returnRn [ (mod, mod_version mod, local_versions)
	     | (mod, local_versions) <- fmToList mv_map
	     ]

  where
     export_mods = case exports of
			Nothing -> []
			Just es -> [mod | IEModuleContents mod <- es, mod /= this_mod]

     add_mv mv_map v@(name, version) 
      = addToFM_C (\ ls _ -> (v:ls)) mv_map mod [v] 
	where
	 (mod,_) = modAndOcc name

     add_mod mv_map mod = addToFM mv_map mod []
\end{code}

%*********************************************************
%*							*
\subsection{Getting binders out of a declaration}
%*							*
%*********************************************************

@getDeclBinders@ returns the names for a @RdrNameHsDecl@.
It's used for both source code (from @availsFromDecl@) and interface files
(from @loadDecl@).

It doesn't deal with source-code specific things: ValD, DefD.  They
are handled by the sourc-code specific stuff in RnNames.

\begin{code}
getDeclBinders :: (RdrName -> SrcLoc -> RnMG Name)		-- New-name function
		-> RdrNameHsDecl
		-> RnMG AvailInfo

getDeclBinders new_name (TyD (TyData _ tycon _ condecls _ _ src_loc))
  = new_name tycon src_loc			`thenRn` \ tycon_name ->
    getConFieldNames new_name condecls		`thenRn` \ sub_names ->
    returnRn (AvailTC tycon_name (tycon_name : sub_names))

getDeclBinders new_name (TyD (TyNew _ tycon _ (NewConDecl con _ con_loc) _ _ src_loc))
  = new_name tycon src_loc		`thenRn` \ tycon_name ->
    new_name con src_loc		`thenRn` \ con_name ->
    returnRn (AvailTC tycon_name [tycon_name, con_name])

getDeclBinders new_name (TyD (TySynonym tycon _ _ src_loc))
  = new_name tycon src_loc		`thenRn` \ tycon_name ->
    returnRn (Avail tycon_name)

getDeclBinders new_name (ClD (ClassDecl _ cname _ sigs _ _ src_loc))
  = new_name cname src_loc			`thenRn` \ class_name ->
    mapRn (getClassOpNames new_name) sigs	`thenRn` \ sub_names ->
    returnRn (AvailTC class_name (class_name : sub_names))

getDeclBinders new_name (SigD (IfaceSig var ty prags src_loc))
  = new_name var src_loc			`thenRn` \ var_name ->
    returnRn (Avail var_name)

getDeclBinders new_name (DefD _)  = returnRn NotAvailable
getDeclBinders new_name (InstD _) = returnRn NotAvailable

----------------
getConFieldNames new_name (ConDecl con _ src_loc : rest)
  = new_name con src_loc		`thenRn` \ n ->
    getConFieldNames new_name rest	`thenRn` \ ns -> 
    returnRn (n:ns)

getConFieldNames new_name (NewConDecl con _ src_loc : rest)
  = new_name con src_loc		`thenRn` \ n ->
    getConFieldNames new_name rest	`thenRn` \ ns -> 
    returnRn (n:ns)

getConFieldNames new_name (ConOpDecl _ con _ src_loc : rest)
  = new_name con src_loc		`thenRn` \ n ->
    getConFieldNames new_name rest	`thenRn` \ ns -> 
    returnRn (n:ns)

getConFieldNames new_name (RecConDecl con fielddecls src_loc : rest)
  = mapRn (\n -> new_name n src_loc) (con:fields)	`thenRn` \ cfs ->
    getConFieldNames new_name rest			`thenRn` \ ns  -> 
    returnRn (cfs ++ ns)
  where
    fields = concat (map fst fielddecls)

getConFieldNames new_name [] = returnRn []

getClassOpNames new_name (ClassOpSig op _ _ src_loc) = new_name op src_loc
\end{code}


%*********************************************************
%*							*
\subsection{Reading an interface file}
%*							*
%*********************************************************

\begin{code}
findAndReadIface :: Pretty -> Module -> RnMG (Maybe ParsedIface)
	-- Nothing <=> file not found, or unreadable, or illegible
	-- Just x  <=> successfully found and parsed 
findAndReadIface doc_str mod
  = traceRn trace_msg			`thenRn_`
    getSearchPathRn			`thenRn` \ dirs ->
    try dirs dirs
  where
    trace_msg = ppHang (ppBesides [ppPStr SLIT("Reading interface for "), 
				   pprModule PprDebug mod, ppSemi])
		     4 (ppBesides [ppPStr SLIT("reason: "), doc_str])

    mod_str = moduleString mod
    hisuf =
      if isPreludeModule mod then
         case opt_HiSuffixPrelude of { Just hisuf -> hisuf; _ -> ".hi"}
      else
         case opt_HiSuffix of {Just hisuf -> hisuf; _ -> ".hi"}

    try all_dirs [] = traceRn (ppPStr SLIT("...failed"))	`thenRn_`
		      returnRn Nothing

    try all_dirs (dir:dirs)
	= readIface file_path	`thenRn` \ read_result ->
	  case read_result of
		Nothing    -> try all_dirs dirs
		Just iface -> traceRn (ppPStr SLIT("...done"))	`thenRn_`
			      returnRn (Just iface)
	where
	  file_path = dir ++ "/" ++ moduleString mod ++ hisuf
\end{code}

@readIface@ trys just one file.

\begin{code}
readIface :: String -> RnMG (Maybe ParsedIface)	
	-- Nothing <=> file not found, or unreadable, or illegible
	-- Just x  <=> successfully found and parsed 
readIface file_path
  = ioToRnMG (hGetStringBuffer file_path)  	`thenRn` \ read_result ->
--OLD:  = ioToRnMG (readFile file_path)  	`thenRn` \ read_result ->
    case read_result of
	Right contents	  -> case parseIface contents of
				Failed err      -> --ioToRnMG (freeStringBuffer contents) `thenRn` \ _ -> 
					           failWithRn Nothing err 
				Succeeded iface -> --ioToRnMG (freeStringBuffer contents) `thenRn` \ _ ->
						   returnRn (Just iface)

	Left  (NoSuchThing _) -> returnRn Nothing

	Left  err	      -> failWithRn Nothing
					    (cannaeReadFile file_path err)

\end{code}

mkSearchPath takes a string consisting of a colon-separated list of directories, and turns it into
a list of directories.  For example:

	mkSearchPath "foo:.:baz"  =  ["foo", ".", "baz"]

\begin{code}
mkSearchPath :: Maybe String -> SearchPath
mkSearchPath Nothing = ["."]
mkSearchPath (Just s)
  = go s
  where
    go "" = []
    go s  = first : go (drop 1 rest)
	  where
	    (first,rest) = span (/= ':') s
\end{code}

%*********************************************************
%*							*
\subsection{Errors}
%*							*
%*********************************************************

\begin{code}
noIfaceErr mod sty
  = ppBesides [ppPStr SLIT("Could not find valid interface file for "), ppQuote (pprModule sty mod)]
--	, ppStr " in"]) 4 (ppAboves (map ppStr dirs))

cannaeReadFile file err sty
  = ppBesides [ppPStr SLIT("Failed in reading file: "), ppStr file, ppPStr SLIT("; error="), ppStr (show err)]

getDeclErr name sty
  = ppSep [ppPStr SLIT("Failed to find interface decl for"), ppr sty name]

getDeclWarn name sty
  = ppSep [ppPStr SLIT("Warning: failed to find (optional) interface decl for"), ppr sty name]
\end{code}
