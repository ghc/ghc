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
	getDecl, getWiredInDecl,
	getImportVersions,

	checkUpToDate,

	getDeclBinders,
	mkSearchPath
    ) where

IMP_Ubiq()


import HsSyn		( HsDecl(..), TyDecl(..), ClassDecl(..), HsTyVar, Bind, HsExpr, Sig(..), 
			  HsBinds(..), MonoBinds, DefaultDecl, ConDecl(..), HsType, BangType, IfaceSig(..),
			  FixityDecl(..), Fixity, Fake, InPat, InstDecl(..), SYN_IE(Version), HsIdInfo
			)
import HsPragmas	( noGenPragmas )
import RdrHsSyn		( SYN_IE(RdrNameHsDecl), SYN_IE(RdrNameInstDecl), 
			  RdrName, rdrNameOcc
			)
import RnEnv		( newGlobalName, lookupRn, addImplicitOccsRn, availNames )
import RnSource		( rnHsType )
import RnMonad
import ParseIface	( parseIface )

import ErrUtils		( SYN_IE(Error), SYN_IE(Warning) )
import FiniteMap	( FiniteMap, emptyFM, unitFM, lookupFM, addToFM, addListToFM, fmToList )
import Name		( Name {-instance NamedThing-}, Provenance, OccName(..),
			  modAndOcc, occNameString, moduleString, pprModule,
			  NameSet(..), emptyNameSet, unionNameSets, nameSetToList,
			  minusNameSet, mkNameSet,
			  isWiredInName, maybeWiredInTyConName, maybeWiredInIdName
			 )
import Id		( GenId, Id(..), idType, dataConTyCon, isDataCon )
import TyCon		( TyCon, tyConDataCons, isSynTyCon, getSynTyConDefn )
import Type		( namesOfType )
import TyVar		( GenTyVar )
import SrcLoc		( mkIfaceSrcLoc )
import PrelMods		( gHC__ )
import Bag
import Maybes		( MaybeErr(..), expectJust, maybeToBool )
import ListSetOps	( unionLists )
import Pretty
import PprStyle		( PprStyle(..) )
import Util		( pprPanic )
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
	Ifaces this_mod mod_vers_map export_env_map vers_map decls_map inst_map inst_mods = ifaces
    in
	-- CHECK WHETHER WE HAVE IT ALREADY
    if maybeToBool (lookupFM export_env_map load_mod) 
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
			new_export_env_map = addToFM export_env_map load_mod ([],[])
			new_ifaces = Ifaces this_mod mod_vers_map 
					    new_export_env_map 
					    vers_map decls_map inst_map inst_mods
		   in
		   setIfacesRn new_ifaces		`thenRn_`
		   failWithRn new_ifaces (noIfaceErr load_mod) ;

	-- Found and parsed!
	Just (ParsedIface _ mod_vers usages exports rd_inst_mods fixs decls insts) ->

	-- LOAD IT INTO Ifaces
    mapRn loadExport exports					`thenRn` \ avails ->
    foldlRn (loadDecl load_mod) (decls_map,vers_map) decls	`thenRn` \ (new_decls_map, new_vers_map) ->
    foldlRn (loadInstDecl load_mod) inst_map insts		`thenRn` \ new_insts_map ->
    let
	 export_env = (avails, fixs)

			-- Exclude this module from the "special-inst" modules
	 new_inst_mods = inst_mods `unionLists` (filter (/= this_mod) rd_inst_mods)

	 new_ifaces = Ifaces this_mod
			     (addToFM mod_vers_map load_mod mod_vers)
			     (addToFM export_env_map load_mod export_env)
			     new_vers_map
			     new_decls_map
			     new_insts_map
			     new_inst_mods 
    in
    setIfacesRn new_ifaces		`thenRn_`
    returnRn new_ifaces
    }

loadExport :: ExportItem -> RnMG AvailInfo
loadExport (mod, occ, occs)
  = new_name occ 		`thenRn` \ name ->
    mapRn new_name occs 	`thenRn` \ names ->
    returnRn (Avail name names)
  where
    new_name occ = newGlobalName mod occ

loadVersion :: Module -> VersionMap -> (OccName,Version) -> RnMG VersionMap
loadVersion mod vers_map (occ, version)
  = newGlobalName mod occ 			`thenRn` \ name ->
    returnRn (addToFM vers_map name version)


loadDecl :: Module -> (DeclsMap, VersionMap)
	 -> (Version, RdrNameHsDecl)
	 -> RnMG (DeclsMap, VersionMap)
loadDecl mod (decls_map, vers_map) (version, decl)
  = getDeclBinders new_implicit_name decl	`thenRn` \ avail@(Avail name _) ->
    returnRn (addListToFM decls_map
			  [(name,(avail,decl)) | name <- availNames avail],
	      addToFM vers_map name version
    )
  where
    new_implicit_name rdr_name loc = newGlobalName mod (rdrNameOcc rdr_name)

loadInstDecl :: Module -> Bag IfaceInst -> RdrNameInstDecl -> RnMG (Bag IfaceInst)
loadInstDecl mod_name insts decl@(InstDecl inst_ty binds uprags dfun_name src_loc)
  = initRnMS emptyRnEnv mod_name InterfaceMode $

	-- Find out what type constructors and classes are mentioned in the
	-- instance declaration.  We have to be a bit clever.
	--
	-- We want to rename the type so that we can find what
	-- (free) type constructors are inside it.  But we must *not* thereby
	-- put new occurrences into the global pool because otherwise we'll force
	-- them all to be loaded.  We kill two birds with ones stone by renaming
	-- with a fresh occurrence pool.
    findOccurrencesRn (rnHsType inst_ty)	`thenRn` \ ty_names ->

    returnRn ((ty_names, mod_name, decl) `consBag` insts)
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
	Nothing -> 	-- Old interface file not found, so we'd better bale out
		    traceRn (ppSep [ppStr "Didnt find old iface", pprModule PprDebug mod_name])	`thenRn_`
		    returnRn False

	Just (ParsedIface _ _ usages _ _ _ _ _) 
		-> 	-- Found it, so now check it
		    checkModUsage usages
  where
	-- Only look in current directory, with suffix .hi
    doc_str = ppSep [ppStr "Need usage info from", pprModule PprDebug mod_name]


checkModUsage [] = returnRn True		-- Yes!  Everything is up to date!

checkModUsage ((mod, old_mod_vers, old_local_vers) : rest)
  = loadInterface doc_str mod		`thenRn` \ ifaces ->
    let
	Ifaces _ mod_vers_map _ new_vers_map _ _ _ = ifaces
	maybe_new_mod_vers = lookupFM mod_vers_map mod
	Just new_mod_vers  = maybe_new_mod_vers
    in
	-- If we can't find a version number for the old module then
	-- bale out saying things aren't up to date
    if not (maybeToBool maybe_new_mod_vers) then
	returnRn False
    else

	-- If the module version hasn't changed, just move on
    if new_mod_vers == old_mod_vers then
	traceRn (ppSep [ppStr "Module version unchanged:", pprModule PprDebug mod])	`thenRn_`
	checkModUsage rest
    else
    traceRn (ppSep [ppStr "Module version has changed:", pprModule PprDebug mod])	`thenRn_`

	-- New module version, so check entities inside
    checkEntityUsage mod new_vers_map old_local_vers	`thenRn` \ up_to_date ->
    if up_to_date then
	traceRn (ppStr "...but the bits I use havn't.")	`thenRn_`
	checkModUsage rest	-- This one's ok, so check the rest
    else
	returnRn False		-- This one failed, so just bail out now
  where
    doc_str = ppSep [ppStr "need version info for", pprModule PprDebug mod]


checkEntityUsage mod new_vers_map [] 
  = returnRn True	-- Yes!  All up to date!

checkEntityUsage mod new_vers_map ((occ_name,old_vers) : rest)
  = newGlobalName mod occ_name		`thenRn` \ name ->
    case lookupFM new_vers_map name of

	Nothing       -> 	-- We used it before, but it ain't there now
			  traceRn (ppSep [ppStr "...and this no longer exported:", ppr PprDebug name])	`thenRn_`
			  returnRn False

	Just new_vers -> 	-- It's there, but is it up to date?
			  if new_vers == old_vers then
				-- Up to date, so check the rest
				checkEntityUsage mod new_vers_map rest
			  else
				traceRn (ppSep [ppStr "...and this is out of date:", ppr PprDebug name])  `thenRn_`
			        returnRn False	-- Out of date, so bale out
\end{code}


%*********************************************************
%*							*
\subsection{Getting in a declaration}
%*							*
%*********************************************************

\begin{code}
getDecl :: Name -> RnMG (AvailInfo, RdrNameHsDecl)
getDecl name
  = traceRn doc_str 			`thenRn_`
    loadInterface doc_str mod		`thenRn` \ (Ifaces _ _ _ _ decls_map _ _) ->
    case lookupFM decls_map name of

      Just avail_w_decl -> returnRn avail_w_decl

      Nothing   	-> 	-- Can happen legitimately for "Optional" occurrences
			   returnRn (NotAvailable, ValD EmptyBinds)
  where
     (mod,_) = modAndOcc name
     doc_str = ppSep [ppStr "Need decl for", ppr PprDebug name]
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
getWiredInDecl :: Name -> RnMG AvailInfo
getWiredInDecl name
  = 	-- Force in the home module in case it has instance decls for
	-- the thing we are interested in
    (if mod == gHC__ then
	returnRn ()			-- Mini hack; GHC is guaranteed not to have
					-- instance decls, so it's a waste of time
					-- to read it
    else
	loadInterface doc_str mod	`thenRn_`
	returnRn ()
    )					 	`thenRn_`

    if (maybeToBool maybe_wired_in_tycon) then
	get_wired_tycon the_tycon
    else				-- Must be a wired-in-Id
    if (isDataCon the_id) then		-- ... a wired-in data constructor
	get_wired_tycon (dataConTyCon the_id)
    else				-- ... a wired-in non data-constructor
   	get_wired_id the_id
  where
    doc_str = ppSep [ppStr "Need home module for wired in thing", ppr PprDebug name]
    (mod,_) = modAndOcc name
    maybe_wired_in_tycon = maybeWiredInTyConName name
    maybe_wired_in_id    = maybeWiredInIdName    name
    Just the_tycon	 = maybe_wired_in_tycon
    Just the_id 	 = maybe_wired_in_id

get_wired_id id
  = addImplicitOccsRn (nameSetToList id_mentioned)	`thenRn_`
    returnRn (Avail (getName id) [])
  where
    id_mentioned	 = namesOfType (idType id)

get_wired_tycon tycon 
  | isSynTyCon tycon
  = addImplicitOccsRn (nameSetToList mentioned)		`thenRn_`
    returnRn (Avail (getName tycon) [])
  where
    (tyvars,ty) = getSynTyConDefn tycon
    mentioned = namesOfType ty `minusNameSet` mkNameSet (map getName tyvars)

get_wired_tycon tycon 
  | otherwise		-- data or newtype
  = addImplicitOccsRn (nameSetToList mentioned)		`thenRn_`
    returnRn (Avail (getName tycon) (map getName data_cons))
  where
    data_cons = tyConDataCons tycon
    mentioned = foldr (unionNameSets . namesOfType . idType) emptyNameSet data_cons
\end{code}


%*********************************************************
%*							*
\subsection{Getting other stuff}
%*							*
%*********************************************************

\begin{code}
getInterfaceExports :: Module -> RnMG (Avails, [(OccName,Fixity)])
getInterfaceExports mod
  = loadInterface doc_str mod		`thenRn` \ (Ifaces _ _ export_envs _ _ _ _) ->
    case lookupFM export_envs mod of
	Nothing ->	-- Not there; it must be that the interface file wasn't found;
			-- the error will have been reported already.
			-- (Actually loadInterface should put the empty export env in there
			--  anyway, but this does no harm.)
		      returnRn ([],[])

	Just stuff -> returnRn stuff
  where
    doc_str = ppSep [pprModule PprDebug mod, ppStr "is directly imported"]


getImportedInstDecls :: RnMG [IfaceInst]
getImportedInstDecls
  = 	-- First load any special-instance modules that aren't aready loaded
    getSpecialInstModules 			`thenRn` \ inst_mods ->
    mapRn load_it inst_mods			`thenRn_`

	-- Now we're ready to grab the instance declarations
    getIfacesRn						`thenRn` \ ifaces ->
    let
	 Ifaces _ _ _ _ _ insts _ = ifaces
    in
    returnRn (bagToList insts) 
  where
    load_it mod = loadInterface (doc_str mod) mod
    doc_str mod = ppSep [pprModule PprDebug mod, ppStr "is a special-instance module"]

getSpecialInstModules :: RnMG [Module]
getSpecialInstModules 
  = getIfacesRn						`thenRn` \ ifaces ->
    let
	 Ifaces _ _ _ _ _ _ inst_mods = ifaces
    in
    returnRn inst_mods
\end{code}

\begin{code}
getImportVersions :: [AvailInfo]			-- Imported avails
		  -> RnMG (VersionInfo Name)	-- Version info for these names

getImportVersions imported_avails	
  = getIfacesRn					`thenRn` \ ifaces ->
    let
	 Ifaces _ mod_versions_map _ version_map _ _ _ = ifaces

	 -- import_versions is harder: we have to group together all the things imported
	 -- from a particular module.  We do this with yet another finite map

	 mv_map :: FiniteMap Module [LocalVersion Name]
	 mv_map		   = foldl add_mv emptyFM imported_avails
	 add_mv mv_map (Avail name _) 
	    | isWiredInName name = mv_map	-- Don't record versions for wired-in names
	    | otherwise = case lookupFM mv_map mod of
				Just versions -> addToFM mv_map mod ((name,version):versions)
				Nothing       -> addToFM mv_map mod [(name,version)]
	    where
	     (mod,_) = modAndOcc name
	     version = case lookupFM version_map name of
			 Just v  -> v
			 Nothing -> pprPanic "getVersionInfo:" (ppr PprDebug name)

	 import_versions = [ (mod, expectJust "import_versions" (lookupFM mod_versions_map mod), local_versions)
			   | (mod, local_versions) <- fmToList mv_map
			   ]

	 -- Question: should we filter the builtins out of import_versions?
    in
    returnRn import_versions
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
    returnRn (Avail tycon_name sub_names)

getDeclBinders new_name (TyD (TyNew _ tycon _ (NewConDecl con _ con_loc) _ _ src_loc))
  = new_name tycon src_loc		`thenRn` \ tycon_name ->
    new_name con src_loc		`thenRn` \ con_name ->
    returnRn (Avail tycon_name [con_name])

getDeclBinders new_name (TyD (TySynonym tycon _ _ src_loc))
  = new_name tycon src_loc		`thenRn` \ tycon_name ->
    returnRn (Avail tycon_name [])

getDeclBinders new_name (ClD (ClassDecl _ cname _ sigs _ _ src_loc))
  = new_name cname src_loc			`thenRn` \ class_name ->
    mapRn (getClassOpNames new_name) sigs	`thenRn` \ sub_names ->
    returnRn (Avail class_name sub_names)

getDeclBinders new_name (SigD (IfaceSig var ty prags src_loc))
  = new_name var src_loc			`thenRn` \ var_name ->
    returnRn (Avail var_name [])

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
    trace_msg = ppHang (ppBesides [ppStr "Reading interface for ", 
				   pprModule PprDebug mod, ppSemi])
		     4 (ppBesides [ppStr "reason: ", doc_str])

    try all_dirs [] = traceRn (ppStr "...failed")	`thenRn_`
		      returnRn Nothing

    try all_dirs (dir:dirs)
	= readIface file_path	`thenRn` \ read_result ->
	  case read_result of
		Nothing    -> try all_dirs dirs
		Just iface -> traceRn (ppStr "...done")	`thenRn_`
			      returnRn (Just iface)
	where
	  file_path = dir ++ "/" ++ moduleString mod ++ ".hi"
\end{code}

@readIface@ trys just one file.

\begin{code}
readIface :: String -> RnMG (Maybe ParsedIface)	
	-- Nothing <=> file not found, or unreadable, or illegible
	-- Just x  <=> successfully found and parsed 
readIface file_path
  = ioToRnMG (readFile file_path)  	`thenRn` \ read_result ->
    case read_result of
	Right contents	  -> case parseIface contents of
				Failed err      -> failWithRn Nothing err 
				Succeeded iface -> returnRn (Just iface)

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
  = ppBesides [ppStr "Could not find interface for ", ppQuote (pprModule sty mod)]
--	, ppStr " in"]) 4 (ppAboves (map ppStr dirs))

cannaeReadFile file err sty
  = ppBesides [ppPStr SLIT("Failed in reading file: "), ppStr file, ppStr "; error=", ppStr (show err)]
\end{code}
