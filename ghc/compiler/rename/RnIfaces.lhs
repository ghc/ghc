%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[RnIfaces]{Cacheing and Renaming of Interfaces}

\begin{code}
module RnIfaces (
	getInterfaceExports,
	getImportedInstDecls,
	getSpecialInstModules, getDeferredDataDecls,
	importDecl, recordSlurp,
	getImportVersions, getSlurpedNames, getRnStats,

	checkUpToDate,

	getDeclBinders,
	mkSearchPath
    ) where

#include "HsVersions.h"

import CmdLineOpts	( opt_PruneTyDecls,  opt_PruneInstDecls, 
			  opt_D_show_rn_imports, opt_IgnoreIfacePragmas
			)
import HsSyn		( HsDecl(..), TyDecl(..), ClassDecl(..), InstDecl(..), IfaceSig(..), 
			  HsType(..), ConDecl(..), IE(..), ConDetails(..), Sig(..),
			  hsDeclName
			)
import BasicTypes	( Version, NewOrData(..), IfaceFlavour(..) )
import RdrHsSyn		( RdrNameHsDecl, RdrNameInstDecl, RdrNameTyDecl,
			  RdrName(..), rdrNameOcc
			)
import RnEnv		( newImportedGlobalName, addImplicitOccsRn,
			  ifaceFlavour, availName, availNames, addAvailToNameSet
			)
import RnSource		( rnHsSigType )
import RnMonad
import RnHsSyn          ( RenamedHsDecl )
import ParseIface	( parseIface, IfaceStuff(..) )

import FiniteMap	( FiniteMap, sizeFM, emptyFM, delFromFM,
			  lookupFM, addToFM, addToFM_C, addListToFM, 
			  fmToList
			)
import Name		( Name {-instance NamedThing-}, Provenance, OccName(..),
			  nameModule, moduleString, pprModule, isLocallyDefined,
			  NameSet(..), emptyNameSet, unionNameSets, nameSetToList,
			  minusNameSet, mkNameSet, elemNameSet, nameUnique, addOneToNameSet,
			  isWiredInName, maybeWiredInTyConName, maybeWiredInIdName,
			  NamedThing(..)
			 )
import Id		( GenId, Id(..), idType, dataConTyCon, isAlgCon )
import TyCon		( TyCon, tyConDataCons, isSynTyCon, getSynTyConDefn )
import Type		( namesOfType )
import TyVar		( GenTyVar )
import SrcLoc		( mkSrcLoc, SrcLoc )
import PrelMods		( pREL_GHC )
import PrelInfo		( cCallishTyKeys )
import Bag
import Maybes		( MaybeErr(..), maybeToBool )
import ListSetOps	( unionLists )
import Outputable
import Unique		( Unique )
import StringBuffer     ( StringBuffer, hGetStringBuffer )
import FastString	( mkFastString )
import Outputable

import IO	( isDoesNotExistError )
import List	( nub )
\end{code}



%*********************************************************
%*							*
\subsection{Statistics}
%*							*
%*********************************************************

\begin{code}
getRnStats :: [RenamedHsDecl] -> RnMG SDoc
getRnStats all_decls
  = getIfacesRn 		`thenRn` \ ifaces ->
    let
	Ifaces this_mod mod_map decls_fm all_names imp_names (unslurped_insts,_) deferred_data_decls inst_mods = ifaces
	n_mods	    = sizeFM mod_map

	decls_imported = filter is_imported_decl all_decls
	decls_read     = [decl | (name, (_, avail, decl)) <- fmToList decls_fm,
				 name == availName avail,
					-- Data, newtype, and class decls are in the decls_fm
					-- under multiple names; the tycon/class, and each
					-- constructor/class op too.
				 not (isLocallyDefined name)
			     ]

	(cd_rd, dd_rd, add_rd, nd_rd, and_rd, sd_rd, vd_rd,     _) = count_decls decls_read
	(cd_sp, dd_sp, add_sp, nd_sp, and_sp, sd_sp, vd_sp, id_sp) = count_decls decls_imported

	inst_decls_unslurped  = length (bagToList unslurped_insts)
	inst_decls_read	      = id_sp + inst_decls_unslurped

	stats = vcat 
		[int n_mods <> text " interfaces read",
		 hsep [int cd_sp, text "class decls imported, out of", 
		        int cd_rd, text "read"],
		 hsep [int dd_sp, text "data decls imported (of which", int add_sp, text "abstractly), out of",  
			int dd_rd, text "read"],
		 hsep [int nd_sp, text "newtype decls imported (of which", int and_sp, text "abstractly), out of",  
		        int nd_rd, text "read"],
		 hsep [int sd_sp, text "type synonym decls imported, out of",  
		        int sd_rd, text "read"],
		 hsep [int vd_sp, text "value signatures imported, out of",  
		        int vd_rd, text "read"],
		 hsep [int id_sp, text "instance decls imported, out of",  
		        int inst_decls_read, text "read"]
		]
    in
    returnRn (hcat [text "Renamer stats: ", stats])

is_imported_decl (DefD _) = False
is_imported_decl (ValD _) = False
is_imported_decl decl     = not (isLocallyDefined (hsDeclName decl))

count_decls decls
  = -- pprTrace "count_decls" (ppr  decls
    --
    --			    $$
    --			    text "========="
    --			    $$
    --			    ppr imported_decls
    --	) $
    (class_decls, 
     data_decls,    abstract_data_decls,
     newtype_decls, abstract_newtype_decls,
     syn_decls, 
     val_decls, 
     inst_decls)
  where
    class_decls   = length [() | ClD _		     	    <- decls]
    data_decls    = length [() | TyD (TyData DataType _ _ _ _ _ _ _) <- decls]
    newtype_decls = length [() | TyD (TyData NewType  _ _ _ _ _ _ _) <- decls]
    abstract_data_decls    = length [() | TyD (TyData DataType _ _ _ [] _ _ _) <- decls]
    abstract_newtype_decls = length [() | TyD (TyData NewType  _ _ _ [] _ _ _) <- decls]
    syn_decls     = length [() | TyD (TySynonym _ _ _ _)    <- decls]
    val_decls     = length [() | SigD _		    	    <- decls]
    inst_decls    = length [() | InstD _		    <- decls]

\end{code}    

%*********************************************************
%*							*
\subsection{Loading a new interface file}
%*							*
%*********************************************************

\begin{code}
loadInterface :: SDoc -> Module -> IfaceFlavour -> RnMG Ifaces
loadInterface doc_str load_mod as_source
 = getIfacesRn 		`thenRn` \ ifaces ->
   let
	Ifaces this_mod mod_map decls 
	       all_names imp_names (insts, tycls_names) 
	       deferred_data_decls inst_mods = ifaces
   in
	-- CHECK WHETHER WE HAVE IT ALREADY
   case lookupFM mod_map load_mod of {
	Just (hif, _, _, _) | hif `as_good_as` as_source
			    -> 	-- Already in the cache; don't re-read it
				returnRn ifaces ;
	other ->

	-- READ THE MODULE IN
   findAndReadIface doc_str load_mod as_source	`thenRn` \ read_result ->
   case read_result of {
	-- Check for not found
	Nothing -> 	-- Not found, so add an empty export env to the Ifaces map
			-- so that we don't look again
		   let
			new_mod_map = addToFM mod_map load_mod (HiFile, 0, [],[])
			new_ifaces = Ifaces this_mod new_mod_map
					    decls all_names imp_names (insts, tycls_names) 
					    deferred_data_decls inst_mods
		   in
		   setIfacesRn new_ifaces		`thenRn_`
		   failWithRn new_ifaces (noIfaceErr load_mod) ;

	-- Found and parsed!
	Just (ParsedIface _ mod_vers usages exports rd_inst_mods fixs rd_decls rd_insts) ->

	-- LOAD IT INTO Ifaces
	-- NB: *first* we do loadDecl, so that the provenance of all the locally-defined
	---    names is done correctly (notably, whether this is an .hi file or .hi-boot file).
	--     If we do loadExport first the wrong info gets into the cache (unless we
	-- 	explicitly tag each export which seems a bit of a bore)
    foldlRn (loadDecl load_mod as_source) decls rd_decls `thenRn` \ new_decls ->
    mapRn loadExport exports				 `thenRn` \ avails_s ->
    foldlRn (loadInstDecl load_mod) insts rd_insts	 `thenRn` \ new_insts ->
    let
	 mod_details = (as_source, mod_vers, concat avails_s, fixs)

			-- Exclude this module from the "special-inst" modules
	 new_inst_mods = inst_mods `unionLists` (filter (/= this_mod) rd_inst_mods)

	 new_ifaces = Ifaces this_mod
			     (addToFM mod_map load_mod mod_details)
			     new_decls
			     all_names imp_names
			     (new_insts, tycls_names)
			     deferred_data_decls 
			     new_inst_mods 
    in
    setIfacesRn new_ifaces		`thenRn_`
    returnRn new_ifaces
    }}

as_good_as HiFile any        = True
as_good_as any    HiBootFile = True
as_good_as _      _	     = False


loadExport :: ExportItem -> RnMG [AvailInfo]
loadExport (mod, hif, entities)
  = mapRn load_entity entities
  where
    new_name occ = newImportedGlobalName mod occ hif

    load_entity (Avail occ)
      =	new_name occ 		`thenRn` \ name ->
	returnRn (Avail name)
    load_entity (AvailTC occ occs)
      =	new_name occ 		`thenRn` \ name ->
        mapRn new_name occs 	`thenRn` \ names ->
        returnRn (AvailTC name names)

loadDecl :: Module 
         -> IfaceFlavour
	 -> DeclsMap
	 -> (Version, RdrNameHsDecl)
	 -> RnMG DeclsMap
loadDecl mod as_source decls_map (version, decl)
  = getDeclBinders new_implicit_name decl	`thenRn` \ avail ->
    returnRn (addListToFM decls_map
			  [(name,(version,avail,decl')) | name <- availNames avail]
    )
  where
    {-
      If a signature decl is being loaded and we're ignoring interface pragmas,
      toss away unfolding information.

      Also, if the signature is loaded from a module we're importing from source,
      we do the same. This is to avoid situations when compiling a pair of mutually
      recursive modules, peering at unfolding info in the interface file of the other, 
      e.g., you compile A, it looks at B's interface file and may as a result change
      it's interface file. Hence, B is recompiled, maybe changing it's interface file,
      which will the ufolding info used in A to become invalid. Simple way out is to
      just ignore unfolding info.
    -}
    decl' = 
     case decl of
       SigD (IfaceSig name tp ls loc) | from_hi_boot || opt_IgnoreIfacePragmas -> 
	    SigD (IfaceSig name tp [] loc)
       _ -> decl

    new_implicit_name rdr_name loc = newImportedGlobalName mod (rdrNameOcc rdr_name) as_source

    from_hi_boot = case as_source of
			HiBootFile -> True
			other	   -> False

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
    initRnMS emptyRnEnv mod_name vanillaInterfaceMode (
        findOccurrencesRn (rnHsSigType (text "an instance decl") munged_inst_ty)	
    )						`thenRn` \ gate_names ->
    returnRn (((mod_name, decl), gate_names) `consBag` insts)

vanillaInterfaceMode = InterfaceMode Compulsory (\_ -> False)
\end{code}


%********************************************************
%*							*
\subsection{Loading usage information}
%*							*
%********************************************************

\begin{code}
checkUpToDate :: Module -> RnMG Bool		-- True <=> no need to recompile
checkUpToDate mod_name
  = findAndReadIface doc_str mod_name HiFile	`thenRn` \ read_result ->

	-- CHECK WHETHER WE HAVE IT ALREADY
    case read_result of
	Nothing -> 	-- Old interface file not found, so we'd better bail out
		    traceRn (sep [ptext SLIT("Didnt find old iface"), 
				    pprModule mod_name])	`thenRn_`
		    returnRn False

	Just (ParsedIface _ _ usages _ _ _ _ _) 
		-> 	-- Found it, so now check it
		    checkModUsage usages
  where
	-- Only look in current directory, with suffix .hi
    doc_str = sep [ptext SLIT("need usage info from"), pprModule mod_name]

checkModUsage [] = returnRn True		-- Yes!  Everything is up to date!

checkModUsage ((mod, hif, old_mod_vers, whats_imported) : rest)
  = loadInterface doc_str mod hif	`thenRn` \ ifaces ->
    let
	Ifaces _ mod_map decls _ _ _ _ _ = ifaces
	maybe_new_mod_vers	         = lookupFM mod_map mod
	Just (_, new_mod_vers, _, _)     = maybe_new_mod_vers
    in
	-- If we can't find a version number for the old module then
	-- bail out saying things aren't up to date
    if not (maybeToBool maybe_new_mod_vers) then
	traceRn (sep [ptext SLIT("Can't find version number for module"), pprModule mod]) `thenRn_`
	returnRn False
    else

	-- If the module version hasn't changed, just move on
    if new_mod_vers == old_mod_vers then
	traceRn (sep [ptext SLIT("Module version unchanged:"), pprModule mod])	`thenRn_`
	checkModUsage rest
    else
    traceRn (sep [ptext SLIT("Module version has changed:"), pprModule mod])	`thenRn_`

	-- Module version changed, so check entities inside

	-- If the usage info wants to say "I imported everything from this module"
	--     it does so by making whats_imported equal to Everything
	-- In that case, we must recompile
    case whats_imported of {
      Everything -> traceRn (ptext SLIT("...and I needed the whole module"))	`thenRn_`
		    returnRn False;		   -- Bale out

      Specifically old_local_vers ->

	-- Non-empty usage list, so check item by item
    checkEntityUsage mod decls old_local_vers	`thenRn` \ up_to_date ->
    if up_to_date then
	traceRn (ptext SLIT("...but the bits I use haven't."))	`thenRn_`
	checkModUsage rest	-- This one's ok, so check the rest
    else
	returnRn False		-- This one failed, so just bail out now
    }
  where
    doc_str = sep [ptext SLIT("need version info for"), pprModule mod]


checkEntityUsage mod decls [] 
  = returnRn True	-- Yes!  All up to date!

checkEntityUsage mod decls ((occ_name,old_vers) : rest)
  = newImportedGlobalName mod occ_name HiFile	`thenRn` \ name ->
    case lookupFM decls name of

	Nothing       -> 	-- We used it before, but it ain't there now
			  putDocRn (sep [ptext SLIT("No longer exported:"), ppr name])	`thenRn_`
			  returnRn False

	Just (new_vers,_,_) 	-- It's there, but is it up to date?
		| new_vers == old_vers
			-- Up to date, so check the rest
		-> checkEntityUsage mod decls rest

		| otherwise
			-- Out of date, so bale out
		-> putDocRn (sep [ptext SLIT("Out of date:"), ppr name])  `thenRn_`
		   returnRn False
\end{code}


%*********************************************************
%*							*
\subsection{Getting in a declaration}
%*							*
%*********************************************************

\begin{code}
importDecl :: Occurrence -> RnSMode -> RnMG (Maybe RdrNameHsDecl)
	-- Returns Nothing for a wired-in or already-slurped decl

importDecl (name, loc) mode
  = checkSlurped name			`thenRn` \ already_slurped ->
    if already_slurped then
--	traceRn (sep [text "Already slurped:", ppr name])	`thenRn_`
	returnRn Nothing	-- Already dealt with
    else
    if isWiredInName name then
	getWiredInDecl name mode
    else 
       getIfacesRn 		`thenRn` \ ifaces ->
       let
         Ifaces this_mod _ _ _ _ _ _ _ = ifaces
         mod = nameModule name
       in
       if mod == this_mod  then    -- Don't bring in decls from
	  pprTrace "importDecl wierdness:" (ppr name) $
	  returnRn Nothing         -- the renamed module's own interface file
			           -- 
       else
       getNonWiredInDecl name loc mode
\end{code}

\begin{code}
getNonWiredInDecl :: Name -> SrcLoc -> RnSMode -> RnMG (Maybe RdrNameHsDecl)
getNonWiredInDecl needed_name loc mode
  = traceRn doc_str					 `thenRn_`
    loadInterface doc_str mod (ifaceFlavour needed_name) `thenRn` \ (Ifaces _ _ decls _ _ _ _ _) ->
    case lookupFM decls needed_name of

	-- Special case for data/newtype type declarations
      Just (version, avail, TyD ty_decl) | is_data_or_newtype ty_decl
	      -> getNonWiredDataDecl needed_name version avail ty_decl	`thenRn` \ (avail', maybe_decl) ->
		 recordSlurp (Just version) necessity avail'	`thenRn_`
		 returnRn maybe_decl

      Just (version,avail,decl)
	      -> recordSlurp (Just version) necessity avail	`thenRn_`
		 returnRn (Just decl)

      Nothing -> 	-- Can happen legitimately for "Optional" occurrences
		   case necessity of { 
				Optional -> addWarnRn (getDeclWarn needed_name loc);
				other	 -> addErrRn  (getDeclErr  needed_name loc)
		   }						`thenRn_` 
		   returnRn Nothing
  where
     necessity = modeToNecessity mode
     doc_str = sep [ptext SLIT("need decl for"), ppr needed_name, ptext SLIT("needed at"), ppr loc]
     mod = nameModule needed_name

     is_data_or_newtype (TyData _ _ _ _ _ _ _ _) = True
     is_data_or_newtype other		         = False

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
getWiredInDecl name mode
  = initRnMS emptyRnEnv mod_name new_mode
	     get_wired				`thenRn` \ avail ->
    recordSlurp Nothing necessity avail		`thenRn_`

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
	mod        = nameModule main_name
	doc_str    = sep [ptext SLIT("need home module for wired in thing"), ppr name]
    in
    (if not main_is_tc || mod == pREL_GHC then
	returnRn ()		
    else
	loadInterface doc_str mod (ifaceFlavour main_name)	`thenRn_`
	returnRn ()
    )								`thenRn_`

    returnRn Nothing		-- No declaration to process further
  where
    necessity = modeToNecessity mode
    new_mode = case mode of 
			InterfaceMode _ _ -> mode
			SourceMode	  -> vanillaInterfaceMode

    get_wired | is_tycon			-- ... a type constructor
	      = get_wired_tycon the_tycon

	      | (isAlgCon the_id) 		-- ... a wired-in data constructor
	      = get_wired_tycon (dataConTyCon the_id)

	      | otherwise			-- ... a wired-in non data-constructor
	      = get_wired_id the_id

    mod_name		 = nameModule name
    maybe_wired_in_tycon = maybeWiredInTyConName name
    is_tycon		 = maybeToBool maybe_wired_in_tycon
    maybe_wired_in_id    = maybeWiredInIdName    name
    Just the_tycon	 = maybe_wired_in_tycon
    Just the_id 	 = maybe_wired_in_id


get_wired_id id
  = addImplicitOccsRn id_mentions 	`thenRn_`
    returnRn (Avail (getName id))
  where
    id_mentions = nameSetToList (namesOfType ty)
    ty = idType id

get_wired_tycon tycon 
  | isSynTyCon tycon
  = addImplicitOccsRn (nameSetToList mentioned)		`thenRn_`
    returnRn (AvailTC tc_name [tc_name])
  where
    tc_name     = getName tycon
    (tyvars,ty) = getSynTyConDefn tycon
    mentioned   = namesOfType ty `minusNameSet` mkNameSet (map getName tyvars)

get_wired_tycon tycon 
  | otherwise		-- data or newtype
  = addImplicitOccsRn (nameSetToList mentioned)		`thenRn_`
    returnRn (AvailTC tycon_name (tycon_name : map getName data_cons))
  where
    tycon_name = getName tycon
    data_cons  = tyConDataCons tycon
    mentioned  = foldr (unionNameSets . namesOfType . idType) emptyNameSet data_cons
\end{code}


    
%*********************************************************
%*							*
\subsection{Getting what a module exports}
%*							*
%*********************************************************

\begin{code}
getInterfaceExports :: Module -> IfaceFlavour -> RnMG (Avails, [(OccName,Fixity)])
getInterfaceExports mod as_source
  = loadInterface doc_str mod as_source	`thenRn` \ (Ifaces _ mod_map _ _ _ _ _ _) ->
    case lookupFM mod_map mod of
	Nothing ->	-- Not there; it must be that the interface file wasn't found;
			-- the error will have been reported already.
			-- (Actually loadInterface should put the empty export env in there
			--  anyway, but this does no harm.)
		      returnRn ([],[])

	Just (_, _, avails, fixities) -> returnRn (avails, fixities)
  where
    doc_str = sep [pprModule mod, ptext SLIT("is directly imported")]
\end{code}


%*********************************************************
%*							*
\subsection{Data type declarations are handled specially}
%*							*
%*********************************************************

Data type declarations get special treatment.  If we import a data type decl
with all its constructors, we end up importing all the types mentioned in 
the constructors' signatures, and hence {\em their} data type decls, and so on.
In effect, we get the transitive closure of data type decls.  Worse, this drags
in tons on instance decls, and their unfoldings, and so on.

If only the type constructor is mentioned, then all this is a waste of time.
If any of the data constructors are mentioned then we really have to 
drag in the whole declaration.

So when we import the type constructor for a @data@ or @newtype@ decl, we
put it in the "deferred data/newtype decl" pile in Ifaces.  Right at the end
we slurp these decls, if they havn't already been dragged in by an occurrence
of a constructor.

\begin{code}
getNonWiredDataDecl needed_name 
		    version
	 	    avail@(AvailTC tycon_name _) 
		    ty_decl@(TyData new_or_data context tycon tyvars condecls derivings pragmas src_loc)
  |  needed_name == tycon_name
  && opt_PruneTyDecls
  && not (nameUnique needed_name `elem` cCallishTyKeys)		
	-- Hack!  Don't prune these tycons whose constructors
	-- the desugarer must be able to see when desugaring
	-- a CCall.  Ugh!

  = 	-- Need the type constructor; so put it in the deferred set for now
    getIfacesRn 		`thenRn` \ ifaces ->
    let
	Ifaces this_mod mod_map decls_fm slurped_names imp_names 
	       unslurped_insts deferred_data_decls inst_mods = ifaces

	new_ifaces = Ifaces this_mod mod_map decls_fm slurped_names imp_names
			    unslurped_insts new_deferred_data_decls inst_mods

	no_constr_ty_decl       = TyData new_or_data [] tycon tyvars [] derivings pragmas src_loc
	new_deferred_data_decls = addToFM deferred_data_decls tycon_name no_constr_ty_decl
		-- Nota bene: we nuke both the constructors and the context in the deferred decl.
		-- If we don't nuke the context then renaming the deferred data decls can give
		-- new unresolved names (for the classes).  This could be handled, but there's
		-- no point.  If the data type is completely abstract then we aren't interested
		-- its context.
    in
    setIfacesRn new_ifaces	`thenRn_`
    returnRn (AvailTC tycon_name [tycon_name], Nothing)

  | otherwise
  = 	-- Need a data constructor, so delete the data decl from the deferred set if it's there
    getIfacesRn 		`thenRn` \ ifaces ->
    let
	Ifaces this_mod mod_map decls_fm slurped_names imp_names 
	       unslurped_insts deferred_data_decls inst_mods = ifaces

	new_ifaces = Ifaces this_mod mod_map decls_fm slurped_names imp_names 
			    unslurped_insts new_deferred_data_decls inst_mods

	new_deferred_data_decls = delFromFM deferred_data_decls tycon_name
    in
    setIfacesRn new_ifaces	`thenRn_`
    returnRn (avail, Just (TyD ty_decl))
\end{code}

\begin{code}
getDeferredDataDecls :: RnMG [(Name, RdrNameTyDecl)]
getDeferredDataDecls 
  = getIfacesRn 		`thenRn` \ (Ifaces _ _ _ _ _ _ deferred_data_decls _) ->
    let
	deferred_list = fmToList deferred_data_decls
	trace_msg = hang (text "Slurping abstract data/newtype decls for: ")
			4 (ppr (map fst deferred_list))
    in
    traceRn trace_msg			`thenRn_`
    returnRn deferred_list
\end{code}


%*********************************************************
%*							*
\subsection{Instance declarations are handled specially}
%*							*
%*********************************************************

\begin{code}
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
	Ifaces this_mod mod_map decls slurped_names imp_names (insts, tycls_names) deferred_data_decls inst_mods = ifaces

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
	    remaining_gates = filter (not . (`elemNameSet` tycls_names)) gates

	(un_gated_insts, still_gated_insts) = foldrBag select_ungated ([], []) insts
	
	new_ifaces = Ifaces this_mod mod_map decls slurped_names imp_names
			    ((listToBag still_gated_insts), tycls_names)
				-- NB: don't throw away tycls_names; we may comre across more instance decls
			    deferred_data_decls 
			    inst_mods
    in
    traceRn (sep [text "getInstDecls:", fsep (map ppr (nameSetToList tycls_names))])	`thenRn_`
    setIfacesRn new_ifaces	`thenRn_`
    returnRn un_gated_insts
  where
    load_it mod = loadInterface (doc_str mod) mod HiFile
    doc_str mod = sep [pprModule mod, ptext SLIT("is a special-instance module")]


getSpecialInstModules :: RnMG [Module]
getSpecialInstModules 
  = getIfacesRn						`thenRn` \ ifaces ->
    let
	 Ifaces _ _ _ _ _ _ _ inst_mods = ifaces
    in
    returnRn inst_mods
\end{code}


%*********************************************************
%*							*
\subsection{Keeping track of what we've slurped, and version numbers}
%*							*
%*********************************************************

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
	 Ifaces _ mod_map _ _ imp_names _ _ _ = ifaces

	 -- mv_map groups together all the things imported from a particular module.
	 mv_map, mv_map_mod :: FiniteMap Module (WhatsImported Name)

	 mv_map_mod = foldl add_mod emptyFM export_mods
		-- mv_map_mod records all the modules that have a "module M"
		-- in this module's export list with an "Everything" 

	 mv_map = foldl add_mv mv_map_mod imp_names
		-- mv_map adds the version numbers of things exported individually

	 mk_version_info (mod, local_versions)
	   = case lookupFM mod_map mod of
		Just (hif, version, _, _) -> (mod, hif, version, local_versions)
    in
    returnRn (map mk_version_info (fmToList mv_map))
  where
     export_mods = case exports of
			Nothing -> []
			Just es -> [mod | IEModuleContents mod <- es, mod /= this_mod]

     add_mv mv_map v@(name, version) 
      = addToFM_C add_item mv_map mod (Specifically [v]) 
	where
	 mod = nameModule name

         add_item Everything        _ = Everything
         add_item (Specifically xs) _ = Specifically (v:xs)

     add_mod mv_map mod = addToFM mv_map mod Everything
\end{code}

\begin{code}
checkSlurped name
  = getIfacesRn 	`thenRn` \ (Ifaces _ _ _ slurped_names _ _ _ _) ->
    returnRn (name `elemNameSet` slurped_names)

getSlurpedNames :: RnMG NameSet
getSlurpedNames
  = getIfacesRn 	`thenRn` \ ifaces ->
    let
	 Ifaces _ _ _ slurped_names _ _ _ _ = ifaces
    in
    returnRn slurped_names

recordSlurp maybe_version necessity avail
  = {- traceRn (hsep [text "Record slurp:", pprAvail avail, 
					-- NB PprForDebug prints export flag, which is too
					-- strict; it's a knot-tied thing in RnNames
		  case necessity of {Compulsory -> text "comp"; Optional -> text "opt" } ])	`thenRn_` 
    -}
    getIfacesRn 	`thenRn` \ ifaces ->
    let
	Ifaces this_mod mod_map decls slurped_names imp_names 
	       (insts, tycls_names) deferred_data_decls inst_mods = ifaces

	new_slurped_names = addAvailToNameSet slurped_names avail

	new_imp_names = case maybe_version of
			   Just version	-> (availName avail, version) : imp_names
			   Nothing      -> imp_names

		-- Add to the names that will let in instance declarations;
		-- but only (a) if it's a type/class
		--	    (b) if it's compulsory (unless the test flag opt_PruneInstDecls is off)
	new_tycls_names = case avail of
				AvailTC tc _  | not opt_PruneInstDecls || 
						case necessity of {Optional -> False; Compulsory -> True }
					      -> tycls_names `addOneToNameSet` tc
				otherwise     -> tycls_names

	new_ifaces = Ifaces this_mod mod_map decls 
			    new_slurped_names 
			    new_imp_names
			    (insts, new_tycls_names)
			    deferred_data_decls 
			    inst_mods
    in
    setIfacesRn new_ifaces
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
getDeclBinders :: (RdrName -> SrcLoc -> RnMG Name)	-- New-name function
		-> RdrNameHsDecl
		-> RnMG AvailInfo

getDeclBinders new_name (TyD (TyData _ _ tycon _ condecls _ _ src_loc))
  = new_name tycon src_loc			`thenRn` \ tycon_name ->
    getConFieldNames new_name condecls		`thenRn` \ sub_names ->
    returnRn (AvailTC tycon_name (tycon_name : nub sub_names))
	-- The "nub" is because getConFieldNames can legitimately return duplicates,
	-- when a record declaration has the same field in multiple constructors

getDeclBinders new_name (TyD (TySynonym tycon _ _ src_loc))
  = new_name tycon src_loc		`thenRn` \ tycon_name ->
    returnRn (AvailTC tycon_name [tycon_name])

getDeclBinders new_name (ClD (ClassDecl _ cname _ sigs _ _ tname dname src_loc))
  = new_name cname src_loc			`thenRn` \ class_name ->
    new_name dname src_loc		        `thenRn` \ datacon_name ->
    new_name tname src_loc		        `thenRn` \ tycon_name ->

	-- Record the names for the class ops
    mapRn (getClassOpNames new_name) sigs	`thenRn` \ sub_names ->

    returnRn (AvailTC class_name (class_name : datacon_name : tycon_name : sub_names))

getDeclBinders new_name (SigD (IfaceSig var ty prags src_loc))
  = new_name var src_loc			`thenRn` \ var_name ->
    returnRn (Avail var_name)

getDeclBinders new_name (DefD _)  = returnRn NotAvailable
getDeclBinders new_name (InstD _) = returnRn NotAvailable

----------------
getConFieldNames new_name (ConDecl con _ (RecCon fielddecls) src_loc : rest)
  = mapRn (\n -> new_name n src_loc) (con:fields)	`thenRn` \ cfs ->
    getConFieldNames new_name rest			`thenRn` \ ns  -> 
    returnRn (cfs ++ ns)
  where
    fields = concat (map fst fielddecls)

getConFieldNames new_name (ConDecl con _ _ src_loc : rest)
  = new_name con src_loc		`thenRn` \ n ->
    getConFieldNames new_name rest	`thenRn` \ ns -> 
    returnRn (n:ns)

getConFieldNames new_name [] = returnRn []

getClassOpNames new_name (ClassOpSig op _ _ src_loc) = new_name op src_loc
\end{code}


%*********************************************************
%*							*
\subsection{Reading an interface file}
%*							*
%*********************************************************

\begin{code}
findAndReadIface :: SDoc -> Module 
	  	 -> IfaceFlavour 
		 -> RnMG (Maybe ParsedIface)
	-- Nothing <=> file not found, or unreadable, or illegible
	-- Just x  <=> successfully found and parsed 
findAndReadIface doc_str mod_name as_source
  = traceRn trace_msg			`thenRn_`
    getModuleHiMap			`thenRn` \ himap ->
    case (lookupFM himap real_mod_name) of
      Nothing    ->
         traceRn (ptext SLIT("...failed"))	`thenRn_`
	 returnRn Nothing
      Just fpath ->
         readIface fpath
{-
    getSearchPathRn			`thenRn` \ dirs ->
    try dirs
-}
  where
    real_mod_name = 
     case as_source of
        HiBootFile -> 'b':moduleString mod_name
	HiFile     -> moduleString mod_name

    trace_msg = sep [hsep [ptext SLIT("Reading"), 
			   case as_source of { HiBootFile -> ptext SLIT("[boot]"); other -> empty},
			   ptext SLIT("interface for"), 
			   ptext mod_name <> semi],
		     nest 4 (ptext SLIT("reason:") <+> doc_str)]

{-
	-- For import {-# SOURCE #-} Foo, "as_source" will be True
	-- and we read Foo.hi-boot, not Foo.hi.  This is used to break
	-- loops among modules.
    mod_suffix hi = case as_source of
			HiBootFile -> ".hi-boot" -- Ignore `ways' for boot files.
			HiFile     -> hi

    try [] = traceRn (ptext SLIT("...failed"))	`thenRn_`
	     returnRn Nothing

    try ((dir,hisuf):dirs)
	= readIface file_path	`thenRn` \ read_result ->
	  case read_result of
	      Nothing    -> try dirs
	      Just iface -> traceRn (ptext SLIT("...done"))	`thenRn_`
			    returnRn (Just iface)
	where
	  file_path = dir ++ '/' : moduleString mod_name ++ (mod_suffix hisuf)
-}
\end{code}

@readIface@ tries just the one file.

\begin{code}
readIface :: String -> RnMG (Maybe ParsedIface)	
	-- Nothing <=> file not found, or unreadable, or illegible
	-- Just x  <=> successfully found and parsed 
readIface file_path
  = ioToRnMG (hGetStringBuffer file_path)       `thenRn` \ read_result ->
    case read_result of
	Right contents	  -> 
             case parseIface contents (mkSrcLoc (mkFastString file_path) 1) of
	          Failed err      -> failWithRn Nothing err 
		  Succeeded (PIface iface) -> 
		        if opt_D_show_rn_imports then
			   putDocRn (hcat[ptext SLIT("Read "), text file_path]) `thenRn_`
			   returnRn (Just iface)
			else
			   returnRn (Just iface)

        Left err ->
	  if isDoesNotExistError err then
	     returnRn Nothing
	  else
	     failWithRn Nothing (cannaeReadFile file_path err)
\end{code}

%*********************************************************
%*						 	 *
\subsection{Utils}
%*							 *
%*********************************************************

@mkSearchPath@ takes a string consisting of a colon-separated list
of directories and corresponding suffixes, and turns it into a list
of (directory, suffix) pairs.  For example:

\begin{verbatim}
 mkSearchPath "foo%.hi:.%.p_hi:baz%.mc_hi"
   = [("foo",".hi"),( ".", ".p_hi"), ("baz",".mc_hi")]
\begin{verbatim}

\begin{code}
mkSearchPath :: Maybe String -> SearchPath
mkSearchPath Nothing = [(".",".hi")]
mkSearchPath (Just s)
  = go s
  where
    go "" = []
    go s  = 
      case span (/= '%') s of
       (dir,'%':rs) ->
         case span (/= ':') rs of
          (hisuf,_:rest) -> (dir,hisuf):go rest
          (hisuf,[])     -> [(dir,hisuf)]
\end{code}

%*********************************************************
%*						 	 *
\subsection{Errors}
%*							 *
%*********************************************************

\begin{code}
noIfaceErr filename
  = hcat [ptext SLIT("Could not find valid interface file "), 
          quotes (pprModule filename)]

cannaeReadFile file err
  = hcat [ptext SLIT("Failed in reading file: "), 
           text file, 
	  ptext SLIT("; error="), 
	   text (show err)]

getDeclErr name loc
  = sep [ptext SLIT("Failed to find interface decl for"), 
         quotes (ppr name), ptext SLIT("needed at"), ppr loc]

getDeclWarn name loc
  = sep [ptext SLIT("Warning: failed to find (optional) interface decl for"), 
         quotes (ppr name), ptext SLIT("desired at"), ppr loc]
\end{code}
