%
% (c) The GRASP Project, Glasgow University, 1992-1998
%
\section[Rename]{Renaming and dependency analysis passes}

\begin{code}
module Rename ( renameModule ) where

#include "HsVersions.h"

import HsSyn
import RdrHsSyn		( RdrNameHsModule )
import RnHsSyn		( RenamedHsModule, RenamedHsDecl, 
			  extractHsTyNames, extractHsCtxtTyNames
			)

import CmdLineOpts	( opt_HiMap, opt_D_dump_rn_trace,
			  opt_D_dump_rn, opt_D_dump_rn_stats,
			  opt_WarnUnusedBinds, opt_WarnUnusedImports
		        )
import RnMonad
import RnNames		( getGlobalNames )
import RnSource		( rnSourceDecls, rnDecl )
import RnIfaces		( getImportedInstDecls, importDecl, getImportVersions,
			  getImportedRules, loadHomeInterface, getSlurped, removeContext
			)
import RnEnv		( availName, availNames, availsToNameSet, 
			  warnUnusedImports, warnUnusedLocalBinds, mapFvRn, lookupImplicitOccRn,
			  FreeVars, plusFVs, plusFV, unitFV, emptyFVs, isEmptyFVs
			)
import Module           ( Module, ModuleName, pprModule, mkSearchPath, mkThisModule )
import Name		( Name, isLocallyDefined,
			  NamedThing(..), ImportReason(..), Provenance(..),
			  pprOccName, nameOccName, nameUnique,
			  getNameProvenance, isUserImportedExplicitlyName,
			  maybeWiredInTyConName, maybeWiredInIdName, isWiredInName
			)
import Id		( idType )
import DataCon		( dataConTyCon, dataConType )
import TyCon		( TyCon, tyConDataCons, isSynTyCon, getSynTyConDefn )
import RdrName		( RdrName )
import NameSet
import PrelMods		( mAIN_Name, pREL_MAIN_Name )
import TysWiredIn	( unitTyCon, intTyCon, doubleTyCon, boolTyCon )
import PrelInfo		( ioTyCon_NAME, thinAirIdNames, fractionalClassKeys, derivingOccurrences )
import Type		( namesOfType, funTyCon )
import ErrUtils		( printErrorsAndWarnings, dumpIfSet, ghcExit )
import BasicTypes	( NewOrData(..) )
import Bag		( isEmptyBag, bagToList )
import FiniteMap	( fmToList, delListFromFM, addToFM, sizeFM, eltsFM )
import UniqSupply	( UniqSupply )
import UniqFM		( lookupUFM )
import Util		( equivClasses )
import Maybes		( maybeToBool )
import SrcLoc		( mkBuiltinSrcLoc )
import Outputable
\end{code}



\begin{code}
renameModule :: UniqSupply
	     -> RdrNameHsModule
	     -> IO (Maybe 
	              ( Module
		      , RenamedHsModule   -- Output, after renaming
		      , InterfaceDetails  -- Interface; for interface file generation
		      , RnNameSupply      -- Final env; for renaming derivings
		      , [ModuleName]	  -- Imported modules; for profiling
		      ))

renameModule us this_mod@(HsModule mod_name vers exports imports local_decls _ loc)
  = 	-- Initialise the renamer monad
    initRn mod_name us (mkSearchPath opt_HiMap) loc
	   (rename this_mod)				>>=
	\ ((maybe_rn_stuff, dump_action), rn_errs_bag, rn_warns_bag) ->

	-- Check for warnings
    printErrorsAndWarnings rn_errs_bag rn_warns_bag	>>

	-- Dump any debugging output
    dump_action 					>>

	-- Return results
    if not (isEmptyBag rn_errs_bag) then
	    ghcExit 1 >> return Nothing
    else
	    return maybe_rn_stuff
\end{code}


\begin{code}
rename this_mod@(HsModule mod_name vers _ imports local_decls deprec loc)
  =  	-- FIND THE GLOBAL NAME ENVIRONMENT
    getGlobalNames this_mod			`thenRn` \ maybe_stuff ->

	-- CHECK FOR EARLY EXIT
    if not (maybeToBool maybe_stuff) then
	-- Everything is up to date; no need to recompile further
	rnDump [] []		`thenRn` \ dump_action ->
	returnRn (Nothing, dump_action)
    else
    let
  	Just (export_env, gbl_env, fixity_env, global_avail_env) = maybe_stuff
    in

	-- RENAME THE SOURCE
    initRnMS gbl_env fixity_env SourceMode (
	rnSourceDecls local_decls
    )					`thenRn` \ (rn_local_decls, source_fvs) ->

	-- SLURP IN ALL THE NEEDED DECLARATIONS
    implicitFVs mod_name rn_local_decls 	`thenRn` \ implicit_fvs -> 
    let
	real_source_fvs = implicit_fvs `plusFV` source_fvs
		-- It's important to do the "plus" this way round, so that
		-- when compiling the prelude, locally-defined (), Bool, etc
		-- override the implicit ones. 
    in
    slurpImpDecls real_source_fvs	`thenRn` \ rn_imp_decls ->
    let
	rn_all_decls	   = rn_local_decls ++ rn_imp_decls
    in

	-- EXIT IF ERRORS FOUND
    checkErrsRn					`thenRn` \ no_errs_so_far ->
    if not no_errs_so_far then
	-- Found errors already, so exit now
	rnDump rn_imp_decls rn_all_decls	`thenRn` \ dump_action ->
	returnRn (Nothing, dump_action)
    else

	-- GENERATE THE VERSION/USAGE INFO
    getImportVersions mod_name export_env	`thenRn` \ my_usages ->
    getNameSupplyRn				`thenRn` \ name_supply ->

	-- REPORT UNUSED NAMES
    reportUnusedNames gbl_env global_avail_env
		      export_env
		      source_fvs			`thenRn_`

	-- RETURN THE RENAMED MODULE
    let
	has_orphans        = any isOrphanDecl rn_local_decls
	direct_import_mods = [mod | ImportDecl mod _ _ _ _ _ <- imports]
	renamed_module = HsModule mod_name vers 
				  trashed_exports trashed_imports
				  rn_all_decls
			          deprec
			          loc
    in
    rnDump rn_imp_decls	rn_all_decls		`thenRn` \ dump_action ->
    returnRn (Just (mkThisModule mod_name,
		    renamed_module, 
		    (has_orphans, my_usages, export_env),
		    name_supply,
		    direct_import_mods), dump_action)
  where
    trashed_exports  = {-trace "rnSource:trashed_exports"-} Nothing
    trashed_imports  = {-trace "rnSource:trashed_imports"-} []
\end{code}

@implicitFVs@ forces the renamer to slurp in some things which aren't
mentioned explicitly, but which might be needed by the type checker.

\begin{code}
implicitFVs mod_name decls
  = mapRn lookupImplicitOccRn implicit_occs	`thenRn` \ implicit_names ->
    returnRn (implicit_main				`plusFV` 
	      mkNameSet (map getName default_tycons)	`plusFV`
	      mkNameSet thinAirIdNames			`plusFV`
	      mkNameSet implicit_names)
  where
 	-- Add occurrences for Int, and (), because they
	-- are the types to which ambigious type variables may be defaulted by
	-- the type checker; so they won't always appear explicitly.
	-- [The () one is a GHC extension for defaulting CCall results.]
	-- ALSO: funTyCon, since it occurs implicitly everywhere!
	--  	 (we don't want to be bothered with making funTyCon a
	--	  free var at every function application!)
	-- Double is dealt with separately in getGates
    default_tycons = [unitTyCon, funTyCon, boolTyCon, intTyCon]

	-- Add occurrences for IO or PrimIO
    implicit_main |  mod_name == mAIN_Name
		  || mod_name == pREL_MAIN_Name = unitFV ioTyCon_NAME
		  |  otherwise 		        = emptyFVs

	-- Now add extra "occurrences" for things that
	-- the deriving mechanism, or defaulting, will later need in order to
	-- generate code
    implicit_occs = foldr ((++) . get) [] decls

    get (TyClD (TyData _ _ _ _ _ (Just deriv_classes) _ _))
       = concat (map get_deriv deriv_classes)
    get other = []

    get_deriv cls = case lookupUFM derivingOccurrences cls of
			Nothing   -> []
			Just occs -> occs
\end{code}

\begin{code}
isOrphanDecl (InstD (InstDecl inst_ty _ _ _ _))
  = not (foldNameSet ((||) . isLocallyDefined) False (extractHsTyNames (removeContext inst_ty)))
	-- The 'removeContext' is because of
	--	instance Foo a => Baz T where ...
	-- The decl is an orphan if Baz and T are both not locally defined,
	--	even if Foo *is* locally defined

isOrphanDecl (RuleD (RuleDecl _ _ _ lhs _ _))
  = check lhs
  where
	-- At the moment we just check for common LHS forms
	-- Expand as necessary.  Getting it wrong just means
	-- more orphans than necessary
    check (HsVar v)   	  = not (isLocallyDefined v)
    check (HsApp f a) 	  = check f && check a
    check (HsLit _)   	  = False
    check (OpApp l o _ r) = check l && check o && check r
    check (NegApp e _)    = check e
    check (HsPar e)	  = check e
    check (SectionL e o)  = check e && check o
    check (SectionR o e)  = check e && check o

    check other	      	  = True 	-- Safe fall through

isOrphanDecl other = False
\end{code}


\begin{code}
dupDefaultDeclErrRn (DefaultDecl _ locn1 : dup_things)
  = pushSrcLocRn locn1	$
    addErrRn msg
  where
    msg = hang (ptext SLIT("Multiple default declarations"))
	       4  (vcat (map pp dup_things))
    pp (DefaultDecl _ locn) = ptext SLIT("here was another default declaration") <+> ppr locn
\end{code}


%*********************************************************
%*						 	 *
\subsection{Slurping declarations}
%*							 *
%*********************************************************

\begin{code}
-------------------------------------------------------
slurpImpDecls source_fvs
  = traceRn (text "slurpImp" <+> fsep (map ppr (nameSetToList source_fvs))) `thenRn_`

	-- The current slurped-set records all local things
    getSlurped					`thenRn` \ source_binders ->
    slurpSourceRefs source_binders source_fvs	`thenRn` \ (decls, needed) ->

	-- And finally get everything else
    closeDecls decls needed

-------------------------------------------------------
slurpSourceRefs :: NameSet			-- Variables defined in source
		-> FreeVars			-- Variables referenced in source
		-> RnMG ([RenamedHsDecl],
			 FreeVars)		-- Un-satisfied needs
-- The declaration (and hence home module) of each gate has
-- already been loaded

slurpSourceRefs source_binders source_fvs
  = go_outer [] 			-- Accumulating decls
	     emptyFVs 			-- Unsatisfied needs
	     emptyFVs			-- Accumulating gates
  	     (nameSetToList source_fvs)	-- Things whose defn hasn't been loaded yet
  where
	-- The outer loop repeatedly slurps the decls for the current gates
	-- and the instance decls 

	-- The outer loop is needed because consider
	--	instance Foo a => Baz (Maybe a) where ...
	-- It may be that @Baz@ and @Maybe@ are used in the source module,
	-- but not @Foo@; so we need to chase @Foo@ too.
	--
	-- We also need to follow superclass refs.  In particular, 'chasing @Foo@' must
	-- include actually getting in Foo's class decl
	--	class Wib a => Foo a where ..
	-- so that its superclasses are discovered.  The point is that Wib is a gate too.
	-- We do this for tycons too, so that we look through type synonyms.

    go_outer decls fvs all_gates []	
	= returnRn (decls, fvs)

    go_outer decls fvs all_gates refs	-- refs are not necessarily slurped yet
	= traceRn (text "go_outer" <+> ppr refs)		`thenRn_`
	  go_inner decls fvs emptyFVs refs			`thenRn` \ (decls1, fvs1, gates1) ->
	  getImportedInstDecls (all_gates `plusFV` gates1)	`thenRn` \ inst_decls ->
	  rnInstDecls decls1 fvs1 gates1 inst_decls		`thenRn` \ (decls2, fvs2, gates2) ->
	  go_outer decls2 fvs2 (all_gates `plusFV` gates2)
			       (nameSetToList (gates2 `minusNameSet` all_gates))
		-- Knock out the all_gates because even if we don't slurp any new
		-- decls we can get some apparently-new gates from wired-in names

    go_inner decls fvs gates []
	= returnRn (decls, fvs, gates)

    go_inner decls fvs gates (wanted_name:refs) 
	| isWiredInName wanted_name
 	= load_home wanted_name		`thenRn_`
	  go_inner decls fvs (gates `plusFV` getWiredInGates wanted_name) refs

	| otherwise
	= importDecl wanted_name 		`thenRn` \ maybe_decl ->
	  case maybe_decl of
	    Nothing   -> go_inner decls fvs gates refs	-- No declaration... (already slurped, or local)
	    Just decl -> rnIfaceDecl decl		`thenRn` \ (new_decl, fvs1) ->
			 go_inner (new_decl : decls)
			          (fvs1 `plusFV` fvs)
			   	  (gates `plusFV` getGates source_fvs new_decl)
			   	  refs

	-- When we find a wired-in name we must load its
	-- home module so that we find any instance decls therein
    load_home name 
	| name `elemNameSet` source_binders = returnRn ()
		-- When compiling the prelude, a wired-in thing may
		-- be defined in this module, in which case we don't
		-- want to load its home module!
		-- Using 'isLocallyDefined' doesn't work because some of
		-- the free variables returned are simply 'listTyCon_Name',
		-- with a system provenance.  We could look them up every time
		-- but that seems a waste.
	| otherwise			      = loadHomeInterface doc name	`thenRn_`
						returnRn ()
        where
	  doc = ptext SLIT("need home module for wired in thing") <+> ppr name

rnInstDecls decls fvs gates []
  = returnRn (decls, fvs, gates)
rnInstDecls decls fvs gates (d:ds) 
  = rnIfaceDecl d		`thenRn` \ (new_decl, fvs1) ->
    rnInstDecls (new_decl:decls) 
	        (fvs1 `plusFV` fvs)
		(gates `plusFV` getInstDeclGates new_decl)
		ds
\end{code}


\begin{code}
-------------------------------------------------------
-- closeDecls keeps going until the free-var set is empty
closeDecls decls needed
  | not (isEmptyFVs needed)
  = slurpDecls decls needed	`thenRn` \ (decls1, needed1) ->
    closeDecls decls1 needed1

  | otherwise
  = getImportedRules 			`thenRn` \ rule_decls ->
    case rule_decls of
	[]    -> returnRn decls	-- No new rules, so we are done
	other -> rnIfaceDecls decls emptyFVs rule_decls 	`thenRn` \ (decls1, needed1) ->
		 closeDecls decls1 needed1
		 

-------------------------------------------------------
rnIfaceDecls :: [RenamedHsDecl] -> FreeVars
	     -> [(Module, RdrNameHsDecl)]
	     -> RnM d ([RenamedHsDecl], FreeVars)
rnIfaceDecls decls fvs []     = returnRn (decls, fvs)
rnIfaceDecls decls fvs (d:ds) = rnIfaceDecl d		`thenRn` \ (new_decl, fvs1) ->
				rnIfaceDecls (new_decl:decls) (fvs1 `plusFV` fvs) ds

rnIfaceDecl (mod, decl) = initIfaceRnMS mod (rnDecl decl)	
			

-------------------------------------------------------
-- Augment decls with any decls needed by needed.
-- Return also free vars of the new decls (only)
slurpDecls decls needed
  = go decls emptyFVs (nameSetToList needed) 
  where
    go decls fvs []         = returnRn (decls, fvs)
    go decls fvs (ref:refs) = slurpDecl decls fvs ref	`thenRn` \ (decls1, fvs1) ->
			      go decls1 fvs1 refs

-------------------------------------------------------
slurpDecl decls fvs wanted_name
  = importDecl wanted_name 		`thenRn` \ maybe_decl ->
    case maybe_decl of
	-- No declaration... (wired in thing)
	Nothing -> returnRn (decls, fvs)

	-- Found a declaration... rename it
	Just decl -> rnIfaceDecl decl		`thenRn` \ (new_decl, fvs1) ->
		     returnRn (new_decl:decls, fvs1 `plusFV` fvs)
\end{code}


%*********************************************************
%*						 	 *
\subsection{Extracting the `gates'}
%*							 *
%*********************************************************

When we import a declaration like
\begin{verbatim}
	data T = T1 Wibble | T2 Wobble
\end{verbatim}
we don't want to treat @Wibble@ and @Wobble@ as gates
{\em unless} @T1@, @T2@ respectively are mentioned by the user program.
If only @T@ is mentioned
we want only @T@ to be a gate;
that way we don't suck in useless instance
decls for (say) @Eq Wibble@, when they can't possibly be useful.

@getGates@ takes a newly imported (and renamed) decl, and the free
vars of the source program, and extracts from the decl the gate names.

\begin{code}
getGates source_fvs (SigD (IfaceSig _ ty _ _))
  = extractHsTyNames ty

getGates source_fvs (TyClD (ClassDecl ctxt cls tvs _ sigs _ _ _ _ _ _))
  = (delListFromNameSet (foldr (plusFV . get) (extractHsCtxtTyNames ctxt) sigs)
		       (map getTyVarName tvs)
     `addOneToNameSet` cls)
    `plusFV` maybe_double
  where
    get (ClassOpSig n _ _ ty _) 
	| n `elemNameSet` source_fvs = extractHsTyNames ty
	| otherwise		     = emptyFVs

	-- If we load any numeric class that doesn't have
	-- Int as an instance, add Double to the gates. 
	-- This takes account of the fact that Double might be needed for
	-- defaulting, but we don't want to load Double (and all its baggage)
	-- if the more exotic classes aren't used at all.
    maybe_double | nameUnique cls `elem` fractionalClassKeys 
		 = unitFV (getName doubleTyCon)
		 | otherwise
		 = emptyFVs

getGates source_fvs (TyClD (TySynonym tycon tvs ty _))
  = delListFromNameSet (extractHsTyNames ty)
		       (map getTyVarName tvs)
	-- A type synonym type constructor isn't a "gate" for instance decls

getGates source_fvs (TyClD (TyData _ ctxt tycon tvs cons _ _ _))
  = delListFromNameSet (foldr (plusFV . get) (extractHsCtxtTyNames ctxt) cons)
		       (map getTyVarName tvs)
    `addOneToNameSet` tycon
  where
    get (ConDecl n tvs ctxt details _)
	| n `elemNameSet` source_fvs
		-- If the constructor is method, get fvs from all its fields
	= delListFromNameSet (get_details details `plusFV` 
		  	      extractHsCtxtTyNames ctxt)
			     (map getTyVarName tvs)
    get (ConDecl n tvs ctxt (RecCon fields) _)
		-- Even if the constructor isn't mentioned, the fields
		-- might be, as selectors.  They can't mention existentially
		-- bound tyvars (typechecker checks for that) so no need for 
		-- the deleteListFromNameSet part
	= foldr (plusFV . get_field) emptyFVs fields
	
    get other_con = emptyFVs

    get_details (VanillaCon tys) = plusFVs (map get_bang tys)
    get_details (InfixCon t1 t2) = get_bang t1 `plusFV` get_bang t2
    get_details (RecCon fields)  = plusFVs [get_bang t | (_, t) <- fields]
    get_details (NewCon t _)	 = extractHsTyNames t

    get_field (fs,t) | any (`elemNameSet` source_fvs) fs = get_bang t
		     | otherwise			 = emptyFVs

    get_bang (Banged   t) = extractHsTyNames t
    get_bang (Unbanged t) = extractHsTyNames t
    get_bang (Unpacked t) = extractHsTyNames t

getGates source_fvs other_decl = emptyFVs
\end{code}

@getWiredInGates@ is just like @getGates@, but it sees a wired-in @Name@
rather than a declaration.

\begin{code}
getWiredInGates :: Name -> FreeVars
getWiredInGates name 	-- No classes are wired in
  | is_id	         = getWiredInGates_s (namesOfType (idType the_id))
  | isSynTyCon the_tycon = getWiredInGates_s
	 (delListFromNameSet (namesOfType ty) (map getName tyvars))
  | otherwise 	         = unitFV name
  where
    maybe_wired_in_id    = maybeWiredInIdName name
    is_id		 = maybeToBool maybe_wired_in_id
    maybe_wired_in_tycon = maybeWiredInTyConName name
    Just the_id 	 = maybe_wired_in_id
    Just the_tycon	 = maybe_wired_in_tycon
    (tyvars,ty) 	 = getSynTyConDefn the_tycon

getWiredInGates_s names = foldr (plusFV . getWiredInGates) emptyFVs (nameSetToList names)
\end{code}

\begin{code}
getInstDeclGates (InstD (InstDecl inst_ty _ _ _ _)) = extractHsTyNames inst_ty
getInstDeclGates other				    = emptyFVs
\end{code}


%*********************************************************
%*						 	 *
\subsection{Unused names}
%*							 *
%*********************************************************

\begin{code}
reportUnusedNames gbl_env avail_env (ExportEnv export_avails _ _) mentioned_names
  = let
	used_names = mentioned_names `unionNameSets` availsToNameSet export_avails

	-- Now, a use of C implies a use of T,
	-- if C was brought into scope by T(..) or T(C)
	really_used_names = used_names `unionNameSets`
	  mkNameSet [ availName avail	
		    | sub_name <- nameSetToList used_names,
	              let avail = case lookupNameEnv avail_env sub_name of
			    Just avail -> avail
		            Nothing -> WARN( True, text "reportUnusedName: not in avail_env" <+> ppr sub_name )
				       Avail sub_name
		    ]

	defined_names = mkNameSet (concat (rdrEnvElts gbl_env))
	defined_but_not_used =
	   nameSetToList (defined_names `minusNameSet` really_used_names)

	-- Filter out the ones only defined implicitly
	bad_locals = [n | n <- defined_but_not_used, isLocallyDefined		  n]
	bad_imps   = [n | n <- defined_but_not_used, isUserImportedExplicitlyName n]
    in
    warnUnusedLocalBinds bad_locals	`thenRn_`
    warnUnusedImports bad_imps

rnDump  :: [RenamedHsDecl] 	-- Renamed imported decls
	-> [RenamedHsDecl] 	-- Renamed local decls
	-> RnMG (IO ())
rnDump imp_decls decls
        | opt_D_dump_rn_trace || 
	  opt_D_dump_rn_stats ||
	  opt_D_dump_rn 
 	= getRnStats imp_decls		`thenRn` \ stats_msg ->

	  returnRn (printErrs stats_msg >> 
		    dumpIfSet opt_D_dump_rn "Renamer:" (vcat (map ppr decls)))

	| otherwise = returnRn (return ())
\end{code}


%*********************************************************
%*							*
\subsection{Statistics}
%*							*
%*********************************************************

\begin{code}
getRnStats :: [RenamedHsDecl] -> RnMG SDoc
getRnStats imported_decls
  = getIfacesRn 		`thenRn` \ ifaces ->
    let
	n_mods = length [() | (_, _, Just _) <- eltsFM (iImpModInfo ifaces)]

	decls_read     = [decl | (_, avail, True, (_,decl)) <- nameEnvElts (iDecls ifaces),
				-- Data, newtype, and class decls are in the decls_fm
				-- under multiple names; the tycon/class, and each
				-- constructor/class op too.
				-- The 'True' selects just the 'main' decl
				 not (isLocallyDefined (availName avail))
			     ]

	(cd_rd, dd_rd, nd_rd, sd_rd, vd_rd,     _) = count_decls decls_read
	(cd_sp, dd_sp, nd_sp, sd_sp, vd_sp, id_sp) = count_decls imported_decls

	unslurped_insts       = iInsts ifaces
	inst_decls_unslurped  = length (bagToList unslurped_insts)
	inst_decls_read	      = id_sp + inst_decls_unslurped

	stats = vcat 
		[int n_mods <+> text "interfaces read",
		 hsep [ int cd_sp, text "class decls imported, out of", 
		        int cd_rd, text "read"],
		 hsep [ int dd_sp, text "data decls imported, out of",  
			int dd_rd, text "read"],
		 hsep [ int nd_sp, text "newtype decls imported, out of",  
		        int nd_rd, text "read"],
		 hsep [int sd_sp, text "type synonym decls imported, out of",  
		        int sd_rd, text "read"],
		 hsep [int vd_sp, text "value signatures imported, out of",  
		        int vd_rd, text "read"],
		 hsep [int id_sp, text "instance decls imported, out of",  
		        int inst_decls_read, text "read"],
		 text "cls dcls slurp" <+> fsep (map (ppr . tyClDeclName) 
					   [d | TyClD d <- imported_decls, isClassDecl d]),
		 text "cls dcls read"  <+> fsep (map (ppr . tyClDeclName) 
					   [d | TyClD d <- decls_read, isClassDecl d])]
    in
    returnRn (hcat [text "Renamer stats: ", stats])

count_decls decls
  = (class_decls, 
     data_decls, 
     newtype_decls,
     syn_decls, 
     val_decls, 
     inst_decls)
  where
    tycl_decls = [d | TyClD d <- decls]
    (class_decls, data_decls, newtype_decls, syn_decls) = countTyClDecls tycl_decls

    val_decls     = length [() | SigD _	  <- decls]
    inst_decls    = length [() | InstD _  <- decls]
\end{code}    

