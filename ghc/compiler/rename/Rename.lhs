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
			  getImportedRules, loadHomeInterface, getSlurped
			)
import RnEnv		( availName, availNames, availsToNameSet, 
			  warnUnusedTopNames, mapFvRn,
			  FreeVars, plusFVs, plusFV, unitFV, emptyFVs, isEmptyFVs
			)
import Module           ( Module, ModuleName, pprModule, mkSearchPath, mkThisModule )
import Name		( Name, isLocallyDefined,
			  NamedThing(..), ImportReason(..), Provenance(..),
			  pprOccName, nameOccName,
			  getNameProvenance, occNameUserString, 
			  maybeWiredInTyConName, maybeWiredInIdName, isWiredInName
			)
import Id		( idType )
import DataCon		( dataConTyCon, dataConType )
import TyCon		( TyCon, tyConDataCons, isSynTyCon, getSynTyConDefn )
import RdrName		( RdrName )
import NameSet
import PrelMods		( mAIN_Name, pREL_MAIN_Name )
import TysWiredIn	( unitTyCon, intTyCon, doubleTyCon, boolTyCon )
import PrelInfo		( ioTyCon_NAME, thinAirIdNames )
import Type		( namesOfType, funTyCon )
import ErrUtils		( pprBagOfErrors, pprBagOfWarnings,
			  doIfSet, dumpIfSet, ghcExit
			)
import BasicTypes	( NewOrData(..) )
import Bag		( isEmptyBag, bagToList )
import FiniteMap	( fmToList, delListFromFM, addToFM, sizeFM, eltsFM )
import UniqSupply	( UniqSupply )
import Util		( equivClasses )
import Maybes		( maybeToBool )
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

renameModule us this_mod@(HsModule mod_name vers exports imports local_decls loc)
  = 	-- Initialise the renamer monad
    initRn mod_name us (mkSearchPath opt_HiMap) loc
	   (rename this_mod)				>>=
	\ (maybe_rn_stuff, rn_errs_bag, rn_warns_bag) ->

	-- Check for warnings
    doIfSet (not (isEmptyBag rn_warns_bag))
	    (printErrs (pprBagOfWarnings rn_warns_bag))	>>

	-- Check for errors; exit if so
    doIfSet (not (isEmptyBag rn_errs_bag))
	    (printErrs (pprBagOfErrors rn_errs_bag)	 >>
	     ghcExit 1
	    )						 >>

	-- Dump output, if any
    (case maybe_rn_stuff of
	Nothing  -> return ()
	Just results@(_, rn_mod, _, _, _)
		 -> dumpIfSet opt_D_dump_rn "Renamer:"
			      (ppr rn_mod)
    )							>>

	-- Return results
    return maybe_rn_stuff
\end{code}


\begin{code}
rename this_mod@(HsModule mod_name vers exports imports local_decls loc)
  =  	-- FIND THE GLOBAL NAME ENVIRONMENT
    getGlobalNames this_mod			`thenRn` \ maybe_stuff ->

	-- CHECK FOR EARLY EXIT
    if not (maybeToBool maybe_stuff) then
	-- Everything is up to date; no need to recompile further
	rnStats []		`thenRn_`
	returnRn Nothing
    else
    let
  	Just (export_env, gbl_env, fixity_env, global_avail_env) = maybe_stuff
    in

	-- RENAME THE SOURCE
    initRnMS gbl_env fixity_env SourceMode (
	rnSourceDecls local_decls
    )					`thenRn` \ (rn_local_decls, source_fvs) ->

	-- SLURP IN ALL THE NEEDED DECLARATIONS
    let
	real_source_fvs = implicitFVs mod_name `plusFV` source_fvs
		-- It's important to do the "plus" this way round, so that
		-- when compiling the prelude, locally-defined (), Bool, etc
		-- override the implicit ones. 
    in
    slurpImpDecls real_source_fvs	`thenRn` \ rn_imp_decls ->

	-- EXIT IF ERRORS FOUND
    checkErrsRn				`thenRn` \ no_errs_so_far ->
    if not no_errs_so_far then
	-- Found errors already, so exit now
	rnStats []		`thenRn_`
	returnRn Nothing
    else

	-- GENERATE THE VERSION/USAGE INFO
    getImportVersions mod_name exports			`thenRn` \ my_usages ->
    getNameSupplyRn					`thenRn` \ name_supply ->

	-- REPORT UNUSED NAMES
    reportUnusedNames gbl_env global_avail_env
		      export_env
		      source_fvs			`thenRn_`

	-- RETURN THE RENAMED MODULE
    let
	has_orphans        = any isOrphanDecl rn_local_decls
	direct_import_mods = [mod | ImportDecl mod _ _ _ _ _ <- imports]
	rn_all_decls	   = rn_imp_decls ++ rn_local_decls 
	renamed_module = HsModule mod_name vers 
				  trashed_exports trashed_imports
				  rn_all_decls
			          loc
    in
    rnStats rn_imp_decls	`thenRn_`
    returnRn (Just (mkThisModule mod_name,
		    renamed_module, 
		    (has_orphans, my_usages, export_env),
		    name_supply,
		    direct_import_mods))
  where
    trashed_exports  = {-trace "rnSource:trashed_exports"-} Nothing
    trashed_imports  = {-trace "rnSource:trashed_imports"-} []
\end{code}

@implicitFVs@ forces the renamer to slurp in some things which aren't
mentioned explicitly, but which might be needed by the type checker.

\begin{code}
implicitFVs mod_name
  = implicit_main		`plusFV` 
    mkNameSet default_tys	`plusFV`
    mkNameSet thinAirIdNames
  where
	-- Add occurrences for Int, Double, and (), because they
	-- are the types to which ambigious type variables may be defaulted by
	-- the type checker; so they won't always appear explicitly.
	-- [The () one is a GHC extension for defaulting CCall results.]
	-- ALSO: funTyCon, since it occurs implicitly everywhere!
	--  	 (we don't want to be bothered with making funTyCon a
	--	  free var at every function application!)
    default_tys = [getName intTyCon, getName doubleTyCon,
		   getName unitTyCon, getName funTyCon, getName boolTyCon]

	-- Add occurrences for IO or PrimIO
    implicit_main |  mod_name == mAIN_Name
		  || mod_name == pREL_MAIN_Name = unitFV ioTyCon_NAME
		  |  otherwise 		        = emptyFVs
\end{code}

\begin{code}
isOrphanDecl (InstD (InstDecl inst_ty _ _ _ _))
  = not (foldNameSet ((||) . isLocallyDefined) False (extractHsTyNames inst_ty))
isOrphanDecl (RuleD (RuleDecl _ _ _ lhs _ _))
  = check lhs
  where
    check (HsVar v)   = not (isLocallyDefined v)
    check (HsApp f a) = check f && check a
    check other	      = True
isOrphanDecl other = False
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
    slurpSourceRefs source_binders source_fvs	`thenRn` \ (decls1, needed1, inst_gates) ->

	-- Now we can get the instance decls
    slurpInstDecls decls1 needed1 inst_gates	`thenRn` \ (decls2, needed2) ->

	-- And finally get everything else
    closeDecls	 decls2 needed2

-------------------------------------------------------
slurpSourceRefs :: NameSet			-- Variables defined in source
		-> FreeVars			-- Variables referenced in source
		-> RnMG ([RenamedHsDecl],
			 FreeVars,		-- Un-satisfied needs
			 FreeVars)		-- "Gates"
-- The declaration (and hence home module) of each gate has
-- already been loaded

slurpSourceRefs source_binders source_fvs
  = go [] 				-- Accumulating decls
       emptyFVs 			-- Unsatisfied needs
       source_fvs			-- Accumulating gates
       (nameSetToList source_fvs)	-- Gates whose defn hasn't been loaded yet
  where
    go decls fvs gates []
	= returnRn (decls, fvs, gates)

    go decls fvs gates (wanted_name:refs) 
	| isWiredInName wanted_name
 	= load_home wanted_name		`thenRn_`
	  go decls fvs (gates `plusFV` getWiredInGates wanted_name) refs

	| otherwise
	= importDecl wanted_name 		`thenRn` \ maybe_decl ->
	  case maybe_decl of
		-- No declaration... (already slurped, or local)
	    Nothing   -> go decls fvs gates refs
	    Just decl -> rnIfaceDecl decl		`thenRn` \ (new_decl, fvs1) ->
			 let
			    new_gates = getGates source_fvs new_decl
			 in
			 go (new_decl : decls)
			    (fvs1 `plusFV` fvs)
			    (gates `plusFV` new_gates)
			    (nameSetToList new_gates ++ refs)

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

-------------------------------------------------------
-- slurpInstDecls imports appropriate instance decls.
-- It has to incorporate a loop, because consider
--	instance Foo a => Baz (Maybe a) where ...
-- It may be that Baz and Maybe are used in the source module,
-- but not Foo; so we need to chase Foo too.

slurpInstDecls decls needed gates
  | isEmptyFVs gates
  = returnRn (decls, needed)

  | otherwise
  = getImportedInstDecls gates				`thenRn` \ inst_decls ->
    rnInstDecls decls needed emptyFVs inst_decls	`thenRn` \ (decls1, needed1, gates1) ->
    slurpInstDecls decls1 needed1 gates1
  where
    rnInstDecls decls fvs gates []
	= returnRn (decls, fvs, gates)
    rnInstDecls decls fvs gates (d:ds) 
	= rnIfaceDecl d		`thenRn` \ (new_decl, fvs1) ->
	  rnInstDecls (new_decl:decls) 
		      (fvs1 `plusFV` fvs)
		      (gates `plusFV` getInstDeclGates new_decl)
		      ds
    

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
\subsection{Extracting the 'gates'}
%*							 *
%*********************************************************

When we import a declaration like

	data T = T1 Wibble | T2 Wobble

we don't want to treat Wibble and Wobble as gates *unless* T1, T2
respectively are mentioned by the user program.  If only T is mentioned
we want only T to be a gate; that way we don't suck in useless instance
decls for (say) Eq Wibble, when they can't possibly be useful.

@getGates@ takes a newly imported (and renamed) decl, and the free
vars of the source program, and extracts from the decl the gate names.

\begin{code}
getGates source_fvs (SigD (IfaceSig _ ty _ _))
  = extractHsTyNames ty

getGates source_fvs (TyClD (ClassDecl ctxt cls tvs sigs _ _ _ _ _ _))
  = delListFromNameSet (foldr (plusFV . get) (extractHsCtxtTyNames ctxt) sigs)
		       (map getTyVarName tvs)
    `addOneToNameSet` cls
  where
    get (ClassOpSig n _ ty _) 
	| n `elemNameSet` source_fvs = extractHsTyNames ty
	| otherwise		     = emptyFVs

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

getWiredInGates is just like getGates, but it sees a wired-in Name
rather than a declaration.

\begin{code}
getWiredInGates :: Name -> FreeVars
getWiredInGates name 	-- No classes are wired in
  | is_id	         = getWiredInGates_s (namesOfType (idType the_id))
  | isSynTyCon the_tycon = getWiredInGates_s (delListFromNameSet (namesOfType ty) (map getName tyvars))
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
reportUnusedNames gbl_env avail_env (ExportEnv export_avails _) mentioned_names
  | not (opt_WarnUnusedBinds || opt_WarnUnusedImports)
  = returnRn ()

  | otherwise
  = let
	used_names = mentioned_names `unionNameSets` availsToNameSet export_avails

	-- Now, a use of C implies a use of T,
	-- if C was brought into scope by T(..) or T(C)
	really_used_names = used_names `unionNameSets`
			    mkNameSet [ availName avail	
				      | sub_name <- nameSetToList used_names,
					let avail = case lookupNameEnv avail_env sub_name of
							Just avail -> avail
							Nothing -> pprTrace "r.u.n" (ppr sub_name) $
								   Avail sub_name
				      ]

	defined_names = mkNameSet (concat (rdrEnvElts gbl_env))
	defined_but_not_used = nameSetToList (defined_names `minusNameSet` really_used_names)

	-- Filter out the ones only defined implicitly
	bad_guys = filter reportableUnusedName defined_but_not_used
    in
    warnUnusedTopNames bad_guys	`thenRn_`
    returnRn ()

reportableUnusedName :: Name -> Bool
reportableUnusedName name
  = explicitlyImported (getNameProvenance name) &&
    not (startsWithUnderscore (occNameUserString (nameOccName name)))
  where
    explicitlyImported (LocalDef _ _) 		             = True	-- Report unused defns of local vars
    explicitlyImported (NonLocalDef (UserImport _ _ expl) _) = expl 	-- Report unused explicit imports
    explicitlyImported other			             = False	-- Don't report others
   
	-- Haskell 98 encourages compilers to suppress warnings about
	-- unused names in a pattern if they start with "_".
    startsWithUnderscore ('_' : _) = True	-- Suppress warnings for names starting
    startsWithUnderscore other     = False	-- with an underscore

rnStats :: [RenamedHsDecl] -> RnMG ()
rnStats imp_decls
        | opt_D_dump_rn_trace || 
	  opt_D_dump_rn_stats ||
	  opt_D_dump_rn 
 	= getRnStats imp_decls		`thenRn` \ msg ->
	  ioToRnM (printErrs msg)	`thenRn_`
	  returnRn ()

	| otherwise = returnRn ()
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

