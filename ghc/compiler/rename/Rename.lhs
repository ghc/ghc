%
% (c) The GRASP Project, Glasgow University, 1992-1998
%
\section[Rename]{Renaming and dependency analysis passes}

\begin{code}
module Rename ( renameModule ) where

#include "HsVersions.h"

import HsSyn
import RdrHsSyn		( RdrNameHsModule, RdrNameHsDecl, RdrNameDeprecation )
import RnHsSyn		( RenamedHsDecl, 
			  extractHsTyNames, extractHsCtxtTyNames
			)

import CmdLineOpts	( DynFlags, DynFlag(..) )
import RnMonad
import RnNames		( getGlobalNames )
import RnSource		( rnSourceDecls, rnDecl )
import RnIfaces		( getImportedInstDecls, importDecl, mkImportInfo, 
			  getInterfaceExports,
			  getImportedRules, getSlurped,
			  ImportDeclResult(..)
			)
import RnHiFiles	( removeContext )
import RnEnv		( availName, availsToNameSet, 
			  emptyAvailEnv, unitAvailEnv, availEnvElts, plusAvailEnv, sortAvails,
			  warnUnusedImports, warnUnusedLocalBinds, warnUnusedModules,
			  lookupOrigNames, lookupGlobalRn, 
			  FreeVars, plusFVs, plusFV, unitFV, emptyFVs, isEmptyFVs, addOneFV
			)
import Module           ( Module, ModuleName, WhereFrom(..),
			  moduleNameUserString, moduleName, 
			  lookupModuleEnv
			)
import Name		( Name, isLocallyDefined, NamedThing(..), getSrcLoc,
			  nameOccName, nameUnique, nameModule,
			  mkNameEnv, nameEnvElts, extendNameEnv
			)
import OccName		( occNameFlavour )
import Id		( idType )
import TyCon		( isSynTyCon, getSynTyConDefn )
import NameSet
import TysWiredIn	( unitTyCon, intTyCon, doubleTyCon, boolTyCon )
import PrelNames	( mAIN_Name, pREL_MAIN_Name, pRELUDE_Name,
			  ioTyCon_RDR,
			  unpackCString_RDR, unpackCStringFoldr_RDR, unpackCStringUtf8_RDR,
			  eqString_RDR
			)
import PrelInfo		( fractionalClassKeys, derivingOccurrences, wiredInThingEnv )
import Type		( namesOfType, funTyCon )
import ErrUtils		( dumpIfSet )
import Bag		( bagToList )
import FiniteMap	( FiniteMap, eltsFM, fmToList, emptyFM, lookupFM, 
			  addToFM_C, elemFM, addToFM
			)
import UniqFM		( lookupUFM )
import Maybes		( maybeToBool, catMaybes )
import Outputable
import IO		( openFile, IOMode(..) )
import HscTypes		( Finder, PersistentCompilerState, HomeIfaceTable, HomeSymbolTable, 
			  ModIface(..), TyThing(..),
			  GlobalRdrEnv, AvailEnv, Avails, GenAvailInfo(..), AvailInfo, 
			  Provenance(..), ImportReason(..), initialVersionInfo,
			  Deprecations(..), lookupDeprec
			 )
import List		( partition, nub )
\end{code}



\begin{code}
renameModule :: DynFlags -> Finder 
	     -> HomeIfaceTable -> HomeSymbolTable
	     -> PersistentCompilerState 
	     -> Module -> RdrNameHsModule 
	     -> IO (PersistentCompilerState, Maybe (ModIface, [RenamedHsDecl]))

renameModule dflags finder hit hst old_pcs this_module rdr_module
  = 	-- Initialise the renamer monad
    do {
	(new_pcs, errors_found, (maybe_rn_stuff, dump_action)) 
	   <- initRn dflags finder hit hst old_pcs this_module (rename this_module rdr_module) ;

	-- Dump any debugging output
	dump_action ;

	-- Return results
	if errors_found then
	    return (old_pcs, Nothing)
        else
	    return (new_pcs, maybe_rn_stuff)
    }
\end{code}

\begin{code}
rename :: Module -> RdrNameHsModule -> RnMG (Maybe (ModIface, [RenamedHsDecl]), IO ())
rename this_module this_mod@(HsModule mod_name vers exports imports local_decls mod_deprec loc)
  =  	-- FIND THE GLOBAL NAME ENVIRONMENT
    getGlobalNames this_mod			`thenRn` \ maybe_stuff ->

	-- CHECK FOR EARLY EXIT
    case maybe_stuff of {
	Nothing -> 	-- Everything is up to date; no need to recompile further
		rnDump [] []		`thenRn` \ dump_action ->
		returnRn (Nothing, dump_action) ;

  	Just (gbl_env, local_gbl_env, export_avails, global_avail_env) ->

	-- DEAL WITH DEPRECATIONS
    rnDeprecs local_gbl_env mod_deprec 
	      [d | DeprecD d <- local_decls]		`thenRn` \ my_deprecs ->

	-- DEAL WITH LOCAL FIXITIES
    fixitiesFromLocalDecls local_gbl_env local_decls	`thenRn` \ local_fixity_env ->

	-- RENAME THE SOURCE
    initRnMS gbl_env local_fixity_env SourceMode (
	rnSourceDecls local_decls
    )					`thenRn` \ (rn_local_decls, source_fvs) ->

	-- SLURP IN ALL THE NEEDED DECLARATIONS
    implicitFVs mod_name rn_local_decls 	`thenRn` \ implicit_fvs -> 
    let
		-- The export_fvs make the exported names look just as if they
		-- occurred in the source program.  For the reasoning, see the
		-- comments with RnIfaces.getImportVersions.
		-- We only need the 'parent name' of the avail;
		-- that's enough to suck in the declaration.
	export_fvs 	= mkNameSet (map availName export_avails)
	real_source_fvs = source_fvs `plusFV` export_fvs

	slurp_fvs	= implicit_fvs `plusFV` real_source_fvs
		-- It's important to do the "plus" this way round, so that
		-- when compiling the prelude, locally-defined (), Bool, etc
		-- override the implicit ones. 
    in
    slurpImpDecls slurp_fvs		`thenRn` \ rn_imp_decls ->

	-- EXIT IF ERRORS FOUND
    rnDump rn_imp_decls rn_local_decls		`thenRn` \ dump_action ->
    checkErrsRn					`thenRn` \ no_errs_so_far ->
    if not no_errs_so_far then
	-- Found errors already, so exit now
	returnRn (Nothing, dump_action)
    else

	-- GENERATE THE VERSION/USAGE INFO
    mkImportInfo mod_name imports 	`thenRn` \ my_usages ->

	-- RETURN THE RENAMED MODULE
    getNameSupplyRn			`thenRn` \ name_supply ->
    getIfacesRn 			`thenRn` \ ifaces ->
    let
	direct_import_mods :: [ModuleName]
	direct_import_mods = nub [m | ImportDecl m _ _ _ _ _ <- imports]

	-- We record fixities even for things that aren't exported,
	-- so that we can change into the context of this moodule easily
	fixities = mkNameEnv [ (name, fixity)
			     | FixitySig name fixity loc <- nameEnvElts local_fixity_env
			     ]


	-- Sort the exports to make them easier to compare for versions
	my_exports = sortAvails export_avails
	
	mod_iface = ModIface {	mi_module   = this_module,
				mi_version  = initialVersionInfo,
				mi_orphan   = any isOrphanDecl rn_local_decls,
				mi_exports  = my_exports,
				mi_globals  = gbl_env,
				mi_usages   = my_usages,
				mi_fixities = fixities,
				mi_deprecs  = my_deprecs,
				mi_decls    = panic "mi_decls"
		    }

	final_decls = rn_local_decls ++ rn_imp_decls
    in

	-- REPORT UNUSED NAMES, AND DEBUG DUMP 
    reportUnusedNames mod_name direct_import_mods
		      gbl_env global_avail_env
		      export_avails source_fvs
		      rn_imp_decls			`thenRn_`

    returnRn (Just (mod_iface, final_decls), dump_action) }
\end{code}

@implicitFVs@ forces the renamer to slurp in some things which aren't
mentioned explicitly, but which might be needed by the type checker.

\begin{code}
implicitFVs mod_name decls
  = lookupOrigNames implicit_occs			`thenRn` \ implicit_names ->
    returnRn (mkNameSet (map getName default_tycons)	`plusFV`
	      implicit_names)
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
		  || mod_name == pREL_MAIN_Name = [ioTyCon_RDR]
		  |  otherwise 		        = []

	-- Now add extra "occurrences" for things that
	-- the deriving mechanism, or defaulting, will later need in order to
	-- generate code
    implicit_occs = string_occs ++ foldr ((++) . get) implicit_main decls

	-- Virtually every program has error messages in it somewhere
    string_occs = [unpackCString_RDR, unpackCStringFoldr_RDR, unpackCStringUtf8_RDR,
		   eqString_RDR]

    get (TyClD (TyData _ _ _ _ _ _ (Just deriv_classes) _ _ _))
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

isOrphanDecl (RuleD (HsRule _ _ _ lhs _ _))
  = check lhs
  where
	-- At the moment we just check for common LHS forms
	-- Expand as necessary.  Getting it wrong just means
	-- more orphans than necessary
    check (HsVar v)   	  = not (isLocallyDefined v)
    check (HsApp f a) 	  = check f && check a
    check (HsLit _)   	  = False
    check (HsOverLit _)	  = False
    check (OpApp l o _ r) = check l && check o && check r
    check (NegApp e _)    = check e
    check (HsPar e)	  = check e
    check (SectionL e o)  = check e && check o
    check (SectionR o e)  = check e && check o

    check other	      	  = True 	-- Safe fall through

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
    slurpSourceRefs source_binders source_fvs	`thenRn` \ (decls, needed) ->

	-- Then get everything else
    closeDecls decls needed			`thenRn` \ decls1 ->

	-- Finally, get any deferred data type decls
    slurpDeferredDecls decls1			`thenRn` \ final_decls -> 

    returnRn final_decls

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
	  foldlRn go_inner (decls, fvs, emptyFVs) refs		`thenRn` \ (decls1, fvs1, gates1) ->
	  getImportedInstDecls (all_gates `plusFV` gates1)	`thenRn` \ inst_decls ->
	  rnInstDecls decls1 fvs1 gates1 inst_decls		`thenRn` \ (decls2, fvs2, gates2) ->
	  go_outer decls2 fvs2 (all_gates `plusFV` gates2)
			       (nameSetToList (gates2 `minusNameSet` all_gates))
		-- Knock out the all_gates because even if we don't slurp any new
		-- decls we can get some apparently-new gates from wired-in names

    go_inner (decls, fvs, gates) wanted_name
	= importDecl wanted_name 		`thenRn` \ import_result ->
	  case import_result of
	    AlreadySlurped -> returnRn (decls, fvs, gates)
	    WiredIn        -> returnRn (decls, fvs, gates `plusFV` getWiredInGates wanted_name)
	    Deferred       -> returnRn (decls, fvs, gates `addOneFV` wanted_name)	-- It's a type constructor
			
	    HereItIs decl -> rnIfaceDecl decl		`thenRn` \ (new_decl, fvs1) ->
			     returnRn (new_decl : decls, 
				       fvs1 `plusFV` fvs,
			   	       gates `plusFV` getGates source_fvs new_decl)

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
  = importDecl wanted_name 		`thenRn` \ import_result ->
    case import_result of
	-- Found a declaration... rename it
	HereItIs decl -> rnIfaceDecl decl		`thenRn` \ (new_decl, fvs1) ->
			 returnRn (new_decl:decls, fvs1 `plusFV` fvs)

	-- No declaration... (wired in thing, or deferred, or already slurped)
	other -> returnRn (decls, fvs)


-------------------------------------------------------
rnIfaceDecls :: [RenamedHsDecl] -> FreeVars
	     -> [(Module, RdrNameHsDecl)]
	     -> RnM d ([RenamedHsDecl], FreeVars)
rnIfaceDecls decls fvs []     = returnRn (decls, fvs)
rnIfaceDecls decls fvs (d:ds) = rnIfaceDecl d		`thenRn` \ (new_decl, fvs1) ->
				rnIfaceDecls (new_decl:decls) (fvs1 `plusFV` fvs) ds

rnIfaceDecl (mod, decl) = initIfaceRnMS mod (rnDecl decl)	
\end{code}


%*********************************************************
%*						 	 *
\subsection{Deferred declarations}
%*							 *
%*********************************************************

The idea of deferred declarations is this.  Suppose we have a function
	f :: T -> Int
	data T = T1 A | T2 B
	data A = A1 X | A2 Y
	data B = B1 P | B2 Q
Then we don't want to load T and all its constructors, and all
the types those constructors refer to, and all the types *those*
constructors refer to, and so on.  That might mean loading many more
interface files than is really necessary.  So we 'defer' loading T.

But f might be strict, and the calling convention for evaluating
values of type T depends on how many constructors T has, so 
we do need to load T, but not the full details of the type T.
So we load the full decl for T, but only skeleton decls for A and B:
	f :: T -> Int
	data T = {- 2 constructors -}

Whether all this is worth it is moot.

\begin{code}
slurpDeferredDecls :: [RenamedHsDecl] -> RnMG [RenamedHsDecl]
slurpDeferredDecls decls = returnRn decls

{-	OMIT FOR NOW
slurpDeferredDecls :: [RenamedHsDecl] -> RnMG [RenamedHsDecl]
slurpDeferredDecls decls
  = getDeferredDecls						`thenRn` \ def_decls ->
    rnIfaceDecls decls emptyFVs (map stripDecl def_decls)	`thenRn` \ (decls1, fvs) ->
    ASSERT( isEmptyFVs fvs )
    returnRn decls1

stripDecl (mod, TyClD (TyData dt _ tc tvs _ nconstrs _ loc name1 name2))
  = (mod, TyClD (TyData dt [] tc tvs [] nconstrs Nothing loc
		name1 name2))
	-- Nuke the context and constructors
	-- But retain the *number* of constructors!
	-- Also the tvs will have kinds on them.
-}
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

getGates source_fvs (TyClD (ClassDecl ctxt cls tvs _ sigs _ _ _ ))
  = (delListFromNameSet (foldr (plusFV . get) (extractHsCtxtTyNames ctxt) sigs)
		        (hsTyVarNames tvs)
     `addOneToNameSet` cls)
    `plusFV` maybe_double
  where
    get (ClassOpSig n _ ty _) 
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
		       (hsTyVarNames tvs)
	-- A type synonym type constructor isn't a "gate" for instance decls

getGates source_fvs (TyClD (TyData _ ctxt tycon tvs cons _ _ _ _ _))
  = delListFromNameSet (foldr (plusFV . get) (extractHsCtxtTyNames ctxt) cons)
		       (hsTyVarNames tvs)
    `addOneToNameSet` tycon
  where
    get (ConDecl n _ tvs ctxt details _)
	| n `elemNameSet` source_fvs
		-- If the constructor is method, get fvs from all its fields
	= delListFromNameSet (get_details details `plusFV` 
		  	      extractHsCtxtTyNames ctxt)
			     (hsTyVarNames tvs)
    get (ConDecl n _ tvs ctxt (RecCon fields) _)
		-- Even if the constructor isn't mentioned, the fields
		-- might be, as selectors.  They can't mention existentially
		-- bound tyvars (typechecker checks for that) so no need for 
		-- the deleteListFromNameSet part
	= foldr (plusFV . get_field) emptyFVs fields
	
    get other_con = emptyFVs

    get_details (VanillaCon tys) = plusFVs (map get_bang tys)
    get_details (InfixCon t1 t2) = get_bang t1 `plusFV` get_bang t2
    get_details (RecCon fields)  = plusFVs [get_bang t | (_, t) <- fields]

    get_field (fs,t) | any (`elemNameSet` source_fvs) fs = get_bang t
		     | otherwise			 = emptyFVs

    get_bang bty = extractHsTyNames (getBangType bty)

getGates source_fvs other_decl = emptyFVs
\end{code}

@getWiredInGates@ is just like @getGates@, but it sees a wired-in @Name@
rather than a declaration.

\begin{code}
getWiredInGates :: Name -> FreeVars
getWiredInGates name 	-- No classes are wired in
  = case lookupNameEnv wiredInThingEnv name of
	Just (AnId the_id) -> getWiredInGates_s (namesOfType (idType the_id))

	Just (ATyCon tc)
	  |  isSynTyCon tc
	  -> getWiredInGates_s (delListFromNameSet (namesOfType ty) (map getName tyvars))
	  where
	     (tyvars,ty)  = getSynTyConDefn tc

	other -> unitFV name

getWiredInGates_s names = foldr (plusFV . getWiredInGates) emptyFVs (nameSetToList names)
\end{code}

\begin{code}
getInstDeclGates (InstD (InstDecl inst_ty _ _ _ _)) = extractHsTyNames inst_ty
getInstDeclGates other				    = emptyFVs
\end{code}


%*********************************************************
%*						 	 *
\subsection{Fixities}
%*							 *
%*********************************************************

\begin{code}
fixitiesFromLocalDecls :: GlobalRdrEnv -> [RdrNameHsDecl] -> RnMG LocalFixityEnv
fixitiesFromLocalDecls gbl_env decls
  = doptRn Opt_WarnUnusedBinds				  `thenRn` \ warn_unused ->
    foldlRn (getFixities warn_unused) emptyNameEnv decls  `thenRn` \ env -> 
    traceRn (text "fixity env" <+> vcat (map ppr (nameEnvElts env)))
							  `thenRn_`
    returnRn env
  where
    getFixities :: Bool -> LocalFixityEnv -> RdrNameHsDecl -> RnMG LocalFixityEnv
    getFixities warn_uu acc (FixD fix)
      = fix_decl warn_uu acc fix

    getFixities warn_uu acc (TyClD (ClassDecl _ _ _ _ sigs _ _ _ ))
      = foldlRn (fix_decl warn_uu) acc [sig | FixSig sig <- sigs]
		-- Get fixities from class decl sigs too.
    getFixities warn_uu acc other_decl
      = returnRn acc

    fix_decl warn_uu acc sig@(FixitySig rdr_name fixity loc)
	= 	-- Check for fixity decl for something not declared
	  pushSrcLocRn loc 			$
	  lookupGlobalRn gbl_env rdr_name	`thenRn` \  maybe_name ->
	  case maybe_name of {
	    Nothing ->	checkRn (not warn_uu) (unusedFixityDecl rdr_name fixity)	`thenRn_` 
			returnRn acc ;

	    Just name ->

		-- Check for duplicate fixity decl
	  case lookupNameEnv acc name of {
	    Just (FixitySig _ _ loc') -> addErrRn (dupFixityDecl rdr_name loc loc')
					 `thenRn_` returnRn acc ;

	    Nothing -> returnRn (extendNameEnv acc name (FixitySig name fixity loc))
	  }}
\end{code}


%*********************************************************
%*						 	 *
\subsection{Deprecations}
%*							 *
%*********************************************************

For deprecations, all we do is check that the names are in scope.
It's only imported deprecations, dealt with in RnIfaces, that we
gather them together.

\begin{code}
rnDeprecs :: GlobalRdrEnv -> Maybe DeprecTxt
	   -> [RdrNameDeprecation] -> RnMG Deprecations
rnDeprecs gbl_env Nothing []
 = returnRn NoDeprecs

rnDeprecs gbl_env (Just txt) decls
 = mapRn (addErrRn . badDeprec) decls 	`thenRn_` 
   returnRn (DeprecAll txt)

rnDeprecs gbl_env Nothing decls
  = mapRn rn_deprec decls	`thenRn` \ pairs ->
    returnRn (DeprecSome (mkNameEnv (catMaybes pairs)))
 where
   rn_deprec (Deprecation rdr_name txt loc)
     = pushSrcLocRn loc			$
       lookupGlobalRn gbl_env rdr_name	`thenRn` \ maybe_name ->
       case maybe_name of
	 Just n  -> returnRn (Just (n,txt))
	 Nothing -> returnRn Nothing
\end{code}


%*********************************************************
%*						 	 *
\subsection{Unused names}
%*							 *
%*********************************************************

\begin{code}
reportUnusedNames :: ModuleName -> [ModuleName] 
		  -> GlobalRdrEnv -> AvailEnv
		  -> Avails -> NameSet -> [RenamedHsDecl] 
		  -> RnMG ()
reportUnusedNames mod_name direct_import_mods 
		  gbl_env avail_env 
		  export_avails mentioned_names
		  imported_decls
  = warnUnusedModules unused_imp_mods				`thenRn_`
    warnUnusedLocalBinds bad_locals				`thenRn_`
    warnUnusedImports bad_imp_names				`thenRn_`
    printMinimalImports mod_name minimal_imports		`thenRn_`
    warnDeprecations really_used_names				`thenRn_`
    returnRn ()

  where
    used_names = mentioned_names `unionNameSets` availsToNameSet export_avails
    
    -- Now, a use of C implies a use of T,
    -- if C was brought into scope by T(..) or T(C)
    really_used_names = used_names `unionNameSets`
      mkNameSet [ parent_name
	        | sub_name <- nameSetToList used_names
    
    		-- Usually, every used name will appear in avail_env, but there 
    		-- is one time when it doesn't: tuples and other built in syntax.  When you
    		-- write (a,b) that gives rise to a *use* of "(,)", so that the
    		-- instances will get pulled in, but the tycon "(,)" isn't actually
    		-- in scope.  Also, (-x) gives rise to an implicit use of 'negate'; 
    		-- similarly,   3.5 gives rise to an implcit use of :%
    		-- Hence the silent 'False' in all other cases
    	      
	        , Just parent_name <- [case lookupNameEnv avail_env sub_name of
			    		Just (AvailTC n _) -> Just n
			    		other		   -> Nothing]
    	    ]
    
    defined_names, defined_and_used, defined_but_not_used :: [(Name,Provenance)]
    defined_names			     = concat (rdrEnvElts gbl_env)
    (defined_and_used, defined_but_not_used) = partition used defined_names
    used (name,_)	  		     = not (name `elemNameSet` really_used_names)
    
    -- Filter out the ones only defined implicitly
    bad_locals :: [Name]
    bad_locals     = [n     | (n,LocalDef) <- defined_but_not_used]
    
    bad_imp_names :: [(Name,Provenance)]
    bad_imp_names  = [(n,p) | (n,p@(NonLocalDef (UserImport mod _ True) _)) <- defined_but_not_used,
  	  		      not (module_unused mod)]
    
    -- inst_mods are directly-imported modules that 
    --	contain instance decl(s) that the renamer decided to suck in
    -- It's not necessarily redundant to import such modules.
    --
    -- NOTE: Consider 
    --	      module This
    --		import M ()
    --
    --	 The import M() is not *necessarily* redundant, even if
    -- 	 we suck in no instance decls from M (e.g. it contains 
    --	 no instance decls, or This contains no code).  It may be 
    --	 that we import M solely to ensure that M's orphan instance 
    --	 decls (or those in its imports) are visible to people who 
    --	 import This.  Sigh. 
    --	 There's really no good way to detect this, so the error message 
    --	 in RnEnv.warnUnusedModules is weakened instead
    inst_mods :: [ModuleName]
    inst_mods = [m | InstD (InstDecl _ _ _ (Just dfun) _) <- imported_decls,
    		 let m = moduleName (nameModule dfun),
    		 m `elem` direct_import_mods
    	    ]
    
    -- To figure out the minimal set of imports, start with the things
    -- that are in scope (i.e. in gbl_env).  Then just combine them
    -- into a bunch of avails, so they are properly grouped
    minimal_imports :: FiniteMap ModuleName AvailEnv
    minimal_imports0 = emptyFM
    minimal_imports1 = foldr add_name     minimal_imports0 defined_and_used
    minimal_imports  = foldr add_inst_mod minimal_imports1 inst_mods
    
    add_name (n,NonLocalDef (UserImport m _ _) _) acc = addToFM_C plusAvailEnv acc (moduleName (nameModule n))
					    			  (unitAvailEnv (mk_avail n))
    add_name (n,other_prov)			  acc = acc

    mk_avail n = case lookupNameEnv avail_env n of
    		Just (AvailTC m _) | n==m      -> AvailTC n [n]
    				   | otherwise -> AvailTC m [n,m]
    		Just avail	   -> Avail n
    		Nothing		   -> pprPanic "mk_avail" (ppr n)
    
    add_inst_mod m acc 
      | m `elemFM` acc = acc	-- We import something already
      | otherwise      = addToFM acc m emptyAvailEnv
    	-- Add an empty collection of imports for a module
    	-- from which we have sucked only instance decls
    
    -- unused_imp_mods are the directly-imported modules 
    -- that are not mentioned in minimal_imports
    unused_imp_mods = [m | m <- direct_import_mods,
    		       not (maybeToBool (lookupFM minimal_imports m)),
    		       m /= pRELUDE_Name]
    
    module_unused :: Module -> Bool
    module_unused mod = moduleName mod `elem` unused_imp_mods


warnDeprecations used_names
  = doptRn Opt_WarnDeprecations				`thenRn` \ warn_drs ->
    if not warn_drs then returnRn () else

    getIfacesRn						`thenRn` \ ifaces ->
    getHomeIfaceTableRn					`thenRn` \ hit ->
    let
	pit     = iPIT ifaces
	deprecs = [ (n,txt)
                  | n <- nameSetToList used_names,
                    Just txt <- [lookup_deprec hit pit n] ]
    in			  
    mapRn_ warnDeprec deprecs

  where
    lookup_deprec hit pit n
	= case lookupModuleEnv hit mod of
		Just iface -> lookupDeprec iface n
		Nothing	   -> case lookupModuleEnv pit mod of
				Just iface -> lookupDeprec iface n
				Nothing	   -> pprPanic "warnDeprecations:" (ppr n)
	where
	  mod = nameModule n

-- ToDo: deal with original imports with 'qualified' and 'as M' clauses
printMinimalImports mod_name imps
  = doptRn Opt_D_dump_minimal_imports		`thenRn` \ dump_minimal ->
    if not dump_minimal then returnRn () else

    mapRn to_ies (fmToList imps)		`thenRn` \ mod_ies ->
    ioToRnM (do { h <- openFile filename WriteMode ;
		  printForUser h (vcat (map ppr_mod_ie mod_ies))
	})					`thenRn_`
    returnRn ()
  where
    filename = moduleNameUserString mod_name ++ ".imports"
    ppr_mod_ie (mod_name, ies) 
	| mod_name == pRELUDE_Name 
	= empty
	| otherwise
	= ptext SLIT("import") <+> ppr mod_name <> 
			    parens (fsep (punctuate comma (map ppr ies)))

    to_ies (mod, avail_env) = mapRn to_ie (availEnvElts avail_env)	`thenRn` \ ies ->
			      returnRn (mod, ies)

    to_ie :: AvailInfo -> RnMG (IE Name)
    to_ie (Avail n)       = returnRn (IEVar n)
    to_ie (AvailTC n [m]) = ASSERT( n==m ) 
			    returnRn (IEThingAbs n)
    to_ie (AvailTC n ns)  = getInterfaceExports (moduleName (nameModule n)) 
						ImportBySystem	 	`thenRn` \ (_, avails) ->
			    case [ms | AvailTC m ms <- avails, m == n] of
			      [ms] | all (`elem` ns) ms -> returnRn (IEThingAll n)
				   | otherwise	        -> returnRn (IEThingWith n (filter (/= n) ns))
			      other -> pprTrace "to_ie" (ppr n <+> ppr (nameModule n) <+> ppr other) $
				       returnRn (IEVar n)

rnDump  :: [RenamedHsDecl] 	-- Renamed imported decls
	-> [RenamedHsDecl] 	-- Renamed local decls
	-> RnMG (IO ())
rnDump imp_decls local_decls
   = doptRn Opt_D_dump_rn_trace 	`thenRn` \ dump_rn_trace ->
     doptRn Opt_D_dump_rn_stats 	`thenRn` \ dump_rn_stats ->
     doptRn Opt_D_dump_rn 		`thenRn` \ dump_rn ->
     if dump_rn_trace || dump_rn_stats || dump_rn then
 	getRnStats imp_decls		`thenRn` \ stats_msg ->
	returnRn (printErrs stats_msg >> 
	          dumpIfSet dump_rn "Renamer:" 
			    (vcat (map ppr (local_decls ++ imp_decls))))
     else
	returnRn (return ())
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
	n_mods = length [() | (_, _, True) <- eltsFM (iImpModInfo ifaces)]

	decls_read     = [decl | (avail, True, (_,decl)) <- nameEnvElts (iDecls ifaces),
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


%************************************************************************
%*									*
\subsection{Errors and warnings}
%*									*
%************************************************************************

\begin{code}
warnDeprec :: (Name, DeprecTxt) -> RnM d ()
warnDeprec (name, txt)
  = pushSrcLocRn (getSrcLoc name)	$
    addWarnRn				$
    sep [ text (occNameFlavour (nameOccName name)) <+> ppr name <+>
          text "is deprecated:", nest 4 (ppr txt) ]


unusedFixityDecl rdr_name fixity
  = hsep [ptext SLIT("Unused fixity declaration for"), quotes (ppr rdr_name)]

dupFixityDecl rdr_name loc1 loc2
  = vcat [ptext SLIT("Multiple fixity declarations for") <+> quotes (ppr rdr_name),
	  ptext SLIT("at ") <+> ppr loc1,
	  ptext SLIT("and") <+> ppr loc2]

badDeprec d
  = sep [ptext SLIT("Illegal deprecation when whole module is deprecated"),
	 nest 4 (ppr d)]
\end{code}


