%
% (c) The GRASP Project, Glasgow University, 1992-1998
%
\section[Rename]{Renaming and dependency analysis passes}

\begin{code}
module Rename ( renameModule ) where

#include "HsVersions.h"

import HsSyn
import RdrHsSyn		( RdrNameHsModule )
import RnHsSyn		( RenamedHsModule, RenamedHsDecl, extractHsTyNames )

import CmdLineOpts	( opt_HiMap, opt_D_show_rn_trace,
			  opt_D_dump_rn, opt_D_show_rn_stats,
			  opt_WarnUnusedBinds, opt_WarnUnusedImports
		        )
import RnMonad
import RnNames		( getGlobalNames )
import RnSource		( rnIfaceDecl, rnSourceDecls )
import RnIfaces		( getImportedInstDecls, importDecl, getImportVersions, getSpecialInstModules,
			  getDeferredDataDecls,
			  mkSearchPath, getSlurpedNames, getRnStats
			)
import RnEnv		( addImplicitOccsRn, availName, availNames, availsToNameSet, 
			  warnUnusedTopNames
			)
import Name		( Name, isLocallyDefined,
			  NamedThing(..), ImportReason(..), Provenance(..),
			  nameModule, pprModule, pprOccName, nameOccName,
			  getNameProvenance, occNameUserString, 
			)
import RdrName		( RdrName )
import NameSet
import TyCon		( TyCon )
import PrelMods		( mAIN, pREL_MAIN )
import TysWiredIn	( unitTyCon, intTyCon, doubleTyCon )
import PrelInfo		( ioTyCon_NAME, thinAirIdNames )
import ErrUtils		( pprBagOfErrors, pprBagOfWarnings,
			  doIfSet, dumpIfSet, ghcExit
			)
import Bag		( isEmptyBag )
import FiniteMap	( fmToList, delListFromFM )
import UniqSupply	( UniqSupply )
import Util		( equivClasses )
import Maybes		( maybeToBool )
import Outputable
\end{code}



\begin{code}
renameModule :: UniqSupply
	     -> RdrNameHsModule
	     -> IO (Maybe 
	              ( RenamedHsModule   -- Output, after renaming
		      , InterfaceDetails  -- Interface; for interface file generatino
		      , RnNameSupply      -- Final env; for renaming derivings
		      , [Module]	  -- Imported modules; for profiling
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
	Just results@(rn_mod, _, _, _)
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
  	Just (export_env, rn_env, global_avail_env) = maybe_stuff
    in

	-- RENAME THE SOURCE
    initRnMS rn_env SourceMode (
	addImplicits mod_name				`thenRn_`
	rnSourceDecls local_decls
    )							`thenRn` \ (rn_local_decls, fvs) ->

	-- SLURP IN ALL THE NEEDED DECLARATIONS
    slurpDecls rn_local_decls		`thenRn` \ rn_all_decls ->

	-- EXIT IF ERRORS FOUND
    checkErrsRn				`thenRn` \ no_errs_so_far ->
    if not no_errs_so_far then
	-- Found errors already, so exit now
	rnStats []		`thenRn_`
	returnRn Nothing
    else

	-- GENERATE THE VERSION/USAGE INFO
    getImportVersions mod_name exports			`thenRn` \ import_versions ->
    getNameSupplyRn					`thenRn` \ name_supply ->

	-- REPORT UNUSED NAMES
    reportUnusedNames rn_env global_avail_env
		      export_env
		      fvs				`thenRn_`

	-- GENERATE THE SPECIAL-INSTANCE MODULE LIST
	-- The "special instance" modules are those modules that contain instance
	-- declarations that contain no type constructor or class that was declared
	-- in that module.
    getSpecialInstModules				`thenRn` \ imported_special_inst_mods ->
    let
	special_inst_decls = [d | InstD d@(InstDecl inst_ty _ _ _ _) <- rn_local_decls,
				  all (not.isLocallyDefined) (nameSetToList (extractHsTyNames inst_ty))
			     ]
	special_inst_mods | null special_inst_decls = imported_special_inst_mods
			  | otherwise		    = mod_name : imported_special_inst_mods
    in
		  
    
	-- RETURN THE RENAMED MODULE
    let
	import_mods = [mod | ImportDecl mod _ _ _ _ <- imports]

	renamed_module = HsModule mod_name vers 
				  trashed_exports trashed_imports
				  rn_all_decls
			          loc
    in
    rnStats rn_all_decls	`thenRn_`
    returnRn (Just (renamed_module, 
		    (import_versions, export_env, special_inst_mods),
		     name_supply,
		     import_mods))
  where
    trashed_exports  = {-trace "rnSource:trashed_exports"-} Nothing
    trashed_imports  = {-trace "rnSource:trashed_imports"-} []
\end{code}

@addImplicits@ forces the renamer to slurp in some things which aren't
mentioned explicitly, but which might be needed by the type checker.

\begin{code}
addImplicits mod_name
  = addImplicitOccsRn (implicit_main ++ default_tys ++ thinAirIdNames)
  where
	-- Add occurrences for Int, Double, and (), because they
	-- are the types to which ambigious type variables may be defaulted by
	-- the type checker; so they won't always appear explicitly.
	-- [The () one is a GHC extension for defaulting CCall results.]
    default_tys = [getName intTyCon, getName doubleTyCon, getName unitTyCon ]

	-- Add occurrences for IO or PrimIO
    implicit_main |  mod_name == mAIN
		  || mod_name == pREL_MAIN = [ioTyCon_NAME]
		  |  otherwise 		   = []
\end{code}


\begin{code}
slurpDecls decls
  = 	-- First of all, get all the compulsory decls
    slurp_compulsories decls	`thenRn` \ decls1 ->

	-- Next get the optional ones
    closeDecls optional_mode decls1	`thenRn` \ decls2 ->

	-- Finally get those deferred data type declarations
    getDeferredDataDecls				`thenRn` \ data_decls ->
    mapRn (rn_data_decl compulsory_mode) data_decls	`thenRn` \ rn_data_decls ->

	-- Done
    returnRn (rn_data_decls ++ decls2)

  where
    compulsory_mode = InterfaceMode Compulsory
    optional_mode   = InterfaceMode Optional

	-- The "slurp_compulsories" function is a loop that alternates
	-- between slurping compulsory decls and slurping the instance
	-- decls thus made relavant.
        -- We *must* loop again here.  Why?  Two reasons:
	-- (a) an instance decl will give rise to an unresolved dfun, whose
	--	decl we must slurp to get its version number; that's the version
	-- 	number for the whole instance decl.  (And its unfolding might mention new
	--  unresolved names.)
	-- (b) an instance decl might give rise to a new unresolved class,
	-- 	whose decl we must slurp, which might let in some new instance decls,
	--	and so on.  Example:  instance Foo a => Baz [a] where ...
    slurp_compulsories decls
      = closeDecls compulsory_mode decls	`thenRn` \ decls1 ->
	
		-- Instance decls still pending?
        getImportedInstDecls			`thenRn` \ inst_decls ->
	if null inst_decls then 
		-- No, none
	    returnRn decls1
	else
		-- Yes, there are some, so rename them and loop
	     traceRn (sep [ptext SLIT("Slurped"), int (length inst_decls), ptext SLIT("instance decls")])
								`thenRn_`
	     mapRn (rn_inst_decl compulsory_mode) inst_decls	`thenRn` \ new_inst_decls ->
    	     slurp_compulsories (new_inst_decls ++ decls1)
\end{code}

\begin{code}
closeDecls :: RnMode
	   -> [RenamedHsDecl]			-- Declarations got so far
	   -> RnMG [RenamedHsDecl]		-- input + extra decls slurped
	-- The monad includes a list of possibly-unresolved Names
	-- This list is empty when closeDecls returns

closeDecls mode decls 
  = popOccurrenceName mode		`thenRn` \ maybe_unresolved ->
    case maybe_unresolved of

	-- No more unresolved names
	Nothing -> returnRn decls
			
	-- An unresolved name
	Just name_w_loc
	  -> 	-- Slurp its declaration, if any
--	     traceRn (sep [ptext SLIT("Considering"), ppr name_w_loc])	`thenRn_`
	     importDecl name_w_loc mode		`thenRn` \ maybe_decl ->
	     case maybe_decl of

		-- No declaration... (wired in thing or optional)
		Nothing   -> closeDecls mode decls

		-- Found a declaration... rename it
		Just decl -> rn_iface_decl mod_name mode decl	`thenRn` \ new_decl ->
			     closeDecls mode (new_decl : decls)
			 where
		           mod_name = nameModule (fst name_w_loc)

rn_iface_decl mod_name mode decl
  = setModuleRn mod_name $
    initRnMS emptyRnEnv mode (rnIfaceDecl decl)
					
rn_inst_decl mode (mod_name,decl)    = rn_iface_decl mod_name mode (InstD decl)
rn_data_decl mode (mod_name,ty_decl) = rn_iface_decl mod_name mode (TyClD ty_decl)
\end{code}

\begin{code}
reportUnusedNames (RnEnv gbl_env _) avail_env (ExportEnv export_avails _) mentioned_names
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
rnStats all_decls
        | opt_D_show_rn_trace || 
	  opt_D_show_rn_stats ||
	  opt_D_dump_rn 
 	= getRnStats all_decls		`thenRn` \ msg ->
	  ioToRnMG (printErrs msg)	`thenRn_`
	  returnRn ()

	| otherwise = returnRn ()
\end{code}

