%
% (c) The GRASP Project, Glasgow University, 1992-1996
%
\section[Rename]{Renaming and dependency analysis passes}

\begin{code}
#include "HsVersions.h"

module Rename ( renameModule ) where

#if __GLASGOW_HASKELL__ <= 201
import PreludeGlaST	( thenPrimIO )
#else
import GlaExts
import IO
#endif

IMP_Ubiq()
IMPORT_1_3(List(partition))

import HsSyn
import RdrHsSyn		( RdrName(..), SYN_IE(RdrNameHsModule), SYN_IE(RdrNameImportDecl) )
import RnHsSyn		( SYN_IE(RenamedHsModule), SYN_IE(RenamedHsDecl), extractHsTyNames )

import CmdLineOpts	( opt_HiMap, opt_WarnNameShadowing, opt_D_show_rn_trace,
			  opt_D_dump_rn, opt_D_show_rn_stats,
			  opt_D_show_unused_imports, opt_PprUserLength
		        )
import RnMonad
import RnNames		( getGlobalNames )
import RnSource		( rnDecl )
import RnIfaces		( getImportedInstDecls, importDecl, getImportVersions, getSpecialInstModules,
			  getDeferredDataDecls,
			  mkSearchPath, getSlurpedNames, getRnStats
			)
import RnEnv		( availsToNameSet, addAvailToNameSet, 
			  addImplicitOccsRn, lookupImplicitOccRn )
import Id		( GenId {- instance NamedThing -} )
import Name		( Name, Provenance, ExportFlag(..), isLocallyDefined,
			  NameSet(..), elemNameSet, mkNameSet, unionNameSets, 
			  nameSetToList, minusNameSet, NamedThing(..),
			  nameModule, pprModule, pprOccName, nameOccName
			)
import TysWiredIn	( unitTyCon, intTyCon, doubleTyCon )
import PrelInfo		( ioTyCon_NAME, primIoTyCon_NAME )
import TyCon		( TyCon )
import PrelMods		( mAIN, gHC_MAIN )
import ErrUtils		( SYN_IE(Error), SYN_IE(Warning) )
import FiniteMap	( emptyFM, eltsFM, fmToList, addToFM, FiniteMap )
import Pretty
import Outputable	( Outputable(..), PprStyle(..) )
import Util		( cmpPString, equivClasses, panic, assertPanic, pprTrace )
#if __GLASGOW_HASKELL__ >= 202
import UniqSupply
#endif
\end{code}



\begin{code}
renameModule :: UniqSupply
	     -> RdrNameHsModule
	     -> IO (Maybe 			-- Nothing <=> everything up to date;
						-- no ned to recompile any further
			  (RenamedHsModule, 	-- Output, after renaming
			   InterfaceDetails,	-- Interface; for interface file generatino
			   RnNameSupply,	-- Final env; for renaming derivings
			   [Module]),	   	-- Imported modules; for profiling
		    Bag Error, 
		    Bag Warning
		   )
\end{code} 


\begin{code}
renameModule us this_mod@(HsModule mod_name vers exports imports fixities local_decls loc)
  = 	-- INITIALISE THE RENAMER MONAD
    initRn mod_name us (mkSearchPath opt_HiMap) loc $

 	-- FIND THE GLOBAL NAME ENVIRONMENT
    getGlobalNames this_mod			`thenRn` \ global_name_info ->

    case global_name_info of {
	Nothing -> 	-- Everything is up to date; no need to recompile further
			rnStats []		`thenRn_`
			returnRn Nothing ;

			-- Otherwise, just carry on
	Just (export_env, rn_env, explicit_names) ->

	-- RENAME THE SOURCE
    initRnMS rn_env mod_name SourceMode (
	addImplicits mod_name				`thenRn_`
	mapRn rnDecl local_decls
    )							`thenRn` \ rn_local_decls ->

	-- SLURP IN ALL THE NEEDED DECLARATIONS
    slurpDecls rn_local_decls				`thenRn` \ rn_all_decls ->


	-- GENERATE THE VERSION/USAGE INFO
    getImportVersions mod_name exports			`thenRn` \ import_versions ->
    getNameSupplyRn					`thenRn` \ name_supply ->

	-- REPORT UNUSED NAMES
    reportUnusedNames explicit_names			`thenRn_`

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
				  trashed_exports trashed_imports trashed_fixities
				  rn_all_decls
			          loc
    in
    rnStats rn_all_decls	`thenRn_`
    returnRn (Just (renamed_module, 
		    (import_versions, export_env, special_inst_mods),
		     name_supply,
		     import_mods))
    }
  where
    trashed_exports  = {-trace "rnSource:trashed_exports"-} Nothing
    trashed_imports  = {-trace "rnSource:trashed_imports"-} []
    trashed_fixities = []
\end{code}

@addImplicits@ forces the renamer to slurp in some things which aren't
mentioned explicitly, but which might be needed by the type checker.

\begin{code}
addImplicits mod_name
  = addImplicitOccsRn (implicit_main ++ default_tys)
  where
	-- Add occurrences for Int, Double, and (), because they
	-- are the types to which ambigious type variables may be defaulted by
	-- the type checker; so they won't every appear explicitly.
	-- [The () one is a GHC extension for defaulting CCall results.]
    default_tys = [getName intTyCon, getName doubleTyCon, getName unitTyCon]

	-- Add occurrences for IO or PrimIO
    implicit_main | mod_name == mAIN     = [ioTyCon_NAME]
		  | mod_name == gHC_MAIN = [primIoTyCon_NAME]
		  | otherwise 		 = []
\end{code}


\begin{code}
slurpDecls decls
  = 	-- First of all, get all the compulsory decls
    slurp_compulsories decls	`thenRn` \ decls1 ->

	-- Next get the optional ones
    closeDecls Optional decls1	`thenRn` \ decls2 ->

	-- Finally get those deferred data type declarations
    getDeferredDataDecls			`thenRn` \ data_decls ->
    mapRn rn_data_decl data_decls		`thenRn` \ rn_data_decls ->

	-- Done
    returnRn (rn_data_decls ++ decls2)

  where
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
      = closeDecls Compulsory decls	`thenRn` \ decls1 ->
	
		-- Instance decls still pending?
        getImportedInstDecls			`thenRn` \ inst_decls ->
	if null inst_decls then 
		-- No, none
	    returnRn decls1
	else
		-- Yes, there are some, so rename them and loop
	     traceRn (sep [ptext SLIT("Slurped"), int (length inst_decls), ptext SLIT("instance decls")])
						`thenRn_`
	     mapRn rn_inst_decl inst_decls	`thenRn` \ new_inst_decls ->
    	     slurp_compulsories (new_inst_decls ++ decls1)
\end{code}

\begin{code}
closeDecls :: Necessity
	   -> [RenamedHsDecl]			-- Declarations got so far
	   -> RnMG [RenamedHsDecl]		-- input + extra decls slurped
	-- The monad includes a list of possibly-unresolved Names
	-- This list is empty when closeDecls returns

closeDecls necessity decls 
  = popOccurrenceName necessity		`thenRn` \ maybe_unresolved ->
    case maybe_unresolved of

	-- No more unresolved names
	Nothing -> returnRn decls
			
	-- An unresolved name
	Just name
	  -> 	-- Slurp its declaration, if any
	     traceRn (sep [ptext SLIT("Considering"), ppr PprDebug name])	`thenRn_`
	     importDecl name necessity		`thenRn` \ maybe_decl ->
	     case maybe_decl of

		-- No declaration... (wired in thing or optional)
		Nothing   -> closeDecls necessity decls

		-- Found a declaration... rename it
		Just decl -> rn_iface_decl mod_name necessity decl	`thenRn` \ new_decl ->
			     closeDecls necessity (new_decl : decls)
			 where
		           mod_name = nameModule name


rn_iface_decl mod_name necessity decl	-- Notice that the rnEnv starts empty
  = initRnMS emptyRnEnv mod_name (InterfaceMode necessity) (rnDecl decl)
					
rn_inst_decl (mod_name,decl)      = rn_iface_decl mod_name Compulsory (InstD decl)

rn_data_decl (tycon_name,ty_decl) = rn_iface_decl mod_name Compulsory (TyD ty_decl)
				  where
				    mod_name = nameModule tycon_name
\end{code}

\begin{code}
reportUnusedNames explicit_avail_names
  | not opt_D_show_unused_imports
  = returnRn ()

  | otherwise
  = getSlurpedNames			`thenRn` \ slurped_names ->
    let
	unused	      = explicit_avail_names `minusNameSet` slurped_names
	(local_unused, imported_unused) = partition isLocallyDefined (nameSetToList unused)
	imports_by_module = equivClasses cmp imported_unused
	name1 `cmp` name2 = nameModule name1 `_CMP_STRING_` nameModule name2 

	pp_imp sty = sep [text "For information: the following unqualified imports are unused:",
			  nest 4 (vcat (map (pp_group sty) imports_by_module))]
	pp_group sty (n:ns) = sep [hcat [text "Module ", pprModule (PprForUser opt_PprUserLength) (nameModule n), char ':'],
				   nest 4 (sep (map (pprOccName sty . nameOccName) (n:ns)))]

	pp_local sty = sep [text "For information: the following local top-level definitions are unused:",
			    nest 4 (sep (map (pprOccName sty . nameOccName) local_unused))]
    in
    (if null imported_unused 
     then returnRn ()
     else addWarnRn pp_imp)	`thenRn_`

    (if null local_unused
     then returnRn ()
     else addWarnRn pp_local)

rnStats :: [RenamedHsDecl] -> RnMG ()
rnStats all_decls
        | opt_D_show_rn_trace || 
	  opt_D_show_rn_stats ||
	  opt_D_dump_rn 
 	= getRnStats all_decls		        `thenRn` \ msg ->
	  ioToRnMG (hPutStr stderr (show msg) >> 
		    hPutStr stderr "\n")	`thenRn_`
	  returnRn ()

	| otherwise = returnRn ()
\end{code}

