%
% (c) The GRASP Project, Glasgow University, 1992-1996
%
\section[Rename]{Renaming and dependency analysis passes}

\begin{code}
#include "HsVersions.h"

module Rename ( renameModule ) where

import PreludeGlaST	( thenPrimIO )

IMP_Ubiq()
IMPORT_1_3(List(partition))

import HsSyn
import RdrHsSyn		( RdrName, SYN_IE(RdrNameHsModule), SYN_IE(RdrNameImportDecl) )
import RnHsSyn		( SYN_IE(RenamedHsModule), SYN_IE(RenamedHsDecl), extractHsTyNames )

import CmdLineOpts	( opt_HiMap )
import RnMonad
import RnNames		( getGlobalNames )
import RnSource		( rnDecl )
import RnIfaces		( getImportedInstDecls, getDecl, getImportVersions, getSpecialInstModules,
			  mkSearchPath, getWiredInDecl
			)
import RnEnv		( availsToNameSet, addAvailToNameSet, 
			  addImplicitOccsRn, lookupImplicitOccRn )
import Id		( GenId {- instance NamedThing -} )
import Name		( Name, Provenance, ExportFlag(..), isLocallyDefined,
			  NameSet(..), elemNameSet, mkNameSet, unionNameSets, nameSetToList,
			  isWiredInName, modAndOcc
			)
import TysWiredIn	( unitTyCon, intTyCon, doubleTyCon )
import PrelInfo		( ioTyCon_NAME, primIoTyCon_NAME )
import TyCon		( TyCon )
import PrelMods		( mAIN, gHC_MAIN )
import ErrUtils		( SYN_IE(Error), SYN_IE(Warning) )
import FiniteMap	( emptyFM, eltsFM, fmToList, addToFM, FiniteMap )
import Pretty
import PprStyle		( PprStyle(..) )
import Util		( panic, assertPanic, pprTrace )
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
			returnRn Nothing ;

			-- Otherwise, just carry on
	Just (export_env, rn_env, local_avails) ->

	-- RENAME THE SOURCE
    initRnMS rn_env mod_name SourceMode (
	addImplicits mod_name				`thenRn_`
	mapRn rnDecl local_decls
    )							`thenRn` \ rn_local_decls ->

	-- SLURP IN ALL THE NEEDED DECLARATIONS
	-- Notice that the rnEnv starts empty
    closeDecls rn_local_decls (availsToNameSet local_avails) []
       						`thenRn` \ (rn_all_decls1, all_names1, imp_avails1) ->

	-- SLURP IN ALL NEEDED INSTANCE DECLARATIONS
	-- We extract instance decls that only mention things (type constructors, classes) that are
	-- already imported.  Those that don't can't possibly be useful to us.
	--
	-- We do another closeDecls, so that we can slurp info for the dictionary functions
	-- for the instance declaration.  These are *not* optional because the version number on
	-- the dfun acts as the version number for the instance declaration itself; if the
	-- instance decl changes, so will its dfun version number.
    getImportedInstDecls 				`thenRn` \ imported_insts ->
    let
	all_big_names = mkNameSet [name | Avail name _ <- local_avails]    `unionNameSets` 
			mkNameSet [name | Avail name _ <- imp_avails1]

	rn_needed_insts = [ initRnMS emptyRnEnv mod_name InterfaceMode (rnDecl (InstD inst_decl))
			  | (inst_names, mod_name, inst_decl) <- imported_insts,
			    all (`elemNameSet` all_big_names) inst_names
			  ]
    in
    sequenceRn rn_needed_insts				`thenRn` \ inst_decls ->
    closeDecls rn_all_decls1 all_names1 imp_avails1 	`thenRn` \ (rn_all_decls2, all_names2, imp_avails2) ->


	-- GENERATE THE VERSION/USAGE INFO
    getImportVersions imp_avails2			`thenRn` \ import_versions ->
    getNameSupplyRn					`thenRn` \ name_supply ->


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
				  (inst_decls ++ rn_all_decls2)
			          loc
    in
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
closeDecls :: [RenamedHsDecl]			-- Declarations got so far
	   -> NameSet				-- Names bound by those declarations
	   -> [AvailInfo]			-- Available stuff generated by closeDecls so far
	   -> RnMG ([RenamedHsDecl],		-- input + extra decls slurped
		    NameSet,			-- input + names bound by extra decls
		    [AvailInfo])		-- input + extra avails from extra decls
	-- The monad includes a list of possibly-unresolved Names
	-- This list is empty when closeDecls returns

closeDecls decls decl_names import_avails
  = popOccurrenceName		`thenRn` \ maybe_unresolved ->

    case maybe_unresolved of

	-- No more unresolved names; we're done
	Nothing ->	returnRn (decls, decl_names, import_avails)

	-- An "unresolved" name that we've already dealt with
	Just (name,_) | name `elemNameSet` decl_names 
	  -> closeDecls decls decl_names import_avails
	
	-- An unresolved name that's wired in.  In this case there's no 
	-- declaration to get, but we still want to record it as now available,
	-- so that we remember to look for instance declarations involving it.
	Just (name,_) | isWiredInName name
	  -> getWiredInDecl name	`thenRn` \ decl_avail ->
		     closeDecls decls 
				(addAvailToNameSet decl_names decl_avail)
				(decl_avail : import_avails)

	-- Genuinely unresolved name
	Just (name,necessity) | otherwise
	  -> getDecl name		`thenRn` \ (decl_avail,new_decl) ->
	     case decl_avail of

		-- Can't find the declaration; check that it was optional
		NotAvailable -> case necessity of { 
					Optional -> addWarnRn (getDeclWarn name);
					other	 -> addErrRn  (getDeclErr  name)
				}						`thenRn_` 
				closeDecls decls decl_names import_avails

		-- Found it
		other -> initRnMS emptyRnEnv mod_name InterfaceMode (
				     rnDecl new_decl
			 )				`thenRn` \ rn_decl ->
			 closeDecls (rn_decl : decls)
				    (addAvailToNameSet decl_names decl_avail)
				    (decl_avail : import_avails)
		     where
		         (mod_name,_) = modAndOcc name

getDeclErr name sty
  = ppSep [ppStr "Failed to find interface decl for", ppr sty name]

getDeclWarn name sty
  = ppSep [ppStr "Warning: failed to find (optional) interface decl for", ppr sty name]
\end{code}


