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
import RnIfaces		( getImportedInstDecls, importDecl, getImportVersions, getSpecialInstModules,
			  mkSearchPath
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
    closeDecls rn_local_decls				`thenRn` \ rn_all_decls ->


	-- GENERATE THE VERSION/USAGE INFO
    getImportVersions mod_name exports			`thenRn` \ import_versions ->
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
				  rn_all_decls
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
	   -> RnMG [RenamedHsDecl]		-- input + extra decls slurped
	-- The monad includes a list of possibly-unresolved Names
	-- This list is empty when closeDecls returns

closeDecls decls 
  = popOccurrenceName		`thenRn` \ maybe_unresolved ->
    case maybe_unresolved of

	-- No more unresolved names
	Nothing ->	-- Slurp instance declarations
		   getImportedInstDecls			`thenRn` \ inst_decls ->
		   traceRn (ppSep [ppPStr SLIT("Slurped"), ppInt (length inst_decls), ppPStr SLIT("instance decls")])
							`thenRn_`

			-- None?  then at last we are done
		   if null inst_decls then
			returnRn decls
		   else	
		   mapRn rn_inst_decl inst_decls	`thenRn` \ new_inst_decls ->

			-- We *must* loop again here.  Why?  Two reasons:
			-- (a) an instance decl will give rise to an unresolved dfun, whose
			--	decl we must slurp to get its version number; that's the version
			-- 	number for the whole instance decl.
			-- (b) an instance decl might give rise to a new unresolved class,
			-- 	whose decl we must slurp, which might let in some new instance decls,
			--	and so on.  Example:  instance Foo a => Baz [a] where ...
	
		   closeDecls (new_inst_decls ++ decls)
			
	-- An unresolved name
	Just (name,necessity)
	  -> 	-- Slurp its declaration, if any
--	     traceRn (ppSep [ppPStr SLIT("Considering"), ppr PprDebug name])	`thenRn_`
	     importDecl name necessity		`thenRn` \ maybe_decl ->
	     case maybe_decl of

		-- No declaration... (wired in thing or optional)
		Nothing   -> closeDecls decls

		-- Found a declaration... rename it
		Just decl -> rn_iface_decl mod_name decl	`thenRn` \ new_decl ->
			     closeDecls (new_decl : decls)
		     where
		         (mod_name,_) = modAndOcc name
  where
					-- Notice that the rnEnv starts empty
    rn_iface_decl mod_name decl  = initRnMS emptyRnEnv mod_name InterfaceMode (rnDecl decl)
    rn_inst_decl (mod_name,decl) = rn_iface_decl mod_name (InstD decl)

\end{code}


