%
% (c) The GRASP/AQUA Project, Glasgow University, 1993-1998
%

\section[MkIface]{Print an interface for a module}

\begin{code}
module MkIface ( 
	showIface, mkIface, mkUsageInfo,
	pprIface, 
	ifaceTyThing,
  ) where

#include "HsVersions.h"

import HsSyn
import HsCore		( HsIdInfo(..), UfExpr(..), toUfExpr, toUfBndr )
import HsTypes		( toHsTyVars )
import TysPrim		( alphaTyVars )
import BasicTypes	( NewOrData(..), Activation(..), FixitySig(..),
			  Version, initialVersion, bumpVersion 
			)
import NewDemand	( isTopSig )
import TcRnMonad
import TcRnTypes	( ImportAvails(..) )
import RnHsSyn		( RenamedInstDecl, RenamedTyClDecl )
import HscTypes		( VersionInfo(..), ModIface(..), 
			  ModGuts(..), ModGuts, 
			  GhciMode(..), HscEnv(..), Dependencies(..),
			  FixityEnv, lookupFixity, collectFixities,
			  IfaceDecls, mkIfaceDecls, dcl_tycl, dcl_rules, dcl_insts,
			  TyThing(..), DFunId, 
			  Avails, AvailInfo, GenAvailInfo(..), availName, 
			  ExternalPackageState(..),
			  ParsedIface(..), Usage(..),
			  Deprecations(..), initialVersionInfo,
			  lookupVersion, lookupIfaceByModName
			)

import CmdLineOpts
import Id		( idType, idInfo, isImplicitId, idCgInfo )
import DataCon		( dataConName, dataConSig, dataConFieldLabels, dataConStrictMarks )
import IdInfo		-- Lots
import CoreSyn		( CoreRule(..), IdCoreRule )
import CoreFVs		( ruleLhsFreeNames )
import CoreUnfold	( neverUnfold, unfoldingTemplate )
import Name		( getName, nameModule, nameModule_maybe, nameOccName,
			  nameIsLocalOrFrom, Name, NamedThing(..) )
import NameEnv
import NameSet
import OccName		( OccName, pprOccName )
import TyCon		( DataConDetails(..), tyConTyVars, tyConDataCons, tyConTheta,
			  isFunTyCon, isPrimTyCon, isNewTyCon, isClassTyCon, 
			  isSynTyCon, isAlgTyCon, isForeignTyCon,
			  getSynTyConDefn, tyConGenInfo, tyConDataConDetails, tyConArity )
import Class		( classExtraBigSig, classTyCon, DefMeth(..) )
import FieldLabel	( fieldLabelType )
import TcType		( tcSplitForAllTys, tcFunResultTy, tidyTopType, deNoteType, tyClsNamesOfDFunHead )
import SrcLoc		( noSrcLoc )
import Module		( Module, ModuleName, moduleNameFS, moduleName, isHomeModule,
			  ModLocation(..), mkSysModuleNameFS, 
			  ModuleEnv, emptyModuleEnv, lookupModuleEnv,
			  extendModuleEnv_C, moduleEnvElts 
			)
import Outputable
import Util		( sortLt, dropList, seqList )
import Binary		( getBinFileWithDict )
import BinIface		( writeBinIface, v_IgnoreHiVersion )
import ErrUtils		( dumpIfSet_dyn )
import FiniteMap
import FastString

import DATA_IOREF	( writeIORef )
import Monad		( when )
import Maybe		( catMaybes, isJust, isNothing )
import Maybes		( orElse )
import IO		( putStrLn )
\end{code}


%************************************************************************
%*				 					*
\subsection{Print out the contents of a binary interface}
%*				 					*
%************************************************************************

\begin{code}
showIface :: FilePath -> IO ()
showIface filename = do
   -- skip the version check; we don't want to worry about profiled vs.
   -- non-profiled interfaces, for example.
   writeIORef v_IgnoreHiVersion True
   parsed_iface <- Binary.getBinFileWithDict filename
   let ParsedIface{
      pi_mod=pi_mod, pi_pkg=pi_pkg, pi_vers=pi_vers,
      pi_deps=pi_deps,
      pi_orphan=pi_orphan, pi_usages=pi_usages,
      pi_exports=pi_exports, pi_decls=pi_decls,
      pi_fixity=pi_fixity, pi_insts=pi_insts,
      pi_rules=pi_rules, pi_deprecs=pi_deprecs } = parsed_iface
   putStrLn (showSDoc (vcat [
	text "__interface" <+> doubleQuotes (ppr pi_pkg)
	   <+> ppr pi_mod <+> ppr pi_vers 
	   <+> (if pi_orphan then char '!' else empty)
	   <+> ptext SLIT("where"),
	-- no instance Outputable (WhatsImported):
	pprExports id (snd pi_exports),
	pprDeps pi_deps,
	pprUsages  id pi_usages,
	hsep (map ppr_fix pi_fixity) <> semi,
	vcat (map ppr_inst pi_insts),
	vcat (map ppr_decl pi_decls),
	ppr pi_rules
	-- no instance Outputable (Either):
	-- ppr pi_deprecs
	]))
   where
    ppr_fix (FixitySig n f _) = ppr f <+> ppr n
    ppr_inst i  = ppr i <+> semi
    ppr_decl (v,d)  = int v <+> ppr d <> semi
\end{code}

%************************************************************************
%*				 					*
\subsection{Completing an interface}
%*				 					*
%************************************************************************

\begin{code}
mkIface :: HscEnv
	-> ModLocation
	-> Maybe ModIface	-- The old interface, if we have it
	-> ModGuts		-- The compiled, tidied module
	-> IO ModIface		-- The new one, complete with decls and versions
-- mkFinalIface 
--	a) completes the interface
--	b) writes it out to a file if necessary

mkIface hsc_env location maybe_old_iface 
	impl@ModGuts{ mg_module = this_mod,
		      mg_usages = usages,
		      mg_deps   = deps,
		      mg_exports = exports,
		      mg_rdr_env = rdr_env,
		      mg_fix_env = fix_env,
		      mg_deprecs = deprecs,
		      mg_insts = insts, 
		      mg_rules = rules,
		      mg_types = types }
  = do	{ 	-- Sort the exports to make them easier to compare for versions
	  let { my_exports = groupAvails this_mod exports ;

	        iface_w_decls = ModIface { mi_module   = this_mod,
					   mi_package  = opt_InPackage,
					   mi_version  = initialVersionInfo,
					   mi_deps     = deps,
					   mi_usages   = usages,
					   mi_exports  = my_exports,
					   mi_decls    = new_decls,
					   mi_orphan   = orphan_mod,
					   mi_boot     = False,
					   mi_fixities = fix_env,
					   mi_globals  = Just rdr_env,
					   mi_deprecs  = deprecs } }

		-- Add version information
	; let (final_iface, maybe_diffs) = _scc_ "versioninfo" addVersionInfo maybe_old_iface iface_w_decls

		-- Write the interface file, if necessary
	; when (must_write_hi_file maybe_diffs)
		(writeBinIface hi_file_path final_iface)
--		(writeIface hi_file_path final_iface)

		-- Debug printing
	; write_diffs dflags final_iface maybe_diffs

	; orphan_mod `seq`
	  return final_iface }

  where
     dflags    = hsc_dflags hsc_env
     ghci_mode = hsc_mode hsc_env

     must_write_hi_file Nothing       = False
     must_write_hi_file (Just _diffs) = ghci_mode /= Interactive
		-- We must write a new .hi file if there are some changes
		-- and we're not in interactive mode
		-- maybe_diffs = 'Nothing' means that even the usages havn't changed, 
		--     so there's no need to write a new interface file.  But even if 
		--     the usages have changed, the module version may not have.

     hi_file_path = ml_hi_file location
     new_decls    = mkIfaceDecls ty_cls_dcls rule_dcls inst_dcls
     inst_dcls    = map ifaceInstance insts
     ty_cls_dcls  = foldNameEnv ifaceTyThing_acc [] types
     rule_dcls    = map ifaceRule rules
     orphan_mod   = isOrphanModule impl

write_diffs :: DynFlags -> ModIface -> Maybe SDoc -> IO ()
write_diffs dflags new_iface Nothing
  = do when (dopt Opt_D_dump_hi_diffs dflags) (printDump (text "INTERFACE UNCHANGED"))
       dumpIfSet_dyn dflags Opt_D_dump_hi "UNCHANGED FINAL INTERFACE" (pprIface new_iface)

write_diffs dflags new_iface (Just sdoc_diffs)
  = do dumpIfSet_dyn dflags Opt_D_dump_hi_diffs "INTERFACE HAS CHANGED" sdoc_diffs
       dumpIfSet_dyn dflags Opt_D_dump_hi "NEW FINAL INTERFACE" (pprIface new_iface)
\end{code}

\begin{code}
isOrphanModule :: ModGuts -> Bool
isOrphanModule (ModGuts {mg_module = this_mod, mg_insts = insts, mg_rules = rules})
  = any orphan_inst insts || any orphan_rule rules
  where
	-- A rule is an orphan if the LHS mentions nothing defined locally
    orphan_inst dfun_id = no_locals (tyClsNamesOfDFunHead (idType dfun_id))
	-- A instance is an orphan if its head mentions nothing defined locally
    orphan_rule rule    = no_locals (ruleLhsFreeNames rule)

    no_locals names     = isEmptyNameSet (filterNameSet (nameIsLocalOrFrom this_mod) names)
\end{code}

Implicit Ids and class tycons aren't included in interface files, so
we miss them out of the accumulating parameter here.

\begin{code}
ifaceTyThing_acc :: TyThing -> [RenamedTyClDecl] -> [RenamedTyClDecl]
ifaceTyThing_acc (ADataCon dc) so_far 		      = so_far
ifaceTyThing_acc (AnId   id) so_far | isImplicitId id = so_far
ifaceTyThing_acc (ATyCon id) so_far | isClassTyCon id = so_far
ifaceTyThing_acc other so_far = ifaceTyThing other : so_far
\end{code}

Convert *any* TyThing into a RenamedTyClDecl.  Used both for
generating interface files and for the ':info' command in GHCi.

\begin{code}
ifaceTyThing :: TyThing -> RenamedTyClDecl
ifaceTyThing (AClass clas) = cls_decl
  where
    cls_decl = ClassDecl { tcdCtxt	= toHsContext sc_theta,
			   tcdName	= getName clas,
			   tcdTyVars	= toHsTyVars clas_tyvars,
			   tcdFDs 	= toHsFDs clas_fds,
			   tcdSigs	= map toClassOpSig op_stuff,
			   tcdMeths	= Nothing, 
			   tcdLoc	= noSrcLoc }

    (clas_tyvars, clas_fds, sc_theta, sc_sels, op_stuff) = classExtraBigSig clas
    tycon     = classTyCon clas
    data_con  = head (tyConDataCons tycon)

    toClassOpSig (sel_id, def_meth)
	= ASSERT(sel_tyvars == clas_tyvars)
	  ClassOpSig (getName sel_id) def_meth (toHsType op_ty) noSrcLoc
	where
		-- Be careful when splitting the type, because of things
		-- like  	class Foo a where
		--		  op :: (?x :: String) => a -> a
		-- and  	class Baz a where
		--		  op :: (Ord a) => a -> a
	  (sel_tyvars, rho_ty) = tcSplitForAllTys (idType sel_id)
	  op_ty		       = tcFunResultTy rho_ty

ifaceTyThing (ATyCon tycon) = ty_decl
  where
    ty_decl | isSynTyCon tycon
	    = TySynonym { tcdName   = getName tycon,
		 	  tcdTyVars = toHsTyVars tyvars,
			  tcdSynRhs = toHsType syn_ty,
			  tcdLoc    = noSrcLoc }

	    | isAlgTyCon tycon
	    = TyData {	tcdND	   = new_or_data,
			tcdCtxt    = toHsContext (tyConTheta tycon),
			tcdName    = getName tycon,
		 	tcdTyVars  = toHsTyVars tyvars,
			tcdCons    = ifaceConDecls (tyConDataConDetails tycon),
			tcdDerivs  = Nothing,
		        tcdGeneric = Just (isJust (tyConGenInfo tycon)),
				-- Just True <=> has generic stuff
			tcdLoc	   = noSrcLoc }

	    | isForeignTyCon tycon
	    = ForeignType { tcdName    = getName tycon,
	    		    tcdExtName = Nothing,
			    tcdFoType  = DNType,	-- The only case at present
			    tcdLoc     = noSrcLoc }

	    | isPrimTyCon tycon || isFunTyCon tycon
		-- needed in GHCi for ':info Int#', for example
	    = TyData {  tcdND     = DataType,
			tcdCtxt   = [],
			tcdName   = getName tycon,
		 	tcdTyVars = toHsTyVars (take (tyConArity tycon) alphaTyVars),
			tcdCons   = Unknown,
			tcdDerivs = Nothing,
		        tcdGeneric  = Just False,
			tcdLoc	     = noSrcLoc }

	    | otherwise = pprPanic "ifaceTyThing" (ppr tycon)

    tyvars      = tyConTyVars tycon
    (_, syn_ty) = getSynTyConDefn tycon
    new_or_data | isNewTyCon tycon = NewType
	        | otherwise	   = DataType

    ifaceConDecls Unknown       = Unknown
    ifaceConDecls (HasCons n)   = HasCons n
    ifaceConDecls (DataCons cs) = DataCons (map ifaceConDecl cs)

    ifaceConDecl data_con 
	= ConDecl (dataConName data_con)
		  (toHsTyVars ex_tyvars)
		  (toHsContext ex_theta)
		  details noSrcLoc
	where
	  (tyvars1, _, ex_tyvars, ex_theta, arg_tys, tycon1) = dataConSig data_con
          field_labels   = dataConFieldLabels data_con
          strict_marks   = dropList ex_theta (dataConStrictMarks data_con)
				-- The 'drop' is because dataConStrictMarks
				-- includes the existential dictionaries
	  details | null field_labels
	    	  = ASSERT( tycon == tycon1 && tyvars == tyvars1 )
	    	    PrefixCon (zipWith BangType strict_marks (map toHsType arg_tys))

    	    	  | otherwise
	    	  = RecCon (zipWith mk_field strict_marks field_labels)

    mk_field strict_mark field_label
	= (getName field_label, BangType strict_mark (toHsType (fieldLabelType field_label)))

ifaceTyThing (AnId id) = iface_sig
  where
    iface_sig = IfaceSig { tcdName   = getName id, 
			   tcdType   = toHsType id_type,
			   tcdIdInfo = hs_idinfo,
			   tcdLoc    =  noSrcLoc }

    id_type = idType id
    id_info = idInfo id
    cg_info = idCgInfo id
    arity_info = arityInfo id_info
    caf_info   = cgCafInfo cg_info

    hs_idinfo | opt_OmitInterfacePragmas
	      = []
 	      | otherwise
  	      = catMaybes [arity_hsinfo,  caf_hsinfo,
			   strict_hsinfo, wrkr_hsinfo,
			   unfold_hsinfo] 

    ------------  Arity  --------------
    arity_hsinfo | arity_info == 0 = Nothing
		 | otherwise       = Just (HsArity arity_info)

    ------------ Caf Info --------------
    caf_hsinfo = case caf_info of
		   NoCafRefs -> Just HsNoCafRefs
		   _other    -> Nothing

    ------------  Strictness  --------------
	-- No point in explicitly exporting TopSig
    strict_hsinfo = case newStrictnessInfo id_info of
			Just sig | not (isTopSig sig) -> Just (HsStrictness sig)
			_other			      -> Nothing

    ------------  Worker  --------------
    work_info   = workerInfo id_info
    has_worker  = case work_info of { HasWorker _ _ -> True; other -> False }
    wrkr_hsinfo = case work_info of
		    HasWorker work_id wrap_arity -> 
			Just (HsWorker (getName work_id) wrap_arity)
		    NoWorker -> Nothing

    ------------  Unfolding  --------------
	-- The unfolding is redundant if there is a worker
    unfold_info = unfoldingInfo id_info
    inline_prag = inlinePragInfo id_info
    rhs		= unfoldingTemplate unfold_info
    unfold_hsinfo |  neverUnfold unfold_info 
		  || has_worker = Nothing
		  | otherwise	= Just (HsUnfold inline_prag (toUfExpr rhs))
\end{code}

\begin{code}
ifaceInstance :: DFunId -> RenamedInstDecl
ifaceInstance dfun_id
  = InstDecl (toHsType tidy_ty) EmptyMonoBinds [] (Just (getName dfun_id)) noSrcLoc			 
  where
    tidy_ty = tidyTopType (deNoteType (idType dfun_id))
		-- The deNoteType is very important.   It removes all type
		-- synonyms from the instance type in interface files.
		-- That in turn makes sure that when reading in instance decls
		-- from interface files that the 'gating' mechanism works properly.
		-- Otherwise you could have
		--	type Tibble = T Int
		--	instance Foo Tibble where ...
		-- and this instance decl wouldn't get imported into a module
		-- that mentioned T but not Tibble.

ifaceRule :: IdCoreRule -> RuleDecl Name
ifaceRule (id, BuiltinRule _ _)
  = pprTrace "toHsRule: builtin" (ppr id) (bogusIfaceRule id)

ifaceRule (id, Rule name act bndrs args rhs)
  = IfaceRule name act (map toUfBndr bndrs) (getName id)
	      (map toUfExpr args) (toUfExpr rhs) noSrcLoc

bogusIfaceRule :: (NamedThing a) => a -> RuleDecl Name
bogusIfaceRule id
  = IfaceRule FSLIT("bogus") NeverActive [] (getName id) [] (UfVar (getName id)) noSrcLoc
\end{code}


%*********************************************************
%*							*
\subsection{Keeping track of what we've slurped, and version numbers}
%*							*
%*********************************************************

mkUsageInfo figures out what the ``usage information'' for this
moudule is; that is, what it must record in its interface file as the
things it uses.  

We produce a line for every module B below the module, A, currently being
compiled:
	import B <n> ;
to record the fact that A does import B indirectly.  This is used to decide
to look to look for B.hi rather than B.hi-boot when compiling a module that
imports A.  This line says that A imports B, but uses nothing in it.
So we'll get an early bale-out when compiling A if B's version changes.

The usage information records:

\begin{itemize}
\item	(a) anything reachable from its body code
\item	(b) any module exported with a @module Foo@
\item   (c) anything reachable from an exported item
\end{itemize}

Why (b)?  Because if @Foo@ changes then this module's export list
will change, so we must recompile this module at least as far as
making a new interface file --- but in practice that means complete
recompilation.

Why (c)?  Consider this:
\begin{verbatim}
	module A( f, g ) where	|	module B( f ) where
	  import B( f )		|	  f = h 3
	  g = ...		|	  h = ...
\end{verbatim}

Here, @B.f@ isn't used in A.  Should we nevertheless record @B.f@ in
@A@'s usages?  Our idea is that we aren't going to touch A.hi if it is
*identical* to what it was before.  If anything about @B.f@ changes
than anyone who imports @A@ should be recompiled in case they use
@B.f@ (they'll get an early exit if they don't).  So, if anything
about @B.f@ changes we'd better make sure that something in A.hi
changes, and the convenient way to do that is to record the version
number @B.f@ in A.hi in the usage list.  If B.f changes that'll force a
complete recompiation of A, which is overkill but it's the only way to 
write a new, slightly different, A.hi.

But the example is tricker.  Even if @B.f@ doesn't change at all,
@B.h@ may do so, and this change may not be reflected in @f@'s version
number.  But with -O, a module that imports A must be recompiled if
@B.h@ changes!  So A must record a dependency on @B.h@.  So we treat
the occurrence of @B.f@ in the export list *just as if* it were in the
code of A, and thereby haul in all the stuff reachable from it.

	*** Conclusion: if A mentions B.f in its export list,
	    behave just as if A mentioned B.f in its source code,
	    and slurp in B.f and all its transitive closure ***

[NB: If B was compiled with -O, but A isn't, we should really *still*
haul in all the unfoldings for B, in case the module that imports A *is*
compiled with -O.  I think this is the case.]

\begin{code}
mkUsageInfo :: HscEnv -> ExternalPackageState
	    -> ImportAvails -> EntityUsage
	    -> [Usage Name]

mkUsageInfo hsc_env eps
	    (ImportAvails { imp_mods = dir_imp_mods,
			    imp_dep_mods = dep_mods })
	    used_names
  = -- seq the list of Usages returned: occasionally these
    -- don't get evaluated for a while and we can end up hanging on to
    -- the entire collection of Ifaces.
    usages `seqList` usages
  where
    usages = catMaybes [ mkUsage mod_name 
		       | (mod_name,_) <- moduleEnvElts dep_mods]

    hpt = hsc_HPT hsc_env
    pit = eps_PIT eps
    
    import_all mod = case lookupModuleEnv dir_imp_mods mod of
    			Just (_,imp_all) -> imp_all
    			Nothing		 -> False
    
    -- ent_map groups together all the things imported and used
    -- from a particular module in this package
    ent_map :: ModuleEnv [Name]
    ent_map  = foldNameSet add_mv emptyModuleEnv used_names
    add_mv name mv_map = extendModuleEnv_C add_item mv_map mod [name]
    		   where
    		     mod = nameModule name
    		     add_item names _ = name:names
    
    -- We want to create a Usage for a home module if 
    --	a) we used something from; has something in used_names
    --	b) we imported all of it, even if we used nothing from it
    --		(need to recompile if its export list changes: export_vers)
    --	c) is a home-package orphan module (need to recompile if its
    --	 	instance decls change: rules_vers)
    mkUsage :: ModuleName -> Maybe (Usage Name)
    mkUsage mod_name
      |  isNothing maybe_iface	-- We can't depend on it if we didn't
      || not (isHomeModule mod)	-- even open the interface!
      || (null used_names
	  && not all_imported
	  && not orphan_mod)
      = Nothing			-- Record no usage info
    
      | otherwise	
      = Just (Usage { usg_name     = moduleName mod,
    	  	      usg_mod      = mod_vers,
    		      usg_exports  = export_vers,
    		      usg_entities = ent_vers,
    		      usg_rules    = rules_vers })
      where
	maybe_iface  = lookupIfaceByModName hpt pit mod_name
		-- In one-shot mode, the interfaces for home-package 
		-- modules accumulate in the PIT not HPT.  Sigh.

        Just iface   = maybe_iface
        mod   	     = mi_module iface
        version_info = mi_version iface
	orphan_mod   = mi_orphan iface
        version_env  = vers_decls   version_info
        mod_vers     = vers_module  version_info
        rules_vers   = vers_rules   version_info
        all_imported = import_all mod 
        export_vers | all_imported = Just (vers_exports version_info)
    		    | otherwise    = Nothing
    
    	-- The sort is to put them into canonical order
        used_names = lookupModuleEnv ent_map mod `orElse` []
        ent_vers = [(n, lookupVersion version_env n) 
    	           | n <- sortLt lt_occ used_names ]
        lt_occ n1 n2 = nameOccName n1 < nameOccName n2
\end{code}

\begin{code}
groupAvails :: Module -> Avails -> [(ModuleName, Avails)]
  -- Group by module and sort by occurrence
  -- This keeps the list in canonical order
groupAvails this_mod avails 
  = [ (mkSysModuleNameFS fs, sortLt lt avails)
    | (fs,avails) <- fmToList groupFM
    ]
  where
    groupFM :: FiniteMap FastString Avails
	-- Deliberately use the FastString so we
	-- get a canonical ordering
    groupFM = foldl add emptyFM avails

    add env avail = addToFM_C combine env mod_fs [avail']
		  where
		    mod_fs = moduleNameFS (moduleName avail_mod)
		    avail_mod = case nameModule_maybe (availName avail) of
					  Just m  -> m
					  Nothing -> this_mod
		    combine old _ = avail':old
		    avail'	  = sortAvail avail

    a1 `lt` a2 = occ1 < occ2
	       where
		 occ1  = nameOccName (availName a1)
		 occ2  = nameOccName (availName a2)

sortAvail :: AvailInfo -> AvailInfo
-- Sort the sub-names into canonical order.
-- The canonical order has the "main name" at the beginning 
-- (if it's there at all)
sortAvail (Avail n) = Avail n
sortAvail (AvailTC n ns) | n `elem` ns = AvailTC n (n : sortLt lt (filter (/= n) ns))
			 | otherwise   = AvailTC n (    sortLt lt ns)
			 where
			   n1 `lt` n2 = nameOccName n1 < nameOccName n2
\end{code}

%************************************************************************
%*				 					*
\subsection{Checking if the new interface is up to date
%*				 					*
%************************************************************************

\begin{code}
addVersionInfo :: Maybe ModIface		-- The old interface, read from M.hi
	       -> ModIface			-- The new interface decls
	       -> (ModIface, Maybe SDoc)	-- Nothing => no change; no need to write new Iface
						-- Just mi => Here is the new interface to write
						-- 	      with correct version numbers

-- NB: the fixities, declarations, rules are all assumed
-- to be sorted by increasing order of hsDeclName, so that 
-- we can compare for equality

addVersionInfo Nothing new_iface
-- No old interface, so definitely write a new one!
  = (new_iface, Just (text "No old interface available"))

addVersionInfo (Just old_iface@(ModIface { mi_version  = old_version, 
				       	   mi_decls    = old_decls,
				       	   mi_fixities = old_fixities,
					   mi_deprecs  = old_deprecs }))
	       new_iface@(ModIface { mi_decls    = new_decls,
				     mi_fixities = new_fixities,
				     mi_deprecs  = new_deprecs })

  | no_output_change && no_usage_change
  = (new_iface, Nothing)
	-- don't return the old iface because it may not have an
	-- mi_globals field set to anything reasonable.

  | otherwise		-- Add updated version numbers
  = --pprTrace "completeIface" (ppr (dcl_tycl old_decls))
    (final_iface, Just pp_diffs)
	
  where
    final_iface = new_iface { mi_version = new_version }
    old_mod_vers = vers_module  old_version
    new_version = VersionInfo { vers_module  = bumpVersion no_output_change old_mod_vers,
				vers_exports = bumpVersion no_export_change (vers_exports old_version),
				vers_rules   = bumpVersion no_rule_change   (vers_rules   old_version),
				vers_decls   = tc_vers }

    no_output_change = no_tc_change && no_rule_change && no_export_change && no_deprec_change
    no_usage_change  = mi_usages old_iface == mi_usages new_iface

    no_export_change = mi_exports old_iface == mi_exports new_iface		-- Kept sorted
    no_rule_change   = dcl_rules old_decls  == dcl_rules  new_decls		-- Ditto
		     && dcl_insts old_decls == dcl_insts  new_decls
    no_deprec_change = old_deprecs	    == new_deprecs

	-- Fill in the version number on the new declarations by looking at the old declarations.
	-- Set the flag if anything changes. 
	-- Assumes that the decls are sorted by hsDeclName.
    (no_tc_change,  pp_tc_diffs,  tc_vers) = diffDecls old_version old_fixities new_fixities
						       (dcl_tycl old_decls) (dcl_tycl new_decls)
    pp_diffs = vcat [pp_tc_diffs,
		     pp_change no_export_change "Export list",
		     pp_change no_rule_change   "Rules",
		     pp_change no_deprec_change "Deprecations",
		     pp_change no_usage_change  "Usages"]
    pp_change True  what = empty
    pp_change False what = text what <+> ptext SLIT("changed")

diffDecls :: VersionInfo				-- Old version
	  -> FixityEnv -> FixityEnv			-- Old and new fixities
	  -> [RenamedTyClDecl] -> [RenamedTyClDecl]	-- Old and new decls
	  -> (Bool,		-- True <=> no change
	      SDoc,		-- Record of differences
	      NameEnv Version)	-- New version map

diffDecls (VersionInfo { vers_module = old_mod_vers, vers_decls = old_decls_vers })
	  old_fixities new_fixities old new
  = diff True empty emptyNameEnv old new
  where
	-- When seeing if two decls are the same, 
	-- remember to check whether any relevant fixity has changed
    eq_tc  d1 d2 = d1 == d2 && all (same_fixity . fst) (tyClDeclNames d1)
    same_fixity n = lookupFixity old_fixities n == lookupFixity new_fixities n

    diff ok_so_far pp new_vers []  []      = (ok_so_far, pp, new_vers)
    diff ok_so_far pp new_vers (od:ods) [] = diff False (pp $$ only_old od) new_vers	      ods []
    diff ok_so_far pp new_vers [] (nd:nds) = diff False (pp $$ only_new nd) new_vers_with_new []  nds
	where
	  new_vers_with_new = extendNameEnv new_vers (tyClDeclName nd) (bumpVersion False old_mod_vers)
		-- When adding a new item, start from the old module version
		-- This way, if you have version 4 of f, then delete f, then add f again,
		-- you'll get version 6 of f, which will (correctly) force recompilation of
		-- clients

    diff ok_so_far pp new_vers (od:ods) (nd:nds)
	= case od_name `compare` nd_name of
		LT -> diff False (pp $$ only_old od) new_vers ods      (nd:nds)
		GT -> diff False (pp $$ only_new nd) new_vers (od:ods) nds
		EQ | od `eq_tc` nd -> diff ok_so_far pp 		   new_vers	      ods nds
		   | otherwise     -> diff False     (pp $$ changed od nd) new_vers_with_diff ods nds
	where
 	  od_name = tyClDeclName od
 	  nd_name = tyClDeclName nd
	  new_vers_with_diff = extendNameEnv new_vers nd_name (bumpVersion False old_version)
	  old_version = lookupVersion old_decls_vers od_name

    only_old d    = ptext SLIT("Only in old iface:") <+> ppr d
    only_new d    = ptext SLIT("Only in new iface:") <+> ppr d
    changed od nd = ptext SLIT("Changed in iface: ") <+> ((ptext SLIT("Old:") <+> ppr od) $$ 
							 (ptext SLIT("New:")  <+> ppr nd))
\end{code}


b%************************************************************************
%*				 					*
\subsection{Writing an interface file}
%*				 					*
%************************************************************************

\begin{code}
pprIface :: ModIface -> SDoc
pprIface iface
 = vcat [ ptext SLIT("__interface")
		<+> doubleQuotes (ftext (mi_package iface))
		<+> ppr (mi_module iface) <+> ppr (vers_module version_info)
		<+> pp_sub_vers
		<+> (if mi_orphan iface then char '!' else empty)
		<+> int opt_HiVersion
		<+> ptext SLIT("where")

	, pprExports nameOccName (mi_exports iface)
	, pprDeps    (mi_deps iface)
	, pprUsages  nameOccName (mi_usages iface)

	, pprFixities (mi_fixities iface) (dcl_tycl decls)
	, pprIfaceDecls (vers_decls version_info) decls
	, pprRulesAndDeprecs (dcl_rules decls) (mi_deprecs iface)
	]
  where
    version_info = mi_version iface
    decls	 = mi_decls iface
    exp_vers     = vers_exports version_info

    rule_vers	 = vers_rules version_info

    pp_sub_vers | exp_vers == initialVersion && rule_vers == initialVersion = empty
		| otherwise = brackets (ppr exp_vers <+> ppr rule_vers)
\end{code}

When printing export lists, we print like this:
	Avail   f		f
	AvailTC C [C, x, y]	C(x,y)
	AvailTC C [x, y]	C!(x,y)		-- Exporting x, y but not C

\begin{code}
pprExports :: Eq a => (a -> OccName) -> [(ModuleName, [GenAvailInfo a])] -> SDoc
pprExports getOcc exports = vcat (map (pprExport getOcc) exports)

pprExport :: Eq a => (a -> OccName) -> (ModuleName, [GenAvailInfo a]) -> SDoc
pprExport getOcc (mod, items)
 = hsep [ ptext SLIT("__export "), ppr mod, hsep (map pp_avail items) ] <> semi
  where
    --pp_avail :: GenAvailInfo a -> SDoc
    pp_avail (Avail name)    		     = ppr (getOcc name)
    pp_avail (AvailTC _ [])		     = empty
    pp_avail (AvailTC n (n':ns)) 
	| n==n'     = ppr (getOcc n) <> pp_export ns
 	| otherwise = ppr (getOcc n) <> char '|' <> pp_export (n':ns)
    
    pp_export []    = empty
    pp_export names = braces (hsep (map (ppr.getOcc) names))

pprOcc :: Name -> SDoc	-- Print the occurrence name only
pprOcc n = pprOccName (nameOccName n)
\end{code}


\begin{code}
pprUsages :: (a -> OccName) -> [Usage a] -> SDoc
pprUsages getOcc usages = vcat (map (pprUsage getOcc) usages)

pprUsage :: (a -> OccName) -> Usage a -> SDoc
pprUsage getOcc usage
  = hsep [ptext SLIT("import"), ppr (usg_name usage), 
	  int (usg_mod usage), 
	  pp_export_version (usg_exports usage),
	  int (usg_rules usage),
	  pp_versions (usg_entities usage)
    ] <> semi
  where
    pp_versions nvs = hsep [ ppr (getOcc n) <+> int v | (n,v) <- nvs ]

    pp_export_version Nothing  = empty
    pp_export_version (Just v) = int v


pprDeps :: Dependencies -> SDoc
pprDeps (Deps { dep_mods = mods, dep_pkgs = pkgs, dep_orphs = orphs})
  = vcat [ptext SLIT("module dependencies:") <+> fsep (map ppr_mod mods),
	  ptext SLIT("package dependencies:") <+> fsep (map ppr pkgs), 
	  ptext SLIT("orphans:") <+> fsep (map ppr orphs)
	]
  where
    ppr_mod (mod_name, boot) = ppr mod_name <+> ppr_boot boot
   
    ppr_boot   True  = text "[boot]"
    ppr_boot   False = empty
\end{code}

\begin{code}
pprIfaceDecls :: NameEnv Int -> IfaceDecls -> SDoc
pprIfaceDecls version_map decls
  = vcat [ vcat [ppr i <+> semi | i <- dcl_insts decls]
	 , vcat (map ppr_decl (dcl_tycl decls))
	 ]
  where
    ppr_decl d  = ppr_vers d <+> ppr d <> semi

	-- Print the version for the decl
    ppr_vers d = case lookupNameEnv version_map (tyClDeclName d) of
		   Nothing -> empty
		   Just v  -> int v
\end{code}

\begin{code}
pprFixities :: FixityEnv
	    -> [TyClDecl Name]
	    -> SDoc
pprFixities fixity_map decls
  = hsep [ ppr fix <+> ppr n 
	 | FixitySig n fix _ <- collectFixities fixity_map decls ] <> semi

-- Disgusting to print these two together, but that's 
-- the way the interface parser currently expects them.
pprRulesAndDeprecs :: (Outputable a) => [a] -> Deprecations -> SDoc
pprRulesAndDeprecs [] NoDeprecs = empty
pprRulesAndDeprecs rules deprecs
  = ptext SLIT("{-##") <+> (pp_rules rules $$ pp_deprecs deprecs) <+> ptext SLIT("##-}")
  where
    pp_rules []    = empty
    pp_rules rules = ptext SLIT("__R") <+> vcat (map ppr rules)

    pp_deprecs NoDeprecs = empty
    pp_deprecs deprecs   = ptext SLIT("__D") <+> guts
			  where
			    guts = case deprecs of
					DeprecAll txt  -> doubleQuotes (ftext txt)
					DeprecSome env -> ppr_deprec_env env

ppr_deprec_env :: NameEnv (Name, FastString) -> SDoc
ppr_deprec_env env = vcat (punctuate semi (map pp_deprec (nameEnvElts env)))
	           where
   	 	     pp_deprec (name, txt) = pprOcc name <+> doubleQuotes (ftext txt)
\end{code}
