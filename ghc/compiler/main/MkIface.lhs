%
% (c) The GRASP/AQUA Project, Glasgow University, 1993-1998
%
\section[MkIface]{Print an interface for a module}

\begin{code}
module MkIface ( 
	mkModDetails, mkModDetailsFromIface, completeIface, 
	writeIface, pprIface
  ) where

#include "HsVersions.h"

import HsSyn
import HsCore		( HsIdInfo(..), UfExpr(..), toUfExpr, toUfBndr )
import HsTypes		( toHsTyVars )
import BasicTypes	( Fixity(..), NewOrData(..),
			  Version, initialVersion, bumpVersion, isLoopBreaker
			)
import RnMonad
import RnHsSyn		( RenamedInstDecl, RenamedTyClDecl )
import TcHsSyn		( TypecheckedRuleDecl )
import HscTypes		( VersionInfo(..), ModIface(..), ModDetails(..),
			  IfaceDecls, mkIfaceDecls, dcl_tycl, dcl_rules, dcl_insts,
			  TyThing(..), DFunId, TypeEnv, Avails,
			  WhatsImported(..), GenAvailInfo(..), 
			  ImportVersion, AvailInfo, Deprecations(..),
			  extendTypeEnvList
			)

import CmdLineOpts
import Id		( Id, idType, idInfo, omitIfaceSigForId, isDictFunId,
			  idSpecialisation, setIdInfo, isLocalId, idName, hasNoBinding
			)
import Var		( isId )
import VarSet
import DataCon		( StrictnessMark(..), dataConId, dataConSig, dataConFieldLabels, dataConStrictMarks )
import IdInfo		-- Lots
import CoreSyn		( CoreBind, CoreRule(..), IdCoreRule, 
			  isBuiltinRule, rulesRules, 
			  bindersOf, bindersOfBinds
			)
import CoreFVs		( ruleSomeLhsFreeVars )
import CoreUnfold	( neverUnfold, unfoldingTemplate )
import Name		( getName, nameModule, Name, NamedThing(..) )
import Name 	-- Env
import OccName		( pprOccName )
import TyCon		( TyCon, getSynTyConDefn, isSynTyCon, isNewTyCon, isAlgTyCon, tyConGenIds,
			  tyConTheta, tyConTyVars, tyConDataCons, tyConFamilySize, isClassTyCon
			)
import Class		( classExtraBigSig, classTyCon, DefMeth(..) )
import FieldLabel	( fieldLabelType )
import Type		( splitSigmaTy, tidyTopType, deNoteType )
import SrcLoc		( noSrcLoc )
import Outputable
import Module		( ModuleName )

import IO		( IOMode(..), openFile, hClose )
\end{code}


%************************************************************************
%*				 					*
\subsection{Write a new interface file}
%*				 					*
%************************************************************************

\begin{code}
mkModDetails :: TypeEnv		-- From typechecker
	     -> [CoreBind]	-- Final bindings
	     -> [Id]		-- Top-level Ids from the code generator; 
				-- they have authoritative arity info
	     -> [IdCoreRule]	-- Tidy orphan rules
	     -> ModDetails
mkModDetails type_env tidy_binds stg_ids orphan_rules
  = ModDetails { md_types = new_type_env,
		 md_rules = rule_dcls,
		 md_insts = filter isDictFunId final_ids }
  where
	-- The competed type environment is gotten from
	-- 	a) keeping the types and classes
	--	b) removing all Ids, 
	--	c) adding Ids with correct IdInfo, including unfoldings,
	--		gotten from the bindings
	-- From (c) we keep only those Ids with Global names;
	--	    the CoreTidy pass makes sure these are all and only
	--	    the externally-accessible ones
	-- This truncates the type environment to include only the 
	-- exported Ids and things needed from them, which saves space
	--
	-- However, we do keep things like constructors, which should not appear 
	-- in interface files, because they are needed by importing modules when
	-- using the compilation manager
    new_type_env = extendTypeEnvList (filterNameEnv keep_it type_env)
				     (map AnId final_ids)

	-- We keep constructor workers, because they won't appear
	-- in the bindings from which final_ids are derived!
    keep_it (AnId id) = hasNoBinding id
    keep_it other     = True

    stg_id_set = mkVarSet stg_ids
    final_ids  = [addStgInfo stg_id_set id | bind <- tidy_binds
					   , id <- bindersOf bind
					   , isGlobalName (idName id)]

	-- The complete rules are gotten by combining
	--	a) the orphan rules
	--	b) rules embedded in the top-level Ids
    rule_dcls | opt_OmitInterfacePragmas = []
	      | otherwise		 = getRules orphan_rules tidy_binds (mkVarSet final_ids)

-- This version is used when we are re-linking a module
-- so we've only run the type checker on its previous interface 
mkModDetailsFromIface :: TypeEnv 
		      -> [TypecheckedRuleDecl]
		      -> ModDetails
mkModDetailsFromIface type_env rules
  = ModDetails { md_types = type_env,
		 md_rules = rule_dcls,
		 md_insts = dfun_ids }
  where
    dfun_ids  = [dfun_id | AnId dfun_id <- nameEnvElts type_env, isDictFunId dfun_id]
    rule_dcls = [(id,rule) | IfaceRuleOut id rule <- rules]
	-- All the rules from an interface are of the IfaceRuleOut form
\end{code}


We have to add on the arity and CAF info computed by the code generator
This is also the moment at which we may forget that this function has
a worker: see the comments below

\begin{code}
addStgInfo :: IdSet 	-- Ids used at code-gen time; they have better pragma info!
	   -> Id -> Id
addStgInfo stg_ids id
  = id `setIdInfo` final_idinfo
  where
    idinfo  = idInfo id
    idinfo' = idinfo `setArityInfo` stg_arity
		     `setCafInfo`   cafInfo stg_idinfo
    final_idinfo | worker_ok = idinfo'
		 | otherwise = idinfo' `setWorkerInfo` NoWorker
		
    stg_idinfo = case lookupVarSet stg_ids id of
			Just id' -> idInfo id'
			Nothing  -> pprTrace "ifaceBinds not found:" (ppr id) $
				    idInfo id

    stg_arity = arityInfo stg_idinfo

    ------------  Worker  --------------
	-- We only treat a function as having a worker if
	-- the exported arity (which is now the number of visible lambdas)
	-- is the same as the arity at the moment of the w/w split
	-- If so, we can safely omit the unfolding inside the wrapper, and
	-- instead re-generate it from the type/arity/strictness info
	-- But if the arity has changed, we just take the simple path and
	-- put the unfolding into the interface file, forgetting the fact
	-- that it's a wrapper.  
	--
	-- How can this happen?  Sometimes we get
	--	f = coerce t (\x y -> $wf x y)
	-- at the moment of w/w split; but the eta reducer turns it into
	--	f = coerce t $wf
	-- which is perfectly fine except that the exposed arity so far as
	-- the code generator is concerned (zero) differs from the arity
	-- when we did the split (2).  
	--
	-- All this arises because we use 'arity' to mean "exactly how many
	-- top level lambdas are there" in interface files; but during the
	-- compilation of this module it means "how many things can I apply
	-- this to".
    worker_ok = case workerInfo idinfo of
		     NoWorker		          -> True
		     HasWorker work_id wrap_arity -> wrap_arity == arityLowerBound stg_arity
\end{code}


\begin{code}
getRules :: [IdCoreRule] 	-- Orphan rules
	 -> [CoreBind]		-- Bindings, with rules in the top-level Ids
	 -> IdSet		-- Ids that are exported, so we need their rules
	 -> [IdCoreRule]
getRules orphan_rules binds emitted
  = orphan_rules ++ local_rules
  where
    local_rules  = [ (fn, rule)
 		   | fn <- bindersOfBinds binds,
		     fn `elemVarSet` emitted,
		     rule <- rulesRules (idSpecialisation fn),
		     not (isBuiltinRule rule),
				-- We can't print builtin rules in interface files
				-- Since they are built in, an importing module
				-- will have access to them anyway

			-- Sept 00: I've disabled this test.  It doesn't stop many, if any, rules
			-- from coming out, and to make it work properly we need to add ????
			--	(put it back in for now)
		     all (`elemVarSet` emitted) (varSetElems (ruleSomeLhsFreeVars interestingId rule))
				-- Spit out a rule only if all its lhs free vars are emitted
				-- This is a good reason not to do it when we emit the Id itself
		   ]

interestingId id = isId id && isLocalId id
\end{code}


%************************************************************************
%*				 					*
\subsection{Completing an interface}
%*				 					*
%************************************************************************

\begin{code}
completeIface :: Maybe ModIface		-- The old interface, if we have it
	      -> ModIface		-- The new one, minus the decls and versions
	      -> ModDetails		-- The ModDetails for this module
	      -> (ModIface, Maybe SDoc)	-- The new one, complete with decls and versions
					-- The SDoc is a debug document giving differences
					-- Nothing => no change

	-- NB: 'Nothing' means that even the usages havn't changed, so there's no
	--     need to write a new interface file.  But even if the usages have
	--     changed, the module version may not have.
completeIface maybe_old_iface new_iface mod_details 
  = addVersionInfo maybe_old_iface (new_iface { mi_decls = new_decls })
  where
     new_decls   = mkIfaceDecls ty_cls_dcls rule_dcls inst_dcls
     inst_dcls   = map ifaceInstance (md_insts mod_details)
     ty_cls_dcls = foldNameEnv ifaceTyCls [] (md_types mod_details)
     rule_dcls   = map ifaceRule (md_rules mod_details)
\end{code}


\begin{code}
ifaceTyCls :: TyThing -> [RenamedTyClDecl] -> [RenamedTyClDecl]
ifaceTyCls (AClass clas) so_far
  = cls_decl : so_far
  where
    cls_decl = ClassDecl { tcdCtxt	= toHsContext sc_theta,
			   tcdName	= getName clas,
			   tcdTyVars	= toHsTyVars clas_tyvars,
			   tcdFDs 	= toHsFDs clas_fds,
			   tcdSigs	= map toClassOpSig op_stuff,
			   tcdMeths	= Nothing, 
			   tcdSysNames  = sys_names,
			   tcdLoc	= noSrcLoc }

    (clas_tyvars, clas_fds, sc_theta, sc_sels, op_stuff) = classExtraBigSig clas
    tycon     = classTyCon clas
    data_con  = head (tyConDataCons tycon)
    sys_names = mkClassDeclSysNames (getName tycon, getName data_con, 
				     getName (dataConId data_con), map getName sc_sels)

    toClassOpSig (sel_id, def_meth)
	= ASSERT(sel_tyvars == clas_tyvars)
	  ClassOpSig (getName sel_id) def_meth' (toHsType op_ty) noSrcLoc
	where
	  (sel_tyvars, _, op_ty) = splitSigmaTy (idType sel_id)
	  def_meth' = case def_meth of
			 NoDefMeth  -> NoDefMeth
			 GenDefMeth -> GenDefMeth
			 DefMeth id -> DefMeth (getName id)

ifaceTyCls (ATyCon tycon) so_far
  | isClassTyCon tycon = so_far
  | otherwise	       = ty_decl : so_far
  where
    ty_decl | isSynTyCon tycon
	    = TySynonym { tcdName   = getName tycon,
		 	  tcdTyVars = toHsTyVars tyvars,
			  tcdSynRhs = toHsType syn_ty,
			  tcdLoc    = noSrcLoc }

	    | isAlgTyCon tycon
	    = TyData {	tcdND	  = new_or_data,
			tcdCtxt   = toHsContext (tyConTheta tycon),
			tcdName   = getName tycon,
		 	tcdTyVars = toHsTyVars tyvars,
			tcdCons   = map ifaceConDecl (tyConDataCons tycon),
			tcdNCons  = tyConFamilySize tycon,
			tcdDerivs = Nothing,
		        tcdSysNames  = map getName (tyConGenIds tycon),
			tcdLoc	     = noSrcLoc }

	    | otherwise = pprPanic "ifaceTyCls" (ppr tycon)

    tyvars      = tyConTyVars tycon
    (_, syn_ty) = getSynTyConDefn tycon
    new_or_data | isNewTyCon tycon = NewType
	        | otherwise	   = DataType

    ifaceConDecl data_con 
	= ConDecl (getName data_con) (getName (dataConId data_con))
		  (toHsTyVars ex_tyvars)
		  (toHsContext ex_theta)
		  details noSrcLoc
	where
	  (tyvars1, _, ex_tyvars, ex_theta, arg_tys, tycon1) = dataConSig data_con
          field_labels   = dataConFieldLabels data_con
          strict_marks   = dataConStrictMarks data_con
	  details | null field_labels
	    	  = ASSERT( tycon == tycon1 && tyvars == tyvars1 )
	    	    VanillaCon (zipWith mk_bang_ty strict_marks arg_tys)

    	    	  | otherwise
	    	  = RecCon (zipWith mk_field strict_marks field_labels)

    mk_bang_ty NotMarkedStrict     ty = Unbanged (toHsType ty)
    mk_bang_ty (MarkedUnboxed _ _) ty = Unpacked (toHsType ty)
    mk_bang_ty MarkedStrict        ty = Banged   (toHsType ty)

    mk_field strict_mark field_label
	= ([getName field_label], mk_bang_ty strict_mark (fieldLabelType field_label))

ifaceTyCls (AnId id) so_far
  | omitIfaceSigForId id = so_far
  | otherwise 		 = iface_sig : so_far
  where
    iface_sig = IfaceSig { tcdName   = getName id, 
			   tcdType   = toHsType id_type,
			   tcdIdInfo = hs_idinfo,
			   tcdLoc    =  noSrcLoc }

    id_type = idType id
    id_info = idInfo id

    hs_idinfo | opt_OmitInterfacePragmas = []
 	      | otherwise		 = arity_hsinfo  ++ caf_hsinfo  ++ cpr_hsinfo ++ 
					   strict_hsinfo ++ wrkr_hsinfo ++ unfold_hsinfo

    ------------  Arity  --------------
    arity_hsinfo = case arityInfo id_info of
			a@(ArityExactly n) -> [HsArity a]
			other		   -> []

    ------------ Caf Info --------------
    caf_hsinfo = case cafInfo id_info of
		   NoCafRefs -> [HsNoCafRefs]
		   otherwise -> []

    ------------ CPR Info --------------
    cpr_hsinfo = case cprInfo id_info of
		   ReturnsCPR -> [HsCprInfo]
		   NoCPRInfo  -> []

    ------------  Strictness  --------------
    strict_hsinfo = case strictnessInfo id_info of
			NoStrictnessInfo -> []
			info		 -> [HsStrictness info]


    ------------  Worker  --------------
    work_info   = workerInfo id_info
    has_worker  = case work_info of { HasWorker _ _ -> True; other -> False }
    wrkr_hsinfo = case work_info of
		    HasWorker work_id wrap_arity -> [HsWorker (getName work_id)]
		    NoWorker			 -> []

    ------------  Unfolding  --------------
	-- The unfolding is redundant if there is a worker
    unfold_info = unfoldingInfo id_info
    inline_prag = inlinePragInfo id_info
    rhs		= unfoldingTemplate unfold_info
    unfold_hsinfo |  neverUnfold unfold_info 
		  || has_worker = []
		  | otherwise	= [HsUnfold inline_prag (toUfExpr rhs)]
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

ifaceRule (id, BuiltinRule _)
  = pprTrace "toHsRule: builtin" (ppr id) (bogusIfaceRule id)

ifaceRule (id, Rule name bndrs args rhs)
  = IfaceRule name (map toUfBndr bndrs) (getName id)
	      (map toUfExpr args) (toUfExpr rhs) noSrcLoc

bogusIfaceRule id
  = IfaceRule SLIT("bogus") [] (getName id) [] (UfVar (getName id)) noSrcLoc
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

addVersionInfo (Just old_iface@(ModIface { mi_version = old_version, 
				       	   mi_decls   = old_decls,
				       	   mi_fixities = old_fixities }))
	       new_iface@(ModIface { mi_decls = new_decls,
				     mi_fixities = new_fixities })

  | no_output_change && no_usage_change
  = (new_iface, Nothing)
	-- don't return the old iface because it may not have an
	-- mi_globals field set to anything reasonable.

  | otherwise		-- Add updated version numbers
  = pprTrace "completeIface" (ppr (dcl_tycl old_decls))
    (final_iface, Just pp_tc_diffs)
	
  where
    final_iface = new_iface { mi_version = new_version }
    new_version = VersionInfo { vers_module  = bumpVersion no_output_change (vers_module  old_version),
				vers_exports = bumpVersion no_export_change (vers_exports old_version),
				vers_rules   = bumpVersion no_rule_change   (vers_rules   old_version),
				vers_decls   = tc_vers }

    no_output_change = no_tc_change && no_rule_change && no_export_change
    no_usage_change  = mi_usages old_iface == mi_usages new_iface

    no_export_change = mi_exports old_iface == mi_exports new_iface		-- Kept sorted
    no_rule_change   = dcl_rules old_decls  == dcl_rules  new_decls		-- Ditto

	-- Fill in the version number on the new declarations by looking at the old declarations.
	-- Set the flag if anything changes. 
	-- Assumes that the decls are sorted by hsDeclName.
    old_vers_decls = vers_decls old_version
    (no_tc_change,  pp_tc_diffs,  tc_vers) = diffDecls old_vers_decls old_fixities new_fixities
						       (dcl_tycl old_decls) (dcl_tycl new_decls)



diffDecls :: NameEnv Version				-- Old version map
	  -> NameEnv Fixity -> NameEnv Fixity		-- Old and new fixities
	  -> [RenamedTyClDecl] -> [RenamedTyClDecl]	-- Old and new decls
	  -> (Bool,		-- True <=> no change
	      SDoc,		-- Record of differences
	      NameEnv Version)	-- New version

diffDecls old_vers old_fixities new_fixities old new
  = diff True empty emptyNameEnv old new
  where
	-- When seeing if two decls are the same, 
	-- remember to check whether any relevant fixity has changed
    eq_tc  d1 d2 = d1 == d2 && all (same_fixity . fst) (tyClDeclNames d1)
    same_fixity n = lookupNameEnv old_fixities n == lookupNameEnv new_fixities n

    diff ok_so_far pp new_vers []  []      = (ok_so_far, pp, new_vers)
    diff ok_so_far pp new_vers (od:ods) [] = diff False (pp $$ only_old od) new_vers ods []
    diff ok_so_far pp new_vers [] (nd:nds) = diff False (pp $$ only_new nd) new_vers [] nds
    diff ok_so_far pp new_vers (od:ods) (nd:nds)
	= case od_name `compare` nd_name of
		LT -> diff False (pp $$ only_old od) new_vers ods      (nd:nds)
		GT -> diff False (pp $$ only_new nd) new_vers (od:ods) nds
		EQ | od `eq_tc` nd -> diff ok_so_far pp 		   new_vers  ods nds
		   | otherwise     -> diff False     (pp $$ changed od nd) new_vers' ods nds
	where
 	  od_name = tyClDeclName od
 	  nd_name = tyClDeclName nd
	  new_vers' = extendNameEnv new_vers nd_name 
				    (bumpVersion False (lookupNameEnv_NF old_vers od_name))

    only_old d   = ptext SLIT("Only in old iface:") <+> ppr d
    only_new d   = ptext SLIT("Only in new iface:") <+> ppr d
    changed d nd = ptext SLIT("Changed in iface: ") <+> ((ptext SLIT("Old:") <+> ppr d) $$ 
							 (ptext SLIT("New:") <+> ppr nd))
\end{code}



%************************************************************************
%*				 					*
\subsection{Writing an interface file}
%*				 					*
%************************************************************************

\begin{code}
writeIface :: FilePath -> ModIface -> IO ()
writeIface hi_path mod_iface
  = do	{ if_hdl <- openFile hi_path WriteMode
	; printForIface if_hdl from_this_mod (pprIface mod_iface)
	; hClose if_hdl
	}
  where
	-- Print names unqualified if they are from this module
    from_this_mod n = nameModule n == this_mod
    this_mod = mi_module mod_iface
	 
pprIface :: ModIface -> SDoc
pprIface iface
 = vcat [ ptext SLIT("__interface")
		<+> doubleQuotes (ptext opt_InPackage)
		<+> ppr (mi_module iface) <+> ppr (vers_module version_info)
		<+> pp_sub_vers
		<+> (if mi_orphan iface then char '!' else empty)
		<+> int opt_HiVersion
		<+> ptext SLIT("where")

	, vcat (map pprExport (mi_exports iface))
	, vcat (map pprUsage (mi_usages iface))

	, pprFixities (mi_fixities iface) (dcl_tycl decls)
	, pprIfaceDecls (vers_decls version_info) decls
	, pprDeprecs (mi_deprecs iface)
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
pprExport :: (ModuleName, Avails) -> SDoc
pprExport (mod, items)
 = hsep [ ptext SLIT("__export "), ppr mod, hsep (map pp_avail items) ] <> semi
  where
    pp_avail :: AvailInfo -> SDoc
    pp_avail (Avail name)    		     = pprOcc name
    pp_avail (AvailTC n [])		     = empty
    pp_avail (AvailTC n (n':ns)) | n==n'     = pprOcc n		    <> pp_export ns
				 | otherwise = pprOcc n <> char '|' <> pp_export (n':ns)
    
    pp_export []    = empty
    pp_export names = braces (hsep (map pprOcc names))

pprOcc :: Name -> SDoc	-- Print the occurrence name only
pprOcc n = pprOccName (nameOccName n)
\end{code}


\begin{code}
pprUsage :: ImportVersion Name -> SDoc
pprUsage (m, has_orphans, is_boot, whats_imported)
  = hsep [ptext SLIT("import"), ppr m, 
	  pp_orphan, pp_boot,
	  pp_versions whats_imported
    ] <> semi
  where
    pp_orphan | has_orphans = char '!'
	      | otherwise   = empty
    pp_boot   | is_boot     = char '@'
              | otherwise   = empty

	-- Importing the whole module is indicated by an empty list
    pp_versions NothingAtAll   		    = empty
    pp_versions (Everything v) 		    = dcolon <+> int v
    pp_versions (Specifically vm ve nvs vr) = dcolon <+> int vm <+> pp_export_version ve <+> int vr 
					      <+> hsep [ pprOcc n <+> int v | (n,v) <- nvs ]

    pp_export_version Nothing  = empty
    pp_export_version (Just v) = int v
\end{code}

\begin{code}
pprIfaceDecls version_map decls
  = vcat [ vcat [ppr i <+> semi | i <- dcl_insts decls]
	 , vcat (map ppr_decl (dcl_tycl decls))
	 , pprRules (dcl_rules decls)
	 ]
  where
    ppr_decl d  = ppr_vers d <+> ppr d <> semi

	-- Print the version for the decl
    ppr_vers d = case lookupNameEnv version_map (tyClDeclName d) of
		   Nothing -> empty
		   Just v  -> int v
\end{code}

\begin{code}
pprFixities fixity_map decls
  = hsep [ ppr fix <+> ppr n 
	 | d <- decls, 
	   (n,_) <- tyClDeclNames d, 
	   Just fix <- [lookupNameEnv fixity_map n]] <> semi

pprRules []    = empty
pprRules rules = hsep [ptext SLIT("{-## __R"), vcat (map ppr rules), ptext SLIT("##-}")]

pprDeprecs NoDeprecs = empty
pprDeprecs deprecs   = ptext SLIT("{-## __D") <+> guts <+> ptext SLIT("##-}")
		     where
		       guts = case deprecs of
				DeprecAll txt  -> doubleQuotes (ptext txt)
				DeprecSome env -> pp_deprecs env

pp_deprecs env = vcat (punctuate semi (map pp_deprec (nameEnvElts env)))
	       where
		 pp_deprec (name, txt) = pprOcc name <+> doubleQuotes (ptext txt)
\end{code}
