%
% (c) The GRASP/AQUA Project, Glasgow University, 1993-1998
%
\section[MkIface]{Print an interface for a module}

\begin{code}
module MkIface ( 
	mkModDetails, mkModDetailsFromIface, completeIface, writeIface
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
import HscTypes		( VersionInfo(..), IfaceDecls(..), ModIface(..), ModDetails(..),
			  TyThing(..), DFunId, TypeEnv, isTyClThing, Avails,
			  WhatsImported(..), GenAvailInfo(..), RdrAvailInfo,
			  ImportVersion
			)

import CmdLineOpts
import Id		( Id, idType, idInfo, omitIfaceSigForId, isUserExportedId, hasNoBinding,
			  idSpecialisation, idName, setIdInfo
			)
import Var		( isId )
import VarSet
import DataCon		( StrictnessMark(..), dataConSig, dataConFieldLabels, dataConStrictMarks )
import IdInfo		-- Lots
import CoreSyn		( CoreExpr, CoreBind, Bind(..), CoreRule(..), IdCoreRule, 
			  isBuiltinRule, rulesRules, rulesRhsFreeVars, emptyCoreRules,
			  bindersOfBinds
			)
import CoreFVs		( exprSomeFreeVars, ruleSomeLhsFreeVars, ruleSomeFreeVars )
import CoreUnfold	( okToUnfoldInHiFile, mkTopUnfolding, neverUnfold, unfoldingTemplate, noUnfolding )
import Name		( isLocallyDefined, getName, 
			  Name, NamedThing(..),
			  plusNameEnv, lookupNameEnv, emptyNameEnv, mkNameEnv,
			  extendNameEnv, lookupNameEnv_NF, nameEnvElts
			)
import OccName		( pprOccName )
import TyCon		( TyCon, getSynTyConDefn, isSynTyCon, isNewTyCon, isAlgTyCon,
			  tyConTheta, tyConTyVars, tyConDataCons, tyConFamilySize
			)
import Class		( classExtraBigSig, DefMeth(..) )
import FieldLabel	( fieldLabelType )
import Type		( splitSigmaTy, tidyTopType, deNoteType )
import SrcLoc		( noSrcLoc )
import Outputable
import Module		( ModuleName, moduleName )

import List		( partition )
import IO		( IOMode(..), openFile, hClose )
\end{code}


%************************************************************************
%*				 					*
\subsection{Write a new interface file}
%*				 					*
%************************************************************************

\begin{code}
mkModDetails :: TypeEnv -> [DFunId]	-- From typechecker
	     -> [CoreBind] -> [Id]	-- Final bindings, plus the top-level Ids from the
					-- code generator; they have authoritative arity info
	     -> [IdCoreRule]		-- Tidy orphan rules
	     -> ModDetails
mkModDetails type_env dfun_ids tidy_binds stg_ids orphan_rules
  = ModDetails { md_types = new_type_env,
		 md_rules = rule_dcls,
		 md_insts = dfun_ids }
  where
	-- The competed type environment is gotten from
	-- 	a) keeping the types and classes
	--	b) removing all Ids, and Ids with correct IdInfo
	--		gotten from the bindings
    new_type_env = mkNameEnv [(getName tycl, tycl) | tycl <- orig_type_env, isTyClThing tycl]
			`plusNameEnv`
		   mkNameEnv [(idName id, AnId id) | id <- final_ids]

    orig_type_env = nameEnvElts type_env

    final_ids = bindsToIds (mkVarSet dfun_ids `unionVarSet` orphan_rule_ids)
			   (mkVarSet stg_ids)
			   tidy_binds

	-- The complete rules are gotten by combining
	--	a) the orphan rules
	--	b) rules embedded in the top-level Ids
    rule_dcls | opt_OmitInterfacePragmas = []
	      | otherwise		  = getRules orphan_rules tidy_binds (mkVarSet final_ids)

    orphan_rule_ids = unionVarSets [ ruleSomeFreeVars interestingId rule 
				   | (_, rule) <- orphan_rules]


-- This version is used when we are re-linking a module
-- so we've only run the type checker on its previous interface 
mkModDetailsFromIface :: TypeEnv -> [DFunId]	-- From typechecker
		      -> [TypecheckedRuleDecl]
		      -> ModDetails
mkModDetailsFromIface type_env dfun_ids rules
  = ModDetails { md_types = type_env,
		 md_rules = rule_dcls,
		 md_insts = dfun_ids }
  where
    rule_dcls = [(id,rule) | IfaceRuleOut id rule <- rules]
	-- All the rules from an interface are of the IfaceRuleOut form


completeIface :: Maybe ModIface		-- The old interface, if we have it
	      -> ModIface		-- The new one, minus the decls and versions
	      -> ModDetails		-- The ModDetails for this module
	      -> Maybe (ModIface, SDoc)	-- The new one, complete with decls and versions
					-- The SDoc is a debug document giving differences
					-- Nothing => no change

	-- NB: 'Nothing' means that even the usages havn't changed, so there's no
	--     need to write a new interface file.  But even if the usages have
	--     changed, the module version may not have.
	--
	-- The IO in the type is solely for debug output
	-- In particular, dumping a record of what has changed
completeIface maybe_old_iface new_iface mod_details 
  = addVersionInfo maybe_old_iface (new_iface { mi_decls = new_decls })
  where
     new_decls = IfaceDecls { dcl_tycl  = ty_cls_dcls,
			      dcl_insts = inst_dcls,
			      dcl_rules = rule_dcls }

     inst_dcls   = map ifaceInstance (md_insts mod_details)
     ty_cls_dcls = map ifaceTyCls (nameEnvElts (md_types mod_details))
     rule_dcls   = map ifaceRule (md_rules mod_details)
\end{code}


%************************************************************************
%*				 					*
\subsection{Types and classes}
%*				 					*
%************************************************************************

\begin{code}
ifaceTyCls :: TyThing -> RenamedTyClDecl
ifaceTyCls (AClass clas)
  = ClassDecl (toHsContext sc_theta)
	      (getName clas)
	      (toHsTyVars clas_tyvars)
	      (toHsFDs clas_fds)
	      (map toClassOpSig op_stuff)
	      EmptyMonoBinds
	      [] noSrcLoc
  where
     (clas_tyvars, clas_fds, sc_theta, _, op_stuff) = classExtraBigSig clas

     toClassOpSig (sel_id, def_meth)
	= ASSERT(sel_tyvars == clas_tyvars)
	  ClassOpSig (getName sel_id) (Just def_meth') (toHsType op_ty) noSrcLoc
	where
	  (sel_tyvars, _, op_ty) = splitSigmaTy (idType sel_id)
	  def_meth' = case def_meth of
			 NoDefMeth  -> NoDefMeth
			 GenDefMeth -> GenDefMeth
			 DefMeth id -> DefMeth (getName id)

ifaceTyCls (ATyCon tycon)
  | isSynTyCon tycon
  = TySynonym (getName tycon)(toHsTyVars tyvars) (toHsType ty) noSrcLoc
  where
    (tyvars, ty) = getSynTyConDefn tycon

ifaceTyCls (ATyCon tycon)
  | isAlgTyCon tycon
  = TyData new_or_data (toHsContext (tyConTheta tycon))
	   (getName tycon)
	   (toHsTyVars tyvars)
	   (map ifaceConDecl (tyConDataCons tycon))
	   (tyConFamilySize tycon)
	   Nothing noSrcLoc (panic "gen1") (panic "gen2")
  where
    tyvars = tyConTyVars tycon
    new_or_data | isNewTyCon tycon = NewType
	        | otherwise	   = DataType

    ifaceConDecl data_con 
	= ConDecl (getName data_con) (error "ifaceConDecl")
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

ifaceTyCls (ATyCon tycon) = pprPanic "ifaceTyCls" (ppr tycon)

ifaceTyCls (AnId id) 
  = IfaceSig (getName id) (toHsType id_type) hs_idinfo noSrcLoc
  where
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
    wrkr_hsinfo = case workerInfo id_info of
		    HasWorker work_id wrap_arity -> [HsWorker (getName work_id)]
		    NoWorker			 -> []

    ------------  Unfolding  --------------
    unfold_info = unfoldingInfo id_info
    inline_prag = inlinePragInfo id_info
    rhs		= unfoldingTemplate unfold_info
    unfold_hsinfo | neverUnfold unfold_info = []
		  | otherwise		    = [HsUnfold inline_prag (toUfExpr rhs)]
\end{code}


%************************************************************************
%*				 					*
\subsection{Instances and rules}
%*				 					*
%************************************************************************

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
\subsection{Compute final Ids}
%*				 					* 
%************************************************************************

A "final Id" has exactly the IdInfo for going into an interface file, or
exporting to another module.

\begin{code}
bindsToIds :: IdSet		-- These Ids are needed already
	   -> IdSet		-- Ids used at code-gen time; they have better pragma info!
	   -> [CoreBind]	-- In dependency order, later depend on earlier
	   -> [Id]		-- Set of Ids actually spat out, complete with exactly the IdInfo
				-- they need for exporting to another module

bindsToIds needed_ids codegen_ids binds
  = go needed_ids (reverse binds) []
		-- Reverse so that later things will 
		-- provoke earlier ones to be emitted
  where
	-- The 'needed' set contains the Ids that are needed by earlier
	-- interface file emissions.  If the Id isn't in this set, and isn't
	-- exported, there's no need to emit anything
    need_id needed_set id = id `elemVarSet` needed_set || isUserExportedId id 

    go needed [] emitted
	| not (isEmptyVarSet needed) = pprTrace "ifaceBinds: free vars:" 
					  (sep (map ppr (varSetElems needed)))
				       emitted
	| otherwise 		     = emitted

    go needed (NonRec id rhs : binds) emitted
	| need_id needed id
	= if omitIfaceSigForId id then
	    go (needed `delVarSet` id) binds (id:emitted)
	  else
	    go ((needed `unionVarSet` extras) `delVarSet` id)
	       binds
	       (new_id:emitted)
	| otherwise
	= go needed binds emitted
	where
	  (new_id, extras) = mkFinalId codegen_ids False id rhs

	-- Recursive groups are a bit more of a pain.  We may only need one to
	-- start with, but it may call out the next one, and so on.  So we
	-- have to look for a fixed point.  We don't want necessarily them all, 
	-- because without -O we may only need the first one (if we don't emit
	-- its unfolding)
    go needed (Rec pairs : binds) emitted
	= go needed' binds emitted' 
	where
	  (new_emitted, extras) = go_rec needed pairs
	  needed'  = (needed `unionVarSet` extras) `minusVarSet` mkVarSet (map fst pairs) 
	  emitted' = new_emitted ++ emitted 

    go_rec :: IdSet -> [(Id,CoreExpr)] -> ([Id], IdSet)
    go_rec needed pairs
	| null needed_prs = ([], emptyVarSet)
	| otherwise 	  = (emitted ++           more_emitted,
			     extras `unionVarSet` more_extras)
	where
	  (needed_prs,leftover_prs)   = partition is_needed pairs
	  (emitted, extras_s)         = unzip [ mkFinalId codegen_ids True id rhs 
				    	      | (id,rhs) <- needed_prs, not (omitIfaceSigForId id)]
	  extras	              = unionVarSets extras_s
	  (more_emitted, more_extras) = go_rec extras leftover_prs

	  is_needed (id,_) = need_id needed id
\end{code}



\begin{code}
mkFinalId :: IdSet		-- The Ids with arity info from the code generator
	  -> Bool			-- True <=> recursive, so don't include unfolding
	  -> Id
	  -> CoreExpr		-- The Id's right hand side
	  -> (Id, IdSet)		-- The emitted id, plus any *extra* needed Ids

mkFinalId codegen_ids is_rec id rhs
  = (id `setIdInfo` new_idinfo, new_needed_ids)
  where
    core_idinfo = idInfo id
    stg_idinfo  = case lookupVarSet codegen_ids id of
			Just id' -> idInfo id'
			Nothing  -> pprTrace "ifaceBinds not found:" (ppr id) $
				    idInfo id

    new_idinfo | opt_OmitInterfacePragmas
	       = vanillaIdInfo
 	       | otherwise		  
	       = core_idinfo `setArityInfo` 	 arity_info
			     `setCafInfo`   	 cafInfo stg_idinfo
			     `setUnfoldingInfo`	 unfold_info
			     `setWorkerInfo`	 worker_info
			     `setSpecInfo`	 emptyCoreRules
	-- We zap the specialisations because they are
	-- passed on separately through the modules IdCoreRules

    ------------  Arity  --------------
    arity_info = arityInfo stg_idinfo
    stg_arity  = arityLowerBound arity_info

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
    worker_info = case workerInfo core_idinfo of
		     info@(HasWorker work_id wrap_arity)
			| wrap_arity == stg_arity -> info
			| otherwise	          -> pprTrace "ifaceId: arity change:" (ppr id) 
						     NoWorker
		     NoWorker		          -> NoWorker

    has_worker = case worker_info of
		   HasWorker _ _ -> True
		   other	 -> False

    HasWorker work_id _ = worker_info

    ------------  Unfolding  --------------
    inline_pragma  = inlinePragInfo core_idinfo
    dont_inline	   = isNeverInlinePrag inline_pragma
    loop_breaker   = isLoopBreaker (occInfo core_idinfo)
    bottoming_fn   = isBottomingStrictness (strictnessInfo core_idinfo)

    unfolding    = mkTopUnfolding rhs
    rhs_is_small = neverUnfold unfolding

    unfold_info | show_unfold = unfolding
		| otherwise   = noUnfolding

    show_unfold = not has_worker	 &&	-- Not unnecessary
		  not bottoming_fn	 &&	-- Not necessary
		  not dont_inline	 &&
		  not loop_breaker	 &&
		  rhs_is_small		 &&	-- Small enough
		  okToUnfoldInHiFile rhs 	-- No casms etc


    ------------  Extra free Ids  --------------
    new_needed_ids | opt_OmitInterfacePragmas = emptyVarSet
	           | otherwise		      = worker_ids	`unionVarSet`
						unfold_ids	`unionVarSet`
						spec_ids

    spec_ids = filterVarSet interestingId (rulesRhsFreeVars (specInfo core_idinfo))

    worker_ids | has_worker && interestingId work_id = unitVarSet work_id
			-- Conceivably, the worker might come from
			-- another module
	       | otherwise = emptyVarSet

    unfold_ids | show_unfold = find_fvs rhs
	       | otherwise   = emptyVarSet

    find_fvs expr = exprSomeFreeVars interestingId expr

interestingId id = isId id && isLocallyDefined id && not (hasNoBinding id)
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
\end{code}


%************************************************************************
%*				 					*
\subsection{Checking if the new interface is up to date
%*				 					*
%************************************************************************

\begin{code}
addVersionInfo :: Maybe ModIface		-- The old interface, read from M.hi
	       -> ModIface			-- The new interface decls
	       -> Maybe (ModIface, SDoc)	-- Nothing => no change; no need to write new Iface
						-- Just mi => Here is the new interface to write
						-- 	      with correct version numbers

-- NB: the fixities, declarations, rules are all assumed
-- to be sorted by increasing order of hsDeclName, so that 
-- we can compare for equality

addVersionInfo Nothing new_iface
-- No old interface, so definitely write a new one!
  = Just (new_iface, text "No old interface available")

addVersionInfo (Just old_iface@(ModIface { mi_version = old_version, 
				       	   mi_decls   = old_decls,
				       	   mi_fixities = old_fixities }))
	       new_iface@(ModIface { mi_decls = new_decls,
				     mi_fixities = new_fixities })

  | no_output_change && no_usage_change
  = Nothing

  | otherwise		-- Add updated version numbers
  = Just (final_iface, pp_tc_diffs)
	
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
    diff ok_so_far pp new_vers old []      = (False,     pp, new_vers)
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
				    (bumpVersion True (lookupNameEnv_NF old_vers od_name))

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
--writeIface :: Finder -> ModIface -> IO ()
writeIface {-finder-} mod_iface
  = do	{ let filename = error "... find the right file..."
	; if_hdl <- openFile filename WriteMode
	; printForIface if_hdl (pprIface mod_iface)
	; hClose if_hdl
	}

pprIface iface
 = vcat [ ptext SLIT("__interface")
		<+> doubleQuotes (ptext opt_InPackage)
		<+> ppr (mi_module iface) <+> ppr (vers_module version_info)
		<+> pp_sub_vers
		<+> (if mi_orphan iface then char '!' else empty)
		<+> int opt_HiVersion
		<+> ptext SLIT("where")

	, pprExport (mi_exports iface)
	, vcat (map pprUsage (mi_usages iface))

	, pprIfaceDecls (vers_decls version_info) 
			(mi_fixities iface)
			(mi_decls iface)

	, pprDeprecs (mi_deprecs iface)
	]
  where
    version_info = mi_version iface
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
    pp_avail :: RdrAvailInfo -> SDoc
    pp_avail (Avail name)      = pprOccName name
    pp_avail (AvailTC name []) = empty
    pp_avail (AvailTC name ns) = hcat [pprOccName name, bang, pp_export ns']
				where
				  bang | name `elem` ns = empty
				       | otherwise	= char '|'
				  ns' = filter (/= name) ns
    
    pp_export []    = empty
    pp_export names = braces (hsep (map pprOccName names))
\end{code}


\begin{code}
pprUsage :: ImportVersion Name -> SDoc
pprUsage (m, has_orphans, is_boot, whats_imported)
  = hsep [ptext SLIT("import"), ppr (moduleName m), 
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
					      <+> hsep [ ppr n <+> int v | (n,v) <- nvs ]

	-- HACK for the moment: print the export-list version even if
	-- we don't use it, so that syntax of interface files doesn't change
    pp_export_version Nothing  = int 1
    pp_export_version (Just v) = int v
\end{code}

\begin{code}
pprIfaceDecls version_map fixity_map decls
  = vcat [ vcat [ppr i <+> semi | i <- dcl_insts decls]
	 , vcat (map ppr_decl (dcl_tycl decls))
	 , pprRules (dcl_rules decls)
	 ]
  where
    ppr_decl d  = (ppr_vers d <+> ppr d <> semi) $$ ppr_fixes d

	-- Print the version for the decl
    ppr_vers d = case lookupNameEnv version_map (tyClDeclName d) of
		   Nothing -> empty
		   Just v  -> int v

	-- Print fixities relevant to the decl
    ppr_fixes d = vcat (map ppr_fix d)
    ppr_fix d   = [ ppr fix <+> ppr n <> semi
		  | n <- tyClDeclNames d, 
		    [Just fix] <- lookupNameEnv fixity_map n
		  ]
\end{code}

\begin{code}
pprRules []    = empty
pprRules rules = hsep [ptext SLIT("{-## __R"), vcat (map ppr rules), ptext SLIT("##-}")]

pprDeprecs []   = empty
pprDeprecs deps = hsep [ ptext SLIT("{-## __D"), guts, ptext SLIT("##-}")]
		where
		  guts = hsep [ ppr ie <+> doubleQuotes (ppr txt) <> semi 
			      | Deprecation ie txt _ <- deps ]
\end{code}
