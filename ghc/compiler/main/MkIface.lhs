%
% (c) The GRASP/AQUA Project, Glasgow University, 1993-1998
%
\section[MkIface]{Print an interface for a module}

\begin{code}
module MkIface ( completeIface ) where

#include "HsVersions.h"

import HsSyn
import HsCore		( HsIdInfo(..), toUfExpr, ifaceSigName )
import HsTypes		( toHsTyVars )
import BasicTypes	( Fixity(..), NewOrData(..),
			  Version, bumpVersion, isLoopBreaker
			)
import RnMonad
import RnHsSyn		( RenamedInstDecl, RenamedTyClDecl, RenamedRuleDecl, RenamedIfaceSig )
import HscTypes		( VersionInfo(..), IfaceDecls(..), ModIface(..), ModDetails(..),
			  TyThing(..), DFunId )

import CmdLineOpts
import Id		( Id, idType, idInfo, omitIfaceSigForId, isUserExportedId, hasNoBinding,
			  idSpecialisation
			)
import Var		( isId )
import VarSet
import DataCon		( StrictnessMark(..), dataConSig, dataConFieldLabels, dataConStrictMarks )
import IdInfo		( IdInfo, StrictnessInfo(..), ArityInfo(..), 
			  CprInfo(..), CafInfo(..),
			  inlinePragInfo, arityInfo, arityLowerBound,
			  strictnessInfo, isBottomingStrictness,
			  cafInfo, specInfo, cprInfo, 
			  occInfo, isNeverInlinePrag,
			  workerInfo, WorkerInfo(..)
			)
import CoreSyn		( CoreExpr, CoreBind, Bind(..), isBuiltinRule, rulesRules, rulesRhsFreeVars )
import CoreFVs		( exprSomeFreeVars, ruleSomeLhsFreeVars, ruleSomeFreeVars )
import CoreUnfold	( okToUnfoldInHiFile, couldBeSmallEnoughToInline )
import Name		( isLocallyDefined, getName, nameModule,
			  Name, NamedThing(..),
			  plusNameEnv, lookupNameEnv, emptyNameEnv, extendNameEnv, lookupNameEnv_NF, nameEnvElts
			)
import TyCon		( TyCon, getSynTyConDefn, isSynTyCon, isNewTyCon, isAlgTyCon,
			  tyConTheta, tyConTyVars, tyConDataCons, tyConFamilySize
			)
import Class		( classExtraBigSig, DefMeth(..) )
import FieldLabel	( fieldLabelType )
import Type		( splitSigmaTy, tidyTopType, deNoteType )

import Rules		( ProtoCoreRule(..) )

import Bag		( bagToList )
import UniqFM		( lookupUFM, listToUFM )
import SrcLoc		( noSrcLoc )
import Bag
import Outputable

import List		( partition )
\end{code}


%************************************************************************
%*				 					*
\subsection{Write a new interface file}
%*				 					*
%************************************************************************

\begin{code}
completeModDetails :: ModDetails
	  	   -> [CoreBind] -> [Id]	-- Final bindings, plus the top-level Ids from the
						-- code generator; they have authoritative arity info
		   -> [ProtoCoreRule]		-- Tidy orphan rules
		   -> ModDetails

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
	      tidy_binds final_ids tidy_orphan_rules
  = let
	new_decls = declsFromDetails mod_details tidy_binds final_ids tidy_orphan_rules
    in
    addVersionInfo maybe_old_iface (new_iface { mi_decls = new_decls })

declsFromDetails :: ModDetails -> [CoreBind] -> [Id] -> [ProtoCoreRule] -> IfaceDecls
declsFromDetails details tidy_binds final_ids tidy_orphan_rules
   = IfaceDecls { dcl_tycl  = ty_cls_dcls ++ bagToList val_dcls,
		  dcl_insts = inst_dcls,
		  dcl_rules = rule_dcls }
   where
     dfun_ids	 = md_insts details
     inst_dcls   = map ifaceInstance dfun_ids
     ty_cls_dcls = map ifaceTyCls (filter emitTyCls (nameEnvElts (md_types details)))
  
     (val_dcls, emitted_ids) = ifaceBinds (mkVarSet dfun_ids `unionVarSet` orphan_rule_ids)
					  final_ids tidy_binds

     rule_dcls | opt_OmitInterfacePragmas = []
	       | otherwise		  = ifaceRules tidy_orphan_rules emitted_ids

     orphan_rule_ids = unionVarSets [ ruleSomeFreeVars interestingId rule 
				    | ProtoCoreRule _ _ rule <- tidy_orphan_rules]

\end{code}

%************************************************************************
%*				 					*
\subsection{Types and classes}
%*				 					*
%************************************************************************

\begin{code}
emitTyCls :: TyThing -> Bool
emitTyCls (ATyCon tc) = True	-- Could filter out wired in ones, but it's not
				-- strictly necessary, and it costs extra time
emitTyCls (AClass cl) = True
emitTyCls (AnId   _)  = False


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
\end{code}

\begin{code}
ifaceRules :: [ProtoCoreRule] -> IdSet -> [RenamedRuleDecl]
ifaceRules rules emitted
  = orphan_rules ++ local_rules
  where
    orphan_rules = [ toHsRule fn rule | ProtoCoreRule _ fn rule <- rules ]
    local_rules  = [ toHsRule fn rule
 		   | fn <- varSetElems emitted, 
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
\subsection{Value bindings}
%*				 					* 
%************************************************************************

\begin{code}
ifaceBinds :: IdSet		-- These Ids are needed already
	   -> [Id]		-- Ids used at code-gen time; they have better pragma info!
	   -> [CoreBind]	-- In dependency order, later depend on earlier
	   -> (Bag RenamedIfaceSig, IdSet)		-- Set of Ids actually spat out

ifaceBinds needed_ids final_ids binds
  = go needed_ids (reverse binds) emptyBag emptyVarSet 
		-- Reverse so that later things will 
		-- provoke earlier ones to be emitted
  where
    final_id_map  = listToUFM [(id,id) | id <- final_ids]
    get_idinfo id = case lookupUFM final_id_map id of
			Just id' -> idInfo id'
			Nothing  -> pprTrace "ifaceBinds not found:" (ppr id) $
				    idInfo id

	-- The 'needed' set contains the Ids that are needed by earlier
	-- interface file emissions.  If the Id isn't in this set, and isn't
	-- exported, there's no need to emit anything
    need_id needed_set id = id `elemVarSet` needed_set || isUserExportedId id 

    go needed [] decls emitted
	| not (isEmptyVarSet needed) = pprTrace "ifaceBinds: free vars:" 
					  (sep (map ppr (varSetElems needed)))
				       (decls, emitted)
	| otherwise 		     = (decls, emitted)

    go needed (NonRec id rhs : binds) decls emitted
	| need_id needed id
	= if omitIfaceSigForId id then
	    go (needed `delVarSet` id) binds decls (emitted `extendVarSet` id)
	  else
	    go ((needed `unionVarSet` extras) `delVarSet` id)
	       binds
	       (decl `consBag` decls)
	       (emitted `extendVarSet` id)
	| otherwise
	= go needed binds decls emitted
	where
	  (decl, extras) = ifaceId get_idinfo False id rhs

	-- Recursive groups are a bit more of a pain.  We may only need one to
	-- start with, but it may call out the next one, and so on.  So we
	-- have to look for a fixed point.  We don't want necessarily them all, 
	-- because without -O we may only need the first one (if we don't emit
	-- its unfolding)
    go needed (Rec pairs : binds) decls emitted
	= go needed' binds decls' emitted' 
	where
	  (new_decls, new_emitted, extras) = go_rec needed pairs
	  decls'   = new_decls `unionBags` decls
	  needed'  = (needed `unionVarSet` extras) `minusVarSet` mkVarSet (map fst pairs) 
	  emitted' = emitted `unionVarSet` new_emitted

    go_rec :: IdSet -> [(Id,CoreExpr)] -> (Bag RenamedIfaceSig, IdSet, IdSet)
    go_rec needed pairs
	| null decls = (emptyBag, emptyVarSet, emptyVarSet)
	| otherwise  = (more_decls   `unionBags`   listToBag decls, 
			more_emitted `unionVarSet` mkVarSet (map fst needed_prs),
			more_extras  `unionVarSet` extras)
	where
	  (needed_prs,leftover_prs) = partition is_needed pairs
	  (decls, extras_s)         = unzip [ifaceId get_idinfo True id rhs 
				            | (id,rhs) <- needed_prs, not (omitIfaceSigForId id)]
	  extras	            = unionVarSets extras_s
	  (more_decls, more_emitted, more_extras) = go_rec extras leftover_prs
	  is_needed (id,_) = need_id needed id
\end{code}


\begin{code}
ifaceId :: (Id -> IdInfo)	-- This function "knows" the extra info added
				-- by the STG passes.  Sigh
	-> Bool			-- True <=> recursive, so don't print unfolding
	-> Id
	-> CoreExpr		-- The Id's right hand side
	-> (RenamedTyClDecl, IdSet)	-- The emitted stuff, plus any *extra* needed Ids

ifaceId get_idinfo is_rec id rhs
  = (IfaceSig (getName id) (toHsType id_type) hs_idinfo noSrcLoc,  new_needed_ids)
  where
    id_type     = idType id
    core_idinfo = idInfo id
    stg_idinfo  = get_idinfo id

    hs_idinfo | opt_OmitInterfacePragmas = []
 	      | otherwise		 = arity_hsinfo  ++ caf_hsinfo  ++ cpr_hsinfo ++ 
					   strict_hsinfo ++ wrkr_hsinfo ++ unfold_hsinfo

    ------------  Arity  --------------
    arity_info   = arityInfo stg_idinfo
    stg_arity	 = arityLowerBound arity_info
    arity_hsinfo = case arityInfo stg_idinfo of
			a@(ArityExactly n) -> [HsArity a]
			other		   -> []

    ------------ Caf Info --------------
    caf_hsinfo = case cafInfo stg_idinfo of
		   NoCafRefs -> [HsNoCafRefs]
		   otherwise -> []

    ------------ CPR Info --------------
    cpr_hsinfo = case cprInfo core_idinfo of
		   ReturnsCPR -> [HsCprInfo]
		   NoCPRInfo  -> []

    ------------  Strictness  --------------
    strict_info   = strictnessInfo core_idinfo
    bottoming_fn  = isBottomingStrictness strict_info
    strict_hsinfo = case strict_info of
			NoStrictnessInfo -> []
			info		 -> [HsStrictness info]


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
    work_info           = workerInfo core_idinfo
    HasWorker work_id _ = work_info

    has_worker = case work_info of
		  HasWorker work_id wrap_arity 
		   | wrap_arity == stg_arity -> True
		   | otherwise		     -> pprTrace "ifaceId: arity change:" (ppr id) 
						False
							  
		  other			     -> False

    wrkr_hsinfo | has_worker = [HsWorker (getName work_id)]
		| otherwise  = []

    ------------  Unfolding  --------------
    inline_pragma  = inlinePragInfo core_idinfo
    dont_inline	   = isNeverInlinePrag inline_pragma

    unfold_hsinfo | show_unfold = [HsUnfold inline_pragma (toUfExpr rhs)]
		  | otherwise   = []

    show_unfold = not has_worker	 &&	-- Not unnecessary
		  not bottoming_fn	 &&	-- Not necessary
		  not dont_inline	 &&
		  not loop_breaker	 &&
		  rhs_is_small		 &&	-- Small enough
		  okToUnfoldInHiFile rhs 	-- No casms etc

    rhs_is_small = couldBeSmallEnoughToInline opt_UF_HiFileThreshold rhs

    ------------  Specialisations --------------
    spec_info   = specInfo core_idinfo
    
    ------------  Occ info  --------------
    loop_breaker  = isLoopBreaker (occInfo core_idinfo)

    ------------  Extra free Ids  --------------
    new_needed_ids | opt_OmitInterfacePragmas = emptyVarSet
	           | otherwise		      = worker_ids	`unionVarSet`
						unfold_ids	`unionVarSet`
						spec_ids

    worker_ids | has_worker && interestingId work_id = unitVarSet work_id
			-- Conceivably, the worker might come from
			-- another module
	       | otherwise = emptyVarSet

    spec_ids = filterVarSet interestingId (rulesRhsFreeVars spec_info)

    unfold_ids | show_unfold = find_fvs rhs
	       | otherwise   = emptyVarSet

    find_fvs expr = exprSomeFreeVars interestingId expr

interestingId id = isId id && isLocallyDefined id && not (hasNoBinding id)
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
  = Just (final_iface, pp_tc_diffs $$ pp_sig_diffs)
	
  where
    final_iface = new_iface { mi_version = new_version }
    new_version = VersionInfo { vers_module  = bumpVersion no_output_change (vers_module  old_version),
				vers_exports = bumpVersion no_export_change (vers_exports old_version),
				vers_rules   = bumpVersion no_rule_change   (vers_rules   old_version),
				vers_decls   = sig_vers `plusNameEnv` tc_vers }

    no_output_change = no_tc_change && no_rule_change && no_export_change
    no_usage_change  = mi_usages old_iface == mi_usages new_iface

    no_export_change = mi_exports old_iface == mi_exports new_iface		-- Kept sorted
    no_rule_change   = dcl_rules old_decls  == dcl_rules  new_decls		-- Ditto

	-- Fill in the version number on the new declarations by looking at the old declarations.
	-- Set the flag if anything changes. 
	-- Assumes that the decls are sorted by hsDeclName.
    old_vers_decls = vers_decls old_version
    (no_tc_change,  pp_tc_diffs,  tc_vers) = diffDecls old_vers_decls (dcl_tycl old_decls) (dcl_tycl new_decls)



diffDecls :: NameEnv Version				-- Old version map
	  -> [RenamedTyClDecl] -> [RenamedTyClDecl]	-- Old and new decls
	  -> (Bool,		-- True <=> no change
	      SDoc,		-- Record of differences
	      NameEnv Version)	-- New version

diffDecls old_vers old new
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
		EQ | od `eq` nd -> diff ok_so_far pp 		         new_vers  ods nds
		   | otherwise  -> diff False	   (pp $$ changed od nd) new_vers' ods nds
	where
 	  od_name = get_name od
 	  nd_name = get_name nd
	  new_vers' = extendNameEnv new_vers nd_name 
				    (bumpVersion True (lookupNameEnv_NF old_vers od_name))

    only_old d   = ptext SLIT("Only in old iface:") <+> ppr d
    only_new d   = ptext SLIT("Only in new iface:") <+> ppr d
    changed d nd = ptext SLIT("Changed in iface: ") <+> ((ptext SLIT("Old:") <+> ppr d) $$ 
							 (ptext SLIT("New:") <+> ppr nd))
\end{code}
