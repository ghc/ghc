%
% (c) The GRASP/AQUA Project, Glasgow University, 1993-1998
%
\section[MkIface]{Print an interface for a module}

\begin{code}
module MkIface ( writeIface  ) where

#include "HsVersions.h"

import IO		( Handle, hPutStr, openFile, 
			  hClose, hPutStrLn, IOMode(..) )

import HsSyn
import HsCore		( HsIdInfo(..), toUfExpr )
import RdrHsSyn		( RdrNameRuleDecl )
import HsPragmas	( DataPragmas(..), ClassPragmas(..) )
import HsTypes		( toHsTyVars )
import BasicTypes	( Fixity(..), FixityDirection(..), NewOrData(..),
			  Version, bumpVersion, initialVersion, isLoopBreaker
			)
import RnMonad
import RnEnv		( availName )

import TcInstUtil	( InstInfo(..) )

import CmdLineOpts
import Id		( Id, idType, idInfo, omitIfaceSigForId, isUserExportedId,
			  idSpecialisation
			)
import Var		( isId )
import VarSet
import DataCon		( StrictnessMark(..), dataConSig, dataConFieldLabels, dataConStrictMarks )
import IdInfo		( IdInfo, StrictnessInfo(..), ArityInfo(..), InlinePragInfo(..), 
			  CprInfo(..), CafInfo(..),
			  inlinePragInfo, arityInfo, arityLowerBound,
			  strictnessInfo, isBottomingStrictness,
			  cafInfo, specInfo, cprInfo, 
			  occInfo, isNeverInlinePrag,
			  workerExists, workerInfo, WorkerInfo(..)
			)
import CoreSyn		( CoreExpr, CoreBind, Bind(..), isBuiltinRule, rulesRules, rulesRhsFreeVars )
import CoreFVs		( exprSomeFreeVars, ruleSomeLhsFreeVars, ruleSomeFreeVars )
import CoreUnfold	( okToUnfoldInHiFile, couldBeSmallEnoughToInline )
import Module		( moduleString, pprModule, pprModuleName )
import RdrName		( RdrName )
import Name		( isLocallyDefined, isWiredInName, toRdrName, nameModule,
			  Name, NamedThing(..)
			)
import OccName		( OccName, pprOccName )
import TyCon		( TyCon, getSynTyConDefn, isSynTyCon, isNewTyCon, isAlgTyCon,
			  tyConTheta, tyConTyVars, tyConDataCons, tyConFamilySize
			)
import Class		( Class, classExtraBigSig )
import FieldLabel	( fieldLabelName, fieldLabelType )
import Type		( mkSigmaTy, splitSigmaTy, mkDictTy, tidyTopType,
			  deNoteType, classesToPreds,
			  Type, ThetaType, PredType(..), ClassContext
		        )

import PprType
import FunDeps		( pprFundeps )
import Rules		( pprProtoCoreRule, ProtoCoreRule(..) )

import Bag		( bagToList, isEmptyBag )
import Maybes		( catMaybes, maybeToBool )
import FiniteMap	( emptyFM, addToFM, addToFM_C, fmToList, FiniteMap )
import UniqFM		( lookupUFM, listToUFM )
import UniqSet		( uniqSetToList )
import Util		( sortLt, mapAccumL )
import SrcLoc		( noSrcLoc )
import Bag
import Outputable
\end{code}


%************************************************************************
%*				 					*
\subsection{Write a new interface file}
%*				 					*
%************************************************************************

\begin{code}
writeIface this_mod old_iface new_iface
	   local_tycons local_classes inst_info
	   final_ids tidy_binds tidy_orphan_rules
  = case opt_ProduceHi of {
      Nothing -> return () ; -- not producing any .hi file

      Just filename ->

    case checkIface old_iface full_new_iface of {
	Nothing -> do { putStrLn "Interface file unchanged" ;
		        return () } ;	-- No need to update .hi file

	Just final_iface ->

    do  let mod_vers_unchanged = case old_iface of
				   Just iface -> pi_vers iface == pi_vers final_iface
				   Nothing -> False
     	if mod_vers_unchanged 
	   then putStrLn "Module version unchanged, but usages differ; hence need new hi file"
	   else return ()

	if_hdl <- openFile filename WriteMode
	printForIface if_hdl (pprIface final_iface)
	hClose if_hdl
    }}    
  where
    full_new_iface = completeIface new_iface local_tycons local_classes
				   	     inst_info final_ids tidy_binds
					     tidy_orphan_rules
\end{code}


%************************************************************************
%*				 					*
\subsection{Checking if the new interface is up to date
%*				 					*
%************************************************************************

\begin{code}
checkIface :: Maybe ParsedIface		-- The old interface, read from M.hi
	   -> ParsedIface		-- The new interface; but with all version numbers = 1
	   -> Maybe ParsedIface		-- Nothing => no change; no need to write new Iface
					-- Just pi => Here is the new interface to write
					-- 	      with correct version numbers

-- NB: the fixities, declarations, rules are all assumed
-- to be sorted by increasing order of hsDeclName, so that 
-- we can compare for equality

checkIface Nothing new_iface
-- No old interface, so definitely write a new one!
  = Just new_iface

checkIface (Just iface) new_iface
  | no_output_change && no_usage_change
  = Nothing

  | otherwise		-- Add updated version numbers
  = 
{-  pprTrace "checkIface" (
	vcat [ppr no_decl_changed <+> ppr no_export_change <+> ppr no_usage_change,
	      text "--------",
	      vcat (map ppr (pi_decls iface)),
	      text "--------",
	      vcat (map ppr (pi_decls new_iface))
	]) $
-}
    Just (new_iface { pi_vers = new_mod_vers,
		      pi_fixity = (new_fixity_vers, new_fixities),
		      pi_rules  = (new_rules_vers,  new_rules),
		      pi_decls  = final_decls
    })
	
  where
    no_usage_change = pi_usages iface == pi_usages new_iface

    no_output_change = no_decl_changed && 
	               new_fixity_vers == fixity_vers && 
	               new_rules_vers == rules_vers &&
	               no_export_change

    no_export_change = pi_exports iface == pi_exports new_iface

    new_mod_vers | no_output_change = mod_vers
		 | otherwise  	    = bumpVersion mod_vers

    mod_vers = pi_vers iface

    (fixity_vers, fixities) = pi_fixity iface
    (_,       new_fixities) = pi_fixity new_iface
    new_fixity_vers | fixities == new_fixities = fixity_vers
		    | otherwise		       = bumpVersion fixity_vers

    (rules_vers, rules) = pi_rules iface
    (_,      new_rules) = pi_rules new_iface
    new_rules_vers  | rules == new_rules = rules_vers
		    | otherwise		 = bumpVersion rules_vers

    (no_decl_changed, final_decls) = merge_decls True [] (pi_decls iface) (pi_decls new_iface)

	-- Fill in the version number on the new declarations
	-- by looking at the old declarations.
	-- Set the flag if anything changes. 
	-- Assumes that the decls are sorted by hsDeclName
    merge_decls ok_so_far acc []  []        = (ok_so_far, reverse acc)
    merge_decls ok_so_far acc old []        = (False, reverse acc)
    merge_decls ok_so_far acc [] (nvd:nvds) = merge_decls False (nvd:acc) [] nvds
    merge_decls ok_so_far acc (vd@(v,d):vds) (nvd@(_,nd):nvds)
	= case d_name `compare` nd_name of
		LT -> merge_decls False acc       vds      (nvd:nvds)
		GT -> merge_decls False (nvd:acc) (vd:vds) nvds
		EQ | d == nd   -> merge_decls ok_so_far (vd:acc) vds nvds
		   | otherwise -> merge_decls False	((bumpVersion v, nd):acc) vds nvds
	where
 	  d_name  = hsDeclName d
 	  nd_name = hsDeclName nd
\end{code}



%************************************************************************
%*				 					*
\subsection{Printing the interface}
%*				 					*
%************************************************************************

\begin{code}
pprIface (ParsedIface { pi_mod = mod, pi_vers = mod_vers, pi_orphan = orphan,
			pi_usages = usages, pi_exports = exports, 
			pi_fixity = (fix_vers, fixities),
			pi_insts = insts, pi_decls = decls, 
			pi_rules = (rule_vers, rules), pi_deprecs = deprecs })
 = vcat [ ptext SLIT("__interface")
		<+> doubleQuotes (ptext opt_InPackage)
		<+> ppr mod <+> ppr mod_vers <+> pp_sub_vers
		<+> (if orphan then char '!' else empty)
		<+> int opt_HiVersion
		<+> ptext SLIT("where")
	, vcat (map pprExport exports)
	, vcat (map pprUsage usages)
	, pprFixities fixities
	, vcat [ppr i <+> semi | i <- insts]
	, vcat [ppr_vers v <+> ppr d <> semi | (v,d) <- decls]
	, pprRules rules
	, pprDeprecs deprecs
	]
  where
    ppr_vers v | v == initialVersion = empty
	       | otherwise	     = int v
    pp_sub_vers 
	| fix_vers == initialVersion && rule_vers == initialVersion = empty
	| otherwise = brackets (ppr fix_vers <+> ppr rule_vers)
\end{code}

When printing export lists, we print like this:
	Avail   f		f
	AvailTC C [C, x, y]	C(x,y)
	AvailTC C [x, y]	C!(x,y)		-- Exporting x, y but not C

\begin{code}
pprExport :: ExportItem -> SDoc
pprExport (mod, items)
 = hsep [ ptext SLIT("__export "), ppr mod, hsep (map upp_avail items) ] <> semi
  where
    upp_avail :: RdrAvailInfo -> SDoc
    upp_avail (Avail name)      = pprOccName name
    upp_avail (AvailTC name []) = empty
    upp_avail (AvailTC name ns) = hcat [pprOccName name, bang, upp_export ns']
				where
				  bang | name `elem` ns = empty
				       | otherwise	= char '|'
				  ns' = filter (/= name) ns
    
    upp_export []    = empty
    upp_export names = braces (hsep (map pprOccName names))
\end{code}


\begin{code}
pprUsage :: ImportVersion OccName -> SDoc
pprUsage (m, has_orphans, is_boot, whats_imported)
  = hsep [ptext SLIT("import"), pprModuleName m, 
	  pp_orphan, pp_boot,
	  upp_import_versions whats_imported
    ] <> semi
  where
    pp_orphan | has_orphans = char '!'
	      | otherwise   = empty
    pp_boot   | is_boot     = char '@'
              | otherwise   = empty

	-- Importing the whole module is indicated by an empty list
    upp_import_versions NothingAtAll   = empty
    upp_import_versions (Everything v) = dcolon <+> int v
    upp_import_versions (Specifically vm vf vr nvs)
      = dcolon <+> int vm <+> int vf <+> int vr <+> hsep [ ppr n <+> int v | (n,v) <- nvs ]
\end{code}


\begin{code}
pprFixities []    = empty
pprFixities fixes = hsep (map ppr fixes) <> semi

pprRules []    = empty
pprRules rules = hsep [ptext SLIT("{-## __R"), hsep (map ppr rules), ptext SLIT("##-}")]

pprDeprecs []   = empty
pprDeprecs deps = hsep [ ptext SLIT("{-## __D"), guts, ptext SLIT("##-}")]
		where
		  guts = hsep [ ppr ie <+> doubleQuotes (ppr txt) <> semi 
			      | Deprecation ie txt _ <- deps ]
\end{code}


%************************************************************************
%*				 					*
\subsection{Completing the new interface}
%*				 					*
%************************************************************************

\begin{code}
completeIface new_iface local_tycons local_classes
		        inst_info final_ids tidy_binds
			tidy_orphan_rules
  = new_iface { pi_decls = [(initialVersion,d) | d <- sortLt lt_decl all_decls],
		pi_insts = sortLt lt_inst_decl inst_dcls,
		pi_rules = (initialVersion, rule_dcls)
    }
  where
     all_decls = cls_dcls ++ ty_dcls ++ bagToList val_dcls
     (inst_dcls, inst_ids) = ifaceInstances inst_info
     cls_dcls = map ifaceClass local_classes
     ty_dcls  = map ifaceTyCon (filter (not . isWiredInName . getName) local_tycons)

     (val_dcls, emitted_ids) = ifaceBinds (inst_ids `unionVarSet` orphan_rule_ids)
					  final_ids tidy_binds

     rule_dcls | opt_OmitInterfacePragmas = []
	       | otherwise		  = ifaceRules tidy_orphan_rules emitted_ids

     orphan_rule_ids = unionVarSets [ ruleSomeFreeVars interestingId rule 
				    | ProtoCoreRule _ _ rule <- tidy_orphan_rules]

lt_inst_decl (InstDecl _ _ _ dfun_id1 _) (InstDecl _ _ _ dfun_id2 _)
   = dfun_id1 < dfun_id2
	-- The dfuns are assigned names df1, df2, etc, 
	-- in order of original textual
	-- occurrence, and this makes as good a sort order as any

lt_decl d1 d2 = hsDeclName d1 < hsDeclName d2
\end{code}


%************************************************************************
%*				 					*
\subsection{Completion stuff}
%*				 					*
%************************************************************************

\begin{code}
ifaceRules :: [ProtoCoreRule] -> IdSet -> [RdrNameRuleDecl]
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
		     all (`elemVarSet` emitted) (varSetElems (ruleSomeLhsFreeVars interestingId rule))
				-- Spit out a rule only if all its lhs free vars are emitted
				-- This is a good reason not to do it when we emit the Id itself
		   ]
\end{code}

\begin{code}			 
ifaceInstances :: Bag InstInfo -> ([RdrNameInstDecl], IdSet)
		   -- The IdSet is the needed dfuns

ifaceInstances inst_infos
  = (decls, needed_ids)
  where			
    decls       = map to_decl togo_insts
    togo_insts	= filter is_togo_inst (bagToList inst_infos)
    needed_ids  = mkVarSet [dfun_id | InstInfo _ _ _ _ dfun_id _ _ _ <- togo_insts]
    is_togo_inst (InstInfo _ _ _ _ dfun_id _ _ _) = isLocallyDefined dfun_id
				 
    -------			 
    to_decl (InstInfo clas tvs tys theta dfun_id _ _ _)
      = let			 
		-- The deNoteType is very important.   It removes all type
		-- synonyms from the instance type in interface files.
		-- That in turn makes sure that when reading in instance decls
		-- from interface files that the 'gating' mechanism works properly.
		-- Otherwise you could have
		--	type Tibble = T Int
		--	instance Foo Tibble where ...
		-- and this instance decl wouldn't get imported into a module
		-- that mentioned T but not Tibble.
	    forall_ty     = mkSigmaTy tvs (classesToPreds theta)
				      (deNoteType (mkDictTy clas tys))
	    tidy_ty = tidyTopType forall_ty
	in			 
	InstDecl (toHsType tidy_ty) EmptyMonoBinds [] (toRdrName dfun_id) noSrcLoc 
\end{code}

\begin{code}
ifaceTyCon :: TyCon -> RdrNameHsDecl
ifaceTyCon tycon
  | isSynTyCon tycon
  = TyClD (TySynonym (toRdrName tycon)
		     (toHsTyVars tyvars) (toHsType ty)
		     noSrcLoc)
  where
    (tyvars, ty) = getSynTyConDefn tycon

ifaceTyCon tycon
  | isAlgTyCon tycon
  = TyClD (TyData new_or_data (toHsContext (tyConTheta tycon))
		  (toRdrName tycon)
		  (toHsTyVars tyvars)
		  (map ifaceConDecl (tyConDataCons tycon))
		  (tyConFamilySize tycon)
		  Nothing NoDataPragmas noSrcLoc)
  where
    tyvars = tyConTyVars tycon
    new_or_data | isNewTyCon tycon = NewType
	        | otherwise	   = DataType

    ifaceConDecl data_con 
	= ConDecl (toRdrName data_con) (error "ifaceConDecl")
		  (toHsTyVars ex_tyvars)
		  (toHsContext ex_theta)
		  details noSrcLoc
	where
	  (tyvars1, _, ex_tyvars, ex_theta, arg_tys, tycon1) = dataConSig data_con
          field_labels   = dataConFieldLabels data_con
          strict_marks   = dataConStrictMarks data_con
	  details
	    | null field_labels
	    = ASSERT( tycon == tycon1 && tyvars == tyvars1 )
	      VanillaCon (zipWith mk_bang_ty strict_marks arg_tys)

    	    | otherwise
	    = RecCon (zipWith mk_field strict_marks field_labels)

    mk_bang_ty NotMarkedStrict     ty = Unbanged (toHsType ty)
    mk_bang_ty (MarkedUnboxed _ _) ty = Unpacked (toHsType ty)
    mk_bang_ty MarkedStrict        ty = Banged   (toHsType ty)

    mk_field strict_mark field_label
	= ([toRdrName field_label], mk_bang_ty strict_mark (fieldLabelType field_label))

ifaceTyCon tycon
  = pprPanic "pprIfaceTyDecl" (ppr tycon)

ifaceClass clas
  = TyClD (ClassDecl (toHsContext sc_theta)
		     (toRdrName clas)
		     (toHsTyVars clas_tyvars)
		     (toHsFDs clas_fds)
		     (map toClassOpSig op_stuff)
		     EmptyMonoBinds NoClassPragmas
		     bogus bogus bogus [] noSrcLoc
    )
  where
     bogus = error "ifaceClass"
     (clas_tyvars, clas_fds, sc_theta, _, op_stuff) = classExtraBigSig clas

     toClassOpSig (sel_id, dm_id, explicit_dm)
	= ASSERT( sel_tyvars == clas_tyvars)
	  ClassOpSig (toRdrName sel_id) bogus explicit_dm (toHsType op_ty) noSrcLoc
	where
	  (sel_tyvars, _, op_ty) = splitSigmaTy (idType sel_id)
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
	   -> (Bag RdrNameHsDecl, IdSet)		-- Set of Ids actually spat out

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

    go needed [] decls emitted
	| not (isEmptyVarSet needed) = pprTrace "ifaceBinds: free vars:" 
					  (sep (map ppr (varSetElems needed)))
				       (decls, emitted)
	| otherwise 		     = (decls, emitted)

    go needed (NonRec id rhs : binds) decls emitted
	= case ifaceId get_idinfo needed False id rhs of
		Nothing		      -> go needed binds decls emitted
		Just (decl, extras) -> let
			needed' = (needed `unionVarSet` extras) `delVarSet` id
			-- 'extras' can include the Id itself via a rule
			emitted' = emitted `extendVarSet` id
			in
			go needed' binds (decl `consBag` decls) emitted'

	-- Recursive groups are a bit more of a pain.  We may only need one to
	-- start with, but it may call out the next one, and so on.  So we
	-- have to look for a fixed point.
    go needed (Rec pairs : binds) decls emitted
	= go needed' binds decls' emitted' 
	where
	  (new_decls, new_emitted, extras) = go_rec needed pairs
	  decls'   = new_decls `unionBags` decls
	  needed'  = (needed `unionVarSet` extras) `minusVarSet` mkVarSet (map fst pairs) 
	  emitted' = emitted `unionVarSet` new_emitted

    go_rec :: IdSet -> [(Id,CoreExpr)] -> (Bag RdrNameHsDecl, IdSet, IdSet)
    go_rec needed pairs
	| null decls = (emptyBag, emptyVarSet, emptyVarSet)
	| otherwise	= (more_decls `unionBags`   listToBag decls, 
			   more_emitted  `unionVarSet` mkVarSet emitted,
			   more_extras   `unionVarSet` extras)
	where
	  maybes	     = map do_one pairs
	  emitted	     = [id   | ((id,_), Just _)  <- pairs `zip` maybes]
	  reduced_pairs	     = [pair | (pair,   Nothing) <- pairs `zip` maybes]
	  (decls, extras_s)  = unzip (catMaybes maybes)
	  extras	     = unionVarSets extras_s
	  (more_decls, more_emitted, more_extras) = go_rec extras reduced_pairs

	  do_one (id,rhs) = ifaceId get_idinfo needed True id rhs
\end{code}


\begin{code}
ifaceId :: (Id -> IdInfo)	-- This function "knows" the extra info added
				-- by the STG passes.  Sigh

	-> IdSet		-- Set of Ids that are needed by earlier interface
				-- file emissions.  If the Id isn't in this set, and isn't
				-- exported, there's no need to emit anything
	-> Bool			-- True <=> recursive, so don't print unfolding
	-> Id
	-> CoreExpr		-- The Id's right hand side
	-> Maybe (RdrNameHsDecl, IdSet)	-- The emitted stuff, plus any *extra* needed Ids

ifaceId get_idinfo needed_ids is_rec id rhs
  | not (id `elemVarSet` needed_ids ||		-- Needed [no id in needed_ids has omitIfaceSigForId]
	(isUserExportedId id && not (omitIfaceSigForId id)))	-- or exported and not to be omitted
  = Nothing 		-- Well, that was easy!

ifaceId get_idinfo needed_ids is_rec id rhs
  = ASSERT2( arity_matches_strictness, ppr id )
    Just (SigD (IfaceSig (toRdrName id) (toHsType id_type) hs_idinfo noSrcLoc),
	  new_needed_ids)
  where
    id_type     = idType id
    core_idinfo = idInfo id
    stg_idinfo  = get_idinfo id

    hs_idinfo | opt_OmitInterfacePragmas = []
 	      | otherwise		 = arity_hsinfo  ++ caf_hsinfo  ++ cpr_hsinfo ++ 
					   strict_hsinfo ++ wrkr_hsinfo ++ unfold_hsinfo

    ------------  Arity  --------------
    arity_info     = arityInfo stg_idinfo
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
    work_info     = workerInfo core_idinfo
    has_worker    = workerExists work_info
    wrkr_hsinfo   = case work_info of
			HasWorker work_id _ -> [HsWorker (toRdrName work_id)]
			other		    -> []

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

    worker_ids = case work_info of
		   HasWorker work_id _ | interestingId work_id -> unitVarSet work_id
			-- Conceivably, the worker might come from
			-- another module
		   other -> emptyVarSet

    spec_ids = filterVarSet interestingId (rulesRhsFreeVars spec_info)

    unfold_ids | show_unfold = find_fvs rhs
	       | otherwise   = emptyVarSet

    find_fvs expr = exprSomeFreeVars interestingId expr

    ------------ Sanity checking --------------
	-- The arity of a wrapper function should match its strictness,
	-- or else an importing module will get very confused indeed.
    arity_matches_strictness 
       = case work_info of
	     HasWorker _ wrap_arity -> wrap_arity == arityLowerBound arity_info
	     other		    -> True
    
interestingId id = isId id && isLocallyDefined id &&
		   not (omitIfaceSigForId id)
\end{code}

