%
% (c) The GRASP/AQUA Project, Glasgow University, 1993-1998
%
\section[MkIface]{Print an interface for a module}

\begin{code}
module MkIface (
	startIface, endIface, ifaceDecls
    ) where

#include "HsVersions.h"

import IO		( Handle, hPutStr, openFile, 
			  hClose, hPutStrLn, IOMode(..) )

import HsSyn
import BasicTypes	( Fixity(..), FixityDirection(..), NewOrData(..) )
import RnMonad
import RnEnv		( availName )

import TcInstUtil	( InstInfo(..) )

import CmdLineOpts
import Id		( Id, idType, idInfo, omitIfaceSigForId, isUserExportedId,
			  getIdSpecialisation
			)
import Var		( isId )
import VarSet
import DataCon		( StrictnessMark(..), dataConSig, dataConFieldLabels, dataConStrictMarks )
import IdInfo		( IdInfo, StrictnessInfo(..), ArityInfo, InlinePragInfo(..), inlinePragInfo,
			  arityInfo, ppArityInfo, arityLowerBound,
			  strictnessInfo, ppStrictnessInfo, isBottomingStrictness,
			  cafInfo, ppCafInfo, specInfo,
			  cprInfo, ppCprInfo, pprInlinePragInfo,
			  occInfo, OccInfo(..),
			  workerExists, workerInfo, ppWorkerInfo
			)
import CoreSyn		( CoreExpr, CoreBind, Bind(..), rulesRules, rulesRhsFreeVars )
import CoreFVs		( exprSomeFreeVars, ruleSomeLhsFreeVars, ruleSomeFreeVars )
import CoreUnfold	( calcUnfoldingGuidance, okToUnfoldInHiFile, couldBeSmallEnoughToInline )
import Module		( moduleString, pprModule, pprModuleName )
import Name		( isLocallyDefined, isWiredInName, nameRdrName, nameModule,
			  Name, NamedThing(..)
			)
import OccName		( OccName, pprOccName )
import TyCon		( TyCon, getSynTyConDefn, isSynTyCon, isNewTyCon, isAlgTyCon,
			  tyConTheta, tyConTyVars, tyConDataCons
			)
import Class		( Class, classExtraBigSig )
import FieldLabel	( fieldLabelName, fieldLabelType )
import Type		( mkSigmaTy, splitSigmaTy, mkDictTy, tidyTopType,
			  deNoteType, classesToPreds,
			  Type, ThetaType, PredType(..), ClassContext
		        )

import PprType
import PprCore		( pprIfaceUnfolding, pprCoreRule )
import FunDeps		( pprFundeps )
import Rules		( pprProtoCoreRule, ProtoCoreRule(..) )

import Bag		( bagToList, isEmptyBag )
import Maybes		( catMaybes, maybeToBool )
import FiniteMap	( emptyFM, addToFM, addToFM_C, fmToList, FiniteMap )
import UniqFM		( lookupUFM, listToUFM )
import UniqSet		( uniqSetToList )
import Util		( sortLt, mapAccumL )
import Bag
import Outputable
\end{code}

We have a function @startIface@ to open the output file and put
(something like) ``interface Foo'' in it.  It gives back a handle
for subsequent additions to the interface file.

We then have one-function-per-block-of-interface-stuff, e.g.,
@ifaceExportList@ produces the @__exports__@ section; it appends
to the handle provided by @startIface@.

\begin{code}
startIface  :: Module -> InterfaceDetails
	    -> IO (Maybe Handle) -- Nothing <=> don't do an interface

ifaceDecls :: Maybe Handle
	   -> [TyCon] -> [Class]
	   -> Bag InstInfo 
	   -> [Id]		-- Ids used at code-gen time; they have better pragma info!
	   -> [CoreBind]	-- In dependency order, later depend on earlier
	   -> [ProtoCoreRule]	-- Rules
	   -> IO ()

endIface    :: Maybe Handle -> IO ()
\end{code}

\begin{code}
startIface mod (has_orphans, import_usages, ExportEnv avails fixities _)
  = case opt_ProduceHi of
      Nothing -> return Nothing ; -- not producing any .hi file

      Just fn -> do 
	if_hdl <- openFile fn WriteMode
	hPutStr		if_hdl ("__interface " ++ moduleString mod)
	hPutStr		if_hdl (' ' : show (opt_HiVersion :: Int) ++ orphan_indicator)
	hPutStrLn	if_hdl " where"
	ifaceExports	if_hdl avails
	ifaceImports	if_hdl import_usages
	ifaceFixities	if_hdl fixities
	return (Just if_hdl)
  where
    orphan_indicator | has_orphans = " !"
		     | otherwise   = ""

endIface Nothing	= return ()
endIface (Just if_hdl)	= hPutStr if_hdl "\n" >> hClose if_hdl
\end{code}


\begin{code}
ifaceDecls Nothing tycons classes inst_info final_ids simplified rules = return ()
ifaceDecls (Just hdl)
	   tycons classes
	   inst_infos
	   final_ids binds
	   orphan_rules		-- Rules defined locally for an Id that is *not* defined locally
  | null_decls = return ()		 
	--  You could have a module with just (re-)exports/instances in it
  | otherwise
  = ifaceClasses hdl classes			>>
    ifaceInstances hdl inst_infos		>>= \ inst_ids ->
    ifaceTyCons hdl tycons			>>
    ifaceBinds hdl (inst_ids `unionVarSet` orphan_rule_ids)
	       final_ids binds			>>= \ emitted_ids ->
    ifaceRules hdl orphan_rules emitted_ids	>>
    return ()
  where
     orphan_rule_ids = unionVarSets [ ruleSomeFreeVars interestingId rule 
				    | ProtoCoreRule _ _ rule <- orphan_rules]

     null_decls = null binds      && 
		  null tycons     &&
	          null classes    && 
	          isEmptyBag inst_infos &&
		  null orphan_rules
\end{code}

\begin{code}
ifaceImports if_hdl import_usages
  = hPutCol if_hdl upp_uses (sortLt lt_imp_vers import_usages)
  where
    upp_uses (m, mv, has_orphans, whats_imported)
      = hsep [ptext SLIT("import"), pprModuleName m, 
	      int mv, pp_orphan,
	      upp_import_versions whats_imported
	] <> semi
      where
	pp_orphan | has_orphans = ptext SLIT("!")
		  | otherwise   = empty

	-- Importing the whole module is indicated by an empty list
    upp_import_versions Everything = empty

	-- For imported versions we do print the version number
    upp_import_versions (Specifically nvs)
      = dcolon <+> hsep [ hsep [ppr_unqual_name n, int v] | (n,v) <- sort_versions nvs ]

ifaceModuleDeps if_hdl [] = return ()
ifaceModuleDeps if_hdl mod_deps
  = let 
	lines = map ppr_mod_dep mod_deps
	ppr_mod_dep (mod, contains_orphans) 
	   | contains_orphans = pprModuleName mod <+> ptext SLIT("!")
	   | otherwise	      = pprModuleName mod
    in 
    printForIface if_hdl (ptext SLIT("__depends") <+> vcat lines <> ptext SLIT(" ;")) >>
    hPutStr if_hdl "\n"

ifaceExports if_hdl [] = return ()
ifaceExports if_hdl avails
  = hPutCol if_hdl do_one_module (fmToList export_fm)
  where
	-- Sort them into groups by module
    export_fm :: FiniteMap Module [AvailInfo]
    export_fm = foldr insert emptyFM avails

    insert avail efm = addToFM_C (++) efm mod [avail] 
		     where
		       mod = nameModule (availName avail)

	-- Print one module's worth of stuff
    do_one_module :: (Module, [AvailInfo]) -> SDoc
    do_one_module (mod_name, avails@(avail1:_))
	= ptext SLIT("__export ") <>
	  hsep [pprModule mod_name,
		hsep (map upp_avail (sortLt lt_avail avails))
	  ] <> semi

ifaceFixities if_hdl [] = return ()
ifaceFixities if_hdl fixities 
  = hPutCol if_hdl upp_fixity fixities

ifaceRules if_hdl rules emitted
  | null orphan_rule_pretties && null local_id_pretties
  = return ()
  | otherwise
  = do	printForIface if_hdl (vcat [
		ptext SLIT("{-## __R"),

		vcat orphan_rule_pretties,

		vcat local_id_pretties,

		ptext SLIT("##-}")
          ])
	
	return ()
  where
    orphan_rule_pretties =  [ pprCoreRule (Just fn) rule
			    | ProtoCoreRule _ fn rule <- rules
			    ]
    local_id_pretties = [ pprCoreRule (Just fn) rule
 		        | fn <- varSetElems emitted, 
			  rule <- rulesRules (getIdSpecialisation fn),
			  all (`elemVarSet` emitted) (varSetElems (ruleSomeLhsFreeVars interestingId rule))
				-- Spit out a rule only if all its lhs free vars are eemitted
		        ]
\end{code}

%************************************************************************
%*				 					*
\subsection{Instance declarations}
%*				 					*
%************************************************************************


\begin{code}			 
ifaceInstances :: Handle -> Bag InstInfo -> IO IdSet		-- The IdSet is the needed dfuns
ifaceInstances if_hdl inst_infos
  | null togo_insts = return emptyVarSet		 
  | otherwise 	    = hPutCol if_hdl pp_inst (sortLt lt_inst togo_insts) >>
		      return needed_ids
  where				 
    togo_insts	= filter is_togo_inst (bagToList inst_infos)
    needed_ids  = mkVarSet [dfun_id | InstInfo _ _ _ _ dfun_id _ _ _ <- togo_insts]
    is_togo_inst (InstInfo _ _ _ _ dfun_id _ _ _) = isLocallyDefined dfun_id
				 
    -------			 
    lt_inst (InstInfo _ _ _ _ dfun_id1 _ _ _)
	    (InstInfo _ _ _ _ dfun_id2 _ _ _)
      = getOccName dfun_id1 < getOccName dfun_id2
	-- The dfuns are assigned names df1, df2, etc, in order of original textual
	-- occurrence, and this makes as good a sort order as any

    -------			 
    pp_inst (InstInfo clas tvs tys theta dfun_id _ _ _)
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
	    renumbered_ty = tidyTopType forall_ty
	in			 
	hcat [ptext SLIT("instance "), pprType renumbered_ty, 
		    ptext SLIT(" = "), ppr_unqual_name dfun_id, semi]
\end{code}


%************************************************************************
%*				 					*
\subsection{Printing values}
%*				 					*
%************************************************************************

\begin{code}
ifaceId :: (Id -> IdInfo)		-- This function "knows" the extra info added
					-- by the STG passes.  Sigh

	    -> IdSet			-- Set of Ids that are needed by earlier interface
					-- file emissions.  If the Id isn't in this set, and isn't
					-- exported, there's no need to emit anything
	    -> Bool			-- True <=> recursive, so don't print unfolding
	    -> Id
	    -> CoreExpr			-- The Id's right hand side
	    -> Maybe (SDoc, IdSet)	-- The emitted stuff, plus any *extra* needed Ids

ifaceId get_idinfo needed_ids is_rec id rhs
  | not (id `elemVarSet` needed_ids ||		-- Needed [no id in needed_ids has omitIfaceSigForId]
	 (isUserExportedId id && not (omitIfaceSigForId id)))	-- or exported and not to be omitted
  = Nothing 		-- Well, that was easy!

ifaceId get_idinfo needed_ids is_rec id rhs
  = ASSERT2( arity_matches_strictness, ppr id )
    Just (hsep [sig_pretty, prag_pretty, char ';'], new_needed_ids)
  where
    core_idinfo = idInfo id
    stg_idinfo  = get_idinfo id

    ty_pretty  = pprType (idType id)
    sig_pretty = hsep [ppr (getOccName id), dcolon, ty_pretty]

    prag_pretty 
     | opt_OmitInterfacePragmas = empty
     | otherwise		= hsep [ptext SLIT("{-##"),
					arity_pretty, 
					caf_pretty,
					cpr_pretty,
					strict_pretty,
					wrkr_pretty,
					unfold_pretty, 
					ptext SLIT("##-}")]

    ------------  Arity  --------------
    arity_info    = arityInfo stg_idinfo
    arity_pretty  = ppArityInfo arity_info

    ------------ Caf Info --------------
    caf_pretty = ppCafInfo (cafInfo stg_idinfo)

    ------------ CPR Info --------------
    cpr_pretty = ppCprInfo (cprInfo core_idinfo)

    ------------  Strictness  --------------
    strict_info   = strictnessInfo core_idinfo
    bottoming_fn  = isBottomingStrictness strict_info
    strict_pretty = ppStrictnessInfo strict_info

    ------------  Worker  --------------
    work_info     = workerInfo core_idinfo
    has_worker    = workerExists work_info
    wrkr_pretty   = ppWorkerInfo work_info
    Just work_id  = work_info


    ------------  Occ info  --------------
    loop_breaker  = case occInfo core_idinfo of
			IAmALoopBreaker -> True
			other		-> False

    ------------  Unfolding  --------------
    inline_pragma  = inlinePragInfo core_idinfo
    dont_inline	   = case inline_pragma of
			IMustNotBeINLINEd False Nothing -> True	-- Unconditional NOINLINE
			other		  	        -> False


    unfold_pretty | show_unfold = ptext SLIT("__U") <> pprInlinePragInfo inline_pragma <+> pprIfaceUnfolding rhs
		  | otherwise   = empty

    show_unfold = not has_worker	 &&	-- Not unnecessary
		  not bottoming_fn	 &&	-- Not necessary
		  not dont_inline	 &&
		  not loop_breaker	 &&
		  rhs_is_small		 &&	-- Small enough
		  okToUnfoldInHiFile rhs 	-- No casms etc

    rhs_is_small = couldBeSmallEnoughToInline (calcUnfoldingGuidance opt_UF_HiFileThreshold rhs)

    ------------  Specialisations --------------
    spec_info   = specInfo core_idinfo
    
    ------------  Extra free Ids  --------------
    new_needed_ids | opt_OmitInterfacePragmas = emptyVarSet
	           | otherwise		      = worker_ids	`unionVarSet`
						unfold_ids	`unionVarSet`
						spec_ids

    worker_ids | has_worker && interestingId work_id = unitVarSet work_id
			-- Conceivably, the worker might come from
			-- another module
	       | otherwise			   = emptyVarSet

    spec_ids = filterVarSet interestingId (rulesRhsFreeVars spec_info)

    unfold_ids | show_unfold = find_fvs rhs
	       | otherwise   = emptyVarSet

    find_fvs expr = exprSomeFreeVars interestingId expr

    ------------ Sanity checking --------------
	-- The arity of a wrapper function should match its strictness,
	-- or else an importing module will get very confused indeed.
	-- [later: actually all that is necessary is for strictness to exceed arity]
    arity_matches_strictness
	= not has_worker ||
	  case strict_info of
	    StrictnessInfo ds _ -> length ds >= arityLowerBound arity_info
	    other		-> True
    
interestingId id = isId id && isLocallyDefined id &&
		   not (omitIfaceSigForId id)
\end{code}

\begin{code}
ifaceBinds :: Handle
	   -> IdSet		-- These Ids are needed already
	   -> [Id]		-- Ids used at code-gen time; they have better pragma info!
	   -> [CoreBind]	-- In dependency order, later depend on earlier
	   -> IO IdSet		-- Set of Ids actually spat out

ifaceBinds hdl needed_ids final_ids binds
  = mapIO (printForIface hdl) (bagToList pretties)	>>
    hPutStr hdl "\n"					>>
    return emitted
  where
    final_id_map  = listToUFM [(id,id) | id <- final_ids]
    get_idinfo id = case lookupUFM final_id_map id of
			Just id' -> idInfo id'
			Nothing  -> pprTrace "ifaceBinds not found:" (ppr id) $
				    idInfo id

    (pretties, emitted) = go needed_ids (reverse binds) emptyBag emptyVarSet 
			-- Reverse so that later things will 
			-- provoke earlier ones to be emitted
    go needed [] pretties emitted
	| not (isEmptyVarSet needed) = pprTrace "ifaceBinds: free vars:" 
					  (sep (map ppr (varSetElems needed)))
				       (pretties, emitted)
	| otherwise 		     = (pretties, emitted)

    go needed (NonRec id rhs : binds) pretties emitted
	= case ifaceId get_idinfo needed False id rhs of
		Nothing		      -> go needed binds pretties emitted
		Just (pretty, extras) -> let
			needed' = (needed `unionVarSet` extras) `delVarSet` id
			-- 'extras' can include the Id itself via a rule
			emitted' = emitted `extendVarSet` id
			in
			go needed' binds (pretty `consBag` pretties) emitted'

	-- Recursive groups are a bit more of a pain.  We may only need one to
	-- start with, but it may call out the next one, and so on.  So we
	-- have to look for a fixed point.
    go needed (Rec pairs : binds) pretties emitted
	= go needed' binds pretties' emitted' 
	where
	  (new_pretties, new_emitted, extras) = go_rec needed pairs
	  pretties' = new_pretties `unionBags` pretties
	  needed'   = (needed `unionVarSet` extras) `minusVarSet` mkVarSet (map fst pairs) 
	  emitted'  = emitted `unionVarSet` new_emitted

    go_rec :: IdSet -> [(Id,CoreExpr)] -> (Bag SDoc, IdSet, IdSet)
    go_rec needed pairs
	| null pretties = (emptyBag, emptyVarSet, emptyVarSet)
	| otherwise	= (more_pretties `unionBags`   listToBag pretties, 
			   more_emitted  `unionVarSet` mkVarSet emitted,
			   more_extras   `unionVarSet` extras)
	where
	  maybes	       = map do_one pairs
	  emitted	       = [id   | ((id,_), Just _)  <- pairs `zip` maybes]
	  reduced_pairs	       = [pair | (pair,   Nothing) <- pairs `zip` maybes]
	  (pretties, extras_s) = unzip (catMaybes maybes)
	  extras	       = unionVarSets extras_s
	  (more_pretties, more_emitted, more_extras) = go_rec extras reduced_pairs

	  do_one (id,rhs) = ifaceId get_idinfo needed True id rhs
\end{code}


%************************************************************************
%*				 					*
\subsection{Random small things}
%*				 					*
%************************************************************************

\begin{code}
ifaceTyCons hdl tycons   = hPutCol hdl upp_tycon (sortLt (<) (filter (for_iface_name . getName) tycons))
ifaceClasses hdl classes = hPutCol hdl upp_class (sortLt (<) (filter (for_iface_name . getName) classes))

for_iface_name name = isLocallyDefined name && 
		      not (isWiredInName name)

upp_tycon tycon = ifaceTyCon tycon
upp_class clas  = ifaceClass clas
\end{code}


\begin{code}
ifaceTyCon :: TyCon -> SDoc
ifaceTyCon tycon
  | isSynTyCon tycon
  = hsep [ ptext SLIT("type"),
	   ppr (getName tycon),
	   pprTyVarBndrs tyvars,
	   ptext SLIT("="),
	   ppr ty,
	   semi
    ]
  where
    (tyvars, ty) = getSynTyConDefn tycon

ifaceTyCon tycon
  | isAlgTyCon tycon
  = hsep [ ptext keyword,
	   ppr_decl_class_context (tyConTheta tycon),
	   ppr (getName tycon),
	   pprTyVarBndrs (tyConTyVars tycon),
	   ptext SLIT("="),
	   hsep (punctuate (ptext SLIT(" | ")) (map ppr_con (tyConDataCons tycon))),
	   semi
    ]
  where
    keyword | isNewTyCon tycon = SLIT("newtype")
	    | otherwise	       = SLIT("data")

    tyvars = tyConTyVars tycon

    ppr_con data_con 
	| null field_labels
	= ASSERT( tycon == tycon1 && tyvars == tyvars1 )
	  hsep [  ppr_ex ex_tyvars ex_theta,
		  ppr name,
		  hsep (map ppr_arg_ty (strict_marks `zip` arg_tys))
	        ]

	| otherwise
	= hsep [  ppr_ex ex_tyvars ex_theta,
		  ppr name,
		  braces $ hsep $ punctuate comma (map ppr_field (strict_marks `zip` field_labels))
	 	]
          where
	   (tyvars1, theta1, ex_tyvars, ex_theta, arg_tys, tycon1) = dataConSig data_con
           field_labels   = dataConFieldLabels data_con
           strict_marks   = dataConStrictMarks data_con
	   name           = getName            data_con

    ppr_ex [] ex_theta = ASSERT( null ex_theta ) empty
    ppr_ex ex_tvs ex_theta = ptext SLIT("__forall") <+> brackets (pprTyVarBndrs ex_tvs)
			     <+> pprIfaceClasses ex_theta <+> ptext SLIT("=>")

    ppr_arg_ty (strict_mark, ty) = ppr_strict_mark strict_mark <> pprParendType ty

    ppr_strict_mark NotMarkedStrict        = empty
    ppr_strict_mark (MarkedUnboxed _ _)    = ptext SLIT("! ! ")
    ppr_strict_mark MarkedStrict           = ptext SLIT("! ")

    ppr_field (strict_mark, field_label)
	= hsep [ ppr (fieldLabelName field_label),
		  dcolon,
		  ppr_strict_mark strict_mark <> pprParendType (fieldLabelType field_label)
		]

ifaceTyCon tycon
  = pprPanic "pprIfaceTyDecl" (ppr tycon)

ifaceClass clas
  = hsep [ptext SLIT("class"),
	   ppr_decl_class_context sc_theta,
	   ppr clas,			-- Print the name
	   pprTyVarBndrs clas_tyvars,
	   pprFundeps clas_fds,
	   pp_ops,
	   semi
	  ]
   where
     (clas_tyvars, clas_fds, sc_theta, _, op_stuff) = classExtraBigSig clas

     pp_ops | null op_stuff  = empty
	    | otherwise      = hsep [ptext SLIT("where"),
				     braces (hsep (punctuate semi (map ppr_classop op_stuff)))
			       ]

     ppr_classop (sel_id, dm_id, explicit_dm)
	= ASSERT( sel_tyvars == clas_tyvars)
	  hsep [ppr (getOccName sel_id),
		if explicit_dm then equals else empty,
	        dcolon,
		ppr op_ty
	  ]
	where
	  (sel_tyvars, _, op_ty) = splitSigmaTy (idType sel_id)

ppr_decl_context :: ThetaType -> SDoc
ppr_decl_context []    = empty
ppr_decl_context theta = pprIfaceTheta theta <+> ptext SLIT(" =>")

ppr_decl_class_context :: ClassContext -> SDoc
ppr_decl_class_context []    = empty
ppr_decl_class_context ctxt  = pprIfaceClasses ctxt <+> ptext SLIT(" =>")

pprIfaceTheta :: ThetaType -> SDoc	-- Use braces rather than parens in interface files
pprIfaceTheta []    = empty
pprIfaceTheta theta = braces (hsep (punctuate comma [pprIfacePred p | p <- theta]))

-- ZZ - not sure who uses this - i.e. whether IParams really show up or not
-- (it's not used to print normal value signatures)
pprIfacePred :: PredType -> SDoc
pprIfacePred (Class clas tys) = pprConstraint clas tys
pprIfacePred (IParam n ty)    = char '?' <> ppr n <+> ptext SLIT("::") <+> ppr ty

pprIfaceClasses :: ClassContext -> SDoc
pprIfaceClasses []    = empty
pprIfaceClasses theta = braces (hsep (punctuate comma [pprConstraint c tys | (c,tys) <- theta]))
\end{code}

%************************************************************************
%*				 					*
\subsection{Random small things}
%*				 					*
%************************************************************************

When printing export lists, we print like this:
	Avail   f		f
	AvailTC C [C, x, y]	C(x,y)
	AvailTC C [x, y]	C!(x,y)		-- Exporting x, y but not C

\begin{code}
upp_avail :: AvailInfo -> SDoc
upp_avail (Avail name)      = pprOccName (getOccName name)
upp_avail (AvailTC name []) = empty
upp_avail (AvailTC name ns) = hcat [pprOccName (getOccName name), bang, upp_export ns']
			    where
			      bang | name `elem` ns = empty
				   | otherwise	    = char '|'
			      ns' = filter (/= name) ns

upp_export :: [Name] -> SDoc
upp_export []    = empty
upp_export names = braces (hsep (map (pprOccName . getOccName) names)) 

upp_fixity :: (Name, Fixity) -> SDoc
upp_fixity (name, fixity) = hsep [ptext SLIT("0"), ppr fixity, ppr name, semi]
	-- Dummy version number!

ppr_unqual_name :: NamedThing a => a -> SDoc		-- Just its occurrence name
ppr_unqual_name name = pprOccName (getOccName name)
\end{code}


%************************************************************************
%*				 					*
\subsection{Comparisons}
%*				 					*
%************************************************************************
				 

The various sorts above simply prevent unnecessary "wobbling" when
things change that don't have to.  We therefore compare lexically, not
by unique

\begin{code}
lt_avail :: AvailInfo -> AvailInfo -> Bool

a1 `lt_avail` a2 = availName a1 `lt_name` availName a2

lt_name :: Name -> Name -> Bool
n1 `lt_name` n2 = nameRdrName n1 < nameRdrName n2

lt_lexical :: NamedThing a => a -> a -> Bool
lt_lexical a1 a2 = getName a1 `lt_name` getName a2

lt_imp_vers :: ImportVersion a -> ImportVersion a -> Bool
lt_imp_vers (m1,_,_,_) (m2,_,_,_) = m1 < m2

sort_versions vs = sortLt lt_vers vs

lt_vers :: LocalVersion Name -> LocalVersion Name -> Bool
lt_vers (n1,v1) (n2,v2) = n1 `lt_name` n2
\end{code}


\begin{code}
hPutCol :: Handle 
	-> (a -> SDoc)
	-> [a]
	-> IO ()
hPutCol hdl fmt xs = mapIO (printForIface hdl . fmt) xs

mapIO :: (a -> IO b) -> [a] -> IO ()
mapIO f []     = return ()
mapIO f (x:xs) = f x >> mapIO f xs
\end{code}
