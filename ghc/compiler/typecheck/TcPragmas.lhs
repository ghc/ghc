%
% (c) The GRASP/AQUA Project, Glasgow University, 1993-1995
%
\section[TcPragmas]{Typecheck ``pragmas'' of various kinds}

\begin{code}
#include "HsVersions.h"

module TcPragmas (
	tcClassOpPragmas,
	tcDataPragmas,
	tcDictFunPragmas,
	tcGenPragmas,
	tcTypePragmas
    ) where

IMPORT_Trace	-- ToDo: rm (debugging)
import Pretty
import Outputable

import TcMonad		-- typechecking monadic machinery
import TcMonadFns	( mkIdsWithGivenTys )
import AbsSyn		-- the stuff being typechecked

import AbsPrel		( PrimOp(..)	-- to see CCallOp
			  IF_ATTACK_PRAGMAS(COMMA tagOf_PrimOp)
			  IF_ATTACK_PRAGMAS(COMMA pprPrimOp)
			)
import AbsUniType
import CE		( lookupCE, nullCE, CE(..) )
import CmdLineOpts
import CostCentre
import E
import Errors
import HsCore		-- ****** NEED TO SEE CONSTRUCTORS ******
import HsPragmas	-- ****** NEED TO SEE CONSTRUCTORS ******
import Id
import IdInfo
import WwLib		( mkWwBodies )
import InstEnv		( lookupClassInstAtSimpleType )
import Maybes		( assocMaybe, catMaybes, Maybe(..) )
import CoreLint		( lintUnfolding )
import PlainCore
import TCE		( TCE(..), UniqFM )
import TVE
import TcMonoType	( tcMonoType )
import TcPolyType	( tcPolyType )
import Util
import SrcLoc
\end{code}

The basic idea is: Given an @Id@ that only lacks its @IdInfo@
(represented as a function \tr{IdInfo -> Id}, use the pragmas given to
figure out the @IdInfo@, then give back the now-complete @Id@.

Of course, the pragmas also need to be checked.

%************************************************************************
%*									*
\subsection[tcClassOpPragmas]{@ClassOp@ pragmas}
%*									*
%************************************************************************

\begin{code}
tcClassOpPragmas :: E			-- Class/TyCon lookup tables
	     -> UniType			-- global type of the class method
	     -> Id			-- *final* ClassOpId
	     -> Id			-- *final* DefaultMethodId
	     -> SpecEnv			-- Instance info for this class op
	     -> RenamedClassOpPragmas 	-- info w/ which to complete, giving...
	     -> Baby_TcM (IdInfo, IdInfo)	-- ... final info for ClassOp and DefaultMethod

tcClassOpPragmas _ _ rec_classop_id rec_defm_id spec_infos NoClassOpPragmas
  = returnB_Tc (noIdInfo `addInfo` spec_infos, noIdInfo)

tcClassOpPragmas e global_ty
		 rec_classop_id rec_defm_id 
		 spec_infos
		 (ClassOpPragmas classop_pragmas defm_pragmas)
  = tcGenPragmas e
		 Nothing{-ty unknown-} rec_classop_id
		 classop_pragmas	`thenB_Tc` \ classop_idinfo ->

    tcGenPragmas e
		 Nothing{-ty unknown-} rec_defm_id
		 defm_pragmas		`thenB_Tc` \ defm_idinfo ->

    returnB_Tc (classop_idinfo `addInfo` spec_infos, defm_idinfo)
\end{code}

%************************************************************************
%*									*
\subsection[tcInstancePragmas]{Instance-related pragmas of various sorts}
%*									*
%************************************************************************

{\em Every} instance declaration produces a ``dictionary function''
(dfun) of some sort; every flavour of @InstancePragmas@ gives a way to
convey information about a DictFunId.

\begin{code}
tcDictFunPragmas
	:: E 			    -- Class/TyCon lookup tables
	-> UniType		    -- DictFunId type
	-> Id			    -- final DictFunId (don't touch)
	-> RenamedInstancePragmas   -- info w/ which to complete, giving...
	-> Baby_TcM IdInfo	    -- ... final DictFun IdInfo

tcDictFunPragmas _ _ final_dfun NoInstancePragmas
  = returnB_Tc noIdInfo

tcDictFunPragmas e dfun_ty final_dfun pragmas
  = let
    	dfun_pragmas
	  = case pragmas of
	      SimpleInstancePragma  	x   -> x
	      ConstantInstancePragma 	x _ -> x
	      SpecialisedInstancePragma x _ -> x
    in
    tcGenPragmas e (Just dfun_ty) final_dfun dfun_pragmas
\end{code}

%************************************************************************
%*									*
\subsection[tcGenPragmas]{Basic pragmas about a value}
%*									*
%************************************************************************

Nota bene: @tcGenPragmas@ guarantees to succeed; if it encounters
a problem, it just returns @noIdInfo@.

\begin{code}
tcGenPragmas
	:: E			-- lookup table
	-> Maybe UniType	-- of Id, if we have it (for convenience)
	-> Id			-- *incomplete* Id (do not *touch*!)
	-> RenamedGenPragmas	-- info w/ which to complete, giving...
	-> Baby_TcM IdInfo	-- IdInfo for this Id

tcGenPragmas e ty_maybe rec_final_id NoGenPragmas
  = returnB_Tc noIdInfo

tcGenPragmas e ty_maybe rec_final_id
	     (GenPragmas arity_maybe upd_maybe def strictness unfold specs)
  = 	-- Guarantee success!
    recoverIgnoreErrorsB_Tc noIdInfo (

	-- OK, now we do the business
    let
	arity_info  = get_arity  arity_maybe
	upd_info    = get_upd    upd_maybe
    in
    tc_strictness e ty_maybe rec_final_id strictness
				`thenB_Tc` \ (strict_info, wrapper_unfold_info) ->

	-- If the unfolding fails to look consistent, we don't
	-- want to junk *all* the IdInfo
    recoverIgnoreErrorsB_Tc noInfo_UF (
	tc_unfolding e unfold
    )				`thenB_Tc` \ unfold_info ->

	-- Same as unfolding; if we fail, don't junk all IdInfo
    recoverIgnoreErrorsB_Tc nullSpecEnv (
	tc_specs e rec_final_id ty_maybe specs
    )				`thenB_Tc` \ spec_env -> 

    returnB_Tc (
	noIdInfo
	`addInfo` arity_info
	`addInfo` upd_info
	`addInfo` def

	    -- The strictness info *may* imply an unfolding
	    -- (the "wrapper_unfold"); that info is added; if
	    -- there is also an explicit unfolding, it will
	    -- take precedence, because it is "added" later.
	`addInfo` strict_info
	`addInfo_UF` wrapper_unfold_info

	`addInfo_UF` unfold_info
	`addInfo` spec_env
    ))
  where
    get_arity Nothing  = noInfo
    get_arity (Just a) = mkArityInfo a

    get_upd Nothing  = noInfo
    get_upd (Just u) = (u :: UpdateInfo)
\end{code}

Don't use the strictness info if a flag set.
\begin{code}
tc_strictness
	:: E
	-> Maybe UniType
	-> Id		-- final Id (do not *touch*)
	-> ImpStrictness Name
	-> Baby_TcM (StrictnessInfo, UnfoldingDetails)

tc_strictness e ty_maybe rec_final_id info
  = getSwitchCheckerB_Tc    `thenB_Tc` \ sw_chkr ->
    if sw_chkr IgnoreStrictnessPragmas then
	returnB_Tc (noInfo, noInfo_UF)
    else
	do_strictness e ty_maybe rec_final_id info
\end{code}

An easy one first:
\begin{code}
do_strictness e ty_maybe rec_final_id NoImpStrictness
  = returnB_Tc (noInfo, noInfo_UF)
\end{code}

We come to a nasty one now.  We have strictness info---possibly
implying a worker---but (for whatever reason) no {\em type}
information for the wrapper.  We therefore want (a)~{\em not} to
create a wrapper unfolding (we {\em cannot}) \& to be sure that one is
never asked for (!); and (b)~we want to keep the strictness/absence
info, because there's too much good stuff there to ignore completely.
We are not bothered about any pragmatic info for any alleged worker.
NB: this code applies only to {\em imported} info.  So here we go:

\begin{code}
do_strictness e Nothing rec_final_id (ImpStrictness is_bot arg_info _)
  = let
	strictness_info
	  = if is_bot
	    then mkBottomStrictnessInfo
	    else mkStrictnessInfo arg_info Nothing
    in
    returnB_Tc (strictness_info, noInfo_UF)
      -- no unfolding: the key --^^^^^^
\end{code}

And, finally, the have-everthing, know-everything, do-everything
``normal case''.
\begin{code}
do_strictness e (Just wrapper_ty) rec_final_id
	      (ImpStrictness is_bot wrap_arg_info wrkr_pragmas)

  | is_bot -- it's a "bottoming Id"
  = returnB_Tc (mkBottomStrictnessInfo, noInfo_UF)

  | not (indicatesWorker wrap_arg_info)
  = -- No worker
    returnB_Tc (mkStrictnessInfo wrap_arg_info Nothing, noInfo_UF)

  | otherwise
  = -- Strictness info suggests a worker.  Things could still
    -- go wrong if there's an abstract type involved, mind you.
    let
	(tv_tmpls, arg_tys, ret_ty) = splitTypeWithDictsAsArgs wrapper_ty
	n_wrapper_args		    = length wrap_arg_info	
		-- Don't have more args than this, else you risk 
		-- losing laziness!!
    in
    getUniquesB_Tc (length tv_tmpls)	`thenB_Tc` \ tyvar_uniqs ->
    getUniquesB_Tc n_wrapper_args	`thenB_Tc` \ arg_uniqs ->
    
    let
        (inst_env, tyvars, tyvar_tys) = instantiateTyVarTemplates tv_tmpls tyvar_uniqs

	inst_arg_tys = map (instantiateTy inst_env) arg_tys
	(undropped_inst_arg_tys, dropped_inst_arg_tys)
	  = splitAt n_wrapper_args inst_arg_tys

	inst_ret_ty  = glueTyArgs dropped_inst_arg_tys
				  (instantiateTy inst_env ret_ty)

	args         = zipWith mk_arg arg_uniqs	undropped_inst_arg_tys
	mk_arg uniq ty = mkSysLocal SLIT("wrap") uniq ty mkUnknownSrcLoc
	-- ASSERT: length args = n_wrapper_args
    in

    uniqSMtoBabyTcM (mkWwBodies inst_ret_ty tyvars args wrap_arg_info)
							`thenB_Tc` \ result ->
    case result of

	Nothing -> 	-- Alas, we met an abstract type
	    returnB_Tc (mkStrictnessInfo wrap_arg_info Nothing, noInfo_UF)

	Just (wrapper_w_hole, worker_w_hole, worker_strictness, worker_ty_w_hole) ->

	    let 
		worker_ty   = worker_ty_w_hole inst_ret_ty
	    in
	    getUniqueB_Tc `thenB_Tc` \ uniq ->
	    fixB_Tc ( \ rec_wrkr_id ->

		tcGenPragmas e
			 (Just worker_ty)
			 rec_wrkr_id
			 wrkr_pragmas	`thenB_Tc` \ wrkr_id_info ->

	    	returnB_Tc (mkWorkerId uniq rec_final_id worker_ty
				(wrkr_id_info `addInfo` worker_strictness))
			-- Note: the above will *clobber* any strictness
			-- info for the worker which was read in from the
			-- interface (but there usually isn't any).

	    ) `thenB_Tc` \ worker_id ->

	    let
		wrapper_rhs = wrapper_w_hole worker_id
		n_tyvars    = length tyvars
		arity	    = length args
	
	    in
	    returnB_Tc (
		mkStrictnessInfo wrap_arg_info (Just worker_id),
		mkUnfolding UnfoldAlways ({-pprTrace "imp wrapper:\n" (ppAboves [ppr PprDebug wrapper_rhs, ppInfo PprDebug (\x->x) worker_strictness])-} wrapper_rhs)
		    -- We only do this for imported things, which this is.
	    )
\end{code}

\begin{code}
tc_specs :: E
	 -> Id -- final Id for which these are specialisations (do not *touch*)
	 -> Maybe UniType
	 -> [([Maybe RenamedMonoType], Int, RenamedGenPragmas)]
	 -> Baby_TcM SpecEnv

tc_specs e rec_main_id Nothing{-no type, we lose-} spec_pragmas
  = returnB_Tc nullSpecEnv  -- ToDo: msg????????

tc_specs e rec_main_id (Just main_ty) spec_pragmas
  = mapB_Tc do_one_pragma spec_pragmas	`thenB_Tc` \ spec_infos ->
    returnB_Tc (mkSpecEnv spec_infos)
  where
    (main_tyvars, _) = splitForalls main_ty
 
    rec_ce  = getE_CE  e
    rec_tce = getE_TCE e

    do_one_pragma (maybe_monotys, dicts_to_ignore, gen_prags)
      = mapB_Tc (tc_ty_maybe rec_ce rec_tce) maybe_monotys
				`thenB_Tc` \ maybe_tys ->
	getSrcLocB_Tc		`thenB_Tc` \ locn ->
	getUniqueB_Tc		`thenB_Tc` \ uniq ->

	checkB_Tc (length main_tyvars /= length maybe_tys)
		(badSpecialisationErr "value" "wrong number of specialising types"
				      (length main_tyvars) maybe_tys locn)
				`thenB_Tc_`
	let 
	    spec_ty = specialiseTy main_ty maybe_tys dicts_to_ignore
	in
	fixB_Tc ( \ rec_spec_id ->

	    tcGenPragmas e (Just spec_ty) rec_spec_id gen_prags
	    	`thenB_Tc` \ spec_id_info ->

	    returnB_Tc (mkSpecId uniq rec_main_id maybe_tys spec_ty spec_id_info)

	) `thenB_Tc` \ spec_id ->

	returnB_Tc (SpecInfo maybe_tys dicts_to_ignore spec_id)

tc_ty_maybe rec_ce rec_tce Nothing = returnB_Tc Nothing
tc_ty_maybe rec_ce rec_tce (Just ty)
  = tcMonoType rec_ce rec_tce nullTVE ty	`thenB_Tc` \ new_ty ->
    returnB_Tc (Just new_ty)
\end{code}

\begin{code}
tc_unfolding e NoImpUnfolding = returnB_Tc noInfo_UF
tc_unfolding e (ImpMagicUnfolding tag) = returnB_Tc (mkMagicUnfolding tag)

tc_unfolding e (ImpUnfolding guidance uf_core)
  = tc_uf_core nullLVE nullTVE uf_core	`thenB_Tc` \ core_expr ->
    getSrcLocB_Tc			`thenB_Tc` \ locn ->
    let
    	-- Bad unfoldings are so painful that we always lint-check them,
	-- marking them with BadUnfolding if lintUnfolding fails
	-- NB: We cant check the lint result and return noInfo_UF if
	--     lintUnfolding failed as this is too strict
	--     Instead getInfo_UF tests for BadUnfolding and converts
	--     to NoUnfoldingDetails when the unfolding is accessed

	maybe_lint_expr = lintUnfolding locn core_expr

	(lint_guidance, lint_expr) = case maybe_lint_expr of
	  Just lint_expr -> (guidance, lint_expr)
          Nothing        -> (BadUnfolding, panic_expr) 
    in
    returnB_Tc (mkUnfolding lint_guidance lint_expr)
  where
    rec_ce  = getE_CE  e
    rec_tce = getE_TCE e

    panic_expr = panic "TcPragmas: BadUnfolding should not be touched"

    tc_uf_core :: LVE	    -- lookup table for local binders
			    -- (others: we hope we can figure them out)
	       -> TVE	    -- lookup table for tyvars
	       -> UnfoldingCoreExpr Name
	       -> Baby_TcM PlainCoreExpr

    tc_uf_core lve tve (UfCoVar v)
      = tc_uf_Id lve v		`thenB_Tc` \ id ->
	returnB_Tc (CoVar id)

    tc_uf_core lve tve (UfCoLit l)
      = returnB_Tc (CoLit l)

    tc_uf_core lve tve (UfCoCon con tys as)
      = tc_uf_Id lve (BoringUfId con)	`thenB_Tc` \ con_id ->
	mapB_Tc (tc_uf_type tve) tys	`thenB_Tc` \ core_tys ->
	mapB_Tc (tc_uf_atom lve tve) as	`thenB_Tc` \ core_atoms ->
	returnB_Tc (CoCon con_id core_tys core_atoms)

    --  If a ccall, we have to patch in the types read from the pragma.

    tc_uf_core lve tve (UfCoPrim (UfCCallOp str is_casm may_gc arg_tys res_ty) app_tys as)
      = ASSERT(null app_tys)
    	mapB_Tc (tc_uf_type tve) arg_tys	`thenB_Tc` \ core_arg_tys ->
        tc_uf_type tve res_ty		`thenB_Tc` \ core_res_ty ->
        mapB_Tc (tc_uf_type tve) app_tys	`thenB_Tc` \ core_app_tys ->
	mapB_Tc (tc_uf_atom lve tve) as	`thenB_Tc` \ core_atoms ->
	returnB_Tc (CoPrim (CCallOp str is_casm may_gc core_arg_tys core_res_ty)
			 core_app_tys core_atoms)

    tc_uf_core lve tve (UfCoPrim (UfOtherOp op) tys as)
      = mapB_Tc (tc_uf_type tve) tys	`thenB_Tc` \ core_tys ->
	mapB_Tc (tc_uf_atom lve tve) as	`thenB_Tc` \ core_atoms ->
	returnB_Tc (CoPrim op core_tys core_atoms)

    tc_uf_core lve tve (UfCoLam binders body)
      = tc_uf_binders tve binders `thenB_Tc` \ lve2 ->
	let
	    new_binders = map snd lve2
	    new_lve     = lve2 `plusLVE` lve
	in
	tc_uf_core new_lve tve body	 `thenB_Tc` \ new_body ->
	returnB_Tc (CoLam new_binders new_body)

    tc_uf_core lve tve (UfCoTyLam tv body)
      = let
	    (new_tv, uniq, new_tv_ty) = tc_uf_tyvar tv
	    new_tve = tve `plusTVE` (unitTVE uniq new_tv_ty)
	in
	tc_uf_core lve new_tve body	 `thenB_Tc` \ new_body ->
	returnB_Tc (CoTyLam new_tv new_body)

    tc_uf_core lve tve (UfCoApp fun arg)
      = tc_uf_core lve tve fun	`thenB_Tc` \ new_fun ->
        tc_uf_atom lve tve arg	`thenB_Tc` \ new_arg ->
	returnB_Tc (CoApp new_fun new_arg)

    tc_uf_core lve tve (UfCoTyApp expr ty)
      = tc_uf_core lve tve expr	`thenB_Tc` \ new_expr ->
        tc_uf_type tve ty	`thenB_Tc` \ new_ty ->
	returnB_Tc (mkCoTyApp new_expr new_ty)

    tc_uf_core lve tve (UfCoCase scrut alts)
      = tc_uf_core lve tve scrut `thenB_Tc` \ new_scrut ->
	tc_alts alts		 `thenB_Tc` \ new_alts ->
	returnB_Tc (CoCase new_scrut new_alts)
      where
	tc_alts (UfCoAlgAlts alts deflt)
	  = mapB_Tc tc_alg_alt alts   `thenB_Tc` \ new_alts ->
	    tc_deflt deflt  	    `thenB_Tc` \ new_deflt ->
	    returnB_Tc (CoAlgAlts new_alts new_deflt)
	  where
	    tc_alg_alt (con, params, rhs)
	      = tc_uf_Id lve (BoringUfId con)	`thenB_Tc` \ con_id ->
		tc_uf_binders tve params	`thenB_Tc` \ lve2 ->
		let
		    new_params = map snd lve2
		    new_lve    = lve2 `plusLVE` lve
		in
		tc_uf_core new_lve tve rhs	`thenB_Tc` \ new_rhs ->
		returnB_Tc (con_id, new_params, new_rhs)

	tc_alts (UfCoPrimAlts alts deflt)
	  = mapB_Tc tc_prim_alt alts  `thenB_Tc` \ new_alts ->
	    tc_deflt deflt  	    `thenB_Tc` \ new_deflt ->
	    returnB_Tc (CoPrimAlts new_alts new_deflt)
	  where
	    tc_prim_alt (lit, rhs)
	      = tc_uf_core lve tve rhs	`thenB_Tc` \ new_rhs ->
		returnB_Tc (lit, new_rhs)

	tc_deflt UfCoNoDefault = returnB_Tc CoNoDefault
	tc_deflt (UfCoBindDefault b rhs)
	  = tc_uf_binders tve [b]	`thenB_Tc` \ lve2 ->
	    let
		[new_b] = map snd lve2
		new_lve = lve2 `plusLVE` lve
	    in
	    tc_uf_core new_lve tve rhs	`thenB_Tc` \ new_rhs ->
	    returnB_Tc (CoBindDefault new_b new_rhs)

    tc_uf_core lve tve (UfCoLet (UfCoNonRec b rhs) body)
      = tc_uf_core lve tve rhs	`thenB_Tc` \ new_rhs ->
	tc_uf_binders tve [b]	`thenB_Tc` \ lve2 ->
	let
	    [new_b] = map snd lve2
	    new_lve = lve2 `plusLVE` lve
	in
	tc_uf_core new_lve tve body `thenB_Tc` \ new_body ->
	returnB_Tc (CoLet (CoNonRec new_b new_rhs) new_body)

    tc_uf_core lve tve (UfCoLet (UfCoRec pairs) body)
      = let
	    (binders, rhss) = unzip pairs
	in
	tc_uf_binders tve binders   `thenB_Tc` \ lve2 ->
	let
	    new_binders = map snd lve2
	    new_lve     = lve2 `plusLVE` lve
	in
	mapB_Tc (tc_uf_core new_lve tve) rhss `thenB_Tc` \ new_rhss ->
	tc_uf_core new_lve tve         body `thenB_Tc` \ new_body ->
	returnB_Tc (CoLet (CoRec (new_binders `zip` new_rhss)) new_body)

    tc_uf_core lve tve (UfCoSCC uf_cc body)
      = tc_uf_cc   uf_cc	    `thenB_Tc` \ new_cc ->
	tc_uf_core lve tve body     `thenB_Tc` \ new_body ->
	returnB_Tc (CoSCC new_cc new_body)
      where
	tc_uf_cc (UfAutoCC id m g is_dupd is_caf)
	  = tc_uf_Id lve id	`thenB_Tc` \ new_id ->
	    returnB_Tc (adjust is_caf is_dupd (mkAutoCC new_id m g IsNotCafCC))

	tc_uf_cc (UfDictCC id m g is_dupd is_caf)
	  = tc_uf_Id lve id	`thenB_Tc` \ new_id ->
	    returnB_Tc (adjust is_caf is_dupd (mkDictCC new_id m g IsNotCafCC))

        tc_uf_cc (UfUserCC n m g d c) = returnB_Tc (adjust c d (mkUserCC n m g))

        tc_uf_cc (UfPreludeDictsCC d) = returnB_Tc (preludeDictsCostCentre d)
        tc_uf_cc (UfAllDictsCC m g d) = returnB_Tc (mkAllDictsCC m g d)

	--------
	adjust is_caf is_dupd cc
	  = let
		maybe_cafify = if is_caf  then cafifyCC else (\x->x)
		maybe_dupify = if is_dupd then dupifyCC else (\x->x)
	    in
	    maybe_dupify (maybe_cafify cc)

    ---------------
    tc_uf_atom lve tve (UfCoLitAtom l)
      = returnB_Tc (CoLitAtom l)

    tc_uf_atom lve tve (UfCoVarAtom v)
      = tc_uf_Id lve v			`thenB_Tc` \ new_v ->
	returnB_Tc (CoVarAtom new_v)

    ---------------
    tc_uf_binders tve ids_and_tys
      = let
	    (ids, tys) = unzip ids_and_tys
	in
	mapB_Tc (tc_uf_type tve) tys	`thenB_Tc` \ new_tys ->

	returnB_Tc (mkIdsWithGivenTys ids new_tys (repeat noIdInfo))

    ---------------
    -- "tyvar" binders (see tcPolyType for the TyVarTemplate equiv):

    tc_uf_tyvar (Short u short_name)
      = let
	    tyvar = mkUserTyVar u short_name
	in
	(tyvar, u, mkTyVarTy tyvar)

    ---------------
    tc_uf_Id lve (BoringUfId v)
      = case (assocMaybe lve v) of
	  Just xx -> returnB_Tc xx
	  Nothing -> case (lookupE_ValueQuietly e v) of
		       Just xx -> returnB_Tc xx
		       Nothing -> -- pprTrace "WARNING: Discarded bad unfolding from interface:\n"
		     		  --	   (ppCat [ppStr "Failed lookup for BoringUfId:",
			     	  --	           ppr PprDebug v])
				  (failB_Tc (panic "tc_uf_Id:BoringUfId: no lookup"))
				  -- will be recover'd from
				  -- ToDo: shouldn't the renamer have handled this? [wdp 94/04/29]

    tc_uf_Id lve (SuperDictSelUfId c sc)
      = let
	    clas       = lookupCE rec_ce c
	    super_clas = lookupCE rec_ce sc
	in
	returnB_Tc (getSuperDictSelId clas super_clas)

    tc_uf_Id lve (ClassOpUfId c op_name)
      = let
	    clas = lookupCE rec_ce c
	    op	 = lookup_class_op clas op_name
	in
	returnB_Tc (getClassOpId clas op)

    tc_uf_Id lve (DefaultMethodUfId c op_name)
      = let
	    clas = lookupCE rec_ce c
	    op	 = lookup_class_op clas op_name
	in
	returnB_Tc (getDefaultMethodId clas op)

    tc_uf_Id lve uf_id@(DictFunUfId c ty)
      = tc_uf_type nullTVE ty	`thenB_Tc` \ new_ty ->
	let
	    clas = lookupCE rec_ce c
	    dfun_id = case (lookupClassInstAtSimpleType clas new_ty) of
			  Just id -> id
			  Nothing -> pprPanic "tc_uf_Id:DictFunUfId:"
					(ppr PprDebug (UfCoVar uf_id))
					-- The class and type are both
					-- visible, so the instance should
					-- jolly well be too!
	in
	returnB_Tc dfun_id

    tc_uf_Id lve (ConstMethodUfId c op_name ty)
      = tc_uf_type nullTVE ty	`thenB_Tc` \ new_ty ->
	let
	    clas = lookupCE rec_ce c
	    op	 = lookup_class_op clas op_name
	in
	returnB_Tc (getConstMethodId clas op new_ty)

    tc_uf_Id lve uf_id@(SpecUfId unspec ty_maybes)
      = tc_uf_Id lve unspec 	    `thenB_Tc` \ unspec_id ->
	mapB_Tc (tc_ty_maybe rec_ce rec_tce) ty_maybes
				    `thenB_Tc` \ maybe_tys ->
        let
	   spec_id = lookupSpecId unspec_id maybe_tys
	in
	returnB_Tc spec_id

    tc_uf_Id lve (WorkerUfId unwrkr)
      = tc_uf_Id lve unwrkr 	`thenB_Tc` \ unwrkr_id ->
        let
	    strictness_info = getIdStrictness unwrkr_id
 	in
	if isLocallyDefined unwrkr_id
	then
	    -- A locally defined value will not have any strictness info (yet),
	    -- so we can't extract the locally defined worker Id from it :-(

            pprTrace "WARNING: Discarded bad unfolding from interface:\n"
		     (ppCat [ppStr "Worker Id in unfolding is defined locally:",
			     ppr PprDebug unwrkr_id])
	    (failB_Tc (panic "tc_uf_Id:WorkerUfId: locally defined"))
	    -- will be recover'd from
	else
    	    returnB_Tc (getWorkerId strictness_info)

    ---------------
    lookup_class_op clas (ClassOpName _ _ _ tag)
      = getClassOps clas !! (tag - 1)

    ---------------------------------------------------------------------
    tc_uf_type :: TVE -> UnfoldingType Name -> Baby_TcM UniType

    tc_uf_type tve ty = tcPolyType rec_ce rec_tce tve ty
\end{code}

%************************************************************************
%*									*
\subsection[tcDataPragmas]{@data@ type pragmas}
%*									*
%************************************************************************

The purpose of a @data@ pragma is to convey data-constructor
information that would otherwise be unknown.

It also records specialisation information which is added to each data
constructor. This info just contains the type info for the
specialisations which exist. No specialised Ids are actually created.

\begin{code}
tcDataPragmas :: TCE -> TVE -> TyCon -> [TyVarTemplate]
	      -> RenamedDataPragmas
	      -> Baby_TcM ([RenamedConDecl],	-- any pragma condecls
			   [SpecInfo])		-- specialisation info from pragmas

tcDataPragmas rec_tce tve rec_tycon new_tyvars (DataPragmas con_decls specs)
  = mapB_Tc do_one_spec specs  		`thenB_Tc` \ spec_infos ->
    returnB_Tc (con_decls, spec_infos)
  where
    do_one_spec maybe_monotys
      = mapB_Tc (tc_ty_maybe nullCE rec_tce) maybe_monotys
				`thenB_Tc` \ maybe_tys ->
	getSrcLocB_Tc		`thenB_Tc` \ locn ->

	checkB_Tc (length new_tyvars /= length maybe_tys)
		(badSpecialisationErr "data" "wrong number of specialising types"
				      (length new_tyvars) maybe_tys locn)
				`thenB_Tc_`

	checkB_Tc (not (all isUnboxedDataType (catMaybes maybe_tys)))
		(badSpecialisationErr "data" "not all unboxed types"
				      (length new_tyvars) maybe_tys locn)
				`thenB_Tc_`

        returnB_Tc (SpecInfo maybe_tys 0 (panic "DataPragma:SpecInfo:SpecId"))
\end{code}

%************************************************************************
%*									*
\subsection[tcTypePragmas]{@type@ synonym pragmas}
%*									*
%************************************************************************

The purpose of a @type@ pragma is to say that the synonym's
representation should not be used by the user.

\begin{code}
tcTypePragmas :: TypePragmas
	      -> Bool		-- True <=> abstract synonym, please

tcTypePragmas NoTypePragmas     = False
tcTypePragmas AbstractTySynonym = True
\end{code}

