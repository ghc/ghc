%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1995
%
\section[SimplCore]{Driver for simplifying @Core@ programs}

\begin{code}
#include "HsVersions.h"

module SimplCore (
	core2core
    ) where

import Type		( getTyConDataCons )
--SAVE:import ArityAnal	( arityAnalProgram )
import Bag
import BinderInfo	( BinderInfo) -- instances only
import CgCompInfo	( uNFOLDING_CREATION_THRESHOLD,
			  uNFOLDING_USE_THRESHOLD,
			  uNFOLDING_OVERRIDE_THRESHOLD,
			  uNFOLDING_CON_DISCOUNT_WEIGHT
			)
import CmdLineOpts
import CoreLint		( lintCoreBindings )
import FloatIn		( floatInwards )
import FloatOut		( floatOutwards )
import Id		( getIdUnfolding,
			  idType, toplevelishId,
			  idWantsToBeINLINEd,
			  unfoldingUnfriendlyId, isWrapperId,
			  mkTemplateLocals
			)
import IdInfo
import LiberateCase	( liberateCase )
import MainMonad
import Maybes
import SAT		( doStaticArgs )
import SCCauto
--ANDY:
--import SimplHaskell	( coreToHaskell )
import SimplMonad	( zeroSimplCount, showSimplCount, TickType, SimplCount )
import SimplPgm		( simplifyPgm )
import SimplVar		( leastItCouldCost )
import Specialise
import SpecUtils	( pprSpecErrs )
import StrictAnal	( saWwTopBinds )
import FoldrBuildWW
import AnalFBWW
#if ! OMIT_DEFORESTER
import Deforest		( deforestProgram )
import DefUtils		( deforestable )
#endif
import UniqSupply
import Util
\end{code}

\begin{code}
core2core :: [CoreToDo]			-- spec of what core-to-core passes to do
	  -> (GlobalSwitch->SwitchResult)-- "global" command-line info lookup fn
	  -> FAST_STRING		-- module name (profiling only)
	  -> PprStyle			-- printing style (for debugging only)
	  -> UniqSupply		-- a name supply
	  -> [TyCon]			-- local data tycons and tycon specialisations
	  -> FiniteMap TyCon [(Bool, [Maybe Type])]
	  -> [CoreBinding]		-- input...
	  -> MainIO
	      ([CoreBinding],	-- results: program, plus...
	       IdEnv UnfoldingDetails,	--  unfoldings to be exported from here
	      SpecialiseData)		--  specialisation data

core2core core_todos sw_chkr module_name ppr_style us local_tycons tycon_specs binds
  = BSCC("Core2Core")
    if null core_todos then -- very rare, I suspect...
	-- well, we still must do some renumbering
	returnMn (
	(snd (instCoreBindings (mkUniqueSupplyGrimily us) binds), nullIdEnv, init_specdata)
	)
    else
	(if do_verbose_core2core then
	    writeMn stderr "VERBOSE CORE-TO-CORE:\n"
	 else returnMn ()) `thenMn_`

	-- better do the main business
	foldl_mn do_core_pass
		(binds, us, nullIdEnv, init_specdata, zeroSimplCount)
		core_todos
		`thenMn` \ (processed_binds, _, inline_env, spec_data, simpl_stats) ->

	(if  switch_is_on D_simplifier_stats
	 then writeMn stderr ("\nSimplifier Stats:\n")
		`thenMn_`
	      writeMn stderr (showSimplCount simpl_stats)
		`thenMn_`
	      writeMn stderr "\n"
	 else returnMn ()
	) `thenMn_`

	returnMn (processed_binds, inline_env, spec_data)
    ESCC
  where
    init_specdata = initSpecData local_tycons tycon_specs

    switch_is_on = switchIsOn sw_chkr

    do_verbose_core2core = switch_is_on D_verbose_core2core

    lib_case_threshold	-- ToDo: HACK HACK HACK : FIX ME FIX ME FIX ME
			-- Use 4x a known threshold
      = case (intSwitchSet sw_chkr UnfoldingOverrideThreshold) of
	  Nothing -> 4 * uNFOLDING_USE_THRESHOLD
	  Just xx -> 4 * xx

    -------------
    core_linter = if switch_is_on DoCoreLinting
		  then lintCoreBindings ppr_style
		  else ( \ whodunnit spec_done binds -> binds )

    --------------
    do_core_pass info@(binds, us, inline_env, spec_data, simpl_stats) to_do
      = let
	    (us1, us2) = splitUniqSupply us
    	in
    	case to_do of
	  CoreDoSimplify simpl_sw_chkr
	    -> BSCC("CoreSimplify")
	       begin_pass ("Simplify" ++ if switchIsOn simpl_sw_chkr SimplDoFoldrBuild
					 then " (foldr/build)" else "") `thenMn_`
	       case (simplifyPgm binds sw_chkr simpl_sw_chkr simpl_stats us1) of
		 (p, it_cnt, simpl_stats2)
		   -> end_pass False us2 p inline_env spec_data simpl_stats2
			       ("Simplify (" ++ show it_cnt ++ ")"
				 ++ if switchIsOn simpl_sw_chkr SimplDoFoldrBuild
				    then " foldr/build" else "")
	       ESCC

	  CoreDoFoldrBuildWorkerWrapper
	    -> BSCC("CoreDoFoldrBuildWorkerWrapper")
	       begin_pass "FBWW" `thenMn_`
	       case (mkFoldrBuildWW switch_is_on us1 binds) of { binds2 ->
	       end_pass False us2 binds2 inline_env spec_data simpl_stats "FBWW"
	       } ESCC

	  CoreDoFoldrBuildWWAnal
	    -> BSCC("CoreDoFoldrBuildWWAnal")
	       begin_pass "AnalFBWW" `thenMn_`
	       case (analFBWW switch_is_on binds) of { binds2 ->
	       end_pass False us2 binds2 inline_env spec_data simpl_stats "AnalFBWW"
	       } ESCC

	  CoreLiberateCase
	    -> BSCC("LiberateCase")
	       begin_pass "LiberateCase" `thenMn_`
	       case (liberateCase lib_case_threshold binds) of { binds2 ->
	       end_pass False us2 binds2 inline_env spec_data simpl_stats "LiberateCase"
	       } ESCC

	  CoreDoCalcInlinings1	-- avoid inlinings w/ cost-centres
	    -> BSCC("CoreInlinings1")
	       begin_pass "CalcInlinings" `thenMn_`
	       case (calcInlinings False sw_chkr inline_env binds) of { inline_env2 ->
	       end_pass False us2 binds inline_env2 spec_data simpl_stats "CalcInlinings"
	       } ESCC

	  CoreDoCalcInlinings2  -- allow inlinings w/ cost-centres
	    -> BSCC("CoreInlinings2")
	       begin_pass "CalcInlinings" `thenMn_`
	       case (calcInlinings True sw_chkr inline_env binds) of { inline_env2 ->
	       end_pass False us2 binds inline_env2 spec_data simpl_stats "CalcInlinings"
	       } ESCC

	  CoreDoFloatInwards
	    -> BSCC("FloatInwards")
	       begin_pass "FloatIn" `thenMn_`
	       case (floatInwards binds) of { binds2 ->
	       end_pass False us2 binds2 inline_env spec_data simpl_stats "FloatIn"
	       } ESCC

	  CoreDoFullLaziness
	    -> BSCC("CoreFloating")
	       begin_pass "FloatOut" `thenMn_`
	       case (floatOutwards switch_is_on us1 binds) of { binds2 ->
	       end_pass False us2 binds2 inline_env spec_data simpl_stats "FloatOut"
	       } ESCC

	  CoreDoStaticArgs
	    -> BSCC("CoreStaticArgs")
	       begin_pass "StaticArgs" `thenMn_`
	       case (doStaticArgs binds us1) of { binds2 ->
	       end_pass False us2 binds2 inline_env spec_data simpl_stats "StaticArgs"
		-- Binds really should be dependency-analysed for static-
		-- arg transformation... Not to worry, they probably are.
		-- (I don't think it *dies* if they aren't [WDP 94/04/15])
	       } ESCC

	  CoreDoStrictness
	    -> BSCC("CoreStranal")
	       begin_pass "StrAnal" `thenMn_`
	       case (saWwTopBinds us1 switch_is_on binds) of { binds2 ->
	       end_pass False us2 binds2 inline_env spec_data simpl_stats "StrAnal"
	       } ESCC

	  CoreDoSpecialising
	    -> BSCC("Specialise")
	       begin_pass "Specialise" `thenMn_`
	       case (specProgram switch_is_on us1 binds spec_data) of {
		 (p, spec_data2@(SpecData _ spec_noerrs _ _ _
					  spec_errs spec_warn spec_tyerrs)) ->

		   -- if we got errors, we die straight away
		   (if not spec_noerrs ||
		       (switch_is_on ShowImportSpecs && not (isEmptyBag spec_warn)) then
			writeMn stderr (ppShow 1000 {-pprCols-}
			    (pprSpecErrs module_name spec_errs spec_warn spec_tyerrs))
			`thenMn_` writeMn stderr "\n"
		    else
			returnMn ()) `thenMn_`

		   (if not spec_noerrs then -- Stop here if specialisation errors occured
			exitMn 1
		   else
			returnMn ()) `thenMn_`

		   end_pass False us2 p inline_env spec_data2 simpl_stats "Specialise"
	       }
	       ESCC

	  CoreDoDeforest
#if OMIT_DEFORESTER
	    -> error "ERROR: CoreDoDeforest: not built into compiler\n"
#else
	    -> BSCC("Deforestation")
	       begin_pass "Deforestation" `thenMn_`
	       case (deforestProgram sw_chkr binds us1) of { binds2 ->
	       end_pass False us2 binds2 inline_env spec_data simpl_stats "Deforestation"
	       }
	       ESCC
#endif

	  CoreDoAutoCostCentres
	    -> BSCC("AutoSCCs")
	       begin_pass "AutoSCCs" `thenMn_`
	       case (addAutoCostCentres sw_chkr module_name binds) of { binds2 ->
	       end_pass False us2 binds2 inline_env spec_data simpl_stats "AutoSCCs"
	       }
	       ESCC

	  CoreDoPrintCore	-- print result of last pass
	    -> end_pass True us2 binds inline_env spec_data simpl_stats "Print"


    -------------------------------------------------

    begin_pass
      = if switch_is_on D_show_passes
	then \ what -> writeMn stderr ("*** Core2Core: "++what++"\n")
	else \ what -> returnMn ()

    end_pass print us2 binds2 inline_env2
	     spec_data2@(SpecData spec_done _ _ _ _ _ _ _)
	     simpl_stats2 what
      = -- report verbosely, if required
	(if (do_verbose_core2core && not print) ||
	    (print && not do_verbose_core2core)
	 then
	    writeMn stderr ("\n*** "++what++":\n")
		`thenMn_`
	    writeMn stderr (ppShow 1000
		(ppAboves (map (pprPlainCoreBinding ppr_style) binds2)))
		`thenMn_`
	    writeMn stderr "\n"
	 else
	    returnMn ()) `thenMn_`
	let
	    linted_binds = core_linter what spec_done binds2
	in
	returnMn
	(linted_binds,	-- processed binds, possibly run thru CoreLint
	 us2,		-- UniqueSupply for the next guy
	 inline_env2,	-- possibly-updated inline env
	 spec_data2,	-- possibly-updated specialisation info
	 simpl_stats2	-- accumulated simplifier stats
	)

-- here so it can be inlined...
foldl_mn f z []     = returnMn z
foldl_mn f z (x:xs) = f z x	`thenMn` \ zz ->
		     foldl_mn f zz xs
\end{code}

--- ToDo: maybe move elsewhere ---

For top-level, exported binders that either (a)~have been INLINEd by
the programmer or (b)~are sufficiently ``simple'' that they should be
inlined, we want to record this info in a suitable IdEnv.

But: if something has a ``wrapper unfolding,'' we do NOT automatically
give it a regular unfolding (exception below).  We usually assume its
worker will get a ``regular'' unfolding.  We can then treat these two
levels of unfolding separately (we tend to be very friendly towards
wrapper unfoldings, for example), giving more fine-tuned control.

The exception is: If the ``regular unfolding'' mentions no other
global Ids (i.e., it's all PrimOps and cases and local Ids) then we
assume it must be really good and we take it anyway.

We also need to check that everything in the RHS (values and types)
will be visible on the other side of an interface, too.

\begin{code}
calcInlinings :: Bool	-- True => inlinings with _scc_s are OK
	      -> (GlobalSwitch -> SwitchResult)
	      -> IdEnv UnfoldingDetails
	      -> [CoreBinding]
	      -> IdEnv UnfoldingDetails

calcInlinings scc_s_OK sw_chkr inline_env_so_far top_binds
  = let
	result = foldl calci inline_env_so_far top_binds
    in
    --pprTrace "inline env:\n" (ppAboves (map pp_item (getIdEnvMapping result)))
    result
  where
    pp_item (binder, details)
      = ppCat [ppr PprDebug binder, ppStr "=>", pp_det details]
      where
    	pp_det NoUnfoldingDetails   = ppStr "_N_"
	pp_det (IWantToBeINLINEd _) = ppStr "INLINE"
    	pp_det (GenForm _ _ expr guide)
    	  = ppAbove (ppr PprDebug guide) (ppr PprDebug expr)
    	pp_det other	    	    = ppStr "???"

    ------------
    switch_is_on = switchIsOn sw_chkr

    my_trace =  if (switch_is_on ReportWhyUnfoldingsDisallowed)
		then trace
		else \ msg stuff -> stuff

    (unfolding_creation_threshold, explicit_creation_threshold)
      = case (intSwitchSet sw_chkr UnfoldingCreationThreshold) of
    	  Nothing -> (uNFOLDING_CREATION_THRESHOLD, False)
	  Just xx -> (xx, True)

    unfold_use_threshold
      = case (intSwitchSet sw_chkr UnfoldingUseThreshold) of
	  Nothing -> uNFOLDING_USE_THRESHOLD
	  Just xx -> xx

    unfold_override_threshold
      = case (intSwitchSet sw_chkr UnfoldingOverrideThreshold) of
	  Nothing -> uNFOLDING_OVERRIDE_THRESHOLD
	  Just xx -> xx

    con_discount_weight = uNFOLDING_CON_DISCOUNT_WEIGHT

    calci inline_env (Rec pairs)
      = foldl (calc True{-recursive-}) inline_env pairs

    calci inline_env bind@(NonRec binder rhs)
      = calc False{-not recursive-} inline_env (binder, rhs)

    ---------------------------------------

    calc is_recursive inline_env (binder, rhs)
      | not (toplevelishId binder)
      = --pprTrace "giving up on not top-level:" (ppr PprDebug binder)
	ignominious_defeat

      | rhs_mentions_an_unmentionable
      || (not explicit_INLINE_requested
	  && (rhs_looks_like_a_caf || guidance_says_don't || guidance_size_too_big))
      = let
	    my_my_trace
	      = if explicit_INLINE_requested
		&& not (isWrapperId binder) -- these always claim to be INLINEd
		&& not have_inlining_already
		then trace  		    -- we'd better have a look...
		else my_trace

	    which = if scc_s_OK then " (late):" else " (early):"
    	in
	--pprTrace "giving up on size:" (ppCat [ppr PprDebug binder, ppr PprDebug
	--	[rhs_mentions_an_unmentionable, explicit_INLINE_requested,
	--	 rhs_looks_like_a_caf, guidance_says_don't, guidance_size_too_big]]) (
	my_my_trace ("unfolding disallowed for"++which++(ppShow 80 (ppr PprDebug binder))) (
	ignominious_defeat
	)
	--)

      | rhs `isWrapperFor` binder
	-- Don't add an explicit "unfolding"; let the worker/wrapper
	-- stuff do its thing.  INLINE things don't get w/w'd, so
	-- they will be OK.
      = --pprTrace "giving up on isWrapperFor:" (ppr PprDebug binder)
	ignominious_defeat

#if ! OMIT_DEFORESTER
	-- For the deforester: bypass the barbed wire for recursive
	-- functions that want to be inlined and are tagged deforestable
	-- by the user, allowing these things to be communicated
	-- across module boundaries.

      | is_recursive &&
	explicit_INLINE_requested &&
	deforestable binder &&
	scc_s_OK			-- hack, only get them in
					-- calc_inlinings2
      = glorious_success UnfoldAlways
#endif

      | is_recursive && not rhs_looks_like_a_data_val
	-- The only recursive defns we are prepared to tolerate at the
	-- moment is top-level very-obviously-a-data-value ones.
	-- We *need* these for dictionaries to be exported!
      = --pprTrace "giving up on rec:" (ppr PprDebug binder)
    	ignominious_defeat

	-- Not really interested unless it's exported, but doing it
	-- this way (not worrying about export-ness) gets us all the
	-- workers/specs, etc., too; which we will need for generating
	-- interfaces.  We are also not interested if this binder is
	-- in the environment we already have (perhaps from a previous
	-- run of calcInlinings -- "earlier" is presumed to mean
	-- "better").

      | explicit_INLINE_requested
      = glorious_success UnfoldAlways

      | otherwise
      = glorious_success guidance

      where
	guidance
	  = calcUnfoldingGuidance scc_s_OK max_out_threshold rhs
	  where
    	    max_out_threshold = if explicit_INLINE_requested
				then 100000 -- you asked for it, you got it
				else unfolding_creation_threshold

	guidance_size
	  = case guidance of
	      UnfoldAlways  	    	  -> 0 -- *extremely* small
	      EssentialUnfolding    	  -> 0 -- ditto
	      UnfoldIfGoodArgs _ _ _ size -> size

	guidance_says_don't = case guidance of { UnfoldNever -> True; _ -> False }

	guidance_size_too_big
	    -- Does the guidance suggest that this unfolding will
	    -- be of no use *no matter* the arguments given to it?
	    -- Could be more sophisticated...
	  = case guidance of
	      UnfoldAlways  	 -> False
	      EssentialUnfolding -> False
	      UnfoldIfGoodArgs _ no_val_args arg_info_vec size

		-> if explicit_creation_threshold then
		      False 	-- user set threshold; don't second-guess...

		   else if no_val_args == 0 && rhs_looks_like_a_data_val then
		      False	-- we'd like a top-level data constr to be
				-- visible even if it is never unfolded
		   else
		      let
			  cost
			    = leastItCouldCost con_discount_weight size no_val_args
				arg_info_vec rhs_arg_tys
		      in
--		      (if (unfold_use_threshold < cost) then (pprTrace "cost:" (ppInt cost)) else \x->x ) (
		      unfold_use_threshold < cost
--		      )


	rhs_looks_like_a_caf = not (manifestlyWHNF rhs)

	rhs_looks_like_a_data_val
	  = case (digForLambdas rhs) of
	      (_, _, [], Con _ _ _) -> True
	      other		    -> False

	rhs_arg_tys
	  = case (digForLambdas rhs) of
	      (_, _, val_binders, _) -> map idType val_binders

	(mentioned_ids, _, _, mentions_litlit)
	  = mentionedInUnfolding (\x -> x) rhs

	rhs_mentions_an_unmentionable
	  = --pprTrace "mentions:" (ppCat [ppr PprDebug binder, ppr PprDebug [(i,unfoldingUnfriendlyId i) | i <- mentioned_ids ]]) (
	    any unfoldingUnfriendlyId mentioned_ids
	    || mentions_litlit
	    --)
	    -- ToDo: probably need to chk tycons/classes...

	mentions_no_other_ids = null mentioned_ids

	explicit_INLINE_requested
	    -- did it come from a user {-# INLINE ... #-}?
	    -- (Warning: must avoid including wrappers.)
	  = idWantsToBeINLINEd binder
	    && not (rhs `isWrapperFor` binder)

	have_inlining_already = maybeToBool (lookupIdEnv inline_env binder)

	ignominious_defeat = inline_env  -- just give back what we got

	{-
	    "glorious_success" is ours if we've found a suitable unfolding.

	    But we check for a couple of fine points.

	    (1) If this Id already has an inlining in the inline_env,
		we don't automatically take it -- the earlier one is
		"likely" to be better.

		But if the new one doesn't mention any other global
		Ids, and it's pretty small (< UnfoldingOverrideThreshold),
		then we take the chance that the new one *is* better.

	    (2) If we have an Id w/ a worker/wrapper split (with
		an unfolding for the wrapper), we tend to want to keep
		it -- and *nuke* any inlining that we conjured up
		earlier.

		But, again, if this unfolding doesn't mention any
		other global Ids (and small enough), then it is
		probably better than the worker/wrappery, so we take
		it.
	-}
	glorious_success guidance
	  = let
		new_env = addOneToIdEnv inline_env binder (mkUnfolding guidance rhs)

		foldr_building = switch_is_on FoldrBuildOn
	    in
	    if (not have_inlining_already) then
		-- Not in env: we take it no matter what
		-- NB: we could check for worker/wrapper-ness,
		-- but the truth is we probably haven't run
		-- the strictness analyser yet.
		new_env

	    else if explicit_INLINE_requested then
		-- If it was a user INLINE, then we know it's already
		-- in the inline_env; we stick with what we already
		-- have.
		--pprTrace "giving up on INLINE:" (ppr PprDebug binder)
		ignominious_defeat

	    else if isWrapperId binder then
		-- It's in the env, but we have since worker-wrapperised;
		-- we either take this new one (because it's so good),
		-- or we *undo* the one in the inline_env, so the
		-- wrapper-inlining will take over.

		if mentions_no_other_ids {- *** && size <= unfold_override_threshold -} then
		    new_env
		else
		    delOneFromIdEnv inline_env binder

	    else
		-- It's in the env, nothing to do w/ worker wrapper;
		-- we'll take it if it is better.

		if not foldr_building	-- ANDY hates us... (see below)
		&& mentions_no_other_ids
		&& guidance_size <= unfold_override_threshold then
		    new_env
		else
		    --pprTrace "giving up on final hurdle:" (ppCat [ppr PprDebug binder, ppInt guidance_size, ppInt unfold_override_threshold])
		    ignominious_defeat -- and at the last hurdle, too!
\end{code}

ANDY, on the hatred of the check above; why obliterate it?  Consider

 head xs = foldr (\ x _ -> x) (_|_) xs

This then is exported via a pragma. However,
*if* you include the extra code above, you will
export the non-foldr/build version.
