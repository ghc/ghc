%
% (c) The GRASP/AQUA Project, Glasgow University, 1993-1995
%
\section[GHC_Main]{Main driver for Glasgow Haskell compiler}

\begin{code}
#include "HsVersions.h"

module Main (
#ifdef __GLASGOW_HASKELL__
	mainPrimIO
#else
	main
#endif
    ) where

import MainMonad
import CmdLineOpts

import AbsCSyn
import AbsPrel		( builtinNameInfo )
import AbsSyn
import AbsUniType	( isDataTyCon, TauType(..), UniType, TyVar, TyCon, Class )
import Bag		( emptyBag, isEmptyBag, Bag )
import CE		( CE(..), UniqFM )
import CodeGen		( codeGen )
import CoreToStg	( topCoreBindsToStg )
import Desugar		( deSugar )
import DsMonad		( DsMatchContext, DsMatchKind, pprDsWarnings )
import E		( getE_TCE, E, GVE(..) )
				-- most of above needed by mkInterface
#ifndef DPH
import Errors		( pprBagOfErrors, Error(..) )
#else
import Errors		( pprBagOfErrors, pprPodizedWarning, Error(..) )
#endif {- Data Parallel Haskell -}
import Id		( mkInstId, Id, Inst )
import Maybes		( maybeToBool, Maybe(..), MaybeErr(..) )
import MkIface		( mkInterface )
import Outputable
import PlainCore	( CoreExpr, CoreBinding, pprPlainCoreBinding,
			  PlainCoreProgram(..), PlainCoreBinding(..)
			)
import Pretty		( PprStyle(..), ppShow, ppAboves, ppAppendFile
			  IF_ATTACK_PRAGMAS(COMMA ppAbove)
			)
#ifdef USE_NEW_READER
import ReadPrefix2	( rdModule )
#else
import {-hide from mkdependHS-}
	ReadPrefix	( rdModule )
#endif
import Rename		-- renameModule ...
import SimplCore	-- core2core
import SimplStg		( stg2stg )
--ANDY: import SimplHaskell
import StgSyn		( pprPlainStgBinding, StgBinding, StgRhs, CostCentre,
			  StgBinderInfo, PlainStgProgram(..), PlainStgBinding(..)
			)
import TCE		( rngTCE, {- UNUSED: printTypeInfoForPop,-} TCE(..)
			  IF_ATTACK_PRAGMAS(COMMA eltsUFM)
			)
import Typecheck	-- typecheckModule ...
import SplitUniq
import Unique		-- lots of UniqueSupplies, etc.
import Util

#if ! OMIT_NATIVE_CODEGEN
import AsmCodeGen	( dumpRealAsm
# if __GLASGOW_HASKELL__
			  , writeRealAsm
# endif
			)
#endif

#ifdef USE_SEMANTIQUE_STRANAL
import ProgEnv		( ProgEnv(..), TreeProgEnv(..), createProgEnv )
import StrAnal		( ppShowStrAnal, OAT )
#endif
#ifdef DPH
import PodizeCore	( podizeCore , PodWarning)
import AbsCTopApal      ( nuAbsCToApal )
import NextUsed         ( pprTopNextUsedC, getTopLevelNexts, AbsCNextUsed,
                          TopAbsCNextUsed(..) , MagicId)

#endif {- Data Parallel Haskell -}
\end{code}

\begin{code}
#ifndef __GLASGOW_HASKELL__
main :: Dialogue

main = mainIOtoDialogue main_io

main_io :: MainIO ()
main_io
#else
mainPrimIO
#endif
  = BSCC("mainIO")
    BSCC("rdInput") readMn stdin ESCC	`thenMn` \ input_pgm ->
    getArgsMn			    	`thenMn` \ raw_cmd_line ->
    classifyOpts raw_cmd_line	    	`thenMn` \ cmd_line_info ->
    BSCC("doPasses")
    doIt cmd_line_info input_pgm
    ESCC ESCC
\end{code}

\begin{code}
doIt :: CmdLineInfo -> String -> MainIO ()
#ifndef DPH
doIt (switch_lookup_fn, core_cmds, stg_cmds) input_pgm
#else
doIt (switch_lookup_fn, core_cmds, podize_cmds, pcore_cmds, stg_cmds) input_pgm
#endif {- Data Parallel Haskell -}
  --
  -- Help functions and boring global variables (e.g., printing style)
  -- are figured out first; the "business end" follows, in the
  -- body of the let.
  --
  = let 
    	-- ****** help functions:

	switch_is_on switch = switchIsOn switch_lookup_fn switch
	    -- essentially, converts SwBool answer to Bool

	string_switch_is_on switch
	  = maybeToBool (stringSwitchSet switch_lookup_fn switch)

	doOutput switch io_action
	  = BSCC("doOutput")
	    case (stringSwitchSet switch_lookup_fn switch) of
	      Nothing	 -> returnMn ()
	      Just fname -> 
		fopen fname "a+"	`thenMn` \ file ->
		if (file == ``NULL'') then
		    error ("doOutput: failed to open:"++fname)
		else
		    io_action file		`thenMn` \ () ->
		    fclose file			`thenMn` \ status ->
		    if status == 0
		    then returnMn ()
		    else error ("doOutput: closed failed: "{-++show status++" "-}++fname)
	    ESCC

	doDump switch hdr string
	  = BSCC("doDump")
	    if (switch_is_on switch)
	    then writeMn stderr hdr		`thenMn_`
		 writeMn stderr ('\n': string)	`thenMn_`
		 writeMn stderr "\n"
	    else returnMn ()
	    ESCC

	-- ****** printing styles and column width:

	pprCols = (80 :: Int) -- could make configurable

	(pprStyle, pprErrorsStyle)
	  = if      switch_is_on PprStyle_All   then
		    (PprShowAll, PprShowAll)
	    else if switch_is_on PprStyle_Debug then
		    (PprDebug, PprDebug)
	    else if switch_is_on PprStyle_User  then
		    (PprForUser, PprForUser)
	    else -- defaults...
		    (PprDebug, PprForUser)

	pp_show p = ppShow {-WAS:pprCols-}10000{-random-} p
    in
    -- non-tuple-ish bindings...
 
	-- ****** possibly fiddle builtin namespaces:

    BIND (BSCC("builtinEnv") 
	  builtinNameInfo switch_is_on {-switch looker-upper-}
	  ESCC
	 )
      _TO_ (init_val_lookup_fn, init_tc_lookup_fn) ->

    -- **********************************************
    -- Welcome to the business end of the main module
    -- of the Glorious Glasgow Haskell compiler!
    -- **********************************************
#ifndef DPH
    doDump Verbose "Glasgow Haskell Compiler, version 0.26" "" `thenMn_`
#else
    doDump Verbose "Data Parallel Haskell Compiler, version 0.06 (Glasgow 0.26)" ""
	`thenMn_`
#endif {- Data Parallel Haskell -}

    -- ******* READER
#ifdef USE_NEW_READER
    BSCC("rdModule") 
    rdModule
    ESCC
	`thenMn` \ (mod_name, export_list_fns, absyn_tree) ->

    BIND (\x -> x) _TO_ bar_foo ->
    -- so BINDs and BENDs add up...
#else
    BIND BSCC("rdModule") 
	 rdModule input_pgm
	 ESCC
    _TO_ (mod_name, export_list_fns, absyn_tree) ->
#endif
    let
	-- reader things used (much?) later
	ds_mod_name = mod_name
	if_mod_name = mod_name
	co_mod_name = mod_name
	st_mod_name = mod_name
	cc_mod_name = mod_name
	-- also: export_list_fns
    in
    doDump D_dump_rif2hs "Parsed, Haskellised:" 
			 (pp_show (ppr pprStyle absyn_tree)) `thenMn_`

    -- UniqueSupplies for later use
    getSplitUniqSupplyMn 'r'	`thenMn` \ rn_uniqs ->	-- renamer
    getSplitUniqSupplyMn 't'	`thenMn` \ tc_uniqs ->	-- typechecker
    getSplitUniqSupplyMn 'd'	`thenMn` \ ds_uniqs ->	-- desugarer
    getSplitUniqSupplyMn 's'	`thenMn` \ sm_uniqs ->	-- core-to-core simplifier
    getSplitUniqSupplyMn 'C'	`thenMn` \ c2s_uniqs ->	-- core-to-stg
    getSplitUniqSupplyMn 'T'	`thenMn` \ st_uniqs ->	-- stg-to-stg passes
    getSplitUniqSupplyMn 'F'	`thenMn` \ fl_uniqs ->	-- absC flattener
    getSplitUniqSupplyMn 'P'	`thenMn` \ prof_uniqs -> -- profiling tidy-upper
    getSplitUniqSupplyMn 'L'	`thenMn` \ pre_ncg_uniqs -> -- native-code generator
    let
	ncg_uniqs = {-mkUniqueSupplyGrimily-} pre_ncg_uniqs
    in
    -- ******* RENAMER
    BIND BSCC("Renamer")
	 renameModule switch_is_on
		      (init_val_lookup_fn, init_tc_lookup_fn)
		      absyn_tree
		      rn_uniqs
	 ESCC
    _TO_ (mod4, import_names, final_name_funs, rn_errs_bag) ->
    let
	-- renamer things used (much?) later
	cc_import_names = import_names
    in

    doDump D_dump_rn4 "Renamer-pass4:"
			(pp_show (ppr pprStyle mod4))	`thenMn_`

    if (not (isEmptyBag rn_errs_bag)) then
	-- Stop right here
	writeMn stderr (ppShow pprCols (pprBagOfErrors pprErrorsStyle rn_errs_bag))
	`thenMn_` writeMn stderr "\n"
	`thenMn_` exitMn 1

    else -- No renaming errors, carry on with...

    -- ******* TYPECHECKER
    BIND (case BSCC("TypeChecker")
	       typecheckModule switch_is_on tc_uniqs final_name_funs mod4
	       ESCC
	  of
	    Succeeded stuff
		-> (emptyBag, stuff)
	    Failed tc_errs_bag
		-> (tc_errs_bag,
		    panic "main: tickled tc_results even though there were errors"))

    _TO_ (tc_errs_bag, tc_results) ->

    let
	ppr_b :: (Inst, TypecheckedExpr) -> Pretty
	ppr_b (i,e) = ppr pprStyle (VarMonoBind (mkInstId i) e)
    in
    if (not (isEmptyBag tc_errs_bag)) then
	-- Must stop *before* trying to dump tc output, because
	-- if it fails it does not give you any useful stuff back!
	writeMn stderr (ppShow pprCols (pprBagOfErrors pprErrorsStyle tc_errs_bag))
	`thenMn_` writeMn stderr "\n"
	`thenMn_` exitMn 1

    else ( -- No typechecking errors either -- so, go for broke!

    BIND tc_results
    _TO_  (typechecked_quad@(class_binds, inst_binds, val_binds, const_binds),
	   interface_stuff@(_,_,_,_,_),  -- @-pat just for strictness...
	   tycon_specs, {-UNUSED:big_env,-} this_mod_env, ddump_deriv) ->
    let
--	big_tce  = getE_TCE big_env
--	big_elts = rngTCE big_tce

	this_mod_tce  = getE_TCE this_mod_env
	this_mod_elts = rngTCE this_mod_tce
	
	local_tycons = [tc | tc <- this_mod_elts,
		 		   isLocallyDefined tc, -- from this module only
				   isDataTyCon tc ] 	-- algebraic types only
    in
--    pprTrace "Envs:" (ppAboves [
--	ppr pprStyle if_global_ids,
--	ppr pprStyle if_tce,
--	ppr pprStyle if_ce,
--	ppr pprStyle this_mod_env,
--	ppr pprStyle big_env
--	]) (

    doDump D_dump_tc "Typechecked:"
		      (pp_show
			(ppAboves [ppr pprStyle class_binds,
				   ppr pprStyle inst_binds,
				   ppAboves (map ppr_b const_binds),
				   ppr pprStyle val_binds]))    `thenMn_`

    doDump D_dump_deriv   "Derived instances:"
			  (pp_show (ddump_deriv pprStyle))	`thenMn_`

--NOT REALLY USED:
--  doDump D_dump_type_info "" (pp_show (printTypeInfoForPop big_tce)) `thenMn_`
    -- ******* DESUGARER
    let
	(desugared,ds_warnings)
	  = BSCC("DeSugarer")
	    deSugar ds_uniqs switch_lookup_fn ds_mod_name typechecked_quad
	    ESCC
    in
    (if isEmptyBag ds_warnings then
	returnMn ()
     else
	writeMn stderr (ppShow pprCols (pprDsWarnings pprErrorsStyle ds_warnings))
	`thenMn_` writeMn stderr "\n"
    ) `thenMn_`

    doDump D_dump_ds "Desugared:" (pp_show (ppAboves
			(map (pprPlainCoreBinding pprStyle) desugared)))   `thenMn_`

    -- ******* CORE-TO-CORE SIMPLIFICATION (NB: I/O op)
    core2core core_cmds switch_lookup_fn co_mod_name pprStyle
	      sm_uniqs local_tycons tycon_specs desugared
		`thenMn` \ (simplified, inlinings_env,
			    SpecData _ _ _ gen_tycons all_tycon_specs
			             spec_errs spec_warn spec_tyerrs) ->

    doDump D_dump_simpl "Simplified:" (pp_show (ppAboves
			(map (pprPlainCoreBinding pprStyle) simplified)))   `thenMn_`

-- ANDY:
--  doDump D_dump_core_passes_info "(Haskell) Simplified:" 
--			(coreToHaskell simplified)			    `thenMn_`

#ifdef DPH
    -- ******* PODIZE (VECTORIZE) THE CORE PROGRAM	
    let
        (warn,podized) = BSCC("PodizeCore")
			 podizeCore podize_cmds switch_is_on
				    uniqSupply_p simplified
			 ESCC
    in
    (if (not (null warn))
     then writeMn stderr "\n"						    `thenMn_`
	  writeMn stderr (ppShow pprCols (ppAboves
                    (map (\w -> pprPodizedWarning w pprErrorsStyle) warn))) `thenMn_`
	  writeMn stderr "\n"
     else returnMn ())							    `thenMn_`
           
    doDump D_dump_pod   "Podization:" (pp_show (ppAboves
		     (map (pprPlainCoreBinding pprStyle) podized)))	    `thenMn_`

    -- ******** CORE-TO-CORE SIMPLIFICATION OF PODIZED PROGRAM
    let 
    	psimplified = BSCC("PodizeCore2Core")
		      core2core pcore_cmds switch_is_on pprStyle
				uniqSupply_S podized
		      ESCC
    in
    doDump D_dump_psimpl "Par Simplified:" (pp_show (ppAboves
			(map (pprPlainCoreBinding pprStyle) psimplified)))  `thenMn_`

#endif {- Data Parallel Haskell -}

#ifdef USE_SEMANTIQUE_STRANAL
    -- ******* SEMANTIQUE STRICTNESS ANALYSER
    doDump D_dump_stranal_sem "Strictness:" (ppShowStrAnal simplified big_env) `thenMn_`
#endif

    -- ******* STG-TO-STG SIMPLIFICATION
    let
#ifndef DPH
	stg_binds   = BSCC("Core2Stg")
		      topCoreBindsToStg c2s_uniqs simplified
		      ESCC
#else
	stg_binds   = BSCC("Core2Stg")
		      topCoreBindsToStg c2s_uniqs psimplified
		      ESCC
#endif {- Data Parallel Haskell -}
    in

    stg2stg stg_cmds switch_lookup_fn st_mod_name pprStyle st_uniqs stg_binds
			`thenMn` \ (stg_binds2, cost_centre_info) ->

    doDump D_dump_stg "STG syntax:" (pp_show (ppAboves
		      (map (pprPlainStgBinding pprStyle) stg_binds2)))	`thenMn_`

    -- ******* INTERFACE GENERATION (needs STG output)
{-  let
	mod_name = "_TestName_"
    	export_list_fns = (\ x -> False, \ x -> False)
	inlinings_env = nullIdEnv
	fixities = []
	if_global_ids = []
	if_ce = nullCE
	if_tce = nullTCE
	if_inst_info = emptyBag
    in
-}  let
	mod_interface
    	  = BSCC("MkInterface")
	    mkInterface switch_is_on if_mod_name export_list_fns
			inlinings_env all_tycon_specs
			interface_stuff
			stg_binds2
	    ESCC
    in
    doOutput ProduceHi BSCC("PrintInterface")
		       ( \ file ->
		         ppAppendFile file 1000{-pprCols-} mod_interface )
		       ESCC 						`thenMn_`

    -- ******* "ABSTRACT", THEN "FLAT", THEN *REAL* C!
    let
	abstractC      = BSCC("CodeGen")
		         codeGen cc_mod_name     -- module name for CC labelling
				 cost_centre_info
				 cc_import_names -- import names for CC registering
				 switch_lookup_fn
				 gen_tycons	 -- type constructors generated locally
				 all_tycon_specs -- tycon specialisations
				 stg_binds2
		         ESCC

    	flat_abstractC = BSCC("FlattenAbsC")
			 flattenAbsC fl_uniqs abstractC
		         ESCC
    in
    doDump D_dump_absC  "Abstract C:" (dumpRealC switch_is_on abstractC)   `thenMn_`

    doDump D_dump_flatC "Flat Abstract C:" (dumpRealC switch_is_on flat_abstractC) `thenMn_`

    -- You can have C (c_output) or assembly-language (ncg_output),
    -- but not both.  [Allowing for both gives a space leak on
    -- flat_abstractC.  WDP 94/10]
    let
	(flat_absC_c, flat_absC_ncg) =
	   case (string_switch_is_on ProduceC || switch_is_on D_dump_realC,
		 string_switch_is_on ProduceS || switch_is_on D_dump_asm) of
	     (True,  False) -> (flat_abstractC, AbsCNop)
	     (False, True)  -> (AbsCNop, flat_abstractC)
	     (False, False) -> (AbsCNop, AbsCNop)
	     (True,  True)  -> error "ERROR: Can't do both .hc and .s at the same time"

	c_output_d = BSCC("PrintRealC")
		     dumpRealC switch_is_on flat_absC_c
    		     ESCC

#ifdef __GLASGOW_HASKELL__
	c_output_w = BSCC("PrintRealC")
		     (\ f -> writeRealC switch_is_on f flat_absC_c)
    		     ESCC
#else
	c_output_w = c_output_d
#endif

#if OMIT_NATIVE_CODEGEN
	ncg_output_d
	  = error "*** GHC not built with a native-code generator ***"
	ncg_output_w = ncg_output_d
#else
	ncg_output_d = BSCC("nativeCode")
		     dumpRealAsm switch_lookup_fn flat_absC_ncg ncg_uniqs
    		     ESCC

#ifdef __GLASGOW_HASKELL__
	ncg_output_w = BSCC("nativeCode")
		     (\ f -> writeRealAsm switch_lookup_fn f flat_absC_ncg ncg_uniqs)
    		     ESCC
#else
	ncg_output_w = ncg_output_d
#endif
#endif
    in
    doDump D_dump_asm "" ncg_output_d `thenMn_`
    doOutput ProduceS    ncg_output_w `thenMn_`

#ifndef DPH
    -- ********* GHC Finished !!!!
    doDump D_dump_realC "" c_output_d `thenMn_`
    doOutput ProduceC 	   c_output_w `thenMn_`

#else
    -- ********* DPH needs native code generator, nearly finished.....
    let 
	next_used_flatC = getTopLevelNexts flat_abstractC []
	apal_module     = nuAbsCToApal uniqSupply_L mod_name next_used_flatC
    in
    doDump D_dump_nextC "Next Used annotated C:" (ppShow pprCols 
				(pprTopNextUsedC next_used_flatC))	    `thenMn_`
    doOutput ProduceC 	("! /* DAP assembler (APAL): */\n"++apal_module)    `thenMn_`

#endif {- Data Parallel Haskell -}
    exitMn 0
    {-)-} BEND ) BEND BEND BEND BEND
\end{code}
