%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1995
%
\section[TcMonoBinds]{TcMonoBinds}

\begin{code}
#include "HsVersions.h"

module TcMonoBnds ( tcMonoBinds ) where

import TcMonad		-- typechecking monad machinery
import AbsSyn		-- the stuff being typechecked

import AbsPrel		( mkPrimIoTy, unitTy, mkListTy, mkFunTy )
import AbsUniType	( applyNonSynTyCon, applySynTyCon )
import CmdLineOpts	( GlobalSwitch(..) )
import E		( growE_LVE, lookupE_Binder, getE_TCE, E, GVE(..), LVE(..) )
#if USE_ATTACK_PRAGMAS
import CE
#endif
import TCE
import Errors		( UnifyErrContext(..) ) -- notably PatMonoBindsCtxt
import Id		( getIdUniType, Id )
import LIE		( nullLIE, plusLIE, LIE )
import NameTypes	( FullName )
import TcGRHSs		( tcGRHSsAndBinds )
import TcMatches	( tcMatchesFun )
import TcPat		( tcPat )
import Unify		( unifyTauTy )
import Unique		( dialogueTyConKey, iOTyConKey )
import Util
\end{code}

\begin{code}
tcMonoBinds :: E -> RenamedMonoBinds -> TcM (TypecheckedMonoBinds, LIE)

tcMonoBinds e EmptyMonoBinds = returnTc (EmptyMonoBinds, nullLIE)

tcMonoBinds e (AndMonoBinds mb1 mb2)
  = tcMonoBinds e mb1		`thenTc` \ (mb1a, lie1) ->
    tcMonoBinds e mb2		`thenTc` \ (mb2a, lie2) ->
    returnTc (AndMonoBinds mb1a mb2a, plusLIE lie1 lie2)

tcMonoBinds e (PatMonoBind pat grhss_and_binds locn)
	 -- much like tcMatches of GRHSMatch
  = addSrcLocTc locn		 (

	 -- LEFT HAND SIDE
    tcPat e pat	     	`thenTc` \ (pat2, lie_pat, pat_ty) ->

	 -- BINDINGS AND THEN GRHSS
    tcGRHSsAndBinds e grhss_and_binds `thenTc` \ (grhss_and_binds2, lie, grhss_ty) ->

    unifyTauTy pat_ty grhss_ty (PatMonoBindsCtxt pat grhss_and_binds) `thenTc_`

    (case pat of
      VarPatIn fun -> chk_main_or_mainIOish_type e fun pat_ty
      _		   -> returnTc (panic "chk_main_or_mainIOish_type (pat)")
    )				      `thenTc_`

	-- Check for primitive types in the pattern (no can do)
{- does not work here
    checkTc (any_con_w_prim_arg pat2)
	    (error "Can't have primitive type in a pattern binding") `thenTc_`
-}

	-- RETURN
    returnTc (PatMonoBind pat2 grhss_and_binds2 locn,
	      plusLIE lie_pat lie)
    )

tcMonoBinds e (FunMonoBind name matches locn)
  = addSrcLocTc locn			(
    let  id = lookupE_Binder e name  in

    tcMatchesFun e name (getIdUniType id) matches   `thenTc` \ (matches', lie) ->

    chk_main_or_mainIOish_type e name (getIdUniType id)  `thenTc_`

    returnTc (FunMonoBind id matches' locn, lie)
    )

chk_main_or_mainIOish_type :: E -> Name -> UniType -> TcM ()

    -- profoundly ugly checking that ...
    --	Main.main	:: Dialogue -- Haskell 1.2
    --  Main.main	:: IO ()    -- Haskell 1.3
    --  Main.mainPrimIO :: PrimIO () -- Glasgow extension

chk_main_or_mainIOish_type e name chk_ty
  = getSwitchCheckerTc	`thenNF_Tc` \ sw_chkr ->
    let
	tce	    = getE_TCE e
	haskell_1_3 = sw_chkr Haskell_1_3

{-OLD:	response_tc = lookupTCE tce (PreludeTyCon responseTyConKey bottom 0 True)
	request_tc  = lookupTCE tce (PreludeTyCon requestTyConKey  bottom 0 True)
	response_ty = applyNonSynTyCon response_tc []
	request_ty  = applyNonSynTyCon request_tc  []
	dialogue_ty = (mkListTy response_ty) `mkFunTy` (mkListTy request_ty)
-}
	dialogue_tc = lookupTCE tce (PreludeTyCon dialogueTyConKey bottom 0 False)
	dialogue_ty = applySynTyCon dialogue_tc []

	io_tc	    = lookupTCE tce (PreludeTyCon iOTyConKey bottom 1 False)
	io_tup0_ty  = applySynTyCon io_tc [unitTy]

	bottom	    = panic "chk_main_or..."
    in
    if is_a_particular_thing SLIT("Main") SLIT("main") name then
	if haskell_1_3 then
	    unifyTauTy io_tup0_ty  chk_ty (MatchCtxt io_tup0_ty  chk_ty)
	else
	    unifyTauTy dialogue_ty chk_ty (MatchCtxt dialogue_ty chk_ty)

    else if is_a_particular_thing SLIT("Main") SLIT("mainPrimIO") name then
       let
	    ioprim_ty = mkPrimIoTy unitTy
       in
       unifyTauTy ioprim_ty chk_ty (MatchCtxt ioprim_ty chk_ty)
    else
       returnTc bottom
  where
    is_a_particular_thing :: FAST_STRING -> FAST_STRING -> Name -> Bool

    is_a_particular_thing mod_wanted nm_wanted (OtherTopId _ full_name)
      = let (mod, nm) = getOrigName full_name
        in  mod == mod_wanted && nm == nm_wanted
    is_a_particular_thing _ _ _ = False
\end{code}
