%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1995
%
\section[TcMonoType]{Typechecking user-specified @MonoTypes@}

\begin{code}
#include "HsVersions.h"

module TcMonoType ( tcMonoType, tcInstanceType ) where

IMPORT_Trace		-- ToDo: rm (debugging)
import Outputable
import Pretty

import TcMonad		-- typechecking monad machinery
import AbsSyn		-- the stuff being typechecked

#ifndef DPH
import AbsPrel		( mkListTy, mkTupleTy, mkFunTy )
#else
import AbsPrel		( mkListTy, mkTupleTy, mkFunTy, mkProcessorTy, mkPodTy )
#endif {- Data Parallel Haskell -}
import AbsUniType	( applySynTyCon, applyNonSynTyCon, mkDictTy,
			  getTyConArity, isSynTyCon, isTyVarTemplateTy,
			  getUniDataTyCon_maybe, maybeUnpackFunTy
			  IF_ATTACK_PRAGMAS(COMMA pprTyCon COMMA pprUniType)
			  IF_ATTACK_PRAGMAS(COMMA cmpUniType)
			)
import UniType		( UniType(..) ) -- ******** CHEATING **** could be undone
import TyCon		--( TyCon(..) ) -- ditto, only more so

import CE		( lookupCE, CE(..) )
import CmdLineOpts	( GlobalSwitch(..) )
import Errors		( confusedNameErr, tyConArityErr, instTypeErr,
			  Error(..)
			)
import Maybes		( Maybe(..) )
import TcPolyType	( tcPolyType )
import TCE		( lookupTCE, TCE(..), UniqFM )
import TVE		( lookupTVE, TVE(..) )
import Util
\end{code}

\begin{code}
tcMonoType :: CE -> TCE -> TVE -> RenamedMonoType -> Baby_TcM UniType

tcMonoType rec_ce rec_tce tve (MonoTyVar name)
  = returnB_Tc (lookupTVE tve name)

tcMonoType rec_ce rec_tce tve (ListMonoTy ty)
  = tcMonoType	rec_ce rec_tce tve ty	`thenB_Tc` \ tau_ty ->
    returnB_Tc (mkListTy tau_ty)

tcMonoType rec_ce rec_tce tve (TupleMonoTy tys)
  = mapB_Tc (tcPolyType rec_ce rec_tce tve) tys	`thenB_Tc` \ tau_tys ->
    returnB_Tc (mkTupleTy (length tau_tys) tau_tys)

tcMonoType rec_ce rec_tce tve (FunMonoTy ty1 ty2)
  = tcMonoType rec_ce rec_tce tve ty1	`thenB_Tc` \ tau_ty1 ->
    tcMonoType rec_ce rec_tce tve ty2	`thenB_Tc` \ tau_ty2 ->
    returnB_Tc (mkFunTy tau_ty1 tau_ty2)

tcMonoType rec_ce rec_tce tve (MonoTyCon name@(WiredInTyCon tycon) tys)
  = let 
	arity	     = getTyConArity tycon
	is_syn_tycon = isSynTyCon tycon
    in
    tcMonoType_help rec_ce rec_tce tve name tycon arity is_syn_tycon tys

tcMonoType rec_ce rec_tce tve (MonoTyCon name@(PreludeTyCon _ _ arity is_data_tycon) tys)
  = tcMonoType_help rec_ce rec_tce tve name
		    (lookupTCE rec_tce name)
		    arity (not is_data_tycon) tys


tcMonoType rec_ce rec_tce tve (MonoTyCon name@(OtherTyCon _ _ arity is_data_tycon _) tys)
  = tcMonoType_help rec_ce rec_tce tve name
		    (lookupTCE rec_tce name)
		    arity (not is_data_tycon) tys

tcMonoType rec_ce rec_tce tve (MonoTyCon bad_name tys)
  = getSrcLocB_Tc		`thenB_Tc` \ locn ->
    failB_Tc (confusedNameErr
		"Bad name for a type constructor (a class, or a Prelude name?)"
		bad_name locn)

-- two for unfoldings only:
tcMonoType rec_ce rec_tce tve (MonoDict c ty)
  = tcMonoType rec_ce rec_tce tve ty	`thenB_Tc` \ new_ty ->
    let
	clas = lookupCE rec_ce c
    in
    returnB_Tc (mkDictTy clas new_ty)

tcMonoType rec_ce rec_tce tve (MonoTyVarTemplate tv_tmpl)
  = returnB_Tc (lookupTVE tve tv_tmpl)

#ifdef DPH
tcMonoType ce tce tve (MonoTyProc tys ty)
  = tcMonoTypes ce tce tve tys	`thenB_Tc` \ tau_tys ->
    tcMonoType	ce tce tve ty	`thenB_Tc` \ tau_ty  ->
    returnB_Tc (mkProcessorTy tau_tys tau_ty)

tcMonoType ce tce tve (MonoTyPod ty)
  = tcMonoType ce tce tve ty	`thenB_Tc` \ tau_ty  ->
    returnB_Tc (mkPodTy tau_ty)
#endif {- Data Parallel Haskell -}

#ifdef DEBUG
tcMonoType rec_ce rec_tce tve bad_ty
  = pprPanic "tcMonoType:" (ppr PprShowAll bad_ty)
#endif
\end{code}

\begin{code}
tcMonoType_help rec_ce rec_tce tve name tycon arity is_syn_tycon tys
  = tcMonoTypes rec_ce rec_tce tve tys	`thenB_Tc`    \ tau_tys ->
    let	 cur_arity = length tys	 in
    getSrcLocB_Tc			`thenB_Tc` \ loc ->

    checkB_Tc (arity /= cur_arity)
	   (tyConArityErr name arity cur_arity loc) `thenB_Tc_`

    returnB_Tc (if is_syn_tycon then
		 applySynTyCon	tycon tau_tys
	      else
		 applyNonSynTyCon tycon tau_tys)

-- also not exported
tcMonoTypes rec_ce rec_tce tve monotypes
   = mapB_Tc (tcMonoType rec_ce rec_tce tve) monotypes
\end{code}

@tcInstanceType@ checks the type {\em and} its syntactic constraints:
it must normally look like: @instance Foo (Tycon a b c ...) ...@
(We're checking the @Tycon a b c ...@ part here...)

The exceptions to this syntactic checking: (1)~if the @GlasgowExts@
flag is on, or (2)~the instance is imported (they must have been
compiled elsewhere).  In these cases, we let them go through anyway.

We can also have instances for functions: @instance Foo (a -> b) ...@.

\begin{code}
tcInstanceType :: CE -> TCE -> TVE
	       -> Bool{-True <=> from this module-} -> SrcLoc
	       -> RenamedMonoType
	       -> Baby_TcM UniType

tcInstanceType ce tce tve from_here locn mono_ty
  = tcMonoType ce tce tve mono_ty	`thenB_Tc` \ tau_ty  ->
    let
	(naughty, unkosher) = bad_shape tau_ty
    in
    getSwitchCheckerB_Tc		`thenB_Tc` \ sw_chkr ->
    checkB_Tc
	(if not from_here || sw_chkr GlasgowExts then -- no "shape" checking
	    naughty
	 else
	    naughty || unkosher
	)
	(instTypeErr tau_ty locn)	`thenB_Tc_`
    returnB_Tc tau_ty
  where
    -- "naughty" if the type is really unacceptable, no
    -- matter what (e.g., a type synonym); "unkosher" if
    -- the Haskell report forbids it, but we allow it through
    -- under -fglasgow-exts.

    bad_shape ty
      = if (is_syn_type ty) then
	   (True, bottom)
	else case (getUniDataTyCon_maybe ty) of
	  Just (_,tys,_) -> (False, not (all isTyVarTemplateTy tys))
	  Nothing        -> case maybeUnpackFunTy ty of
	   		      Just (t1, t2) -> (False,
						not (all isTyVarTemplateTy [t1, t2]))
			      Nothing       -> (True, bottom)
      where
	bottom = panic "bad_shape"

	is_syn_type ty -- ToDo: move to AbsUniType (or friend)?
	  = case ty of
	      UniSyn _ _ _ -> True
	      _ -> False
\end{code}
