%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1995
%
\section[TcMonad]{@TcMonad@: monad machinery for the typechecker}

\begin{code}
#include "HsVersions.h"

module TcMonad (
	TcM(..), TcResult{-abstract-},
	thenTc, thenTc_, returnTc, failTc, checkTc,
	listTc, mapTc, mapAndUnzipTc,
	fixTc, foldlTc, initTc,
	recoverTc, recoverQuietlyTc,

	NF_TcM(..),
	thenNF_Tc, thenLazilyNF_Tc, returnNF_Tc, listNF_Tc, mapNF_Tc,
	fixNF_Tc, noFailTc,

	Baby_TcM(..), Baby_TcResult{-abstract-},
	returnB_Tc, thenB_Tc, thenB_Tc_,
	failB_Tc, recoverIgnoreErrorsB_Tc,
	fixB_Tc, mapB_Tc,
	babyTcMtoTcM, babyTcMtoNF_TcM,
	getUniqueB_Tc, getUniquesB_Tc,
	addSrcLocB_Tc, getSrcLocB_Tc,
	getSwitchCheckerB_Tc, checkB_Tc,
	uniqSMtoBabyTcM,

	getSwitchCheckerTc,
	getDefaultingTys, setDefaultingTys,
	getUniquesTc, getUniqueTc,
	rn4MtoTcM,

	getTyVarUniquesTc, getTyVarUniqueTc,

	applyTcSubstToTy, applyTcSubstToTys,
--UNUSED:	applyTcSubstToThetaTy,
	applyTcSubstToTyVar, applyTcSubstToTyVars,
	applyTcSubstToId,
	applyTcSubstToInst, applyTcSubstToInsts,
	extendSubstTc, pruneSubstTc,

	addSrcLocTc, getSrcLocTc,
	checkMaybeTc,	 checkMaybesTc,
	checkMaybeErrTc, -- UNUSED: checkMaybeErrsTc,

	lookupInst_Tc, lookupNoBindInst_Tc,

	-- and to make the interface self-sufficient ...
	UniqueSupply, SplitUniqSupply,
	Bag, Maybe, MaybeErr, Error(..), PprStyle, Pretty(..),
	PrettyRep, SrcLoc, Subst, TyVar, TyVarTemplate, TyCon,
	Class, UniType, TauType(..), ThetaType(..), SigmaType(..),
	UnifyErrContext, Unique, Expr,
	TypecheckedExpr(..), TypecheckedPat, Id, IdInfo, Inst,
	GlobalSwitch, SUniqSM(..), Rn4M(..), GlobalNameFuns(..),
	GlobalNameFun(..), Name, ProtoName

	IF_ATTACK_PRAGMAS(COMMA getSUnique COMMA getSUniques)
	IF_ATTACK_PRAGMAS(COMMA splitUniqSupply COMMA mkUniqueGrimily)
	IF_ATTACK_PRAGMAS(COMMA applySubstToId)
	IF_ATTACK_PRAGMAS(COMMA applySubstToInst)
	IF_ATTACK_PRAGMAS(COMMA applySubstToThetaTy)
	IF_ATTACK_PRAGMAS(COMMA applySubstToTy)
	IF_ATTACK_PRAGMAS(COMMA applySubstToTyVar)
    ) where

import AbsSyn
import AbsUniType	( TyVar, TyVarTemplate, TyCon, Class, UniType,
			  TauType(..), ThetaType(..), SigmaType(..)
			  IF_ATTACK_PRAGMAS(COMMA cmpUniType)
			)
import Bag		( Bag, snocBag, emptyBag, isEmptyBag )
import CmdLineOpts	( GlobalSwitch )
import Errors		( noInstanceErr, unifyErr, pprBagOfErrors,
			  Error(..), UnifyErrInfo(..), UnifyErrContext(..)
			)
import FiniteMap	( emptyFM, FiniteMap )
import Id		( applySubstToId )
import Inst		( applySubstToInst )
import InstEnv		( lookupInst, lookupNoBindInst, Inst )
import Maybes		( Maybe(..), MaybeErr(..) )
import Pretty
import RenameMonad4	( Rn4M(..), GlobalNameFuns(..), GlobalNameFun(..) )
import SrcLoc		( mkUnknownSrcLoc )
import Subst
import Unify
import SplitUniq
import Unique
import Util

infixr 9 `thenTc`, `thenTc_`, `thenNF_Tc`, `thenLazilyNF_Tc`
\end{code}

%************************************************************************
%*									*
\subsection[TcM-TcM]{Plain @TcM@ monadery}
%*									*
%************************************************************************

The following @TcM@ is of the garden variety which can fail, and does
as soon as possible.

\begin{code}
-- internal use only...
type InTcM output
	=  (GlobalSwitch -> Bool)	-- so we can chk cmd-line switches
	-> [UniType]			-- types used for defaulting; down only
	-> Subst			-- substitution; threaded
	-> SplitUniqSupply		-- threaded
	-> Bag Error			-- threaded
	-> SrcLoc			-- only passed downwards
	-> output

data TcResult result
  = TcSucceeded result
		Subst
		(Bag Error)
  | TcFailed	Subst
		(Bag Error)

type TcM result
	= InTcM (TcResult result)

#ifdef __GLASGOW_HASKELL__
{-# INLINE thenTc #-}
{-# INLINE thenTc_ #-}
{-# INLINE returnTc #-}
#endif

thenTc  :: TcM a -> (a -> TcM b) -> TcM b
thenTc_ :: TcM a -> TcM b -> TcM b

thenTc expr cont sw_chkr dtys subst us errs src_loc
  = case splitUniqSupply us	    of { (s1, s2) ->
    case (expr sw_chkr dtys subst s1 errs src_loc) of
      TcFailed subst errs -> TcFailed subst errs
      TcSucceeded result subst2 errs2
	-> cont result sw_chkr dtys subst2 s2 errs2 src_loc
    }

thenTc_ expr cont sw_chkr dtys subst us errs src_loc
  = case splitUniqSupply us	    of { (s1, s2) ->
    case (expr sw_chkr dtys subst s1 errs src_loc) of
      TcFailed subst errs -> TcFailed subst errs
      TcSucceeded _ subst2 errs2
	-> cont sw_chkr dtys subst2 s2 errs2 src_loc
    }

returnTc :: a -> TcM a
returnTc result sw_chkr dtys subst us errs src_loc
  = TcSucceeded result subst errs

failTc err sw_chkr dtys subst us errs src_loc
  = TcFailed subst (errs `snocBag` err)
\end{code}

@recoverTc@ recovers from an error, by providing a value to use
instead.  It is also lazy, in that it always succeeds immediately; the
thing inside is only even looked at when you pull on the errors, or on
the value returned.

@recoverQuietlyTc@ doesn't even report the errors found---it is used
when looking at pragmas.

\begin{code}
recoverTc, recoverQuietlyTc :: a -> TcM a -> NF_TcM a

recoverTc use_this_if_err expr sw_chkr dtys subst uniqs_in errs_in src_loc
  = case (expr sw_chkr dtys (pushSubstUndos subst) uniqs_in errs_in src_loc) of
      TcSucceeded result subst_out errs_out -> 
	(result, combineSubstUndos subst_out, errs_out)

      TcFailed subst_out errs_out ->
	(use_this_if_err, undoSubstUndos subst_out, errs_out)
	  -- Note that we return the *undone* substitution
	  -- and the *incoming* UniqueSupply

recoverQuietlyTc use_this_if_err expr sw_chkr dtys subst uniqs_in errs_in src_loc
  = (r2, s2, e2)
  where
    (r2, s2, e2)
      = case (expr sw_chkr dtys (pushSubstUndos subst) uniqs_in errs_in src_loc) of
          TcSucceeded result subst_out errs_out -> 
	    (result, combineSubstUndos subst_out, errs_out)

          TcFailed subst_out errs_out ->
	    (use_this_if_err, undoSubstUndos subst_out, errs_in)
	  -- Note that we return the *undone* substitution,
	  -- the *incoming* UniqueSupply, and the *incoming* errors
\end{code}

The following @TcM@ checks a condition and fails with the given error
message.

\begin{code}
checkTc :: Bool -> Error -> TcM ()

checkTc True  err = failTc err
checkTc False err = returnTc ()

listTc :: [TcM a] -> TcM [a]

listTc [] = returnTc []
listTc (x:xs)
 = x		`thenTc` \ r ->
   listTc xs	`thenTc` \ rs ->
   returnTc (r:rs)

mapTc :: (a -> TcM b) -> [a] -> TcM [b]
mapTc f [] = returnTc []
mapTc f (x:xs)
 = f x		`thenTc` \ r ->
   mapTc f xs	`thenTc` \ rs ->
   returnTc (r:rs)

mapAndUnzipTc :: (a -> TcM (b, c)) -> [a] -> TcM ([b], [c])

mapAndUnzipTc f [] = returnTc ([], [])
mapAndUnzipTc f (x:xs)
 = f x			`thenTc` \ (r1,  r2)  ->
   mapAndUnzipTc f xs	`thenTc` \ (rs1, rs2) ->
   returnTc (r1:rs1, r2:rs2)

foldlTc :: (a -> b -> TcM a) -> a -> [b] -> TcM a
foldlTc f a []	   = returnTc a
foldlTc f a (b:bs) = f a b	`thenTc` \ a2 ->
		     foldlTc f a2 bs

fixTc :: (x -> TcM x) -> TcM x
fixTc m sw_chkr dtys subst us errs src_loc
  = lim
  where
    lim    = m result sw_chkr dtys subst us errs src_loc
    result = case lim of
	       TcSucceeded result _ _ -> result
#ifdef DEBUG
	       TcFailed _ errs -> pprPanic "Failed in fixTc:\n" (pprBagOfErrors PprDebug errs)
#endif
\end{code}

And the machinery to start things up:

\begin{code}
aRRAY_SIZE :: Int
aRRAY_SIZE  = 511

initTc	:: (GlobalSwitch -> Bool)
	-> SplitUniqSupply
	-> TcM result
	-> MaybeErr result (Bag Error)

initTc sw_chkr us tc
  = case (tc sw_chkr [{-no defaults-}] init_subst us emptyBag mkUnknownSrcLoc) of
      TcFailed _ errs -> Failed errs
      TcSucceeded result subst2 errs
	-> if isEmptyBag errs then
	      Succeeded result
	   else
	      Failed errs

init_subst = mkEmptySubst aRRAY_SIZE -- out here to avoid initTc CAF...sigh
\end{code}


%************************************************************************
%*									*
\subsection[TcM-NF_TcM]{No-fail @NF_TcM@ monadery}
%*									*
%************************************************************************

This is a no-fail version of a TcM.

\begin{code}
-- ToDo: re-order fields to match TcM?
type NF_TcM result = InTcM (result, Subst, Bag Error)

#ifdef __GLASGOW_HASKELL__
{-# INLINE thenNF_Tc #-}
{-# INLINE thenLazilyNF_Tc #-}
{-# INLINE returnNF_Tc #-}
#endif

thenNF_Tc, thenLazilyNF_Tc :: NF_TcM a -> (a -> InTcM b) -> InTcM b
-- ...Lazily... is purely a performance thing (WDP 95/09)
\end{code}

In particular, @thenNF_Tc@ has all of these types:
\begin{pseudocode}
thenNF_Tc :: NF_TcM a -> (a -> TcM b)	 -> TcM b
thenNF_Tc :: NF_TcM a -> (a -> NF_TcM b) -> NF_TcM b
\end{pseudocode}

\begin{code}
thenNF_Tc expr cont sw_chkr dtys subst us errs src_loc
  = case splitUniqSupply us	    of { (s1, s2) ->
    case (expr sw_chkr dtys subst s1 errs src_loc) of
     (result, subst2, errs2)
       -> cont result sw_chkr dtys subst2 s2 errs2 src_loc
    }

thenLazilyNF_Tc expr cont sw_chkr dtys subst us errs src_loc
  = let
	(s1, s2) = splitUniqSupply us
    in
    case (expr sw_chkr dtys subst s1 errs src_loc) of {
     (result, subst2, errs2)
       -> cont result sw_chkr dtys subst2 s2 errs2 src_loc
    }

returnNF_Tc :: a -> NF_TcM a
returnNF_Tc result sw_chkr dtys subst us errs src_loc
  = (result, subst, errs)

listNF_Tc :: [NF_TcM a] -> NF_TcM [a]
listNF_Tc [] = returnNF_Tc []
listNF_Tc (x:xs)
  = x			`thenNF_Tc` \ r ->
    listNF_Tc xs	`thenNF_Tc` \ rs ->
    returnNF_Tc (r:rs)

mapNF_Tc :: (a -> NF_TcM b) -> [a] -> NF_TcM [b]
mapNF_Tc f [] = returnNF_Tc []
mapNF_Tc f (x:xs)
  = f x			`thenNF_Tc` \ r ->
    mapNF_Tc f xs	`thenNF_Tc` \ rs ->
    returnNF_Tc (r:rs)

fixNF_Tc :: (a -> NF_TcM a) -> NF_TcM a
fixNF_Tc m sw_chkr dtys subst us errs src_loc
  = lim
  where
    lim = m result sw_chkr dtys subst us errs src_loc
    (result, _, _) = lim
\end{code}

@noFailTc@ takes a \tr{TcM a} and returns a \tr{NF_TcM a}.  You use it
when you are darn sure that the TcM won't actually fail!

\begin{code}
noFailTc :: TcM a -> NF_TcM a

noFailTc expr sw_chkr dtys subst us errs src_loc
  = case (expr sw_chkr dtys subst us errs src_loc) of
      TcFailed _ _ -> panic "Failure in noFailTc!"
      TcSucceeded result subst errs
    	-> (result, subst, errs)
\end{code}

%************************************************************************
%*									*
\subsection[TcM-uniq-extract]{Extractings Uniques from the monad}
%*									*
%************************************************************************

These functions extract uniques from the monad. There are two unique
supplies embedded in the monad.
\begin{itemize}
\item
normal unique supply
\item
special unique supply for TyVars (these index the substitution)
\end{itemize}

\begin{code}
getUniquesTc :: Int -> NF_TcM [Unique]
getUniquesTc n sw_chkr dtys subst us errs src_loc
  = case (getSUniques n us) of { uniques ->
    (uniques, subst, errs) }

-- This simpler version is often adequate:

getUniqueTc :: NF_TcM Unique
getUniqueTc sw_chkr dtys subst us errs src_loc
  = case (getSUnique us) of { unique ->
    (unique, subst, errs) }

rn4MtoTcM :: GlobalNameFuns -> Rn4M a -> NF_TcM (a, Bag Error)

rn4MtoTcM name_funs rn_action sw_chkr dtys subst us errs src_loc
  = let
	(rn_result, rn_errs)
	  = rn_action sw_chkr name_funs emptyFM emptyBag us mkUnknownSrcLoc
	    -- laziness may be good for you (see below)
    in
    ((rn_result, rn_errs), subst, errs)

-- Special uniques for TyVars extracted from the substitution

getTyVarUniquesTc :: Int -> NF_TcM [Unique]
getTyVarUniquesTc n sw_chkr dtys subst us errs src_loc
  = returnNF_Tc uniques sw_chkr dtys subst2 us errs src_loc
  where
    (subst2, uniques) = getSubstTyVarUniques n subst

getTyVarUniqueTc :: NF_TcM Unique
getTyVarUniqueTc sw_chkr dtys subst us errs src_loc
  = returnNF_Tc unique sw_chkr dtys subst2 us errs src_loc
  where
    (subst2, unique) = getSubstTyVarUnique subst
\end{code}

%************************************************************************
%*									*
\subsection[TcM-extract]{Extractings other things from the monad}
%*									*
%************************************************************************

These are functions which extract things from the monad.

Extending and applying the substitution.

ToDo: Unify.lhs BackSubst.lhs Id.lhs Inst.lhs: The TcMonad is used in
a number of places where only the sequenced substitution is required.
A lighter weight sequence substitution monad would be more appropriate
with TcMonad interface functions defined here.

\begin{code}
getTcSubst  	      ::	      NF_TcM Subst
applyTcSubstToTy      :: TauType   -> NF_TcM TauType     
--UNUSED:applyTcSubstToThetaTy :: ThetaType -> NF_TcM ThetaType 
applyTcSubstToTyVar   :: TyVar     -> NF_TcM TauType
applyTcSubstToId      :: Id	   -> NF_TcM Id
applyTcSubstToInst    :: Inst	   -> NF_TcM Inst

getTcSubst sw_chkr dtys subst us errs src_loc
  = returnNF_Tc subst sw_chkr dtys subst us errs src_loc

applyTcSubstToTy ty sw_chkr dtys subst us errs src_loc
  = case (applySubstToTy subst ty) of { (subst2, new_tau_ty) ->
    returnNF_Tc new_tau_ty sw_chkr dtys subst2 us errs src_loc
    }

{- UNUSED:
applyTcSubstToThetaTy theta_ty sw_chkr dtys subst us errs src_loc
  = case (applySubstToThetaTy subst theta_ty) of { (subst2, new_theta_ty) ->
    returnNF_Tc new_theta_ty sw_chkr dtys subst2 us errs src_loc
    }
-}

applyTcSubstToTyVar tyvar sw_chkr dtys subst us errs src_loc
  = case (applySubstToTyVar subst tyvar) of { (subst2, new_tau_ty) ->
    returnNF_Tc new_tau_ty sw_chkr dtys subst2 us errs src_loc
    }

applyTcSubstToId tyvar sw_chkr dtys subst us errs src_loc
  = case (applySubstToId subst tyvar) of { (subst2, new_tau_ty) ->
    returnNF_Tc new_tau_ty sw_chkr dtys subst2 us errs src_loc
    }

applyTcSubstToInst inst sw_chkr dtys subst us errs src_loc
  = case (applySubstToInst subst inst) of { (subst2, new_inst) ->
    returnNF_Tc new_inst sw_chkr dtys subst2 us errs src_loc
    }

applyTcSubstToTyVars :: [TyVar]   -> NF_TcM [UniType]
applyTcSubstToTys    :: [TauType] -> NF_TcM [TauType]

applyTcSubstToTyVars tyvars = mapNF_Tc applyTcSubstToTyVar tyvars
applyTcSubstToTys    tys    = mapNF_Tc applyTcSubstToTy    tys
applyTcSubstToInsts  insts  = mapNF_Tc applyTcSubstToInst  insts
\end{code}

\begin{code}
extendSubstTc :: TyVar -> UniType -> UnifyErrContext -> TcM ()

extendSubstTc tyvar ty err_ctxt sw_chkr dtys subst us errs src_loc
  = case (extendSubst tyvar ty subst) of { (new_subst, extend_result) ->
    case extend_result of
      SubstOK ->
	TcSucceeded () new_subst errs

      OccursCheck tyvar ty ->
	TcFailed new_subst
		 (errs `snocBag` (unifyErr (TypeRec tyvar ty) err_ctxt src_loc))

      AlreadyBound ty1 ->
	    -- This should only happen in the case of a call to
	    -- extendSubstTc from the unifier!  The way things are now
	    -- we can't check for the AlreadyBound case in other calls
	    -- to extendSubstTc, but we're confident it never shows up.
	    -- Ugh!
	unifyTauTy ty1 ty err_ctxt sw_chkr dtys new_subst us errs src_loc
    }
\end{code}


@pruneSubstTc@ does nothing with an array substitution implementation!!!
\begin{code}
pruneSubstTc :: [TyVar] -- Type vars whose substitutions should be kept
	     -> TcM a	-- Type-check this
	     -> TcM a	-- Return same result but pruned subst

pruneSubstTc keep_tyvars m sw_chkr dtys subst uniqs errs src_loc
  = m sw_chkr dtys subst uniqs errs src_loc
\end{code}

\begin{code}
getSwitchCheckerTc :: NF_TcM (GlobalSwitch -> Bool)
getSwitchCheckerTc sw_chkr = returnNF_Tc sw_chkr sw_chkr
\end{code}

\begin{code}
getDefaultingTys :: NF_TcM [UniType]
getDefaultingTys sw_chkr dtys = returnNF_Tc dtys sw_chkr dtys

setDefaultingTys :: [UniType] -> TcM a -> TcM a
setDefaultingTys dtys action sw_chkr _ subst us errs src_loc
  = action sw_chkr dtys subst us errs src_loc
\end{code}

\begin{code}
addSrcLocTc :: SrcLoc -> TcM a -> TcM a
addSrcLocTc new_locn expr sw_chkr dtys subst us errs src_loc
  = expr sw_chkr dtys subst us errs new_locn

getSrcLocTc :: NF_TcM SrcLoc
getSrcLocTc sw_chkr dtys subst us errs src_loc
  = (src_loc, subst, errs)
\end{code}

%************************************************************************
%*									*
\subsection[TcM-check]{Error-detecting functions}
%*									*
%************************************************************************

The following TcM checks a Maybe type and fails with the given
error message.

\begin{code}
checkMaybeTc :: Maybe val -> Error -> TcM val
checkMaybeTc (Just result) err = returnTc result
checkMaybeTc Nothing	   err = failTc	  err

checkMaybesTc :: [Maybe val] -> Error -> TcM [val]
checkMaybesTc []	    err = returnTc []
checkMaybesTc (Nothing:xs)  err = failTc   err
checkMaybesTc ((Just v):xs) err
  = checkMaybesTc xs err `thenTc` \ xs2 ->
    returnTc (v:xs2)

checkMaybeErrTc :: MaybeErr val err -> (err -> Error) -> TcM val
checkMaybeErrTc (Succeeded result) errfun = returnTc result
checkMaybeErrTc (Failed err)	   errfun = failTc (errfun err)

{- UNUSED:
checkMaybeErrsTc :: [MaybeErr val err] -> (err -> Error) -> TcM [val]

checkMaybeErrsTc []		    err_fun = returnTc []
checkMaybeErrsTc ((Failed err) :xs) err_fun = failTc (err_fun err)
checkMaybeErrsTc ((Succeeded v):xs) err_fun
  = checkMaybeErrsTc xs err_fun `thenTc` \ xs2 ->
    returnTc (v:xs2)
-}
\end{code}

%************************************************************************
%*									*
\subsection[TcM-Insts]{Looking up instances}
%*									*
%************************************************************************

\begin{code}
lookupInst_Tc :: Inst -> TcM (TypecheckedExpr, [Inst])

lookupInst_Tc inst sw_chkr dtys subst uniqs errs src_loc
  = case (lookupInst uniqs inst) of
      Nothing -> TcFailed subst (errs `snocBag` (noInstanceErr inst))

      Just (expr, insts) -> TcSucceeded (expr, insts) subst errs

lookupNoBindInst_Tc :: Inst -> TcM [Inst]

lookupNoBindInst_Tc inst sw_chkr dtys subst uniqs errs src_loc
  = case (lookupNoBindInst uniqs inst) of
      Nothing -> TcFailed subst (errs `snocBag` (noInstanceErr inst))

      Just insts -> TcSucceeded insts subst errs
\end{code}







%************************************************************************
%*									*
\subsection[Baby_TcM]{``Baby'' @TcM@ monadery---when we don't need the full bang}
%*									*
%************************************************************************

The "baby" Tc monad doesn't pass around the substitution.
That means you can't use it to type-check bindings, but you can use
if for everything else (interfaces, type decls, first pass of class and
instance decls etc).

Less importantly, it doesn't pass around the list of default decls either.


Type declarations
~~~~~~~~~~~~~~~~~

\begin{code}
type Baby_TcM result
	=  (GlobalSwitch -> Bool)
	-> SplitUniqSupply
	-> Bag Error			-- threaded
	-> SrcLoc			-- only passed downwards
	-> Baby_TcResult result

data Baby_TcResult result
  = BabyTcFailed    (Bag Error)

  | BabyTcSucceeded result (Bag Error)
\end{code}


Standard plumbing
~~~~~~~~~~~~~~~~~

\begin{code}
thenB_Tc   :: Baby_TcM a -> (a -> Baby_TcM b) -> Baby_TcM b
returnB_Tc :: a -> Baby_TcM a

#ifdef __GLASGOW_HASKELL__
{-# INLINE thenB_Tc #-}
{-# INLINE returnB_Tc #-}
#endif

thenB_Tc a b sw us errs loc
  = case (splitUniqSupply us) of { (s1, s2) ->
    case (a sw s1 errs loc) of
      BabyTcFailed errs2          -> BabyTcFailed errs2
      BabyTcSucceeded a_res errs2 -> b a_res sw s2 errs2 loc
    }

returnB_Tc result sw us errs loc = BabyTcSucceeded result errs
failB_Tc   err    sw us errs loc = BabyTcFailed (errs `snocBag` err)

recoverIgnoreErrorsB_Tc return_on_failure try_this sw us errs loc
  = BabyTcSucceeded result errs
  where
    result = case try_this sw us emptyBag loc of
		BabyTcSucceeded result errs_from_branch -> result
		BabyTcFailed errs_from_branch 	        -> return_on_failure

fixB_Tc :: (a -> Baby_TcM a) -> Baby_TcM a
fixB_Tc k sw us errs loc
  = result
  where
    result = k val sw us errs loc
    val = case result of
	    BabyTcSucceeded val errs -> val
	    BabyTcFailed errs	     -> panic "fixB_Tc failed"

babyTcMtoTcM :: Baby_TcM a -> TcM a
babyTcMtoTcM m sw_chkr dtys subst us errs src_loc
  = case m sw_chkr us errs src_loc of
	BabyTcSucceeded result errs2 -> TcSucceeded result subst errs2
	BabyTcFailed errs2	     -> TcFailed subst errs2

babyTcMtoNF_TcM :: Baby_TcM a -> NF_TcM a
babyTcMtoNF_TcM m sw_chkr dtys subst us errs src_loc
  = case m sw_chkr us errs src_loc of
	BabyTcSucceeded result errs2 -> (result, subst, errs2)
	BabyTcFailed errs2	     -> panic "babyTcMtoNF_TcM"
\end{code}

\begin{code}
uniqSMtoBabyTcM :: SUniqSM a -> Baby_TcM a

uniqSMtoBabyTcM u_action sw us errs loc
  = let
	u_result = u_action us
	-- at least one use *needs* this laziness
    in
    BabyTcSucceeded u_result errs
\end{code}

\begin{code}
thenB_Tc_ m k = m `thenB_Tc` \ _ -> 
		k

mapB_Tc :: (a -> Baby_TcM b) -> [a] -> Baby_TcM [b]
mapB_Tc f []     = returnB_Tc []
mapB_Tc f (x:xs) = f x		`thenB_Tc` \ fx -> 
		   mapB_Tc f xs	`thenB_Tc` \ fxs -> 
		   returnB_Tc (fx:fxs)
\end{code}


Primitives
~~~~~~~~~~

\begin{code}
getUniqueB_Tc  :: Baby_TcM Unique
getUniquesB_Tc :: Int -> Baby_TcM [Unique]

getUniqueB_Tc sw us errs loc
  = case (getSUnique us) of { unique ->
    BabyTcSucceeded unique errs }

getUniquesB_Tc n sw us errs loc
  = case (getSUniques n us) of { uniques ->
    BabyTcSucceeded uniques errs }

addSrcLocB_Tc :: SrcLoc -> Baby_TcM a -> Baby_TcM a
addSrcLocB_Tc new_locn m sw us errs loc
  = m sw us errs new_locn

getSrcLocB_Tc sw us errs loc = BabyTcSucceeded loc errs

getSwitchCheckerB_Tc :: Baby_TcM (GlobalSwitch -> Bool)
getSwitchCheckerB_Tc sw_chkr us errs loc = BabyTcSucceeded sw_chkr errs
\end{code}


Useful functions
~~~~~~~~~~~~~~~~

\begin{code}
checkB_Tc :: Bool -> Error -> Baby_TcM ()

checkB_Tc True  err = failB_Tc err
checkB_Tc False err = returnB_Tc ()
\end{code}
