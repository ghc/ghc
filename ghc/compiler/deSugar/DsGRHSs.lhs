%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1994
%
\section[DsGRHSs]{Matching guarded right-hand-sides (GRHSs)}

\begin{code}
#include "HsVersions.h"

module DsGRHSs ( dsGuarded, dsGRHSs ) where

IMP_Ubiq()
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ <= 201
IMPORT_DELOOPER(DsLoop)		-- break dsExpr/dsBinds-ish loop
#else
import {-# SOURCE #-} DsExpr  ( dsExpr )
import {-# SOURCE #-} DsBinds ( dsBinds )
import {-# SOURCE #-} Match   ( match )
#endif

import HsSyn		( GRHSsAndBinds(..), GRHS(..),
			  HsExpr(..), HsBinds, Stmt(..), 
			  HsLit, Match, Fixity, DoOrListComp, HsType, ArithSeqInfo
			 )
import TcHsSyn		( SYN_IE(TypecheckedGRHSsAndBinds), SYN_IE(TypecheckedGRHS),
			  SYN_IE(TypecheckedPat), SYN_IE(TypecheckedHsBinds),
			  SYN_IE(TypecheckedHsExpr), SYN_IE(TypecheckedStmt)
			)
import CoreSyn		( SYN_IE(CoreBinding), GenCoreBinding(..), SYN_IE(CoreExpr), mkCoLetsAny )

import DsMonad
import DsUtils

#if __GLASGOW_HASKELL__ < 200
import Id		( GenId )
#endif
import CoreUtils	( coreExprType, mkCoreIfThenElse )
import PrelVals		( nON_EXHAUSTIVE_GUARDS_ERROR_ID )
import Outputable	( PprStyle(..) )
import SrcLoc		( SrcLoc{-instance-} )
import Type             ( SYN_IE(Type) )
import Unique		( Unique, otherwiseIdKey )
import UniqFM           ( Uniquable(..) )
import Util		( panic )
\end{code}

@dsGuarded@ is used for both @case@ expressions and pattern bindings.
It desugars:
\begin{verbatim}
	| g1 -> e1
	...
	| gn -> en
	where binds
\end{verbatim}
producing an expression with a runtime error in the corner if
necessary.  The type argument gives the type of the ei.

\begin{code}
dsGuarded :: TypecheckedGRHSsAndBinds
	  -> DsM CoreExpr

dsGuarded (GRHSsAndBindsOut grhss binds err_ty)
  = dsBinds Nothing binds			`thenDs` \ core_binds ->
    dsGRHSs err_ty PatBindMatch [] grhss 	`thenDs` \ (MatchResult can_it_fail _ core_grhss_fn _) ->
    case can_it_fail of
	CantFail -> returnDs (mkCoLetsAny core_binds (core_grhss_fn (panic "It can't fail")))
	CanFail  -> mkErrorAppDs nON_EXHAUSTIVE_GUARDS_ERROR_ID err_ty "" `thenDs` \ error_expr ->
		    returnDs (mkCoLetsAny core_binds (core_grhss_fn error_expr))
\end{code}

Desugar a list of (grhs, expr) pairs [grhs = guarded
right-hand-side], as in:
\begin{verbatim}
p | g1 = e1
  | g2 = e2
  ...
  | gm = em
\end{verbatim}
We supply a @CoreExpr@ for the case in which all of
the guards fail.

\begin{code}
dsGRHSs :: Type				-- Type of RHSs
	-> DsMatchKind -> [TypecheckedPat]	-- These are to build a MatchContext from
	-> [TypecheckedGRHS]			-- Guarded RHSs
	-> DsM MatchResult

dsGRHSs ty kind pats [grhs] = dsGRHS ty kind pats grhs

dsGRHSs ty kind pats (grhs:grhss)
  = dsGRHS ty kind pats grhs	`thenDs` \ match_result1 ->
    dsGRHSs ty kind pats grhss	`thenDs` \ match_result2 ->
    combineGRHSMatchResults match_result1 match_result2

dsGRHS ty kind pats (OtherwiseGRHS expr locn)
  = putSrcLocDs locn $
    dsExpr expr 	`thenDs` \ core_expr ->
    let
	expr_fn = \ ignore -> core_expr
    in
    returnDs (MatchResult CantFail ty expr_fn (DsMatchContext kind pats locn))

dsGRHS ty kind pats (GRHS guard expr locn)
  = putSrcLocDs locn $
    dsExpr expr 	`thenDs` \ core_expr ->
    let
	expr_fn = \ ignore -> core_expr
    in
    matchGuard guard (MatchResult CantFail ty expr_fn (DsMatchContext kind pats locn))
\end{code}




%************************************************************************
%*									*
%*  matchGuard : make a MatchResult from a guarded RHS			*
%*									*
%************************************************************************

\begin{code}
matchGuard :: [TypecheckedStmt] 	-- Guard
	   -> MatchResult		-- What to do if the guard succeeds
	   -> DsM MatchResult

matchGuard [] body_result = returnDs body_result

	-- Turn an "otherwise" guard is a no-op
matchGuard (GuardStmt (HsVar v) _ : stmts) body_result
  | uniqueOf v == otherwiseIdKey
  = matchGuard stmts body_result

matchGuard (GuardStmt expr _ : stmts) body_result
  = matchGuard stmts body_result	`thenDs` \ (MatchResult _ ty body_fn cxt) ->
    dsExpr expr				`thenDs` \ core_expr ->
    let
	expr_fn = \ fail -> mkCoreIfThenElse core_expr (body_fn fail) fail
    in
    returnDs (MatchResult CanFail ty expr_fn cxt)

matchGuard (LetStmt binds : stmts) body_result
  = matchGuard stmts body_result	`thenDs` \ match_result ->
    dsBinds Nothing binds		`thenDs` \ core_binds ->
    returnDs (mkCoLetsMatchResult core_binds match_result)

matchGuard (BindStmt pat rhs _ : stmts) body_result
  = matchGuard stmts body_result			`thenDs` \ match_result ->
    dsExpr rhs						`thenDs` \ core_rhs ->
    newSysLocalDs (coreExprType core_rhs)		`thenDs` \ scrut_var ->
    match [scrut_var] [EqnInfo [pat] match_result] []	`thenDs` \ match_result' ->
    returnDs (mkCoLetsMatchResult [NonRec scrut_var core_rhs] match_result')
\end{code}
