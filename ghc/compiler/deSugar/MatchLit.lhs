%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[MatchLit]{Pattern-matching literal patterns}

\begin{code}
module MatchLit ( dsLit, matchLiterals ) where

#include "HsVersions.h"

import {-# SOURCE #-} Match  ( match )
import {-# SOURCE #-} DsExpr ( dsExpr )

import DsMonad
import DsUtils

import HsSyn		( HsLit(..), Pat(..), HsExpr(..) )
import TcHsSyn		( TypecheckedPat )
import Id		( Id )
import CoreSyn
import TyCon		( tyConDataCons )
import TcType		( tcSplitTyConApp, isIntegerTy  )

import PrelNames	( ratioTyConKey )
import Unique		( hasKey )
import Literal		( mkMachInt, Literal(..) )
import Maybes		( catMaybes )
import Panic		( panic, assertPanic )
import Ratio 		( numerator, denominator )
\end{code}

%************************************************************************
%*									*
		Desugaring literals
	[used to be in DsExpr, but DsMeta needs it,
	 and it's nice to avoid a loop]
%*									*
%************************************************************************

We give int/float literals type @Integer@ and @Rational@, respectively.
The typechecker will (presumably) have put \tr{from{Integer,Rational}s}
around them.

ToDo: put in range checks for when converting ``@i@''
(or should that be in the typechecker?)

For numeric literals, we try to detect there use at a standard type
(@Int@, @Float@, etc.) are directly put in the right constructor.
[NB: down with the @App@ conversion.]

See also below where we look for @DictApps@ for \tr{plusInt}, etc.

\begin{code}
dsLit :: HsLit -> DsM CoreExpr
dsLit (HsChar c)       = returnDs (mkCharExpr c)
dsLit (HsCharPrim c)   = returnDs (mkLit (MachChar c))
dsLit (HsString str)   = mkStringLitFS str
dsLit (HsStringPrim s) = returnDs (mkLit (MachStr s))
dsLit (HsInteger i)    = mkIntegerExpr i
dsLit (HsInt i)	       = returnDs (mkIntExpr i)
dsLit (HsIntPrim i)    = returnDs (mkIntLit i)
dsLit (HsFloatPrim f)  = returnDs (mkLit (MachFloat f))
dsLit (HsDoublePrim d) = returnDs (mkLit (MachDouble d))

dsLit (HsRat r ty)
  = mkIntegerExpr (numerator r)		`thenDs` \ num ->
    mkIntegerExpr (denominator r)	`thenDs` \ denom ->
    returnDs (mkConApp ratio_data_con [Type integer_ty, num, denom])
  where
    (ratio_data_con, integer_ty) 
	= case tcSplitTyConApp ty of
		(tycon, [i_ty]) -> ASSERT(isIntegerTy i_ty && tycon `hasKey` ratioTyConKey)
				   (head (tyConDataCons tycon), i_ty)
\end{code}

%************************************************************************
%*									*
		Pattern matching on literals
%*									*
%************************************************************************

\begin{code}
matchLiterals :: [Id]
	      -> [EquationInfo]
	      -> DsM MatchResult
\end{code}

This first one is a {\em special case} where the literal patterns are
unboxed numbers (NB: the fiddling introduced by @tidyEqnInfo@).  We
want to avoid using the ``equality'' stuff provided by the
typechecker, and do a real ``case'' instead.  In that sense, the code
is much like @matchConFamily@, which uses @match_cons_used@ to create
the alts---here we use @match_prims_used@.

\begin{code}
matchLiterals all_vars@(var:vars) eqns_info@(EqnInfo n ctx (LitPat literal : ps1) _ : eqns)
  = -- GENERATE THE ALTS
    match_prims_used vars eqns_info `thenDs` \ prim_alts ->

    -- MAKE THE PRIMITIVE CASE
    returnDs (mkCoPrimCaseMatchResult var prim_alts)
  where
    match_prims_used _ [{-no more eqns-}] = returnDs []

    match_prims_used vars eqns_info@(EqnInfo n ctx (pat@(LitPat literal):ps1) _ : eqns)
      = let
	    (shifted_eqns_for_this_lit, eqns_not_for_this_lit)
	      = partitionEqnsByLit pat eqns_info
	in
	-- recursive call to make other alts...
	match_prims_used vars eqns_not_for_this_lit       `thenDs` \ rest_of_alts ->

	-- (prim pats have no args; no selectMatchVars as in match_cons_used)
	-- now do the business to make the alt for _this_ LitPat ...
	match vars shifted_eqns_for_this_lit 	`thenDs` \ match_result ->
	returnDs (
	    (mk_core_lit literal, match_result)
	    : rest_of_alts
	)
      where
	mk_core_lit :: HsLit -> Literal

	mk_core_lit (HsIntPrim     i) 	 = mkMachInt  i
	mk_core_lit (HsCharPrim    c) 	 = MachChar   c
	mk_core_lit (HsStringPrim  s) 	 = MachStr    s
	mk_core_lit (HsFloatPrim   f) 	 = MachFloat  f
	mk_core_lit (HsDoublePrim  d)    = MachDouble d
    	mk_core_lit other	         = panic "matchLiterals:mk_core_lit:unhandled"
\end{code}

\begin{code}
matchLiterals all_vars@(var:vars)
  eqns_info@(EqnInfo n ctx (pat@(NPatOut literal lit_ty eq_chk):ps1) _ : eqns)
  = let
	(shifted_eqns_for_this_lit, eqns_not_for_this_lit)
	  = partitionEqnsByLit pat eqns_info
    in
    dsExpr (HsApp eq_chk (HsVar var))		`thenDs` \ pred_expr ->
    match vars shifted_eqns_for_this_lit        `thenDs` \ inner_match_result ->
    let
	match_result1 = mkGuardedMatchResult pred_expr inner_match_result
    in
    if (null eqns_not_for_this_lit)
    then
	returnDs match_result1
    else
        matchLiterals all_vars eqns_not_for_this_lit  	  `thenDs` \ match_result2 ->
	returnDs (combineMatchResults match_result1 match_result2)
\end{code}

For an n+k pattern, we use the various magic expressions we've been given.
We generate:
\begin{verbatim}
    if ge var lit then
	let n = sub var lit
	in  <expr-for-a-successful-match>
    else
	<try-next-pattern-or-whatever>
\end{verbatim}


\begin{code}
matchLiterals all_vars@(var:vars) eqns_info@(EqnInfo n ctx (pat@(NPlusKPatOut master_n k ge sub):ps1) _ : eqns)
  = let
	(shifted_eqns_for_this_lit, eqns_not_for_this_lit)
	  = partitionEqnsByLit pat eqns_info
    in
    match vars shifted_eqns_for_this_lit	`thenDs` \ inner_match_result ->

    dsExpr (HsApp ge (HsVar var))		`thenDs` \ ge_expr ->
    dsExpr (HsApp sub (HsVar var))		`thenDs` \ nminusk_expr ->

    let
	match_result1 = mkGuardedMatchResult ge_expr $
			mkCoLetsMatchResult [NonRec master_n nminusk_expr] $
			inner_match_result
    in
    if (null eqns_not_for_this_lit)
    then 
	returnDs match_result1
    else 
	matchLiterals all_vars eqns_not_for_this_lit 	`thenDs` \ match_result2 ->
	returnDs (combineMatchResults match_result1 match_result2)
\end{code}

Given a blob of @LitPat@s/@NPat@s, we want to split them into those
that are ``same''/different as one we are looking at.  We need to know
whether we're looking at a @LitPat@/@NPat@, and what literal we're after.

\begin{code}
partitionEqnsByLit :: TypecheckedPat
		   -> [EquationInfo]
		   -> ([EquationInfo], 	-- These ones are for this lit, AND
					-- they've been "shifted" by stripping
					-- off the first pattern
		       [EquationInfo]	-- These are not for this lit; they
					-- are exactly as fed in.
		      )

partitionEqnsByLit master_pat eqns
  = ( \ (xs,ys) -> (catMaybes xs, catMaybes ys))
	(unzip (map (partition_eqn master_pat) eqns))
  where
    partition_eqn :: TypecheckedPat -> EquationInfo -> (Maybe EquationInfo, Maybe EquationInfo)

    partition_eqn (LitPat k1) (EqnInfo n ctx (LitPat k2 : remaining_pats) match_result)
      | k1 == k2 = (Just (EqnInfo n ctx remaining_pats match_result), Nothing)
			  -- NB the pattern is stripped off the EquationInfo

    partition_eqn (NPatOut k1 _ _) (EqnInfo n ctx (NPatOut k2 _ _ : remaining_pats) match_result)
      | k1 == k2 = (Just (EqnInfo n ctx remaining_pats match_result), Nothing)
			  -- NB the pattern is stripped off the EquationInfo

    partition_eqn (NPlusKPatOut master_n k1 _ _)
 	          (EqnInfo n ctx (NPlusKPatOut n' k2 _ _ : remaining_pats) match_result)
      | k1 == k2 = (Just (EqnInfo n ctx remaining_pats new_match_result), Nothing)
			  -- NB the pattern is stripped off the EquationInfo
      where
	new_match_result | master_n == n' = match_result
			 | otherwise 	  = mkCoLetsMatchResult
					       [NonRec n' (Var master_n)] match_result

	-- Wild-card patterns, which will only show up in the shadows,
        -- go into both groups
    partition_eqn master_pat eqn@(EqnInfo n ctx (WildPat _ : remaining_pats) match_result)
			= (Just (EqnInfo n ctx remaining_pats match_result), Just eqn)

	-- Default case; not for this pattern
    partition_eqn master_pat eqn = (Nothing, Just eqn)
\end{code}

