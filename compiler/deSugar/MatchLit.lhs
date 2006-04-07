%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[MatchLit]{Pattern-matching literal patterns}

\begin{code}
module MatchLit ( dsLit, dsOverLit,
		  tidyLitPat, tidyNPat,
		  matchLiterals, matchNPlusKPats, matchNPats ) where

#include "HsVersions.h"

import {-# SOURCE #-} Match  ( match )
import {-# SOURCE #-} DsExpr ( dsExpr )

import DsMonad
import DsUtils

import HsSyn
import Id		( Id, idType )
import CoreSyn
import TyCon		( tyConDataCons )
import TcType		( tcSplitTyConApp, isIntegerTy, isIntTy, 
			  isFloatTy, isDoubleTy, isStringTy )
import Type		( Type )
import PrelNames	( ratioTyConKey )
import TysWiredIn	( stringTy, consDataCon, intDataCon, floatDataCon, doubleDataCon )
import PrelNames	( eqStringName )
import Unique		( hasKey )
import Literal		( mkMachInt, Literal(..) )
import SrcLoc		( noLoc )
import ListSetOps	( equivClasses, runs )
import Ratio 		( numerator, denominator )
import SrcLoc		( Located(..) )
import Outputable
import FastString	( lengthFS, unpackFS )
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
dsLit (HsString str)   = mkStringExprFS str
dsLit (HsStringPrim s) = returnDs (mkLit (MachStr s))
dsLit (HsInteger i _)  = mkIntegerExpr i
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

dsOverLit :: HsOverLit Id -> DsM CoreExpr
-- Post-typechecker, the SyntaxExpr field of an OverLit contains 
-- (an expression for) the literal value itself
dsOverLit (HsIntegral   _ lit) = dsExpr lit
dsOverLit (HsFractional _ lit) = dsExpr lit
\end{code}

%************************************************************************
%*									*
	Tidying lit pats
%*									*
%************************************************************************

\begin{code}
tidyLitPat :: HsLit -> LPat Id -> LPat Id
-- Result has only the following HsLits:
--	HsIntPrim, HsCharPrim, HsFloatPrim
--	HsDoublePrim, HsStringPrim, HsString
--  * HsInteger, HsRat, HsInt can't show up in LitPats
--  * We get rid of HsChar right here
tidyLitPat (HsChar c) pat = mkCharLitPat c
tidyLitPat (HsString s) pat
  | lengthFS s <= 1	-- Short string literals only
  = foldr (\c pat -> mkPrefixConPat consDataCon [mkCharLitPat c,pat] stringTy)
	  (mkNilPat stringTy) (unpackFS s)
	-- The stringTy is the type of the whole pattern, not 
	-- the type to instantiate (:) or [] with!
tidyLitPat lit	      pat = pat

----------------
tidyNPat :: HsOverLit Id -> Maybe (SyntaxExpr Id) -> Type -> LPat Id -> LPat Id
tidyNPat over_lit mb_neg lit_ty default_pat
  | isIntTy    lit_ty = mk_con_pat intDataCon    (HsIntPrim int_val)
  | isFloatTy  lit_ty = mk_con_pat floatDataCon  (HsFloatPrim  rat_val)
  | isDoubleTy lit_ty = mk_con_pat doubleDataCon (HsDoublePrim rat_val)
  | otherwise	      = default_pat
  where
    mk_con_pat con lit = mkPrefixConPat con [noLoc $ LitPat lit] lit_ty 
    neg_lit = case (mb_neg, over_lit) of
		(Nothing, 	       _)   -> over_lit
		(Just _,  HsIntegral i s)   -> HsIntegral   (-i) s
		(Just _,  HsFractional f s) -> HsFractional (-f) s
			     
    int_val :: Integer
    int_val = case neg_lit of
		HsIntegral   i _ -> i
		HsFractional f _ -> panic "tidyNPat"
	
    rat_val :: Rational
    rat_val = case neg_lit of
		HsIntegral   i _ -> fromInteger i
		HsFractional f _ -> f
\end{code}


%************************************************************************
%*									*
		Pattern matching on LitPat
%*									*
%************************************************************************

\begin{code}
matchLiterals :: [Id]
	      -> Type	-- Type of the whole case expression
	      -> [EquationInfo]
	      -> DsM MatchResult
-- All the EquationInfos have LitPats at the front

matchLiterals (var:vars) ty eqns
  = do	{	-- Group by literal
	  let groups :: [[(Literal, EquationInfo)]]
	      groups = equivClasses cmpTaggedEqn (tagLitEqns eqns)

		-- Deal with each group
	; alts <- mapM match_group groups

	 	-- Combine results.  For everything except String
		-- we can use a case expression; for String we need
		-- a chain of if-then-else
	; if isStringTy (idType var) then
	    do	{ mrs <- mapM wrap_str_guard alts
		; return (foldr1 combineMatchResults mrs) }
	  else 
	    return (mkCoPrimCaseMatchResult var ty alts)
	}
  where
    match_group :: [(Literal, EquationInfo)] -> DsM (Literal, MatchResult)
    match_group group
	= do { let (lits, eqns) = unzip group
	     ; match_result <- match vars ty (shiftEqns eqns)
	     ; return (head lits, match_result) }

    wrap_str_guard :: (Literal,MatchResult) -> DsM MatchResult
	-- Equality check for string literals
    wrap_str_guard (MachStr s, mr)
	= do { eq_str <- dsLookupGlobalId eqStringName
	     ; lit    <- mkStringExprFS s
	     ; let pred = mkApps (Var eq_str) [Var var, lit]
	     ; return (mkGuardedMatchResult pred mr) }
\end{code}

%************************************************************************
%*									*
		Pattern matching on NPat
%*									*
%************************************************************************

\begin{code}
matchNPats :: [Id] -> Type -> [EquationInfo] -> DsM MatchResult
-- All the EquationInfos have NPat at the front

matchNPats (var:vars) ty eqns
  = do {  let groups :: [[(Literal, EquationInfo)]]
	      groups = equivClasses cmpTaggedEqn (tagLitEqns eqns)

	; match_results <- mapM (match_group . map snd) groups

	; ASSERT( not (null match_results) )
	  return (foldr1 combineMatchResults match_results) }
  where
    match_group :: [EquationInfo] -> DsM MatchResult
    match_group (eqn1:eqns)
	= do { lit_expr <- dsOverLit lit
	     ; neg_lit <- case mb_neg of
			    Nothing -> return lit_expr
			    Just neg -> do { neg_expr <- dsExpr neg
					   ; return (App neg_expr lit_expr) }
	     ; eq_expr <- dsExpr eq_chk
	     ; let pred_expr = mkApps eq_expr [Var var, neg_lit]
	     ; match_result <- match vars ty (eqn1' : shiftEqns eqns)
	     ; return (adjustMatchResult (eqn_wrap eqn1) $
			-- Bring the eqn1 wrapper stuff into scope because
			-- it may be used in pred_expr
		       mkGuardedMatchResult pred_expr match_result) }
	where
	  NPat lit mb_neg eq_chk _ : pats1 = eqn_pats eqn1
	  eqn1' = eqn1 { eqn_wrap = idWrapper, eqn_pats = pats1 }
\end{code}


%************************************************************************
%*									*
		Pattern matching on n+k patterns
%*									*
%************************************************************************

For an n+k pattern, we use the various magic expressions we've been given.
We generate:
\begin{verbatim}
    if ge var lit then
	let n = sub var lit
	in  <expr-for-a-successful-match>
    else
	<try-next-pattern-or-whatever>
\end{verbatim}

WATCH OUT!  Consider

	f (n+1) = ...
	f (n+2) = ...
	f (n+1) = ...

We can't group the first and third together, because the second may match 
the same thing as the first.  Contrast
	f 1 = ...
	f 2 = ...
	f 1 = ...
where we can group the first and third.  Hence 'runs' rather than 'equivClasses'

\begin{code}
matchNPlusKPats all_vars@(var:vars) ty eqns
  = do {  let groups :: [[(Literal, EquationInfo)]]
	      groups = runs eqTaggedEqn (tagLitEqns eqns)

	; match_results <- mapM (match_group . map snd) groups

	; ASSERT( not (null match_results) )
	  return (foldr1 combineMatchResults match_results) }
  where
    match_group :: [EquationInfo] -> DsM MatchResult
    match_group (eqn1:eqns)
	= do { ge_expr     <- dsExpr ge
	     ; minus_expr  <- dsExpr minus
	     ; lit_expr    <- dsOverLit lit
	     ; let pred_expr   = mkApps ge_expr [Var var, lit_expr]
		   minusk_expr = mkApps minus_expr [Var var, lit_expr]
	     ; match_result <- match vars ty (eqn1' : map shift eqns)
	     ; return  (adjustMatchResult (eqn_wrap eqn1) 	     $
			-- Bring the eqn1 wrapper stuff into scope because
			-- it may be used in ge_expr, minusk_expr
		        mkGuardedMatchResult pred_expr 		    $
			mkCoLetMatchResult (NonRec n1 minusk_expr)  $
			match_result) }
	where
	  NPlusKPat (L _ n1) lit ge minus : pats1 = eqn_pats eqn1
	  eqn1' = eqn1 { eqn_wrap = idWrapper, eqn_pats = pats1 }

	  shift eqn@(EqnInfo { eqn_wrap = wrap,
			       eqn_pats = NPlusKPat (L _ n) _ _ _ : pats })
	    = eqn { eqn_wrap = wrap . wrapBind n n1, eqn_pats = pats }  
\end{code}


%************************************************************************
%*									*
		Grouping functions
%*									*
%************************************************************************

Given a blob of @LitPat@s/@NPat@s, we want to split them into those
that are ``same''/different as one we are looking at.  We need to know
whether we're looking at a @LitPat@/@NPat@, and what literal we're after.

\begin{code}
-- Tag equations by the leading literal
-- NB: we have ordering on Core Literals, but not on HsLits
cmpTaggedEqn :: (Literal,EquationInfo) -> (Literal,EquationInfo) -> Ordering
cmpTaggedEqn (lit1,_) (lit2,_) = lit1 `compare` lit2

eqTaggedEqn :: (Literal,EquationInfo) -> (Literal,EquationInfo) -> Bool
eqTaggedEqn (lit1,_) (lit2,_) = lit1 == lit2

tagLitEqns :: [EquationInfo] -> [(Literal, EquationInfo)]
tagLitEqns eqns = [(get_lit (firstPat eqn), eqn) | eqn <- eqns]

get_lit :: Pat Id -> Literal
-- Get a Core literal to use (only) a grouping key
-- Hence its type doesn't need to match the type of the original literal
get_lit (LitPat (HsIntPrim     i)) = mkMachInt  i
get_lit (LitPat (HsCharPrim    c)) = MachChar   c
get_lit (LitPat (HsStringPrim  s)) = MachStr    s
get_lit (LitPat (HsFloatPrim   f)) = MachFloat  f
get_lit (LitPat (HsDoublePrim  d)) = MachDouble d
get_lit (LitPat (HsString s))	   = MachStr    s

get_lit (NPat (HsIntegral i _) Nothing  _ _)   = MachInt i
get_lit (NPat (HsIntegral i _) (Just _) _ _)   = MachInt (-i)
get_lit (NPat (HsFractional r _) Nothing  _ _) = MachFloat r
get_lit (NPat (HsFractional r _) (Just _) _ _) = MachFloat (-r)

get_lit (NPlusKPat _ (HsIntegral i _) _ _) = MachInt i

-- These ones can't happen
-- get_lit (LitPat (HsChar c))
-- get_lit (LitPat (HsInt i))	
get_lit other = pprPanic "get_lit:bad pattern" (ppr other)
\end{code}

