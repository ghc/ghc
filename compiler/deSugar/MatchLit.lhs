%
% (c) The University of Glasgow 2006
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%

Pattern-matching literal patterns

\begin{code}
module MatchLit ( dsLit, dsOverLit, hsLitKey, hsOverLitKey,
		  tidyLitPat, tidyNPat, 
		  matchLiterals, matchNPlusKPats, matchNPats ) where

#include "HsVersions.h"

import {-# SOURCE #-} Match  ( match )
import {-# SOURCE #-} DsExpr ( dsExpr )

import DsMonad
import DsUtils

import HsSyn
import Id
import CoreSyn
import TyCon
import DataCon
import TcType
import Type
import PrelNames
import TysWiredIn
import PrelNames
import Unique
import Literal
import SrcLoc
import Ratio
import SrcLoc
import Outputable
import Util
import FastString
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
dsLit (HsStringPrim s) = returnDs (mkLit (MachStr s))
dsLit (HsCharPrim c)   = returnDs (mkLit (MachChar c))
dsLit (HsIntPrim i)    = returnDs (mkLit (MachInt i))
dsLit (HsFloatPrim f)  = returnDs (mkLit (MachFloat f))
dsLit (HsDoublePrim d) = returnDs (mkLit (MachDouble d))

dsLit (HsChar c)       = returnDs (mkCharExpr c)
dsLit (HsString str)   = mkStringExprFS str
dsLit (HsInteger i _)  = mkIntegerExpr i
dsLit (HsInt i)	       = returnDs (mkIntExpr i)

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

\begin{code}
hsLitKey :: HsLit -> Literal
-- Get a Core literal to use (only) a grouping key
-- Hence its type doesn't need to match the type of the original literal
--	(and doesn't for strings)
-- It only works for primitive types and strings; 
-- others have been removed by tidy
hsLitKey (HsIntPrim     i) = mkMachInt  i
hsLitKey (HsCharPrim    c) = MachChar   c
hsLitKey (HsStringPrim  s) = MachStr    s
hsLitKey (HsFloatPrim   f) = MachFloat  f
hsLitKey (HsDoublePrim  d) = MachDouble d
hsLitKey (HsString s)	   = MachStr    s

hsOverLitKey :: HsOverLit a -> Bool -> Literal
-- Ditto for HsOverLit; the boolean indicates to negate
hsOverLitKey (HsIntegral i _) False   = MachInt i
hsOverLitKey (HsIntegral i _) True    = MachInt (-i)
hsOverLitKey (HsFractional r _) False = MachFloat r
hsOverLitKey (HsFractional r _) True  = MachFloat (-r)
\end{code}

%************************************************************************
%*									*
	Tidying lit pats
%*									*
%************************************************************************

\begin{code}
tidyLitPat :: HsLit -> Pat Id
-- Result has only the following HsLits:
--	HsIntPrim, HsCharPrim, HsFloatPrim
--	HsDoublePrim, HsStringPrim, HsString
--  * HsInteger, HsRat, HsInt can't show up in LitPats
--  * We get rid of HsChar right here
tidyLitPat (HsChar c) = unLoc (mkCharLitPat c)
tidyLitPat (HsString s)
  | lengthFS s <= 1	-- Short string literals only
  = unLoc $ foldr (\c pat -> mkPrefixConPat consDataCon [mkCharLitPat c, pat] stringTy)
	          (mkNilPat stringTy) (unpackFS s)
	-- The stringTy is the type of the whole pattern, not 
	-- the type to instantiate (:) or [] with!
tidyLitPat lit = LitPat lit

----------------
tidyNPat :: HsOverLit Id -> Maybe (SyntaxExpr Id) -> SyntaxExpr Id
	 -> Type -> Pat Id
tidyNPat over_lit mb_neg eq lit_ty
  | isIntTy    lit_ty = mk_con_pat intDataCon    (HsIntPrim int_val)
  | isFloatTy  lit_ty = mk_con_pat floatDataCon  (HsFloatPrim  rat_val)
  | isDoubleTy lit_ty = mk_con_pat doubleDataCon (HsDoublePrim rat_val)
  | otherwise	      = NPat over_lit mb_neg eq lit_ty
  where
    mk_con_pat :: DataCon -> HsLit -> Pat Id
    mk_con_pat con lit = unLoc (mkPrefixConPat con [noLoc $ LitPat lit] lit_ty)
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
	      -> Type			-- Type of the whole case expression
	      -> [[EquationInfo]]	-- All PgLits
	      -> DsM MatchResult

matchLiterals (var:vars) ty sub_groups
  = do	{	-- Deal with each group
	; alts <- mapM match_group sub_groups

	 	-- Combine results.  For everything except String
		-- we can use a case expression; for String we need
		-- a chain of if-then-else
	; if isStringTy (idType var) then
	    do	{ eq_str <- dsLookupGlobalId eqStringName
		; mrs <- mapM (wrap_str_guard eq_str) alts
		; return (foldr1 combineMatchResults mrs) }
	  else 
	    return (mkCoPrimCaseMatchResult var ty alts)
	}
  where
    match_group :: [EquationInfo] -> DsM (Literal, MatchResult)
    match_group eqns
	= do { let LitPat hs_lit = firstPat (head eqns)
	     ; match_result <- match vars ty (shiftEqns eqns)
	     ; return (hsLitKey hs_lit, match_result) }

    wrap_str_guard :: Id -> (Literal,MatchResult) -> DsM MatchResult
	-- Equality check for string literals
    wrap_str_guard eq_str (MachStr s, mr)
	= do { lit    <- mkStringExprFS s
	     ; let pred = mkApps (Var eq_str) [Var var, lit]
	     ; return (mkGuardedMatchResult pred mr) }
\end{code}


%************************************************************************
%*									*
		Pattern matching on NPat
%*									*
%************************************************************************

\begin{code}
matchNPats :: [Id] -> Type -> [[EquationInfo]] -> DsM MatchResult
	-- All NPats, but perhaps for different literals
matchNPats vars ty groups
  = do {  match_results <- mapM (matchOneNPat vars ty) groups
	; return (foldr1 combineMatchResults match_results) }

matchOneNPat (var:vars) ty (eqn1:eqns)	-- All for the same literal
  = do	{ let NPat lit mb_neg eq_chk _ = firstPat eqn1
	; lit_expr <- dsOverLit lit
	; neg_lit <- case mb_neg of
			    Nothing -> return lit_expr
			    Just neg -> do { neg_expr <- dsExpr neg
					   ; return (App neg_expr lit_expr) }
	; eq_expr <- dsExpr eq_chk
	; let pred_expr = mkApps eq_expr [Var var, neg_lit]
	; match_result <- match vars ty (shiftEqns (eqn1:eqns))
	; return (mkGuardedMatchResult pred_expr match_result) }
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


\begin{code}
matchNPlusKPats :: [Id] -> Type -> [EquationInfo] -> DsM MatchResult
	-- All NPlusKPats, for the *same* literal k
matchNPlusKPats all_vars@(var:vars) ty (eqn1:eqns)
  = do	{ let NPlusKPat (L _ n1) lit ge minus = firstPat eqn1
	; ge_expr     <- dsExpr ge
	; minus_expr  <- dsExpr minus
	; lit_expr    <- dsOverLit lit
	; let pred_expr   = mkApps ge_expr [Var var, lit_expr]
	      minusk_expr = mkApps minus_expr [Var var, lit_expr]
	      (wraps, eqns') = mapAndUnzip (shift n1) (eqn1:eqns)
	; match_result <- match vars ty eqns'
	; return  (mkGuardedMatchResult pred_expr 		$
		   mkCoLetMatchResult (NonRec n1 minusk_expr)	$
		   adjustMatchResult (foldr1 (.) wraps)		$
		   match_result) }
  where
    shift n1 eqn@(EqnInfo { eqn_pats = NPlusKPat (L _ n) _ _ _ : pats })
	= (wrapBind n n1, eqn { eqn_pats = pats })
	-- The wrapBind is a no-op for the first equation
\end{code}
