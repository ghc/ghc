%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[MatchLit]{Pattern-matching literal patterns}

\begin{code}
module MatchLit ( dsLit, tidyLitPat, tidyNPat,
		  matchLiterals, matchNPlusKPats, matchNPats ) where

#include "HsVersions.h"

import {-# SOURCE #-} Match  ( match )
import {-# SOURCE #-} DsExpr ( dsExpr )

import DsMonad
import DsUtils

import HsSyn
import Id		( Id )
import CoreSyn
import TyCon		( tyConDataCons )
import TcType		( tcSplitTyConApp, isIntegerTy, isIntTy, isFloatTy, isDoubleTy )
import Type		( Type )
import PrelNames	( ratioTyConKey )
import TysWiredIn	( stringTy, consDataCon, intDataCon, floatDataCon, doubleDataCon )
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
--	HsDoublePrim, HsStringPrim ?
-- * HsInteger, HsRat, HsInt can't show up in LitPats,
-- * HsString has been turned into an NPat in tcPat
-- and we get rid of HsChar right here
tidyLitPat (HsChar c) pat = mkCharLitPat c
tidyLitPat lit	      pat = pat

tidyNPat :: HsLit -> Type -> LPat Id -> LPat Id
tidyNPat (HsString s) _ pat
  | lengthFS s <= 1	-- Short string literals only
  = foldr (\c pat -> mkPrefixConPat consDataCon [mkCharLitPat c,pat] stringTy)
	  (mkNilPat stringTy) (unpackFS s)
	-- The stringTy is the type of the whole pattern, not 
	-- the type to instantiate (:) or [] with!

tidyNPat lit lit_ty default_pat
  | isIntTy lit_ty     	= mkPrefixConPat intDataCon    [noLoc $ LitPat (mk_int lit)]    lit_ty 
  | isFloatTy lit_ty  	= mkPrefixConPat floatDataCon  [noLoc $ LitPat (mk_float lit)]  lit_ty 
  | isDoubleTy lit_ty 	= mkPrefixConPat doubleDataCon [noLoc $ LitPat (mk_double lit)] lit_ty 
  | otherwise		= default_pat

  where
    mk_int    (HsInteger i _) = HsIntPrim i

    mk_float  (HsInteger i _) = HsFloatPrim (fromInteger i)
    mk_float  (HsRat f _)     = HsFloatPrim f

    mk_double (HsInteger i _) = HsDoublePrim (fromInteger i)
    mk_double (HsRat f _)     = HsDoublePrim f
\end{code}


%************************************************************************
%*									*
		Pattern matching on LitPat
%*									*
%************************************************************************

\begin{code}
matchLiterals :: [Id] -> Type -> [EquationInfo] -> DsM MatchResult
-- All the EquationInfos have LitPats at the front

matchLiterals (var:vars) ty eqns
  = do	{ -- GROUP BY LITERAL
	  let groups :: [[(Literal, EquationInfo)]]
	      groups = equivClasses cmpTaggedEqn (tagLitEqns eqns)

	    -- DO THE MATCHING FOR EACH GROUP
	; alts <- mapM match_group groups

	    -- MAKE THE PRIMITIVE CASE
	; return (mkCoPrimCaseMatchResult var ty alts) }
  where
    match_group :: [(Literal, EquationInfo)] -> DsM (Literal, MatchResult)
    match_group group
	= do { let (lits, eqns) = unzip group
	     ; match_result <- match vars ty (shiftEqns eqns)
	     ; return (head lits, match_result) }
\end{code}

%************************************************************************
%*									*
		Pattern matching on NPat
%*									*
%************************************************************************

\begin{code}
matchNPats :: [Id] -> Type -> [EquationInfo] -> DsM MatchResult
-- All the EquationInfos have NPatOut at the front

matchNPats (var:vars) ty eqns
  = do {  let groups :: [[(Literal, EquationInfo)]]
	      groups = equivClasses cmpTaggedEqn (tagLitEqns eqns)

	; match_results <- mapM (match_group . map snd) groups

	; ASSERT( not (null match_results) )
	  return (foldr1 combineMatchResults match_results) }
  where
    match_group :: [EquationInfo] -> DsM MatchResult
    match_group (eqn1:eqns)
	= do { pred_expr <- dsExpr (HsApp (noLoc eq_chk) (nlHsVar var))
	     ; match_result <- match vars ty (eqn1' : shiftEqns eqns)
	     ; return (adjustMatchResult (eqn_wrap eqn1) $
			-- Bring the eqn1 wrapper stuff into scope because
			-- it may be used in pred_expr
		       mkGuardedMatchResult pred_expr match_result) }
	where
	  NPatOut _ _ eq_chk : pats1 = eqn_pats eqn1
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
	= do { ge_expr      <- dsExpr (HsApp (noLoc ge)  (nlHsVar var))
	     ; minusk_expr  <- dsExpr (HsApp (noLoc sub) (nlHsVar var))
	     ; match_result <- match vars ty (eqn1' : map shift eqns)
	     ; return  (adjustMatchResult (eqn_wrap eqn1) 	     $
			-- Bring the eqn1 wrapper stuff into scope because
			-- it may be used in ge_expr, minusk_expr
		        mkGuardedMatchResult ge_expr 		    $
			mkCoLetMatchResult (NonRec n1 minusk_expr)  $
			match_result) }
	where
	  NPlusKPatOut (L _ n1) _ ge sub : pats1 = eqn_pats eqn1
	  eqn1' = eqn1 { eqn_wrap = idWrapper, eqn_pats = pats1 }

	  shift eqn@(EqnInfo { eqn_wrap = wrap,
			       eqn_pats = NPlusKPatOut (L _ n) _ _ _ : pats })
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
tagLitEqns eqns
  = [(get_lit eqn, eqn) | eqn <- eqns]
  where
    get_lit eqn = case firstPat eqn of
		    LitPat  hs_lit       -> mk_core_lit hs_lit
		    NPatOut hs_lit _ _   -> mk_core_lit hs_lit
		    NPlusKPatOut _ i _ _ -> MachInt i
		    other -> panic "tagLitEqns:bad pattern"

mk_core_lit :: HsLit -> Literal
mk_core_lit (HsIntPrim     i) = mkMachInt  i
mk_core_lit (HsCharPrim    c) = MachChar   c
mk_core_lit (HsStringPrim  s) = MachStr    s
mk_core_lit (HsFloatPrim   f) = MachFloat  f
mk_core_lit (HsDoublePrim  d) = MachDouble d

	-- These ones are only needed in the NPatOut case, 
	-- and the Literal is only used as a key for grouping,
	-- so the type doesn't matter.  Actually I think HsInt, HsChar
	-- can't happen, but it does no harm to include them
mk_core_lit (HsString s)    = MachStr s
mk_core_lit (HsRat r _)     = MachFloat r
mk_core_lit (HsInteger i _) = MachInt i
mk_core_lit (HsInt i)       = MachInt i
mk_core_lit (HsChar c)      = MachChar c
\end{code}

