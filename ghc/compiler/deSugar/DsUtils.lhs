%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[DsUtils]{Utilities for desugaring}

This module exports some utility functions of no great interest.

\begin{code}
module DsUtils (
	CanItFail(..), EquationInfo(..), MatchResult(..),
        EqnNo, EqnSet,

	tidyLitPat, tidyNPat,

	mkDsLet, mkDsLets,

	cantFailMatchResult, extractMatchResult,
	combineMatchResults, 
	adjustMatchResult, adjustMatchResultDs,
	mkCoLetsMatchResult, mkGuardedMatchResult, 
	mkCoPrimCaseMatchResult, mkCoAlgCaseMatchResult,

	mkErrorAppDs, mkNilExpr, mkConsExpr, mkListExpr,
	mkIntExpr, mkCharExpr,
	mkStringLit, mkStringLitFS, mkIntegerExpr, 

	mkSelectorBinds, mkTupleExpr, mkTupleSelector, 
	mkTupleType, mkTupleCase, mkBigCoreTup,
	mkCoreTup, mkCoreSel, mkCoreTupTy,
	
	dsReboundNames, lookupReboundName,

	selectMatchVar
    ) where

#include "HsVersions.h"

import {-# SOURCE #-}	Match ( matchSimply )
import {-# SOURCE #-}	DsExpr( dsExpr )

import HsSyn
import TcHsSyn		( TypecheckedPat, hsPatType )
import CoreSyn
import Constants	( mAX_TUPLE_SIZE )
import DsMonad

import CoreUtils	( exprType, mkIfThenElse, mkCoerce, bindNonRec )
import MkId		( iRREFUT_PAT_ERROR_ID, mkReboxingAlt, mkNewTypeBody )
import Id		( idType, Id, mkWildId, mkTemplateLocals, mkSysLocal )
import Name		( Name )
import Literal		( Literal(..), inIntRange, tARGET_MAX_INT )
import TyCon		( isNewTyCon, tyConDataCons )
import DataCon		( DataCon, dataConSourceArity )
import Type		( mkFunTy, isUnLiftedType, Type, splitTyConApp )
import TcType		( tcTyConAppTyCon, isIntTy, isFloatTy, isDoubleTy )
import TysPrim		( intPrimTy )
import TysWiredIn	( nilDataCon, consDataCon, 
                          tupleCon, mkTupleTy,
			  unitDataConId, unitTy,
                          charTy, charDataCon, 
                          intTy, intDataCon, smallIntegerDataCon, 
			  floatDataCon, 
                          doubleDataCon,
			  stringTy, isPArrFakeCon )
import BasicTypes	( Boxity(..) )
import UniqSet		( mkUniqSet, minusUniqSet, isEmptyUniqSet, UniqSet )
import UniqSupply	( splitUniqSupply, uniqFromSupply )
import PrelNames	( unpackCStringName, unpackCStringUtf8Name, 
			  plusIntegerName, timesIntegerName, 
			  lengthPName, indexPName )
import Outputable
import UnicodeUtil      ( intsToUtf8, stringToUtf8 )
import Util             ( isSingleton, notNull, zipEqual )
import ListSetOps	( assocDefault )
import FastString
\end{code}



%************************************************************************
%*									*
		Rebindable syntax
%*									*
%************************************************************************

\begin{code}
dsReboundNames :: ReboundNames Id 
	       -> DsM ([CoreBind], 	-- Auxiliary bindings
		       [(Name,Id)])	-- Maps the standard name to its value

dsReboundNames rebound_ids
  = mapAndUnzipDs mk_bind rebound_ids	`thenDs` \ (binds_s, prs) ->
    return (concat binds_s, prs)
  where
	-- The cheapo special case can happen when we 
	-- make an intermediate HsDo when desugaring a RecStmt
    mk_bind (std_name, HsVar id) = return ([], (std_name, id))
    mk_bind (std_name, expr) 	 = dsExpr expr				`thenDs` \ rhs ->
			     	   newSysLocalDs (exprType rhs)		`thenDs` \ id ->
			     	   return ([NonRec id rhs], (std_name, id))

lookupReboundName :: [(Name,Id)] -> Name -> CoreExpr
lookupReboundName prs std_name
  = Var (assocDefault (mk_panic std_name) prs std_name)
  where
    mk_panic std_name = pprPanic "dsReboundNames" (ptext SLIT("Not found:") <+> ppr std_name)
\end{code}


%************************************************************************
%*									*
\subsection{Tidying lit pats}
%*									*
%************************************************************************

\begin{code}
tidyLitPat :: HsLit -> TypecheckedPat -> TypecheckedPat
tidyLitPat (HsChar c) pat = mkCharLitPat c
tidyLitPat lit        pat = pat

tidyNPat :: HsLit -> Type -> TypecheckedPat -> TypecheckedPat
tidyNPat (HsString s) _ pat
  | lengthFS s <= 1	-- Short string literals only
  = foldr (\c pat -> mkPrefixConPat consDataCon [mkCharLitPat c,pat] stringTy)
	  (mkNilPat stringTy) (unpackIntFS s)
	-- The stringTy is the type of the whole pattern, not 
	-- the type to instantiate (:) or [] with!
  where

tidyNPat lit lit_ty default_pat
  | isIntTy lit_ty     	= mkPrefixConPat intDataCon    [LitPat (mk_int lit)]    lit_ty 
  | isFloatTy lit_ty  	= mkPrefixConPat floatDataCon  [LitPat (mk_float lit)]  lit_ty 
  | isDoubleTy lit_ty 	= mkPrefixConPat doubleDataCon [LitPat (mk_double lit)] lit_ty 
  | otherwise		= default_pat

  where
    mk_int    (HsInteger i) = HsIntPrim i

    mk_float  (HsInteger i) = HsFloatPrim (fromInteger i)
    mk_float  (HsRat f _)   = HsFloatPrim f

    mk_double (HsInteger i) = HsDoublePrim (fromInteger i)
    mk_double (HsRat f _)   = HsDoublePrim f
\end{code}


%************************************************************************
%*									*
\subsection{Building lets}
%*									*
%************************************************************************

Use case, not let for unlifted types.  The simplifier will turn some
back again.

\begin{code}
mkDsLet :: CoreBind -> CoreExpr -> CoreExpr
mkDsLet (NonRec bndr rhs) body
  | isUnLiftedType (idType bndr) = Case rhs bndr [(DEFAULT,[],body)]
mkDsLet bind body
  = Let bind body

mkDsLets :: [CoreBind] -> CoreExpr -> CoreExpr
mkDsLets binds body = foldr mkDsLet body binds
\end{code}


%************************************************************************
%*									*
\subsection{ Selecting match variables}
%*									*
%************************************************************************

We're about to match against some patterns.  We want to make some
@Ids@ to use as match variables.  If a pattern has an @Id@ readily at
hand, which should indeed be bound to the pattern as a whole, then use it;
otherwise, make one up.

\begin{code}
selectMatchVar :: TypecheckedPat -> DsM Id
selectMatchVar (VarPat var)     = returnDs var
selectMatchVar (AsPat var pat) 	= returnDs var
selectMatchVar (LazyPat pat)   	= selectMatchVar pat
selectMatchVar other_pat       	= newSysLocalDs (hsPatType other_pat) -- OK, better make up one...
\end{code}


%************************************************************************
%*									*
%* type synonym EquationInfo and access functions for its pieces	*
%*									*
%************************************************************************
\subsection[EquationInfo-synonym]{@EquationInfo@: a useful synonym}

The ``equation info'' used by @match@ is relatively complicated and
worthy of a type synonym and a few handy functions.

\begin{code}

type EqnNo   = Int
type EqnSet  = UniqSet EqnNo

data EquationInfo
  = EqnInfo
	EqnNo		-- The number of the equation

	DsMatchContext	-- The context info is used when producing warnings
			-- about shadowed patterns.  It's the context
			-- of the *first* thing matched in this group.
			-- Should perhaps be a list of them all!

	[TypecheckedPat]    -- The patterns for an eqn

      	MatchResult	    -- Encapsulates the guards and bindings
\end{code}

\begin{code}
data MatchResult
  = MatchResult
	CanItFail	-- Tells whether the failure expression is used
	(CoreExpr -> DsM CoreExpr)
			-- Takes a expression to plug in at the
			-- failure point(s). The expression should
			-- be duplicatable!

data CanItFail = CanFail | CantFail

orFail CantFail CantFail = CantFail
orFail _        _	 = CanFail
\end{code}

Functions on MatchResults

\begin{code}
cantFailMatchResult :: CoreExpr -> MatchResult
cantFailMatchResult expr = MatchResult CantFail (\ ignore -> returnDs expr)

extractMatchResult :: MatchResult -> CoreExpr -> DsM CoreExpr
extractMatchResult (MatchResult CantFail match_fn) fail_expr
  = match_fn (error "It can't fail!")

extractMatchResult (MatchResult CanFail match_fn) fail_expr
  = mkFailurePair fail_expr	 	`thenDs` \ (fail_bind, if_it_fails) ->
    match_fn if_it_fails		`thenDs` \ body ->
    returnDs (mkDsLet fail_bind body)


combineMatchResults :: MatchResult -> MatchResult -> MatchResult
combineMatchResults (MatchResult CanFail      body_fn1)
		    (MatchResult can_it_fail2 body_fn2)
  = MatchResult can_it_fail2 body_fn
  where
    body_fn fail = body_fn2 fail			`thenDs` \ body2 ->
		   mkFailurePair body2	 		`thenDs` \ (fail_bind, duplicatable_expr) ->
		   body_fn1 duplicatable_expr		`thenDs` \ body1 ->
		   returnDs (Let fail_bind body1)

combineMatchResults match_result1@(MatchResult CantFail body_fn1) match_result2
  = match_result1


adjustMatchResult :: (CoreExpr -> CoreExpr) -> MatchResult -> MatchResult
adjustMatchResult encl_fn (MatchResult can_it_fail body_fn)
  = MatchResult can_it_fail (\fail -> body_fn fail	`thenDs` \ body ->
				      returnDs (encl_fn body))

adjustMatchResultDs :: (CoreExpr -> DsM CoreExpr) -> MatchResult -> MatchResult
adjustMatchResultDs encl_fn (MatchResult can_it_fail body_fn)
  = MatchResult can_it_fail (\fail -> body_fn fail	`thenDs` \ body ->
				      encl_fn body)


mkCoLetsMatchResult :: [CoreBind] -> MatchResult -> MatchResult
mkCoLetsMatchResult binds match_result
  = adjustMatchResult (mkDsLets binds) match_result


mkGuardedMatchResult :: CoreExpr -> MatchResult -> MatchResult
mkGuardedMatchResult pred_expr (MatchResult can_it_fail body_fn)
  = MatchResult CanFail (\fail -> body_fn fail	`thenDs` \ body ->
				  returnDs (mkIfThenElse pred_expr body fail))

mkCoPrimCaseMatchResult :: Id				-- Scrutinee
		    -> [(Literal, MatchResult)]		-- Alternatives
		    -> MatchResult
mkCoPrimCaseMatchResult var match_alts
  = MatchResult CanFail mk_case
  where
    mk_case fail
      = mapDs (mk_alt fail) match_alts		`thenDs` \ alts ->
	returnDs (Case (Var var) var ((DEFAULT, [], fail) : alts))

    mk_alt fail (lit, MatchResult _ body_fn) = body_fn fail	`thenDs` \ body ->
					       returnDs (LitAlt lit, [], body)


mkCoAlgCaseMatchResult :: Id					-- Scrutinee
		    -> [(DataCon, [CoreBndr], MatchResult)]	-- Alternatives
		    -> MatchResult

mkCoAlgCaseMatchResult var match_alts
  | isNewTyCon tycon		-- Newtype case; use a let
  = ASSERT( null (tail match_alts) && null (tail arg_ids) )
    mkCoLetsMatchResult [NonRec arg_id newtype_rhs] match_result

  | isPArrFakeAlts match_alts	-- Sugared parallel array; use a literal case 
  = MatchResult CanFail mk_parrCase

  | otherwise			-- Datatype case; use a case
  = MatchResult fail_flag mk_case
  where
	-- Common stuff
    scrut_ty = idType var
    tycon    = tcTyConAppTyCon scrut_ty		-- Newtypes must be opaque here

	-- Stuff for newtype
    (_, arg_ids, match_result) = head match_alts
    arg_id 	       	       = head arg_ids
    newtype_rhs 	       = mkNewTypeBody tycon (idType arg_id) (Var var)
		
	-- Stuff for data types
    data_cons      = tyConDataCons tycon
    match_results  = [match_result | (_,_,match_result) <- match_alts]

    fail_flag | exhaustive_case
	      = foldr1 orFail [can_it_fail | MatchResult can_it_fail _ <- match_results]
	      | otherwise
	      = CanFail

    wild_var = mkWildId (idType var)
    mk_case fail = mapDs (mk_alt fail) match_alts	`thenDs` \ alts ->
		   returnDs (Case (Var var) wild_var (mk_default fail ++ alts))

    mk_alt fail (con, args, MatchResult _ body_fn)
	= body_fn fail				`thenDs` \ body ->
	  getUniquesDs				`thenDs` \ us ->
	  returnDs (mkReboxingAlt us con args body)

    mk_default fail | exhaustive_case = []
		    | otherwise       = [(DEFAULT, [], fail)]

    un_mentioned_constructors
        = mkUniqSet data_cons `minusUniqSet` mkUniqSet [ con | (con, _, _) <- match_alts]
    exhaustive_case = isEmptyUniqSet un_mentioned_constructors

	-- Stuff for parallel arrays
	-- 
	-- * the following is to desugar cases over fake constructors for
	--   parallel arrays, which are introduced by `tidy1' in the `PArrPat'
	--   case
	--
	-- Concerning `isPArrFakeAlts':
	--
	-- * it is *not* sufficient to just check the type of the type
	--   constructor, as we have to be careful not to confuse the real
	--   representation of parallel arrays with the fake constructors;
	--   moreover, a list of alternatives must not mix fake and real
	--   constructors (this is checked earlier on)
	--
	-- FIXME: We actually go through the whole list and make sure that
	--	  either all or none of the constructors are fake parallel
	--	  array constructors.  This is to spot equations that mix fake
	--	  constructors with the real representation defined in
	--	  `PrelPArr'.  It would be nicer to spot this situation
	--	  earlier and raise a proper error message, but it can really
	--	  only happen in `PrelPArr' anyway.
	--
    isPArrFakeAlts [(dcon, _, _)]      = isPArrFakeCon dcon
    isPArrFakeAlts ((dcon, _, _):alts) = 
      case (isPArrFakeCon dcon, isPArrFakeAlts alts) of
        (True , True ) -> True
        (False, False) -> False
	_	       -> 
	  panic "DsUtils: You may not mix `[:...:]' with `PArr' patterns"
    --
    mk_parrCase fail = 		   
      dsLookupGlobalId lengthPName			`thenDs` \lengthP  ->
      unboxAlt						`thenDs` \alt      ->
      returnDs (Case (len lengthP) (mkWildId intTy) [alt])
      where
	elemTy      = case splitTyConApp (idType var) of
		        (_, [elemTy]) -> elemTy
		        _	        -> panic panicMsg
        panicMsg    = "DsUtils.mkCoAlgCaseMatchResult: not a parallel array?"
	len lengthP = mkApps (Var lengthP) [Type elemTy, Var var]
	--
	unboxAlt = 
	  newSysLocalDs intPrimTy			`thenDs` \l	   ->
	  dsLookupGlobalId indexPName		`thenDs` \indexP   ->
	  mapDs (mkAlt indexP) match_alts               `thenDs` \alts     ->
	  returnDs (DataAlt intDataCon, [l], (Case (Var l) wild (dft : alts)))
          where
	    wild = mkWildId intPrimTy
	    dft  = (DEFAULT, [], fail)
	--
	-- each alternative matches one array length (corresponding to one
	-- fake array constructor), so the match is on a literal; each
	-- alternative's body is extended by a local binding for each
	-- constructor argument, which are bound to array elements starting
	-- with the first
	--
	mkAlt indexP (con, args, MatchResult _ bodyFun) = 
	  bodyFun fail					`thenDs` \body     ->
	  returnDs (LitAlt lit, [], mkDsLets binds body)
	  where
	    lit   = MachInt $ toInteger (dataConSourceArity con)
	    binds = [NonRec arg (indexExpr i) | (i, arg) <- zip [1..] args]
	    --
	    indexExpr i = mkApps (Var indexP) [Type elemTy, Var var, mkIntExpr i]
\end{code}


%************************************************************************
%*									*
\subsection{Desugarer's versions of some Core functions}
%*									*
%************************************************************************

\begin{code}
mkErrorAppDs :: Id 		-- The error function
	     -> Type		-- Type to which it should be applied
	     -> String		-- The error message string to pass
	     -> DsM CoreExpr

mkErrorAppDs err_id ty msg
  = getSrcLocDs			`thenDs` \ src_loc ->
    let
	full_msg = showSDoc (hcat [ppr src_loc, text "|", text msg])
	core_msg = Lit (MachStr (mkFastString (stringToUtf8 full_msg)))
    in
    returnDs (mkApps (Var err_id) [Type ty, core_msg])
\end{code}


*************************************************************
%*									*
\subsection{Making literals}
%*									*
%************************************************************************

\begin{code}
mkCharExpr    :: Int	    -> CoreExpr	     -- Returns	C# c :: Int
mkIntExpr     :: Integer    -> CoreExpr	     -- Returns	I# i :: Int
mkIntegerExpr :: Integer    -> DsM CoreExpr  -- Result :: Integer
mkStringLit   :: String     -> DsM CoreExpr  -- Result :: String
mkStringLitFS :: FastString -> DsM CoreExpr  -- Result :: String

mkIntExpr  i = mkConApp intDataCon  [mkIntLit i]
mkCharExpr c = mkConApp charDataCon [mkLit (MachChar c)]

mkIntegerExpr i
  | inIntRange i  	-- Small enough, so start from an Int
  = returnDs (mkSmallIntegerLit i)

-- Special case for integral literals with a large magnitude:
-- They are transformed into an expression involving only smaller
-- integral literals. This improves constant folding.

  | otherwise 		-- Big, so start from a string
  = dsLookupGlobalId plusIntegerName		`thenDs` \ plus_id ->
    dsLookupGlobalId timesIntegerName	`thenDs` \ times_id ->
    let 
        plus a b  = Var plus_id  `App` a `App` b
        times a b = Var times_id `App` a `App` b

	-- Transform i into (x1 + (x2 + (x3 + (...) * b) * b) * b) with abs xi <= b
	horner :: Integer -> Integer -> CoreExpr
	horner b i | abs q <= 1 = if r == 0 || r == i 
				  then mkSmallIntegerLit i 
				  else mkSmallIntegerLit r `plus` mkSmallIntegerLit (i-r)
	           | r == 0     =                   	      horner b q `times` mkSmallIntegerLit b
	           | otherwise  = mkSmallIntegerLit r `plus` (horner b q `times` mkSmallIntegerLit b)
  		   where
		     (q,r) = i `quotRem` b

    in
    returnDs (horner tARGET_MAX_INT i)

mkSmallIntegerLit i = mkConApp smallIntegerDataCon [mkIntLit i]

mkStringLit str	= mkStringLitFS (mkFastString str)

mkStringLitFS str
  | nullFastString str
  = returnDs (mkNilExpr charTy)

  | lengthFS str == 1
  = let
	the_char = mkCharExpr (headIntFS str)
    in
    returnDs (mkConsExpr charTy the_char (mkNilExpr charTy))

  | all safeChar int_chars
  = dsLookupGlobalId unpackCStringName	`thenDs` \ unpack_id ->
    returnDs (App (Var unpack_id) (Lit (MachStr str)))

  | otherwise
  = dsLookupGlobalId unpackCStringUtf8Name	`thenDs` \ unpack_id ->
    returnDs (App (Var unpack_id) (Lit (MachStr (mkFastString (intsToUtf8 int_chars)))))

  where
    int_chars = unpackIntFS str
    safeChar c = c >= 1 && c <= 0xFF
\end{code}


%************************************************************************
%*									*
\subsection[mkSelectorBind]{Make a selector bind}
%*									*
%************************************************************************

This is used in various places to do with lazy patterns.
For each binder $b$ in the pattern, we create a binding:
\begin{verbatim}
    b = case v of pat' -> b'
\end{verbatim}
where @pat'@ is @pat@ with each binder @b@ cloned into @b'@.

ToDo: making these bindings should really depend on whether there's
much work to be done per binding.  If the pattern is complex, it
should be de-mangled once, into a tuple (and then selected from).
Otherwise the demangling can be in-line in the bindings (as here).

Boring!  Boring!  One error message per binder.  The above ToDo is
even more helpful.  Something very similar happens for pattern-bound
expressions.

\begin{code}
mkSelectorBinds :: TypecheckedPat	-- The pattern
		-> CoreExpr    		-- Expression to which the pattern is bound
		-> DsM [(Id,CoreExpr)]

mkSelectorBinds (VarPat v) val_expr
  = returnDs [(v, val_expr)]

mkSelectorBinds pat val_expr
  | isSingleton binders || is_simple_pat pat
  = 	-- Given   p = e, where p binds x,y
	-- we are going to make
	--	v = p	(where v is fresh)
	--	x = case v of p -> x
	--	y = case v of p -> x

	-- Make up 'v'
	-- NB: give it the type of *pattern* p, not the type of the *rhs* e.
	-- This does not matter after desugaring, but there's a subtle 
	-- issue with implicit parameters. Consider
	--	(x,y) = ?i
	-- Then, ?i is given type {?i :: Int}, a SourceType, which is opaque
	-- to the desugarer.  (Why opaque?  Because newtypes have to be.  Why
	-- does it get that type?  So that when we abstract over it we get the
	-- right top-level type  (?i::Int) => ...)
	--
	-- So to get the type of 'v', use the pattern not the rhs.  Often more
	-- efficient too.
    newSysLocalDs (hsPatType pat)	`thenDs` \ val_var ->

	-- For the error message we make one error-app, to avoid duplication.
	-- But we need it at different types... so we use coerce for that
    mkErrorAppDs iRREFUT_PAT_ERROR_ID 
		 unitTy (showSDoc (ppr pat))	`thenDs` \ err_expr ->
    newSysLocalDs unitTy			`thenDs` \ err_var ->
    mapDs (mk_bind val_var err_var) binders	`thenDs` \ binds ->
    returnDs ( (val_var, val_expr) : 
	       (err_var, err_expr) :
	       binds )


  | otherwise
  = mkErrorAppDs iRREFUT_PAT_ERROR_ID 
		 tuple_ty (showSDoc (ppr pat))			`thenDs` \ error_expr ->
    matchSimply val_expr PatBindRhs pat local_tuple error_expr	`thenDs` \ tuple_expr ->
    newSysLocalDs tuple_ty					`thenDs` \ tuple_var ->
    let
	mk_tup_bind binder
	  = (binder, mkTupleSelector binders binder tuple_var (Var tuple_var))
    in
    returnDs ( (tuple_var, tuple_expr) : map mk_tup_bind binders )
  where
    binders	= collectPatBinders pat
    local_tuple = mkTupleExpr binders
    tuple_ty    = exprType local_tuple

    mk_bind scrut_var err_var bndr_var
    -- (mk_bind sv err_var) generates
    --		bv = case sv of { pat -> bv; other -> coerce (type-of-bv) err_var }
    -- Remember, pat binds bv
      = matchSimply (Var scrut_var) PatBindRhs pat
		    (Var bndr_var) error_expr			`thenDs` \ rhs_expr ->
        returnDs (bndr_var, rhs_expr)
      where
        error_expr = mkCoerce (idType bndr_var) (Var err_var)

    is_simple_pat (TuplePat ps Boxed)    = all is_triv_pat ps
    is_simple_pat (ConPatOut _ ps _ _ _) = all is_triv_pat (hsConArgs ps)
    is_simple_pat (VarPat _)	       	 = True
    is_simple_pat (ParPat p)		 = is_simple_pat p
    is_simple_pat other		       	 = False

    is_triv_pat (VarPat v)  = True
    is_triv_pat (WildPat _) = True
    is_triv_pat (ParPat p)  = is_triv_pat p
    is_triv_pat other       = False
\end{code}


%************************************************************************
%*									*
		Tuples
%*									*
%************************************************************************

@mkTupleExpr@ builds a tuple; the inverse to @mkTupleSelector@.  

* If it has only one element, it is the identity function.

* If there are more elements than a big tuple can have, it nests 
  the tuples.  

Nesting policy.  Better a 2-tuple of 10-tuples (3 objects) than
a 10-tuple of 2-tuples (11 objects).  So we want the leaves to be big.

\begin{code}
mkTupleExpr :: [Id] -> CoreExpr
mkTupleExpr ids = mkBigCoreTup (map Var ids)

-- corresponding type
mkTupleType :: [Id] -> Type
mkTupleType ids = mkBigTuple mkCoreTupTy (map idType ids)

mkBigCoreTup :: [CoreExpr] -> CoreExpr
mkBigCoreTup = mkBigTuple mkCoreTup

mkBigTuple :: ([a] -> a) -> [a] -> a
mkBigTuple small_tuple as = mk_big_tuple (chunkify as)
  where
	-- Each sub-list is short enough to fit in a tuple
    mk_big_tuple [as] = small_tuple as
    mk_big_tuple as_s = mk_big_tuple (chunkify (map small_tuple as_s))

chunkify :: [a] -> [[a]]
-- The sub-lists of the result all have length <= mAX_TUPLE_SIZE
-- But there may be more than mAX_TUPLE_SIZE sub-lists
chunkify xs
  | n_xs <= mAX_TUPLE_SIZE = {- pprTrace "Small" (ppr n_xs) -} [xs] 
  | otherwise		   = {- pprTrace "Big"   (ppr n_xs) -} (split xs)
  where
    n_xs     = length xs
    split [] = []
    split xs = take mAX_TUPLE_SIZE xs : split (drop mAX_TUPLE_SIZE xs)
\end{code}


@mkTupleSelector@ builds a selector which scrutises the given
expression and extracts the one name from the list given.
If you want the no-shadowing rule to apply, the caller
is responsible for making sure that none of these names
are in scope.

If there is just one id in the ``tuple'', then the selector is
just the identity.

If it's big, it does nesting
	mkTupleSelector [a,b,c,d] b v e
	  = case e of v { 
		(p,q) -> case p of p {
			   (a,b) -> b }}
We use 'tpl' vars for the p,q, since shadowing does not matter.

In fact, it's more convenient to generate it innermost first, getting

	case (case e of v 
		(p,q) -> p) of p
	  (a,b) -> b

\begin{code}
mkTupleSelector :: [Id]		-- The tuple args
		-> Id		-- The selected one
		-> Id		-- A variable of the same type as the scrutinee
		-> CoreExpr	-- Scrutinee
		-> CoreExpr

mkTupleSelector vars the_var scrut_var scrut
  = mk_tup_sel (chunkify vars) the_var
  where
    mk_tup_sel [vars] the_var = mkCoreSel vars the_var scrut_var scrut
    mk_tup_sel vars_s the_var = mkCoreSel group the_var tpl_v $
				mk_tup_sel (chunkify tpl_vs) tpl_v
	where
	  tpl_tys = [mkCoreTupTy (map idType gp) | gp <- vars_s]
	  tpl_vs  = mkTemplateLocals tpl_tys
	  [(tpl_v, group)] = [(tpl,gp) | (tpl,gp) <- zipEqual "mkTupleSelector" tpl_vs vars_s,
					 the_var `elem` gp ]
\end{code}

A generalization of @mkTupleSelector@, allowing the body
of the case to be an arbitrary expression.

If the tuple is big, it is nested:

	mkTupleCase uniqs [a,b,c,d] body v e
	  = case e of v { (p,q) ->
	    case p of p { (a,b) ->
	    case q of q { (c,d) ->
	    body }}}

To avoid shadowing, we use uniqs to invent new variables p,q.

ToDo: eliminate cases where none of the variables are needed.

\begin{code}
mkTupleCase
	:: UniqSupply	-- for inventing names of intermediate variables
	-> [Id]		-- the tuple args
	-> CoreExpr	-- body of the case
	-> Id		-- a variable of the same type as the scrutinee
	-> CoreExpr	-- scrutinee
	-> CoreExpr

mkTupleCase uniqs vars body scrut_var scrut
  = mk_tuple_case uniqs (chunkify vars) body
  where
    mk_tuple_case us [vars] body
      = mkSmallTupleCase vars body scrut_var scrut
    mk_tuple_case us vars_s body
      = let
	    (us', vars', body') = foldr one_tuple_case (us, [], body) vars_s
	in
	mk_tuple_case us' (chunkify vars') body'
    one_tuple_case chunk_vars (us, vs, body)
      = let
	    (us1, us2) = splitUniqSupply us
	    scrut_var = mkSysLocal FSLIT("ds") (uniqFromSupply us1)
			(mkCoreTupTy (map idType chunk_vars))
	    body' = mkSmallTupleCase chunk_vars body scrut_var (Var scrut_var)
	in (us2, scrut_var:vs, body')
\end{code}

The same, but with a tuple small enough not to need nesting.

\begin{code}
mkSmallTupleCase
	:: [Id]		-- the tuple args
	-> CoreExpr	-- body of the case
	-> Id		-- a variable of the same type as the scrutinee
	-> CoreExpr	-- scrutinee
	-> CoreExpr

mkSmallTupleCase [var] body _scrut_var scrut
  = bindNonRec var scrut body
mkSmallTupleCase vars body scrut_var scrut
  = Case scrut scrut_var [(DataAlt (tupleCon Boxed (length vars)), vars, body)]
\end{code}

%************************************************************************
%*									*
\subsection[mkFailurePair]{Code for pattern-matching and other failures}
%*									*
%************************************************************************

Call the constructor Ids when building explicit lists, so that they
interact well with rules.

\begin{code}
mkNilExpr :: Type -> CoreExpr
mkNilExpr ty = mkConApp nilDataCon [Type ty]

mkConsExpr :: Type -> CoreExpr -> CoreExpr -> CoreExpr
mkConsExpr ty hd tl = mkConApp consDataCon [Type ty, hd, tl]

mkListExpr :: Type -> [CoreExpr] -> CoreExpr
mkListExpr ty xs = foldr (mkConsExpr ty) (mkNilExpr ty) xs
			    

-- The next three functions make tuple types, constructors and selectors,
-- with the rule that a 1-tuple is represented by the thing itselg
mkCoreTupTy :: [Type] -> Type
mkCoreTupTy [ty] = ty
mkCoreTupTy tys  = mkTupleTy Boxed (length tys) tys

mkCoreTup :: [CoreExpr] -> CoreExpr			    
-- Builds exactly the specified tuple.
-- No fancy business for big tuples
mkCoreTup []  = Var unitDataConId
mkCoreTup [c] = c
mkCoreTup cs  = mkConApp (tupleCon Boxed (length cs))
			 (map (Type . exprType) cs ++ cs)

mkCoreSel :: [Id]	-- The tuple args
	  -> Id		-- The selected one
	  -> Id		-- A variable of the same type as the scrutinee
	  -> CoreExpr	-- Scrutinee
	  -> CoreExpr
-- mkCoreSel [x,y,z] x v e
-- ===>  case e of v { (x,y,z) -> x
mkCoreSel [var] should_be_the_same_var scrut_var scrut
  = ASSERT(var == should_be_the_same_var)
    scrut

mkCoreSel vars the_var scrut_var scrut
  = ASSERT( notNull vars )
    Case scrut scrut_var 
	 [(DataAlt (tupleCon Boxed (length vars)), vars, Var the_var)]
\end{code}


%************************************************************************
%*									*
\subsection[mkFailurePair]{Code for pattern-matching and other failures}
%*									*
%************************************************************************

Generally, we handle pattern matching failure like this: let-bind a
fail-variable, and use that variable if the thing fails:
\begin{verbatim}
	let fail.33 = error "Help"
	in
	case x of
		p1 -> ...
		p2 -> fail.33
		p3 -> fail.33
		p4 -> ...
\end{verbatim}
Then
\begin{itemize}
\item
If the case can't fail, then there'll be no mention of @fail.33@, and the
simplifier will later discard it.

\item
If it can fail in only one way, then the simplifier will inline it.

\item
Only if it is used more than once will the let-binding remain.
\end{itemize}

There's a problem when the result of the case expression is of
unboxed type.  Then the type of @fail.33@ is unboxed too, and
there is every chance that someone will change the let into a case:
\begin{verbatim}
	case error "Help" of
	  fail.33 -> case ....
\end{verbatim}

which is of course utterly wrong.  Rather than drop the condition that
only boxed types can be let-bound, we just turn the fail into a function
for the primitive case:
\begin{verbatim}
	let fail.33 :: Void -> Int#
	    fail.33 = \_ -> error "Help"
	in
	case x of
		p1 -> ...
		p2 -> fail.33 void
		p3 -> fail.33 void
		p4 -> ...
\end{verbatim}

Now @fail.33@ is a function, so it can be let-bound.

\begin{code}
mkFailurePair :: CoreExpr	-- Result type of the whole case expression
	      -> DsM (CoreBind,	-- Binds the newly-created fail variable
				-- to either the expression or \ _ -> expression
		      CoreExpr)	-- Either the fail variable, or fail variable
				-- applied to unit tuple
mkFailurePair expr
  | isUnLiftedType ty
  = newFailLocalDs (unitTy `mkFunTy` ty)	`thenDs` \ fail_fun_var ->
    newSysLocalDs unitTy			`thenDs` \ fail_fun_arg ->
    returnDs (NonRec fail_fun_var (Lam fail_fun_arg expr),
	      App (Var fail_fun_var) (Var unitDataConId))

  | otherwise
  = newFailLocalDs ty 		`thenDs` \ fail_var ->
    returnDs (NonRec fail_var expr, Var fail_var)
  where
    ty = exprType expr
\end{code}


