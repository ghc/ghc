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

	mkErrorAppDs, mkNilExpr, mkConsExpr,
	mkStringLit, mkStringLitFS, mkIntegerLit, 

	mkSelectorBinds, mkTupleExpr, mkTupleSelector,

	selectMatchVar
    ) where

#include "HsVersions.h"

import {-# SOURCE #-} Match ( matchSimply )

import HsSyn
import TcHsSyn		( TypecheckedPat )
import DsHsSyn		( outPatType, collectTypedPatBinders )
import CoreSyn

import DsMonad

import CoreUtils	( exprType, mkIfThenElse )
import PrelInfo		( iRREFUT_PAT_ERROR_ID )
import MkId		( rebuildConArgs )
import Id		( idType, Id, mkWildId )
import Literal		( Literal(..), inIntRange, tARGET_MAX_INT )
import TyCon		( isNewTyCon, tyConDataCons, isRecursiveTyCon )
import DataCon		( DataCon, dataConStrictMarks, dataConId )
import TcType		( mkFunTy, isUnLiftedType, Type )
import TcType		( tcSplitTyConApp, isIntTy, isFloatTy, isDoubleTy )
import TysPrim		( intPrimTy, charPrimTy, floatPrimTy, doublePrimTy )
import TysWiredIn	( nilDataCon, consDataCon, 
                          tupleCon,
			  unitDataConId, unitTy,
                          charTy, charDataCon, 
                          intDataCon, smallIntegerDataCon, 
			  floatDataCon, 
                          doubleDataCon,
			  stringTy
			)
import BasicTypes	( Boxity(..) )
import UniqSet		( mkUniqSet, minusUniqSet, isEmptyUniqSet, UniqSet )
import PrelNames	( unpackCStringName, unpackCStringUtf8Name, 
			  plusIntegerName, timesIntegerName )
import Outputable
import UnicodeUtil      ( stringToUtf8 )
\end{code}



%************************************************************************
%*									*
\subsection{Tidying lit pats}
%*									*
%************************************************************************

\begin{code}
tidyLitPat :: HsLit -> TypecheckedPat -> TypecheckedPat
tidyLitPat (HsChar c) pat = ConPat charDataCon   charTy [] [] [LitPat (HsCharPrim c)   charPrimTy]
tidyLitPat lit        pat = pat

tidyNPat :: HsLit -> Type -> TypecheckedPat -> TypecheckedPat
tidyNPat (HsString s) _ pat
  | _LENGTH_ s <= 1	-- Short string literals only
  = foldr (\c pat -> ConPat consDataCon stringTy [] [] [mk_char_lit c,pat])
	  (ConPat nilDataCon stringTy [] [] []) (_UNPK_INT_ s)
	-- The stringTy is the type of the whole pattern, not 
	-- the type to instantiate (:) or [] with!
  where
    mk_char_lit c = ConPat charDataCon charTy [] [] [LitPat (HsCharPrim c) charPrimTy]

tidyNPat lit lit_ty default_pat
  | isIntTy lit_ty     	= ConPat intDataCon    lit_ty [] [] [LitPat (mk_int lit)    intPrimTy]
  | isFloatTy lit_ty  	= ConPat floatDataCon  lit_ty [] [] [LitPat (mk_float lit)  floatPrimTy]
  | isDoubleTy lit_ty 	= ConPat doubleDataCon lit_ty [] [] [LitPat (mk_double lit) doublePrimTy]
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
selectMatchVar other_pat       	= newSysLocalDs (outPatType other_pat) -- OK, better make up one...
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

  | otherwise			-- Datatype case; use a case
  = MatchResult fail_flag mk_case
  where
	-- Common stuff
    scrut_ty    = idType var
    (tycon, _)  = tcSplitTyConApp scrut_ty		-- Newtypes must be opaque here

	-- Stuff for newtype
    (_, arg_ids, match_result) = head match_alts
    arg_id 	       	       = head arg_ids

    newtype_rhs | isRecursiveTyCon tycon 	-- Recursive case; need a case
	 	= Note (Coerce (idType arg_id) scrut_ty) (Var var)
		| otherwise			-- Normal case (newtype is transparent)
		= Var var
		
	-- Stuff for data types
    data_cons = tyConDataCons tycon

    match_results             = [match_result | (_,_,match_result) <- match_alts]

    fail_flag | exhaustive_case
	      = foldr1 orFail [can_it_fail | MatchResult can_it_fail _ <- match_results]
	      | otherwise
	      = CanFail

    wild_var = mkWildId (idType var)
    mk_case fail = mapDs (mk_alt fail) match_alts	`thenDs` \ alts ->
		   returnDs (Case (Var var) wild_var (mk_default fail ++ alts))

    mk_alt fail (con, args, MatchResult _ body_fn)
	= body_fn fail						`thenDs` \ body ->
	  getUniquesDs						`thenDs` \ us ->
	  let
	     (binds, real_args) = rebuildConArgs args (dataConStrictMarks con) us
	  in
	  returnDs (DataAlt con, real_args, mkDsLets binds body)

    mk_default fail | exhaustive_case = []
		    | otherwise       = [(DEFAULT, [], fail)]

    un_mentioned_constructors
        = mkUniqSet data_cons `minusUniqSet` mkUniqSet [ con | (con, _, _) <- match_alts]
    exhaustive_case = isEmptyUniqSet un_mentioned_constructors
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
    in
    mkStringLit full_msg		`thenDs` \ core_msg ->
    returnDs (mkApps (Var err_id) [Type ty, core_msg])
\end{code}


*************************************************************
%*									*
\subsection{Making literals}
%*									*
%************************************************************************

\begin{code}
mkIntegerLit :: Integer -> DsM CoreExpr
mkIntegerLit i
  | inIntRange i  	-- Small enough, so start from an Int
  = returnDs (mkSmallIntegerLit i)

-- Special case for integral literals with a large magnitude:
-- They are transformed into an expression involving only smaller
-- integral literals. This improves constant folding.

  | otherwise 		-- Big, so start from a string
  = dsLookupGlobalValue plusIntegerName		`thenDs` \ plus_id ->
    dsLookupGlobalValue timesIntegerName	`thenDs` \ times_id ->
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

mkStringLit   :: String       -> DsM CoreExpr
mkStringLit str	= mkStringLitFS (_PK_ str)

mkStringLitFS :: FAST_STRING  -> DsM CoreExpr
mkStringLitFS str
  | _NULL_ str
  = returnDs (mkNilExpr charTy)

  | _LENGTH_ str == 1
  = let
	the_char = mkConApp charDataCon [mkLit (MachChar (_HEAD_INT_ str))]
    in
    returnDs (mkConsExpr charTy the_char (mkNilExpr charTy))

  | all safeChar chars
  = dsLookupGlobalValue unpackCStringName	`thenDs` \ unpack_id ->
    returnDs (App (Var unpack_id) (Lit (MachStr str)))

  | otherwise
  = dsLookupGlobalValue unpackCStringUtf8Name	`thenDs` \ unpack_id ->
    returnDs (App (Var unpack_id) (Lit (MachStr (_PK_ (stringToUtf8 chars)))))

  where
    chars = _UNPK_INT_ str
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
  | length binders == 1 || is_simple_pat pat
  = newSysLocalDs (exprType val_expr)	`thenDs` \ val_var ->

	-- For the error message we don't use mkErrorAppDs to avoid
	-- duplicating the string literal each time
    newSysLocalDs stringTy			`thenDs` \ msg_var ->
    getSrcLocDs					`thenDs` \ src_loc ->
    let
	full_msg = showSDoc (hcat [ppr src_loc, text "|", ppr pat])
    in
    mkStringLit full_msg			`thenDs` \ core_msg -> 
    mapDs (mk_bind val_var msg_var) binders	`thenDs` \ binds ->
    returnDs ( (val_var, val_expr) : 
	       (msg_var, core_msg) :
	       binds )


  | otherwise
  = mkErrorAppDs iRREFUT_PAT_ERROR_ID tuple_ty (showSDoc (ppr pat))
    `thenDs` \ error_expr ->
    matchSimply val_expr PatBindRhs pat local_tuple error_expr
    `thenDs` \ tuple_expr ->
    newSysLocalDs tuple_ty
    `thenDs` \ tuple_var ->
    let
	mk_tup_bind binder =
	  (binder, mkTupleSelector binders binder tuple_var (Var tuple_var))
    in
    returnDs ( (tuple_var, tuple_expr) : map mk_tup_bind binders )
  where
    binders	= collectTypedPatBinders pat
    local_tuple = mkTupleExpr binders
    tuple_ty    = exprType local_tuple

    mk_bind scrut_var msg_var bndr_var
    -- (mk_bind sv bv) generates
    --		bv = case sv of { pat -> bv; other -> error-msg }
    -- Remember, pat binds bv
      = matchSimply (Var scrut_var) PatBindRhs pat
		    (Var bndr_var) error_expr			`thenDs` \ rhs_expr ->
        returnDs (bndr_var, rhs_expr)
      where
        binder_ty = idType bndr_var
        error_expr = mkApps (Var iRREFUT_PAT_ERROR_ID) [Type binder_ty, Var msg_var]

    is_simple_pat (TuplePat ps Boxed)  = all is_triv_pat ps
    is_simple_pat (ConPat _ _ _ _ ps)  = all is_triv_pat ps
    is_simple_pat (VarPat _)	       = True
    is_simple_pat (RecPat _ _ _ _ ps)  = and [is_triv_pat p | (_,p,_) <- ps]
    is_simple_pat other		       = False

    is_triv_pat (VarPat v)  = True
    is_triv_pat (WildPat _) = True
    is_triv_pat other       = False
\end{code}


@mkTupleExpr@ builds a tuple; the inverse to @mkTupleSelector@.  If it
has only one element, it is the identity function.

\begin{code}
mkTupleExpr :: [Id] -> CoreExpr

mkTupleExpr []	 = Var unitDataConId
mkTupleExpr [id] = Var id
mkTupleExpr ids	 = mkConApp (tupleCon Boxed (length ids))
			    (map (Type . idType) ids ++ [ Var i | i <- ids ])
\end{code}


@mkTupleSelector@ builds a selector which scrutises the given
expression and extracts the one name from the list given.
If you want the no-shadowing rule to apply, the caller
is responsible for making sure that none of these names
are in scope.

If there is just one id in the ``tuple'', then the selector is
just the identity.

\begin{code}
mkTupleSelector :: [Id]		-- The tuple args
		-> Id		-- The selected one
		-> Id		-- A variable of the same type as the scrutinee
		-> CoreExpr	-- Scrutinee
		-> CoreExpr

mkTupleSelector [var] should_be_the_same_var scrut_var scrut
  = ASSERT(var == should_be_the_same_var)
    scrut

mkTupleSelector vars the_var scrut_var scrut
  = ASSERT( not (null vars) )
    Case scrut scrut_var [(DataAlt (tupleCon Boxed (length vars)), vars, Var the_var)]
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
mkNilExpr ty = App (Var (dataConId nilDataCon)) (Type ty)

mkConsExpr :: Type -> CoreExpr -> CoreExpr -> CoreExpr
mkConsExpr ty hd tl = mkApps (Var (dataConId consDataCon)) [Type ty, hd, tl]
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



