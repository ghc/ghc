%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[DsUtils]{Utilities for desugaring}

This module exports some utility functions of no great interest.

\begin{code}
#include "HsVersions.h"

module DsUtils (
	CanItFail(..), EquationInfo(..), MatchResult(..),

	combineGRHSMatchResults,
	combineMatchResults,
	dsExprToAtom,
	mkCoAlgCaseMatchResult,
	mkAppDs, mkConDs, mkPrimDs,
	mkCoLetsMatchResult,
	mkCoPrimCaseMatchResult,
	mkFailurePair,
	mkGuardedMatchResult,
	mkSelectorBinds,
	mkTupleBind,
	mkTupleExpr,
	selectMatchVars
    ) where

import Ubiq
import DsLoop		( match, matchSimply )

import HsSyn		( HsExpr(..), OutPat(..), HsLit(..),
			  Match, HsBinds, Stmt, Qual, PolyType, ArithSeqInfo )
import TcHsSyn		( TypecheckedPat(..) )
import DsHsSyn		( outPatType )
import CoreSyn

import DsMonad

import CoreUtils	( coreExprType, escErrorMsg, mkCoreIfThenElse, mkErrorApp )
import PrelInfo		( stringTy )
import Id		( idType, getInstantiatedDataConSig, mkTupleCon,
			  DataCon(..), DictVar(..), Id(..), GenId )
import TyCon		( mkTupleTyCon )
import Type		( mkTyVarTys, mkRhoTy, mkFunTys,
			  applyTyCon, getAppDataTyCon )
import UniqSet		( mkUniqSet, minusUniqSet, uniqSetToList, UniqSet(..) )
import Util		( panic, assertPanic )

isUnboxedDataType = panic "DsUtils.isUnboxedDataType"
quantifyTy = panic "DsUtils.quantifyTy"
splitDictType = panic "DsUtils.splitDictType"
mkCoTyApps = panic "DsUtils.mkCoTyApps"
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
data EquationInfo
  = EqnInfo
	[TypecheckedPat]    -- the patterns for an eqn
      	MatchResult	    -- Encapsulates the guards and bindings
\end{code}

\begin{code}
data MatchResult
  = MatchResult
	CanItFail
	Type		-- Type of argument expression

	(CoreExpr -> CoreExpr)
			-- Takes a expression to plug in at the
			-- failure point(s). The expression should
			-- be duplicatable!

	DsMatchContext	-- The context info is used when producing warnings
			-- about shadowed patterns.  It's the context
			-- of the *first* thing matched in this group.
			-- Should perhaps be a list of them all!

data CanItFail = CanFail | CantFail

orFail CantFail CantFail = CantFail
orFail _        _	 = CanFail


mkCoLetsMatchResult :: [CoreBinding] -> MatchResult -> MatchResult
mkCoLetsMatchResult binds (MatchResult can_it_fail ty body_fn cxt)
  = MatchResult can_it_fail ty (\body -> mkCoLetsAny binds (body_fn body)) cxt

mkGuardedMatchResult :: CoreExpr -> MatchResult -> DsM MatchResult
mkGuardedMatchResult pred_expr (MatchResult can_it_fail ty body_fn cxt)
  = returnDs (MatchResult CanFail
			  ty
			  (\fail -> mkCoreIfThenElse pred_expr (body_fn fail) fail)
			  cxt
    )

mkCoPrimCaseMatchResult :: Id				-- Scrutinee
		    -> [(Literal, MatchResult)]	-- Alternatives
		    -> DsM MatchResult
mkCoPrimCaseMatchResult var alts
  = newSysLocalDs (idType var)	`thenDs` \ wild ->
    returnDs (MatchResult CanFail
			  ty1
			  (mk_case alts wild)
			  cxt1)
  where
    ((_,MatchResult _ ty1 _ cxt1) : _) = alts

    mk_case alts wild fail_expr
      = Case (Var var) (PrimAlts final_alts (BindDefault wild fail_expr))
      where
	final_alts = [ (lit, body_fn fail_expr)
		     | (lit, MatchResult _ _ body_fn _) <- alts
		     ]


mkCoAlgCaseMatchResult :: Id				-- Scrutinee
		    -> [(DataCon, [Id], MatchResult)]	-- Alternatives
		    -> DsM MatchResult
mkCoAlgCaseMatchResult var alts
  =	    -- Find all the constructors in the type which aren't
	    -- explicitly mentioned in the alternatives:
    case un_mentioned_constructors of
	[] ->	-- All constructors mentioned, so no default needed
		returnDs (MatchResult can_any_alt_fail
			  	      ty1
				      (mk_case alts (\ignore -> NoDefault))
				      cxt1)

	[con] ->     -- Just one constructor missing, so add a case for it
		     -- We need to build new locals for the args of the constructor,
		     -- and figuring out their types is somewhat tiresome.
		let
			(_,arg_tys,_) = getInstantiatedDataConSig con tycon_arg_tys
		in
		newSysLocalsDs arg_tys	`thenDs` \ arg_ids ->

		     -- Now we are ready to construct the new alternative
		let
			new_alt = (con, arg_ids, MatchResult CanFail ty1 id NoMatchContext)
		in
		returnDs (MatchResult CanFail
			  	      ty1
				      (mk_case (new_alt:alts) (\ignore -> NoDefault))
				      cxt1)

	other ->      -- Many constructors missing, so use a default case
		newSysLocalDs scrut_ty		`thenDs` \ wild ->
		returnDs (MatchResult CanFail
			  	      ty1
				      (mk_case alts (\fail_expr -> BindDefault wild fail_expr))
				      cxt1)
  where
    scrut_ty = idType var
    (tycon, tycon_arg_tys, data_cons) = getAppDataTyCon scrut_ty

    un_mentioned_constructors
      = uniqSetToList (mkUniqSet data_cons `minusUniqSet` mkUniqSet [ con | (con, _, _) <- alts] )

    match_results = [match_result | (_,_,match_result) <- alts]
    (MatchResult _ ty1 _ cxt1 : _) = match_results
    can_any_alt_fail = foldr1 orFail [can_it_fail | MatchResult can_it_fail _ _ _ <- match_results]

    mk_case alts deflt_fn fail_expr
      = Case (Var var) (AlgAlts final_alts (deflt_fn fail_expr))
      where
	final_alts = [ (con, args, body_fn fail_expr)
		     | (con, args, MatchResult _ _ body_fn _) <- alts
		     ]


combineMatchResults :: MatchResult -> MatchResult -> DsM MatchResult
combineMatchResults (MatchResult CanFail      ty1 body_fn1 cxt1)
		    (MatchResult can_it_fail2 ty2 body_fn2 cxt2)
  = mkFailurePair ty1		`thenDs` \ (bind_fn, duplicatable_expr) ->
    let
	new_body_fn1 = \body1 -> Let (bind_fn body1) (body_fn1 duplicatable_expr)
	new_body_fn2 = \body2 -> new_body_fn1 (body_fn2 body2)
    in
    returnDs (MatchResult can_it_fail2 ty1 new_body_fn2 cxt1)

combineMatchResults match_result1@(MatchResult CantFail ty body_fn1 cxt1)
				  match_result2
  = returnDs match_result1


-- The difference in combineGRHSMatchResults is that there is no
-- need to let-bind to avoid code duplication
combineGRHSMatchResults :: MatchResult -> MatchResult -> DsM MatchResult
combineGRHSMatchResults (MatchResult CanFail     ty1 body_fn1 cxt1)
			(MatchResult can_it_fail ty2 body_fn2 cxt2)
  = returnDs (MatchResult can_it_fail ty1 (\ body -> body_fn1 (body_fn2 body)) cxt1)

combineGRHSMatchResults match_result1 match_result2
  = 	-- Delegate to avoid duplication of code
    combineMatchResults match_result1 match_result2
\end{code}

%************************************************************************
%*									*
\subsection[dsExprToAtom]{Take an expression and produce an atom}
%*									*
%************************************************************************

\begin{code}
dsExprToAtom :: CoreExpr		    -- The argument expression
	     -> (CoreArg -> DsM CoreExpr)   -- Something taking the argument *atom*,
					    -- and delivering an expression E
	     -> DsM CoreExpr		    -- Either E or let x=arg-expr in E

dsExprToAtom (Var v) continue_with = continue_with (VarArg v)
dsExprToAtom (Lit v) continue_with = continue_with (LitArg v)

dsExprToAtom arg_expr continue_with
  = let
	ty = coreExprType arg_expr
    in
    newSysLocalDs ty			`thenDs` \ arg_id ->
    continue_with (VarArg arg_id)	`thenDs` \ body   ->
    returnDs (
	if isUnboxedDataType ty
	then Case arg_expr (PrimAlts [] (BindDefault arg_id body))
	else Let (NonRec arg_id arg_expr) body
    )

dsExprsToAtoms :: [CoreExpr]
	       -> ([CoreArg] -> DsM CoreExpr)
	       -> DsM CoreExpr

dsExprsToAtoms [] continue_with
  = continue_with []

dsExprsToAtoms (arg:args) continue_with
  = dsExprToAtom   arg 	$ \ arg_atom  ->
    dsExprsToAtoms args $ \ arg_atoms ->
    continue_with (arg_atom:arg_atoms)
\end{code}

%************************************************************************
%*									*
\subsection{Desugarer's versions of some Core functions}
%*									*
%************************************************************************

Plumb the desugarer's @UniqueSupply@ in/out of the @UniqSupply@ monad
world.
\begin{code}
mkAppDs  :: CoreExpr -> [Type] -> [CoreExpr] -> DsM CoreExpr
mkConDs  :: Id       -> [Type] -> [CoreExpr] -> DsM CoreExpr
mkPrimDs :: PrimOp   -> [Type] -> [CoreExpr] -> DsM CoreExpr

mkAppDs fun tys arg_exprs 
  = dsExprsToAtoms arg_exprs $ \ vals ->
    returnDs (mkApp fun [] tys vals)

mkConDs con tys arg_exprs
  = dsExprsToAtoms arg_exprs $ \ vals ->
    returnDs (mkCon con [] tys vals)

mkPrimDs op tys arg_exprs
  = dsExprsToAtoms arg_exprs $ \ vals ->
    returnDs (mkPrim op [] tys vals)
\end{code}

%************************************************************************
%*									*
\subsection[mkSelectorBind]{Make a selector bind}
%*									*
%************************************************************************

This is used in various places to do with lazy patterns.
For each binder $b$ in the pattern, we create a binding:

    b = case v of pat' -> b'

where pat' is pat with each binder b cloned into b'.

ToDo: making these bindings should really depend on whether there's
much work to be done per binding.  If the pattern is complex, it
should be de-mangled once, into a tuple (and then selected from).
Otherwise the demangling can be in-line in the bindings (as here).

Boring!  Boring!  One error message per binder.  The above ToDo is
even more helpful.  Something very similar happens for pattern-bound
expressions.

\begin{code}
mkSelectorBinds :: [TyVar]	    -- Variables wrt which the pattern is polymorphic
		-> TypecheckedPat   -- The pattern
		-> [(Id,Id)]	    -- Monomorphic and polymorphic binders for
				    -- the pattern
		-> CoreExpr    -- Expression to which the pattern is bound
		-> DsM [(Id,CoreExpr)]

mkSelectorBinds tyvars pat locals_and_globals val_expr
  = getSrcLocDs		`thenDs` \ (src_file, src_line) ->

    if is_simple_tuple_pat pat then
	mkTupleBind tyvars [] locals_and_globals val_expr
    else
	newSysLocalDs stringTy	`thenDs` \ str_var -> -- to hold the string
	let
	    src_loc_str   = escErrorMsg ('"' : src_file) ++ "%l" ++ src_line
	    error_string  = src_loc_str ++ "%~" --> ": pattern-match failed on an irrefutable pattern"
	    error_msg     = mkErrorApp res_ty str_var error_string
	in
	matchSimply val_expr pat res_ty local_tuple error_msg `thenDs` \ tuple_expr ->
	mkTupleBind tyvars [] locals_and_globals tuple_expr
  where
    locals	= [local | (local, _) <- locals_and_globals]
    local_tuple = mkTupleExpr locals
    res_ty      = coreExprType local_tuple

    is_simple_tuple_pat (TuplePat ps) = all is_var_pat ps
    is_simple_tuple_pat other         = False

    is_var_pat (VarPat v) = True
    is_var_pat other      = False -- Even wild-card patterns aren't acceptable
\end{code}

We're about to match against some patterns.  We want to make some
@Ids@ to use as match variables.  If a pattern has an @Id@ readily at
hand, which should indeed be bound to the pattern as a whole, then use it;
otherwise, make one up.
\begin{code}
selectMatchVars :: [TypecheckedPat] -> DsM [Id]
selectMatchVars pats
  = mapDs var_from_pat_maybe pats
  where
    var_from_pat_maybe (VarPat var)	= returnDs var
    var_from_pat_maybe (AsPat var pat)	= returnDs var
    var_from_pat_maybe (LazyPat pat)	= var_from_pat_maybe pat
    var_from_pat_maybe other_pat
      = newSysLocalDs (outPatType other_pat) -- OK, better make up one...
\end{code}

\begin{code}
mkTupleBind :: [TyVar]	    -- Abstract wrt these...
	-> [DictVar]	    -- ... and these

	-> [(Id, Id)]	    -- Local, global pairs, equal in number
			    -- to the size of the tuple.  The types
			    -- of the globals is the generalisation of
			    -- the corresp local, wrt the tyvars and dicts

	-> CoreExpr    -- Expr whose value is a tuple; the expression
			    -- may mention the tyvars and dicts

	-> DsM [(Id, CoreExpr)]	-- Bindings for the globals
\end{code}

The general call is
\begin{verbatim}
	mkTupleBind tyvars dicts [(l1,g1), ..., (ln,gn)] tup_expr
\end{verbatim}
If $n=1$, the result is:
\begin{verbatim}
	g1 = /\ tyvars -> \ dicts -> rhs
\end{verbatim}
Otherwise, the result is:
\begin{verbatim}
	tup = /\ tyvars -> \ dicts -> tup_expr
	g1  = /\ tyvars -> \ dicts -> case (tup tyvars dicts) of
					(l1, ..., ln) -> l1
	...etc...
\end{verbatim}

\begin{code}
mkTupleBind tyvars dicts [(local,global)] tuple_expr
  = returnDs [(global, mkLam tyvars dicts tuple_expr)]
\end{code}

The general case:

\begin{code}
mkTupleBind tyvars dicts local_global_prs tuple_expr
  = newSysLocalDs tuple_var_ty	`thenDs` \ tuple_var ->

    zipWithDs (mk_selector (Var tuple_var))
	      local_global_prs
	      [(0::Int) .. (length local_global_prs - 1)]
				`thenDs` \ tup_selectors ->
    returnDs (
	(tuple_var, mkLam tyvars dicts tuple_expr)
	: tup_selectors
    )
  where
    locals, globals :: [Id]
    locals  = [local  | (local,global) <- local_global_prs]
    globals = [global | (local,global) <- local_global_prs]

    no_of_binders = length local_global_prs
    tyvar_tys = mkTyVarTys tyvars

    tuple_var_ty :: Type
    tuple_var_ty
      = case (quantifyTy tyvars (mkRhoTy theta
				  (applyTyCon (mkTupleTyCon no_of_binders)
					      (map idType locals)))) of
	  (_{-tossed templates-}, ty) -> ty
      where
	theta = map (splitDictType . idType) dicts

    mk_selector :: CoreExpr -> (Id, Id) -> Int -> DsM (Id, CoreExpr)

    mk_selector tuple_var_expr (local, global) which_local
      = mapDs duplicateLocalDs locals{-the whole bunch-} `thenDs` \ binders ->
	let
	    selected = binders !! which_local
	in
	returnDs (
	    global,
	    mkLam tyvars dicts (
		mkTupleSelector (mkApp_XX (mkCoTyApps tuple_var_expr tyvar_tys) dicts)
				binders selected)
	)

mkApp_XX :: CoreExpr -> [Id] -> CoreExpr
mkApp_XX expr []	 = expr
mkApp_XX expr (id:ids) = mkApp_XX (App expr (VarArg id)) ids
\end{code}



@mkTupleExpr@ builds a tuple; the inverse to @mkTupleSelector@.  If it
has only one element, it is the identity function.
\begin{code}
mkTupleExpr :: [Id] -> CoreExpr

mkTupleExpr []	 = Con (mkTupleCon 0) []
mkTupleExpr [id] = Var id
mkTupleExpr ids	 = mkCon (mkTupleCon (length ids))
			 [{-usages-}]
			 (map idType ids)
			 [ VarArg i | i <- ids ]
\end{code}


@mkTupleSelector@ builds a selector which scrutises the given
expression and extracts the one name from the list given.
If you want the no-shadowing rule to apply, the caller
is responsible for making sure that none of these names
are in scope.

If there is just one id in the ``tuple'', then the selector is
just the identity.

\begin{code}
mkTupleSelector :: CoreExpr	-- Scrutinee
		-> [Id]			-- The tuple args
		-> Id			-- The selected one
		-> CoreExpr

mkTupleSelector expr [] the_var = panic "mkTupleSelector"

mkTupleSelector expr [var] should_be_the_same_var
  = ASSERT(var == should_be_the_same_var)
    expr

mkTupleSelector expr vars the_var
 = Case expr (AlgAlts [(mkTupleCon arity, vars, Var the_var)]
			  NoDefault)
 where
   arity = length vars
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
If the case can't fail, then there'll be no mention of fail.33, and the
simplifier will later discard it.

\item
If it can fail in only one way, then the simplifier will inline it.

\item
Only if it is used more than once will the let-binding remain.
\end{itemize}

There's a problem when the result of the case expression is of
unboxed type.  Then the type of fail.33 is unboxed too, and
there is every chance that someone will change the let into a case:
\begin{verbatim}
	case error "Help" of
	  fail.33 -> case ....
\end{verbatim}

which is of course utterly wrong.  Rather than drop the condition that
only boxed types can be let-bound, we just turn the fail into a function
for the primitive case:
\begin{verbatim}
	let fail.33 :: () -> Int#
	    fail.33 = \_ -> error "Help"
	in
	case x of
		p1 -> ...
		p2 -> fail.33 ()
		p3 -> fail.33 ()
		p4 -> ...
\end{verbatim}

Now fail.33 is a function, so it can be let-bound.

\begin{code}
mkFailurePair :: Type		-- Result type of the whole case expression
	      -> DsM (CoreExpr -> CoreBinding,
				-- Binds the newly-created fail variable
				-- to either the expression or \ _ -> expression
		      CoreExpr)	-- Either the fail variable, or fail variable
				-- applied to unit tuple
mkFailurePair ty
  | isUnboxedDataType ty
  = newFailLocalDs (mkFunTys [unit_ty] ty)	`thenDs` \ fail_fun_var ->
    newSysLocalDs unit_ty			`thenDs` \ fail_fun_arg ->
    returnDs (\ body ->
		NonRec fail_fun_var (Lam (ValBinder fail_fun_arg) body),
	      App (Var fail_fun_var) (VarArg unit_id))

  | otherwise
  = newFailLocalDs ty 		`thenDs` \ fail_var ->
    returnDs (\ body -> NonRec fail_var body, Var fail_var)

unit_id :: Id	-- out here to avoid CAF (sigh)
unit_id = mkTupleCon 0

unit_ty :: Type
unit_ty = idType unit_id
\end{code}
