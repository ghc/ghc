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
	dsExprToAtom, DsCoreArg(..),
	mkCoAlgCaseMatchResult,
	mkAppDs, mkConDs, mkPrimDs, mkErrorAppDs,
	mkCoLetsMatchResult,
	mkCoPrimCaseMatchResult,
	mkFailurePair,
	mkGuardedMatchResult,
	mkSelectorBinds,
	mkTupleBind,
	mkTupleExpr,
	selectMatchVars,
	showForErr
    ) where

IMP_Ubiq()
IMPORT_DELOOPER(DsLoop)		( match, matchSimply )

import HsSyn		( HsExpr(..), OutPat(..), HsLit(..),
			  Match, HsBinds, Stmt, Qualifier, PolyType, ArithSeqInfo )
import TcHsSyn		( TypecheckedPat(..) )
import DsHsSyn		( outPatType )
import CoreSyn

import DsMonad

import CoreUtils	( coreExprType, mkCoreIfThenElse )
import PprStyle		( PprStyle(..) )
import PrelVals		( iRREFUT_PAT_ERROR_ID, voidId )
import Pretty		( ppShow )
import Id		( idType, dataConArgTys, mkTupleCon,
			  pprId{-ToDo:rm-},
			  DataCon(..), DictVar(..), Id(..), GenId )
import Literal		( Literal(..) )
import TyCon		( mkTupleTyCon, isNewTyCon, tyConDataCons )
import Type		( mkTyVarTys, mkRhoTy, mkForAllTys, mkFunTys,
			  mkTheta, isUnboxedType, applyTyCon, getAppTyCon
			)
import TysPrim		( voidTy )
import UniqSet		( mkUniqSet, minusUniqSet, uniqSetToList, UniqSet(..) )
import Util		( panic, assertPanic, pprTrace{-ToDo:rm-} )
import PprCore{-ToDo:rm-}
--import PprType--ToDo:rm
import Pretty--ToDo:rm
import TyVar--ToDo:rm
import Unique--ToDo:rm
import Usage--ToDo:rm
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
  | isNewTyCon tycon		-- newtype case; use a let
  = ASSERT( newtype_sanity )
    returnDs (mkCoLetsMatchResult [coercion_bind] match_result)

  | otherwise			-- datatype case  
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
			arg_tys = dataConArgTys con tycon_arg_tys
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
	-- Common stuff
    scrut_ty = idType var
    (tycon, tycon_arg_tys) = --pprTrace "CoAlgCase:" (pprType PprDebug scrut_ty) $ 
			     getAppTyCon scrut_ty

	-- Stuff for newtype
    (con_id, arg_ids, match_result) = head alts
    arg_id 	   		    = head arg_ids
    coercion_bind		    = NonRec arg_id (Coerce (CoerceOut con_id) 
							    (idType arg_id)
							    (Var var))
    newtype_sanity		    = null (tail alts) && null (tail arg_ids)

	-- Stuff for data types
    data_cons = tyConDataCons tycon

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
dsExprToAtom :: DsCoreArg		    -- The argument expression
	     -> (CoreArg -> DsM CoreExpr)   -- Something taking the argument *atom*,
					    -- and delivering an expression E
	     -> DsM CoreExpr		    -- Either E or let x=arg-expr in E

dsExprToAtom (UsageArg u) continue_with = continue_with (UsageArg u)
dsExprToAtom (TyArg    t) continue_with = continue_with (TyArg    t)
dsExprToAtom (LitArg   l) continue_with = continue_with (LitArg   l)

dsExprToAtom (VarArg (Var v)) continue_with = continue_with (VarArg v)
dsExprToAtom (VarArg (Lit v)) continue_with = continue_with (LitArg v)

dsExprToAtom (VarArg arg_expr) continue_with
  = let
	ty = coreExprType arg_expr
    in
    newSysLocalDs ty			`thenDs` \ arg_id ->
    continue_with (VarArg arg_id)	`thenDs` \ body   ->
    returnDs (
	if isUnboxedType ty
	then Case arg_expr (PrimAlts [] (BindDefault arg_id body))
	else Let (NonRec arg_id arg_expr) body
    )

dsExprsToAtoms :: [DsCoreArg]
	       -> ([CoreArg] -> DsM CoreExpr)
	       -> DsM CoreExpr

dsExprsToAtoms [] continue_with = continue_with []

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

\begin{code}
type DsCoreArg = GenCoreArg CoreExpr{-NB!-} TyVar UVar

mkAppDs  :: CoreExpr -> [DsCoreArg] -> DsM CoreExpr
mkConDs  :: Id       -> [DsCoreArg] -> DsM CoreExpr
mkPrimDs :: PrimOp   -> [DsCoreArg] -> DsM CoreExpr

mkAppDs fun args
  = dsExprsToAtoms args $ \ atoms ->
    returnDs (mkGenApp fun atoms)

mkConDs con args
  = dsExprsToAtoms args $ \ atoms ->
    returnDs (Con  con atoms)

mkPrimDs op args
  = dsExprsToAtoms args $ \ atoms ->
    returnDs (Prim op  atoms)
\end{code}

\begin{code}
showForErr :: Outputable a => a -> String		-- Boring but useful
showForErr thing = ppShow 80 (ppr PprForUser thing)

mkErrorAppDs :: Id 		-- The error function
	     -> Type		-- Type to which it should be applied
	     -> String		-- The error message string to pass
	     -> DsM CoreExpr

mkErrorAppDs err_id ty msg
  = getSrcLocDs			`thenDs` \ (file, line) ->
    let
	full_msg = file ++ "|" ++ line ++ "|" ++msg
	msg_lit  = NoRepStr (_PK_ full_msg)
    in
    returnDs (mkApp (Var err_id) [] [ty] [LitArg msg_lit])
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
  = if is_simple_tuple_pat pat then
	mkTupleBind tyvars [] locals_and_globals val_expr
    else
	mkErrorAppDs iRREFUT_PAT_ERROR_ID res_ty ""	`thenDs` \ error_msg ->
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
  = --pprTrace "mkTupleBind:\n" (ppAboves [ppCat (map (pprId PprShowAll) locals), ppCat (map (pprId PprShowAll) globals), {-ppr PprDebug local_tuple, pprType PprDebug res_ty,-} ppr PprDebug tuple_expr]) $

    newSysLocalDs tuple_var_ty	`thenDs` \ tuple_var ->

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
      = mkForAllTys tyvars $
	mkRhoTy theta	   $
	applyTyCon (mkTupleTyCon no_of_binders)
		   (map idType locals)
      where
	theta = mkTheta (map idType dicts)

    mk_selector :: CoreExpr -> (Id, Id) -> Int -> DsM (Id, CoreExpr)

    mk_selector tuple_var_expr (local, global) which_local
      = mapDs duplicateLocalDs locals{-the whole bunch-} `thenDs` \ binders ->
	let
	    selected = binders !! which_local
	in
	returnDs (
	    global,
	    mkLam tyvars dicts (
		mkTupleSelector
		    (mkValApp (mkTyApp tuple_var_expr tyvar_tys)
			      (map VarArg dicts))
		    binders
		    selected)
	)
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
	let fail.33 :: Void -> Int#
	    fail.33 = \_ -> error "Help"
	in
	case x of
		p1 -> ...
		p2 -> fail.33 void
		p3 -> fail.33 void
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
  | isUnboxedType ty
  = newFailLocalDs (mkFunTys [voidTy] ty)	`thenDs` \ fail_fun_var ->
    newSysLocalDs voidTy			`thenDs` \ fail_fun_arg ->
    returnDs (\ body ->
		NonRec fail_fun_var (Lam (ValBinder fail_fun_arg) body),
	      App (Var fail_fun_var) (VarArg voidId))

  | otherwise
  = newFailLocalDs ty 		`thenDs` \ fail_var ->
    returnDs (\ body -> NonRec fail_var body, Var fail_var)
\end{code}



