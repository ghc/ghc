%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[CoreUtils]{Utility functions on @Core@ syntax}

\begin{code}
module CoreUtils (
	coreExprType, coreAltsType, coreExprCc,

	mkCoreIfThenElse,
	argToExpr,
	unTagBinders, unTagBindersAlts,
	
	maybeErrorApp,
	nonErrorRHSs,
	squashableDictishCcExpr
    ) where

#include "HsVersions.h"

import CoreSyn

import CostCentre	( isDictCC, CostCentre, noCostCentre )
import MkId		( mkSysLocal )
import Id		( idType, isBottomingId,
			  mkIdWithNewUniq,
			  dataConRepType,
			  addOneToIdEnv, growIdEnvList, lookupIdEnv,
			  isNullIdEnv, IdEnv, Id
			)
import Literal		( literalType, Literal(..) )
import Maybes		( catMaybes, maybeToBool )
import PprCore
import PrimOp		( primOpType, PrimOp(..) )
import SrcLoc		( noSrcLoc )
import TyVar		( cloneTyVar,
			  isEmptyTyVarEnv, addToTyVarEnv, TyVarEnv,
			  TyVar, GenTyVar
			)
import Type		( mkFunTy, mkForAllTy, mkTyVarTy,
			  splitFunTy_maybe, applyTys, isUnpointedType,
			  splitSigmaTy, splitFunTys, instantiateTy,
			  Type
			)
import TysWiredIn	( trueDataCon, falseDataCon )
import Unique		( Unique )
import BasicTypes	( Unused )
import UniqSupply	( returnUs, thenUs,
			  mapUs, mapAndUnzipUs, getUnique,
			  UniqSM, UniqSupply
			)
import Util		( zipEqual )
import Outputable

type TypeEnv = TyVarEnv Type
\end{code}

%************************************************************************
%*									*
\subsection{Find the type of a Core atom/expression}
%*									*
%************************************************************************

\begin{code}
coreExprType :: CoreExpr -> Type

coreExprType (Var var) = idType   var
coreExprType (Lit lit) = literalType lit

coreExprType (Let _ body)	= coreExprType body
coreExprType (Case _ alts)	= coreAltsType alts

coreExprType (Note (Coerce ty _) e) = ty
coreExprType (Note other_note e)    = coreExprType e

-- a Con is a fully-saturated application of a data constructor
-- a Prim is <ditto> of a PrimOp

coreExprType (Con con args) = 
--			      pprTrace "appTyArgs" (hsep [ppr con, semi, 
--						 	   ppr con_ty, semi,
--							   ppr args]) $
    			      applyTypeToArgs con_ty args
			    where
				con_ty = dataConRepType con

coreExprType (Prim op args) = applyTypeToArgs (primOpType op) args

coreExprType (Lam (ValBinder binder) expr)
  = idType binder `mkFunTy` coreExprType expr

coreExprType (Lam (TyBinder tyvar) expr)
  = mkForAllTy tyvar (coreExprType expr)

coreExprType (App expr (TyArg ty))
  = 	-- Gather type args; more efficient to instantiate the type all at once
    go expr [ty]
  where
    go (App expr (TyArg ty)) tys = go expr (ty:tys)
    go expr		     tys = applyTys (coreExprType expr) tys

coreExprType (App expr val_arg)
  = ASSERT(isValArg val_arg)
    let
	fun_ty = coreExprType expr
    in
    case (splitFunTy_maybe fun_ty) of
	  Just (_, result_ty) -> result_ty
#ifdef DEBUG
	  Nothing -> pprPanic "coreExprType:\n"
	  		(vcat [ppr fun_ty,  ppr (App expr val_arg)])
#endif
\end{code}

\begin{code}
coreAltsType :: CoreCaseAlts -> Type

coreAltsType (AlgAlts [] deflt)         = default_ty deflt
coreAltsType (AlgAlts ((_,_,rhs1):_) _) = coreExprType rhs1

coreAltsType (PrimAlts [] deflt)       = default_ty deflt
coreAltsType (PrimAlts ((_,rhs1):_) _) = coreExprType rhs1

default_ty NoDefault           = panic "coreExprType:Case:default_ty"
default_ty (BindDefault _ rhs) = coreExprType rhs
\end{code}

\begin{code}
applyTypeToArgs op_ty (TyArg ty : args)
  =	-- Accumulate type arguments so we can instantiate all at once
    applyTypeToArgs (applyTys op_ty tys) rest_args
  where
    (tys, rest_args)         = go [ty] args
    go tys (TyArg ty : args) = go (ty:tys) args
    go tys rest_args	     = (reverse tys, rest_args)

applyTypeToArgs op_ty (val_or_lit_arg:args)
  = case (splitFunTy_maybe op_ty) of
	Just (_, res_ty) -> applyTypeToArgs res_ty args

applyTypeToArgs op_ty [] = op_ty
\end{code}

coreExprCc gets the cost centre enclosing an expression, if any.
It looks inside lambdas because (scc "foo" \x.e) = \x.scc "foo" e

\begin{code}
coreExprCc :: GenCoreExpr val_bdr val_occ flexi -> CostCentre
coreExprCc (Note (SCC cc) e)   = cc
coreExprCc (Note other_note e) = coreExprCc e
coreExprCc (Lam _ e)           = coreExprCc e
coreExprCc other               = noCostCentre
\end{code}

%************************************************************************
%*									*
\subsection{Routines to manufacture bits of @CoreExpr@}
%*									*
%************************************************************************

\begin{code}
mkCoreIfThenElse (Var bool) then_expr else_expr
    | bool == trueDataCon   = then_expr
    | bool == falseDataCon  = else_expr

mkCoreIfThenElse guard then_expr else_expr
  = Case guard
      (AlgAlts [ (trueDataCon,  [], then_expr),
		 (falseDataCon, [], else_expr) ]
       NoDefault )
\end{code}

For making @Apps@ and @Lets@, we must take appropriate evasive
action if the thing being bound has unboxed type.  @mkCoApp@ requires
a name supply to do its work.

@mkCoApps@, @mkCoCon@ and @mkCoPrim@ also handle the
arguments-must-be-atoms constraint.

\begin{code}
data CoreArgOrExpr
  = AnArg   CoreArg
  | AnExpr  CoreExpr

mkCoApps :: CoreExpr -> [CoreArgOrExpr] -> UniqSM CoreExpr
mkCoCon  :: Id       -> [CoreArgOrExpr] -> UniqSM CoreExpr
mkCoPrim :: PrimOp   -> [CoreArgOrExpr] -> UniqSM CoreExpr

mkCoApps fun args = co_thing (mkGenApp fun) args
mkCoCon  con args = co_thing (Con  con)     args
mkCoPrim  op args = co_thing (Prim op)      args 

co_thing :: ([CoreArg] -> CoreExpr)
	 -> [CoreArgOrExpr]
	 -> UniqSM CoreExpr

co_thing thing arg_exprs
  = mapAndUnzipUs expr_to_arg arg_exprs `thenUs` \ (args, maybe_binds) ->
    returnUs (mkCoLetsUnboxedToCase (catMaybes maybe_binds) (thing args))
  where
    expr_to_arg :: CoreArgOrExpr
		-> UniqSM (CoreArg, Maybe CoreBinding)

    expr_to_arg (AnArg  arg)     = returnUs (arg,      Nothing)
    expr_to_arg (AnExpr (Var v)) = returnUs (VarArg v, Nothing)
    expr_to_arg (AnExpr (Lit l)) = returnUs (LitArg l, Nothing)
    expr_to_arg (AnExpr other_expr)
      = let
	    e_ty = coreExprType other_expr
	in
	getUnique `thenUs` \ uniq ->
	let
	    new_var  = mkSysLocal SLIT("a") uniq e_ty noSrcLoc
	in
	returnUs (VarArg new_var, Just (NonRec new_var other_expr))
\end{code}

\begin{code}
argToExpr ::
  GenCoreArg val_occ flexi -> GenCoreExpr val_bdr val_occ flexi

argToExpr (VarArg v)   = Var v
argToExpr (LitArg lit) = Lit lit
\end{code}

All the following functions operate on binders, perform a uniform
transformation on them; ie. the function @(\ x -> (x,False))@
annotates all binders with False.

\begin{code}
unTagBinders :: GenCoreExpr (Id,tag) bdee flexi -> GenCoreExpr Id bdee flexi
unTagBinders expr = bop_expr fst expr

unTagBindersAlts :: GenCoreCaseAlts (Id,tag) bdee flexi -> GenCoreCaseAlts Id bdee flexi
unTagBindersAlts alts = bop_alts fst alts
\end{code}

\begin{code}
bop_expr  :: (a -> b) -> GenCoreExpr a bdee flexi -> GenCoreExpr b bdee flexi

bop_expr f (Var b)	     = Var b
bop_expr f (Lit lit)	     = Lit lit
bop_expr f (Con con args)    = Con con args
bop_expr f (Prim op args)    = Prim op args
bop_expr f (Lam binder expr) = Lam  (bop_binder f binder) (bop_expr f expr)
bop_expr f (App expr arg)    = App  (bop_expr f expr) arg
bop_expr f (Note note expr)  = Note note (bop_expr f expr)
bop_expr f (Let bind expr)   = Let  (bop_bind f bind) (bop_expr f expr)
bop_expr f (Case expr alts)  = Case (bop_expr f expr) (bop_alts f alts)

bop_binder f (ValBinder   v) = ValBinder (f v)
bop_binder f (TyBinder    t) = TyBinder    t

bop_bind f (NonRec b e)	= NonRec (f b) (bop_expr f e)
bop_bind f (Rec pairs)	= Rec [(f b, bop_expr f e) | (b, e) <- pairs]

bop_alts f (AlgAlts alts deflt)
  = AlgAlts  [ (con, [f b | b <- binders], bop_expr f e)
	     | (con, binders, e) <- alts ]
	     (bop_deflt f deflt)

bop_alts f (PrimAlts alts deflt)
  = PrimAlts [ (lit, bop_expr f e) | (lit, e) <- alts ]
    	     (bop_deflt f deflt)

bop_deflt f (NoDefault)		 = NoDefault
bop_deflt f (BindDefault b expr) = BindDefault (f b) (bop_expr f expr)
\end{code}

OLD (but left here because of the nice example): @singleAlt@ checks
whether a bunch of case alternatives is actually just one alternative.
It specifically {\em ignores} alternatives which consist of just a
call to @error@, because they won't result in any code duplication.

Example:
\begin{verbatim}
	case (case <something> of
		True  -> <rhs>
		False -> error "Foo") of
	<alts>

===>

	case <something> of
	   True ->  case <rhs> of
		    <alts>
	   False -> case error "Foo" of
		    <alts>

===>

	case <something> of
	   True ->  case <rhs> of
		    <alts>
	   False -> error "Foo"
\end{verbatim}
Notice that the \tr{<alts>} don't get duplicated.

\begin{code}
nonErrorRHSs :: GenCoreCaseAlts a Id Unused -> [GenCoreExpr a Id Unused]

nonErrorRHSs alts
  = filter not_error_app (find_rhss alts)
  where
    find_rhss (AlgAlts  as deflt) = [rhs | (_,_,rhs) <- as] ++ deflt_rhs deflt
    find_rhss (PrimAlts as deflt) = [rhs | (_,rhs)   <- as] ++ deflt_rhs deflt

    deflt_rhs NoDefault           = []
    deflt_rhs (BindDefault _ rhs) = [rhs]

    not_error_app rhs
      = case (maybeErrorApp rhs Nothing) of
	  Just _  -> False
	  Nothing -> True
\end{code}

maybeErrorApp checks whether an expression is of the form

	error ty args

If so, it returns

	Just (error ty' args)

where ty' is supplied as an argument to maybeErrorApp.

Here's where it is useful:

		case (error ty "Foo" e1 e2) of <alts>
 ===>
		error ty' "Foo"

where ty' is the type of any of the alternatives.  You might think
this never occurs, but see the comments on the definition of
@singleAlt@.

Note: we *avoid* the case where ty' might end up as a primitive type:
this is very uncool (totally wrong).

NOTICE: in the example above we threw away e1 and e2, but not the
string "Foo".  How did we know to do that?

Answer: for now anyway, we only handle the case of a function whose
type is of form

	bottomingFn :: forall a. t1 -> ... -> tn -> a
	    	    	      ^---------------------^ NB!

Furthermore, we only count a bottomingApp if the function is applied
to more than n args.  If so, we transform:

	bottomingFn ty e1 ... en en+1 ... em
to
	bottomingFn ty' e1 ... en

That is, we discard en+1 .. em

\begin{code}
maybeErrorApp
	:: GenCoreExpr a Id Unused	-- Expr to look at
	-> Maybe Type			-- Just ty => a result type *already cloned*;
					-- Nothing => don't know result ty; we
					-- *pretend* that the result ty won't be
					-- primitive -- somebody later must
					-- ensure this.
	-> Maybe (GenCoreExpr b Id Unused)

maybeErrorApp expr result_ty_maybe
  = case (collectArgs expr) of
      (Var fun, [ty], other_args)
	| isBottomingId fun
	&& maybeToBool result_ty_maybe -- we *know* the result type
				       -- (otherwise: live a fairy-tale existence...)
	&& not (isUnpointedType result_ty) ->

	case (splitSigmaTy (idType fun)) of
	  ([tyvar], [], tau_ty) ->
	      case (splitFunTys tau_ty) of { (arg_tys, res_ty) ->
	      let
		  n_args_to_keep = length arg_tys
		  args_to_keep   = take n_args_to_keep other_args
	      in
	      if  (res_ty == mkTyVarTy tyvar)
	       && n_args_to_keep <= length other_args
	      then
		    -- Phew!  We're in business
		  Just (mkGenApp (Var fun) (TyArg result_ty : args_to_keep))
	      else
		  Nothing
	      }

	  other -> Nothing  -- Function type wrong shape
      other -> Nothing
  where
    Just result_ty = result_ty_maybe
\end{code}

\begin{code}
squashableDictishCcExpr :: CostCentre -> GenCoreExpr a b c -> Bool

squashableDictishCcExpr cc expr
  = if not (isDictCC cc) then
	False -- that was easy...
    else
	squashable expr -- note: quite like the "atomic_rhs" stuff in simplifier
  where
    squashable (Var _)      = True
    squashable (Con  _ _)   = True -- I think so... WDP 94/09
    squashable (Prim _ _)   = True -- ditto
    squashable (App f a)
      | notValArg a	    = squashable f
    squashable other	    = False
\end{code}
