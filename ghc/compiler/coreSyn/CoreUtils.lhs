%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[CoreUtils]{Utility functions on @Core@ syntax}

\begin{code}
#include "HsVersions.h"

module CoreUtils (
	coreExprType, coreAltsType,

	substCoreExpr

	, mkCoreIfThenElse
	, mkErrorApp, escErrorMsg
	, argToExpr
	, unTagBinders, unTagBindersAlts
	, manifestlyWHNF, manifestlyBottom
{-	exprSmallEnoughToDup,
	coreExprArity,
	isWrapperFor,
	maybeErrorApp,
	nonErrorRHSs,
	squashableDictishCcExpr,

-}  ) where

import Ubiq
import IdLoop	-- for pananoia-checking purposes

import CoreSyn

import CostCentre	( isDictCC )
import Id		( idType, mkSysLocal, getIdArity, isBottomingId,
			  addOneToIdEnv, growIdEnvList, lookupIdEnv,
			  isNullIdEnv, IdEnv(..),
			  GenId{-instances-}
			)
import IdInfo		( arityMaybe )
import Literal		( literalType, isNoRepLit, Literal(..) )
import Maybes		( catMaybes )
import PprCore		( GenCoreExpr{-instances-}, GenCoreArg{-instances-} )
import PprStyle		( PprStyle(..) )
import PprType		( GenType{-instances-}, GenTyVar{-instance-} )
import Pretty		( ppAboves )
import PrelInfo		( trueDataCon, falseDataCon,
			  augmentId, buildId,
			  pAT_ERROR_ID
			)
import PrimOp		( primOpType, PrimOp(..) )
import SrcLoc		( mkUnknownSrcLoc )
import TyVar		( isNullTyVarEnv, TyVarEnv(..), GenTyVar{-instances-} )
import Type		( mkFunTys, mkForAllTy, mkForAllUsageTy,
			  getFunTy_maybe, applyTy, splitSigmaTy
			)
import Unique		( Unique{-instances-} )
import UniqSupply	( initUs, returnUs, thenUs,
			  mapUs, mapAndUnzipUs,
			  UniqSM(..), UniqSupply
			)
import Util		( zipEqual, panic, pprPanic, assertPanic )

type TypeEnv = TyVarEnv Type
applyUsage = panic "CoreUtils.applyUsage:ToDo"
dup_binder = panic "CoreUtils.dup_binder"
applyTypeEnvToTy = panic "CoreUtils.applyTypeEnvToTy"
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
coreExprType (SCC _ expr)	= coreExprType expr
coreExprType (Case _ alts)	= coreAltsType alts

-- a Con is a fully-saturated application of a data constructor
-- a Prim is <ditto> of a PrimOp

coreExprType (Con con args) = applyTypeToArgs (idType    con) args
coreExprType (Prim op args) = applyTypeToArgs (primOpType op) args

coreExprType (Lam (ValBinder binder) expr)
  = mkFunTys [idType binder] (coreExprType expr)

coreExprType (Lam (TyBinder tyvar) expr)
  = mkForAllTy tyvar (coreExprType expr)

coreExprType (Lam (UsageBinder uvar) expr)
  = mkForAllUsageTy uvar (panic "coreExprType:Lam UsageBinder") (coreExprType expr)

coreExprType (App expr (TyArg ty))
  = applyTy (coreExprType expr) ty

coreExprType (App expr (UsageArg use))
  = applyUsage (coreExprType expr) use

coreExprType (App expr val_arg)
  = ASSERT(isValArg val_arg)
    let
	fun_ty = coreExprType expr
    in
    case (getFunTy_maybe fun_ty) of
	  Just (_, result_ty) -> result_ty
#ifdef DEBUG
	  Nothing -> pprPanic "coreExprType:\n"
		(ppAboves [ppr PprDebug fun_ty,
			   ppr PprShowAll (App expr val_arg)])
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
applyTypeToArgs = panic "applyTypeToArgs"
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

\begin{code}
mkErrorApp :: Type -> Id -> String -> CoreExpr

mkErrorApp ty str_var error_msg
  = Let (NonRec str_var (Lit (NoRepStr (_PK_ error_msg)))) (
    mkApp (Var pAT_ERROR_ID) [] [ty] [VarArg str_var])

escErrorMsg [] = []
escErrorMsg ('%':xs) = '%' : '%' : escErrorMsg xs
escErrorMsg (x:xs)   = x : escErrorMsg xs
\end{code}

For making @Apps@ and @Lets@, we must take appropriate evasive
action if the thing being bound has unboxed type.  @mkCoApp@ requires
a name supply to do its work.  Other-monad code will call @mkCoApp@
through its own interface function (e.g., the desugarer uses
@mkCoAppDs@).

@mkCoApp@, @mkCoCon@ and @mkCoPrim@ also handle the
arguments-must-be-atoms constraint.

\begin{code}
{- LATER:
--mkCoApp :: CoreExpr -> CoreExpr -> UniqSM CoreExpr

mkCoApp e1 (Var v) = returnUs (App e1 (VarArg v))
mkCoApp e1 (Lit l) = returnUs (App e1 (LitArg l))
mkCoApp e1 e2
  = let
	e2_ty = coreExprType e2
    in
    panic "getUnique"	`thenUs` \ uniq ->
    let
	new_var = mkSysLocal SLIT("a") uniq e2_ty mkUnknownSrcLoc
    in
    returnUs (
    	mkCoLetUnboxedToCase (NonRec new_var e2)
			     (App e1 (VarArg new_var))
    )
-}
\end{code}

\begin{code}
{-LATER
mkCoCon  :: Id     -> [CoreExpr] -> UniqSM CoreExpr
mkCoPrim :: PrimOp -> [CoreExpr] -> UniqSM CoreExpr

mkCoCon con args = mkCoThing (Con con) args
mkCoPrim op args = mkCoThing (Prim op) args

mkCoThing thing arg_exprs
  = mapAndUnzipUs expr_to_arg arg_exprs `thenUs` \ (args, maybe_binds) ->
    returnUs (mkCoLetsUnboxedToCase (catMaybes maybe_binds) (thing args))
  where
    expr_to_arg :: CoreExpr
	       -> UniqSM (CoreArg, Maybe CoreBinding)

    expr_to_arg (Var v) = returnUs (VarArg v, Nothing)
    expr_to_arg (Lit l) = returnUs (LitArg l, Nothing)
    expr_to_arg other_expr
      = let
	    e_ty = coreExprType other_expr
	in
	panic "getUnique" `thenUs` \ uniq ->
	let
	    new_var  = mkSysLocal SLIT("a") uniq e_ty mkUnknownSrcLoc
	    new_atom = VarArg new_var
	in
	returnUs (new_atom, Just (NonRec new_var other_expr))
-}
\end{code}

\begin{code}
argToExpr ::
  GenCoreArg val_occ tyvar uvar -> GenCoreExpr val_bdr val_occ tyvar uvar

argToExpr (VarArg v)   = Var v
argToExpr (LitArg lit) = Lit lit
\end{code}

\begin{code}
{- LATER:
--mkCoApps ::
--  GenCoreExpr val_bdr val_occ tyvar uvar ->
--  [GenCoreExpr val_bdr val_occ tyvar uvar] ->
--  UniqSM(GenCoreExpr val_bdr val_occ tyvar uvar)

mkCoApps fun []  = returnUs fun
mkCoApps fun (arg:args)
  = mkCoApp fun arg `thenUs` \ new_fun ->
    mkCoApps new_fun args
\end{code}

\begin{code}
exprSmallEnoughToDup :: GenCoreExpr binder Id -> Bool

exprSmallEnoughToDup (Con _ _ _)   = True	-- Could check # of args
exprSmallEnoughToDup (Prim op _ _) = not (fragilePrimOp op)	-- Could check # of args
exprSmallEnoughToDup (Lit lit) = not (isNoRepLit lit)

exprSmallEnoughToDup expr  -- for now, just: <var> applied to <args>
  = case (collectArgs expr) of { (fun, args) ->
    case fun of
      Var v -> v /= buildId
		 && v /= augmentId
		 && length args <= 6 -- or 10 or 1 or 4 or anything smallish.
      _       -> False
    }
-}
\end{code}
Question (ADR): What is the above used for?  Is a _ccall_ really small
enough?

@manifestlyWHNF@ looks at a Core expression and returns \tr{True} if
it is obviously in weak head normal form.  It isn't a disaster if it
errs on the conservative side (returning \tr{False})---I've probably
left something out... [WDP]

\begin{code}
manifestlyWHNF :: GenCoreExpr bndr Id tyvar uvar -> Bool

manifestlyWHNF (Var _)	  = True
manifestlyWHNF (Lit _)	  = True
manifestlyWHNF (Con _ _)  = True
manifestlyWHNF (SCC _ e)  = manifestlyWHNF e
manifestlyWHNF (Let _ e)  = False
manifestlyWHNF (Case _ _) = False

manifestlyWHNF (Lam (ValBinder _) _) = True
manifestlyWHNF (Lam other_binder  e) = manifestlyWHNF e

manifestlyWHNF other_expr   -- look for manifest partial application
  = case (collectArgs other_expr) of { (fun, args) ->
    case fun of
      Var f ->  let
		    num_val_args = numValArgs args
		in
		num_val_args == 0 -- Just a type application of
				  -- a variable (f t1 t2 t3);
				  -- counts as WHNF.
		||
		case (arityMaybe (getIdArity f)) of
		  Nothing     -> False
		  Just arity  -> num_val_args < arity

      _ -> False
    }
\end{code}

@manifestlyBottom@ looks at a Core expression and returns \tr{True} if
it is obviously bottom, that is, it will certainly return bottom at
some point.  It isn't a disaster if it errs on the conservative side
(returning \tr{False}).

\begin{code}
manifestlyBottom :: GenCoreExpr bndr Id tyvar uvar -> Bool

manifestlyBottom (Var v)     = isBottomingId v
manifestlyBottom (Lit _)     = False
manifestlyBottom (Con  _ _)  = False
manifestlyBottom (Prim _ _)  = False
manifestlyBottom (SCC _ e)   = manifestlyBottom e
manifestlyBottom (Let _ e)   = manifestlyBottom e

  -- We do not assume \x.bottom == bottom:
manifestlyBottom (Lam (ValBinder _) _) = False
manifestlyBottom (Lam other_binder  e) = manifestlyBottom e

manifestlyBottom (Case e a)
  = manifestlyBottom e
  || (case a of
	AlgAlts  alts def -> all mbalg  alts && mbdef def
	PrimAlts alts def -> all mbprim alts && mbdef def
     )
  where
    mbalg  (_,_,e') = manifestlyBottom e'

    mbprim (_,e')   = manifestlyBottom e'

    mbdef NoDefault          = True
    mbdef (BindDefault _ e') = manifestlyBottom e'

manifestlyBottom other_expr   -- look for manifest partial application
  = case (collectArgs other_expr) of { (fun, args) ->
    case fun of
      Var f | isBottomingId f -> True
		-- Application of a function which always gives
		-- bottom; we treat this as a WHNF, because it
		-- certainly doesn't need to be shared!
      _ -> False
    }
\end{code}

\begin{code}
{-LATER:
coreExprArity
	:: (Id -> Maybe (GenCoreExpr bndr Id))
	-> GenCoreExpr bndr Id
	-> Int
coreExprArity f (Lam _ expr) = coreExprArity f expr + 1
coreExprArity f (CoTyLam _ expr) = coreExprArity f expr
coreExprArity f (App expr arg) = max (coreExprArity f expr - 1) 0
coreExprArity f (CoTyApp expr _) = coreExprArity f expr
coreExprArity f (Var v) = max further info
   where
	further
	     = case f v of
		Nothing -> 0
		Just expr -> coreExprArity f expr
	info = case (arityMaybe (getIdArity v)) of
		Nothing    -> 0
		Just arity -> arity
coreExprArity f _ = 0
\end{code}

@isWrapperFor@: we want to see exactly:
\begin{verbatim}
/\ ... \ args -> case <arg> of ... -> case <arg> of ... -> wrkr <stuff>
\end{verbatim}

Probably a little too HACKY [WDP].

\begin{code}
isWrapperFor :: CoreExpr -> Id -> Bool

expr `isWrapperFor` var
  = case (collectBinders  expr) of { (_, _, args, body) -> -- lambdas off the front
    unravel_casing args body
    --NO, THANKS: && not (null args)
    }
  where
    var's_worker = getWorkerId (getIdStrictness var)

    is_elem = isIn "isWrapperFor"

    --------------
    unravel_casing case_ables (Case scrut alts)
      = case (collectArgs scrut) of { (fun, args) ->
	case fun of
	  Var scrut_var -> let
				answer =
				     scrut_var /= var && all (doesn't_mention var) args
				  && scrut_var `is_elem` case_ables
				  && unravel_alts case_ables alts
			     in
			     answer

	  _ -> False
	}

    unravel_casing case_ables other_expr
      = case (collectArgs other_expr) of { (fun, args) ->
	case fun of
	  Var wrkr -> let
			    answer =
				-- DOESN'T WORK: wrkr == var's_worker
				wrkr /= var
			     && isWorkerId wrkr
			     && all (doesn't_mention var)  args
			     && all (only_from case_ables) args
			in
			answer

	  _ -> False
	}

    --------------
    unravel_alts case_ables (AlgAlts [(_,params,rhs)] NoDefault)
      = unravel_casing (params ++ case_ables) rhs
    unravel_alts case_ables other = False

    -------------------------
    doesn't_mention var (ValArg (VarArg v)) = v /= var
    doesn't_mention var other = True

    -------------------------
    only_from case_ables (ValArg (VarArg v)) = v `is_elem` case_ables
    only_from case_ables other = True
-}
\end{code}

All the following functions operate on binders, perform a uniform
transformation on them; ie. the function @(\ x -> (x,False))@
annotates all binders with False.

\begin{code}
unTagBinders :: GenCoreExpr (Id,tag) bdee tv uv -> GenCoreExpr Id bdee tv uv
unTagBinders expr = bop_expr fst expr

unTagBindersAlts :: GenCoreCaseAlts (Id,tag) bdee tv uv -> GenCoreCaseAlts Id bdee tv uv
unTagBindersAlts alts = bop_alts fst alts
\end{code}

\begin{code}
bop_expr  :: (a -> b) -> GenCoreExpr a bdee tv uv -> GenCoreExpr b bdee tv uv

bop_expr f (Var b)	     = Var b
bop_expr f (Lit lit)	     = Lit lit
bop_expr f (Con con args)    = Con con args
bop_expr f (Prim op args)    = Prim op args
bop_expr f (Lam binder expr) = Lam  (bop_binder f binder) (bop_expr f expr)
bop_expr f (App expr arg)    = App  (bop_expr f expr) arg
bop_expr f (SCC label expr)  = SCC  label (bop_expr f expr)
bop_expr f (Let bind expr)   = Let  (bop_bind f bind) (bop_expr f expr)
bop_expr f (Case expr alts)  = Case (bop_expr f expr) (bop_alts f alts)

bop_binder f (ValBinder   v) = ValBinder (f v)
bop_binder f (TyBinder    t) = TyBinder    t
bop_binder f (UsageBinder u) = UsageBinder u

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
{- LATER:
nonErrorRHSs :: GenCoreCaseAlts binder Id -> [GenCoreExpr binder Id]

nonErrorRHSs alts = filter not_error_app (find_rhss alts)
  where
    find_rhss (AlgAlts  alts deflt) = [rhs | (_,_,rhs) <- alts] ++ deflt_rhs deflt
    find_rhss (PrimAlts alts deflt) = [rhs | (_,rhs)   <- alts] ++ deflt_rhs deflt

    deflt_rhs NoDefault           = []
    deflt_rhs (BindDefault _ rhs) = [rhs]

    not_error_app rhs = case maybeErrorApp rhs Nothing of
			 Just _  -> False
			 Nothing -> True
\end{code}

maybeErrorApp checkes whether an expression is of the form

	error ty args

If so, it returns

	Just (error ty' args)

where ty' is supplied as an argument to maybeErrorApp.

Here's where it is useful:

		case (error ty "Foo" e1 e2) of <alts>
 ===>
		error ty' "Foo"

where ty' is the type of any of the alternatives.
You might think this never occurs, but see the comments on
the definition of @singleAlt@.

Note: we *avoid* the case where ty' might end up as a
primitive type: this is very uncool (totally wrong).

NOTICE: in the example above we threw away e1 and e2, but
not the string "Foo".  How did we know to do that?

Answer: for now anyway, we only handle the case of a function
whose type is of form

	bottomingFn :: forall a. t1 -> ... -> tn -> a
	    	    	      ^---------------------^ NB!

Furthermore, we only count a bottomingApp if the function is
applied to more than n args.  If so, we transform:

	bottomingFn ty e1 ... en en+1 ... em
to
	bottomingFn ty' e1 ... en

That is, we discard en+1 .. em

\begin{code}
maybeErrorApp :: GenCoreExpr bndr Id   -- Expr to look at
	      -> Maybe Type	    -- Just ty => a result type *already cloned*;
				    -- Nothing => don't know result ty; we
				    -- *pretend* that the result ty won't be
				    -- primitive -- somebody later must
				    -- ensure this.
	       -> Maybe (GenCoreExpr bndr Id)

maybeErrorApp expr result_ty_maybe
  = case collectArgs expr of
      (Var fun, (TypeArg ty : other_args))
	| isBottomingId fun
	&& maybeToBool result_ty_maybe -- we *know* the result type
				       -- (otherwise: live a fairy-tale existence...)
	&& not (isPrimType result_ty) ->
	case splitSigmaTy (idType fun) of
	  ([tyvar_tmpl], [], tau_ty) ->
	      case (splitTyArgs tau_ty) of { (arg_tys, res_ty) ->
	      let
		  n_args_to_keep = length arg_tys
		  args_to_keep   = take n_args_to_keep other_args
	      in
	      if  res_ty == mkTyVarTemplateTy tyvar_tmpl &&
		  n_args_to_keep <= length other_args
	      then
		    -- Phew!  We're in business
		  Just (mkGenApp (Var fun)
			      (TypeArg result_ty : args_to_keep))
	      else
		  Nothing
	      }

	  other -> 	-- Function type wrong shape
		    Nothing
      other -> Nothing
  where
    Just result_ty = result_ty_maybe
\end{code}

\begin{code}
squashableDictishCcExpr :: CostCentre -> GenCoreExpr a b -> Bool

squashableDictishCcExpr cc expr
  = if not (isDictCC cc) then
	False -- that was easy...
    else
	squashable expr -- note: quite like the "atomic_rhs" stuff in simplifier
  where
    squashable (Var _)      = True
    squashable (CoTyApp f _)  = squashable f
    squashable (Con _ _ _)  = True -- I think so... WDP 94/09
    squashable (Prim _ _ _) = True -- ditto
    squashable other	      = False
-}
\end{code}

%************************************************************************
%*									*
\subsection{Core-renaming utils}
%*									*
%************************************************************************

\begin{code}
substCoreExpr	:: ValEnv
		-> TypeEnv -- TyVar=>Type
		-> CoreExpr
		-> UniqSM CoreExpr

substCoreExpr venv tenv expr
  -- if the envs are empty, then avoid doing anything
  = if (isNullIdEnv venv && isNullTyVarEnv tenv) then
       returnUs expr
    else
       do_CoreExpr venv tenv expr
\end{code}

The equiv code for @Types@ is in @TyUtils@.

Because binders aren't necessarily unique: we don't do @plusEnvs@
(which check for duplicates); rather, we use the shadowing version,
@growIdEnv@ (and shorthand @addOneToIdEnv@).

@do_CoreBindings@ takes into account the semantics of a list of
@CoreBindings@---things defined early in the list are visible later in
the list, but not vice versa.

\begin{code}
type ValEnv  = IdEnv CoreExpr

do_CoreBindings :: ValEnv
		-> TypeEnv
		-> [CoreBinding]
		-> UniqSM [CoreBinding]

do_CoreBinding :: ValEnv
	       -> TypeEnv
	       -> CoreBinding
	       -> UniqSM (CoreBinding, ValEnv)

do_CoreBindings venv tenv [] = returnUs []
do_CoreBindings venv tenv (b:bs)
  = do_CoreBinding  venv     tenv b	`thenUs` \ (new_b,  new_venv) ->
    do_CoreBindings new_venv tenv bs	`thenUs` \  new_bs ->
    returnUs (new_b : new_bs)

do_CoreBinding venv tenv (NonRec binder rhs)
  = do_CoreExpr venv tenv rhs	`thenUs` \ new_rhs ->

    dup_binder tenv binder	`thenUs` \ (new_binder, (old, new)) ->
    -- now plug new bindings into envs
    let  new_venv = addOneToIdEnv venv old new  in

    returnUs (NonRec new_binder new_rhs, new_venv)

do_CoreBinding venv tenv (Rec binds)
  = -- for letrec, we plug in new bindings BEFORE cloning rhss
    mapAndUnzipUs (dup_binder tenv) binders `thenUs` \ (new_binders, new_maps) ->
    let  new_venv = growIdEnvList venv new_maps in

    mapUs (do_CoreExpr new_venv tenv) rhss `thenUs` \ new_rhss ->
    returnUs (Rec (new_binders `zipEqual` new_rhss), new_venv)
  where
    (binders, rhss) = unzip binds
\end{code}

\begin{code}
do_CoreArg :: ValEnv
	    -> TypeEnv
	    -> CoreArg
	    -> UniqSM CoreExpr

do_CoreArg venv tenv (LitArg lit)     = returnUs (Lit lit)
do_CoreArg venv tenv (TyArg ty)	      = panic "do_CoreArg: TyArg"
do_CoreArg venv tenv (UsageArg usage) = panic "do_CoreArg: UsageArg"
do_CoreArg venv tenv (VarArg v)
  = returnUs (
      case (lookupIdEnv venv v) of
	Nothing   -> --false:ASSERT(toplevelishId v)
		     Var v
	Just expr -> expr
    )
\end{code}

\begin{code}
do_CoreExpr :: ValEnv
	    -> TypeEnv
	    -> CoreExpr
	    -> UniqSM CoreExpr

do_CoreExpr venv tenv orig_expr@(Var var)
  = returnUs (
      case (lookupIdEnv venv var) of
	Nothing	    -> --false:ASSERT(toplevelishId var) (SIGH)
		       orig_expr
	Just expr   -> expr
    )

do_CoreExpr venv tenv e@(Lit _) = returnUs e

do_CoreExpr venv tenv (Con con as)
  = panic "CoreUtils.do_CoreExpr:Con"
{- LATER:
  = mapUs  (do_CoreArg venv tenv) as `thenUs`  \ new_as ->
    mkCoCon con new_as
-}

do_CoreExpr venv tenv (Prim op as)
  = panic "CoreUtils.do_CoreExpr:Prim"
{- LATER:
  = mapUs  (do_CoreArg venv tenv) as 	`thenUs`  \ new_as ->
    do_PrimOp op			`thenUs`  \ new_op ->
    mkCoPrim new_op new_as
  where
    do_PrimOp (CCallOp label is_asm may_gc arg_tys result_ty)
      = let
	    new_arg_tys   = map (applyTypeEnvToTy tenv) arg_tys
	    new_result_ty = applyTypeEnvToTy tenv result_ty
	in
	returnUs (CCallOp label is_asm may_gc new_arg_tys new_result_ty)

    do_PrimOp other_op = returnUs other_op
-}

do_CoreExpr venv tenv (Lam binder expr)
  = dup_binder tenv binder `thenUs` \(new_binder, (old,new)) ->
    let  new_venv = addOneToIdEnv venv old new  in
    do_CoreExpr new_venv tenv expr  `thenUs` \ new_expr ->
    returnUs (Lam new_binder new_expr)

do_CoreExpr venv tenv (App expr arg)
  = panic "CoreUtils.do_CoreExpr:App"
{-
  = do_CoreExpr venv tenv expr	`thenUs` \ new_expr ->
    do_CoreArg  venv tenv arg   `thenUs` \ new_arg  ->
    mkCoApp new_expr new_arg
-}

do_CoreExpr venv tenv (Case expr alts)
  = do_CoreExpr venv tenv expr	    `thenUs` \ new_expr ->
    do_alts venv tenv alts	    `thenUs` \ new_alts ->
    returnUs (Case new_expr new_alts)
  where
    do_alts venv tenv (AlgAlts alts deflt)
      = mapUs (do_boxed_alt venv tenv) alts `thenUs` \ new_alts ->
    	do_default venv tenv deflt	    `thenUs` \ new_deflt ->
	returnUs (AlgAlts new_alts new_deflt)
      where
	do_boxed_alt venv tenv (con, binders, expr)
	  = mapAndUnzipUs (dup_binder tenv) binders `thenUs` \ (new_binders, new_vmaps) ->
	    let  new_venv = growIdEnvList venv new_vmaps  in
	    do_CoreExpr new_venv tenv expr  `thenUs` \ new_expr ->
	    returnUs (con, new_binders, new_expr)


    do_alts venv tenv (PrimAlts alts deflt)
      = mapUs (do_unboxed_alt venv tenv) alts `thenUs` \ new_alts ->
    	do_default venv tenv deflt	      `thenUs` \ new_deflt ->
	returnUs (PrimAlts new_alts new_deflt)
      where
	do_unboxed_alt venv tenv (lit, expr)
	  = do_CoreExpr venv tenv expr	`thenUs` \ new_expr ->
	    returnUs (lit, new_expr)

    do_default venv tenv NoDefault = returnUs NoDefault

    do_default venv tenv (BindDefault binder expr)
      =	dup_binder tenv binder		`thenUs` \ (new_binder, (old, new)) ->
	let  new_venv = addOneToIdEnv venv old new  in
	do_CoreExpr new_venv tenv expr	`thenUs` \ new_expr ->
	returnUs (BindDefault new_binder new_expr)

do_CoreExpr venv tenv (Let core_bind expr)
  = do_CoreBinding venv tenv core_bind	`thenUs` \ (new_bind, new_venv) ->
    -- and do the body of the let
    do_CoreExpr new_venv tenv expr  	`thenUs` \ new_expr ->
    returnUs (Let new_bind new_expr)

do_CoreExpr venv tenv (SCC label expr)
  = do_CoreExpr venv tenv expr	    	`thenUs` \ new_expr ->
    returnUs (SCC label new_expr)
\end{code}
