%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
%************************************************************************
%*									*
\section[CoreToStg]{Converting core syntax to STG syntax}
%*									*
%************************************************************************

Convert a @CoreSyntax@ program to a @StgSyntax@ program.

\begin{code}
module CoreToStg ( topCoreBindsToStg ) where

#include "HsVersions.h"

import CoreSyn		-- input
import StgSyn		-- output

import CoreUtils	( coreExprType )
import SimplUtils	( findDefault )
import CostCentre	( noCCS )
import Id		( Id, mkUserLocal, idType,
			  externallyVisibleId, setIdUnique
			)
import Name		( varOcc )
import VarEnv
import Const		( Con(..), isWHNFCon, Literal(..) )
import PrimOp		( PrimOp(..) )
import Type		( isUnLiftedType, isUnboxedTupleType, Type )
import Unique		( Unique, Uniquable(..) )
import UniqSupply	-- all of it, really
import Outputable
\end{code}


	***************  OVERVIEW   *********************


The business of this pass is to convert Core to Stg.  On the way:

* We discard type lambdas and applications. In so doing we discard
  "trivial" bindings such as
	x = y t1 t2
  where t1, t2 are types

* We don't pin on correct arities any more, because they can be mucked up
  by the lambda lifter.  In particular, the lambda lifter can take a local
  letrec-bound variable and make it a lambda argument, which shouldn't have
  an arity.  So SetStgVarInfo sets arities now.

* We do *not* pin on the correct free/live var info; that's done later.
  Instead we use bOGUS_LVS and _FVS as a placeholder.

[Quite a bit of stuff that used to be here has moved 
 to tidyCorePgm (SimplCore.lhs) SLPJ Nov 96]


%************************************************************************
%*									*
\subsection[coreToStg-programs]{Converting a core program and core bindings}
%*									*
%************************************************************************

March 98: We keep a small environment to give all locally bound
Names new unique ids, since the code generator assumes that binders
are unique across a module. (Simplifier doesn't maintain this
invariant any longer.)

\begin{code}
type StgEnv = IdEnv Id
\end{code}

No free/live variable information is pinned on in this pass; it's added
later.  For this pass
we use @bOGUS_LVs@ and @bOGUS_FVs@ as placeholders.

\begin{code}
bOGUS_LVs :: StgLiveVars
bOGUS_LVs = panic "bOGUS_LVs" -- emptyUniqSet (used when pprTracing)

bOGUS_FVs :: [Id]
bOGUS_FVs = panic "bOGUS_FVs" -- [] (ditto)
\end{code}

\begin{code}
topCoreBindsToStg :: UniqSupply	-- name supply
		  -> [CoreBind]	-- input
		  -> [StgBinding]	-- output

topCoreBindsToStg us core_binds
  = initUs us (coreBindsToStg emptyVarEnv core_binds)
  where
    coreBindsToStg :: StgEnv -> [CoreBind] -> UniqSM [StgBinding]

    coreBindsToStg env [] = returnUs []
    coreBindsToStg env (b:bs)
      = coreBindToStg  env b		`thenUs` \ (new_b, new_env) ->
    	coreBindsToStg new_env bs 	`thenUs` \ new_bs ->
    	returnUs (new_b ++ new_bs)
\end{code}

%************************************************************************
%*									*
\subsection[coreToStg-binds]{Converting bindings}
%*									*
%************************************************************************

\begin{code}
coreBindToStg :: StgEnv
	      -> CoreBind
	      -> UniqSM ([StgBinding],	-- Empty or singleton
		    	 StgEnv)	-- Floats

coreBindToStg env (NonRec binder rhs)
  = coreRhsToStg env rhs	`thenUs` \ stg_rhs ->
    newLocalId env binder	`thenUs` \ (new_env, new_binder) ->
    returnUs ([StgNonRec new_binder stg_rhs], new_env)

coreBindToStg env (Rec pairs)
  = newLocalIds env binders		`thenUs` \ (env', binders') ->
    mapUs (coreRhsToStg env') rhss      `thenUs` \ stg_rhss ->
    returnUs ([StgRec (binders' `zip` stg_rhss)], env')
  where
    (binders, rhss) = unzip pairs
\end{code}


%************************************************************************
%*									*
\subsection[coreToStg-rhss]{Converting right hand sides}
%*									*
%************************************************************************

\begin{code}
coreRhsToStg :: StgEnv -> CoreExpr -> UniqSM StgRhs

coreRhsToStg env core_rhs
  = coreExprToStg env core_rhs 	`thenUs` \ stg_expr ->
    returnUs (exprToRhs stg_expr)

exprToRhs (StgLet (StgNonRec var1 rhs) (StgApp var2 []))
  | var1 == var2 
  = rhs
	-- This curious stuff is to unravel what a lambda turns into
	-- We have to do it this way, rather than spot a lambda in the
	-- incoming rhs.  Why?  Because trivial bindings might conceal
	-- what the rhs is actually like.

exprToRhs (StgCon (DataCon con) args _) = StgRhsCon noCCS con args

exprToRhs expr 
	= StgRhsClosure noCCS		-- No cost centre (ToDo?)
		        stgArgOcc	-- safe
			noSRT		-- figure out later
			bOGUS_FVs
			Updatable	-- Be pessimistic
			[]
			expr

\end{code}


%************************************************************************
%*									*
\subsection[coreToStg-atoms{Converting atoms}
%*									*
%************************************************************************

\begin{code}
coreArgsToStg :: StgEnv -> [CoreArg]
	      -> UniqSM ([(Id,StgExpr)], [StgArg])

coreArgsToStg env []
  = returnUs ([], [])

coreArgsToStg env (Type ty : as)	-- Discard type arguments
  = coreArgsToStg env as

coreArgsToStg env (a:as)
  = coreArgToStg env a		`thenUs` \ (bs1, a') ->
    coreArgsToStg env as	`thenUs` \ (bs2, as') ->
    returnUs (bs1 ++ bs2, a' : as')

-- This is where we arrange that a non-trivial argument is let-bound

coreArgToStg :: StgEnv -> CoreArg -> UniqSM ([(Id,StgExpr)], StgArg)

coreArgToStg env arg
  = coreExprToStgFloat env arg	`thenUs` \ (binds, arg') ->
    case (binds, arg') of
	([], StgCon con [] _) | isWHNFCon con -> returnUs ([], StgConArg con)
	([], StgApp v [])		      -> returnUs ([], StgVarArg v)

	-- A non-trivial argument: we must let (or case-bind)
	-- We don't do the case part here... we leave that to mkStgLets

	-- Further complication: if we're converting this binding into
	-- a case,  then try to avoid generating any case-of-case
	-- expressions by pulling out the floats.
	(_, other) ->
		 newStgVar ty	`thenUs` \ v ->
		 if isUnLiftedType ty
		   then returnUs (binds ++ [(v,arg')], StgVarArg v)
		   else returnUs ([(v, mkStgLets binds arg')], StgVarArg v)
	  where 
		ty = coreExprType arg

\end{code}


%************************************************************************
%*									*
\subsection[coreToStg-exprs]{Converting core expressions}
%*									*
%************************************************************************

\begin{code}
coreExprToStg :: StgEnv -> CoreExpr -> UniqSM StgExpr

coreExprToStg env (Var var)
  = returnUs (StgApp (stgLookup env var) [])

\end{code}

%************************************************************************
%*									*
\subsubsection[coreToStg-lambdas]{Lambda abstractions}
%*									*
%************************************************************************

\begin{code}
coreExprToStg env expr@(Lam _ _)
  = let
	(binders, body) = collectBinders expr
	id_binders      = filter isId binders
    in
    newLocalIds env id_binders		`thenUs` \ (env', binders') ->
    coreExprToStg env' body             `thenUs` \ stg_body ->

    if null id_binders then -- it was all type/usage binders; tossed
	returnUs stg_body
    else
    case stg_body of

      -- if the body reduced to a lambda too...
      (StgLet (StgNonRec var (StgRhsClosure cc bi srt fvs uf args body))
	      (StgApp var' []))
       | var == var' ->
 	returnUs (StgLet (StgNonRec var 
			    (StgRhsClosure noCCS
				stgArgOcc
				noSRT
				bOGUS_FVs
				ReEntrant
				(binders' ++ args)
				body))
		(StgApp var []))
				    
      other ->

	-- We must let-bind the lambda
	newStgVar (coreExprType expr)	`thenUs` \ var ->
	returnUs
	  (StgLet (StgNonRec var (StgRhsClosure noCCS
				  stgArgOcc
				  noSRT
				  bOGUS_FVs
				  ReEntrant 	-- binders is non-empty
				  binders'
				  stg_body))
	   (StgApp var []))
\end{code}

%************************************************************************
%*									*
\subsubsection[coreToStg-let(rec)]{Let and letrec expressions}
%*									*
%************************************************************************

\begin{code}
coreExprToStg env (Let bind body)
  = coreBindToStg env     bind   `thenUs` \ (stg_binds, new_env) ->
    coreExprToStg new_env body   `thenUs` \ stg_body ->
    returnUs (foldr StgLet stg_body stg_binds)
\end{code}


%************************************************************************
%*									*
\subsubsection[coreToStg-scc]{SCC expressions}
%*									*
%************************************************************************

Covert core @scc@ expression directly to STG @scc@ expression.
\begin{code}
coreExprToStg env (Note (SCC cc) expr)
  = coreExprToStg env expr   `thenUs` \ stg_expr ->
    returnUs (StgSCC cc stg_expr)
\end{code}

\begin{code}
coreExprToStg env (Note other_note expr) = coreExprToStg env expr
\end{code}

The rest are handled by coreExprStgFloat.

\begin{code}
coreExprToStg env expr
  = coreExprToStgFloat env expr  `thenUs` \ (binds,stg_expr) ->
    returnUs (mkStgLets binds stg_expr)
\end{code}

%************************************************************************
%*									*
\subsubsection[coreToStg-applications]{Applications}
%*									*
%************************************************************************

\begin{code}
coreExprToStgFloat env expr@(App _ _)
  = let
	(fun,args)    = collect_args expr []
    in
    coreArgsToStg env args		`thenUs` \ (binds, stg_args) ->

	-- Now deal with the function
    case (fun, stg_args) of
      (Var fun_id, _) -> 	-- A function Id, so do an StgApp; it's ok if
				-- there are no arguments.
			    returnUs (binds, 
				   StgApp (stgLookup env fun_id) stg_args)

      (non_var_fun, []) -> 	-- No value args, so recurse into the function
			    ASSERT( null binds )
			    coreExprToStg env non_var_fun `thenUs` \e ->
			    returnUs ([], e)

      other ->	-- A non-variable applied to things; better let-bind it.
		newStgVar (coreExprType fun)	`thenUs` \ fun_id ->
		coreExprToStg env fun		`thenUs` \ (stg_fun) ->
		let
		   fun_rhs = StgRhsClosure noCCS    -- No cost centre (ToDo?)
					   stgArgOcc
					   noSRT
					   bOGUS_FVs
					   SingleEntry	-- Only entered once
					   []
					   stg_fun
		in
		returnUs (binds,
			  StgLet (StgNonRec fun_id fun_rhs) $
			  StgApp fun_id stg_args)
  where
	-- Collect arguments
    collect_args (App fun arg)            args = collect_args fun (arg:args)
    collect_args (Note (Coerce _ _) expr) args = collect_args expr args
    collect_args (Note InlineCall   expr) args = collect_args expr args
    collect_args fun                      args = (fun, args)
\end{code}

%************************************************************************
%*									*
\subsubsection[coreToStg-con]{Constructors}
%*									*
%************************************************************************

\begin{code}
coreExprToStgFloat env expr@(Con (PrimOp (CCallOp (Right _) a b c)) args)
  = getUniqueUs			`thenUs` \ u ->
    coreArgsToStg env args      `thenUs` \ (binds, stg_atoms) ->
    let con' = PrimOp (CCallOp (Right u) a b c) in
    returnUs (binds, StgCon con' stg_atoms (coreExprType expr))

coreExprToStgFloat env expr@(Con con args)
  = coreArgsToStg env args	`thenUs` \ (binds, stg_atoms) ->
    returnUs (binds, StgCon con stg_atoms (coreExprType expr))
\end{code}

%************************************************************************
%*									*
\subsubsection[coreToStg-cases]{Case expressions}
%*									*
%************************************************************************

\begin{code}
coreExprToStgFloat env expr@(Case scrut bndr alts)
  = coreExprToStgFloat env scrut		`thenUs` \ (binds, scrut') ->
    newLocalId env bndr				`thenUs` \ (env', bndr') ->
    alts_to_stg env' (findDefault alts)		`thenUs` \ alts' ->
    returnUs (binds, mkStgCase scrut' bndr' alts')
  where
    scrut_ty  = idType bndr
    prim_case = isUnLiftedType scrut_ty && not (isUnboxedTupleType scrut_ty)

    alts_to_stg env (alts, deflt)
      | prim_case
      = default_to_stg env deflt		`thenUs` \ deflt' ->
	mapUs (prim_alt_to_stg env) alts	`thenUs` \ alts' ->
	returnUs (StgPrimAlts scrut_ty alts' deflt')

      | otherwise
      = default_to_stg env deflt		`thenUs` \ deflt' ->
	mapUs (alg_alt_to_stg env) alts		`thenUs` \ alts' ->
	returnUs (StgAlgAlts scrut_ty alts' deflt')

    alg_alt_to_stg env (DataCon con, bs, rhs)
	  = coreExprToStg env rhs    `thenUs` \ stg_rhs ->
	    returnUs (con, bs, [ True | b <- bs ]{-bogus use mask-}, stg_rhs)

    prim_alt_to_stg env (Literal lit, args, rhs)
	  = ASSERT( null args )
	    coreExprToStg env rhs    `thenUs` \ stg_rhs ->
	    returnUs (lit, stg_rhs)

    default_to_stg env Nothing
      = returnUs StgNoDefault

    default_to_stg env (Just rhs)
      = coreExprToStg env rhs    `thenUs` \ stg_rhs ->
	returnUs (StgBindDefault stg_rhs)
		-- The binder is used for prim cases and not otherwise
		-- (hack for old code gen)
\end{code}

\begin{code}
coreExprToStgFloat env expr
  = coreExprToStg env expr `thenUs` \stg_expr ->
    returnUs ([], stg_expr)
\end{code}

%************************************************************************
%*									*
\subsection[coreToStg-misc]{Miscellaneous helping functions}
%*									*
%************************************************************************

There's not anything interesting we can ASSERT about \tr{var} if it
isn't in the StgEnv. (WDP 94/06)

\begin{code}
stgLookup :: StgEnv -> Id -> Id
stgLookup env var = case (lookupVarEnv env var) of
		      Nothing  -> var
		      Just var -> var
\end{code}

Invent a fresh @Id@:
\begin{code}
newStgVar :: Type -> UniqSM Id
newStgVar ty
 = getUniqueUs	 		`thenUs` \ uniq ->
   returnUs (mkUserLocal (varOcc SLIT("stg")) uniq ty)
\end{code}

\begin{code}
newLocalId env id
  | externallyVisibleId id
  = returnUs (env, id)

  | otherwise
  =	-- Local binder, give it a new unique Id.
    getUniqueUs			`thenUs` \ uniq ->
    let
      id'     = setIdUnique id uniq
      new_env = extendVarEnv env id id'
    in
    returnUs (new_env, id')

newLocalIds :: StgEnv -> [Id] -> UniqSM (StgEnv, [Id])
newLocalIds env []
  = returnUs (env, [])
newLocalIds env (b:bs)
  = newLocalId env b	`thenUs` \ (env', b') ->
    newLocalIds env' bs	`thenUs` \ (env'', bs') ->
    returnUs (env'', b':bs')
\end{code}


\begin{code}
mkStgLets :: [(Id,StgExpr)] -> StgExpr -> StgExpr
mkStgLets binds body = foldr mkStgLet body binds

mkStgLet (bndr, rhs) body
  | isUnboxedTupleType bndr_ty
  = panic "mkStgLets: unboxed tuple"
  | isUnLiftedType bndr_ty
  = mkStgCase rhs bndr (StgPrimAlts bndr_ty [] (StgBindDefault body))

  | otherwise
  = StgLet (StgNonRec bndr (exprToRhs rhs)) body
  where
    bndr_ty = idType bndr

mkStgCase (StgLet bind expr) bndr alts
  = StgLet bind (mkStgCase expr bndr alts)
mkStgCase scrut bndr alts
  = StgCase scrut bOGUS_LVs bOGUS_LVs bndr noSRT alts
\end{code}
