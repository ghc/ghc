%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
%************************************************************************
%*									*
\section[CoreToStg]{Converting core syntax to STG syntax}
%*									*
%************************************************************************

Convert a @CoreSyntax@ program to a @StgSyntax@ program.

\begin{code}
#include "HsVersions.h"

module CoreToStg ( topCoreBindsToStg ) where

IMP_Ubiq(){-uitous-}
IMPORT_1_3(Ratio(numerator,denominator))

import CoreSyn		-- input
import StgSyn		-- output

import Bag		( emptyBag, unitBag, unionBags, unionManyBags, bagToList )
import CoreUtils	( coreExprType )
import CostCentre	( noCostCentre )
import Id		( mkSysLocal, idType, isBottomingId,
			  externallyVisibleId,

			  nullIdEnv, addOneToIdEnv, lookupIdEnv, growIdEnvList,
			  SYN_IE(IdEnv), GenId{-instance NamedThing-}, SYN_IE(Id)
			)
import Literal		( mkMachInt, Literal(..) )
import PrelVals		( unpackCStringId, unpackCString2Id,
			  integerZeroId, integerPlusOneId,
			  integerPlusTwoId, integerMinusOneId
			)
import PrimOp		( PrimOp(..) )
import SpecUtils	( mkSpecialisedCon )
import SrcLoc		( noSrcLoc )
import TyCon		( TyCon{-instance Uniquable-} )
import Type		( getAppDataTyConExpandingDicts, SYN_IE(Type) )
import TysWiredIn	( stringTy )
import Unique		( integerTyConKey, ratioTyConKey, Unique{-instance Eq-} )
import UniqSupply	-- all of it, really
import Util		( zipLazy, panic, assertPanic{-, pprTrace ToDo:rm-} )

isLeakFreeType x y = False -- safe option; ToDo
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

Because we're going to come across ``boring'' bindings like
\tr{let x = /\ tyvars -> y in ...}, we want to keep a small
environment, so we can just replace all occurrences of \tr{x}
with \tr{y}.

\begin{code}
type StgEnv = IdEnv StgArg
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
		  -> [CoreBinding]	-- input
		  -> [StgBinding]	-- output

topCoreBindsToStg us core_binds
  = initUs us (coreBindsToStg nullIdEnv core_binds)
  where
    coreBindsToStg :: StgEnv -> [CoreBinding] -> UniqSM [StgBinding]

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
	      -> CoreBinding
	      -> UniqSM ([StgBinding],	-- Empty or singleton
		    	 StgEnv)	-- Floats

coreBindToStg env (NonRec binder rhs)
  = coreRhsToStg env rhs	`thenUs` \ stg_rhs ->
    let
	-- Binds to return if RHS is trivial
	triv_binds | externallyVisibleId binder = [StgNonRec binder stg_rhs]	-- Retain it
		   | otherwise	      	        = []				-- Discard it
    in
    case stg_rhs of
      StgRhsClosure cc bi fvs upd [] (StgApp atom [] lvs) ->
		-- Trivial RHS, so augment envt, and ditch the binding
		returnUs (triv_binds, new_env)
	   where
		new_env = addOneToIdEnv env binder atom

      StgRhsCon cc con_id [] ->
		-- Trivial RHS, so augment envt, and ditch the binding
		returnUs (triv_binds, new_env)
	   where
		new_env = addOneToIdEnv env binder (StgConArg con_id)

      other -> 	-- Non-trivial RHS, so don't augment envt
		returnUs ([StgNonRec binder stg_rhs], env)

coreBindToStg env (Rec pairs)
  = -- NB: *** WE DO NOT CHECK FOR TRIV_BINDS in REC BIND ****
    -- (possibly ToDo)
    let
	(binders, rhss) = unzip pairs
    in
    mapUs (coreRhsToStg env) rhss `thenUs` \ stg_rhss ->
    returnUs ([StgRec (binders `zip` stg_rhss)], env)
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

    let stg_rhs = case stg_expr of
		    StgLet (StgNonRec var1 rhs) (StgApp (StgVarArg var2) [] _)
			| var1 == var2 -> rhs
			-- This curious stuff is to unravel what a lambda turns into
			-- We have to do it this way, rather than spot a lambda in the
			-- incoming rhs.  Why?  Because trivial bindings might conceal
			-- what the rhs is actually like.

		    StgCon con args _ -> StgRhsCon noCostCentre con args

		    other -> StgRhsClosure noCostCentre	-- No cost centre (ToDo?)
					   stgArgOcc	-- safe
					   bOGUS_FVs
					   Updatable	-- Be pessimistic
					   []
					   stg_expr
    in
    returnUs stg_rhs
\end{code}


%************************************************************************
%*									*
\subsection[coreToStg-atoms{Converting atoms}
%*									*
%************************************************************************

\begin{code}
coreArgsToStg :: StgEnv -> [CoreArg] -> ([Type], [StgArg])

coreArgsToStg env [] = ([], [])
coreArgsToStg env (a:as)
  = case a of
	TyArg    t -> (t:trest, vrest)
	UsageArg u -> (trest,   vrest)
	VarArg   v -> (trest,   stgLookup env v : vrest)
	LitArg   l -> (trest,   StgLitArg l     : vrest)
  where
    (trest,vrest) = coreArgsToStg env as
\end{code}


%************************************************************************
%*									*
\subsection[coreToStg-exprs]{Converting core expressions}
%*									*
%************************************************************************

\begin{code}
coreExprToStg :: StgEnv -> CoreExpr -> UniqSM StgExpr

coreExprToStg env (Lit lit)
  = returnUs (StgApp (StgLitArg lit) [] bOGUS_LVs)

coreExprToStg env (Var var)
  = returnUs (mk_app (stgLookup env var) [])

coreExprToStg env (Con con args)
  = let
	(types, stg_atoms) = coreArgsToStg env args
	spec_con = mkSpecialisedCon con types
    in
    returnUs (StgCon spec_con stg_atoms bOGUS_LVs)

coreExprToStg env (Prim op args)
  = let
	(types, stg_atoms) = coreArgsToStg env args
    in
    returnUs (StgPrim op stg_atoms bOGUS_LVs)
\end{code}

%************************************************************************
%*									*
\subsubsection[coreToStg-lambdas]{Lambda abstractions}
%*									*
%************************************************************************

\begin{code}
coreExprToStg env expr@(Lam _ _)
  = let
	(_,_, binders, body) = collectBinders expr
    in
    coreExprToStg env body		`thenUs` \ stg_body ->

    if null binders then -- it was all type/usage binders; tossed
	returnUs stg_body
    else
	newStgVar (coreExprType expr)	`thenUs` \ var ->
	returnUs
	  (StgLet (StgNonRec var
				  (StgRhsClosure noCostCentre
				  stgArgOcc
				  bOGUS_FVs
				  ReEntrant 	-- binders is non-empty
				  binders
				  stg_body))
	   (StgApp (StgVarArg var) [] bOGUS_LVs))
\end{code}

%************************************************************************
%*									*
\subsubsection[coreToStg-applications]{Applications}
%*									*
%************************************************************************

\begin{code}
coreExprToStg env expr@(App _ _)
  = let
	(fun,args)    = collect_args expr []
	(_, stg_args) = coreArgsToStg env args
    in
	-- Now deal with the function
    case (fun, args) of
      (Var fun_id, _) -> 	-- A function Id, so do an StgApp; it's ok if
				-- there are no arguments.
			    returnUs (mk_app (stgLookup env fun_id) stg_args)

      (non_var_fun, []) -> 	-- No value args, so recurse into the function
			    coreExprToStg env non_var_fun

      other ->	-- A non-variable applied to things; better let-bind it.
--		pprTrace "coreExprToStg" (ppr PprDebug expr) $
		newStgVar (coreExprType fun)	`thenUs` \ fun_id ->
		coreExprToStg env fun		`thenUs` \ (stg_fun) ->
		let
		   fun_rhs = StgRhsClosure noCostCentre	-- No cost centre (ToDo?)
					   stgArgOcc
					   bOGUS_FVs
					   SingleEntry	-- Only entered once
					   []
					   stg_fun
		in
		returnUs (StgLet (StgNonRec fun_id fun_rhs)
			   	 (StgApp (StgVarArg fun_id) stg_args bOGUS_LVs))
  where
	-- Collect arguments, discarding type/usage applications
    collect_args (App e   (TyArg _))    args = collect_args e   args
    collect_args (App e   (UsageArg _)) args = collect_args e   args
    collect_args (App fun arg)          args = collect_args fun (arg:args)
    collect_args (Coerce _ _ expr)      args = collect_args expr args
    collect_args fun                    args = (fun, args)
\end{code}

%************************************************************************
%*									*
\subsubsection[coreToStg-cases]{Case expressions}
%*									*
%************************************************************************

\begin{code}
coreExprToStg env (Case discrim alts)
  = coreExprToStg env discrim		`thenUs` \ stg_discrim ->
    alts_to_stg discrim alts		`thenUs` \ stg_alts ->
    getUnique				`thenUs` \ uniq ->
    returnUs (
	StgCase stg_discrim
		bOGUS_LVs
		bOGUS_LVs
		uniq
		stg_alts
    )
  where
    discrim_ty		    = coreExprType discrim
    (_, discrim_ty_args, _) = getAppDataTyConExpandingDicts discrim_ty

    alts_to_stg discrim (AlgAlts alts deflt)
      = default_to_stg discrim deflt		`thenUs` \ stg_deflt ->
	mapUs boxed_alt_to_stg alts		`thenUs` \ stg_alts  ->
	returnUs (StgAlgAlts discrim_ty stg_alts stg_deflt)
      where
	boxed_alt_to_stg (con, bs, rhs)
	  = coreExprToStg env rhs    `thenUs` \ stg_rhs ->
	    returnUs (spec_con, bs, [ True | b <- bs ]{-bogus use mask-}, stg_rhs)
	  where
	    spec_con = mkSpecialisedCon con discrim_ty_args

    alts_to_stg discrim (PrimAlts alts deflt)
      = default_to_stg discrim deflt		`thenUs` \ stg_deflt ->
	mapUs unboxed_alt_to_stg alts		`thenUs` \ stg_alts  ->
	returnUs (StgPrimAlts discrim_ty stg_alts stg_deflt)
      where
	unboxed_alt_to_stg (lit, rhs)
	  = coreExprToStg env rhs    `thenUs` \ stg_rhs ->
	    returnUs (lit, stg_rhs)

    default_to_stg discrim NoDefault
      = returnUs StgNoDefault

    default_to_stg discrim (BindDefault binder rhs)
      = coreExprToStg env rhs    `thenUs` \ stg_rhs ->
	returnUs (StgBindDefault binder True{-used? no it is lying-} stg_rhs)
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
    returnUs (mkStgLets stg_binds stg_body)
\end{code}


%************************************************************************
%*									*
\subsubsection[coreToStg-scc]{SCC expressions}
%*									*
%************************************************************************

Covert core @scc@ expression directly to STG @scc@ expression.
\begin{code}
coreExprToStg env (SCC cc expr)
  = coreExprToStg env expr   `thenUs` \ stg_expr ->
    returnUs (StgSCC (coreExprType expr) cc stg_expr)
\end{code}

\begin{code}
coreExprToStg env (Coerce c ty expr) = coreExprToStg env expr
\end{code}


%************************************************************************
%*									*
\subsection[coreToStg-misc]{Miscellaneous helping functions}
%*									*
%************************************************************************

There's not anything interesting we can ASSERT about \tr{var} if it
isn't in the StgEnv. (WDP 94/06)

\begin{code}
stgLookup :: StgEnv -> Id -> StgArg
stgLookup env var = case (lookupIdEnv env var) of
		      Nothing   -> StgVarArg var
		      Just atom -> atom
\end{code}

Invent a fresh @Id@:
\begin{code}
newStgVar :: Type -> UniqSM Id
newStgVar ty
 = getUnique			`thenUs` \ uniq ->
   returnUs (mkSysLocal SLIT("stg") uniq ty noSrcLoc)
\end{code}

\begin{code}
mkStgLets ::   [StgBinding]
	    -> StgExpr	-- body of let
	    -> StgExpr

mkStgLets binds body = foldr StgLet body binds

-- mk_app spots an StgCon in a function position, 
-- and turns it into an StgCon. See notes with
-- getArgAmode in CgBindery.
mk_app (StgConArg con) args = StgCon con       args bOGUS_LVs
mk_app other_fun       args = StgApp other_fun args bOGUS_LVs
\end{code}
