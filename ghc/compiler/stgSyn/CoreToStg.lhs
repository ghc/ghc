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
			  nullIdEnv, addOneToIdEnv, lookupIdEnv,
			  SYN_IE(IdEnv), GenId{-instance NamedThing-}
			)
import Literal		( mkMachInt, Literal(..) )
import PrelVals		( unpackCStringId, unpackCString2Id,
			  integerZeroId, integerPlusOneId,
			  integerPlusTwoId, integerMinusOneId
			)
import PrimOp		( PrimOp(..) )
import SpecUtils	( mkSpecialisedCon )
import SrcLoc		( mkUnknownSrcLoc )
import TyCon		( TyCon{-instance Uniquable-} )
import Type		( maybeAppDataTyCon, getAppDataTyConExpandingDicts )
import TysWiredIn	( stringTy )
import Unique		( integerTyConKey, ratioTyConKey, Unique{-instance Eq-} )
import UniqSupply	-- all of it, really
import Util		( panic, assertPanic{-, pprTrace ToDo:rm-} )
--import Pretty--ToDo:rm
--import PprStyle--ToDo:rm
--import PprType  --ToDo:rm
--import Outputable--ToDo:rm
--import PprEnv--ToDo:rm

isLeakFreeType x y = False -- safe option; ToDo
\end{code}


	***************  OVERVIEW   *********************


The business of this pass is to convert Core to Stg.  On the way:

* We discard type lambdas and applications. In so doing we discard
  "trivial" bindings such as
	x = y t1 t2
  where t1, t2 are types

* We make the representation of NoRep literals explicit, and
  float their bindings to the top level

* We do *not* pin on the correct free/live var info; that's done later.
  Instead we use bOGUS_LVS and _FVS as a placeholder.

* We convert	case x of {...; x' -> ...x'...}
	to
		case x of {...; _  -> ...x... }

  See notes in SimplCase.lhs, near simplDefault for the reasoning here.


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
  = case (initUs us (binds_to_stg nullIdEnv core_binds)) of
      (_, stuff) -> stuff
  where
    binds_to_stg :: StgEnv -> [CoreBinding] -> UniqSM [StgBinding]

    binds_to_stg env [] = returnUs []
    binds_to_stg env (b:bs)
      = do_top_bind  env     b  `thenUs` \ (new_b, new_env, float_binds) ->
    	binds_to_stg new_env bs `thenUs` \ new_bs ->
    	returnUs (bagToList float_binds ++ 	-- Literals
		  new_b ++
		  new_bs)

    do_top_bind env bind@(Rec pairs)
      = coreBindToStg env bind

    do_top_bind env bind@(NonRec var rhs)
      = coreBindToStg env bind		`thenUs` \ (stg_binds, new_env, float_binds) ->
{- TESTING:
	let
	    ppr_blah xs = ppInterleave ppComma (map pp_x xs)
	    pp_x (u,x) = ppBesides [pprUnique u, ppStr ": ", ppr PprDebug x]
	in
	pprTrace "do_top_bind:" (ppAbove (ppr PprDebug stg_binds) (ppr_blah (ufmToList new_env))) $
-}
 	case stg_binds of
	   [StgNonRec var (StgRhsClosure cc bi fvs u [] rhs_body)] ->
		-- Mega-special case; there's still a binding there
		-- no fvs (of course), *no args*, "let" rhs
		let
		  (extra_float_binds, rhs_body') = seek_liftable [] rhs_body
		in
		returnUs (extra_float_binds ++
			  [StgNonRec var (StgRhsClosure cc bi fvs u [] rhs_body')],
			  new_env,
			  float_binds)

	   other -> returnUs (stg_binds, new_env, float_binds)

    --------------------
    -- HACK: look for very simple, obviously-liftable bindings
    -- that can come up to the top level; those that couldn't
    -- 'cause they were big-lambda constrained in the Core world.

    seek_liftable :: [StgBinding] 	-- accumulator...
		  -> StgExpr	-- look for top-lev liftables
		  -> ([StgBinding], StgExpr)	-- result

    seek_liftable acc expr@(StgLet inner_bind body)
      | is_liftable inner_bind
      =	seek_liftable (inner_bind : acc) body

    seek_liftable acc other_expr = (reverse acc, other_expr) -- Finished

    --------------------
    is_liftable (StgNonRec binder (StgRhsClosure _ _ _ _ args body))
      = not (null args) -- it's manifestly a function...
	|| isLeakFreeType [] (idType binder)
	|| is_whnf body
	-- ToDo: use a decent manifestlyWHNF function for STG?
      where
	is_whnf (StgCon _ _ _) 	    = True
	is_whnf (StgApp (StgVarArg v) _ _) = isBottomingId v
	is_whnf other 			    = False

    is_liftable (StgRec [(_, StgRhsClosure _ _ _ _ args body)])
      = not (null args) -- it's manifestly a (recursive) function...

    is_liftable anything_else = False
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
		    	 StgEnv,		-- New envt
			 Bag StgBinding)	-- Floats

coreBindToStg env (NonRec binder rhs)
  = coreRhsToStg env rhs	`thenUs` \ (stg_rhs, rhs_binds) ->

    let
	-- Binds to return if RHS is trivial
	triv_binds = if externallyVisibleId binder then
			-- pprTrace "coreBindToStg:keeping:" (ppCat [ppr PprDebug binder, ppr PprDebug (externallyVisibleId binder)]) $
			[StgNonRec binder stg_rhs]	-- Retain it
		     else
			-- pprTrace "coreBindToStg:tossing:" (ppCat [ppr PprDebug binder, ppr PprDebug (externallyVisibleId binder)]) $
			[]				-- Discard it
    in
    case stg_rhs of
      StgRhsClosure cc bi fvs upd [] (StgApp atom [] lvs) ->
		-- Trivial RHS, so augment envt, and ditch the binding
		returnUs (triv_binds, new_env, rhs_binds)
	   where
		new_env = addOneToIdEnv env binder atom

      StgRhsCon cc con_id [] ->
		-- Trivial RHS, so augment envt, and ditch the binding
		returnUs (triv_binds, new_env, rhs_binds)
	   where
		new_env = addOneToIdEnv env binder (StgVarArg con_id)

      other -> 	-- Non-trivial RHS, so don't augment envt
		returnUs ([StgNonRec binder stg_rhs], env, rhs_binds)

coreBindToStg env (Rec pairs)
  = -- NB: *** WE DO NOT CHECK FOR TRIV_BINDS in REC BIND ****
    -- (possibly ToDo)
    let
	(binders, rhss) = unzip pairs
    in
    mapAndUnzipUs (coreRhsToStg env) rhss `thenUs` \ (stg_rhss, rhs_binds) ->
    returnUs ([StgRec (binders `zip` stg_rhss)], env, unionManyBags rhs_binds)
\end{code}


%************************************************************************
%*									*
\subsection[coreToStg-rhss]{Converting right hand sides}
%*									*
%************************************************************************

\begin{code}
coreRhsToStg :: StgEnv -> CoreExpr -> UniqSM (StgRhs, Bag StgBinding)

coreRhsToStg env core_rhs
  = coreExprToStg env core_rhs 	`thenUs` \ (stg_expr, stg_binds) ->

    let stg_rhs = case stg_expr of
		    StgLet (StgNonRec var1 rhs) (StgApp (StgVarArg var2) [] _)
			| var1 == var2 -> rhs
			-- This curious stuff is to unravel what a lambda turns into
			-- We have to do it this way, rather than spot a lambda in the
			-- incoming rhs

		    StgCon con args _ -> StgRhsCon noCostCentre con args

		    other -> StgRhsClosure noCostCentre	-- No cost centre (ToDo?)
					   stgArgOcc	-- safe
					   bOGUS_FVs
					   Updatable	-- Be pessimistic
					   []
					   stg_expr
    in
    returnUs (stg_rhs, stg_binds)
\end{code}


%************************************************************************
%*									*
\subsection[coreToStg-lits]{Converting literals}
%*									*
%************************************************************************

Literals: the NoRep kind need to be de-no-rep'd.
We always replace them with a simple variable, and float a suitable
binding out to the top level.

If an Integer is small enough (Haskell implementations must support
Ints in the range $[-2^29+1, 2^29-1]$), wrap it up in @int2Integer@;
otherwise, wrap with @litString2Integer@.

\begin{code}
tARGET_MIN_INT, tARGET_MAX_INT :: Integer
tARGET_MIN_INT = -536870912
tARGET_MAX_INT =  536870912

litToStgArg :: Literal -> UniqSM (StgArg, Bag StgBinding)

litToStgArg (NoRepStr s)
  = newStgVar stringTy 		`thenUs` \ var ->
    let
	rhs = StgRhsClosure noCostCentre -- No cost centre (ToDo?)
			    stgArgOcc	 -- safe
			    bOGUS_FVs
			    Updatable	 -- WAS: ReEntrant (see note below)
			    []		 -- No arguments
			    val

-- We used not to update strings, so that they wouldn't clog up the heap,
-- but instead be unpacked each time.  But on some programs that costs a lot
-- [eg hpg], so now we update them.

	val = if (any is_NUL (_UNPK_ s)) then -- must cater for NULs in literal string
		StgApp (StgVarArg unpackCString2Id)
		     [StgLitArg (MachStr s),
		      StgLitArg (mkMachInt (toInteger (_LENGTH_ s)))]
		     bOGUS_LVs
	      else
		StgApp (StgVarArg unpackCStringId)
		     [StgLitArg (MachStr s)]
		     bOGUS_LVs
    in
    returnUs (StgVarArg var, unitBag (StgNonRec var rhs))
  where
    is_NUL c = c == '\0'

litToStgArg (NoRepInteger i integer_ty)
  -- extremely convenient to look out for a few very common
  -- Integer literals!
  | i == 0    = returnUs (StgVarArg integerZeroId,     emptyBag)
  | i == 1    = returnUs (StgVarArg integerPlusOneId,  emptyBag)
  | i == 2    = returnUs (StgVarArg integerPlusTwoId,  emptyBag)
  | i == (-1) = returnUs (StgVarArg integerMinusOneId, emptyBag)

  | otherwise
  = newStgVar integer_ty	`thenUs` \ var ->
    let
	rhs = StgRhsClosure noCostCentre -- No cost centre (ToDo?)
			    stgArgOcc	 -- safe
			    bOGUS_FVs
			    Updatable	 -- Update an integer
			    []		 -- No arguments
			    val

	val
	  | i > tARGET_MIN_INT && i < tARGET_MAX_INT
	  =	-- Start from an Int
	    StgPrim Int2IntegerOp [StgLitArg (mkMachInt i)] bOGUS_LVs

	  | otherwise
	  = 	-- Start from a string
	    StgPrim Addr2IntegerOp [StgLitArg (MachStr (_PK_ (show i)))] bOGUS_LVs
    in
    returnUs (StgVarArg var, unitBag (StgNonRec var rhs))

litToStgArg (NoRepRational r rational_ty)
  = --ASSERT(is_rational_ty)
    --(if is_rational_ty then \x->x else pprTrace "litToStgArg:not rational?" (pprType PprDebug rational_ty)) $
    litToStgArg (NoRepInteger (numerator   r) integer_ty) `thenUs` \ (num_atom,   binds1) ->
    litToStgArg (NoRepInteger (denominator r) integer_ty) `thenUs` \ (denom_atom, binds2) ->
    newStgVar rational_ty			`thenUs` \ var ->
    let
	 rhs = StgRhsCon noCostCentre	-- No cost centre (ToDo?)
			 ratio_data_con	-- Constructor
			 [num_atom, denom_atom]
    in
    returnUs (StgVarArg var, binds1 `unionBags`
			    binds2 `unionBags`
			    unitBag (StgNonRec var rhs))
  where
    (is_rational_ty, ratio_data_con, integer_ty)
      = case (maybeAppDataTyCon rational_ty) of
	  Just (tycon, [i_ty], [con])
	    -> ASSERT(is_integer_ty i_ty)
	       (uniqueOf tycon == ratioTyConKey, con, i_ty)

	  _ -> (False, panic "ratio_data_con", panic "integer_ty")

    is_integer_ty ty
      = case (maybeAppDataTyCon ty) of
	  Just (tycon, [], _) -> uniqueOf tycon == integerTyConKey
	  _ -> False

litToStgArg other_lit = returnUs (StgLitArg other_lit, emptyBag)
\end{code}


%************************************************************************
%*									*
\subsection[coreToStg-atoms{Converting atoms}
%*									*
%************************************************************************

\begin{code}
coreArgsToStg :: StgEnv -> [CoreArg] -> UniqSM ([Type], [StgArg], Bag StgBinding)

coreArgsToStg env [] = returnUs ([], [], emptyBag)
coreArgsToStg env (a:as)
  = coreArgsToStg env as    `thenUs` \ (tys, args, binds) ->
    do_arg a tys args binds
  where
    do_arg a trest vrest binds
      = case a of
	  TyArg    t -> returnUs (t:trest, vrest, binds)
	  UsageArg u -> returnUs (trest, vrest, binds)
	  VarArg   v -> returnUs (trest, stgLookup env v : vrest, binds)
	  LitArg   i -> litToStgArg i `thenUs` \ (v, bs) ->
			returnUs (trest, v:vrest, bs `unionBags` binds)
\end{code}

There's not anything interesting we can ASSERT about \tr{var} if it
isn't in the StgEnv. (WDP 94/06)
\begin{code}
stgLookup :: StgEnv -> Id -> StgArg

stgLookup env var = case (lookupIdEnv env var) of
		      Nothing   -> StgVarArg var
		      Just atom -> atom
\end{code}

%************************************************************************
%*									*
\subsection[coreToStg-exprs]{Converting core expressions}
%*									*
%************************************************************************

\begin{code}
coreExprToStg :: StgEnv
	      -> CoreExpr
	      -> UniqSM (StgExpr,		-- Result
			 Bag StgBinding)	-- Float these to top level
\end{code}

\begin{code}
coreExprToStg env (Lit lit)
  = litToStgArg lit	`thenUs` \ (atom, binds) ->
    returnUs (StgApp atom [] bOGUS_LVs, binds)

coreExprToStg env (Var var)
  = returnUs (StgApp (stgLookup env var) [] bOGUS_LVs, emptyBag)

coreExprToStg env (Con con args)
  = coreArgsToStg env args  `thenUs` \ (types, stg_atoms, stg_binds) ->
    let
	spec_con = mkSpecialisedCon con types
    in
    returnUs (StgCon spec_con stg_atoms bOGUS_LVs, stg_binds)

coreExprToStg env (Prim op args)
  = coreArgsToStg env args  `thenUs` \ (_, stg_atoms, stg_binds) ->
    returnUs (StgPrim op stg_atoms bOGUS_LVs, stg_binds)
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
    coreExprToStg env body		`thenUs` \ stuff@(stg_body, binds) ->

    if null binders then -- it was all type/usage binders; tossed
	returnUs stuff
    else
	newStgVar (coreExprType expr)	`thenUs` \ var ->
	returnUs
	  (StgLet (StgNonRec var (StgRhsClosure noCostCentre
				  stgArgOcc
				  bOGUS_FVs
				  ReEntrant 	-- binders is non-empty
				  binders
				  stg_body))
	   (StgApp (StgVarArg var) [] bOGUS_LVs),
	   binds)
\end{code}

%************************************************************************
%*									*
\subsubsection[coreToStg-applications]{Applications}
%*									*
%************************************************************************

\begin{code}
coreExprToStg env expr@(App _ _)
  = let
	(fun,args) = collect_args expr []
    in
	-- Deal with the arguments
    coreArgsToStg env args `thenUs` \ (_, stg_args, arg_binds) ->

	-- Now deal with the function
    case (fun, args) of
      (Var fun_id, _) -> 	-- A function Id, so do an StgApp; it's ok if
				-- there are no arguments.
			    returnUs (StgApp (stgLookup env fun_id) stg_args bOGUS_LVs, arg_binds)

      (non_var_fun, []) -> 	-- No value args, so recurse into the function
			    coreExprToStg env non_var_fun

      other ->	-- A non-variable applied to things; better let-bind it.
		newStgVar (coreExprType fun)	`thenUs` \ fun_id ->
		coreExprToStg env fun		`thenUs` \ (stg_fun, fun_binds) ->
		let
		   fun_rhs = StgRhsClosure noCostCentre	-- No cost centre (ToDo?)
					   stgArgOcc
					   bOGUS_FVs
					   SingleEntry	-- Only entered once
					   []
					   stg_fun
		in
		returnUs (StgLet (StgNonRec fun_id fun_rhs)
			   	  (StgApp (StgVarArg fun_id) stg_args bOGUS_LVs),
			   arg_binds `unionBags` fun_binds)
  where
	-- Collect arguments, discarding type/usage applications
    collect_args (App e   (TyArg _))    args = collect_args e   args
    collect_args (App e   (UsageArg _)) args = collect_args e   args
    collect_args (App fun arg)          args = collect_args fun (arg:args)
    collect_args fun                    args = (fun, args)
\end{code}

%************************************************************************
%*									*
\subsubsection[coreToStg-cases]{Case expressions}
%*									*
%************************************************************************

At this point, we *mangle* cases involving fork# and par# in the
discriminant.  The original templates for these primops (see
@PrelVals.lhs@) constructed case expressions with boolean results
solely to fool the strictness analyzer, the simplifier, and anyone
else who might want to fool with the evaluation order.  Now, we
believe that once the translation to STG code is performed, our
evaluation order is safe.  Therefore, we convert expressions of the
form:

    case par# e of
      True -> rhs
      False -> parError#

to

    case par# e of
      _ -> rhs

\begin{code}

coreExprToStg env (Case discrim@(Prim op _) alts)
  | funnyParallelOp op
  = getUnique			`thenUs` \ uniq ->
    coreExprToStg env discrim	`thenUs` \ (stg_discrim, discrim_binds) ->
    alts_to_stg alts		`thenUs` \ (stg_alts, alts_binds) ->
    returnUs (
	StgCase stg_discrim
		bOGUS_LVs
		bOGUS_LVs
		uniq
		stg_alts,
	discrim_binds `unionBags` alts_binds
    )
  where
    funnyParallelOp SeqOp  = True
    funnyParallelOp ParOp  = True
    funnyParallelOp ForkOp = True
    funnyParallelOp _      = False

    discrim_ty = coreExprType discrim

    alts_to_stg (PrimAlts _ (BindDefault binder rhs))
      =	coreExprToStg env rhs  `thenUs` \ (stg_rhs, rhs_binds) ->
	let
	    stg_deflt = StgBindDefault binder False stg_rhs
	in
	    returnUs (StgPrimAlts discrim_ty [] stg_deflt, rhs_binds)

-- OK, back to real life...

coreExprToStg env (Case discrim alts)
  = coreExprToStg env discrim		`thenUs` \ (stg_discrim, discrim_binds) ->
    alts_to_stg discrim alts	`thenUs` \ (stg_alts, alts_binds) ->
    getUnique				`thenUs` \ uniq ->
    returnUs (
	StgCase stg_discrim
		bOGUS_LVs
		bOGUS_LVs
		uniq
		stg_alts,
	discrim_binds `unionBags` alts_binds
    )
  where
    discrim_ty		    = coreExprType discrim
    (_, discrim_ty_args, _) = getAppDataTyConExpandingDicts discrim_ty

    alts_to_stg discrim (AlgAlts alts deflt)
      = default_to_stg discrim deflt		`thenUs` \ (stg_deflt, deflt_binds) ->
	mapAndUnzipUs boxed_alt_to_stg alts	`thenUs` \ (stg_alts, alts_binds)  ->
	returnUs (StgAlgAlts discrim_ty stg_alts stg_deflt,
		  deflt_binds `unionBags` unionManyBags alts_binds)
      where
	boxed_alt_to_stg (con, bs, rhs)
	  = coreExprToStg env rhs    `thenUs` \ (stg_rhs, rhs_binds) ->
	    returnUs ((spec_con, bs, [ True | b <- bs ]{-bogus use mask-}, stg_rhs),
		       rhs_binds)
	  where
	    spec_con = mkSpecialisedCon con discrim_ty_args

    alts_to_stg discrim (PrimAlts alts deflt)
      = default_to_stg discrim deflt		`thenUs` \ (stg_deflt,deflt_binds) ->
	mapAndUnzipUs unboxed_alt_to_stg alts	`thenUs` \ (stg_alts, alts_binds)  ->
	returnUs (StgPrimAlts discrim_ty stg_alts stg_deflt,
		  deflt_binds `unionBags` unionManyBags alts_binds)
      where
	unboxed_alt_to_stg (lit, rhs)
	  = coreExprToStg env rhs    `thenUs` \ (stg_rhs, rhs_binds) ->
	    returnUs ((lit, stg_rhs), rhs_binds)

    default_to_stg discrim NoDefault
      = returnUs (StgNoDefault, emptyBag)

    default_to_stg discrim (BindDefault binder rhs)
      = coreExprToStg new_env rhs    `thenUs` \ (stg_rhs, rhs_binds) ->
	returnUs (StgBindDefault binder True{-used? no it is lying-} stg_rhs,
		  rhs_binds)
      where
	--
	-- We convert	case x of {...; x' -> ...x'...}
	--	to
	--		case x of {...; _  -> ...x... }
	--
	-- See notes in SimplCase.lhs, near simplDefault for the reasoning.
	-- It's quite easily done: simply extend the environment to bind the
	-- default binder to the scrutinee.
	--
	new_env = case discrim of
		    Var v -> addOneToIdEnv env binder (stgLookup env v)
		    other   -> env
\end{code}

%************************************************************************
%*									*
\subsubsection[coreToStg-let(rec)]{Let and letrec expressions}
%*									*
%************************************************************************

\begin{code}
coreExprToStg env (Let bind body)
  = coreBindToStg env     bind   `thenUs` \ (stg_binds, new_env, float_binds1) ->
    coreExprToStg new_env body   `thenUs` \ (stg_body, float_binds2) ->
    returnUs (mkStgLets stg_binds stg_body, float_binds1 `unionBags` float_binds2)
\end{code}


%************************************************************************
%*									*
\subsubsection[coreToStg-scc]{SCC expressions}
%*									*
%************************************************************************

Covert core @scc@ expression directly to STG @scc@ expression.
\begin{code}
coreExprToStg env (SCC cc expr)
  = coreExprToStg env expr   `thenUs` \ (stg_expr, binds) ->
    returnUs (StgSCC (coreExprType expr) cc stg_expr, binds)
\end{code}

\begin{code}
coreExprToStg env (Coerce c ty expr) = coreExprToStg env expr
\end{code}


%************************************************************************
%*									*
\subsection[coreToStg-misc]{Miscellaneous helping functions}
%*									*
%************************************************************************

Utilities.

Invent a fresh @Id@:
\begin{code}
newStgVar :: Type -> UniqSM Id
newStgVar ty
 = getUnique			`thenUs` \ uniq ->
   returnUs (mkSysLocal SLIT("stg") uniq ty mkUnknownSrcLoc)
\end{code}

\begin{code}
mkStgLets ::   [StgBinding]
	    -> StgExpr	-- body of let
	    -> StgExpr

mkStgLets binds body = foldr StgLet body binds
\end{code}
