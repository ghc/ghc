%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1995
%
%************************************************************************
%*									*
\section[CoreToStg]{Converting core syntax to STG syntax}
%*									*
%************************************************************************

Convert a @CoreSyntax@ program to a @StgSyntax@ program.


\begin{code}
#include "HsVersions.h"

module CoreToStg (
	topCoreBindsToStg,

	-- and to make the interface self-sufficient...
	SplitUniqSupply, Id, CoreExpr, CoreBinding, StgBinding,
	StgRhs, StgBinderInfo
    ) where

import PlainCore	-- input
import AnnCoreSyn	-- intermediate form on which all work is done
import StgSyn		-- output
import SplitUniq
import Unique		-- the UniqueSupply monadery used herein

import AbsPrel		( unpackCStringId, unpackCString2Id, stringTy,
			  integerTy, rationalTy, ratioDataCon,
			  PrimOp(..),		-- For Int2IntegerOp etc
			  integerZeroId, integerPlusOneId,
			  integerPlusTwoId, integerMinusOneId
			  IF_ATTACK_PRAGMAS(COMMA mkListTy COMMA charTy)
			  IF_ATTACK_PRAGMAS(COMMA tagOf_PrimOp)
			  IF_ATTACK_PRAGMAS(COMMA pprPrimOp)
			)

import AbsUniType	( isPrimType, isLeakFreeType, getUniDataTyCon )
import Bag		-- Bag operations
import BasicLit		( mkMachInt, BasicLit(..), PrimKind )	-- ToDo: its use is ugly...
import CostCentre	( noCostCentre, CostCentre )
import Id		( mkSysLocal, getIdUniType, isBottomingId
			  IF_ATTACK_PRAGMAS(COMMA bottomIsGuaranteed)
			)
import IdEnv
import Maybes		( Maybe(..), catMaybes )
import Outputable	( isExported )
import Pretty		-- debugging only!
import SpecTyFuns	( mkSpecialisedCon )
import SrcLoc		( SrcLoc, mkUnknownSrcLoc )
import Util
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
type StgEnv = IdEnv PlainStgAtom
\end{code}

No free/live variable information is pinned on in this pass; it's added
later.  For this pass
we use @bOGUS_LVs@ and @bOGUS_FVs@ as placeholders.

\begin{code}
bOGUS_LVs :: PlainStgLiveVars
bOGUS_LVs = panic "bOGUS_LVs" -- emptyUniqSet (used when pprTracing)

bOGUS_FVs :: [Id]
bOGUS_FVs = panic "bOGUS_FVs" -- [] (ditto)
\end{code}

\begin{code}
topCoreBindsToStg :: SplitUniqSupply	-- name supply
		  -> [PlainCoreBinding]	-- input
		  -> [PlainStgBinding]	-- output

topCoreBindsToStg us core_binds
  = case (initSUs us (binds_to_stg nullIdEnv core_binds)) of
      (_, stuff) -> stuff
  where
    binds_to_stg :: StgEnv -> [PlainCoreBinding] -> SUniqSM [PlainStgBinding]

    binds_to_stg env [] = returnSUs []
    binds_to_stg env (b:bs)
      = do_top_bind  env     b  `thenSUs` \ (new_b, new_env, float_binds) ->
    	binds_to_stg new_env bs `thenSUs` \ new_bs ->
    	returnSUs (bagToList float_binds ++ 	-- Literals
		  new_b ++ 
	          new_bs)

    do_top_bind env bind@(CoRec pairs) 
      = coreBindToStg env bind

    do_top_bind env bind@(CoNonRec var rhs)
      = coreBindToStg env bind		`thenSUs` \ (stg_binds, new_env, float_binds) ->
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
		returnSUs (extra_float_binds ++ 
			  [StgNonRec var (StgRhsClosure cc bi fvs u [] rhs_body')],
			  new_env,
			  float_binds)

	   other -> returnSUs (stg_binds, new_env, float_binds)

    --------------------
    -- HACK: look for very simple, obviously-liftable bindings
    -- that can come up to the top level; those that couldn't
    -- 'cause they were big-lambda constrained in the Core world.

    seek_liftable :: [PlainStgBinding] 	-- accumulator...
		  -> PlainStgExpr	-- look for top-lev liftables
		  -> ([PlainStgBinding], PlainStgExpr)	-- result

    seek_liftable acc expr@(StgLet inner_bind body)
      | is_liftable inner_bind
      =	seek_liftable (inner_bind : acc) body

    seek_liftable acc other_expr = (reverse acc, other_expr) -- Finished

    --------------------
    is_liftable (StgNonRec binder (StgRhsClosure _ _ _ _ args body))
      = not (null args) -- it's manifestly a function...
	|| isLeakFreeType [] (getIdUniType binder)
	|| is_whnf body
	-- ToDo: use a decent manifestlyWHNF function for STG?
      where
	is_whnf (StgConApp _ _ _) 	    = True
	is_whnf (StgApp (StgVarAtom v) _ _) = isBottomingId v
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
	      -> PlainCoreBinding
	      -> SUniqSM ([PlainStgBinding],	-- Empty or singleton
		    	 StgEnv,		-- New envt
			 Bag PlainStgBinding)	-- Floats

coreBindToStg env (CoNonRec binder rhs)
  = coreRhsToStg env rhs	`thenSUs` \ (stg_rhs, rhs_binds) ->

    let
	-- Binds to return if RHS is trivial
	triv_binds = if isExported binder then
			[StgNonRec binder stg_rhs]	-- Retain it
		     else
			[]				-- Discard it
    in
    case stg_rhs of
      StgRhsClosure cc bi fvs upd [] (StgApp atom [] lvs) -> 	
		-- Trivial RHS, so augment envt, and ditch the binding
		returnSUs (triv_binds, new_env, rhs_binds)
	   where
		new_env = addOneToIdEnv env binder atom
			  
      StgRhsCon cc con_id [] -> 
		-- Trivial RHS, so augment envt, and ditch the binding
		returnSUs (triv_binds, new_env, rhs_binds)
	   where
		new_env = addOneToIdEnv env binder (StgVarAtom con_id)

      other -> 	-- Non-trivial RHS, so don't augment envt
		returnSUs ([StgNonRec binder stg_rhs], env, rhs_binds)

coreBindToStg env (CoRec pairs)
  = -- NB: *** WE DO NOT CHECK FOR TRIV_BINDS in REC BIND ****
    -- (possibly ToDo)
    let
	(binders, rhss) = unzip pairs
    in
    mapAndUnzipSUs (coreRhsToStg env) rhss `thenSUs` \ (stg_rhss, rhs_binds) ->
    returnSUs ([StgRec (binders `zip` stg_rhss)], env, unionManyBags rhs_binds)
\end{code}


%************************************************************************
%*									*
\subsection[coreToStg-rhss]{Converting right hand sides}
%*									*
%************************************************************************

\begin{code}
coreRhsToStg :: StgEnv -> PlainCoreExpr -> SUniqSM (PlainStgRhs, Bag PlainStgBinding)

coreRhsToStg env core_rhs
  = coreExprToStg env core_rhs 	`thenSUs` \ (stg_expr, stg_binds) ->

    let stg_rhs = case stg_expr of
		    StgLet (StgNonRec var1 rhs) (StgApp (StgVarAtom var2) [] _)
			| var1 == var2 -> rhs
			-- This curious stuff is to unravel what a lambda turns into
			-- We have to do it this way, rather than spot a lambda in the
			-- incoming rhs

		    StgConApp con args _ -> StgRhsCon noCostCentre con args

		    other -> StgRhsClosure noCostCentre	-- No cost centre (ToDo?)
					   stgArgOcc	-- safe
				           bOGUS_FVs
				           Updatable	-- Be pessimistic
				           []
				           stg_expr
    in
    returnSUs (stg_rhs, stg_binds)
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

litToStgAtom :: BasicLit -> SUniqSM (PlainStgAtom, Bag PlainStgBinding)

litToStgAtom (NoRepStr s)
  = newStgVar stringTy 		`thenSUs` \ var ->
    let
	rhs = StgRhsClosure noCostCentre -- No cost centre (ToDo?)
			    stgArgOcc	 -- safe
			    bOGUS_FVs
			    Updatable	 -- OLD: ReEntrant (see note below)
			    []		 -- No arguments
			    val

-- We used not to update strings, so that they wouldn't clog up the heap,
-- but instead be unpacked each time.  But on some programs that costs a lot 
-- [eg hpg], so now we update them.

	val = if (any is_NUL (_UNPK_ s)) then -- must cater for NULs in literal string
		StgApp (StgVarAtom unpackCString2Id) 
		     [StgLitAtom (MachStr s),
		      StgLitAtom (mkMachInt (toInteger (_LENGTH_ s)))]
		     bOGUS_LVs
	      else
		StgApp (StgVarAtom unpackCStringId) 
		     [StgLitAtom (MachStr s)]
		     bOGUS_LVs
    in
    returnSUs (StgVarAtom var, unitBag (StgNonRec var rhs))
  where
    is_NUL c = c == '\0'

litToStgAtom (NoRepInteger i)
  -- extremely convenient to look out for a few very common
  -- Integer literals!
  | i == 0    = returnSUs (StgVarAtom integerZeroId,     emptyBag)
  | i == 1    = returnSUs (StgVarAtom integerPlusOneId,  emptyBag)
  | i == 2    = returnSUs (StgVarAtom integerPlusTwoId,  emptyBag)
  | i == (-1) = returnSUs (StgVarAtom integerMinusOneId, emptyBag)

  | otherwise
  = newStgVar integerTy 		`thenSUs` \ var ->
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
	    StgPrimApp Int2IntegerOp [StgLitAtom (mkMachInt i)] bOGUS_LVs

	  | otherwise
	  = 	-- Start from a string
	    StgPrimApp Addr2IntegerOp [StgLitAtom (MachStr (_PK_ (show i)))] bOGUS_LVs
    in
    returnSUs (StgVarAtom var, unitBag (StgNonRec var rhs))

litToStgAtom (NoRepRational r)
 = litToStgAtom (NoRepInteger (numerator   r))	`thenSUs` \ (num_atom,   binds1) ->
   litToStgAtom (NoRepInteger (denominator r))	`thenSUs` \ (denom_atom, binds2) ->
   newStgVar rationalTy			`thenSUs` \ var ->
   let
	rhs = StgRhsCon noCostCentre	-- No cost centre (ToDo?)
		        ratioDataCon	-- Constructor
			[num_atom, denom_atom]
   in
   returnSUs (StgVarAtom var, binds1 `unionBags` 
			   binds2 `unionBags`
			   unitBag (StgNonRec var rhs))

litToStgAtom other_lit = returnSUs (StgLitAtom other_lit, emptyBag)
\end{code}


%************************************************************************
%*									*
\subsection[coreToStg-atoms{Converting atoms}
%*									*
%************************************************************************

\begin{code}
coreAtomToStg :: StgEnv -> PlainCoreAtom -> SUniqSM (PlainStgAtom, Bag PlainStgBinding)

coreAtomToStg env (CoVarAtom var) = returnSUs (stgLookup env var, emptyBag)
coreAtomToStg env (CoLitAtom lit) = litToStgAtom lit
\end{code}

There's not anything interesting we can ASSERT about \tr{var} if it
isn't in the StgEnv. (WDP 94/06)
\begin{code}
stgLookup :: StgEnv -> Id -> PlainStgAtom

stgLookup env var = case (lookupIdEnv env var) of
		      Nothing   -> StgVarAtom var
		      Just atom -> atom
\end{code}

%************************************************************************
%*									*
\subsection[coreToStg-exprs]{Converting core expressions}
%*									*
%************************************************************************

\begin{code}
coreExprToStg :: StgEnv 
	      -> PlainCoreExpr 
	      -> SUniqSM (PlainStgExpr,		-- Result
			 Bag PlainStgBinding)	-- Float these to top level
\end{code}

\begin{code}
coreExprToStg env (CoLit lit) 
  = litToStgAtom lit	`thenSUs` \ (atom, binds) ->
    returnSUs (StgApp atom [] bOGUS_LVs, binds)

coreExprToStg env (CoVar var)
  = returnSUs (StgApp (stgLookup env var) [] bOGUS_LVs, emptyBag)

coreExprToStg env (CoCon con types args)
  = mapAndUnzipSUs (coreAtomToStg env) args `thenSUs` \ (stg_atoms, stg_binds) ->
    returnSUs (StgConApp spec_con stg_atoms bOGUS_LVs, unionManyBags stg_binds)
  where
    spec_con = mkSpecialisedCon con types

coreExprToStg env (CoPrim op tys args)
  = mapAndUnzipSUs (coreAtomToStg env) args `thenSUs` \ (stg_atoms, stg_binds) ->
    returnSUs (StgPrimApp op stg_atoms bOGUS_LVs, unionManyBags stg_binds)
\end{code}

%************************************************************************
%*									*
\subsubsection[coreToStg-type-stuff]{Type application and abstraction}
%*									*
%************************************************************************

This type information dies in this Core-to-STG translation.

\begin{code}
coreExprToStg env (CoTyLam tyvar expr) = coreExprToStg env expr
coreExprToStg env (CoTyApp expr  ty)   = coreExprToStg env expr
\end{code}

%************************************************************************
%*									*
\subsubsection[coreToStg-lambdas]{Lambda abstractions}
%*									*
%************************************************************************

\begin{code}
coreExprToStg env expr@(CoLam binders body) 
  = coreExprToStg env body		`thenSUs` \ (stg_body, binds) ->
    newStgVar (typeOfCoreExpr expr)	`thenSUs` \ var ->
    returnSUs (StgLet (StgNonRec var (StgRhsClosure noCostCentre
						   stgArgOcc
						   bOGUS_FVs
						   ReEntrant 	-- binders is non-empty
						   binders 
						   stg_body))
		     (StgApp (StgVarAtom var) [] bOGUS_LVs),
	      binds)
\end{code}

%************************************************************************
%*									*
\subsubsection[coreToStg-applications]{Applications}
%*									*
%************************************************************************

\begin{code}
coreExprToStg env expr@(CoApp _ _)
  = 	-- Deal with the arguments
    mapAndUnzipSUs (coreAtomToStg env) args `thenSUs` \ (stg_args, arg_binds) ->

	-- Now deal with the function
    case fun of 
      CoVar fun_id -> returnSUs (StgApp (stgLookup env fun_id) stg_args bOGUS_LVs, 
				unionManyBags arg_binds)

      other ->	-- A non-variable applied to things; better let-bind it.
		newStgVar (typeOfCoreExpr fun)	`thenSUs` \ fun_id ->
		coreExprToStg env fun		`thenSUs` \ (stg_fun, fun_binds) ->
		let
		   fun_rhs = StgRhsClosure noCostCentre	-- No cost centre (ToDo?)
					   stgArgOcc
					   bOGUS_FVs
					   SingleEntry	-- Only entered once
					   []
					   stg_fun
		in
		returnSUs (StgLet (StgNonRec fun_id fun_rhs)
			   	  (StgApp (StgVarAtom fun_id) stg_args bOGUS_LVs),
			   unionManyBags arg_binds `unionBags` 
			   fun_binds)
  where
    (fun,args) = collect_args expr []

	-- Collect arguments, discarding type applications
    collect_args (CoApp fun arg) args = collect_args fun (arg:args)
    collect_args (CoTyApp e t)   args = collect_args e args
    collect_args fun             args = (fun, args)
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

coreExprToStg env (CoCase discrim@(CoPrim op tys args) alts)
  | funnyParallelOp op =
    getSUnique			`thenSUs` \ uniq ->
    coreExprToStg env discrim	`thenSUs` \ (stg_discrim, discrim_binds) ->
    alts_to_stg alts		`thenSUs` \ (stg_alts, alts_binds) ->
    returnSUs (
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

    discrim_ty = typeOfCoreExpr discrim

    alts_to_stg (CoPrimAlts _ (CoBindDefault binder rhs))
      =	coreExprToStg env rhs  `thenSUs` \ (stg_rhs, rhs_binds) ->
        let 
            stg_deflt = StgBindDefault binder False stg_rhs
        in
	    returnSUs (StgPrimAlts discrim_ty [] stg_deflt, rhs_binds)

-- OK, back to real life...

coreExprToStg env (CoCase discrim alts)
  = coreExprToStg env discrim		`thenSUs` \ (stg_discrim, discrim_binds) ->
    alts_to_stg discrim alts	`thenSUs` \ (stg_alts, alts_binds) ->
    getSUnique				`thenSUs` \ uniq ->
    returnSUs (
	StgCase stg_discrim
		bOGUS_LVs
		bOGUS_LVs
		uniq
		stg_alts,
	discrim_binds `unionBags` alts_binds
    )
  where
    discrim_ty		    = typeOfCoreExpr discrim
    (_, discrim_ty_args, _) = getUniDataTyCon discrim_ty

    alts_to_stg discrim (CoAlgAlts alts deflt)
      = default_to_stg discrim deflt		`thenSUs` \ (stg_deflt, deflt_binds) ->
	mapAndUnzipSUs boxed_alt_to_stg alts	`thenSUs` \ (stg_alts, alts_binds)  ->
	returnSUs (StgAlgAlts discrim_ty stg_alts stg_deflt,
		  deflt_binds `unionBags` unionManyBags alts_binds)
      where
	boxed_alt_to_stg (con, bs, rhs)
	  = coreExprToStg env rhs    `thenSUs` \ (stg_rhs, rhs_binds) ->
	    returnSUs ((spec_con, bs, [ True | b <- bs ]{-bogus use mask-}, stg_rhs),
		       rhs_binds)
	  where
	    spec_con = mkSpecialisedCon con discrim_ty_args

    alts_to_stg discrim (CoPrimAlts alts deflt)
      = default_to_stg discrim deflt		`thenSUs` \ (stg_deflt,deflt_binds) ->
	mapAndUnzipSUs unboxed_alt_to_stg alts	`thenSUs` \ (stg_alts, alts_binds)  ->
	returnSUs (StgPrimAlts discrim_ty stg_alts stg_deflt,
		  deflt_binds `unionBags` unionManyBags alts_binds)
      where
	unboxed_alt_to_stg (lit, rhs)
	  = coreExprToStg env rhs    `thenSUs` \ (stg_rhs, rhs_binds) ->
	    returnSUs ((lit, stg_rhs), rhs_binds)

#ifdef DPH
    alts_to_stg (CoParAlgAlts tycon ctxt params alts deflt)
      = default_to_stg deflt	    `thenSUs` \ stg_deflt ->
	mapSUs boxed_alt_to_stg alts `thenSUs` \ stg_alts  ->
	returnSUs (StgParAlgAlts discrim_ty ctxt params stg_alts stg_deflt)
      where
	boxed_alt_to_stg (con, rhs)
	  = coreExprToStg env rhs    `thenSUs` \ stg_rhs ->
	    returnSUs (con, stg_rhs)

    alts_to_stg (CoParPrimAlts tycon ctxt alts deflt)
      = default_to_stg deflt	      `thenSUs` \ stg_deflt ->
	mapSUs unboxed_alt_to_stg alts `thenSUs` \ stg_alts  ->
	returnSUs (StgParPrimAlts discrim_ty ctxt stg_alts stg_deflt)
      where
	unboxed_alt_to_stg (lit, rhs)
	  = coreExprToStg env rhs    `thenSUs` \ stg_rhs ->
	    returnSUs (lit, stg_rhs)
#endif {- Data Parallel Haskell -}

    default_to_stg discrim CoNoDefault
      = returnSUs (StgNoDefault, emptyBag)

    default_to_stg discrim (CoBindDefault binder rhs)
      = coreExprToStg new_env rhs    `thenSUs` \ (stg_rhs, rhs_binds) ->
	returnSUs (StgBindDefault binder True{-used? no it is lying-} stg_rhs,
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
		    CoVar v -> addOneToIdEnv env binder (stgLookup env v)
		    other   -> env
\end{code}

%************************************************************************
%*									*
\subsubsection[coreToStg-let(rec)]{Let and letrec expressions}
%*									*
%************************************************************************

\begin{code}
coreExprToStg env (CoLet bind body)
  = coreBindToStg env     bind   `thenSUs` \ (stg_binds, new_env, float_binds1) ->
    coreExprToStg new_env body   `thenSUs` \ (stg_body, float_binds2) ->
    returnSUs (mkStgLets stg_binds stg_body, float_binds1 `unionBags` float_binds2)
\end{code}


%************************************************************************
%*									*
\subsubsection[coreToStg-scc]{SCC expressions}
%*									*
%************************************************************************

Covert core @scc@ expression directly to STG @scc@ expression.
\begin{code}
coreExprToStg env (CoSCC cc expr)
  = coreExprToStg env expr   `thenSUs` \ (stg_expr, binds) ->
    returnSUs (StgSCC (typeOfCoreExpr expr) cc stg_expr, binds)
\end{code}

%************************************************************************
%*									*
\subsubsection[coreToStg-dataParallel]{Data Parallel expressions}
%*									*
%************************************************************************
\begin{code}
#ifdef DPH
coreExprToStg env (_, AnnCoParCon con ctxt types args)
  = mapAndUnzipSUs (arg2stg env) args	`thenSUs` \ (stg_atoms, stg_binds) ->
    returnSUs (mkStgLets	(catMaybes stg_binds)
	                (StgParConApp con ctxt stg_atoms bOGUS_LVs))

coreExprToStg env (_,AnnCoParComm ctxt expr comm)
  = coreExprToStg env expr		`thenSUs` \ stg_expr             ->
    annComm_to_stg comm			`thenSUs` \ (stg_comm,stg_binds) ->
    returnSUs (mkStgLets (catMaybes stg_binds)
		        (StgParComm ctxt stg_expr stg_comm))
    ))
  where
    annComm_to_stg (AnnCoParSend args)
      = mapAndUnzipSUs (arg2stg env) args `thenSUs` \ (stg_atoms, stg_binds) ->
        returnSUs (StgParSend stg_atoms,stg_binds)

    annComm_to_stg (AnnCoParFetch args)
      = mapAndUnzipSUs (arg2stg env) args `thenSUs` \ (stg_atoms, stg_binds) ->
        returnSUs (StgParFetch stg_atoms,stg_binds)

    annComm_to_stg (AnnCoToPodized)
      = returnSUs (StgToPodized,[])
    annComm_to_stg (AnnCoFromPodized)
      = returnSUs (StgFromPodized,[])
#endif {- Data Parallel Haskell -}
\end{code}

\begin{code}
#ifdef DEBUG
coreExprToStg env other = panic "coreExprToStg: it really failed here"
#endif
\end{code}

%************************************************************************
%*									*
\subsection[coreToStg-misc]{Miscellaneous helping functions}
%*									*
%************************************************************************

Utilities.

Invent a fresh @Id@:
\begin{code}
newStgVar :: UniType -> SUniqSM Id
newStgVar ty
 = getSUnique			`thenSUs` \ uniq ->
   returnSUs (mkSysLocal SLIT("stg") uniq ty mkUnknownSrcLoc)
\end{code}

\begin{code}
mkStgLets ::   [PlainStgBinding]
	    -> PlainStgExpr	-- body of let
	    -> PlainStgExpr

mkStgLets binds body = foldr StgLet body binds
\end{code}
