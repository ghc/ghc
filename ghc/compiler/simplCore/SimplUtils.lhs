%
% (c) The AQUA Project, Glasgow University, 1993-1996
%
\section[SimplUtils]{The simplifier utilities}

\begin{code}
#include "HsVersions.h"

module SimplUtils (

	floatExposesHNF,

	etaCoreExpr, mkRhsTyLam,

	etaExpandCount,

	simplIdWantsToBeINLINEd,

	singleConstructorType, typeOkForCase
    ) where

IMP_Ubiq(){-uitous-}
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ <= 201
IMPORT_DELOOPER(SmplLoop)		-- paranoia checking
#endif

import BinderInfo
import CmdLineOpts	( opt_DoEtaReduction, SimplifierSwitch(..) )
import CoreSyn
import CoreUnfold	( SimpleUnfolding, mkFormSummary, exprIsTrivial, FormSummary(..) )
import Id		( idType, isBottomingId, addInlinePragma, addIdDemandInfo,
			  idWantsToBeINLINEd, dataConArgTys, SYN_IE(Id),
			  getIdArity, GenId{-instance Eq-}
			)
import IdInfo		( ArityInfo(..), DemandInfo )
import Maybes		( maybeToBool )
import PrelVals		( augmentId, buildId )
import PrimOp		( primOpIsCheap )
import SimplEnv
import SimplMonad
import Type		( tyVarsOfType, mkForAllTys, mkTyVarTys, isPrimType, getTyVar_maybe,
			  maybeAppDataTyConExpandingDicts, SYN_IE(Type)
			)
import TyCon		( isDataTyCon )
import TyVar		( elementOfTyVarSet,
			  GenTyVar{-instance Eq-} )
import Util		( isIn, panic, assertPanic )

\end{code}


Floating
~~~~~~~~
The function @floatExposesHNF@ tells whether let/case floating will
expose a head normal form.  It is passed booleans indicating the
desired strategy.

\begin{code}
floatExposesHNF
	:: Bool 		-- Float let(rec)s out of rhs
	-> Bool 		-- Float cheap primops out of rhs
	-> Bool 		-- OK to duplicate code
	-> GenCoreExpr bdr Id tyvar uvar
	-> Bool

floatExposesHNF float_lets float_primops ok_to_dup rhs
  = try rhs
  where
    try (Case (Prim _ _) (PrimAlts alts deflt) )
      | float_primops && (null alts || ok_to_dup)
      = or (try_deflt deflt : map try_alt alts)

    try (Let bind body) | float_lets = try body

    --    `build g'
    -- is like a HNF,
    -- because it *will* become one.
    -- likewise for `augment g h'
    --
    try (App (App (Var bld) _) _)	  | bld == buildId   = True
    try (App (App (App (Var aug) _) _) _) | aug == augmentId = True

    try other = case mkFormSummary other of
			VarForm   -> True
			ValueForm -> True
			other	  -> False
	{- but *not* necessarily "BottomForm"...

	   We may want to float a let out of a let to expose WHNFs,
	    but to do that to expose a "bottom" is a Bad Idea:
	    let x = let y = ...
		    in ...error ...y... --  manifestly bottom using y
	    in ...
	    =/=>
	    let y = ...
	    in let x = ...error ...y...
	       in ...

	    as y is only used in case of an error, we do not want
	    to allocate it eagerly as that's a waste.
	-}

    try_alt (lit,rhs) = try rhs

    try_deflt NoDefault           = False
    try_deflt (BindDefault _ rhs) = try rhs
\end{code}


Local tyvar-lifting
~~~~~~~~~~~~~~~~~~~
mkRhsTyLam tries this transformation, when the big lambda appears as
the RHS of a let(rec) binding:

	/\abc -> let(rec) x = e in b
   ==>
	let(rec) x' = /\abc -> let x = x' a b c in e
	in 
	/\abc -> let x = x' a b c in b

This is good because it can turn things like:

	let f = /\a -> letrec g = ... g ... in g
into
	letrec g' = /\a -> ... g' a ...
	in
	let f = /\ a -> f a

which is better.  In effect, it means that big lambdas don't impede
let-floating.

This optimisation is CRUCIAL in eliminating the junk introduced by
desugaring mutually recursive definitions.  Don't eliminate it lightly!

So far as the implemtation is concerned:

	Invariant: go F e = /\tvs -> F e
	
	Equalities:
		go F (Let x=e in b)
		= Let x' = /\tvs -> F e 
		  in 
		  go G b
		where
		    G = F . Let x = x' tvs
	
		go F (Letrec xi=ei in b)
		= Letrec {xi' = /\tvs -> G ei} 
		  in
		  go G b
		where
		  G = F . Let {xi = xi' tvs}

\begin{code}
mkRhsTyLam [] body = returnSmpl body

mkRhsTyLam tyvars body
  = go (\x -> x) body
  where
    tyvar_tys = mkTyVarTys tyvars

    go fn (Let bind@(NonRec var rhs) body) | exprIsTrivial rhs
      = go (fn . Let bind) body

    go fn (Let bind@(NonRec var rhs) body)
      = mk_poly var				`thenSmpl` \ (var', rhs') ->
	go (fn . Let (mk_silly_bind var rhs')) body	`thenSmpl` \ body' ->
	returnSmpl (Let (NonRec var' (mkTyLam tyvars (fn rhs))) body')

    go fn (Let (Rec prs) body)
       = mapAndUnzipSmpl mk_poly vars		`thenSmpl` \ (vars', rhss') ->
	 let
	    gn body = fn $ foldr Let body (zipWith mk_silly_bind vars rhss')
	 in
	 go gn body				`thenSmpl` \ body' ->
	 returnSmpl (Let (Rec (vars' `zip` [mkTyLam tyvars (gn rhs) | rhs <- rhss])) body')
       where
	 (vars,rhss) = unzip prs

    go fn body = returnSmpl (mkTyLam tyvars (fn body))

    mk_poly var
      = newId (mkForAllTys tyvars (idType var))	`thenSmpl` \ poly_id ->
	returnSmpl (poly_id, mkTyApp (Var poly_id) tyvar_tys)

    mk_silly_bind var rhs = NonRec (addInlinePragma var) rhs
		-- The addInlinePragma is really important!  If we don't say 
		-- INLINE on these silly little bindings then look what happens!
		-- Suppose we start with:
		--
		--	x = let g = /\a -> \x -> f x x
		--	    in 
		--	    /\ b -> let g* = g b in E
		--
		-- Then: 	* the binding for g gets floated out
		-- 		* but then it gets inlined into the rhs of g*
		--		* then the binding for g* is floated out of the /\b
		--		* so we're back to square one
		-- The silly binding for g* must be INLINE, so that no inlining
		-- will happen in its RHS.
\end{code}

Eta reduction
~~~~~~~~~~~~~
@etaCoreExpr@ trys an eta reduction at the top level of a Core Expr.

e.g.	\ x y -> f x y	===>  f

It is used
	a) Before constructing an Unfolding, to 
	   try to make the unfolding smaller;
	b) In tidyCoreExpr, which is done just before converting to STG.

But we only do this if it gets rid of a whole lambda, not part.
The idea is that lambdas are often quite helpful: they indicate
head normal forms, so we don't want to chuck them away lightly.
But if they expose a simple variable then we definitely win.  Even
if they expose a type application we win.  So we check for this special
case.

It does arise:

	f xs = [y | (y,_) <- xs]

gives rise to a recursive function for the list comprehension, and
f turns out to be just a single call to this recursive function.

Doing eta on type lambdas is useful too:

	/\a -> <expr> a	   ===>     <expr>

where <expr> doesn't mention a.
This is sometimes quite useful, because we can get the sequence:

	f ab d = let d1 = ...d... in
		 letrec f' b x = ...d...(f' b)... in
		 f' b
specialise ==>

	f.Int b = letrec f' b x = ...dInt...(f' b)... in
		  f' b

float ==>

	f' b x = ...dInt...(f' b)...
	f.Int b = f' b

Now we really want to simplify to

	f.Int = f'

and then replace all the f's with f.Ints.

N.B. We are careful not to partially eta-reduce a sequence of type
applications since this breaks the specialiser:

	/\ a -> f Char# a  	=NO=> f Char#

\begin{code}
etaCoreExpr :: CoreExpr -> CoreExpr


etaCoreExpr expr@(Lam bndr body)
  | opt_DoEtaReduction
  = case etaCoreExpr body of
	App fun arg | eta_match bndr arg &&
		      residual_ok fun
		    -> fun			-- Eta
	other	    -> expr			-- Can't eliminate it, so do nothing at all
  where
    eta_match (ValBinder v) (VarArg v') = v == v'
    eta_match (TyBinder tv) (TyArg  ty) = case getTyVar_maybe ty of
						Nothing  -> False
						Just tv' -> tv == tv'
    eta_match bndr	    arg 	= False

    residual_ok :: CoreExpr -> Bool	-- Checks for type application
					-- and function not one of the
					-- bound vars

    (VarArg v) `mentions` (ValBinder v') = v == v'
    (TyArg ty) `mentions` (TyBinder tv)  = tv `elementOfTyVarSet` tyVarsOfType ty
    bndr       `mentions` arg 	 	 = False

    residual_ok (Var v)
	= not (VarArg v `mentions` bndr)
    residual_ok (App fun arg)
	| arg `mentions` bndr = False
	| otherwise	      = residual_ok fun
    residual_ok (Coerce coercion ty body)
	| TyArg ty `mentions` bndr = False
	| otherwise		   = residual_ok body

    residual_ok other	     = False		-- Safe answer
	-- This last clause may seem conservative, but consider:
	--	primops, constructors, and literals, are impossible here
	-- 	let and case are unlikely (the argument would have been floated inside)
	--	SCCs we probably want to be conservative about (not sure, but it's safe to be)
	
etaCoreExpr expr = expr		-- The common case
\end{code}
	

Eta expansion
~~~~~~~~~~~~~
@etaExpandCount@ takes an expression, E, and returns an integer n,
such that

	E  ===>   (\x1::t1 x1::t2 ... xn::tn -> E x1 x2 ... xn)

is a safe transformation.  In particular, the transformation should
not cause work to be duplicated, unless it is ``cheap'' (see
@manifestlyCheap@ below).

@etaExpandCount@ errs on the conservative side.  It is always safe to
return 0.

An application of @error@ is special, because it can absorb as many
arguments as you care to give it.  For this special case we return
100, to represent "infinity", which is a bit of a hack.

\begin{code}
etaExpandCount :: GenCoreExpr bdr Id tyvar uvar
	       -> Int	-- Number of extra args you can safely abstract

etaExpandCount (Lam (ValBinder _) body)
  = 1 + etaExpandCount body

etaExpandCount (Let bind body)
  | all manifestlyCheap (rhssOfBind bind)
  = etaExpandCount body

etaExpandCount (Case scrut alts)
  | manifestlyCheap scrut
  = minimum [etaExpandCount rhs | rhs <- rhssOfAlts alts]

etaExpandCount fun@(Var _)     = eta_fun fun
etaExpandCount (App fun arg)
  | notValArg arg = eta_fun fun
  | otherwise     = case etaExpandCount fun of
		      0 -> 0
		      n -> n-1	-- Knock off one

etaExpandCount other = 0    -- Give up
	-- Lit, Con, Prim,
	-- non-val Lam,
	-- Scc (pessimistic; ToDo),
	-- Let with non-whnf rhs(s),
	-- Case with non-whnf scrutinee

-----------------------------
eta_fun :: GenCoreExpr bdr Id tv uv -- The function
	-> Int			    -- How many args it can safely be applied to

eta_fun (App fun arg) | notValArg arg = eta_fun fun

eta_fun expr@(Var v)
  | isBottomingId v		-- Bottoming ids have "infinite arity"
  = 10000			-- Blargh.  Infinite enough!

eta_fun expr@(Var v) = idMinArity v

eta_fun other = 0		-- Give up
\end{code}

@manifestlyCheap@ looks at a Core expression and returns \tr{True} if
it is obviously in weak head normal form, or is cheap to get to WHNF.
By ``cheap'' we mean a computation we're willing to duplicate in order
to bring a couple of lambdas together.  The main examples of things
which aren't WHNF but are ``cheap'' are:

  * 	case e of
	  pi -> ei

	where e, and all the ei are cheap; and

  *	let x = e
	in b

	where e and b are cheap; and

  *	op x1 ... xn

	where op is a cheap primitive operator

\begin{code}
manifestlyCheap :: GenCoreExpr bndr Id tv uv -> Bool

manifestlyCheap (Var _)        = True
manifestlyCheap (Lit _)        = True
manifestlyCheap (Con _ _)      = True
manifestlyCheap (SCC _ e)      = manifestlyCheap e
manifestlyCheap (Coerce _ _ e) = manifestlyCheap e
manifestlyCheap (Lam x e)      = if isValBinder x then True else manifestlyCheap e
manifestlyCheap (Prim op _)    = primOpIsCheap op

manifestlyCheap (Let bind body)
  = manifestlyCheap body && all manifestlyCheap (rhssOfBind bind)

manifestlyCheap (Case scrut alts)
  = manifestlyCheap scrut && all manifestlyCheap (rhssOfAlts alts)

manifestlyCheap other_expr   -- look for manifest partial application
  = case (collectArgs other_expr) of { (fun, _, _, vargs) ->
    case fun of

      Var f | isBottomingId f -> True	-- Application of a function which
					-- always gives bottom; we treat this as
					-- a WHNF, because it certainly doesn't
					-- need to be shared!

      Var f -> let
		    num_val_args = length vargs
	       in
	       num_val_args == 0 ||	-- Just a type application of
					-- a variable (f t1 t2 t3)
					-- counts as WHNF
	       num_val_args < idMinArity f

      _ -> False
    }

\end{code}


\begin{code}
simplIdWantsToBeINLINEd :: Id -> SimplEnv -> Bool

simplIdWantsToBeINLINEd id env
  = {-	We used to arrange that in the final simplification pass we'd switch
	off all INLINE pragmas, so that we'd inline workers back into the
	body of their wrapper if the wrapper hadn't itself been inlined by then.
	This occurred especially for methods in dictionaries.

	We no longer do this:
		a) there's a good chance that the exported wrapper will get
		inlined in some importing scope, in which case we don't 
		want to lose the w/w idea.

		b) The occurrence analyser must agree about what has an
		INLINE pragma.  Not hard, but delicate.
	
		c) if the worker gets inlined we have to tell the wrapepr
		that it's no longer a wrapper, else the interface file stuff
		asks for a worker that no longer exists.
		  
    if switchIsSet env IgnoreINLINEPragma
    then False
    else 
    -}

    idWantsToBeINLINEd id

idMinArity id = case getIdArity id of
			UnknownArity   -> 0
			ArityAtLeast n -> n
			ArityExactly n -> n

singleConstructorType :: Type -> Bool
singleConstructorType ty
  = case (maybeAppDataTyConExpandingDicts ty) of
      Just (tycon, ty_args, [con]) | isDataTyCon tycon -> True
      other			   		       -> False

typeOkForCase :: Type -> Bool
typeOkForCase ty
  = case (maybeAppDataTyConExpandingDicts ty) of
      Just (tycon, ty_args, [])                 		    -> False
      Just (tycon, ty_args, non_null_data_cons) | isDataTyCon tycon -> True
      other	                                   		    -> False
      -- Null data cons => type is abstract, which code gen can't 
      -- currently handle.  (ToDo: when return-in-heap is universal we
      -- don't need to worry about this.)
\end{code}
