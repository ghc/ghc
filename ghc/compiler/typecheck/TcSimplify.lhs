%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[TcSimplify]{TcSimplify}



\begin{code}
module TcSimplify (
	tcSimplifyInfer, tcSimplifyInferCheck, tcSimplifyCheck, 
	tcSimplifyToDicts, tcSimplifyIPs, tcSimplifyTop, 

	tcSimplifyThetas, tcSimplifyCheckThetas,
	bindInstsOfLocalFuns
    ) where

#include "HsVersions.h"

import HsSyn		( MonoBinds(..), HsExpr(..), andMonoBinds, andMonoBindList )
import TcHsSyn		( TcExpr, TcId, 
			  TcMonoBinds, TcDictBinds
			)

import TcMonad
import Inst		( lookupInst, lookupSimpleInst, LookupInstResult(..),
			  tyVarsOfInst, predsOfInsts, predsOfInst,
			  isDict, isClassDict, 
			  isStdClassTyVarDict, isMethodFor,
			  instToId, tyVarsOfInsts,
			  instBindingRequired, instCanBeGeneralised,
			  newDictsFromOld, instMentionsIPs,
			  getDictClassTys, isTyVarDict,
			  instLoc, pprInst, zonkInst, tidyInsts,
			  Inst, LIE, pprInsts, pprInstsInFull,
			  mkLIE, lieToList 
			)
import TcEnv		( tcGetGlobalTyVars, tcGetInstEnv )
import InstEnv		( lookupInstEnv, classInstEnv, InstLookupResult(..) )

import TcType		( zonkTcTyVarsAndFV, tcInstTyVars )
import TcUnify		( unifyTauTy )
import Id		( idType )
import Name		( Name )
import NameSet		( mkNameSet )
import Class		( classBigSig )
import FunDeps		( oclose, grow, improve )
import PrelInfo		( isNumericClass, isCreturnableClass, isCcallishClass )

import Type		( Type, ThetaType, PredType, mkClassPred,
			  mkTyVarTy, getTyVar, isTyVarClassPred,
			  splitSigmaTy, tyVarsOfPred,
			  getClassPredTys_maybe, isClassPred, isIPPred,
			  inheritablePred
			)
import Subst		( mkTopTyVarSubst, substTheta, substTy )
import TysWiredIn	( unitTy )
import VarSet
import FiniteMap
import Outputable
import ListSetOps	( equivClasses )
import Util		( zipEqual )
import List		( partition )
import CmdLineOpts
\end{code}


%************************************************************************
%*									*
\subsection{NOTES}
%*									*
%************************************************************************

	--------------------------------------	
		Notes on quantification
	--------------------------------------	

Suppose we are about to do a generalisation step.
We have in our hand

	G	the environment
	T	the type of the RHS
	C	the constraints from that RHS

The game is to figure out

	Q	the set of type variables over which to quantify
	Ct	the constraints we will *not* quantify over
	Cq	the constraints we will quantify over

So we're going to infer the type

	forall Q. Cq => T

and float the constraints Ct further outwards.  

Here are the things that *must* be true:

 (A)	Q intersect fv(G) = EMPTY			limits how big Q can be
 (B)	Q superset fv(Cq union T) \ oclose(fv(G),C)	limits how small Q can be

(A) says we can't quantify over a variable that's free in the
environment.  (B) says we must quantify over all the truly free
variables in T, else we won't get a sufficiently general type.  We do
not *need* to quantify over any variable that is fixed by the free
vars of the environment G.

	BETWEEN THESE TWO BOUNDS, ANY Q WILL DO!

Example:	class H x y | x->y where ...

	fv(G) = {a}	C = {H a b, H c d}
			T = c -> b

	(A)  Q intersect {a} is empty
	(B)  Q superset {a,b,c,d} \ oclose({a}, C) = {a,b,c,d} \ {a,b} = {c,d}

	So Q can be {c,d}, {b,c,d}

Other things being equal, however, we'd like to quantify over as few
variables as possible: smaller types, fewer type applications, more
constraints can get into Ct instead of Cq.


-----------------------------------------
We will make use of

  fv(T)	 	the free type vars of T

  oclose(vs,C)	The result of extending the set of tyvars vs
		using the functional dependencies from C

  grow(vs,C)	The result of extend the set of tyvars vs
		using all conceivable links from C.  

		E.g. vs = {a}, C = {H [a] b, K (b,Int) c, Eq e}
		Then grow(vs,C) = {a,b,c}

		Note that grow(vs,C) `superset` grow(vs,simplify(C))
		That is, simplfication can only shrink the result of grow.

Notice that 
   oclose is conservative one way:      v `elem` oclose(vs,C) => v is definitely fixed by vs
   grow is conservative the other way:  if v might be fixed by vs => v `elem` grow(vs,C)


-----------------------------------------

Choosing Q
~~~~~~~~~~
Here's a good way to choose Q:

	Q = grow( fv(T), C ) \ oclose( fv(G), C )

That is, quantify over all variable that that MIGHT be fixed by the
call site (which influences T), but which aren't DEFINITELY fixed by
G.  This choice definitely quantifies over enough type variables,
albeit perhaps too many.

Why grow( fv(T), C ) rather than fv(T)?  Consider

	class H x y | x->y where ...
	
	T = c->c
	C = (H c d)

  If we used fv(T) = {c} we'd get the type

	forall c. H c d => c -> b

  And then if the fn was called at several different c's, each of 
  which fixed d differently, we'd get a unification error, because
  d isn't quantified.  Solution: quantify d.  So we must quantify
  everything that might be influenced by c.

Why not oclose( fv(T), C )?  Because we might not be able to see
all the functional dependencies yet:

	class H x y | x->y where ...
	instance H x y => Eq (T x y) where ...

	T = c->c
	C = (Eq (T c d))

  Now oclose(fv(T),C) = {c}, because the functional dependency isn't
  apparent yet, and that's wrong.  We must really quantify over d too.


There really isn't any point in quantifying over any more than
grow( fv(T), C ), because the call sites can't possibly influence
any other type variables.



	--------------------------------------	
		Notes on ambiguity  
	--------------------------------------	

It's very hard to be certain when a type is ambiguous.  Consider

	class K x
	class H x y | x -> y
	instance H x y => K (x,y)

Is this type ambiguous?
	forall a b. (K (a,b), Eq b) => a -> a

Looks like it!  But if we simplify (K (a,b)) we get (H a b) and
now we see that a fixes b.  So we can't tell about ambiguity for sure
without doing a full simplification.  And even that isn't possible if
the context has some free vars that may get unified.  Urgle!

Here's another example: is this ambiguous?
	forall a b. Eq (T b) => a -> a
Not if there's an insance decl (with no context)
	instance Eq (T b) where ...

You may say of this example that we should use the instance decl right
away, but you can't always do that:

	class J a b where ...
	instance J Int b where ...

	f :: forall a b. J a b => a -> a

(Notice: no functional dependency in J's class decl.)
Here f's type is perfectly fine, provided f is only called at Int.
It's premature to complain when meeting f's signature, or even
when inferring a type for f.



However, we don't *need* to report ambiguity right away.  It'll always
show up at the call site.... and eventually at main, which needs special
treatment.  Nevertheless, reporting ambiguity promptly is an excellent thing.

So heres the plan.  We WARN about probable ambiguity if

	fv(Cq) is not a subset of  oclose(fv(T) union fv(G), C)

(all tested before quantification).
That is, all the type variables in Cq must be fixed by the the variables
in the environment, or by the variables in the type.  

Notice that we union before calling oclose.  Here's an example:

	class J a b c | a b -> c
	fv(G) = {a}

Is this ambiguous?
	forall b c. (J a b c) => b -> b

Only if we union {a} from G with {b} from T before using oclose,
do we see that c is fixed.  

It's a bit vague exactly which C we should use for this oclose call.  If we 
don't fix enough variables we might complain when we shouldn't (see
the above nasty example).  Nothing will be perfect.  That's why we can
only issue a warning.


Can we ever be *certain* about ambiguity?  Yes: if there's a constraint

	c in C such that fv(c) intersect (fv(G) union fv(T)) = EMPTY

then c is a "bubble"; there's no way it can ever improve, and it's 
certainly ambiguous.  UNLESS it is a constant (sigh).  And what about
the nasty example?

	class K x
	class H x y | x -> y
	instance H x y => K (x,y)

Is this type ambiguous?
	forall a b. (K (a,b), Eq b) => a -> a

Urk.  The (Eq b) looks "definitely ambiguous" but it isn't.  What we are after
is a "bubble" that's a set of constraints

	Cq = Ca union Cq'  st  fv(Ca) intersect (fv(Cq') union fv(T) union fv(G)) = EMPTY

Hence another idea.  To decide Q start with fv(T) and grow it
by transitive closure in Cq (no functional dependencies involved).
Now partition Cq using Q, leaving the definitely-ambiguous and probably-ok.
The definitely-ambigous can then float out, and get smashed at top level
(which squashes out the constants, like Eq (T a) above)


	--------------------------------------	
		Notes on implicit parameters
	--------------------------------------	

Consider

	f x = ...?y...

Then we get an LIE like (?y::Int).  Doesn't constrain a type variable,
but we must nevertheless infer a type like

	f :: (?y::Int) => Int -> Int

so that f is passed the value of y at the call site.  Is this legal?
	
	f :: Int -> Int
	f x = x + ?y

Should f be overloaded on "?y" ?  Or does the type signature say that it
shouldn't be?  Our position is that it should be illegal.  Otherwise
you can change the *dynamic* semantics by adding a type signature:

	(let f x = x + ?y	-- f :: (?y::Int) => Int -> Int
 	 in (f 3, f 3 with ?y=5))  with ?y = 6

		returns (3+6, 3+5)
vs
	(let f :: Int -> Int 
	    f x = x + ?y
	 in (f 3, f 3 with ?y=5))  with ?y = 6

		returns (3+6, 3+6)

URK!  Let's not do this. So this is illegal:

	f :: Int -> Int
	f x = x + ?y

BOTTOM LINE: you *must* quantify over implicit parameters.


	--------------------------------------	
		Notes on principal types
	--------------------------------------	

    class C a where
      op :: a -> a
    
    f x = let g y = op (y::Int) in True

Here the principal type of f is (forall a. a->a)
but we'll produce the non-principal type
    f :: forall a. C Int => a -> a

	
%************************************************************************
%*									*
\subsection{tcSimplifyInfer}
%*									*
%************************************************************************

tcSimplify is called when we *inferring* a type.  Here's the overall game plan:

    1. Compute Q = grow( fvs(T), C )
    
    2. Partition C based on Q into Ct and Cq.  Notice that ambiguous 
       predicates will end up in Ct; we deal with them at the top level
    
    3. Try improvement, using functional dependencies
    
    4. If Step 3 did any unification, repeat from step 1
       (Unification can change the result of 'grow'.)

Note: we don't reduce dictionaries in step 2.  For example, if we have
Eq (a,b), we don't simplify to (Eq a, Eq b).  So Q won't be different 
after step 2.  However note that we may therefore quantify over more
type variables than we absolutely have to.

For the guts, we need a loop, that alternates context reduction and
improvement with unification.  E.g. Suppose we have

	class C x y | x->y where ...
    
and tcSimplify is called with:
	(C Int a, C Int b)
Then improvement unifies a with b, giving
	(C Int a, C Int a)

If we need to unify anything, we rattle round the whole thing all over
again. 


\begin{code}
tcSimplifyInfer
	:: SDoc 
	-> [TcTyVar]		-- fv(T); type vars 
	-> LIE			-- Wanted
	-> TcM ([TcTyVar],	-- Tyvars to quantify (zonked)
		LIE,		-- Free
		TcDictBinds,	-- Bindings
		[TcId])		-- Dict Ids that must be bound here (zonked)
\end{code}


\begin{code}
tcSimplifyInfer doc tau_tvs wanted_lie
  = inferLoop doc tau_tvs (lieToList wanted_lie)	`thenTc` \ (qtvs, frees, binds, irreds) ->

	-- Check for non-generalisable insts
    mapTc_ addCantGenErr (filter (not . instCanBeGeneralised) irreds)	`thenTc_`

    returnTc (qtvs, mkLIE frees, binds, map instToId irreds)

inferLoop doc tau_tvs wanteds
  =   	-- Step 1
    zonkTcTyVarsAndFV tau_tvs		`thenNF_Tc` \ tau_tvs' ->
    mapNF_Tc zonkInst wanteds		`thenNF_Tc` \ wanteds' ->
    tcGetGlobalTyVars			`thenNF_Tc` \ gbl_tvs ->
    let
 	preds = predsOfInsts wanteds'
	qtvs  = grow preds tau_tvs' `minusVarSet` oclose preds gbl_tvs
	
	try_me inst 	
	  | isFree qtvs inst  = Free
	  | isClassDict inst  = DontReduceUnlessConstant	-- Dicts
	  | otherwise	      = ReduceMe 			-- Lits and Methods
    in
		-- Step 2
    reduceContext doc try_me [] wanteds'    `thenTc` \ (no_improvement, frees, binds, irreds) ->
	
		-- Step 3
    if no_improvement then
	returnTc (varSetElems qtvs, frees, binds, irreds)
    else
	-- If improvement did some unification, we go round again.  There
	-- are two subtleties:
	--   a) We start again with irreds, not wanteds
	-- 	Using an instance decl might have introduced a fresh type variable
	--	which might have been unified, so we'd get an infinite loop
	--	if we started again with wanteds!  See example [LOOP]
	--
	--   b) It's also essential to re-process frees, because unification
	--      might mean that a type variable that looked free isn't now.
	--
	-- Hence the (irreds ++ frees)

	inferLoop doc tau_tvs (irreds ++ frees)	`thenTc` \ (qtvs1, frees1, binds1, irreds1) ->
	returnTc (qtvs1, frees1, binds `AndMonoBinds` binds1, irreds1)
\end{code}	

Example [LOOP]

	class If b t e r | b t e -> r
	instance If T t e t
	instance If F t e e
	class Lte a b c | a b -> c where lte :: a -> b -> c
	instance Lte Z b T
	instance (Lte a b l,If l b a c) => Max a b c

Wanted:	Max Z (S x) y

Then we'll reduce using the Max instance to:
	(Lte Z (S x) l, If l (S x) Z y)
and improve by binding l->T, after which we can do some reduction 
on both the Lte and If constraints.  What we *can't* do is start again
with (Max Z (S x) y)!

\begin{code}
isFree qtvs inst
  =  not (tyVarsOfInst inst `intersectsVarSet` qtvs)	-- Constrains no quantified vars
  && all inheritablePred (predsOfInst inst)		-- And no implicit parameter involved
							-- (see "Notes on implicit parameters")
\end{code}


%************************************************************************
%*									*
\subsection{tcSimplifyCheck}
%*									*
%************************************************************************

@tcSimplifyCheck@ is used when we know exactly the set of variables
we are going to quantify over.  For example, a class or instance declaration.

\begin{code}
tcSimplifyCheck
	 :: SDoc 
	 -> [TcTyVar]		-- Quantify over these
	 -> [Inst]		-- Given
	 -> LIE			-- Wanted
	 -> TcM (LIE,		-- Free
		 TcDictBinds)	-- Bindings

tcSimplifyCheck doc qtvs givens wanted_lie
  = checkLoop doc qtvs givens (lieToList wanted_lie)	`thenTc` \ (frees, binds, irreds) ->

	-- Complain about any irreducible ones
    complainCheck doc givens irreds		`thenNF_Tc_`

	-- Done
    returnTc (mkLIE frees, binds)

checkLoop doc qtvs givens wanteds
  =   	-- Step 1
    zonkTcTyVarsAndFV qtvs		`thenNF_Tc` \ qtvs' ->
    mapNF_Tc zonkInst givens		`thenNF_Tc` \ givens' ->
    mapNF_Tc zonkInst wanteds		`thenNF_Tc` \ wanteds' ->
    let
	      -- When checking against a given signature we always reduce
	      -- until we find a match against something given, or can't reduce
	try_me inst | isFree qtvs' inst  = Free
		    | otherwise          = ReduceMe 
    in
		-- Step 2
    reduceContext doc try_me givens' wanteds'    `thenTc` \ (no_improvement, frees, binds, irreds) ->
	
		-- Step 3
    if no_improvement then
	returnTc (frees, binds, irreds)
    else
	checkLoop doc qtvs givens' (irreds ++ frees)	`thenTc` \ (frees1, binds1, irreds1) ->
	returnTc (frees1, binds `AndMonoBinds` binds1, irreds1)

complainCheck doc givens irreds
  = mapNF_Tc zonkInst given_dicts			`thenNF_Tc` \ givens' ->
    mapNF_Tc (addNoInstanceErr doc given_dicts) irreds	`thenNF_Tc_`
    returnTc ()
  where
    given_dicts = filter isDict givens
	-- Filter out methods, which are only added to 
	-- the given set as an optimisation
\end{code}



%************************************************************************
%*									*
\subsection{tcSimplifyAndCheck}
%*									*
%************************************************************************

@tcSimplifyInferCheck@ is used when we know the consraints we are to simplify
against, but we don't know the type variables over which we are going to quantify.
This happens when we have a type signature for a mutually recursive
group.

\begin{code}
tcSimplifyInferCheck
	 :: SDoc 
	 -> [TcTyVar]		-- fv(T)
	 -> [Inst]		-- Given
	 -> LIE			-- Wanted
	 -> TcM ([TcTyVar],	-- Variables over which to quantify
		 LIE,		-- Free
		 TcDictBinds)	-- Bindings

tcSimplifyInferCheck doc tau_tvs givens wanted
  = inferCheckLoop doc tau_tvs givens (lieToList wanted)	`thenTc` \ (qtvs, frees, binds, irreds) ->

	-- Complain about any irreducible ones
    complainCheck doc givens irreds		`thenNF_Tc_`

	-- Done
    returnTc (qtvs, mkLIE frees, binds)

inferCheckLoop doc tau_tvs givens wanteds
  =   	-- Step 1
    zonkTcTyVarsAndFV tau_tvs		`thenNF_Tc` \ tau_tvs' ->
    mapNF_Tc zonkInst givens		`thenNF_Tc` \ givens' ->
    mapNF_Tc zonkInst wanteds		`thenNF_Tc` \ wanteds' ->
    tcGetGlobalTyVars			`thenNF_Tc` \ gbl_tvs ->

    let
  	-- Figure out what we are going to generalise over
	-- You might think it should just be the signature tyvars,
	-- but in bizarre cases you can get extra ones
	-- 	f :: forall a. Num a => a -> a
	--	f x = fst (g (x, head [])) + 1
	--	g a b = (b,a)
	-- Here we infer g :: forall a b. a -> b -> (b,a)
	-- We don't want g to be monomorphic in b just because
	-- f isn't quantified over b.
	qtvs    = (tau_tvs' `unionVarSet` tyVarsOfInsts givens') `minusVarSet` gbl_tvs
			-- We could close gbl_tvs, but its not necessary for
			-- soundness, and it'll only affect which tyvars, not which 
			-- dictionaries, we quantify over

	      -- When checking against a given signature we always reduce
	      -- until we find a match against something given, or can't reduce
	try_me inst | isFree qtvs inst  = Free
		    | otherwise         = ReduceMe 
    in
		-- Step 2
    reduceContext doc try_me givens' wanteds'    `thenTc` \ (no_improvement, frees, binds, irreds) ->
	
		-- Step 3
    if no_improvement then
	returnTc (varSetElems qtvs, frees, binds, irreds)
    else
	inferCheckLoop doc tau_tvs givens' (irreds ++ frees)	`thenTc` \ (qtvs1, frees1, binds1, irreds1) ->
	returnTc (qtvs1, frees1, binds `AndMonoBinds` binds1, irreds1)
\end{code}


%************************************************************************
%*									*
\subsection{tcSimplifyToDicts}
%*									*
%************************************************************************

On the LHS of transformation rules we only simplify methods and constants,
getting dictionaries.  We want to keep all of them unsimplified, to serve
as the available stuff for the RHS of the rule.

The same thing is used for specialise pragmas. Consider
	
	f :: Num a => a -> a
	{-# SPECIALISE f :: Int -> Int #-}
	f = ...

The type checker generates a binding like:

	f_spec = (f :: Int -> Int)

and we want to end up with

	f_spec = _inline_me_ (f Int dNumInt)

But that means that we must simplify the Method for f to (f Int dNumInt)! 
So tcSimplifyToDicts squeezes out all Methods.

IMPORTANT NOTE:  we *don't* want to do superclass commoning up.  Consider

	fromIntegral :: (Integral a, Num b) => a -> b
	{-# RULES "foo"  fromIntegral = id :: Int -> Int #-}

Here, a=b=Int, and Num Int is a superclass of Integral Int. But we *dont* 
want to get

	forall dIntegralInt.
	fromIntegral Int Int dIntegralInt (scsel dIntegralInt) = id Int

because the scsel will mess up matching.  Instead we want

	forall dIntegralInt, dNumInt.
	fromIntegral Int Int dIntegralInt dNumInt = id Int

Hence "DontReduce NoSCs"

\begin{code}
tcSimplifyToDicts :: LIE -> TcM ([Inst], TcDictBinds)
tcSimplifyToDicts wanted_lie
  = simpleReduceLoop doc try_me wanteds		`thenTc` \ (frees, binds, irreds) ->
	-- Since try_me doesn't look at types, we don't need to 
	-- do any zonking, so it's safe to call reduceContext directly
    ASSERT( null frees )
    returnTc (irreds, binds)

  where
    doc = text "tcSimplifyToDicts"
    wanteds = lieToList wanted_lie

	-- Reduce methods and lits only; stop as soon as we get a dictionary
    try_me inst	| isDict inst = DontReduce NoSCs
		| otherwise   = ReduceMe
\end{code}


%************************************************************************
%*									*
\subsection{Filtering at a dynamic binding}
%*									*
%************************************************************************

When we have
	let ?x = R in B

we must discharge all the ?x constraints from B.  We also do an improvement
step; if we have ?x::t1 and ?x::t2 we must unify t1, t2.  No need to iterate, though.

\begin{code}
tcSimplifyIPs :: [Name]		-- The implicit parameters bound here
	      -> LIE
	      -> TcM (LIE, TcDictBinds)
tcSimplifyIPs ip_names wanted_lie
  = simpleReduceLoop doc try_me wanteds	`thenTc` \ (frees, binds, irreds) ->
	-- The irreducible ones should be a subset of the implicit
	-- parameters we provided
    ASSERT( all here_ip irreds )
    returnTc (mkLIE frees, binds)
    
  where
    doc	    = text "tcSimplifyIPs" <+> ppr ip_names
    wanteds = lieToList wanted_lie
    ip_set  = mkNameSet ip_names
    here_ip ip = isDict ip && ip `instMentionsIPs` ip_set

	-- Simplify any methods that mention the implicit parameter
    try_me inst | inst `instMentionsIPs` ip_set = ReduceMe
		| otherwise		        = Free
\end{code}


%************************************************************************
%*									*
\subsection[binds-for-local-funs]{@bindInstsOfLocalFuns@}
%*									*
%************************************************************************

When doing a binding group, we may have @Insts@ of local functions.
For example, we might have...
\begin{verbatim}
let f x = x + 1	    -- orig local function (overloaded)
    f.1 = f Int	    -- two instances of f
    f.2 = f Float
 in
    (f.1 5, f.2 6.7)
\end{verbatim}
The point is: we must drop the bindings for @f.1@ and @f.2@ here,
where @f@ is in scope; those @Insts@ must certainly not be passed
upwards towards the top-level.	If the @Insts@ were binding-ified up
there, they would have unresolvable references to @f@.

We pass in an @init_lie@ of @Insts@ and a list of locally-bound @Ids@.
For each method @Inst@ in the @init_lie@ that mentions one of the
@Ids@, we create a binding.  We return the remaining @Insts@ (in an
@LIE@), as well as the @HsBinds@ generated.

\begin{code}
bindInstsOfLocalFuns ::	LIE -> [TcId] -> TcM (LIE, TcMonoBinds)

bindInstsOfLocalFuns init_lie local_ids
  | null overloaded_ids 
	-- Common case
  = returnTc (init_lie, EmptyMonoBinds)

  | otherwise
  = simpleReduceLoop doc try_me wanteds		`thenTc` \ (frees, binds, irreds) -> 
    ASSERT( null irreds )
    returnTc (mkLIE frees, binds)
  where
    doc		     = text "bindInsts" <+> ppr local_ids
    wanteds	     = lieToList init_lie
    overloaded_ids   = filter is_overloaded local_ids
    is_overloaded id = case splitSigmaTy (idType id) of
			  (_, theta, _) -> not (null theta)

    overloaded_set = mkVarSet overloaded_ids	-- There can occasionally be a lot of them
						-- so it's worth building a set, so that 
						-- lookup (in isMethodFor) is faster

    try_me inst | isMethodFor overloaded_set inst = ReduceMe
		| otherwise		          = Free
\end{code}


%************************************************************************
%*									*
\subsection{Data types for the reduction mechanism}
%*									*
%************************************************************************

The main control over context reduction is here

\begin{code}
data WhatToDo 
 = ReduceMe		-- Try to reduce this
			-- If there's no instance, behave exactly like
			-- DontReduce: add the inst to 
			-- the irreductible ones, but don't 
			-- produce an error message of any kind.
			-- It might be quite legitimate such as (Eq a)!

 | DontReduce WantSCs		-- Return as irreducible 

 | DontReduceUnlessConstant	-- Return as irreducible unless it can
				-- be reduced to a constant in one step

 | Free			  -- Return as free

data WantSCs = NoSCs | AddSCs	-- Tells whether we should add the superclasses
				-- of a predicate when adding it to the avails
\end{code}



\begin{code}
type RedState = (Avails,	-- What's available
		 [Inst])	-- Insts for which try_me returned Free

type Avails = FiniteMap Inst Avail

data Avail
  = Irred		-- Used for irreducible dictionaries,
			-- which are going to be lambda bound

  | BoundTo TcId	-- Used for dictionaries for which we have a binding
			-- e.g. those "given" in a signature

  | NoRhs 		-- Used for Insts like (CCallable f)
			-- where no witness is required.

  | Rhs 		-- Used when there is a RHS 
	TcExpr	 	-- The RHS
	[Inst]		-- Insts free in the RHS; we need these too

pprAvails avails = vcat [ppr inst <+> equals <+> pprAvail avail
			| (inst,avail) <- fmToList avails ]

instance Outputable Avail where
    ppr = pprAvail

pprAvail NoRhs	      = text "<no rhs>"
pprAvail Irred	      = text "Irred"
pprAvail (BoundTo x)  = text "Bound to" <+> ppr x
pprAvail (Rhs rhs bs) = ppr rhs <+> braces (ppr bs)
\end{code}

Extracting the bindings from a bunch of Avails.
The bindings do *not* come back sorted in dependency order.
We assume that they'll be wrapped in a big Rec, so that the
dependency analyser can sort them out later

The loop startes
\begin{code}
bindsAndIrreds :: Avails
	       -> [Inst]		-- Wanted
	       -> (TcDictBinds, 	-- Bindings
		   [Inst])		-- Irreducible ones

bindsAndIrreds avails wanteds
  = go avails EmptyMonoBinds [] wanteds
  where
    go avails binds irreds [] = (binds, irreds)

    go avails binds irreds (w:ws)
      = case lookupFM avails w of
	  Nothing    -> -- Free guys come out here
			-- (If we didn't do addFree we could use this as the
			--  criterion for free-ness, and pick up the free ones here too)
			go avails binds irreds ws

	  Just NoRhs -> go avails binds irreds ws

	  Just Irred -> go (addToFM avails w (BoundTo (instToId w))) binds (w:irreds) ws

	  Just (BoundTo id) -> go avails new_binds irreds ws
			    where
				-- For implicit parameters, all occurrences share the same
				-- Id, so there is no need for synonym bindings
			       new_binds | new_id == id = binds
					 | otherwise	= addBind binds new_id (HsVar id)
			       new_id   = instToId w

	  Just (Rhs rhs ws') -> go avails' (addBind binds id rhs) irreds (ws' ++ ws)
			     where
				id	 = instToId w
				avails'  = addToFM avails w (BoundTo id)

addBind binds id rhs = binds `AndMonoBinds` VarMonoBind id rhs
\end{code}


%************************************************************************
%*									*
\subsection[reduce]{@reduce@}
%*									*
%************************************************************************

When the "what to do" predicate doesn't depend on the quantified type variables,
matters are easier.  We don't need to do any zonking, unless the improvement step
does something, in which case we zonk before iterating.

The "given" set is always empty.

\begin{code}
simpleReduceLoop :: SDoc
	 	 -> (Inst -> WhatToDo)		-- What to do, *not* based on the quantified type variables
		 -> [Inst]			-- Wanted
		 -> TcM ([Inst],		-- Free
			 TcDictBinds,
			 [Inst])		-- Irreducible

simpleReduceLoop doc try_me wanteds
  = mapNF_Tc zonkInst wanteds			`thenNF_Tc` \ wanteds' ->
    reduceContext doc try_me [] wanteds'	`thenTc` \ (no_improvement, frees, binds, irreds) ->
    if no_improvement then
	returnTc (frees, binds, irreds)
    else
	simpleReduceLoop doc try_me (irreds ++ frees)	`thenTc` \ (frees1, binds1, irreds1) ->
	returnTc (frees1, binds `AndMonoBinds` binds1, irreds1)
\end{code}	



\begin{code}
reduceContext :: SDoc
	      -> (Inst -> WhatToDo)
	      -> [Inst]			-- Given
	      -> [Inst]			-- Wanted
	      -> NF_TcM (Bool, 		-- True <=> improve step did no unification
			 [Inst],	-- Free
			 TcDictBinds,	-- Dictionary bindings
			 [Inst])	-- Irreducible

reduceContext doc try_me givens wanteds
  =
    traceTc (text "reduceContext" <+> (vcat [
	     text "----------------------",
	     doc,
	     text "given" <+> ppr givens,
	     text "wanted" <+> ppr wanteds,
	     text "----------------------"
	     ]))					`thenNF_Tc_`

        -- Build the Avail mapping from "givens"
    foldlNF_Tc addGiven (emptyFM, []) givens		`thenNF_Tc` \ init_state ->

        -- Do the real work
    reduceList (0,[]) try_me wanteds init_state		`thenNF_Tc` \ state@(avails, frees) ->

	-- Do improvement, using everything in avails
	-- In particular, avails includes all superclasses of everything
    tcImprove avails					`thenTc` \ no_improvement ->

    traceTc (text "reduceContext end" <+> (vcat [
	     text "----------------------",
	     doc,
	     text "given" <+> ppr givens,
	     text "wanted" <+> ppr wanteds,
	     text "----", 
	     text "avails" <+> pprAvails avails,
	     text "frees" <+> ppr frees,
	     text "no_improvement =" <+> ppr no_improvement,
	     text "----------------------"
	     ])) 					`thenNF_Tc_`
     let
	(binds, irreds) = bindsAndIrreds avails wanteds
     in
     returnTc (no_improvement, frees, binds, irreds)

tcImprove avails
 =  tcGetInstEnv 				`thenTc` \ inst_env ->
    let
	preds = predsOfInsts (keysFM avails)
		-- Avails has all the superclasses etc (good)
		-- It also has all the intermediates of the deduction (good)
		-- It does not have duplicates (good)
		-- NB that (?x::t1) and (?x::t2) will be held separately in avails
		--    so that improve will see them separate
	eqns  = improve (classInstEnv inst_env) preds
     in
     if null eqns then
	returnTc True
     else
	traceTc (ptext SLIT("Improve:") <+> vcat (map ppr_eqn eqns))	`thenNF_Tc_`
        mapTc_ unify eqns	`thenTc_`
	returnTc False
  where
    unify (qtvs, t1, t2) = tcInstTyVars (varSetElems qtvs)	`thenNF_Tc` \ (_, _, tenv) ->
			   unifyTauTy (substTy tenv t1) (substTy tenv t2)
    ppr_eqn (qtvs, t1, t2) = ptext SLIT("forall") <+> braces (pprWithCommas ppr (varSetElems qtvs)) <+>
			     ppr t1 <+> equals <+> ppr t2
\end{code}

The main context-reduction function is @reduce@.  Here's its game plan.

\begin{code}
reduceList :: (Int,[Inst])		-- Stack (for err msgs)
					-- along with its depth
       	   -> (Inst -> WhatToDo)
       	   -> [Inst]
       	   -> RedState
       	   -> TcM RedState
\end{code}

@reduce@ is passed
     try_me:	given an inst, this function returns
		  Reduce       reduce this
		  DontReduce   return this in "irreds"
		  Free	       return this in "frees"

     wanteds:	The list of insts to reduce
     state:	An accumulating parameter of type RedState 
		that contains the state of the algorithm
 
  It returns a RedState.

The (n,stack) pair is just used for error reporting.  
n is always the depth of the stack.
The stack is the stack of Insts being reduced: to produce X
I had to produce Y, to produce Y I had to produce Z, and so on.

\begin{code}
reduceList (n,stack) try_me wanteds state
  | n > opt_MaxContextReductionDepth
  = failWithTc (reduceDepthErr n stack)

  | otherwise
  =
#ifdef DEBUG
   (if n > 8 then
	pprTrace "Jeepers! ReduceContext:" (reduceDepthMsg n stack)
    else (\x->x))
#endif
    go wanteds state
  where
    go []     state = returnTc state
    go (w:ws) state = reduce (n+1, w:stack) try_me w state	`thenTc` \ state' ->
		      go ws state'

    -- Base case: we're done!
reduce stack try_me wanted state
    -- It's the same as an existing inst, or a superclass thereof
  | isAvailable state wanted
  = returnTc state

  | otherwise
  = case try_me wanted of {

      DontReduce want_scs -> addIrred want_scs state wanted

    ; DontReduceUnlessConstant ->    -- It's irreducible (or at least should not be reduced)
  				     -- First, see if the inst can be reduced to a constant in one step
	try_simple (addIrred AddSCs)	-- Assume want superclasses

    ; Free ->	-- It's free so just chuck it upstairs
  		-- First, see if the inst can be reduced to a constant in one step
	try_simple addFree

    ; ReduceMe ->		-- It should be reduced
	lookupInst wanted	      `thenNF_Tc` \ lookup_result ->
	case lookup_result of
	    GenInst wanteds' rhs -> reduceList stack try_me wanteds' state	`thenTc` \ state' -> 
				    addWanted state' wanted rhs wanteds'
	    SimpleInst rhs       -> addWanted state wanted rhs []

	    NoInstance ->    -- No such instance! 
			     -- Add it and its superclasses
		    	     addIrred AddSCs state wanted

    }
  where
    try_simple do_this_otherwise
      = lookupInst wanted	  `thenNF_Tc` \ lookup_result ->
	case lookup_result of
	    SimpleInst rhs -> addWanted state wanted rhs []
	    other	   -> do_this_otherwise state wanted
\end{code}


\begin{code}
isAvailable :: RedState -> Inst -> Bool
isAvailable (avails, _) wanted = wanted `elemFM` avails
	-- NB: the Ord instance of Inst compares by the class/type info
	-- *not* by unique.  So 
	--	d1::C Int ==  d2::C Int

-------------------------
addFree :: RedState -> Inst -> NF_TcM RedState
	-- When an Inst is tossed upstairs as 'free' we nevertheless add it
	-- to avails, so that any other equal Insts will be commoned up right
	-- here rather than also being tossed upstairs.  This is really just
	-- an optimisation, and perhaps it is more trouble that it is worth,
	-- as the following comments show!
	--
	-- NB1: do *not* add superclasses.  If we have
	--	df::Floating a
	--	dn::Num a
	-- but a is not bound here, then we *don't* want to derive 
	-- dn from df here lest we lose sharing.
	--
	-- NB2: do *not* add the Inst to avails at all if it's a method.
	-- The following situation shows why this is bad:
	--	truncate :: forall a. RealFrac a => forall b. Integral b => a -> b
	-- From an application (truncate f i) we get
	--	t1 = truncate at f 
	--	t2 = t1 at i
	-- If we have also have a second occurrence of truncate, we get
	--	t3 = truncate at f
	--	t4 = t3 at i
	-- When simplifying with i,f free, we might still notice that
	--   t1=t3; but alas, the binding for t2 (which mentions t1)
	--   will continue to float out!
	-- Solution: never put methods in avail till they are captured
	-- in which case addFree isn't used
	--
	-- NB3: make sure that CCallable/CReturnable use NoRhs rather
	--	than BoundTo, else we end up with bogus bindings.
	--	c.f. instBindingRequired in addWanted
addFree (avails, frees) free
  | isDict free = returnNF_Tc (addToFM avails free avail, free:frees)
  | otherwise   = returnNF_Tc (avails,			  free:frees)
  where
    avail | instBindingRequired free = BoundTo (instToId free)
	  | otherwise		     = NoRhs

addWanted :: RedState -> Inst -> TcExpr -> [Inst] -> NF_TcM RedState
addWanted state@(avails, frees) wanted rhs_expr wanteds
-- Do *not* add superclasses as well.  Here's an example of why not
-- 	class Eq a => Foo a b 
--	instance Eq a => Foo [a] a
-- If we are reducing
--	(Foo [t] t)
-- we'll first deduce that it holds (via the instance decl).  We  
-- must not then overwrite the Eq t constraint with a superclass selection!
-- 	ToDo: this isn't entirely unsatisfactory, because
--	      we may also lose some entirely-legitimate sharing this way

  = ASSERT( not (isAvailable state wanted) )
    returnNF_Tc (addToFM avails wanted avail, frees)
  where 
    avail | instBindingRequired wanted = Rhs rhs_expr wanteds
	  | otherwise		       = ASSERT( null wanteds ) NoRhs

addGiven :: RedState -> Inst -> NF_TcM RedState
addGiven state given = addAvailAndSCs state given (BoundTo (instToId given))

addIrred :: WantSCs -> RedState -> Inst -> NF_TcM RedState
addIrred NoSCs  (avails,frees) irred = returnNF_Tc (addToFM avails irred Irred, frees)
addIrred AddSCs state	       irred = addAvailAndSCs state irred Irred

addAvailAndSCs :: RedState -> Inst -> Avail -> NF_TcM RedState
addAvailAndSCs (avails, frees) wanted avail
  = add_avail_and_scs avails wanted avail	`thenNF_Tc` \ avails' ->
    returnNF_Tc (avails', frees)

---------------------
add_avail_and_scs :: Avails -> Inst -> Avail -> NF_TcM Avails
add_avail_and_scs avails wanted avail
  = add_scs (addToFM avails wanted avail) wanted

add_scs :: Avails -> Inst -> NF_TcM Avails
	-- Add all the superclasses of the Inst to Avails
	-- Invariant: the Inst is already in Avails.

add_scs avails dict
  | not (isClassDict dict)
  = returnNF_Tc avails

  | otherwise	-- It is a dictionary
  = newDictsFromOld dict sc_theta'	`thenNF_Tc` \ sc_dicts ->
    foldlNF_Tc add_sc avails (zipEqual "add_scs" sc_dicts sc_sels)
  where
    (clas, tys) = getDictClassTys dict
    (tyvars, sc_theta, sc_sels, _) = classBigSig clas
    sc_theta' = substTheta (mkTopTyVarSubst tyvars tys) sc_theta

    add_sc avails (sc_dict, sc_sel)	-- Add it, and its superclasses
      = case lookupFM avails sc_dict of
	  Just (BoundTo _) -> returnNF_Tc avails	-- See Note [SUPER] below
	  other		   -> add_avail_and_scs avails sc_dict avail
      where
	sc_sel_rhs = DictApp (TyApp (HsVar sc_sel) tys) [instToId dict]
	avail      = Rhs sc_sel_rhs [dict]
\end{code}

Note [SUPER].  We have to be careful here.  If we are *given* d1:Ord a,
and want to deduce (d2:C [a]) where

	class Ord a => C a where
	instance Ord a => C [a] where ...

Then we'll use the instance decl to deduce C [a] and then add the 
superclasses of C [a] to avails.  But we must not overwrite the binding
for d1:Ord a (which is given) with a superclass selection or we'll just
build a loop!  Hence looking for BoundTo.  Crudely, BoundTo is cheaper
than a selection.


%************************************************************************
%*									*
\section{tcSimplifyTop: defaulting}
%*									*
%************************************************************************


If a dictionary constrains a type variable which is
	* not mentioned in the environment
	* and not mentioned in the type of the expression
then it is ambiguous. No further information will arise to instantiate
the type variable; nor will it be generalised and turned into an extra
parameter to a function.

It is an error for this to occur, except that Haskell provided for
certain rules to be applied in the special case of numeric types.
Specifically, if
	* at least one of its classes is a numeric class, and
	* all of its classes are numeric or standard
then the type variable can be defaulted to the first type in the
default-type list which is an instance of all the offending classes.

So here is the function which does the work.  It takes the ambiguous
dictionaries and either resolves them (producing bindings) or
complains.  It works by splitting the dictionary list by type
variable, and using @disambigOne@ to do the real business.

@tcSimplifyTop@ is called once per module to simplify all the constant
and ambiguous Insts.

We need to be careful of one case.  Suppose we have

	instance Num a => Num (Foo a b) where ...

and @tcSimplifyTop@ is given a constraint (Num (Foo x y)).  Then it'll simplify
to (Num x), and default x to Int.  But what about y??  

It's OK: the final zonking stage should zap y to (), which is fine.


\begin{code}
tcSimplifyTop :: LIE -> TcM TcDictBinds
tcSimplifyTop wanted_lie
  = simpleReduceLoop (text "tcSimplTop") try_me wanteds	`thenTc` \ (frees, binds, irreds) ->
    ASSERT( null frees )

    let
		-- All the non-std ones are definite errors
	(stds, non_stds) = partition isStdClassTyVarDict irreds
	
		-- Group by type variable
	std_groups = equivClasses cmp_by_tyvar stds

		-- Pick the ones which its worth trying to disambiguate
	(std_oks, std_bads) = partition worth_a_try std_groups

		-- Have a try at disambiguation 
		-- if the type variable isn't bound
		-- up with one of the non-standard classes
	worth_a_try group@(d:_) = not (non_std_tyvars `intersectsVarSet` tyVarsOfInst d)
	non_std_tyvars		= unionVarSets (map tyVarsOfInst non_stds)

		-- Collect together all the bad guys
	bad_guys = non_stds ++ concat std_bads
    in
	-- Disambiguate the ones that look feasible
    mapTc disambigGroup std_oks		`thenTc` \ binds_ambig ->

	-- And complain about the ones that don't
	-- This group includes both non-existent instances 
	--	e.g. Num (IO a) and Eq (Int -> Int)
	-- and ambiguous dictionaries
	--	e.g. Num a
    addTopAmbigErrs bad_guys		`thenNF_Tc_`

    returnTc (binds `andMonoBinds` andMonoBindList binds_ambig)
  where
    wanteds	= lieToList wanted_lie
    try_me inst	= ReduceMe

    d1 `cmp_by_tyvar` d2 = get_tv d1 `compare` get_tv d2

get_tv d   = case getDictClassTys d of
		   (clas, [ty]) -> getTyVar "tcSimplifyTop" ty
get_clas d = case getDictClassTys d of
		   (clas, [ty]) -> clas
\end{code}

@disambigOne@ assumes that its arguments dictionaries constrain all
the same type variable.

ADR Comment 20/6/94: I've changed the @CReturnable@ case to default to
@()@ instead of @Int@.  I reckon this is the Right Thing to do since
the most common use of defaulting is code like:
\begin{verbatim}
	_ccall_ foo	`seqPrimIO` bar
\end{verbatim}
Since we're not using the result of @foo@, the result if (presumably)
@void@.

\begin{code}
disambigGroup :: [Inst]	-- All standard classes of form (C a)
	      -> TcM TcDictBinds

disambigGroup dicts
  |   any isNumericClass classes 	-- Guaranteed all standard classes
	  -- see comment at the end of function for reasons as to 
	  -- why the defaulting mechanism doesn't apply to groups that
	  -- include CCallable or CReturnable dicts.
   && not (any isCcallishClass classes)
  = 	-- THE DICTS OBEY THE DEFAULTABLE CONSTRAINT
	-- SO, TRY DEFAULT TYPES IN ORDER

	-- Failure here is caused by there being no type in the
	-- default list which can satisfy all the ambiguous classes.
	-- For example, if Real a is reqd, but the only type in the
	-- default list is Int.
    tcGetDefaultTys			`thenNF_Tc` \ default_tys ->
    let
      try_default [] 	-- No defaults work, so fail
	= failTc

      try_default (default_ty : default_tys)
	= tryTc_ (try_default default_tys) $	-- If default_ty fails, we try
						-- default_tys instead
	  tcSimplifyCheckThetas [] theta	`thenTc` \ _ ->
	  returnTc default_ty
        where
	  theta = [mkClassPred clas [default_ty] | clas <- classes]
    in
	-- See if any default works, and if so bind the type variable to it
	-- If not, add an AmbigErr
    recoverTc (addAmbigErrs dicts			`thenNF_Tc_` 
	       returnTc EmptyMonoBinds)	$

    try_default default_tys		 	`thenTc` \ chosen_default_ty ->

	-- Bind the type variable and reduce the context, for real this time
    unifyTauTy chosen_default_ty (mkTyVarTy tyvar)	`thenTc_`
    simpleReduceLoop (text "disambig" <+> ppr dicts)
		     try_me dicts			`thenTc` \ (frees, binds, ambigs) ->
    WARN( not (null frees && null ambigs), ppr frees $$ ppr ambigs )
    warnDefault dicts chosen_default_ty			`thenTc_`
    returnTc binds

  | all isCreturnableClass classes
  = 	-- Default CCall stuff to (); we don't even both to check that () is an 
	-- instance of CReturnable, because we know it is.
    unifyTauTy (mkTyVarTy tyvar) unitTy    `thenTc_`
    returnTc EmptyMonoBinds
    
  | otherwise -- No defaults
  = addAmbigErrs dicts	`thenNF_Tc_`
    returnTc EmptyMonoBinds

  where
    try_me inst = ReduceMe			-- This reduce should not fail
    tyvar       = get_tv (head dicts)		-- Should be non-empty
    classes     = map get_clas dicts
\end{code}

[Aside - why the defaulting mechanism is turned off when
 dealing with arguments and results to ccalls.

When typechecking _ccall_s, TcExpr ensures that the external
function is only passed arguments (and in the other direction,
results) of a restricted set of 'native' types. This is
implemented via the help of the pseudo-type classes,
@CReturnable@ (CR) and @CCallable@ (CC.)
 
The interaction between the defaulting mechanism for numeric
values and CC & CR can be a bit puzzling to the user at times.
For example,

    x <- _ccall_ f
    if (x /= 0) then
       _ccall_ g x
     else
       return ()

What type has 'x' got here? That depends on the default list
in operation, if it is equal to Haskell 98's default-default
of (Integer, Double), 'x' has type Double, since Integer
is not an instance of CR. If the default list is equal to
Haskell 1.4's default-default of (Int, Double), 'x' has type
Int. 

To try to minimise the potential for surprises here, the
defaulting mechanism is turned off in the presence of
CCallable and CReturnable.

]


%************************************************************************
%*									*
\subsection[simple]{@Simple@ versions}
%*									*
%************************************************************************

Much simpler versions when there are no bindings to make!

@tcSimplifyThetas@ simplifies class-type constraints formed by
@deriving@ declarations and when specialising instances.  We are
only interested in the simplified bunch of class/type constraints.

It simplifies to constraints of the form (C a b c) where
a,b,c are type variables.  This is required for the context of
instance declarations.

\begin{code}
tcSimplifyThetas :: ThetaType		-- Wanted
	       	 -> TcM ThetaType		-- Needed

tcSimplifyThetas wanteds
  = doptsTc Opt_GlasgowExts 		`thenNF_Tc` \ glaExts ->
    reduceSimple [] wanteds		`thenNF_Tc` \ irreds ->
    let
	-- For multi-param Haskell, check that the returned dictionaries
 	-- don't have any of the form (C Int Bool) for which
	-- we expect an instance here
	-- For Haskell 98, check that all the constraints are of the form C a,
	-- where a is a type variable
    	bad_guys | glaExts   = [pred | pred <- irreds, 
				       isEmptyVarSet (tyVarsOfPred pred)]
		 | otherwise = [pred | pred <- irreds, 
				       not (isTyVarClassPred pred)]
    in
    if null bad_guys then
	returnTc irreds
    else
       mapNF_Tc addNoInstErr bad_guys		`thenNF_Tc_`
       failTc
\end{code}

@tcSimplifyCheckThetas@ just checks class-type constraints, essentially;
used with \tr{default} declarations.  We are only interested in
whether it worked or not.

\begin{code}
tcSimplifyCheckThetas :: ThetaType	-- Given
		      -> ThetaType	-- Wanted
		      -> TcM ()

tcSimplifyCheckThetas givens wanteds
  = reduceSimple givens wanteds    `thenNF_Tc`	\ irreds ->
    if null irreds then
       returnTc ()
    else
       mapNF_Tc addNoInstErr irreds		`thenNF_Tc_`
       failTc
\end{code}


\begin{code}
type AvailsSimple = FiniteMap PredType Bool
		    -- True  => irreducible 
		    -- False => given, or can be derived from a given or from an irreducible

reduceSimple :: ThetaType			-- Given
	     -> ThetaType			-- Wanted
	     -> NF_TcM ThetaType		-- Irreducible

reduceSimple givens wanteds
  = reduce_simple (0,[]) givens_fm wanteds	`thenNF_Tc` \ givens_fm' ->
    returnNF_Tc [pred | (pred,True) <- fmToList givens_fm']
  where
    givens_fm     = foldl addNonIrred emptyFM givens

reduce_simple :: (Int,ThetaType)		-- Stack
	      -> AvailsSimple
	      -> ThetaType
	      -> NF_TcM AvailsSimple

reduce_simple (n,stack) avails wanteds
  = go avails wanteds
  where
    go avails []     = returnNF_Tc avails
    go avails (w:ws) = reduce_simple_help (n+1,w:stack) avails w	`thenNF_Tc` \ avails' ->
		       go avails' ws

reduce_simple_help stack givens wanted
  | wanted `elemFM` givens
  = returnNF_Tc givens

  | Just (clas, tys) <- getClassPredTys_maybe wanted
  = lookupSimpleInst clas tys	`thenNF_Tc` \ maybe_theta ->
    case maybe_theta of
      Nothing ->    returnNF_Tc (addSimpleIrred givens wanted)
      Just theta -> reduce_simple stack (addNonIrred givens wanted) theta

  | otherwise
  = returnNF_Tc (addSimpleIrred givens wanted)

addSimpleIrred :: AvailsSimple -> PredType -> AvailsSimple
addSimpleIrred givens pred
  = addSCs (addToFM givens pred True) pred

addNonIrred :: AvailsSimple -> PredType -> AvailsSimple
addNonIrred givens pred
  = addSCs (addToFM givens pred False) pred

addSCs givens pred
  | not (isClassPred pred) = givens
  | otherwise		   = foldl add givens sc_theta
 where
   Just (clas,tys) = getClassPredTys_maybe pred
   (tyvars, sc_theta_tmpl, _, _) = classBigSig clas
   sc_theta = substTheta (mkTopTyVarSubst tyvars tys) sc_theta_tmpl

   add givens ct
     = case lookupFM givens ct of
       Nothing    -> -- Add it and its superclasses
		     addSCs (addToFM givens ct False) ct

       Just True  -> -- Set its flag to False; superclasses already done
		     addToFM givens ct False

       Just False -> -- Already done
		     givens
			   
\end{code}


%************************************************************************
%*									*
\section{Errors and contexts}
%*									*
%************************************************************************

ToDo: for these error messages, should we note the location as coming
from the insts, or just whatever seems to be around in the monad just
now?

\begin{code}
addTopAmbigErrs dicts
  = mapNF_Tc complain tidy_dicts
  where
    fixed_tvs = oclose (predsOfInsts tidy_dicts) emptyVarSet
    (tidy_env, tidy_dicts) = tidyInsts dicts
    complain d | any isIPPred (predsOfInst d)	      = addTopIPErr tidy_env d
	       | not (isTyVarDict d) ||
	         tyVarsOfInst d `subVarSet` fixed_tvs = addTopInstanceErr tidy_env d
	       | otherwise			      = addAmbigErr tidy_env d

addTopIPErr tidy_env tidy_dict
  = addInstErrTcM (instLoc tidy_dict) 
	(tidy_env, 
	 ptext SLIT("Unbound implicit parameter") <+> quotes (pprInst tidy_dict))

-- Used for top-level irreducibles
addTopInstanceErr tidy_env tidy_dict
  = addInstErrTcM (instLoc tidy_dict) 
	(tidy_env, 
	 ptext SLIT("No instance for") <+> quotes (pprInst tidy_dict))

addAmbigErrs dicts
  = mapNF_Tc (addAmbigErr tidy_env) tidy_dicts
  where
    (tidy_env, tidy_dicts) = tidyInsts dicts

addAmbigErr tidy_env tidy_dict
  = addInstErrTcM (instLoc tidy_dict)
	(tidy_env,
	 sep [text "Ambiguous type variable(s)" <+> pprQuotedList ambig_tvs,
	      nest 4 (text "in the constraint" <+> quotes (pprInst tidy_dict))])
  where
    ambig_tvs = varSetElems (tyVarsOfInst tidy_dict)

warnDefault dicts default_ty
  = doptsTc Opt_WarnTypeDefaults  `thenTc` \ warn_flag ->
    if warn_flag 
	then mapNF_Tc warn groups  `thenNF_Tc_`  returnNF_Tc ()
	else returnNF_Tc ()

  where
	-- Tidy them first
    (_, tidy_dicts) = tidyInsts dicts

	-- Group the dictionaries by source location
    groups      = equivClasses cmp tidy_dicts
    i1 `cmp` i2 = get_loc i1 `compare` get_loc i2
    get_loc i   = case instLoc i of { (_,loc,_) -> loc }

    warn [dict] = tcAddSrcLoc (get_loc dict) $
		  warnTc True (ptext SLIT("Defaulting") <+> quotes (pprInst dict) <+> 
			       ptext SLIT("to type") <+> quotes (ppr default_ty))

    warn dicts  = tcAddSrcLoc (get_loc (head dicts)) $
		  warnTc True (vcat [ptext SLIT("Defaulting the following constraint(s) to type") <+> quotes (ppr default_ty),
				     pprInstsInFull dicts])

-- The error message when we don't find a suitable instance
-- is complicated by the fact that sometimes this is because
-- there is no instance, and sometimes it's because there are
-- too many instances (overlap).  See the comments in TcEnv.lhs
-- with the InstEnv stuff.
addNoInstanceErr what_doc givens dict
  = tcGetInstEnv	`thenNF_Tc` \ inst_env ->
    let
    	doc = vcat [sep [herald <+> quotes (pprInst tidy_dict),
			 nest 4 $ ptext SLIT("from the context") <+> pprInsts tidy_givens],
		    ambig_doc,
		    ptext SLIT("Probable fix:"),
		    nest 4 fix1,
		    nest 4 fix2]
    
    	herald = ptext SLIT("Could not") <+> unambig_doc <+> ptext SLIT("deduce")
    	unambig_doc | ambig_overlap = ptext SLIT("unambiguously")	
		    | otherwise     = empty
    
    	ambig_doc 
	    | not ambig_overlap = empty
	    | otherwise 	    
	    = vcat [ptext SLIT("The choice of (overlapping) instance declaration"),
		    nest 4 (ptext SLIT("depends on the instantiation of") <+> 
			    quotes (pprWithCommas ppr (varSetElems (tyVarsOfInst tidy_dict))))]
    
    	fix1 = sep [ptext SLIT("Add") <+> quotes (pprInst tidy_dict),
		    ptext SLIT("to the") <+> what_doc]
    
    	fix2 | isTyVarDict dict || ambig_overlap
	     = empty
	     | otherwise
	     = ptext SLIT("Or add an instance declaration for") <+> quotes (pprInst tidy_dict)
    
    	(tidy_env, tidy_dict:tidy_givens) = tidyInsts (dict:givens)
    
	    -- Checks for the ambiguous case when we have overlapping instances
    	ambig_overlap | isClassDict dict
		      = case lookupInstEnv inst_env clas tys of
			    NoMatch ambig -> ambig
			    other 	  -> False
		      | otherwise = False
		      where
    			(clas,tys) = getDictClassTys dict
    in
    addInstErrTcM (instLoc dict) (tidy_env, doc)

-- Used for the ...Thetas variants; all top level
addNoInstErr pred
  = addErrTc (ptext SLIT("No instance for") <+> quotes (ppr pred))

reduceDepthErr n stack
  = vcat [ptext SLIT("Context reduction stack overflow; size =") <+> int n,
	  ptext SLIT("Use -fcontext-stack20 to increase stack size to (e.g.) 20"),
	  nest 4 (pprInstsInFull stack)]

reduceDepthMsg n stack = nest 4 (pprInstsInFull stack)

-----------------------------------------------
addCantGenErr inst
  = addErrTc (sep [ptext SLIT("Cannot generalise these overloadings (in a _ccall_):"), 
		   nest 4 (ppr inst <+> pprInstLoc (instLoc inst))])
\end{code}
