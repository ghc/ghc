%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[TcSimplify]{TcSimplify}



\begin{code}
module TcSimplify (
	tcSimplifyInfer, tcSimplifyInferCheck,
	tcSimplifyCheck, tcSimplifyRestricted,
	tcSimplifyToDicts, tcSimplifyIPs, tcSimplifyTop,

	tcSimplifyDeriv, tcSimplifyDefault,
	bindInstsOfLocalFuns
    ) where

#include "HsVersions.h"

import {-# SOURCE #-} TcUnify( unifyTauTy )

import HsSyn		( MonoBinds(..), HsExpr(..), andMonoBinds, andMonoBindList )
import TcHsSyn		( TcExpr, TcId,
			  TcMonoBinds, TcDictBinds
			)

import TcMonad
import Inst		( lookupInst, lookupSimpleInst, LookupInstResult(..),
			  tyVarsOfInst, predsOfInsts, predsOfInst, newDicts,
			  isDict, isClassDict, isLinearInst, linearInstType,
			  isStdClassTyVarDict, isMethodFor, isMethod,
			  instToId, tyVarsOfInsts,  cloneDict,
			  ipNamesOfInsts, ipNamesOfInst, dictPred,
			  instBindingRequired, instCanBeGeneralised,
			  newDictsFromOld, newMethodAtLoc,
			  getDictClassTys, isTyVarDict,
			  instLoc, pprInst, zonkInst, tidyInsts, tidyMoreInsts,
			  Inst, LIE, pprInsts, pprInstsInFull,
			  mkLIE, lieToList
			)
import TcEnv		( tcGetGlobalTyVars, tcGetInstEnv, tcLookupGlobalId )
import InstEnv		( lookupInstEnv, classInstEnv, InstLookupResult(..) )
import TcMType		( zonkTcTyVarsAndFV, tcInstTyVars, checkAmbiguity )
import TcType		( TcTyVar, TcTyVarSet, ThetaType, PredType, 
			  mkClassPred, isOverloadedTy, mkTyConApp,
			  mkTyVarTy, tcGetTyVar, isTyVarClassPred, mkTyVarTys,
			  tyVarsOfPred, getClassPredTys_maybe, isClassPred, isIPPred,
			  inheritablePred, predHasFDs )
import Id		( idType, mkUserLocal )
import Var		( TyVar )
import Name		( getOccName, getSrcLoc )
import NameSet		( NameSet, mkNameSet, elemNameSet )
import Class		( classBigSig )
import FunDeps		( oclose, grow, improve, pprEquationDoc )
import PrelInfo		( isNumericClass, isCreturnableClass, isCcallishClass, 
			  splitIdName, fstIdName, sndIdName )

import Subst		( mkTopTyVarSubst, substTheta, substTy )
import TysWiredIn	( unitTy, pairTyCon )
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

So here's the plan.  We WARN about probable ambiguity if

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
The definitely-ambiguous can then float out, and get smashed at top level
(which squashes out the constants, like Eq (T a) above)


	--------------------------------------
		Notes on principal types
	--------------------------------------

    class C a where
      op :: a -> a

    f x = let g y = op (y::Int) in True

Here the principal type of f is (forall a. a->a)
but we'll produce the non-principal type
    f :: forall a. C Int => a -> a


	--------------------------------------
		Notes on implicit parameters
	--------------------------------------

Question 1: can we "inherit" implicit parameters
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider this:

	f x = (x::Int) + ?y

where f is *not* a top-level binding.
From the RHS of f we'll get the constraint (?y::Int).
There are two types we might infer for f:

	f :: Int -> Int

(so we get ?y from the context of f's definition), or

	f :: (?y::Int) => Int -> Int

At first you might think the first was better, becuase then
?y behaves like a free variable of the definition, rather than
having to be passed at each call site.  But of course, the WHOLE
IDEA is that ?y should be passed at each call site (that's what
dynamic binding means) so we'd better infer the second.

BOTTOM LINE: when *inferring types* you *must* quantify 
over implicit parameters. See the predicate isFreeWhenInferring.


Question 2: type signatures
~~~~~~~~~~~~~~~~~~~~~~~~~~~
BUT WATCH OUT: When you supply a type signature, we can't force you
to quantify over implicit parameters.  For example:

	(?x + 1) :: Int

This is perfectly reasonable.  We do not want to insist on

	(?x + 1) :: (?x::Int => Int)

That would be silly.  Here, the definition site *is* the occurrence site,
so the above strictures don't apply.  Hence the difference between
tcSimplifyCheck (which *does* allow implicit paramters to be inherited)
and tcSimplifyCheckBind (which does not).

What about when you supply a type signature for a binding?
Is it legal to give the following explicit, user type 
signature to f, thus:

	f :: Int -> Int
	f x = (x::Int) + ?y

At first sight this seems reasonable, but it has the nasty property
that adding a type signature changes the dynamic semantics.
Consider this:

	(let f x = (x::Int) + ?y
 	 in (f 3, f 3 with ?y=5))  with ?y = 6

		returns (3+6, 3+5)
vs
	(let f :: Int -> Int
	     f x = x + ?y
	 in (f 3, f 3 with ?y=5))  with ?y = 6

		returns (3+6, 3+6)

Indeed, simply inlining f (at the Haskell source level) would change the
dynamic semantics.

Nevertheless, as Launchbury says (email Oct 01) we can't really give the
semantics for a Haskell program without knowing its typing, so if you 
change the typing you may change the semantics.

To make things consistent in all cases where we are *checking* against
a supplied signature (as opposed to inferring a type), we adopt the
rule: 

	a signature does not need to quantify over implicit params.

[This represents a (rather marginal) change of policy since GHC 5.02,
which *required* an explicit signature to quantify over all implicit
params for the reasons mentioned above.]

But that raises a new question.  Consider 

	Given (signature)	?x::Int
	Wanted (inferred)	?x::Int, ?y::Bool

Clearly we want to discharge the ?x and float the ?y out.  But
what is the criterion that distinguishes them?  Clearly it isn't
what free type variables they have.  The Right Thing seems to be
to float a constraint that
	neither mentions any of the quantified type variables
	nor any of the quantified implicit parameters

See the predicate isFreeWhenChecking.


Question 3: monomorphism
~~~~~~~~~~~~~~~~~~~~~~~~
There's a nasty corner case when the monomorphism restriction bites:

	z = (x::Int) + ?y

The argument above suggests that we *must* generalise
over the ?y parameter, to get
	z :: (?y::Int) => Int,
but the monomorphism restriction says that we *must not*, giving
	z :: Int.
Why does the momomorphism restriction say this?  Because if you have

	let z = x + ?y in z+z

you might not expect the addition to be done twice --- but it will if
we follow the argument of Question 2 and generalise over ?y.



Possible choices
~~~~~~~~~~~~~~~~
(A) Always generalise over implicit parameters
    Bindings that fall under the monomorphism restriction can't
	be generalised

    Consequences:
	* Inlining remains valid
	* No unexpected loss of sharing
	* But simple bindings like
		z = ?y + 1
	  will be rejected, unless you add an explicit type signature
	  (to avoid the monomorphism restriction)
		z :: (?y::Int) => Int
		z = ?y + 1
	  This seems unacceptable

(B) Monomorphism restriction "wins"
    Bindings that fall under the monomorphism restriction can't
	be generalised
    Always generalise over implicit parameters *except* for bindings
	that fall under the monomorphism restriction

    Consequences
	* Inlining isn't valid in general
	* No unexpected loss of sharing
	* Simple bindings like
		z = ?y + 1
	  accepted (get value of ?y from binding site)

(C) Always generalise over implicit parameters
    Bindings that fall under the monomorphism restriction can't
	be generalised, EXCEPT for implicit parameters
    Consequences
	* Inlining remains valid
	* Unexpected loss of sharing (from the extra generalisation)
	* Simple bindings like
		z = ?y + 1
	  accepted (get value of ?y from occurrence sites)


Discussion
~~~~~~~~~~
None of these choices seems very satisfactory.  But at least we should
decide which we want to do.

It's really not clear what is the Right Thing To Do.  If you see

	z = (x::Int) + ?y

would you expect the value of ?y to be got from the *occurrence sites*
of 'z', or from the valuue of ?y at the *definition* of 'z'?  In the
case of function definitions, the answer is clearly the former, but
less so in the case of non-fucntion definitions.   On the other hand,
if we say that we get the value of ?y from the definition site of 'z',
then inlining 'z' might change the semantics of the program.

Choice (C) really says "the monomorphism restriction doesn't apply
to implicit parameters".  Which is fine, but remember that every
innocent binding 'x = ...' that mentions an implicit parameter in
the RHS becomes a *function* of that parameter, called at each
use of 'x'.  Now, the chances are that there are no intervening 'with'
clauses that bind ?y, so a decent compiler should common up all
those function calls.  So I think I strongly favour (C).  Indeed,
one could make a similar argument for abolishing the monomorphism
restriction altogether.

BOTTOM LINE: we choose (B) at present.  See tcSimplifyRestricted



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
	-> TcTyVarSet		-- fv(T); type vars
	-> LIE			-- Wanted
	-> TcM ([TcTyVar],	-- Tyvars to quantify (zonked)
		LIE,		-- Free
		TcDictBinds,	-- Bindings
		[TcId])		-- Dict Ids that must be bound here (zonked)
\end{code}


\begin{code}
tcSimplifyInfer doc tau_tvs wanted_lie
  = inferLoop doc (varSetElems tau_tvs)
	      (lieToList wanted_lie)	`thenTc` \ (qtvs, frees, binds, irreds) ->

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
	  | isFreeWhenInferring qtvs inst = Free
	  | isClassDict inst 		  = DontReduceUnlessConstant	-- Dicts
	  | otherwise	    		  = ReduceMe 			-- Lits and Methods
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

	-- However, NOTICE that when we are done, we might have some bindings, but
	-- the final qtvs might be empty.  See [NO TYVARS] below.
				
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

[NO TYVARS]

	class Y a b | a -> b where
	    y :: a -> X b
	
	instance Y [[a]] a where
	    y ((x:_):_) = X x
	
	k :: X a -> X a -> X a

	g :: Num a => [X a] -> [X a]
	g xs = h xs
	    where
	    h ys = ys ++ map (k (y [[0]])) xs

The excitement comes when simplifying the bindings for h.  Initially
try to simplify {y @ [[t1]] t2, 0 @ t1}, with initial qtvs = {t2}.
From this we get t1:=:t2, but also various bindings.  We can't forget
the bindings (because of [LOOP]), but in fact t1 is what g is
polymorphic in.  

The net effect of [NO TYVARS] 

\begin{code}
isFreeWhenInferring :: TyVarSet -> Inst	-> Bool
isFreeWhenInferring qtvs inst
  =  isFreeWrtTyVars qtvs inst			-- Constrains no quantified vars
  && all inheritablePred (predsOfInst inst)	-- And no implicit parameter involved
						-- (see "Notes on implicit parameters")

isFreeWhenChecking :: TyVarSet	-- Quantified tyvars
	 	   -> NameSet	-- Quantified implicit parameters
		   -> Inst -> Bool
isFreeWhenChecking qtvs ips inst
  =  isFreeWrtTyVars qtvs inst
  && isFreeWrtIPs    ips inst

isFreeWrtTyVars qtvs inst = not (tyVarsOfInst inst `intersectsVarSet` qtvs)
isFreeWrtIPs     ips inst = not (any (`elemNameSet` ips) (ipNamesOfInst inst))
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

-- tcSimplifyCheck is used when checking expression type signatures,
-- class decls, instance decls etc.
-- Note that we psss isFree (not isFreeAndInheritable) to tcSimplCheck
-- It's important that we can float out non-inheritable predicates
-- Example:		(?x :: Int) is ok!
tcSimplifyCheck doc qtvs givens wanted_lie
  = tcSimplCheck doc get_qtvs
		 givens wanted_lie	`thenTc` \ (qtvs', frees, binds) ->
    returnTc (frees, binds)
  where
    get_qtvs = zonkTcTyVarsAndFV qtvs


-- tcSimplifyInferCheck is used when we know the constraints we are to simplify
-- against, but we don't know the type variables over which we are going to quantify.
-- This happens when we have a type signature for a mutually recursive group
tcSimplifyInferCheck
	 :: SDoc
	 -> TcTyVarSet		-- fv(T)
	 -> [Inst]		-- Given
	 -> LIE			-- Wanted
	 -> TcM ([TcTyVar],	-- Variables over which to quantify
		 LIE,		-- Free
		 TcDictBinds)	-- Bindings

tcSimplifyInferCheck doc tau_tvs givens wanted_lie
  = tcSimplCheck doc get_qtvs givens wanted_lie
  where
	-- Figure out which type variables to quantify over
	-- You might think it should just be the signature tyvars,
	-- but in bizarre cases you can get extra ones
	-- 	f :: forall a. Num a => a -> a
	--	f x = fst (g (x, head [])) + 1
	--	g a b = (b,a)
	-- Here we infer g :: forall a b. a -> b -> (b,a)
	-- We don't want g to be monomorphic in b just because
	-- f isn't quantified over b.
    all_tvs = varSetElems (tau_tvs `unionVarSet` tyVarsOfInsts givens)

    get_qtvs = zonkTcTyVarsAndFV all_tvs	`thenNF_Tc` \ all_tvs' ->
	       tcGetGlobalTyVars		`thenNF_Tc` \ gbl_tvs ->
	       let
	          qtvs = all_tvs' `minusVarSet` gbl_tvs
			-- We could close gbl_tvs, but its not necessary for
			-- soundness, and it'll only affect which tyvars, not which
			-- dictionaries, we quantify over
	       in
	       returnNF_Tc qtvs
\end{code}

Here is the workhorse function for all three wrappers.

\begin{code}
tcSimplCheck doc get_qtvs givens wanted_lie
  = check_loop givens (lieToList wanted_lie)	`thenTc` \ (qtvs, frees, binds, irreds) ->

	-- Complain about any irreducible ones
    complainCheck doc givens irreds		`thenNF_Tc_`

	-- Done
    returnTc (qtvs, mkLIE frees, binds)

  where
    ip_set = mkNameSet (ipNamesOfInsts givens)

    check_loop givens wanteds
      =		-- Step 1
    	mapNF_Tc zonkInst givens	`thenNF_Tc` \ givens' ->
    	mapNF_Tc zonkInst wanteds	`thenNF_Tc` \ wanteds' ->
    	get_qtvs 			`thenNF_Tc` \ qtvs' ->

 		    -- Step 2
    	let
 	    -- When checking against a given signature we always reduce
 	    -- until we find a match against something given, or can't reduce
 	    try_me inst | isFreeWhenChecking qtvs' ip_set inst = Free
 			| otherwise  			       = ReduceMe
    	in
    	reduceContext doc try_me givens' wanteds'	`thenTc` \ (no_improvement, frees, binds, irreds) ->

 		    -- Step 3
    	if no_improvement then
 	    returnTc (varSetElems qtvs', frees, binds, irreds)
    	else
 	    check_loop givens' (irreds ++ frees) 	`thenTc` \ (qtvs', frees1, binds1, irreds1) ->
 	    returnTc (qtvs', frees1, binds `AndMonoBinds` binds1, irreds1)
\end{code}


%************************************************************************
%*									*
\subsection{tcSimplifyRestricted}
%*									*
%************************************************************************

\begin{code}
tcSimplifyRestricted 	-- Used for restricted binding groups
			-- i.e. ones subject to the monomorphism restriction
	:: SDoc
	-> TcTyVarSet		-- Free in the type of the RHSs
	-> LIE			-- Free in the RHSs
	-> TcM ([TcTyVar],	-- Tyvars to quantify (zonked)
		LIE,		-- Free
		TcDictBinds)	-- Bindings

tcSimplifyRestricted doc tau_tvs wanted_lie
  = 	-- First squash out all methods, to find the constrained tyvars
   	-- We can't just take the free vars of wanted_lie because that'll
	-- have methods that may incidentally mention entirely unconstrained variables
	--  	e.g. a call to 	f :: Eq a => a -> b -> b
	-- Here, b is unconstrained.  A good example would be
	--	foo = f (3::Int)
	-- We want to infer the polymorphic type
	--	foo :: forall b. b -> b
    let
	wanteds = lieToList wanted_lie
	try_me inst = ReduceMe		-- Reduce as far as we can.  Don't stop at
					-- dicts; the idea is to get rid of as many type
					-- variables as possible, and we don't want to stop
					-- at (say) Monad (ST s), because that reduces
					-- immediately, with no constraint on s.
    in
    simpleReduceLoop doc try_me wanteds		`thenTc` \ (_, _, constrained_dicts) ->

	-- Next, figure out the tyvars we will quantify over
    zonkTcTyVarsAndFV (varSetElems tau_tvs)	`thenNF_Tc` \ tau_tvs' ->
    tcGetGlobalTyVars				`thenNF_Tc` \ gbl_tvs ->
    let
	constrained_tvs = tyVarsOfInsts constrained_dicts
	qtvs = (tau_tvs' `minusVarSet` oclose (predsOfInsts constrained_dicts) gbl_tvs)
			 `minusVarSet` constrained_tvs
    in

	-- The first step may have squashed more methods than
	-- necessary, so try again, this time knowing the exact
	-- set of type variables to quantify over.
	--
	-- We quantify only over constraints that are captured by qtvs;
	-- these will just be a subset of non-dicts.  This in contrast
	-- to normal inference (using isFreeWhenInferring) in which we quantify over
	-- all *non-inheritable* constraints too.  This implements choice
	-- (B) under "implicit parameter and monomorphism" above.
	--
	-- Remember that we may need to do *some* simplification, to
	-- (for example) squash {Monad (ST s)} into {}.  It's not enough
	-- just to float all constraints
    mapNF_Tc zonkInst (lieToList wanted_lie)	`thenNF_Tc` \ wanteds' ->
    let
        try_me inst | isFreeWrtTyVars qtvs inst = Free
	            | otherwise                 = ReduceMe
    in
    reduceContext doc try_me [] wanteds'	`thenTc` \ (no_improvement, frees, binds, irreds) ->
    ASSERT( no_improvement )
    ASSERT( null irreds )
	-- No need to loop because simpleReduceLoop will have
	-- already done any improvement necessary

    returnTc (varSetElems qtvs, mkLIE frees, binds)
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
step; if we have ?x::t1 and ?x::t2 we must unify t1, t2.

Actually, the constraints from B might improve the types in ?x. For example

	f :: (?x::Int) => Char -> Char
	let ?x = 3 in f 'c'

then the constraint (?x::Int) arising from the call to f will
force the binding for ?x to be of type Int.

\begin{code}
tcSimplifyIPs :: [Inst]		-- The implicit parameters bound here
	      -> LIE
	      -> TcM (LIE, TcDictBinds)
tcSimplifyIPs given_ips wanted_lie
  = simpl_loop given_ips wanteds	`thenTc` \ (frees, binds) ->
    returnTc (mkLIE frees, binds)
  where
    doc	     = text "tcSimplifyIPs" <+> ppr given_ips
    wanteds  = lieToList wanted_lie
    ip_set   = mkNameSet (ipNamesOfInsts given_ips)

	-- Simplify any methods that mention the implicit parameter
    try_me inst | isFreeWrtIPs ip_set inst = Free
		| otherwise		   = ReduceMe

    simpl_loop givens wanteds
      = mapNF_Tc zonkInst givens		`thenNF_Tc` \ givens' ->
        mapNF_Tc zonkInst wanteds		`thenNF_Tc` \ wanteds' ->

        reduceContext doc try_me givens' wanteds'    `thenTc` \ (no_improvement, frees, binds, irreds) ->

        if no_improvement then
	    ASSERT( null irreds )
	    returnTc (frees, binds)
	else
	    simpl_loop givens' (irreds ++ frees)	`thenTc` \ (frees1, binds1) ->
	    returnTc (frees1, binds `AndMonoBinds` binds1)
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
    is_overloaded id = isOverloadedTy (idType id)

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

reduceMe :: Inst -> WhatToDo
reduceMe inst = ReduceMe

data WantSCs = NoSCs | AddSCs	-- Tells whether we should add the superclasses
				-- of a predicate when adding it to the avails
\end{code}



\begin{code}
type Avails = FiniteMap Inst Avail

data Avail
  = IsFree		-- Used for free Insts
  | Irred		-- Used for irreducible dictionaries,
			-- which are going to be lambda bound

  | Given TcId 		-- Used for dictionaries for which we have a binding
			-- e.g. those "given" in a signature
	  Bool		-- True <=> actually consumed (splittable IPs only)

  | NoRhs 		-- Used for Insts like (CCallable f)
			-- where no witness is required.

  | Rhs 		-- Used when there is a RHS
	TcExpr	 	-- The RHS
	[Inst]		-- Insts free in the RHS; we need these too

  | Linear 		-- Splittable Insts only.
	Int		-- The Int is always 2 or more; indicates how
			-- many copies are required
	Inst 		-- The splitter
	Avail		-- Where the "master copy" is

  | LinRhss		-- Splittable Insts only; this is used only internally
			-- 	by extractResults, where a Linear 
			--	is turned into an LinRhss
	[TcExpr]	-- A supply of suitable RHSs

pprAvails avails = vcat [ppr inst <+> equals <+> pprAvail avail
			| (inst,avail) <- fmToList avails ]

instance Outputable Avail where
    ppr = pprAvail

pprAvail NoRhs	       	= text "<no rhs>"
pprAvail IsFree	       	= text "Free"
pprAvail Irred	       	= text "Irred"
pprAvail (Given x b)   	= text "Given" <+> ppr x <+> 
		 	  if b then text "(used)" else empty
pprAvail (Rhs rhs bs)   = text "Rhs" <+> ppr rhs <+> braces (ppr bs)
pprAvail (Linear n i a) = text "Linear" <+> ppr n <+> braces (ppr i) <+> ppr a
pprAvail (LinRhss rhss) = text "LinRhss" <+> ppr rhss
\end{code}

Extracting the bindings from a bunch of Avails.
The bindings do *not* come back sorted in dependency order.
We assume that they'll be wrapped in a big Rec, so that the
dependency analyser can sort them out later

The loop startes
\begin{code}
extractResults :: Avails
	       -> [Inst]		-- Wanted
	       -> NF_TcM (TcDictBinds, 	-- Bindings
			  [Inst],	-- Irreducible ones
			  [Inst])	-- Free ones

extractResults avails wanteds
  = go avails EmptyMonoBinds [] [] wanteds
  where
    go avails binds irreds frees [] 
      = returnNF_Tc (binds, irreds, frees)

    go avails binds irreds frees (w:ws)
      = case lookupFM avails w of
	  Nothing    -> pprTrace "Urk: extractResults" (ppr w) $
			go avails binds irreds frees ws

	  Just NoRhs  -> go avails		 binds irreds     frees     ws
	  Just IsFree -> go (add_free avails w)  binds irreds     (w:frees) ws
	  Just Irred  -> go (add_given avails w) binds (w:irreds) frees     ws

	  Just (Given id _) -> go avails new_binds irreds frees ws
			    where
			       new_binds | id == instToId w = binds
					 | otherwise        = addBind binds w (HsVar id)
		-- The sought Id can be one of the givens, via a superclass chain
		-- and then we definitely don't want to generate an x=x binding!

	  Just (Rhs rhs ws') -> go (add_given avails w) new_binds irreds frees (ws' ++ ws)
			     where
				new_binds = addBind binds w rhs

	  Just (LinRhss (rhs:rhss))	-- Consume one of the Rhss
		-> go new_avails new_binds irreds frees ws
		where		
		   new_binds  = addBind binds w rhs
		   new_avails = addToFM avails w (LinRhss rhss)

	  Just (Linear n split_inst avail)
	    -> split n (instToId split_inst) avail w	`thenNF_Tc` \ (binds', (rhs:rhss), irreds') ->
	       go (addToFM avails w (LinRhss rhss))
		  (binds `AndMonoBinds` addBind binds' w rhs)
		  (irreds' ++ irreds) frees (split_inst:ws)


    add_given avails w 
	| instBindingRequired w = addToFM avails w (Given (instToId w) True)
	| otherwise		= addToFM avails w NoRhs
	-- NB: make sure that CCallable/CReturnable use NoRhs rather
	--	than Given, else we end up with bogus bindings.

    add_free avails w | isMethod w = avails
		      | otherwise  = add_given avails w
	-- NB: Hack alert!  
	-- Do *not* replace Free by Given if it's a method.
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
	-- (split n i a) returns: n rhss
	--			  auxiliary bindings
	--			  1 or 0 insts to add to irreds


split :: Int -> TcId -> Avail -> Inst 
      -> NF_TcM (TcDictBinds, [TcExpr], [Inst])
-- (split n split_id avail wanted) returns
--	* a list of 'n' expressions, all of which witness 'avail'
--	* a bunch of auxiliary bindings to support these expressions
--	* one or zero insts needed to witness the whole lot
--	  (maybe be zero if the initial Inst is a Given)
split n split_id avail wanted
  = go n
  where
    ty  = linearInstType wanted
    pair_ty = mkTyConApp pairTyCon [ty,ty]
    id  = instToId wanted
    occ = getOccName id
    loc = getSrcLoc id

    go 1 = case avail of
	     Given id _ -> returnNF_Tc (EmptyMonoBinds, [HsVar id], [])
	     Irred      -> cloneDict wanted		`thenNF_Tc` \ w' ->
			   returnNF_Tc (EmptyMonoBinds, [HsVar (instToId w')], [w'])

    go n = go ((n+1) `div` 2)		`thenNF_Tc` \ (binds1, rhss, irred) ->
	   expand n rhss		`thenNF_Tc` \ (binds2, rhss') ->
	   returnNF_Tc (binds1 `AndMonoBinds` binds2, rhss', irred)

	-- (expand n rhss) 
	-- Given ((n+1)/2) rhss, make n rhss, using auxiliary bindings
	--  e.g.  expand 3 [rhs1, rhs2]
	--	  = ( { x = split rhs1 },
	--	      [fst x, snd x, rhs2] )
    expand n rhss
	| n `rem` 2 == 0 = go rhss 	-- n is even
	| otherwise  	 = go (tail rhss)	`thenNF_Tc` \ (binds', rhss') ->
			   returnNF_Tc (binds', head rhss : rhss')
	where
	  go rhss = mapAndUnzipNF_Tc do_one rhss	`thenNF_Tc` \ (binds', rhss') ->
		    returnNF_Tc (andMonoBindList binds', concat rhss')

	  do_one rhs = tcGetUnique 			`thenNF_Tc` \ uniq -> 
		       tcLookupGlobalId fstIdName	`thenNF_Tc` \ fst_id -> 
		       tcLookupGlobalId sndIdName	`thenNF_Tc` \ snd_id -> 
		       let 
			  x = mkUserLocal occ uniq pair_ty loc
		       in
		       returnNF_Tc (VarMonoBind x (mk_app split_id rhs),
				    [mk_fs_app fst_id ty x, mk_fs_app snd_id ty x])

mk_fs_app id ty var = HsVar id `TyApp` [ty,ty] `HsApp` HsVar var

mk_app id rhs = HsApp (HsVar id) rhs

addBind binds inst rhs = binds `AndMonoBinds` VarMonoBind (instToId inst) rhs
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
    foldlNF_Tc addGiven emptyFM givens			`thenNF_Tc` \ init_state ->

        -- Do the real work
    reduceList (0,[]) try_me wanteds init_state		`thenNF_Tc` \ avails ->

	-- Do improvement, using everything in avails
	-- In particular, avails includes all superclasses of everything
    tcImprove avails					`thenTc` \ no_improvement ->

    extractResults avails wanteds			`thenNF_Tc` \ (binds, irreds, frees) ->

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

    returnTc (no_improvement, frees, binds, irreds)

tcImprove avails
 =  tcGetInstEnv 				`thenTc` \ inst_env ->
    let
	preds = [ (pred, pp_loc)
		| inst <- keysFM avails,
		  let pp_loc = pprInstLoc (instLoc inst),
		  pred <- predsOfInst inst,
		  predHasFDs pred
		]
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
	traceTc (ptext SLIT("Improve:") <+> vcat (map pprEquationDoc eqns))	`thenNF_Tc_`
        mapTc_ unify eqns	`thenTc_`
	returnTc False
  where
    unify ((qtvs, t1, t2), doc)
	 = tcAddErrCtxt doc			$
	   tcInstTyVars (varSetElems qtvs)	`thenNF_Tc` \ (_, _, tenv) ->
	   unifyTauTy (substTy tenv t1) (substTy tenv t2)
\end{code}

The main context-reduction function is @reduce@.  Here's its game plan.

\begin{code}
reduceList :: (Int,[Inst])		-- Stack (for err msgs)
					-- along with its depth
       	   -> (Inst -> WhatToDo)
       	   -> [Inst]
       	   -> Avails
       	   -> TcM Avails
\end{code}

@reduce@ is passed
     try_me:	given an inst, this function returns
		  Reduce       reduce this
		  DontReduce   return this in "irreds"
		  Free	       return this in "frees"

     wanteds:	The list of insts to reduce
     state:	An accumulating parameter of type Avails
		that contains the state of the algorithm

  It returns a Avails.

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
  | Just avail <- isAvailable state wanted
  = if isLinearInst wanted then
	addLinearAvailable state avail wanted	`thenNF_Tc` \ (state', wanteds') ->
	reduceList stack try_me wanteds' state'
    else
	returnTc state		-- No op for non-linear things

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
-------------------------
isAvailable :: Avails -> Inst -> Maybe Avail
isAvailable avails wanted = lookupFM avails wanted
	-- NB 1: the Ord instance of Inst compares by the class/type info
	-- *not* by unique.  So
	--	d1::C Int ==  d2::C Int

addLinearAvailable :: Avails -> Avail -> Inst -> NF_TcM (Avails, [Inst])
addLinearAvailable avails avail wanted
  | need_split avail
  = tcLookupGlobalId splitIdName		`thenNF_Tc` \ split_id ->
    newMethodAtLoc (instLoc wanted) split_id 
		   [linearInstType wanted]	`thenNF_Tc` \ (split_inst,_) ->
    returnNF_Tc (addToFM avails wanted (Linear 2 split_inst avail), [split_inst])

  | otherwise
  = returnNF_Tc (addToFM avails wanted avail', [])
  where
    avail' = case avail of
		Given id _   -> Given id True
		Linear n i a -> Linear (n+1) i a 

    need_split Irred	      = True
    need_split (Given _ used) = used
    need_split (Linear _ _ _) = False

-------------------------
addFree :: Avails -> Inst -> NF_TcM Avails
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
addFree avails free = returnNF_Tc (addToFM avails free IsFree)

addWanted :: Avails -> Inst -> TcExpr -> [Inst] -> NF_TcM Avails
addWanted avails wanted rhs_expr wanteds
-- Do *not* add superclasses as well.  Here's an example of why not
-- 	class Eq a => Foo a b
--	instance Eq a => Foo [a] a
-- If we are reducing
--	(Foo [t] t)
-- we'll first deduce that it holds (via the instance decl).  We
-- must not then overwrite the Eq t constraint with a superclass selection!
-- 	ToDo: this isn't entirely unsatisfactory, because
--	      we may also lose some entirely-legitimate sharing this way

  = ASSERT( not (wanted `elemFM` avails) )
    returnNF_Tc (addToFM avails wanted avail)
  where
    avail | instBindingRequired wanted = Rhs rhs_expr wanteds
	  | otherwise		       = ASSERT( null wanteds ) NoRhs

addGiven :: Avails -> Inst -> NF_TcM Avails
addGiven state given = addAvailAndSCs state given (Given (instToId given) False)

addIrred :: WantSCs -> Avails -> Inst -> NF_TcM Avails
addIrred NoSCs  state irred = returnNF_Tc (addToFM state irred Irred)
addIrred AddSCs state irred = addAvailAndSCs state irred Irred

addAvailAndSCs :: Avails -> Inst -> Avail -> NF_TcM Avails
addAvailAndSCs avails wanted avail
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
	  Just (Given _ _) -> returnNF_Tc avails	-- See Note [SUPER] below
	  other		   -> addAvailAndSCs avails sc_dict avail
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
build a loop!  Hence looking for Given.  Crudely, Given is cheaper
than a selection.


%************************************************************************
%*									*
\section{tcSimplifyTop: defaulting}
%*									*
%************************************************************************


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
  = simpleReduceLoop (text "tcSimplTop") reduceMe wanteds	`thenTc` \ (frees, binds, irreds) ->
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

    d1 `cmp_by_tyvar` d2 = get_tv d1 `compare` get_tv d2

get_tv d   = case getDictClassTys d of
		   (clas, [ty]) -> tcGetTyVar "tcSimplify" ty
get_clas d = case getDictClassTys d of
		   (clas, [ty]) -> clas
\end{code}

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
	  tcSimplifyDefault theta		`thenTc` \ _ ->
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
		     reduceMe dicts			`thenTc` \ (frees, binds, ambigs) ->
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

End of aside]


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
tcSimplifyDeriv :: [TyVar]	
		-> ThetaType		-- Wanted
	        -> TcM ThetaType	-- Needed

tcSimplifyDeriv tyvars theta
  = tcInstTyVars tyvars					`thenNF_Tc` \ (tvs, _, tenv) ->
	-- The main loop may do unification, and that may crash if 
	-- it doesn't see a TcTyVar, so we have to instantiate. Sigh
	-- ToDo: what if two of them do get unified?
    newDicts DataDeclOrigin (substTheta tenv theta)	`thenNF_Tc` \ wanteds ->
    simpleReduceLoop doc reduceMe wanteds		`thenTc` \ (frees, _, irreds) ->
    ASSERT( null frees )			-- reduceMe never returns Free

    doptsTc Opt_AllowUndecidableInstances		`thenNF_Tc` \ undecidable_ok ->
    let
 	tv_set      = mkVarSet tvs
	simpl_theta = map dictPred irreds	-- reduceMe squashes all non-dicts

	check_pred pred
	  | isEmptyVarSet pred_tyvars	-- Things like (Eq T) should be rejected
	  = addErrTc (noInstErr pred)

	  | not undecidable_ok && not (isTyVarClassPred pred)
	  -- Check that the returned dictionaries are all of form (C a b)
	  -- 	(where a, b are type variables).  
	  -- We allow this if we had -fallow-undecidable-instances,
	  -- but note that risks non-termination in the 'deriving' context-inference
	  -- fixpoint loop.   It is useful for situations like
	  --	data Min h a = E | M a (h a)
	  -- which gives the instance decl
	  --	instance (Eq a, Eq (h a)) => Eq (Min h a)
          = addErrTc (noInstErr pred)
  
	  | not (pred_tyvars `subVarSet` tv_set) 
	  -- Check for a bizarre corner case, when the derived instance decl should
	  -- have form 	instance C a b => D (T a) where ...
	  -- Note that 'b' isn't a parameter of T.  This gives rise to all sorts
	  -- of problems; in particular, it's hard to compare solutions for
	  -- equality when finding the fixpoint.  So I just rule it out for now.
	  = addErrTc (badDerivedPred pred)
  
	  | otherwise
	  = returnNF_Tc ()
	  where
	    pred_tyvars = tyVarsOfPred pred

	rev_env = mkTopTyVarSubst tvs (mkTyVarTys tyvars)
		-- This reverse-mapping is a Royal Pain, 
		-- but the result should mention TyVars not TcTyVars
    in
   
    mapNF_Tc check_pred simpl_theta		`thenNF_Tc_`
    checkAmbiguity tvs simpl_theta tv_set	`thenTc_`
    returnTc (substTheta rev_env simpl_theta)
  where
    doc    = ptext SLIT("deriving classes for a data type")
\end{code}

@tcSimplifyDefault@ just checks class-type constraints, essentially;
used with \tr{default} declarations.  We are only interested in
whether it worked or not.

\begin{code}
tcSimplifyDefault :: ThetaType	-- Wanted; has no type variables in it
		  -> TcM ()

tcSimplifyDefault theta
  = newDicts DataDeclOrigin theta		`thenNF_Tc` \ wanteds ->
    simpleReduceLoop doc reduceMe wanteds	`thenTc` \ (frees, _, irreds) ->
    ASSERT( null frees )	-- try_me never returns Free
    mapNF_Tc (addErrTc . noInstErr) irreds 	`thenNF_Tc_`
    if null irreds then
	returnTc ()
    else
	failTc
  where
    doc = ptext SLIT("default declaration")
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
groupInsts :: [Inst] -> [[Inst]]
-- Group together insts with the same origin
-- We want to report them together in error messages
groupInsts [] 		= []
groupInsts (inst:insts) = (inst:friends) : groupInsts others
			where
				-- (It may seem a bit crude to compare the error messages,
				--  but it makes sure that we combine just what the user sees,
				--  and it avoids need equality on InstLocs.)
			  (friends, others) = partition is_friend insts
			  loc_msg	    = showSDoc (pprInstLoc (instLoc inst))
			  is_friend friend  = showSDoc (pprInstLoc (instLoc friend)) == loc_msg


addTopAmbigErrs dicts
  = mapNF_Tc (addTopInstanceErrs tidy_env) (groupInsts no_insts)	`thenNF_Tc_`
    mapNF_Tc (addTopIPErrs tidy_env)       (groupInsts bad_ips)		`thenNF_Tc_`
    mapNF_Tc (addAmbigErr tidy_env)	   ambigs			`thenNF_Tc_`
    returnNF_Tc ()
  where
    fixed_tvs = oclose (predsOfInsts tidy_dicts) emptyVarSet
    (tidy_env, tidy_dicts) = tidyInsts dicts
    (bad_ips, non_ips)     = partition is_ip tidy_dicts
    (no_insts, ambigs)     = partition no_inst non_ips
    is_ip d   = any isIPPred (predsOfInst d)
    no_inst d = not (isTyVarDict d) || tyVarsOfInst d `subVarSet` fixed_tvs

plural [x] = empty
plural xs  = char 's'

addTopIPErrs tidy_env tidy_dicts
  = addInstErrTcM (instLoc (head tidy_dicts))
	(tidy_env,
	 ptext SLIT("Unbound implicit parameter") <> plural tidy_dicts <+> pprInsts tidy_dicts)

-- Used for top-level irreducibles
addTopInstanceErrs tidy_env tidy_dicts
  = addInstErrTcM (instLoc (head tidy_dicts))
	(tidy_env,
	 ptext SLIT("No instance") <> plural tidy_dicts <+> 
	 	ptext SLIT("for") <+> pprInsts tidy_dicts)

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
    tcAddSrcLoc (get_loc (head dicts)) (warnTc warn_flag warn_msg)
  where
	-- Tidy them first
    (_, tidy_dicts) = tidyInsts dicts
    get_loc i = case instLoc i of { (_,loc,_) -> loc }
    warn_msg  = vcat [ptext SLIT("Defaulting the following constraint(s) to type") <+>
				quotes (ppr default_ty),
		      pprInstsInFull tidy_dicts]

complainCheck doc givens irreds
  = mapNF_Tc zonkInst given_dicts_and_ips		 	  `thenNF_Tc` \ givens' ->
    mapNF_Tc (addNoInstanceErrs doc givens') (groupInsts irreds)  `thenNF_Tc_`
    returnNF_Tc ()
  where
    given_dicts_and_ips = filter (not . isMethod) givens
	-- Filter out methods, which are only added to
	-- the given set as an optimisation

addNoInstanceErrs what_doc givens dicts
  = getDOptsTc		`thenNF_Tc` \ dflags ->
    tcGetInstEnv	`thenNF_Tc` \ inst_env ->
    let
    	(tidy_env1, tidy_givens) = tidyInsts givens
	(tidy_env2, tidy_dicts)  = tidyMoreInsts tidy_env1 dicts

    	doc = vcat [sep [herald <+> pprInsts tidy_dicts,
			 nest 4 $ ptext SLIT("from the context") <+> pprInsts tidy_givens],
		    ambig_doc,
		    ptext SLIT("Probable fix:"),
		    nest 4 fix1,
		    nest 4 fix2]

    	herald = ptext SLIT("Could not") <+> unambig_doc <+> ptext SLIT("deduce")
    	unambig_doc | ambig_overlap = ptext SLIT("unambiguously")
		    | otherwise     = empty

		-- The error message when we don't find a suitable instance
		-- is complicated by the fact that sometimes this is because
		-- there is no instance, and sometimes it's because there are
		-- too many instances (overlap).  See the comments in TcEnv.lhs
		-- with the InstEnv stuff.

    	ambig_doc
	    | not ambig_overlap = empty
	    | otherwise
	    = vcat [ptext SLIT("The choice of (overlapping) instance declaration"),
		    nest 4 (ptext SLIT("depends on the instantiation of") <+>
			    quotes (pprWithCommas ppr (varSetElems (tyVarsOfInsts tidy_dicts))))]

    	fix1 = sep [ptext SLIT("Add") <+> pprInsts tidy_dicts,
		    ptext SLIT("to the") <+> what_doc]

    	fix2 | null instance_dicts 
	     = empty
	     | otherwise
	     = ptext SLIT("Or add an instance declaration for") <+> pprInsts instance_dicts

	instance_dicts = [d | d <- tidy_dicts, isClassDict d, not (isTyVarDict d)]
		-- Insts for which it is worth suggesting an adding an instance declaration
		-- Exclude implicit parameters, and tyvar dicts

	    -- Checks for the ambiguous case when we have overlapping instances
	ambig_overlap = any ambig_overlap1 dicts
    	ambig_overlap1 dict 
		| isClassDict dict
		= case lookupInstEnv dflags inst_env clas tys of
			    NoMatch ambig -> ambig
			    other 	  -> False
		| otherwise = False
		where
    		  (clas,tys) = getDictClassTys dict
    in
    addInstErrTcM (instLoc (head dicts)) (tidy_env2, doc)

-- Used for the ...Thetas variants; all top level
noInstErr pred = ptext SLIT("No instance for") <+> quotes (ppr pred)

badDerivedPred pred
  = vcat [ptext SLIT("Can't derive instances where the instance context mentions"),
	  ptext SLIT("type variables that are not data type parameters"),
	  nest 2 (ptext SLIT("Offending constraint:") <+> ppr pred)]

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
