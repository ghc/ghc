%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[TcSimplify]{TcSimplify}



\begin{code}
module TcSimplify (
	tcSimplifyInfer, tcSimplifyInferCheck,
	tcSimplifyCheck, tcSimplifyRestricted,
	tcSimplifyToDicts, tcSimplifyIPs, 
	tcSimplifySuperClasses,
	tcSimplifyTop, tcSimplifyInteractive,
	tcSimplifyBracket,

	tcSimplifyDeriv, tcSimplifyDefault,
	bindInstsOfLocalFuns
    ) where

#include "HsVersions.h"

import {-# SOURCE #-} TcUnify( unifyTauTy )
import HsSyn		( HsBind(..), HsExpr(..), LHsExpr, emptyLHsBinds )
import TcHsSyn		( TcId, TcDictBinds, mkHsApp, mkHsTyApp, mkHsDictApp )

import TcRnMonad
import Inst		( lookupInst, LookupInstResult(..),
			  tyVarsOfInst, fdPredsOfInsts, newDicts, 
			  isDict, isClassDict, isLinearInst, linearInstType,
			  isStdClassTyVarDict, isMethodFor, isMethod,
			  instToId, tyVarsOfInsts,  cloneDict,
			  ipNamesOfInsts, ipNamesOfInst, dictPred,
			  instBindingRequired, fdPredsOfInst,
			  newDictsAtLoc, tcInstClassOp,
			  getDictClassTys, isTyVarDict, instLoc,
			  zonkInst, tidyInsts, tidyMoreInsts,
			  Inst, pprInsts, pprDictsInFull, pprInstInFull, tcGetInstEnvs,
			  isInheritableInst, pprDictsTheta
			)
import TcEnv		( tcGetGlobalTyVars, tcLookupId, findGlobals, pprBinders,
			  lclEnvElts, tcMetaTy )
import InstEnv		( lookupInstEnv, classInstances, pprInstances )
import TcMType		( zonkTcTyVarsAndFV, tcInstTyVars, checkAmbiguity )
import TcType		( TcTyVar, TcTyVarSet, ThetaType, TcPredType, 
                          mkClassPred, isOverloadedTy, mkTyConApp, isSkolemTyVar,
			  mkTyVarTy, tcGetTyVar, isTyVarClassPred, mkTyVarTys,
			  tyVarsOfPred, tcEqType, pprPred, mkPredTy )
import TcIface		( checkWiredInTyCon )
import Id		( idType, mkUserLocal )
import Var		( TyVar )
import Name		( Name, getOccName, getSrcLoc )
import NameSet		( NameSet, mkNameSet, elemNameSet )
import Class		( classBigSig, classKey )
import FunDeps		( oclose, grow, improve, pprEquationDoc )
import PrelInfo		( isNumericClass ) 
import PrelNames	( splitName, fstName, sndName, integerTyConName,
			  showClassKey, eqClassKey, ordClassKey )
import Type		( zipTopTvSubst, substTheta, substTy )
import TysWiredIn	( pairTyCon, doubleTy, doubleTyCon )
import ErrUtils		( Message )
import BasicTypes	( TopLevelFlag, isNotTopLevel )
import VarSet
import VarEnv		( TidyEnv )
import FiniteMap
import Bag
import Outputable
import ListSetOps	( equivClasses )
import Util		( zipEqual, isSingleton )
import List		( partition )
import SrcLoc		( Located(..) )
import DynFlags		( DynFlag(..) )
import StaticFlags
\end{code}


%************************************************************************
%*									*
\subsection{NOTES}
%*									*
%************************************************************************

	--------------------------------------
	Notes on functional dependencies (a bug)
	--------------------------------------

| > class Foo a b | a->b
| >
| > class Bar a b | a->b
| >
| > data Obj = Obj
| >
| > instance Bar Obj Obj
| >
| > instance (Bar a b) => Foo a b
| >
| > foo:: (Foo a b) => a -> String
| > foo _ = "works"
| >
| > runFoo:: (forall a b. (Foo a b) => a -> w) -> w
| > runFoo f = f Obj
| 
| *Test> runFoo foo
| 
| <interactive>:1:
|     Could not deduce (Bar a b) from the context (Foo a b)
|       arising from use of `foo' at <interactive>:1
|     Probable fix:
|         Add (Bar a b) to the expected type of an expression
|     In the first argument of `runFoo', namely `foo'
|     In the definition of `it': it = runFoo foo
| 
| Why all of the sudden does GHC need the constraint Bar a b? The
| function foo didn't ask for that... 

The trouble is that to type (runFoo foo), GHC has to solve the problem:

	Given constraint	Foo a b
	Solve constraint	Foo a b'

Notice that b and b' aren't the same.  To solve this, just do
improvement and then they are the same.  But GHC currently does
	simplify constraints
	apply improvement
	and loop

That is usually fine, but it isn't here, because it sees that Foo a b is
not the same as Foo a b', and so instead applies the instance decl for
instance Bar a b => Foo a b.  And that's where the Bar constraint comes
from.

The Right Thing is to improve whenever the constraint set changes at
all.  Not hard in principle, but it'll take a bit of fiddling to do.  



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
	The need for forall's in constraints
	--------------------------------------

[Exchange on Haskell Cafe 5/6 Dec 2000]

  class C t where op :: t -> Bool
  instance C [t] where op x = True

  p y = (let f :: c -> Bool; f x = op (y >> return x) in f, y ++ [])
  q y = (y ++ [], let f :: c -> Bool; f x = op (y >> return x) in f)

The definitions of p and q differ only in the order of the components in
the pair on their right-hand sides.  And yet:

  ghc and "Typing Haskell in Haskell" reject p, but accept q;
  Hugs rejects q, but accepts p;
  hbc rejects both p and q;
  nhc98 ... (Malcolm, can you fill in the blank for us!).

The type signature for f forces context reduction to take place, and
the results of this depend on whether or not the type of y is known,
which in turn depends on which component of the pair the type checker
analyzes first.  

Solution: if y::m a, float out the constraints
	Monad m, forall c. C (m c)
When m is later unified with [], we can solve both constraints.


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


Question 4: top level
~~~~~~~~~~~~~~~~~~~~~
At the top level, monomorhism makes no sense at all.

    module Main where
	main = let ?x = 5 in print foo

	foo = woggle 3

	woggle :: (?x :: Int) => Int -> Int
	woggle y = ?x + y

We definitely don't want (foo :: Int) with a top-level implicit parameter
(?x::Int) becuase there is no way to bind it.  


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
	-> [Inst]		-- Wanted
	-> TcM ([TcTyVar],	-- Tyvars to quantify (zonked)
		TcDictBinds,	-- Bindings
		[TcId])		-- Dict Ids that must be bound here (zonked)
	-- Any free (escaping) Insts are tossed into the environment
\end{code}


\begin{code}
tcSimplifyInfer doc tau_tvs wanted_lie
  = inferLoop doc (varSetElems tau_tvs)
	      wanted_lie		`thenM` \ (qtvs, frees, binds, irreds) ->

    extendLIEs frees							`thenM_`
    returnM (qtvs, binds, map instToId irreds)

inferLoop doc tau_tvs wanteds
  =   	-- Step 1
    zonkTcTyVarsAndFV tau_tvs		`thenM` \ tau_tvs' ->
    mappM zonkInst wanteds		`thenM` \ wanteds' ->
    tcGetGlobalTyVars			`thenM` \ gbl_tvs ->
    let
 	preds = fdPredsOfInsts wanteds'
	qtvs  = grow preds tau_tvs' `minusVarSet` oclose preds gbl_tvs

	try_me inst
	  | isFreeWhenInferring qtvs inst = Free
	  | isClassDict inst 		  = DontReduceUnlessConstant	-- Dicts
	  | otherwise	    		  = ReduceMe NoSCs		-- Lits and Methods
    in
    traceTc (text "infloop" <+> vcat [ppr tau_tvs', ppr wanteds', ppr preds, 
				      ppr (grow preds tau_tvs'), ppr qtvs])	`thenM_`
		-- Step 2
    reduceContext doc try_me [] wanteds'    `thenM` \ (no_improvement, frees, binds, irreds) ->

		-- Step 3
    if no_improvement then
	returnM (varSetElems qtvs, frees, binds, irreds)
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
				
	inferLoop doc tau_tvs (irreds ++ frees)	`thenM` \ (qtvs1, frees1, binds1, irreds1) ->
	returnM (qtvs1, frees1, binds `unionBags` binds1, irreds1)
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
  =  isFreeWrtTyVars qtvs inst		-- Constrains no quantified vars
  && isInheritableInst inst		-- And no implicit parameter involved
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
	 -> [Inst]		-- Wanted
	 -> TcM TcDictBinds	-- Bindings

-- tcSimplifyCheck is used when checking expression type signatures,
-- class decls, instance decls etc.
--
-- NB: tcSimplifyCheck does not consult the
--	global type variables in the environment; so you don't
--	need to worry about setting them before calling tcSimplifyCheck
tcSimplifyCheck doc qtvs givens wanted_lie
  = ASSERT( all isSkolemTyVar qtvs )
    do	{ (qtvs', frees, binds) <- tcSimplCheck doc get_qtvs AddSCs givens wanted_lie
	; extendLIEs frees
	; return binds }
  where
--  get_qtvs = zonkTcTyVarsAndFV qtvs
    get_qtvs = return (mkVarSet qtvs)	-- All skolems


-- tcSimplifyInferCheck is used when we know the constraints we are to simplify
-- against, but we don't know the type variables over which we are going to quantify.
-- This happens when we have a type signature for a mutually recursive group
tcSimplifyInferCheck
	 :: SDoc
	 -> TcTyVarSet		-- fv(T)
	 -> [Inst]		-- Given
	 -> [Inst]		-- Wanted
	 -> TcM ([TcTyVar],	-- Variables over which to quantify
		 TcDictBinds)	-- Bindings

tcSimplifyInferCheck doc tau_tvs givens wanted_lie
  = do	{ (qtvs', frees, binds) <- tcSimplCheck doc get_qtvs AddSCs givens wanted_lie
	; extendLIEs frees
	; return (qtvs', binds) }
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

    get_qtvs = zonkTcTyVarsAndFV all_tvs	`thenM` \ all_tvs' ->
	       tcGetGlobalTyVars		`thenM` \ gbl_tvs ->
	       let
	          qtvs = all_tvs' `minusVarSet` gbl_tvs
			-- We could close gbl_tvs, but its not necessary for
			-- soundness, and it'll only affect which tyvars, not which
			-- dictionaries, we quantify over
	       in
	       returnM qtvs
\end{code}

Here is the workhorse function for all three wrappers.

\begin{code}
tcSimplCheck doc get_qtvs want_scs givens wanted_lie
  = do	{ (qtvs, frees, binds, irreds) <- check_loop givens wanted_lie

		-- Complain about any irreducible ones
	; if not (null irreds)
	  then do { givens' <- mappM zonkInst given_dicts_and_ips
		  ; groupErrs (addNoInstanceErrs (Just doc) givens') irreds }
	  else return ()

	; returnM (qtvs, frees, binds) }
  where
    given_dicts_and_ips = filter (not . isMethod) givens
	-- For error reporting, filter out methods, which are 
	-- only added to the given set as an optimisation

    ip_set = mkNameSet (ipNamesOfInsts givens)

    check_loop givens wanteds
      =		-- Step 1
    	mappM zonkInst givens	`thenM` \ givens' ->
    	mappM zonkInst wanteds	`thenM` \ wanteds' ->
    	get_qtvs 		`thenM` \ qtvs' ->

 		    -- Step 2
    	let
 	    -- When checking against a given signature we always reduce
 	    -- until we find a match against something given, or can't reduce
 	    try_me inst | isFreeWhenChecking qtvs' ip_set inst = Free
 			| otherwise  			       = ReduceMe want_scs
    	in
    	reduceContext doc try_me givens' wanteds'	`thenM` \ (no_improvement, frees, binds, irreds) ->

 		    -- Step 3
    	if no_improvement then
 	    returnM (varSetElems qtvs', frees, binds, irreds)
    	else
 	    check_loop givens' (irreds ++ frees) 	`thenM` \ (qtvs', frees1, binds1, irreds1) ->
 	    returnM (qtvs', frees1, binds `unionBags` binds1, irreds1)
\end{code}


%************************************************************************
%*									*
		tcSimplifySuperClasses
%*									*
%************************************************************************

Note [SUPERCLASS-LOOP 1]
~~~~~~~~~~~~~~~~~~~~~~~~
We have to be very, very careful when generating superclasses, lest we
accidentally build a loop. Here's an example:

  class S a

  class S a => C a where { opc :: a -> a }
  class S b => D b where { opd :: b -> b }
  
  instance C Int where
     opc = opd
  
  instance D Int where
     opd = opc

From (instance C Int) we get the constraint set {ds1:S Int, dd:D Int}
Simplifying, we may well get:
	$dfCInt = :C ds1 (opd dd)
	dd  = $dfDInt
	ds1 = $p1 dd
Notice that we spot that we can extract ds1 from dd.  

Alas!  Alack! We can do the same for (instance D Int):

	$dfDInt = :D ds2 (opc dc)
	dc  = $dfCInt
	ds2 = $p1 dc

And now we've defined the superclass in terms of itself.

Solution: never generate a superclass selectors at all when
satisfying the superclass context of an instance declaration.

Two more nasty cases are in
	tcrun021
	tcrun033

\begin{code}
tcSimplifySuperClasses qtvs givens sc_wanteds
  = ASSERT( all isSkolemTyVar qtvs )
    do	{ (_, frees, binds1) <- tcSimplCheck doc get_qtvs NoSCs givens sc_wanteds
	; binds2	     <- tc_simplify_top doc False NoSCs frees
	; return (binds1 `unionBags` binds2) }
  where
    get_qtvs = return (mkVarSet qtvs)
    doc = ptext SLIT("instance declaration superclass context")
\end{code}


%************************************************************************
%*									*
\subsection{tcSimplifyRestricted}
%*									*
%************************************************************************

tcSimplifyRestricted infers which type variables to quantify for a 
group of restricted bindings.  This isn't trivial.

Eg1:	id = \x -> x
	We want to quantify over a to get id :: forall a. a->a
	
Eg2:	eq = (==)
	We do not want to quantify over a, because there's an Eq a 
	constraint, so we get eq :: a->a->Bool	(notice no forall)

So, assume:
	RHS has type 'tau', whose free tyvars are tau_tvs
	RHS has constraints 'wanteds'

Plan A (simple)
  Quantify over (tau_tvs \ ftvs(wanteds))
  This is bad. The constraints may contain (Monad (ST s))
  where we have 	instance Monad (ST s) where...
  so there's no need to be monomorphic in s!

  Also the constraint might be a method constraint,
  whose type mentions a perfectly innocent tyvar:
	  op :: Num a => a -> b -> a
  Here, b is unconstrained.  A good example would be
	foo = op (3::Int)
  We want to infer the polymorphic type
	foo :: forall b. b -> b


Plan B (cunning, used for a long time up to and including GHC 6.2)
  Step 1: Simplify the constraints as much as possible (to deal 
  with Plan A's problem).  Then set
	qtvs = tau_tvs \ ftvs( simplify( wanteds ) )

  Step 2: Now simplify again, treating the constraint as 'free' if 
  it does not mention qtvs, and trying to reduce it otherwise.
  The reasons for this is to maximise sharing.

  This fails for a very subtle reason.  Suppose that in the Step 2
  a constraint (Foo (Succ Zero) (Succ Zero) b) gets thrown upstairs as 'free'.
  In the Step 1 this constraint might have been simplified, perhaps to
  (Foo Zero Zero b), AND THEN THAT MIGHT BE IMPROVED, to bind 'b' to 'T'.
  This won't happen in Step 2... but that in turn might prevent some other
  constraint (Baz [a] b) being simplified (e.g. via instance Baz [a] T where {..}) 
  and that in turn breaks the invariant that no constraints are quantified over.

  Test typecheck/should_compile/tc177 (which failed in GHC 6.2) demonstrates
  the problem.


Plan C (brutal)
  Step 1: Simplify the constraints as much as possible (to deal 
  with Plan A's problem).  Then set
	qtvs = tau_tvs \ ftvs( simplify( wanteds ) )
  Return the bindings from Step 1.
  

A note about Plan C (arising from "bug" reported by George Russel March 2004)
Consider this:

      instance (HasBinary ty IO) => HasCodedValue ty

      foo :: HasCodedValue a => String -> IO a

      doDecodeIO :: HasCodedValue a => () -> () -> IO a
      doDecodeIO codedValue view  
        = let { act = foo "foo" } in  act

You might think this should work becuase the call to foo gives rise to a constraint
(HasCodedValue t), which can be satisfied by the type sig for doDecodeIO.  But the
restricted binding act = ... calls tcSimplifyRestricted, and PlanC simplifies the
constraint using the (rather bogus) instance declaration, and now we are stuffed.

I claim this is not really a bug -- but it bit Sergey as well as George.  So here's
plan D


Plan D (a variant of plan B)
  Step 1: Simplify the constraints as much as possible (to deal 
  with Plan A's problem), BUT DO NO IMPROVEMENT.  Then set
	qtvs = tau_tvs \ ftvs( simplify( wanteds ) )

  Step 2: Now simplify again, treating the constraint as 'free' if 
  it does not mention qtvs, and trying to reduce it otherwise.

  The point here is that it's generally OK to have too few qtvs; that is,
  to make the thing more monomorphic than it could be.  We don't want to
  do that in the common cases, but in wierd cases it's ok: the programmer
  can always add a signature.  

  Too few qtvs => too many wanteds, which is what happens if you do less
  improvement.


\begin{code}
tcSimplifyRestricted 	-- Used for restricted binding groups
			-- i.e. ones subject to the monomorphism restriction
	:: SDoc
	-> TopLevelFlag
	-> [Name]		-- Things bound in this group
	-> TcTyVarSet		-- Free in the type of the RHSs
	-> [Inst]		-- Free in the RHSs
	-> TcM ([TcTyVar],	-- Tyvars to quantify (zonked)
		TcDictBinds)	-- Bindings
	-- tcSimpifyRestricted returns no constraints to
	-- quantify over; by definition there are none.
	-- They are all thrown back in the LIE

tcSimplifyRestricted doc top_lvl bndrs tau_tvs wanteds
	-- Zonk everything in sight
  = mappM zonkInst wanteds			`thenM` \ wanteds' ->
    zonkTcTyVarsAndFV (varSetElems tau_tvs)	`thenM` \ tau_tvs' ->
    tcGetGlobalTyVars				`thenM` \ gbl_tvs' ->

   	-- 'reduceMe': Reduce as far as we can.  Don't stop at
	-- dicts; the idea is to get rid of as many type
	-- variables as possible, and we don't want to stop
	-- at (say) Monad (ST s), because that reduces
	-- immediately, with no constraint on s.
	--
	-- BUT do no improvement!  See Plan D above
    reduceContextWithoutImprovement 
	doc reduceMe wanteds' 		`thenM` \ (_frees, _binds, constrained_dicts) ->

	-- Next, figure out the tyvars we will quantify over
    let
	constrained_tvs = tyVarsOfInsts constrained_dicts
	qtvs = (tau_tvs' `minusVarSet` oclose (fdPredsOfInsts constrained_dicts) gbl_tvs')
			 `minusVarSet` constrained_tvs
    in
    traceTc (text "tcSimplifyRestricted" <+> vcat [
		pprInsts wanteds, pprInsts _frees, pprInsts constrained_dicts,
		ppr _binds,
		ppr constrained_tvs, ppr tau_tvs', ppr qtvs ])	`thenM_`

	-- The first step may have squashed more methods than
	-- necessary, so try again, this time more gently, knowing the exact
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
	--
	-- At top level, we *do* squash methods becuase we want to 
	-- expose implicit parameters to the test that follows
    let
	is_nested_group = isNotTopLevel top_lvl
        try_me inst | isFreeWrtTyVars qtvs inst,
		      (is_nested_group || isDict inst) = Free
	            | otherwise  		       = ReduceMe AddSCs
    in
    reduceContextWithoutImprovement 
	doc try_me wanteds' 		`thenM` \ (frees, binds, irreds) ->
    ASSERT( null irreds )

	-- See "Notes on implicit parameters, Question 4: top level"
    if is_nested_group then
	extendLIEs frees	`thenM_`
        returnM (varSetElems qtvs, binds)
    else
	let
    	    (non_ips, bad_ips) = partition isClassDict frees
	in    
	addTopIPErrs bndrs bad_ips	`thenM_`
	extendLIEs non_ips		`thenM_`
        returnM (varSetElems qtvs, binds)
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

because the scsel will mess up RULE matching.  Instead we want

	forall dIntegralInt, dNumInt.
	fromIntegral Int Int dIntegralInt dNumInt = id Int

Hence "WithoutSCs"

\begin{code}
tcSimplifyToDicts :: [Inst] -> TcM (TcDictBinds)
tcSimplifyToDicts wanteds
  = simpleReduceLoop doc try_me wanteds		`thenM` \ (frees, binds, irreds) ->
	-- Since try_me doesn't look at types, we don't need to
	-- do any zonking, so it's safe to call reduceContext directly
    ASSERT( null frees )
    extendLIEs irreds		`thenM_`
    returnM binds

  where
    doc = text "tcSimplifyToDicts"

	-- Reduce methods and lits only; stop as soon as we get a dictionary
    try_me inst	| isDict inst = KeepDictWithoutSCs	-- See notes above re "WithoutSCs"
		| otherwise   = ReduceMe NoSCs
\end{code}



tcSimplifyBracket is used when simplifying the constraints arising from
a Template Haskell bracket [| ... |].  We want to check that there aren't
any constraints that can't be satisfied (e.g. Show Foo, where Foo has no
Show instance), but we aren't otherwise interested in the results.
Nor do we care about ambiguous dictionaries etc.  We will type check
this bracket again at its usage site.

\begin{code}
tcSimplifyBracket :: [Inst] -> TcM ()
tcSimplifyBracket wanteds
  = simpleReduceLoop doc reduceMe wanteds	`thenM_`
    returnM ()
  where
    doc = text "tcSimplifyBracket"
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
	      -> [Inst]		-- Wanted
	      -> TcM TcDictBinds
tcSimplifyIPs given_ips wanteds
  = simpl_loop given_ips wanteds	`thenM` \ (frees, binds) ->
    extendLIEs frees			`thenM_`
    returnM binds
  where
    doc	     = text "tcSimplifyIPs" <+> ppr given_ips
    ip_set   = mkNameSet (ipNamesOfInsts given_ips)

	-- Simplify any methods that mention the implicit parameter
    try_me inst | isFreeWrtIPs ip_set inst = Free
		| otherwise		   = ReduceMe NoSCs

    simpl_loop givens wanteds
      = mappM zonkInst givens		`thenM` \ givens' ->
        mappM zonkInst wanteds		`thenM` \ wanteds' ->

        reduceContext doc try_me givens' wanteds'    `thenM` \ (no_improvement, frees, binds, irreds) ->

        if no_improvement then
	    ASSERT( null irreds )
	    returnM (frees, binds)
	else
	    simpl_loop givens' (irreds ++ frees)	`thenM` \ (frees1, binds1) ->
	    returnM (frees1, binds `unionBags` binds1)
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
bindInstsOfLocalFuns ::	[Inst] -> [TcId] -> TcM TcDictBinds
-- Simlifies only MethodInsts, and generate only bindings of form 
--	fm = f tys dicts
-- We're careful not to even generate bindings of the form
--	d1 = d2
-- You'd think that'd be fine, but it interacts with what is
-- arguably a bug in Match.tidyEqnInfo (see notes there)

bindInstsOfLocalFuns wanteds local_ids
  | null overloaded_ids
	-- Common case
  = extendLIEs wanteds		`thenM_`
    returnM emptyLHsBinds

  | otherwise
  = simpleReduceLoop doc try_me for_me	`thenM` \ (frees, binds, irreds) ->
    ASSERT( null irreds )
    extendLIEs not_for_me	`thenM_`
    extendLIEs frees		`thenM_`
    returnM binds
  where
    doc		     = text "bindInsts" <+> ppr local_ids
    overloaded_ids   = filter is_overloaded local_ids
    is_overloaded id = isOverloadedTy (idType id)
    (for_me, not_for_me) = partition (isMethodFor overloaded_set) wanteds

    overloaded_set = mkVarSet overloaded_ids	-- There can occasionally be a lot of them
						-- so it's worth building a set, so that
						-- lookup (in isMethodFor) is faster
    try_me inst | isMethod inst = ReduceMe NoSCs
		| otherwise	= Free
\end{code}


%************************************************************************
%*									*
\subsection{Data types for the reduction mechanism}
%*									*
%************************************************************************

The main control over context reduction is here

\begin{code}
data WhatToDo
 = ReduceMe WantSCs	-- Try to reduce this
			-- If there's no instance, behave exactly like
			-- DontReduce: add the inst to
			-- the irreductible ones, but don't
			-- produce an error message of any kind.
			-- It might be quite legitimate such as (Eq a)!

 | KeepDictWithoutSCs	-- Return as irreducible; don't add its superclasses
			-- Rather specialised: see notes with tcSimplifyToDicts

 | DontReduceUnlessConstant	-- Return as irreducible unless it can
				-- be reduced to a constant in one step

 | Free			  -- Return as free

reduceMe :: Inst -> WhatToDo
reduceMe inst = ReduceMe AddSCs

data WantSCs = NoSCs | AddSCs	-- Tells whether we should add the superclasses
				-- of a predicate when adding it to the avails
\end{code}



\begin{code}
type Avails = FiniteMap Inst Avail
emptyAvails = emptyFM

data Avail
  = IsFree		-- Used for free Insts
  | Irred		-- Used for irreducible dictionaries,
			-- which are going to be lambda bound

  | Given TcId 		-- Used for dictionaries for which we have a binding
			-- e.g. those "given" in a signature
	  Bool		-- True <=> actually consumed (splittable IPs only)

  | NoRhs 		-- Used for Insts like (CCallable f)
			-- where no witness is required.
			-- ToDo: remove?

  | Rhs 		-- Used when there is a RHS
	(LHsExpr TcId) 	-- The RHS
	[Inst]		-- Insts free in the RHS; we need these too

  | Linear 		-- Splittable Insts only.
	Int		-- The Int is always 2 or more; indicates how
			-- many copies are required
	Inst 		-- The splitter
	Avail		-- Where the "master copy" is

  | LinRhss		-- Splittable Insts only; this is used only internally
			-- 	by extractResults, where a Linear 
			--	is turned into an LinRhss
	[LHsExpr TcId]	-- A supply of suitable RHSs

pprAvails avails = vcat [sep [ppr inst, nest 2 (equals <+> pprAvail avail)]
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
	       -> TcM (TcDictBinds, 	-- Bindings
			[Inst],		-- Irreducible ones
			[Inst])		-- Free ones

extractResults avails wanteds
  = go avails emptyBag [] [] wanteds
  where
    go avails binds irreds frees [] 
      = returnM (binds, irreds, frees)

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
					 | otherwise        = addBind binds w (L (instSpan w) (HsVar id))
		-- The sought Id can be one of the givens, via a superclass chain
		-- and then we definitely don't want to generate an x=x binding!

	  Just (Rhs rhs ws') -> go (add_given avails w) new_binds irreds frees (ws' ++ ws)
			     where
				new_binds = addBind binds w rhs

	  Just (Linear n split_inst avail)	-- Transform Linear --> LinRhss
	    -> get_root irreds frees avail w		`thenM` \ (irreds', frees', root_id) ->
	       split n (instToId split_inst) root_id w	`thenM` \ (binds', rhss) ->
	       go (addToFM avails w (LinRhss rhss))
		  (binds `unionBags` binds')
		  irreds' frees' (split_inst : w : ws)

	  Just (LinRhss (rhs:rhss))		-- Consume one of the Rhss
		-> go new_avails new_binds irreds frees ws
		where		
		   new_binds  = addBind binds w rhs
		   new_avails = addToFM avails w (LinRhss rhss)

    get_root irreds frees (Given id _) w = returnM (irreds, frees, id)
    get_root irreds frees Irred	       w = cloneDict w	`thenM` \ w' ->
					   returnM (w':irreds, frees, instToId w')
    get_root irreds frees IsFree       w = cloneDict w	`thenM` \ w' ->
					   returnM (irreds, w':frees, instToId w')

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


split :: Int -> TcId -> TcId -> Inst 
      -> TcM (TcDictBinds, [LHsExpr TcId])
-- (split n split_id root_id wanted) returns
--	* a list of 'n' expressions, all of which witness 'avail'
--	* a bunch of auxiliary bindings to support these expressions
--	* one or zero insts needed to witness the whole lot
--	  (maybe be zero if the initial Inst is a Given)
--
-- NB: 'wanted' is just a template

split n split_id root_id wanted
  = go n
  where
    ty      = linearInstType wanted
    pair_ty = mkTyConApp pairTyCon [ty,ty]
    id      = instToId wanted
    occ     = getOccName id
    loc     = getSrcLoc id
    span    = instSpan wanted

    go 1 = returnM (emptyBag, [L span $ HsVar root_id])

    go n = go ((n+1) `div` 2)		`thenM` \ (binds1, rhss) ->
	   expand n rhss		`thenM` \ (binds2, rhss') ->
	   returnM (binds1 `unionBags` binds2, rhss')

	-- (expand n rhss) 
	-- Given ((n+1)/2) rhss, make n rhss, using auxiliary bindings
	--  e.g.  expand 3 [rhs1, rhs2]
	--	  = ( { x = split rhs1 },
	--	      [fst x, snd x, rhs2] )
    expand n rhss
	| n `rem` 2 == 0 = go rhss 	-- n is even
	| otherwise  	 = go (tail rhss)	`thenM` \ (binds', rhss') ->
			   returnM (binds', head rhss : rhss')
	where
	  go rhss = mapAndUnzipM do_one rhss	`thenM` \ (binds', rhss') ->
		    returnM (listToBag binds', concat rhss')

	  do_one rhs = newUnique 			`thenM` \ uniq -> 
		       tcLookupId fstName		`thenM` \ fst_id ->
		       tcLookupId sndName		`thenM` \ snd_id ->
		       let 
			  x = mkUserLocal occ uniq pair_ty loc
		       in
		       returnM (L span (VarBind x (mk_app span split_id rhs)),
				[mk_fs_app span fst_id ty x, mk_fs_app span snd_id ty x])

mk_fs_app span id ty var = L span (HsVar id) `mkHsTyApp` [ty,ty] `mkHsApp` (L span (HsVar var))

mk_app span id rhs = L span (HsApp (L span (HsVar id)) rhs)

addBind binds inst rhs = binds `unionBags` unitBag (L (instLocSrcSpan (instLoc inst)) 
						      (VarBind (instToId inst) rhs))
instSpan wanted = instLocSrcSpan (instLoc wanted)
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
  = mappM zonkInst wanteds			`thenM` \ wanteds' ->
    reduceContext doc try_me [] wanteds'	`thenM` \ (no_improvement, frees, binds, irreds) ->
    if no_improvement then
	returnM (frees, binds, irreds)
    else
	simpleReduceLoop doc try_me (irreds ++ frees)	`thenM` \ (frees1, binds1, irreds1) ->
	returnM (frees1, binds `unionBags` binds1, irreds1)
\end{code}



\begin{code}
reduceContext :: SDoc
	      -> (Inst -> WhatToDo)
	      -> [Inst]			-- Given
	      -> [Inst]			-- Wanted
	      -> TcM (Bool, 		-- True <=> improve step did no unification
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
	     ]))					`thenM_`

        -- Build the Avail mapping from "givens"
    foldlM addGiven emptyAvails givens			`thenM` \ init_state ->

        -- Do the real work
    reduceList (0,[]) try_me wanteds init_state		`thenM` \ avails ->

	-- Do improvement, using everything in avails
	-- In particular, avails includes all superclasses of everything
    tcImprove avails					`thenM` \ no_improvement ->

    extractResults avails wanteds			`thenM` \ (binds, irreds, frees) ->

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
	     ])) 					`thenM_`

    returnM (no_improvement, frees, binds, irreds)

-- reduceContextWithoutImprovement differs from reduceContext
--	(a) no improvement
--	(b) 'givens' is assumed empty
reduceContextWithoutImprovement doc try_me wanteds
  =
    traceTc (text "reduceContextWithoutImprovement" <+> (vcat [
	     text "----------------------",
	     doc,
	     text "wanted" <+> ppr wanteds,
	     text "----------------------"
	     ]))					`thenM_`

        -- Do the real work
    reduceList (0,[]) try_me wanteds emptyAvails	`thenM` \ avails ->
    extractResults avails wanteds			`thenM` \ (binds, irreds, frees) ->

    traceTc (text "reduceContextWithoutImprovement end" <+> (vcat [
	     text "----------------------",
	     doc,
	     text "wanted" <+> ppr wanteds,
	     text "----",
	     text "avails" <+> pprAvails avails,
	     text "frees" <+> ppr frees,
	     text "----------------------"
	     ])) 					`thenM_`

    returnM (frees, binds, irreds)

tcImprove :: Avails -> TcM Bool		-- False <=> no change
-- Perform improvement using all the predicates in Avails
tcImprove avails
 =  tcGetInstEnvs 			`thenM` \ inst_envs -> 
    let
	preds = [ (pred, pp_loc)
		| (inst, avail) <- fmToList avails,
		  pred <- get_preds inst avail,
		  let pp_loc = pprInstLoc (instLoc inst)
		]
		-- Avails has all the superclasses etc (good)
		-- It also has all the intermediates of the deduction (good)
		-- It does not have duplicates (good)
		-- NB that (?x::t1) and (?x::t2) will be held separately in avails
		--    so that improve will see them separate

	-- For free Methods, we want to take predicates from their context,
	-- but for Methods that have been squished their context will already
	-- be in Avails, and we don't want duplicates.  Hence this rather
	-- horrid get_preds function
	get_preds inst IsFree = fdPredsOfInst inst
	get_preds inst other | isDict inst = [dictPred inst]
			     | otherwise   = []

	eqns = improve get_insts preds
	get_insts clas = classInstances inst_envs clas
     in
     if null eqns then
	returnM True
     else
	traceTc (ptext SLIT("Improve:") <+> vcat (map pprEquationDoc eqns))	`thenM_`
        mappM_ unify eqns	`thenM_`
	returnM False
  where
    unify ((qtvs, pairs), doc)
	 = addErrCtxt doc			$
	   tcInstTyVars (varSetElems qtvs)	`thenM` \ (_, _, tenv) ->
	   mapM_ (unif_pr tenv) pairs
    unif_pr tenv (ty1,ty2) =  unifyTauTy (substTy tenv ty1) (substTy tenv ty2)
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
	pprTrace "Interesting! Context reduction stack deeper than 8:" 
		 (nest 2 (pprStack stack))
    else (\x->x))
#endif
    go wanteds state
  where
    go []     state = returnM state
    go (w:ws) state = reduce (n+1, w:stack) try_me w state	`thenM` \ state' ->
		      go ws state'

    -- Base case: we're done!
reduce stack try_me wanted avails
    -- It's the same as an existing inst, or a superclass thereof
  | Just avail <- isAvailable avails wanted
  = if isLinearInst wanted then
	addLinearAvailable avails avail wanted	`thenM` \ (avails', wanteds') ->
	reduceList stack try_me wanteds' avails'
    else
	returnM avails		-- No op for non-linear things

  | otherwise
  = case try_me wanted of {

      KeepDictWithoutSCs -> addIrred NoSCs avails wanted

    ; DontReduceUnlessConstant ->    -- It's irreducible (or at least should not be reduced)
  				     -- First, see if the inst can be reduced to a constant in one step
	try_simple (addIrred AddSCs)	-- Assume want superclasses

    ; Free ->	-- It's free so just chuck it upstairs
  		-- First, see if the inst can be reduced to a constant in one step
	try_simple addFree

    ; ReduceMe want_scs ->	-- It should be reduced
	lookupInst wanted	      `thenM` \ lookup_result ->
	case lookup_result of
	    GenInst wanteds' rhs -> addIrred NoSCs avails wanted		`thenM` \ avails1 ->
				    reduceList stack try_me wanteds' avails1	`thenM` \ avails2 ->
				    addWanted want_scs avails2 wanted rhs wanteds'
		-- Experiment with temporarily doing addIrred *before* the reduceList, 
		-- which has the effect of adding the thing we are trying
		-- to prove to the database before trying to prove the things it
		-- needs.  See note [RECURSIVE DICTIONARIES]
		-- NB: we must not do an addWanted before, because that adds the
		--     superclasses too, and thaat can lead to a spurious loop; see
		--     the examples in [SUPERCLASS-LOOP]
		-- So we do an addIrred before, and then overwrite it afterwards with addWanted

	    SimpleInst rhs -> addWanted want_scs avails wanted rhs []

	    NoInstance ->    -- No such instance!
			     -- Add it and its superclasses
		    	     addIrred want_scs avails wanted
    }
  where
    try_simple do_this_otherwise
      = lookupInst wanted	  `thenM` \ lookup_result ->
	case lookup_result of
	    SimpleInst rhs -> addWanted AddSCs avails wanted rhs []
	    other	   -> do_this_otherwise avails wanted
\end{code}


\begin{code}
-------------------------
isAvailable :: Avails -> Inst -> Maybe Avail
isAvailable avails wanted = lookupFM avails wanted
	-- NB 1: the Ord instance of Inst compares by the class/type info
	--  *not* by unique.  So
	--	d1::C Int ==  d2::C Int

addLinearAvailable :: Avails -> Avail -> Inst -> TcM (Avails, [Inst])
addLinearAvailable avails avail wanted
	-- avails currently maps [wanted -> avail]
	-- Extend avails to reflect a neeed for an extra copy of avail

  | Just avail' <- split_avail avail
  = returnM (addToFM avails wanted avail', [])

  | otherwise
  = tcLookupId splitName			`thenM` \ split_id ->
    tcInstClassOp (instLoc wanted) split_id 
		  [linearInstType wanted]	`thenM` \ split_inst ->
    returnM (addToFM avails wanted (Linear 2 split_inst avail), [split_inst])

  where
    split_avail :: Avail -> Maybe Avail
	-- (Just av) if there's a modified version of avail that
	-- 	     we can use to replace avail in avails
	-- Nothing   if there isn't, so we need to create a Linear
    split_avail (Linear n i a)		    = Just (Linear (n+1) i a)
    split_avail (Given id used) | not used  = Just (Given id True)
				| otherwise = Nothing
    split_avail Irred			    = Nothing
    split_avail IsFree			    = Nothing
    split_avail other = pprPanic "addLinearAvailable" (ppr avail $$ ppr wanted $$ ppr avails)
		  
-------------------------
addFree :: Avails -> Inst -> TcM Avails
	-- When an Inst is tossed upstairs as 'free' we nevertheless add it
	-- to avails, so that any other equal Insts will be commoned up right
	-- here rather than also being tossed upstairs.  This is really just
	-- an optimisation, and perhaps it is more trouble that it is worth,
	-- as the following comments show!
	--
	-- NB: do *not* add superclasses.  If we have
	--	df::Floating a
	--	dn::Num a
	-- but a is not bound here, then we *don't* want to derive
	-- dn from df here lest we lose sharing.
	--
addFree avails free = returnM (addToFM avails free IsFree)

addWanted :: WantSCs -> Avails -> Inst -> LHsExpr TcId -> [Inst] -> TcM Avails
addWanted want_scs avails wanted rhs_expr wanteds
  = addAvailAndSCs want_scs avails wanted avail
  where
    avail | instBindingRequired wanted = Rhs rhs_expr wanteds
	  | otherwise		       = ASSERT( null wanteds ) NoRhs

addGiven :: Avails -> Inst -> TcM Avails
addGiven avails given = addAvailAndSCs AddSCs avails given (Given (instToId given) False)
	-- Always add superclasses for 'givens'
	--
	-- No ASSERT( not (given `elemFM` avails) ) because in an instance
	-- decl for Ord t we can add both Ord t and Eq t as 'givens', 
	-- so the assert isn't true

addIrred :: WantSCs -> Avails -> Inst -> TcM Avails
addIrred want_scs avails irred = ASSERT2( not (irred `elemFM` avails), ppr irred $$ ppr avails )
    	      		         addAvailAndSCs want_scs avails irred Irred

addAvailAndSCs :: WantSCs -> Avails -> Inst -> Avail -> TcM Avails
addAvailAndSCs want_scs avails inst avail
  | not (isClassDict inst) = return avails_with_inst
  | NoSCs <- want_scs	   = return avails_with_inst
  | otherwise		   = do { traceTc (text "addAvailAndSCs" <+> vcat [ppr inst, ppr deps])
				; addSCs is_loop avails_with_inst inst }
  where
    avails_with_inst = addToFM avails inst avail

    is_loop pred = any (`tcEqType` mkPredTy pred) dep_tys
			-- Note: this compares by *type*, not by Unique
    deps         = findAllDeps (unitVarSet (instToId inst)) avail
    dep_tys	 = map idType (varSetElems deps)

    findAllDeps :: IdSet -> Avail -> IdSet
    -- Find all the Insts that this one depends on
    -- See Note [SUPERCLASS-LOOP]
    -- Watch out, though.  Since the avails may contain loops 
    -- (see Note [RECURSIVE DICTIONARIES]), so we need to track the ones we've seen so far
    findAllDeps so_far (Rhs _ kids) = foldl find_all so_far kids
    findAllDeps so_far other	    = so_far

    find_all :: IdSet -> Inst -> IdSet
    find_all so_far kid
      | kid_id `elemVarSet` so_far	  = so_far
      | Just avail <- lookupFM avails kid = findAllDeps so_far' avail
      | otherwise			  = so_far'
      where
	so_far' = extendVarSet so_far kid_id	-- Add the new kid to so_far
	kid_id = instToId kid

addSCs :: (TcPredType -> Bool) -> Avails -> Inst -> TcM Avails
	-- Add all the superclasses of the Inst to Avails
	-- The first param says "dont do this because the original thing
	--	depends on this one, so you'd build a loop"
	-- Invariant: the Inst is already in Avails.

addSCs is_loop avails dict
  = do	{ sc_dicts <- newDictsAtLoc (instLoc dict) sc_theta'
	; foldlM add_sc avails (zipEqual "add_scs" sc_dicts sc_sels) }
  where
    (clas, tys) = getDictClassTys dict
    (tyvars, sc_theta, sc_sels, _) = classBigSig clas
    sc_theta' = substTheta (zipTopTvSubst tyvars tys) sc_theta

    add_sc avails (sc_dict, sc_sel)
      | is_loop (dictPred sc_dict) = return avails 	-- See Note [SUPERCLASS-LOOP 2]
      | is_given sc_dict 	   = return avails
      | otherwise		   = addSCs is_loop avails' sc_dict
      where
	sc_sel_rhs = mkHsDictApp (mkHsTyApp (L (instSpan dict) (HsVar sc_sel)) tys) [instToId dict]
	avails'    = addToFM avails sc_dict (Rhs sc_sel_rhs [dict])

    is_given :: Inst -> Bool
    is_given sc_dict = case lookupFM avails sc_dict of
			  Just (Given _ _) -> True	-- Given is cheaper than superclass selection
			  other		   -> False	
\end{code}

Note [SUPERCLASS-LOOP 2]
~~~~~~~~~~~~~~~~~~~~~~~~
But the above isn't enough.  Suppose we are *given* d1:Ord a,
and want to deduce (d2:C [a]) where

	class Ord a => C a where
	instance Ord [a] => C [a] where ...

Then we'll use the instance decl to deduce C [a] from Ord [a], and then add the
superclasses of C [a] to avails.  But we must not overwrite the binding
for Ord [a] (which is obtained from Ord a) with a superclass selection or we'll just
build a loop! 

Here's another variant, immortalised in tcrun020
	class Monad m => C1 m
	class C1 m => C2 m x
	instance C2 Maybe Bool
For the instance decl we need to build (C1 Maybe), and it's no good if
we run around and add (C2 Maybe Bool) and its superclasses to the avails 
before we search for C1 Maybe.

Here's another example 
 	class Eq b => Foo a b
	instance Eq a => Foo [a] a
If we are reducing
	(Foo [t] t)

we'll first deduce that it holds (via the instance decl).  We must not
then overwrite the Eq t constraint with a superclass selection!

At first I had a gross hack, whereby I simply did not add superclass constraints
in addWanted, though I did for addGiven and addIrred.  This was sub-optimal,
becuase it lost legitimate superclass sharing, and it still didn't do the job:
I found a very obscure program (now tcrun021) in which improvement meant the
simplifier got two bites a the cherry... so something seemed to be an Irred
first time, but reducible next time.

Now we implement the Right Solution, which is to check for loops directly 
when adding superclasses.  It's a bit like the occurs check in unification.


Note [RECURSIVE DICTIONARIES]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider 
    data D r = ZeroD | SuccD (r (D r));
    
    instance (Eq (r (D r))) => Eq (D r) where
        ZeroD     == ZeroD     = True
        (SuccD a) == (SuccD b) = a == b
        _         == _         = False;
    
    equalDC :: D [] -> D [] -> Bool;
    equalDC = (==);

We need to prove (Eq (D [])).  Here's how we go:

	d1 : Eq (D [])

by instance decl, holds if
	d2 : Eq [D []]
	where 	d1 = dfEqD d2

by instance decl of Eq, holds if
	d3 : D []
	where	d2 = dfEqList d3
		d1 = dfEqD d2

But now we can "tie the knot" to give

	d3 = d1
	d2 = dfEqList d3
	d1 = dfEqD d2

and it'll even run!  The trick is to put the thing we are trying to prove
(in this case Eq (D []) into the database before trying to prove its
contributing clauses.
	

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
tcSimplifyTop, tcSimplifyInteractive :: [Inst] -> TcM TcDictBinds
tcSimplifyTop wanteds
  = tc_simplify_top doc False {- Not interactive loop -} AddSCs wanteds
  where 
    doc = text "tcSimplifyTop"

tcSimplifyInteractive wanteds
  = tc_simplify_top doc True  {- Interactive loop -}     AddSCs wanteds
  where 
    doc = text "tcSimplifyTop"

-- The TcLclEnv should be valid here, solely to improve
-- error message generation for the monomorphism restriction
tc_simplify_top doc is_interactive want_scs wanteds
  = do	{ lcl_env <- getLclEnv
	; traceTc (text "tcSimplifyTop" <+> ppr (lclEnvElts lcl_env))

	; let try_me inst = ReduceMe want_scs
	; (frees, binds, irreds) <- simpleReduceLoop doc try_me wanteds

	; let
		-- All the non-std ones are definite errors
	    (stds, non_stds) = partition isStdClassTyVarDict irreds
    
		    -- Group by type variable
	    std_groups = equivClasses cmp_by_tyvar stds
    
		    -- Pick the ones which its worth trying to disambiguate
		    -- namely, the onese whose type variable isn't bound
		    -- up with one of the non-standard classes
	    (std_oks, std_bads)	= partition worth_a_try std_groups
	    worth_a_try group@(d:_) = not (non_std_tyvars `intersectsVarSet` tyVarsOfInst d)
	    non_std_tyvars		= unionVarSets (map tyVarsOfInst non_stds)
    
		    -- Collect together all the bad guys
	    bad_guys 	   = non_stds ++ concat std_bads
    	    (non_ips, bad_ips) = partition isClassDict bad_guys
    	    (ambigs, no_insts) = partition isTyVarDict non_ips
	    -- If the dict has no type constructors involved, it must be ambiguous,
	    -- except I suppose that another error with fundeps maybe should have
	    -- constrained those type variables

	-- Report definite errors
	; ASSERT( null frees )
	  groupErrs (addNoInstanceErrs Nothing []) no_insts
	; strangeTopIPErrs bad_ips

	-- Deal with ambiguity errors, but only if
	-- if there has not been an error so far:
	-- errors often give rise to spurious ambiguous Insts.
	-- For example:
	--   f = (*)	-- Monomorphic
	--   g :: Num a => a -> a
	--   g x = f x x
	-- Here, we get a complaint when checking the type signature for g,
	-- that g isn't polymorphic enough; but then we get another one when
	-- dealing with the (Num a) context arising from f's definition;
	-- we try to unify a with Int (to default it), but find that it's
	-- already been unified with the rigid variable from g's type sig
	; binds_ambig <- ifErrsM (returnM []) $
	    do	{ -- Complain about the ones that don't fall under
		  -- the Haskell rules for disambiguation
		  -- This group includes both non-existent instances
		  --	e.g. Num (IO a) and Eq (Int -> Int)
		  -- and ambiguous dictionaries
		  --	e.g. Num a
		  addTopAmbigErrs ambigs

		  -- Disambiguate the ones that look feasible
		; mappM (disambigGroup is_interactive) std_oks }

	; return (binds `unionBags` unionManyBags binds_ambig) }

----------------------------------
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
disambigGroup :: Bool	-- True <=> simplifying at top-level interactive loop
	      -> [Inst]	-- All standard classes of form (C a)
	      -> TcM TcDictBinds

disambigGroup is_interactive dicts
  |   any std_default_class classes 	-- Guaranteed all standard classes
  = 	-- THE DICTS OBEY THE DEFAULTABLE CONSTRAINT
	-- SO, TRY DEFAULT TYPES IN ORDER

	-- Failure here is caused by there being no type in the
	-- default list which can satisfy all the ambiguous classes.
	-- For example, if Real a is reqd, but the only type in the
	-- default list is Int.
    get_default_tys			`thenM` \ default_tys ->
    let
      try_default [] 	-- No defaults work, so fail
	= failM

      try_default (default_ty : default_tys)
	= tryTcLIE_ (try_default default_tys) $	-- If default_ty fails, we try
						-- default_tys instead
	  tcSimplifyDefault theta		`thenM` \ _ ->
	  returnM default_ty
        where
	  theta = [mkClassPred clas [default_ty] | clas <- classes]
    in
	-- See if any default works
    tryM (try_default default_tys)	`thenM` \ mb_ty ->
    case mb_ty of
	Left  _ 		-> bomb_out
	Right chosen_default_ty -> choose_default chosen_default_ty

  | otherwise 				-- No defaults
  = bomb_out

  where
    tyvar   = get_tv (head dicts)	-- Should be non-empty
    classes = map get_clas dicts

    std_default_class cls
      =  isNumericClass cls
      || (is_interactive && 
	  classKey cls `elem` [showClassKey, eqClassKey, ordClassKey])
	 	-- In interactive mode, we default Show a to Show ()
		-- to avoid graututious errors on "show []"

    choose_default default_ty	-- Commit to tyvar = default_ty
      =	-- Bind the type variable 
	unifyTauTy default_ty (mkTyVarTy tyvar)	`thenM_`
	-- and reduce the context, for real this time
	simpleReduceLoop (text "disambig" <+> ppr dicts)
		         reduceMe dicts			`thenM` \ (frees, binds, ambigs) ->
	WARN( not (null frees && null ambigs), ppr frees $$ ppr ambigs )
	warnDefault dicts default_ty			`thenM_`
	returnM binds

    bomb_out = addTopAmbigErrs dicts	`thenM_`
	       returnM emptyBag

get_default_tys
  = do 	{ mb_defaults <- getDefaultTys
	; case mb_defaults of
		Just tys -> return tys
		Nothing  -> 	-- No use-supplied default;
				-- use [Integer, Double]
			    do { integer_ty <- tcMetaTy integerTyConName
			       ; checkWiredInTyCon doubleTyCon
			       ; return [integer_ty, doubleTy] } }
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
  = tcInstTyVars tyvars			`thenM` \ (tvs, _, tenv) ->
	-- The main loop may do unification, and that may crash if 
	-- it doesn't see a TcTyVar, so we have to instantiate. Sigh
	-- ToDo: what if two of them do get unified?
    newDicts DerivOrigin (substTheta tenv theta)	`thenM` \ wanteds ->
    simpleReduceLoop doc reduceMe wanteds		`thenM` \ (frees, _, irreds) ->
    ASSERT( null frees )			-- reduceMe never returns Free

    doptM Opt_AllowUndecidableInstances		`thenM` \ undecidable_ok ->
    let
 	tv_set      = mkVarSet tvs

 	(bad_insts, ok_insts) = partition is_bad_inst irreds
	is_bad_inst dict 
	   = let pred = dictPred dict	-- reduceMe squashes all non-dicts
	     in isEmptyVarSet (tyVarsOfPred pred)
		  -- Things like (Eq T) are bad
	     || (not undecidable_ok && not (isTyVarClassPred pred))
		  -- The returned dictionaries should be of form (C a b)
		  -- 	(where a, b are type variables).  
		  -- We allow non-tyvar dicts if we had -fallow-undecidable-instances,
		  -- but note that risks non-termination in the 'deriving' context-inference
		  -- fixpoint loop.   It is useful for situations like
		  --	data Min h a = E | M a (h a)
		  -- which gives the instance decl
		  --	instance (Eq a, Eq (h a)) => Eq (Min h a)
  
	simpl_theta = map dictPred ok_insts
	weird_preds = [pred | pred <- simpl_theta
			    , not (tyVarsOfPred pred `subVarSet` tv_set)]  
	  -- Check for a bizarre corner case, when the derived instance decl should
	  -- have form 	instance C a b => D (T a) where ...
	  -- Note that 'b' isn't a parameter of T.  This gives rise to all sorts
	  -- of problems; in particular, it's hard to compare solutions for
	  -- equality when finding the fixpoint.  So I just rule it out for now.
  
	rev_env = zipTopTvSubst tvs (mkTyVarTys tyvars)
		-- This reverse-mapping is a Royal Pain, 
		-- but the result should mention TyVars not TcTyVars
    in
   
    addNoInstanceErrs Nothing [] bad_insts		`thenM_`
    mapM_ (addErrTc . badDerivedPred) weird_preds	`thenM_`
    checkAmbiguity tvs simpl_theta tv_set		`thenM_`
    returnM (substTheta rev_env simpl_theta)
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
  = newDicts DefaultOrigin theta		`thenM` \ wanteds ->
    simpleReduceLoop doc reduceMe wanteds	`thenM` \ (frees, _, irreds) ->
    ASSERT( null frees )	-- try_me never returns Free
    addNoInstanceErrs Nothing []  irreds 	`thenM_`
    if null irreds then
	returnM ()
    else
	failM
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
groupErrs :: ([Inst] -> TcM ())	-- Deal with one group
	  -> [Inst]		-- The offending Insts
          -> TcM ()
-- Group together insts with the same origin
-- We want to report them together in error messages

groupErrs report_err [] 
  = returnM ()
groupErrs report_err (inst:insts) 
  = do_one (inst:friends)		`thenM_`
    groupErrs report_err others

  where
	-- (It may seem a bit crude to compare the error messages,
	--  but it makes sure that we combine just what the user sees,
	--  and it avoids need equality on InstLocs.)
   (friends, others) = partition is_friend insts
   loc_msg	     = showSDoc (pprInstLoc (instLoc inst))
   is_friend friend  = showSDoc (pprInstLoc (instLoc friend)) == loc_msg
   do_one insts = addInstCtxt (instLoc (head insts)) (report_err insts)
		-- Add location and context information derived from the Insts

-- Add the "arising from..." part to a message about bunch of dicts
addInstLoc :: [Inst] -> Message -> Message
addInstLoc insts msg = msg $$ nest 2 (pprInstLoc (instLoc (head insts)))

plural [x] = empty
plural xs  = char 's'

addTopIPErrs :: [Name] -> [Inst] -> TcM ()
addTopIPErrs bndrs [] 
  = return ()
addTopIPErrs bndrs ips
  = addErrTcM (tidy_env, mk_msg tidy_ips)
  where
    (tidy_env, tidy_ips) = tidyInsts ips
    mk_msg ips = vcat [sep [ptext SLIT("Implicit parameters escape from the monomorphic top-level binding(s) of"),
			    pprBinders bndrs <> colon],
		       nest 2 (vcat (map ppr_ip ips)),
		       monomorphism_fix]
    ppr_ip ip = pprPred (dictPred ip) <+> pprInstLoc (instLoc ip)

strangeTopIPErrs :: [Inst] -> TcM ()
strangeTopIPErrs dicts	-- Strange, becuase addTopIPErrs should have caught them all
  = groupErrs report tidy_dicts
  where
    (tidy_env, tidy_dicts) = tidyInsts dicts
    report dicts = addErrTcM (tidy_env, mk_msg dicts)
    mk_msg dicts = addInstLoc dicts (ptext SLIT("Unbound implicit parameter") <> 
				     plural tidy_dicts <+> pprDictsTheta tidy_dicts)

addNoInstanceErrs :: Maybe SDoc	-- Nothing => top level
				-- Just d => d describes the construct
		  -> [Inst]	-- What is given by the context or type sig
		  -> [Inst]	-- What is wanted
		  -> TcM ()	
addNoInstanceErrs mb_what givens [] 
  = returnM ()
addNoInstanceErrs mb_what givens dicts
  =	-- Some of the dicts are here because there is no instances
	-- and some because there are too many instances (overlap)
    tcGetInstEnvs	`thenM` \ inst_envs ->
    let
    	(tidy_env1, tidy_givens) = tidyInsts givens
	(tidy_env2, tidy_dicts)  = tidyMoreInsts tidy_env1 dicts

	-- Run through the dicts, generating a message for each
	-- overlapping one, but simply accumulating all the 
	-- no-instance ones so they can be reported as a group
	(overlap_doc, no_inst_dicts) = foldl check_overlap (empty, []) tidy_dicts
    	check_overlap (overlap_doc, no_inst_dicts) dict 
	  | not (isClassDict dict) = (overlap_doc, dict : no_inst_dicts)
	  | otherwise
	  = case lookupInstEnv inst_envs clas tys of
		-- The case of exactly one match and no unifiers means
		-- a successful lookup.  That can't happen here, becuase
		-- dicts only end up here if they didn't match in Inst.lookupInst
#ifdef DEBUG
		([m],[]) -> pprPanic "addNoInstanceErrs" (ppr dict)
#endif
		([], _)  -> (overlap_doc, dict : no_inst_dicts) 	-- No match
		res	 -> (mk_overlap_msg dict res $$ overlap_doc, no_inst_dicts)
	  where
	    (clas,tys) = getDictClassTys dict
    in
	
	-- Now generate a good message for the no-instance bunch
    mk_probable_fix tidy_env2 no_inst_dicts	`thenM` \ (tidy_env3, probable_fix) ->
    let
    	no_inst_doc | null no_inst_dicts = empty
		    | otherwise = vcat [addInstLoc no_inst_dicts heading, probable_fix]
	heading | null givens = ptext SLIT("No instance") <> plural no_inst_dicts <+> 
				ptext SLIT("for") <+> pprDictsTheta no_inst_dicts
		| otherwise   = sep [ptext SLIT("Could not deduce") <+> pprDictsTheta no_inst_dicts,
				     nest 2 $ ptext SLIT("from the context") <+> pprDictsTheta tidy_givens]
    in
	-- And emit both the non-instance and overlap messages
    addErrTcM (tidy_env3, no_inst_doc $$ overlap_doc)
  where
    mk_overlap_msg dict (matches, unifiers)
      = vcat [	addInstLoc [dict] ((ptext SLIT("Overlapping instances for") 
					<+> pprPred (dictPred dict))),
    		sep [ptext SLIT("Matching instances") <> colon,
    		     nest 2 (vcat [pprInstances ispecs, pprInstances unifiers])],
		ASSERT( not (null matches) )
		if not (isSingleton matches)
    		then 	-- Two or more matches
		     empty
    		else 	-- One match, plus some unifiers
		ASSERT( not (null unifiers) )
		parens (vcat [ptext SLIT("The choice depends on the instantiation of") <+>
	    		         quotes (pprWithCommas ppr (varSetElems (tyVarsOfInst dict))),
			      ptext SLIT("Use -fallow-incoherent-instances to use the first choice above")])]
      where
    	ispecs = [ispec | (_, ispec) <- matches]

    mk_probable_fix tidy_env dicts	
      = returnM (tidy_env, sep [ptext SLIT("Probable fix:"), nest 2 (vcat fixes)])
      where
	fixes = add_ors (fix1 ++ fix2)

    	fix1 = case mb_what of
		 Nothing   -> []	-- Top level
		 Just what -> -- Nested (type signatures, instance decls)
			      [ sep [ ptext SLIT("add") <+> pprDictsTheta dicts,
			        ptext SLIT("to the") <+> what] ]

    	fix2 | null instance_dicts = []
	     | otherwise	   = [ ptext SLIT("add an instance declaration for")
				       <+> pprDictsTheta instance_dicts ]
	instance_dicts = [d | d <- dicts, isClassDict d, not (isTyVarDict d)]
		-- Insts for which it is worth suggesting an adding an instance declaration
		-- Exclude implicit parameters, and tyvar dicts

	add_ors :: [SDoc] -> [SDoc]	-- The empty case should not happen
	add_ors []      = [ptext SLIT("[No suggested fixes]")]	-- Strange
	add_ors (f1:fs) = f1 : map (ptext SLIT("or") <+>) fs

addTopAmbigErrs dicts
-- Divide into groups that share a common set of ambiguous tyvars
  = mapM report (equivClasses cmp [(d, tvs_of d) | d <- tidy_dicts])
  where
    (tidy_env, tidy_dicts) = tidyInsts dicts

    tvs_of :: Inst -> [TcTyVar]
    tvs_of d = varSetElems (tyVarsOfInst d)
    cmp (_,tvs1) (_,tvs2) = tvs1 `compare` tvs2
    
    report :: [(Inst,[TcTyVar])] -> TcM ()
    report pairs@((inst,tvs) : _)	-- The pairs share a common set of ambiguous tyvars
	= mkMonomorphismMsg tidy_env tvs	`thenM` \ (tidy_env, mono_msg) ->
	  setSrcSpan (instLocSrcSpan (instLoc inst)) $
		-- the location of the first one will do for the err message
	  addErrTcM (tidy_env, msg $$ mono_msg)
	where
	  dicts = map fst pairs
	  msg = sep [text "Ambiguous type variable" <> plural tvs <+> 
			  pprQuotedList tvs <+> in_msg,
		     nest 2 (pprDictsInFull dicts)]
	  in_msg = text "in the constraint" <> plural dicts <> colon


mkMonomorphismMsg :: TidyEnv -> [TcTyVar] -> TcM (TidyEnv, Message)
-- There's an error with these Insts; if they have free type variables
-- it's probably caused by the monomorphism restriction. 
-- Try to identify the offending variable
-- ASSUMPTION: the Insts are fully zonked
mkMonomorphismMsg tidy_env inst_tvs
  = findGlobals (mkVarSet inst_tvs) tidy_env	`thenM` \ (tidy_env, docs) ->
    returnM (tidy_env, mk_msg docs)
  where
    mk_msg []   = ptext SLIT("Probable fix: add a type signature that fixes these type variable(s)")
			-- This happens in things like
			--	f x = show (read "foo")
			-- whre monomorphism doesn't play any role
    mk_msg docs = vcat [ptext SLIT("Possible cause: the monomorphism restriction applied to the following:"),
			nest 2 (vcat docs),
			monomorphism_fix
		       ]
monomorphism_fix :: SDoc
monomorphism_fix = ptext SLIT("Probable fix:") <+> 
		   (ptext SLIT("give these definition(s) an explicit type signature")
		    $$ ptext SLIT("or use -fno-monomorphism-restriction"))
    
warnDefault dicts default_ty
  = doptM Opt_WarnTypeDefaults  `thenM` \ warn_flag ->
    addInstCtxt (instLoc (head dicts)) (warnTc warn_flag warn_msg)
  where
	-- Tidy them first
    (_, tidy_dicts) = tidyInsts dicts
    warn_msg  = vcat [ptext SLIT("Defaulting the following constraint(s) to type") <+>
				quotes (ppr default_ty),
		      pprDictsInFull tidy_dicts]

-- Used for the ...Thetas variants; all top level
badDerivedPred pred
  = vcat [ptext SLIT("Can't derive instances where the instance context mentions"),
	  ptext SLIT("type variables that are not data type parameters"),
	  nest 2 (ptext SLIT("Offending constraint:") <+> ppr pred)]

reduceDepthErr n stack
  = vcat [ptext SLIT("Context reduction stack overflow; size =") <+> int n,
	  ptext SLIT("Use -fcontext-stack20 to increase stack size to (e.g.) 20"),
	  nest 4 (pprStack stack)]

pprStack stack = vcat (map pprInstInFull stack)
\end{code}
