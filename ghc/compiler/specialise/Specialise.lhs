%
% (c) The GRASP/AQUA Project, Glasgow University, 1993-1996
%
\section[Specialise]{Stamping out overloading, and (optionally) polymorphism}

\begin{code}
module Specialise ( specProgram ) where

#include "HsVersions.h"

import MkId		( mkUserLocal )
import Id		( Id, DictVar, idType, mkTemplateLocals,

			  getIdSpecialisation, setIdSpecialisation, isSpecPragmaId,

			  IdSet, mkIdSet, addOneToIdSet, intersectIdSets, isEmptyIdSet, 
			         emptyIdSet, unionIdSets, minusIdSet, unitIdSet, elementOfIdSet,

			  IdEnv, mkIdEnv, lookupIdEnv, addOneToIdEnv, delOneFromIdEnv
			)

import Type		( Type, mkTyVarTy, splitSigmaTy, instantiateTy, isDictTy,
			  tyVarsOfType, tyVarsOfTypes, applyTys, mkForAllTys
			)
import TyCon		( TyCon )
import TyVar		( TyVar, mkTyVar, mkSysTyVar,
			  TyVarSet, mkTyVarSet, isEmptyTyVarSet, intersectTyVarSets,
			   	    elementOfTyVarSet, unionTyVarSets, emptyTyVarSet,
				    minusTyVarSet,
			  TyVarEnv, mkTyVarEnv, delFromTyVarEnv
			)
import Kind		( mkBoxedTypeKind )
import CoreSyn
import FreeVars		( exprFreeVars, exprFreeTyVars )
import PprCore		()	-- Instances 
import Name		( NamedThing(..), getSrcLoc, mkSysLocalName, isLocallyDefined )
import SrcLoc		( noSrcLoc )
import SpecEnv		( addToSpecEnv, lookupSpecEnv, specEnvValues )

import UniqSupply	( UniqSupply,
			  UniqSM, initUs, thenUs, returnUs, getUnique, mapUs
			)
import Unique		( mkAlphaTyVarUnique )
import FiniteMap
import Maybes		( MaybeErr(..), maybeToBool, catMaybes )
import Bag
import List		( partition )
import Util		( zipEqual )
import Outputable


infixr 9 `thenSM`
\end{code}

%************************************************************************
%*									*
\subsection[notes-Specialise]{Implementation notes [SLPJ, Aug 18 1993]}
%*									*
%************************************************************************

These notes describe how we implement specialisation to eliminate
overloading.

The specialisation pass works on Core
syntax, complete with all the explicit dictionary application,
abstraction and construction as added by the type checker.  The
existing type checker remains largely as it is.

One important thought: the {\em types} passed to an overloaded
function, and the {\em dictionaries} passed are mutually redundant.
If the same function is applied to the same type(s) then it is sure to
be applied to the same dictionary(s)---or rather to the same {\em
values}.  (The arguments might look different but they will evaluate
to the same value.)

Second important thought: we know that we can make progress by
treating dictionary arguments as static and worth specialising on.  So
we can do without binding-time analysis, and instead specialise on
dictionary arguments and no others.

The basic idea
~~~~~~~~~~~~~~
Suppose we have

	let f = <f_rhs>
	in <body>

and suppose f is overloaded.

STEP 1: CALL-INSTANCE COLLECTION

We traverse <body>, accumulating all applications of f to types and
dictionaries.

(Might there be partial applications, to just some of its types and
dictionaries?  In principle yes, but in practice the type checker only
builds applications of f to all its types and dictionaries, so partial
applications could only arise as a result of transformation, and even
then I think it's unlikely.  In any case, we simply don't accumulate such
partial applications.)

There's a choice of whether to collect details of all *polymorphic* functions
or simply all *overloaded* ones.  How to sort this out?
  Pass in a predicate on the function to say if it is "interesting"?
  This is dependent on the user flags: SpecialiseOverloaded
				       SpecialiseUnboxed
				       SpecialiseAll

STEP 2: EQUIVALENCES

So now we have a collection of calls to f:
	f t1 t2 d1 d2
	f t3 t4 d3 d4
	...
Notice that f may take several type arguments.  To avoid ambiguity, we
say that f is called at type t1/t2 and t3/t4.

We take equivalence classes using equality of the *types* (ignoring
the dictionary args, which as mentioned previously are redundant).

STEP 3: SPECIALISATION

For each equivalence class, choose a representative (f t1 t2 d1 d2),
and create a local instance of f, defined thus:

	f@t1/t2 = <f_rhs> t1 t2 d1 d2

f_rhs presumably has some big lambdas and dictionary lambdas, so lots
of simplification will now result.  However we don't actually *do* that
simplification.  Rather, we leave it for the simplifier to do.  If we
*did* do it, though, we'd get more call instances from the specialised
RHS.  We can work out what they are by instantiating the call-instance
set from f's RHS with the types t1, t2.

Add this new id to f's IdInfo, to record that f has a specialised version.

Before doing any of this, check that f's IdInfo doesn't already
tell us about an existing instance of f at the required type/s.
(This might happen if specialisation was applied more than once, or
it might arise from user SPECIALIZE pragmas.)

Recursion
~~~~~~~~~
Wait a minute!  What if f is recursive?  Then we can't just plug in
its right-hand side, can we?

But it's ok.  The type checker *always* creates non-recursive definitions
for overloaded recursive functions.  For example:

	f x = f (x+x)		-- Yes I know its silly

becomes

	f a (d::Num a) = let p = +.sel a d
			 in
			 letrec fl (y::a) = fl (p y y)
			 in
			 fl

We still have recusion for non-overloaded functions which we
speciailise, but the recursive call should get specialised to the
same recursive version.


Polymorphism 1
~~~~~~~~~~~~~~

All this is crystal clear when the function is applied to *constant
types*; that is, types which have no type variables inside.  But what if
it is applied to non-constant types?  Suppose we find a call of f at type
t1/t2.  There are two possibilities:

(a) The free type variables of t1, t2 are in scope at the definition point
of f.  In this case there's no problem, we proceed just as before.  A common
example is as follows.  Here's the Haskell:

	g y = let f x = x+x
	      in f y + f y

After typechecking we have

	g a (d::Num a) (y::a) = let f b (d'::Num b) (x::b) = +.sel b d' x x
				in +.sel a d (f a d y) (f a d y)

Notice that the call to f is at type type "a"; a non-constant type.
Both calls to f are at the same type, so we can specialise to give:

	g a (d::Num a) (y::a) = let f@a (x::a) = +.sel a d x x
				in +.sel a d (f@a y) (f@a y)


(b) The other case is when the type variables in the instance types
are *not* in scope at the definition point of f.  The example we are
working with above is a good case.  There are two instances of (+.sel a d),
but "a" is not in scope at the definition of +.sel.  Can we do anything?
Yes, we can "common them up", a sort of limited common sub-expression deal.
This would give:

	g a (d::Num a) (y::a) = let +.sel@a = +.sel a d
				    f@a (x::a) = +.sel@a x x
				in +.sel@a (f@a y) (f@a y)

This can save work, and can't be spotted by the type checker, because
the two instances of +.sel weren't originally at the same type.

Further notes on (b)

* There are quite a few variations here.  For example, the defn of
  +.sel could be floated ouside the \y, to attempt to gain laziness.
  It certainly mustn't be floated outside the \d because the d has to
  be in scope too.

* We don't want to inline f_rhs in this case, because
that will duplicate code.  Just commoning up the call is the point.

* Nothing gets added to +.sel's IdInfo.

* Don't bother unless the equivalence class has more than one item!

Not clear whether this is all worth it.  It is of course OK to
simply discard call-instances when passing a big lambda.

Polymorphism 2 -- Overloading
~~~~~~~~~~~~~~
Consider a function whose most general type is

	f :: forall a b. Ord a => [a] -> b -> b

There is really no point in making a version of g at Int/Int and another
at Int/Bool, because it's only instancing the type variable "a" which
buys us any efficiency. Since g is completely polymorphic in b there
ain't much point in making separate versions of g for the different
b types.

That suggests that we should identify which of g's type variables
are constrained (like "a") and which are unconstrained (like "b").
Then when taking equivalence classes in STEP 2, we ignore the type args
corresponding to unconstrained type variable.  In STEP 3 we make
polymorphic versions.  Thus:

	f@t1/ = /\b -> <f_rhs> t1 b d1 d2

We do this.


Dictionary floating
~~~~~~~~~~~~~~~~~~~
Consider this

	f a (d::Num a) = let g = ...
			 in
			 ...(let d1::Ord a = Num.Ord.sel a d in g a d1)...

Here, g is only called at one type, but the dictionary isn't in scope at the
definition point for g.  Usually the type checker would build a
definition for d1 which enclosed g, but the transformation system
might have moved d1's defn inward.  Solution: float dictionary bindings
outwards along with call instances.

Consider

	f x = let g p q = p==q
		  h r s = (r+s, g r s)
	      in
	      h x x


Before specialisation, leaving out type abstractions we have

	f df x = let g :: Eq a => a -> a -> Bool
		     g dg p q = == dg p q
		     h :: Num a => a -> a -> (a, Bool)
		     h dh r s = let deq = eqFromNum dh
				in (+ dh r s, g deq r s)
	      in
	      h df x x

After specialising h we get a specialised version of h, like this:

		    h' r s = let deq = eqFromNum df
			     in (+ df r s, g deq r s)

But we can't naively make an instance for g from this, because deq is not in scope
at the defn of g.  Instead, we have to float out the (new) defn of deq
to widen its scope.  Notice that this floating can't be done in advance -- it only
shows up when specialisation is done.

User SPECIALIZE pragmas
~~~~~~~~~~~~~~~~~~~~~~~
Specialisation pragmas can be digested by the type checker, and implemented
by adding extra definitions along with that of f, in the same way as before

	f@t1/t2 = <f_rhs> t1 t2 d1 d2

Indeed the pragmas *have* to be dealt with by the type checker, because
only it knows how to build the dictionaries d1 and d2!  For example

	g :: Ord a => [a] -> [a]
	{-# SPECIALIZE f :: [Tree Int] -> [Tree Int] #-}

Here, the specialised version of g is an application of g's rhs to the
Ord dictionary for (Tree Int), which only the type checker can conjure
up.  There might not even *be* one, if (Tree Int) is not an instance of
Ord!  (All the other specialision has suitable dictionaries to hand
from actual calls.)

Problem.  The type checker doesn't have to hand a convenient <f_rhs>, because
it is buried in a complex (as-yet-un-desugared) binding group.
Maybe we should say

	f@t1/t2 = f* t1 t2 d1 d2

where f* is the Id f with an IdInfo which says "inline me regardless!".
Indeed all the specialisation could be done in this way.
That in turn means that the simplifier has to be prepared to inline absolutely
any in-scope let-bound thing.


Again, the pragma should permit polymorphism in unconstrained variables:

	h :: Ord a => [a] -> b -> b
	{-# SPECIALIZE h :: [Int] -> b -> b #-}

We *insist* that all overloaded type variables are specialised to ground types,
(and hence there can be no context inside a SPECIALIZE pragma).
We *permit* unconstrained type variables to be specialised to
	- a ground type
	- or left as a polymorphic type variable
but nothing in between.  So

	{-# SPECIALIZE h :: [Int] -> [c] -> [c] #-}

is *illegal*.  (It can be handled, but it adds complication, and gains the
programmer nothing.)


SPECIALISING INSTANCE DECLARATIONS
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider

	instance Foo a => Foo [a] where
		...
	{-# SPECIALIZE instance Foo [Int] #-}

The original instance decl creates a dictionary-function
definition:

	dfun.Foo.List :: forall a. Foo a -> Foo [a]

The SPECIALIZE pragma just makes a specialised copy, just as for
ordinary function definitions:

	dfun.Foo.List@Int :: Foo [Int]
	dfun.Foo.List@Int = dfun.Foo.List Int dFooInt

The information about what instance of the dfun exist gets added to
the dfun's IdInfo in the same way as a user-defined function too.


Automatic instance decl specialisation?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Can instance decls be specialised automatically?  It's tricky.
We could collect call-instance information for each dfun, but
then when we specialised their bodies we'd get new call-instances
for ordinary functions; and when we specialised their bodies, we might get
new call-instances of the dfuns, and so on.  This all arises because of
the unrestricted mutual recursion between instance decls and value decls.

Still, there's no actual problem; it just means that we may not do all
the specialisation we could theoretically do.

Furthermore, instance decls are usually exported and used non-locally,
so we'll want to compile enough to get those specialisations done.

Lastly, there's no such thing as a local instance decl, so we can
survive solely by spitting out *usage* information, and then reading that
back in as a pragma when next compiling the file.  So for now,
we only specialise instance decls in response to pragmas.


SPITTING OUT USAGE INFORMATION
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

To spit out usage information we need to traverse the code collecting
call-instance information for all imported (non-prelude?) functions
and data types. Then we equivalence-class it and spit it out.

This is done at the top-level when all the call instances which escape
must be for imported functions and data types.

*** Not currently done ***


Partial specialisation by pragmas
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
What about partial specialisation:

	k :: (Ord a, Eq b) => [a] -> b -> b -> [a]
	{-# SPECIALIZE k :: Eq b => [Int] -> b -> b -> [a] #-}

or even

	{-# SPECIALIZE k :: Eq b => [Int] -> [b] -> [b] -> [a] #-}

Seems quite reasonable.  Similar things could be done with instance decls:

	instance (Foo a, Foo b) => Foo (a,b) where
		...
	{-# SPECIALIZE instance Foo a => Foo (a,Int) #-}
	{-# SPECIALIZE instance Foo b => Foo (Int,b) #-}

Ho hum.  Things are complex enough without this.  I pass.


Requirements for the simplifer
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The simplifier has to be able to take advantage of the specialisation.

* When the simplifier finds an application of a polymorphic f, it looks in
f's IdInfo in case there is a suitable instance to call instead.  This converts

	f t1 t2 d1 d2 	===>   f_t1_t2

Note that the dictionaries get eaten up too!

* Dictionary selection operations on constant dictionaries must be
  short-circuited:

	+.sel Int d	===>  +Int

The obvious way to do this is in the same way as other specialised
calls: +.sel has inside it some IdInfo which tells that if it's applied
to the type Int then it should eat a dictionary and transform to +Int.

In short, dictionary selectors need IdInfo inside them for constant
methods.

* Exactly the same applies if a superclass dictionary is being
  extracted:

	Eq.sel Int d   ===>   dEqInt

* Something similar applies to dictionary construction too.  Suppose
dfun.Eq.List is the function taking a dictionary for (Eq a) to
one for (Eq [a]).  Then we want

	dfun.Eq.List Int d	===> dEq.List_Int

Where does the Eq [Int] dictionary come from?  It is built in
response to a SPECIALIZE pragma on the Eq [a] instance decl.

In short, dfun Ids need IdInfo with a specialisation for each
constant instance of their instance declaration.

All this uses a single mechanism: the SpecEnv inside an Id


What does the specialisation IdInfo look like?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The SpecEnv of an Id maps a list of types (the template) to an expression

	[Type]  |->  Expr

For example, if f has this SpecInfo:

	[Int, a]  ->  \d:Ord Int. f' a

it means that we can replace the call

	f Int t  ===>  (\d. f' t)

This chucks one dictionary away and proceeds with the
specialised version of f, namely f'.


What can't be done this way?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
There is no way, post-typechecker, to get a dictionary for (say)
Eq a from a dictionary for Eq [a].  So if we find

	==.sel [t] d

we can't transform to

	eqList (==.sel t d')

where
	eqList :: (a->a->Bool) -> [a] -> [a] -> Bool

Of course, we currently have no way to automatically derive
eqList, nor to connect it to the Eq [a] instance decl, but you
can imagine that it might somehow be possible.  Taking advantage
of this is permanently ruled out.

Still, this is no great hardship, because we intend to eliminate
overloading altogether anyway!



A note about non-tyvar dictionaries
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Some Ids have types like

	forall a,b,c. Eq a -> Ord [a] -> tau

This seems curious at first, because we usually only have dictionary
args whose types are of the form (C a) where a is a type variable.
But this doesn't hold for the functions arising from instance decls,
which sometimes get arguements with types of form (C (T a)) for some
type constructor T.

Should we specialise wrt this compound-type dictionary?  We used to say
"no", saying:
	"This is a heuristic judgement, as indeed is the fact that we 
	specialise wrt only dictionaries.  We choose *not* to specialise
	wrt compound dictionaries because at the moment the only place
	they show up is in instance decls, where they are simply plugged
	into a returned dictionary.  So nothing is gained by specialising
	wrt them."

But it is simpler and more uniform to specialise wrt these dicts too;
and in future GHC is likely to support full fledged type signatures 
like
	f ;: Eq [(a,b)] => ...


%************************************************************************
%*									*
\subsubsection{The new specialiser}
%*									*
%************************************************************************

Our basic game plan is this.  For let(rec) bound function
	f :: (C a, D c) => (a,b,c,d) -> Bool

* Find any specialised calls of f, (f ts ds), where 
  ts are the type arguments t1 .. t4, and
  ds are the dictionary arguments d1 .. d2.

* Add a new definition for f1 (say):

	f1 = /\ b d -> (..body of f..) t1 b t3 d d1 d2

  Note that we abstract over the unconstrained type arguments.

* Add the mapping

	[t1,b,t3,d]  |->  \d1 d2 -> f1 b d

  to the specialisations of f.  This will be used by the
  simplifier to replace calls 
		(f t1 t2 t3 t4) da db
  by
		(\d1 d1 -> f1 t2 t4) da db

  All the stuff about how many dictionaries to discard, and what types
  to apply the specialised function to, are handled by the fact that the
  SpecEnv contains a template for the result of the specialisation.

We don't build *partial* specialisations for f.  For example:

  f :: Eq a => a -> a -> Bool
  {-# SPECIALISE f :: (Eq b, Eq c) => (b,c) -> (b,c) -> Bool #-}

Here, little is gained by making a specialised copy of f.
There's a distinct danger that the specialised version would
first build a dictionary for (Eq b, Eq c), and then select the (==) 
method from it!  Even if it didn't, not a great deal is saved.

We do, however, generate polymorphic, but not overloaded, specialisations:

  f :: Eq a => [a] -> b -> b -> b
  {#- SPECIALISE f :: [Int] -> b -> b -> b #-}

Hence, the invariant is this: 

	*** no specialised version is overloaded ***


%************************************************************************
%*									*
\subsubsection{The exported function}
%*									*
%************************************************************************

\begin{code}
specProgram :: UniqSupply -> [CoreBinding] -> [CoreBinding]
specProgram us binds
  = initSM us (go binds 	`thenSM` \ (binds', uds') ->
	       returnSM (dumpAllDictBinds uds' binds')
	      )
  where
    go []	    = returnSM ([], emptyUDs)
    go (bind:binds) = go binds 		`thenSM` \ (binds', uds) ->
		      specBind bind uds	`thenSM` \ (bind', uds') ->
		      returnSM (bind' ++ binds', uds')
\end{code}

%************************************************************************
%*									*
\subsubsection{@specExpr@: the main function}
%*									*
%************************************************************************

\begin{code}
specExpr :: CoreExpr -> SpecM (CoreExpr, UsageDetails)

---------------- First the easy cases --------------------
specExpr e@(Var _)    = returnSM (e, emptyUDs)
specExpr e@(Lit _)    = returnSM (e, emptyUDs)
specExpr e@(Con _ _)  = returnSM (e, emptyUDs)
specExpr e@(Prim _ _) = returnSM (e, emptyUDs)

specExpr (Note note body)
  = specExpr body 	`thenSM` \ (body', uds) ->
    returnSM (Note note body', uds)


---------------- Applications might generate a call instance --------------------
specExpr e@(App fun arg)
  = go fun [arg]
  where
    go (App fun arg) args = go fun (arg:args)
    go (Var f)       args = returnSM (e, mkCallUDs f args)
    go other	     args = specExpr other	`thenSM` \ (e', uds) ->
			    returnSM (foldl App e' args, uds)

---------------- Lambda/case require dumping of usage details --------------------
specExpr e@(Lam _ _)
  = specExpr body 	`thenSM` \ (body', uds) ->
    let
	(filtered_uds, body'') = dumpUDs bndrs uds body'
    in
    returnSM (foldr Lam body'' bndrs, filtered_uds)
  where
    (bndrs, body) = go [] e

	-- More efficient to collect a group of binders together all at once
    go bndrs (Lam bndr e) = go (bndr:bndrs) e
    go bndrs e            = (reverse bndrs, e)


specExpr (Case scrut alts)
  = specExpr scrut	`thenSM` \ (scrut', uds_scrut) ->
    spec_alts alts	`thenSM` \ (alts', uds_alts) ->
    returnSM (Case scrut' alts', uds_scrut `plusUDs` uds_alts)
  where
    spec_alts (AlgAlts alts deflt)
	= mapAndCombineSM spec_alg_alt alts	`thenSM` \ (alts', uds1) ->
	  spec_deflt deflt			`thenSM` \ (deflt', uds2) ->
	  returnSM (AlgAlts alts' deflt', uds1 `plusUDs` uds2)

    spec_alts (PrimAlts alts deflt)
	= mapAndCombineSM spec_prim_alt alts	`thenSM` \ (alts', uds1) ->
	  spec_deflt deflt			`thenSM` \ (deflt', uds2) ->
	  returnSM (PrimAlts alts' deflt', uds1 `plusUDs` uds2)

    spec_alg_alt (con, args, rhs)
	= specExpr rhs		`thenSM` \ (rhs', uds) ->
	  let
	     (uds', rhs'') = dumpUDs (map ValBinder args) uds rhs'
	  in
	  returnSM ((con, args, rhs''), uds')

    spec_prim_alt (lit, rhs)
	= specExpr rhs		`thenSM` \ (rhs', uds) ->
	  returnSM ((lit, rhs'), uds)

    spec_deflt NoDefault = returnSM (NoDefault, emptyUDs)
    spec_deflt (BindDefault arg rhs)
	= specExpr rhs		`thenSM` \ (rhs', uds) ->
	  let
	     (uds', rhs'') = dumpUDs [ValBinder arg] uds rhs'
	  in
	  returnSM (BindDefault arg rhs'', uds')

---------------- Finally, let is the interesting case --------------------
specExpr (Let bind body)
  =	-- Deal with the body
    specExpr body				`thenSM` \ (body', body_uds) ->

	-- Deal with the bindings
    specBind bind body_uds			`thenSM` \ (binds', uds) ->

	-- All done
    returnSM (foldr Let body' binds', uds)
\end{code}

%************************************************************************
%*									*
\subsubsection{Dealing with a binding}
%*									*
%************************************************************************

\begin{code}
specBind :: CoreBinding
	 -> UsageDetails		-- Info on how the scope of the binding
	 -> SpecM ([CoreBinding],	-- New bindings
		   UsageDetails)	-- And info to pass upstream

specBind (NonRec bndr rhs) body_uds
  | isDictTy (idType bndr)
  =	-- It's a dictionary binding
	-- Pick it up and float it outwards.
    specExpr rhs				`thenSM` \ (rhs', rhs_uds) ->
    let
	all_uds = rhs_uds `plusUDs` addDictBind body_uds bndr rhs'
    in
    returnSM ([], all_uds)

  | isSpecPragmaId bndr
  = specExpr rhs				`thenSM` \ (rhs', rhs_uds) ->
    returnSM ([], rhs_uds `plusUDs` body_uds)

  | otherwise
  =   -- Deal with the RHS, specialising it according
      -- to the calls found in the body
    specDefn (calls body_uds) (bndr,rhs)	`thenSM` \ ((bndr',rhs'), spec_defns, spec_uds) ->
    let
	(all_uds, (dict_binds, dump_calls)) 
		= splitUDs [ValBinder bndr]
			   (body_uds `plusUDs` spec_uds)
			-- It's important that the `plusUDs` is this way round,
			-- because body_uds may bind dictionaries that are
			-- used in the calls passed to specDefn.  So the
			-- dictionary bindings in spec_uds may mention 
			-- dictionaries bound in body_uds.

        -- If we make specialisations then we Rec the whole lot together
        -- If not, leave it as a NonRec
        new_bind | null spec_defns = NonRec bndr' rhs'
                 | otherwise       = Rec ((bndr',rhs'):spec_defns)
    in
    returnSM ( new_bind : mkDictBinds dict_binds, all_uds )

specBind (Rec pairs) body_uds
  = mapSM (specDefn (calls body_uds)) pairs	`thenSM` \ stuff ->
    let
	(pairs', spec_defns_s, spec_uds_s) = unzip3 stuff
	spec_defns = concat spec_defns_s
	spec_uds   = plusUDList spec_uds_s

	(all_uds, (dict_binds, dump_calls)) 
		= splitUDs (map (ValBinder . fst) pairs)
			   (body_uds `plusUDs` spec_uds)
			-- See notes for non-rec case

        new_bind = Rec (spec_defns ++ 
			pairs'     ++ 
			[(d,r) | (d,r,_,_) <- dict_binds])
		-- We need to Rec together the dict_binds too, because they
		-- can be recursive; this happens when an overloaded function
		-- is used as a method in an instance declaration.
		-- (The particular program that showed this up was
		--  docon/source/auxil/DInteger.hs)
    in
    returnSM (	[new_bind], all_uds )
    
specDefn :: CallDetails			-- Info on how it is used in its scope
	 -> (Id, CoreExpr)		-- The thing being bound and its un-processed RHS
	 -> SpecM ((Id, CoreExpr),	-- The thing and its processed RHS
					-- 	the Id may now have specialisations attached
		   [(Id,CoreExpr)],	-- Extra, specialised bindings
		   UsageDetails		-- Stuff to fling upwards from the RHS and its
	    )				-- 	specialised versions

specDefn calls (fn, rhs)
	-- The first case is the interesting one
  |  n_tyvars == length rhs_tyvars	-- Rhs of fn's defn has right number of big lambdas
  && n_dicts  <= length rhs_bndrs	-- and enough dict args
  && not (null calls_for_me)		-- And there are some calls to specialise
  =   -- Specialise the body of the function
    specExpr body					`thenSM` \ (body', body_uds) ->
    let
	(float_uds, bound_uds@(dict_binds,_)) = splitUDs rhs_bndrs body_uds
    in

      -- Make a specialised version for each call in calls_for_me
    mapSM (spec_call bound_uds) calls_for_me		`thenSM` \ stuff ->
    let
	(spec_defns, spec_uds, spec_env_stuff) = unzip3 stuff

	fn'  = addIdSpecialisations fn spec_env_stuff
	rhs' = foldr Lam (mkDictLets dict_binds body') rhs_bndrs 
    in
    returnSM ((fn',rhs'), 
	      spec_defns, 
	      float_uds `plusUDs` plusUDList spec_uds)

  | otherwise	-- No calls or RHS doesn't fit our preconceptions
  = specExpr rhs			`thenSM` \ (rhs', rhs_uds) ->
    returnSM ((fn, rhs'), [], rhs_uds)
  
  where
    fn_type		 = idType fn
    (tyvars, theta, tau) = splitSigmaTy fn_type
    n_tyvars		 = length tyvars
    n_dicts		 = length theta

    (rhs_tyvars, rhs_ids, rhs_body) = collectBinders rhs
    rhs_dicts = take n_dicts rhs_ids
    rhs_bndrs = map TyBinder rhs_tyvars ++ map ValBinder rhs_dicts
    body      = mkValLam (drop n_dicts rhs_ids) rhs_body
		-- Glue back on the non-dict lambdas

    calls_for_me = case lookupFM calls fn of
			Nothing -> []
			Just cs -> fmToList cs

    ----------------------------------------------------------
	-- Specialise to one particular call pattern
    spec_call :: ProtoUsageDetails          -- From the original body, captured by
					    -- the dictionary lambdas
              -> ([Maybe Type], [DictVar])  -- Call instance
              -> SpecM ((Id,CoreExpr),    	  -- Specialised definition
	                UsageDetails,             -- Usage details from specialised body
                	([TyVar], [Type], CoreExpr))       -- Info for the Id's SpecEnv
    spec_call bound_uds (call_ts, call_ds)
      = ASSERT( length call_ts == n_tyvars && length call_ds == n_dicts )
		-- Calls are only recorded for properly-saturated applications
	
        -- Supppose the call is for f [Just t1, Nothing, Just t3, Nothing] [d1, d2]

		-- Construct the new binding
		-- 	f1 = /\ b d -> (..rhs of f..) t1 b t3 d d1 d2
		-- and the type of this binder
        let
	  mk_spec_ty Nothing   = newTyVarSM   `thenSM` \ tyvar ->
			         returnSM (Just tyvar, mkTyVarTy tyvar)
	  mk_spec_ty (Just ty) = returnSM (Nothing,    ty)
        in
        mapSM mk_spec_ty call_ts   `thenSM` \ stuff ->
        let
	   (maybe_spec_tyvars, spec_tys) = unzip stuff
           spec_tyvars = catMaybes maybe_spec_tyvars
	   spec_rhs    = mkTyLam spec_tyvars $
                         mkGenApp rhs (map TyArg spec_tys ++ map VarArg call_ds)
  	   spec_id_ty  = mkForAllTys spec_tyvars (instantiateTy ty_env tau)
	   ty_env      = mkTyVarEnv (zipEqual "spec_call" tyvars spec_tys)
	in

	newIdSM fn spec_id_ty		`thenSM` \ spec_f ->


		-- Construct the stuff for f's spec env
		--	[b,d] [t1,b,t3,d]  |->  \d1 d2 -> f1 b d
		-- The only awkward bit is that d1,d2 might well be global
		-- dictionaries, so it's tidier to make new local variables
		-- for the lambdas in the RHS, rather than lambda-bind the
		-- dictionaries themselves.
		--
		-- In fact we use the standard template locals, so that the
		-- they don't need to be "tidied" before putting in interface files
	let
	   arg_ds	 = mkTemplateLocals (map idType call_ds)
	   spec_env_rhs  = mkValLam arg_ds $
			   mkTyApp (Var spec_f) $
			   map mkTyVarTy spec_tyvars
           spec_env_info = (spec_tyvars, spec_tys, spec_env_rhs)
        in

		-- Specialise the UDs from f's RHS
	let
		-- Only the overloaded tyvars should be free in the uds
	   ty_env   = [ (rhs_tyvar,ty) 
		      | (rhs_tyvar, Just ty) <- zipEqual "specUDs1" rhs_tyvars call_ts
		      ]
	   dict_env = zipEqual "specUDs2" rhs_dicts call_ds
	in
        specUDs ty_env dict_env bound_uds			`thenSM` \ spec_uds ->

        returnSM ((spec_f, spec_rhs),
	          spec_uds,
		  spec_env_info
	)
\end{code}

%************************************************************************
%*									*
\subsubsection{UsageDetails and suchlike}
%*									*
%************************************************************************

\begin{code}
type FreeDicts = IdSet

data UsageDetails 
  = MkUD {
	dict_binds :: !(Bag DictBind),
			-- Floated dictionary bindings
			-- The order is important; 
			-- in ds1 `union` ds2, bindings in ds2 can depend on those in ds1
			-- (Remember, Bags preserve order in GHC.)
			-- The FreeDicts is the free vars of the RHS

	calls     :: !CallDetails
    }

type DictBind = (DictVar, CoreExpr, TyVarSet, FreeDicts)
			-- The FreeDicts are the free dictionaries (only)
			-- of the RHS of the dictionary bindings
			-- Similarly the TyVarSet

emptyUDs = MkUD { dict_binds = emptyBag, calls = emptyFM }

type ProtoUsageDetails = ([DictBind],
			  [(Id, [Maybe Type], [DictVar])]
			 )

------------------------------------------------------------			
type CallDetails  = FiniteMap Id CallInfo
type CallInfo     = FiniteMap [Maybe Type]	-- Nothing => unconstrained type argument
		              [DictVar]		-- Dict args
	-- The finite maps eliminate duplicates
	-- The list of types and dictionaries is guaranteed to
	-- match the type of f

callDetailsToList calls = [ (id,tys,dicts)
			  | (id,fm) <- fmToList calls,
		            (tys,dicts) <- fmToList fm
			  ]

listToCallDetails calls  = foldr (unionCalls . singleCall) emptyFM calls

unionCalls :: CallDetails -> CallDetails -> CallDetails
unionCalls c1 c2 = plusFM_C plusFM c1 c2

singleCall (id, tys, dicts) = unitFM id (unitFM tys dicts)

mkCallUDs f args 
  | null theta
  || length spec_tys /= n_tyvars
  || length dicts    /= n_dicts
  = emptyUDs	-- Not overloaded

  | otherwise
  = MkUD {dict_binds = emptyBag, 
	  calls = singleCall (f, spec_tys, dicts)
    }
  where
    (tyvars, theta, tau) = splitSigmaTy (idType f)
    constrained_tyvars   = foldr (unionTyVarSets . tyVarsOfTypes . snd) emptyTyVarSet theta 
    n_tyvars		 = length tyvars
    n_dicts		 = length theta

    spec_tys = [mk_spec_ty tv ty | (tv, TyArg ty) <- tyvars `zip` args]
    dicts    = [d | (_, VarArg d) <- theta `zip` (drop n_tyvars args)]
    
    mk_spec_ty tyvar ty | tyvar `elementOfTyVarSet` constrained_tyvars
			= Just ty
			| otherwise
			= Nothing

------------------------------------------------------------			
plusUDs :: UsageDetails -> UsageDetails -> UsageDetails
plusUDs (MkUD {dict_binds = db1, calls = calls1})
	(MkUD {dict_binds = db2, calls = calls2})
  = MkUD {dict_binds, calls}
  where
    dict_binds = db1    `unionBags`   db2 
    calls      = calls1 `unionCalls`  calls2

plusUDList = foldr plusUDs emptyUDs

mkDB dict rhs = (dict, rhs, db_ftvs, db_fvs)
	      where
		db_ftvs	= exprFreeTyVars rhs
		db_fvs  = exprFreeVars isLocallyDefined rhs

addDictBind uds dict rhs = uds { dict_binds = mkDB dict rhs `consBag` dict_binds uds }

dumpAllDictBinds (MkUD {dict_binds = dbs}) binds
  = foldrBag add binds dbs
  where
    add (dict,rhs,_,_) binds = NonRec dict rhs : binds

mkDictBinds :: [DictBind] -> [CoreBinding]
mkDictBinds = map (\(d,r,_,_) -> NonRec d r)

mkDictLets :: [DictBind] -> CoreExpr -> CoreExpr
mkDictLets dbs body = foldr mk body dbs
		    where
		      mk (d,r,_,_) e = Let (NonRec d r) e 

dumpUDs :: [CoreBinder]
	-> UsageDetails -> CoreExpr
	-> (UsageDetails, CoreExpr)
dumpUDs bndrs uds body
  = (free_uds, mkDictLets dict_binds body)
  where
    (free_uds, (dict_binds, _)) = splitUDs bndrs uds

splitUDs :: [CoreBinder]
	 -> UsageDetails
	 -> (UsageDetails,		-- These don't mention the binders
	     ProtoUsageDetails)		-- These do
	     
splitUDs bndrs uds@(MkUD {dict_binds = orig_dbs, 
			  calls      = orig_calls})

  = if isEmptyBag dump_dbs && null dump_calls then
   	-- Common case: binder doesn't affect floats
	(uds, ([],[]))	

    else
  	-- Binders bind some of the fvs of the floats
  	(MkUD {dict_binds = free_dbs, 
	       calls      = listToCallDetails free_calls},
	 (bagToList dump_dbs, dump_calls)
	)

  where
    tyvar_set    = mkTyVarSet [tv | TyBinder tv <- bndrs]
    id_set       = mkIdSet    [id | ValBinder id <- bndrs]

    (free_dbs, dump_dbs, dump_idset) 
	  = foldlBag dump_db (emptyBag, emptyBag, id_set) orig_dbs
		-- Important that it's foldl not foldr;
		-- we're accumulating the set of dumped ids in dump_set

	-- Filter out any calls that mention things that are being dumped
	-- Don't need to worry about the tyvars because the dicts will
	-- spot the captured ones; any fully polymorphic arguments will
	-- be Nothings in the call details
    orig_call_list = callDetailsToList orig_calls
    (dump_calls, free_calls) = partition captured orig_call_list
    captured (id,tys,dicts)  = any (`elementOfIdSet` dump_idset) (id:dicts)

    dump_db (free_dbs, dump_dbs, dump_idset) db@(dict, rhs, ftvs, fvs)
	|  isEmptyIdSet    (dump_idset `intersectIdSets`    fvs)
	&& isEmptyTyVarSet (tyvar_set  `intersectTyVarSets` ftvs)
	= (free_dbs `snocBag` db, dump_dbs, dump_idset)

	| otherwise	-- Dump it
	= (free_dbs, dump_dbs `snocBag` db,
	   dump_idset `addOneToIdSet` dict)
\end{code}

Given a type and value substitution, specUDs creates a specialised copy of
the given UDs

\begin{code}
specUDs :: [(TyVar,Type)] -> [(DictVar,DictVar)] -> ProtoUsageDetails -> SpecM UsageDetails
specUDs tv_env_list dict_env_list (dbs, calls)
  = specDBs dict_env_list dbs		`thenSM` \ (dict_env_list', dbs') ->
    let
	dict_env = mkIdEnv dict_env_list'
    in
    returnSM (MkUD { dict_binds = dbs',
	  	     calls      = listToCallDetails (map (inst_call dict_env) calls)
    })
  where
    bound_tyvars = mkTyVarSet (map fst tv_env_list)
    tv_env   = mkTyVarEnv tv_env_list	-- Doesn't change

    inst_call dict_env (id, tys, dicts) = (id, map inst_maybe_ty tys, 
				               map (lookupId dict_env) dicts)

    inst_maybe_ty Nothing   = Nothing
    inst_maybe_ty (Just ty) = Just (instantiateTy tv_env ty)

    specDBs dict_env []
	= returnSM (dict_env, emptyBag)
    specDBs dict_env ((dict, rhs, ftvs, fvs) : dbs)
	= newIdSM dict (instantiateTy tv_env (idType dict))	`thenSM` \ dict' ->
	  let
	    rhs'      = foldl App (foldr Lam rhs (t_bndrs ++ d_bndrs)) (t_args ++ d_args)
	    (t_bndrs, t_args) = unzip [(TyBinder tv, TyArg ty)  | (tv,ty) <- tv_env_list,
								   tv `elementOfTyVarSet` ftvs]
	    (d_bndrs, d_args) = unzip [(ValBinder d, VarArg d') | (d,d')  <- dict_env,
								   d `elementOfIdSet` fvs]
	    dict_env' = (dict,dict') : dict_env
	    ftvs' = tyVarsOfTypes [ty | TyArg ty <- t_args] `unionTyVarSets`
		    (ftvs `minusTyVarSet` bound_tyvars)
	    fvs'  = mkIdSet [d | VarArg d <- d_args] `unionIdSets`
		    (fvs `minusIdSet` mkIdSet [d | ValBinder d <- d_bndrs])
	  in
	  specDBs dict_env' dbs		`thenSM` \ (dict_env'', dbs') ->
	  returnSM ( dict_env'', (dict', rhs', ftvs', fvs') `consBag` dbs' )
\end{code}

%************************************************************************
%*									*
\subsubsection{Boring helper functions}
%*									*
%************************************************************************

\begin{code}
lookupId:: IdEnv Id -> Id -> Id
lookupId env id = case lookupIdEnv env id of
			Nothing  -> id
			Just id' -> id'

addIdSpecialisations id spec_stuff
  = (if not (null errs) then
	pprTrace "Duplicate specialisations" (vcat (map ppr errs))
     else \x -> x
    )
    setIdSpecialisation id new_spec_env
  where
    (new_spec_env, errs) = foldr add (getIdSpecialisation id, []) spec_stuff

    add (tyvars, tys, template) (spec_env, errs)
	= case addToSpecEnv True spec_env tyvars tys template of
		Succeeded spec_env' -> (spec_env', errs)
		Failed err 	    -> (spec_env, err:errs)

----------------------------------------
type SpecM a = UniqSM a

thenSM    = thenUs
returnSM  = returnUs
getUniqSM = getUnique
mapSM     = mapUs
initSM	  = initUs

mapAndCombineSM f []     = returnSM ([], emptyUDs)
mapAndCombineSM f (x:xs) = f x	`thenSM` \ (y, uds1) ->
			   mapAndCombineSM f xs	`thenSM` \ (ys, uds2) ->
			   returnSM (y:ys, uds1 `plusUDs` uds2)

newIdSM old_id new_ty
  = getUnique		`thenSM` \ uniq ->
    returnSM (mkUserLocal (getOccName old_id) 
			  uniq
			  new_ty
			  (getSrcLoc old_id)
    )

newTyVarSM
  = getUnique		`thenSM` \ uniq ->
    returnSM (mkSysTyVar uniq mkBoxedTypeKind)
\end{code}


		Old (but interesting) stuff about unboxed bindings
		~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

What should we do when a value is specialised to a *strict* unboxed value?

	map_*_* f (x:xs) = let h = f x
			       t = map f xs
			   in h:t

Could convert let to case:

	map_*_Int# f (x:xs) = case f x of h# ->
			      let t = map f xs
			      in h#:t

This may be undesirable since it forces evaluation here, but the value
may not be used in all branches of the body. In the general case this
transformation is impossible since the mutual recursion in a letrec
cannot be expressed as a case.

There is also a problem with top-level unboxed values, since our
implementation cannot handle unboxed values at the top level.

Solution: Lift the binding of the unboxed value and extract it when it
is used:

	map_*_Int# f (x:xs) = let h = case (f x) of h# -> _Lift h#
				  t = map f xs
			      in case h of
				 _Lift h# -> h#:t

Now give it to the simplifier and the _Lifting will be optimised away.

The benfit is that we have given the specialised "unboxed" values a
very simplep lifted semantics and then leave it up to the simplifier to
optimise it --- knowing that the overheads will be removed in nearly
all cases.

In particular, the value will only be evaluted in the branches of the
program which use it, rather than being forced at the point where the
value is bound. For example:

	filtermap_*_* p f (x:xs)
	  = let h = f x
		t = ...
	    in case p x of
		True  -> h:t
		False -> t
   ==>
	filtermap_*_Int# p f (x:xs)
	  = let h = case (f x) of h# -> _Lift h#
		t = ...
	    in case p x of
		True  -> case h of _Lift h#
			   -> h#:t
		False -> t

The binding for h can still be inlined in the one branch and the
_Lifting eliminated.


Question: When won't the _Lifting be eliminated?

Answer: When they at the top-level (where it is necessary) or when
inlining would duplicate work (or possibly code depending on
options). However, the _Lifting will still be eliminated if the
strictness analyser deems the lifted binding strict.

