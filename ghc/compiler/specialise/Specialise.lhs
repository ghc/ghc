%
% (c) The GRASP/AQUA Project, Glasgow University, 1993-1995
%
\section[Specialise]{Stamping out overloading, and (optionally) polymorphism}

\begin{code}
#include "HsVersions.h"

module Specialise (
	specProgram,
	initSpecData,

	SpecialiseData(..),
	FiniteMap, Bag

    ) where

import PlainCore
import SpecTyFuns

IMPORT_Trace
import Outputable	-- ToDo: these may be removable...
import Pretty

import AbsPrel		( liftDataCon, PrimOp(..), PrimKind -- for CCallOp
			  IF_ATTACK_PRAGMAS(COMMA tagOf_PrimOp)
			  IF_ATTACK_PRAGMAS(COMMA pprPrimOp)
			)
import AbsUniType
import Bag
import CmdLineOpts	( GlobalSwitch(..) )
import CoreLift		( mkLiftedId, liftExpr, bindUnlift, applyBindUnlifts )
import FiniteMap
import Id
import IdEnv
import IdInfo 		-- All of it
import InstEnv		( lookupClassInstAtSimpleType )
import Maybes		( catMaybes, firstJust, maybeToBool, Maybe(..) )
import TyVarEnv		-- ( growTyVarEnvList, nullTyVarEnv, TyVarEnv, TypeEnv(..) )
import UniqSet		-- All of it
import Util
import SplitUniq

infixr 9 `thenSM`
\end{code}

%************************************************************************
%*									*
\subsection[notes-Specialise]{Implementation notes [SLPJ, Aug 18 1993]}
%*									*
%************************************************************************

These notes describe how we implement specialisation to eliminate
overloading, and optionally to eliminate unboxed polymorphism, and
full polymorphism.

The specialisation pass is a partial evaluator which works on Core
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

(f_rhs presumably has some big lambdas and dictionary lambdas, so lots
of simplification will now result.)  Then we should recursively do
everything again.

The new id has its own unique, but its print-name (if exported) has
an explicit representation of the instance types t1/t2.

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

We still have recusion for non-overloadd functions which we
speciailise, but the recursive call should get speciailised to the
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

This seems pretty simple, and a Good Thing.

Polymorphism 3 -- Unboxed
~~~~~~~~~~~~~~

If we are speciailising at unboxed types we must speciailise
regardless of the overloading constraint.  In the exaple above it is
worth speciailising at types Int/Int#, Int/Bool# and a/Int#, Int#/Int#
etc.

Note that specialising an overloaded type at an uboxed type requires
an unboxed instance -- we cannot default to an unspecialised version!


Dictionary floating
~~~~~~~~~~~~~~~~~~~
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

DELICATE MATTER: the way we tell a dictionary binding is by looking to
see if it has a Dict type.  If the type has been "undictify'd", so that
it looks like a tuple, then the dictionary binding won't be floated, and
an opportunity to specialise might be lost.

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

In fact, matters are a little bit more complicated than this.
When we make one of these specialised instances, we are defining
a constant dictionary, and so we want immediate access to its constant
methods and superclasses.  Indeed, these constant methods and superclasses
must be in the IdInfo for the class selectors!  We need help from the 
typechecker to sort this out, perhaps by generating a separate IdInfo
for each.

Automatic instance decl specialisation?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Can instance decls be specialised automatically?  It's tricky.
We could collect call-instance information for each dfun, but
then when we specialised their bodies we'd get new call-instances
for ordinary functions; and when we specialised their bodies, we might get
new call-instances of the dfuns, and so on.  This all arises because of
the unrestricted mutual recursion between instance decls and value decls.

Furthermore, instance decls are usually exported and used non-locally,
so we'll want to compile enough to get those specialisations done.

Lastly, there's no such thing as a local instance decl, so we can
survive solely by spitting out *usage* information, and then reading that
back in as a pragma when next compiling the file.  So for now, 
we only specialise instance decls in response to pragmas.

That means that even if an instance decl ain't otherwise exported it 
needs to be spat out as with a SPECIALIZE pragma.  Furthermore, it needs
something to say which module defined the instance, so the usage info
can be fed into the right reqts info file.  Blegh.


SPECIAILISING DATA DECLARATIONS
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

With unboxed specialisation (or full specialisation) we also require
data types (and their constructors) to be speciailised on unboxed
type arguments.

In addition to normal call instances we gather TyCon call instances at
unboxed types, determine equivalence classes for the locally defined
TyCons and build speciailised data constructor Ids for each TyCon and
substitute these in the CoCon calls.

We need the list of local TyCons to partition the TyCon instance info.
We pass out a FiniteMap from local TyCons to Specialised Instances to
give to the interface and code genertors.

N.B. The specialised data constructors reference the original data
constructor and type constructor which do not have the updated
specialisation info attached.  Any specialisation info must be
extracted from the TyCon map returned.


SPITTING OUT USAGE INFORMATION
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

To spit out usage information we need to traverse the code collecting
call-instance information for all imported (non-prelude?) functions
and data types. Then we equivalence-class it and spit it out.

This is done at the top-level when all the call instances which escape
must be for imported functions and data types.


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


What does the specialisation IdInfo look like?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

	SpecInfo
		[Maybe UniType] -- Instance types
		Int		-- No of dicts to eat
		Id		-- Specialised version

For example, if f has this SpecInfo: 

	SpecInfo [Just t1, Nothing, Just t3] 2 f'

then

	f t1 t2 t3 d1 d2  ===>  f t2

The "Nothings" identify type arguments in which the specialised
version is polymorphic.

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


Mutter mutter
~~~~~~~~~~~~~
What about types/classes mentioned in SPECIALIZE pragmas spat out,
but not otherwise exported.  Even if they are exported, what about
their original names.  

Suggestion: use qualified names in pragmas, omitting module for
prelude and "this module".


Mutter mutter 2
~~~~~~~~~~~~~~~
Consider this

	f a (d::Num a) = let g = ...
			 in
			 ...(let d1::Ord a = Num.Ord.sel a d in g a d1)...

Here, g is only called at one type, but the dictionary isn't in scope at the
definition point for g.  Usually the type checker would build a
definition for d1 which enclosed g, but the transformation system
might have moved d1's defn inward.


Unboxed bindings
~~~~~~~~~~~~~~~~

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
very simple lifted semantics and then leave it up to the simplifier to
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



%************************************************************************
%*									*
\subsubsection[CallInstances]{@CallInstances@ data type}
%*									*
%************************************************************************

\begin{code}
type FreeVarsSet   = UniqSet Id
type FreeTyVarsSet = UniqSet TyVar

data CallInstance 
  = CallInstance 
		Id 			-- This Id; *new* ie *cloned* id
		[Maybe UniType]		-- Specialised at these types (*new*, cloned)
					-- Nothing => no specialisation on this type arg
					--	      is required (flag dependent).
		[PlainCoreArg]		-- And these dictionaries; all ValArgs
		FreeVarsSet		-- Free vars of the dict-args in terms of *new* ids
		(Maybe SpecInfo)	-- For specialisation with explicit SpecId
\end{code}

\begin{code}
pprCI :: CallInstance -> Pretty
pprCI (CallInstance id spec_tys dicts _ maybe_specinfo)
  = ppHang (ppCat [ppStr "Call inst for", ppr PprDebug id])
	 4 (ppAboves [ppCat (ppStr "types" : [pprMaybeTy PprDebug ty | ty <- spec_tys]),
		      case maybe_specinfo of
			Nothing -> ppCat (ppStr "dicts" : [ppr PprDebug dict | dict <- dicts])
		        Just (SpecInfo _ _ spec_id)
				-> ppCat [ppStr "Explicit SpecId", ppr PprDebug spec_id]
		     ])

isUnboxedCI :: CallInstance -> Bool
isUnboxedCI (CallInstance _ spec_tys _ _ _)
  = any isUnboxedDataType (catMaybes spec_tys)

isExplicitCI :: CallInstance -> Bool
isExplicitCI (CallInstance _ _ _ _ (Just _))
  = True
isExplicitCI (CallInstance _ _ _ _ Nothing)
  = False
\end{code}

Comparisons are based on the {\em types}, ignoring the dictionary args:

\begin{code}

cmpCI :: CallInstance -> CallInstance -> TAG_
cmpCI (CallInstance id1 tys1 _ _ _) (CallInstance id2 tys2 _ _ _) 
  = case cmpId id1 id2 of { EQ_ -> cmpUniTypeMaybeList tys1 tys2; other -> other }

cmpCI_tys :: CallInstance -> CallInstance -> TAG_
cmpCI_tys (CallInstance _ tys1 _ _ _) (CallInstance _ tys2 _ _ _)
  = cmpUniTypeMaybeList tys1 tys2

isCIofTheseIds :: [Id] -> CallInstance -> Bool
isCIofTheseIds ids (CallInstance ci_id _ _ _ _) = any (eqId ci_id) ids

singleCI :: Id -> [Maybe UniType] -> [PlainCoreArg] -> UsageDetails
singleCI id tys dicts
  = UsageDetails (unitBag (CallInstance id tys dicts fv_set Nothing))
		 emptyBag [] emptyUniqSet
  where
    fv_set = mkUniqSet (id : [dict | ValArg (CoVarAtom dict) <- dicts])

explicitCI :: Id -> [Maybe UniType] -> SpecInfo -> UsageDetails
explicitCI id tys specinfo
  = UsageDetails (unitBag call_inst) emptyBag [] emptyUniqSet
  where
    call_inst = CallInstance id tys dicts fv_set (Just specinfo)
    dicts  = panic "Specialise:explicitCI:dicts"
    fv_set = singletonUniqSet id

getCIs :: [Id] -> UsageDetails -> ([CallInstance], UsageDetails)
getCIs ids (UsageDetails cis tycon_cis dbs fvs)
  = let
	(cis_here, cis_not_here) = partitionBag (isCIofTheseIds ids) cis
	cis_here_list = bagToList cis_here
    in
    -- pprTrace "getCIs:"
    --     (ppHang (ppBesides [ppStr "{", ppr PprDebug ids, ppStr "}"])
    --		 4 (ppAboves (map pprCI cis_here_list)))
    (cis_here_list, UsageDetails cis_not_here tycon_cis dbs fvs)

dumpCIs :: Bag CallInstance	-- The call instances
	-> [Id]			-- Bound ids *new*
	-> Bag CallInstance	-- Kept call instances
dumpCIs cis bound_ids 
 = (if not (isEmptyBag cis_dict_bound_arg) then
        (if isEmptyBag unboxed_cis_dict_bound_arg
	 then (\ x y -> y) -- pprTrace "dumpCIs: bound dictionary arg ... \n"
	 else pprTrace "dumpCIs: bound dictionary arg ... WITH UNBOXED TYPES!\n")
	          (ppHang (ppBesides [ppStr "{", ppr PprDebug bound_ids, ppStr "}"])
   	                4 (ppAboves (map pprCI (bagToList cis_dump))))
    else id)
   cis_keep
 where
   (cis_dump, cis_keep) = partitionBag mentions_bound_ids cis

   mentions_bound_ids (CallInstance _ _ _ fv_set _) 
	= or [i `elementOfUniqSet` fv_set | i <- bound_ids]

   (cis_of_bound_id, cis_dict_bound_arg) = partitionBag (isCIofTheseIds bound_ids) cis_dump
   (unboxed_cis_dict_bound_arg, _)       = partitionBag isUnboxedCI cis_dict_bound_arg

\end{code}

Any call instances of a bound_id can be safely dumped, because any
recursive calls should be at the same instance as the parent instance.

   letrec f = /\a -> \x::a -> ...(f t x')...

Here, the type, t, at which f is used in its own RHS should be
just "a"; that is, the recursive call is at the same type as
the original call. That means that when specialising f at some
type, say Int#, we shouldn't find any *new* instances of f 
arising from specialising f's RHS.  The only instance we'll find
is another call of (f Int#).

ToDo: We should check this rather than just dumping them.

However, we do report any call instances which are mysteriously dumped
because they have a dictionary argument which is bound here ...

ToDo: Under what circumstances does this occur, if at all?

%************************************************************************
%*									*
\subsubsection[TyConInstances]{@TyConInstances@ data type}
%*									*
%************************************************************************

\begin{code}
data TyConInstance
  = TyConInstance TyCon			-- Type Constructor
		  [Maybe UniType]	-- Applied to these specialising types

cmpTyConI :: TyConInstance -> TyConInstance -> TAG_
cmpTyConI (TyConInstance tc1 tys1) (TyConInstance tc2 tys2) 
  = case cmpTyCon tc1 tc2 of { EQ_ -> cmpUniTypeMaybeList tys1 tys2; other -> other }

cmpTyConI_tys :: TyConInstance -> TyConInstance -> TAG_
cmpTyConI_tys (TyConInstance _ tys1) (TyConInstance _ tys2) 
  = cmpUniTypeMaybeList tys1 tys2

singleTyConI :: TyCon -> [Maybe UniType] -> UsageDetails
singleTyConI ty_con spec_tys 
  = UsageDetails emptyBag (unitBag (TyConInstance ty_con spec_tys)) [] emptyUniqSet

isTyConIofThisTyCon :: TyCon -> TyConInstance -> Bool
isTyConIofThisTyCon ty_con (TyConInstance inst_ty_con _) = eqTyCon ty_con inst_ty_con

isLocalSpecTyConI :: Bool -> TyConInstance -> Bool
isLocalSpecTyConI comp_prel (TyConInstance inst_ty_con _) = isLocalSpecTyCon comp_prel inst_ty_con

getLocalSpecTyConIs :: Bool -> UsageDetails -> ([TyConInstance], UsageDetails)
getLocalSpecTyConIs comp_prel (UsageDetails cis tycon_cis dbs fvs)
  = let
	(tycon_cis_local, tycon_cis_global)
	  = partitionBag (isLocalSpecTyConI comp_prel) tycon_cis
	tycon_cis_local_list = bagToList tycon_cis_local
    in
    (tycon_cis_local_list, UsageDetails cis tycon_cis_global dbs fvs)
\end{code}


%************************************************************************
%*									*
\subsubsection[UsageDetails]{@UsageDetails@ data type}
%*									*
%************************************************************************

\begin{code}
data UsageDetails
  = UsageDetails 
	(Bag CallInstance) 	-- The collection of call-instances
	(Bag TyConInstance) 	-- Constructor call-instances
	[DictBindDetails]	-- Dictionary bindings in data-dependence order!
	FreeVarsSet		-- Free variables (excl imported ones, incl top level) (cloned)
\end{code}

The DictBindDetails are fully processed; their call-instance information is
incorporated in the call-instances of the
UsageDetails which includes the DictBindDetails.  The free vars in a usage details
will *include* the binders of the DictBind details.

A @DictBindDetails@ contains bindings for dictionaries *only*.

\begin{code}
data DictBindDetails 
  = DictBindDetails 
	[Id]			-- Main binders, originally visible in scope of binding (cloned)
	PlainCoreBinding	-- Fully processed
	FreeVarsSet		-- Free in binding group (cloned)
	FreeTyVarsSet		-- Free in binding group
\end{code}

\begin{code}
emptyUDs    :: UsageDetails
unionUDs    :: UsageDetails -> UsageDetails -> UsageDetails
unionUDList :: [UsageDetails] -> UsageDetails

emptyUDs      = UsageDetails emptyBag emptyBag [] emptyUniqSet

unionUDs (UsageDetails cis1 tycon_cis1 dbs1 fvs1) (UsageDetails cis2 tycon_cis2 dbs2 fvs2) 
 = UsageDetails (unionBags cis1 cis2) (unionBags tycon_cis1 tycon_cis2)
	        (dbs1 ++ dbs2) (fvs1 `unionUniqSets` fvs2)
	-- The append here is really redundant, since the bindings don't
	-- scope over each other.  ToDo.

unionUDList = foldr unionUDs emptyUDs

singleFvUDs (CoVarAtom v) | not (isImportedId v)
 = UsageDetails emptyBag emptyBag [] (singletonUniqSet v)
singleFvUDs other
 = emptyUDs

singleConUDs con = UsageDetails emptyBag emptyBag [] (singletonUniqSet con)

dumpDBs :: [DictBindDetails] 
	-> [TyVar]		-- TyVars being bound (cloned)
	-> [Id]			-- Ids being bound (cloned)
	-> FreeVarsSet		-- Fvs of body
	-> ([PlainCoreBinding],	-- These ones have to go here
	    [DictBindDetails],	-- These can float further
	    [Id],		-- Incoming list + names of dicts bound here
	    FreeVarsSet		-- Incominf fvs + fvs of dicts bound here
	   )
dumpDBs [] bound_tyvars bound_ids fvs = ([], [], bound_ids, fvs)

dumpDBs ((db@(DictBindDetails dbinders dbind db_fvs db_ftv)):dbs) 
	bound_tyvars bound_ids fvs
  | or [i `elementOfUniqSet` db_fvs  | i <- bound_ids]
    ||
    or [tv `elementOfUniqSet` db_ftv | tv <- bound_tyvars]
  = let		-- Ha!  Dump it!
	(dbinds_here, dbs_outer, full_bound_ids, full_fvs)
	   = dumpDBs dbs bound_tyvars (dbinders ++ bound_ids) (db_fvs `unionUniqSets` fvs)
    in
    (dbind : dbinds_here, dbs_outer, full_bound_ids, full_fvs)

  | otherwise	-- This one can float out further
  = let
	(dbinds_here, dbs_outer, full_bound_ids, full_fvs)
	   = dumpDBs dbs bound_tyvars bound_ids fvs
    in
    (dbinds_here, db : dbs_outer, full_bound_ids, full_fvs)


     
dumpUDs :: UsageDetails
	-> [Id]			-- Ids which are just being bound; *new*
	-> [TyVar]		-- TyVars which are just being bound
	-> ([PlainCoreBinding],	-- Bindings from UsageDetails which mention the ids
	    UsageDetails)	-- The above bindings removed, and
				-- any call-instances which mention the ids dumped too

dumpUDs (UsageDetails cis tycon_cis dbs fvs) bound_ids tvs
  = let
	(dict_binds_here, dbs_outer, full_bound_ids, full_fvs) = dumpDBs dbs tvs bound_ids fvs
	cis_outer = dumpCIs cis full_bound_ids
	fvs_outer = full_fvs `minusUniqSet` (mkUniqSet full_bound_ids)
    in
    (dict_binds_here, UsageDetails cis_outer tycon_cis dbs_outer fvs_outer)
\end{code}

\begin{code}
addDictBinds :: [Id] -> PlainCoreBinding -> UsageDetails	-- Dict binding and RHS usage
	     -> UsageDetails	 				-- The usage to augment
	     -> UsageDetails
addDictBinds dbinders dbind (UsageDetails db_cis db_tycon_cis db_dbs db_fvs)
	 	            (UsageDetails cis    tycon_cis    dbs    fvs)
  = UsageDetails (db_cis `unionBags` cis)
		 (db_tycon_cis `unionBags` tycon_cis)
		 (db_dbs ++ [DictBindDetails dbinders dbind db_fvs db_ftvs] ++ dbs) 
		 fvs
  where
	-- The free tyvars of the dictionary bindings should really be
	-- gotten from the RHSs, but I'm pretty sure it's good enough just
	-- to look at the type of the dictionary itself.  
	-- Doing the proper job would entail keeping track of free tyvars as
	-- well as free vars, which would be a bore.
    db_ftvs = mkUniqSet (extractTyVarsFromTys (map getIdUniType dbinders))
\end{code}

%************************************************************************
%*									*
\subsection[cloning-binders]{The Specialising IdEnv and CloneInfo}
%*									*
%************************************************************************

@SpecIdEnv@ maps old Ids to their new "clone". There are three cases:

1) (NoLift CoLitAtom l) : an Id which is bound to a literal

2) (NoLift CoLitAtom l) : an Id bound to a "new" Id	      
   The new Id is a possibly-type-specialised clone of the original

3) Lifted lifted_id unlifted_id :

   This indicates that the original Id has been specialised to an
   unboxed value which must be lifted (see "Unboxed bindings" above)
     @unlifted_id@ is the unboxed clone of the original Id
     @lifted_id@ is a *lifted* version of the original Id

   When you lookup Ids which are Lifted, you have to insert a case
   expression to un-lift the value (done with @bindUnlift@)

   You also have to insert a case to lift the value in the binding
   (done with @liftExpr@)


\begin{code}
type SpecIdEnv = IdEnv CloneInfo

data CloneInfo
 = NoLift PlainCoreAtom	-- refers to cloned id or literal

 | Lifted Id		-- lifted, cloned id
	  Id		-- unlifted, cloned id

\end{code}

%************************************************************************
%*									*
\subsection[specialise-data]{Data returned by specialiser}
%*									*
%************************************************************************

\begin{code}
data SpecialiseData
 = SpecData Bool
		-- True <=> Specialisation performed
	    Bool
		-- False <=> Specialisation completed with errors

	    [TyCon]
		-- Local tycons declared in this module

	    [TyCon]
		-- Those in-scope data types for which we want to
		-- generate code for their constructors.
		-- Namely: data types declared in this module + 
		-- 	   any big tuples used in this module
		-- The initial (and default) value is the local tycons

	    (FiniteMap TyCon [[Maybe UniType]])
		-- TyCon specialisations to be generated
		-- We generate specialisations for data types defined
		-- in this module and any tuples used in this module
		-- The initial (and default) value is the specialisations
		-- requested by source-level SPECIALIZE data pragmas
		-- and _SPECIALISE_ pragmas in the interface files

	    (Bag (Id,[Maybe UniType]))
		-- Imported specialisation errors
	    (Bag (Id,[Maybe UniType]))
		-- Imported specialisation warnings
	    (Bag (TyCon,[Maybe UniType]))
		-- Imported TyCon specialisation errors

initSpecData local_tycons tycon_specs
 = SpecData False True local_tycons local_tycons tycon_specs emptyBag emptyBag emptyBag
\end{code}

ToDo[sansom]: Transformation data to process specialisation requests.

%************************************************************************
%*									*
\subsection[specProgram]{Specialising a core program}
%*									*
%************************************************************************

\begin{code}
specProgram :: (GlobalSwitch -> Bool)
	    -> SplitUniqSupply
	    -> [PlainCoreBinding]	-- input ...
	    -> SpecialiseData
	    -> ([PlainCoreBinding],	-- main result
		SpecialiseData)		-- result specialise data

specProgram sw_chker uniqs binds
	   (SpecData False _ local_tycons _ init_specs init_errs init_warn init_tyerrs)
  = case (initSM (specTyConsAndScope (specTopBinds binds)) sw_chker uniqs) of
      (final_binds, tycon_specs_list, 
	UsageDetails import_cis import_tycis _ fvs)
	 -> let
		used_conids   = filter isDataCon (uniqSetToList fvs)
		used_tycons   = map getDataConTyCon used_conids
		used_gen      = filter isLocalGenTyCon used_tycons
		gen_tycons    = setToList (mkSet local_tycons `union` mkSet used_gen)

		result_specs  = addListToFM_C (++) init_specs tycon_specs_list
 
		uniq_cis      = map head (equivClasses cmpCI (bagToList import_cis))
		cis_list      = [(id, tys) | CallInstance id tys _ _ _ <- uniq_cis]
	        (cis_unboxed, cis_other) = partition (isUnboxedSpecialisation . snd) cis_list
		cis_warn      = init_warn `unionBags` listToBag cis_other
		cis_errs      = init_errs `unionBags` listToBag cis_unboxed

		uniq_tycis    = map head (equivClasses cmpTyConI (bagToList import_tycis))
		tycis_unboxed = [(con, tys) | TyConInstance con tys <- uniq_tycis]
		tycis_errs    = init_tyerrs `unionBags` listToBag tycis_unboxed

		no_errs       = isEmptyBag cis_errs && isEmptyBag tycis_errs
				  && (not (sw_chker SpecialiseImports) || isEmptyBag cis_warn)
	    in
	    (final_binds,
	     SpecData True no_errs local_tycons gen_tycons result_specs
			           cis_errs cis_warn tycis_errs)

specProgram sw_chker uniqs binds (SpecData True _ _ _ _ _ _ _)
  = panic "Specialise:specProgram: specialiser called more than once"

-- It may be possible safely to call the specialiser more than once, 
-- but I am not sure there is any benefit in doing so (Patrick)

-- ToDo: What about unfoldings performed after specialisation ???
\end{code}

%************************************************************************
%*									*
\subsection[specTyConsAndScope]{Specialising data constructors within tycons}
%*									*
%************************************************************************

In the specialiser we just collect up the specialisations which will
be required. We don't create the specialised constructors in
Core. These are only introduced when we convert to StgSyn.

ToDo: Perhaps this should be done in CoreToStg to ensure no inconsistencies!

\begin{code}
specTyConsAndScope :: SpecM ([PlainCoreBinding], UsageDetails)
           	   -> SpecM ([PlainCoreBinding], [(TyCon,[[Maybe UniType]])], UsageDetails)

specTyConsAndScope scopeM
  = scopeM			`thenSM` \ (binds, scope_uds) ->
    getSwitchCheckerSM		`thenSM` \ sw_chkr ->
    let
       (tycons_cis, gotci_scope_uds)
         = getLocalSpecTyConIs (sw_chkr CompilingPrelude) scope_uds

       tycon_specs_list = collectTyConSpecs tycons_cis
    in
    (if sw_chkr SpecialiseTrace && not (null tycon_specs_list) then
	 pprTrace "Specialising TyCons:\n"
	          (ppAboves [ if not (null specs) then
		                  ppHang (ppCat [(ppr PprDebug tycon), ppStr "at types"])
				       4 (ppAboves (map pp_specs specs))
			      else ppNil
			    | (tycon, specs) <- tycon_specs_list])
    else id) (
    returnSM (binds, tycon_specs_list, gotci_scope_uds)
    )
  where
    collectTyConSpecs []
      = []
    collectTyConSpecs tycons_cis@(TyConInstance tycon _ : _)
      = (tycon, tycon_specs) : collectTyConSpecs other_tycons_cis
      where
        (tycon_cis, other_tycons_cis) = partition (isTyConIofThisTyCon tycon) tycons_cis
        uniq_cis = map head (equivClasses cmpTyConI_tys tycon_cis)
	tycon_specs = [spec_tys | TyConInstance _ spec_tys <- uniq_cis]

    pp_specs specs = ppInterleave ppNil [pprMaybeTy PprDebug ty | ty <- specs]

    
{- UNUSED: create specialised constructors in Core

NB: this code may have some bitrot (Andy & Will 95/06)

specTyConsAndScope spec_tycons scopeM
  = fixSM (\ ~(_, _, _, rec_spec_infos) ->
      bindConIds cons_tospec rec_spec_infos (
        scopeM			`thenSM` \ (binds, scope_uds) ->
	let
	   (tycons_cis, gotci_scope_uds)
	     = getLocalSpecTyConIs (sw_chkr CompilingPrelude) scope_uds
        in
	mapAndUnzipSM (inst_tycon tycons_cis) spec_tycons
				`thenSM` \ (tycon_specs_list, spec_infoss) ->
        returnSM (binds, tycon_specs_list, gotci_scope_uds, concat spec_infoss)
      )

    ) `thenSM` \ (binds, tycon_specs_list, final_uds, spec_infos) ->
    returnSM (binds, tycon_specs_list, final_uds)

  where
    conss_tospec  = map getTyConDataCons spec_tycons
    cons_tospec   = concat conss_tospec

    inst_tycon tycons_cis tycon
      = mapSM mk_con_specs (getTyConDataCons tycon) `thenSM` \ spec_infos ->
	getSwitchCheckerSM			    `thenSM` \ sw_chkr ->
        (if sw_chkr SpecialiseTrace && not (null tycon_cis) then
	 pprTrace "Specialising:"
		  (ppHang (ppCat [ppr PprDebug tycon, ppStr "at types"])
		        4 (ppAboves (map pp_inst uniq_cis)))
        else id) (
	returnSM ((tycon, tycon_specs), spec_infos)
	)
      where
        tycon_cis = filter (isTyConIofThisTyCon tycon) tycons_cis
        uniq_cis  = map head (equivClasses cmpTyConI_tys tycon_cis)

	tycon_specs = [spec_tys | TyConInstance _ spec_tys <- uniq_cis]

        mk_con_specs con_id
          = mapSM (mk_con_spec con_id) uniq_cis
        mk_con_spec con_id (TyConInstance _ spec_tys)
	  = newSpecIds [con_id] spec_tys 0 copy_arity_info_and `thenSM` \ [spec_id] ->
	    returnSM (SpecInfo spec_tys 0 spec_id)

	copy_arity_info old new = addIdArity new (getDataConArity old)

        pp_inst (TyConInstance _ spec_tys)
	  = ppInterleave ppNil [pprMaybeTy PprDebug ty | ty <- spec_tys]
-}
\end{code}

%************************************************************************
%*									*
\subsection[specTopBinds]{Specialising top-level bindings}
%*									*
%************************************************************************

\begin{code}
specTopBinds :: [PlainCoreBinding] 
	     -> SpecM ([PlainCoreBinding], UsageDetails)

specTopBinds binds
  = spec_top_binds binds    `thenSM`  \ (binds, UsageDetails cis tycis dbind_details fvs) ->
    let
	-- Add bindings for floated dbinds and collect fvs
	-- In actual fact many of these bindings are dead code since dict
	-- arguments are dropped when a specialised call is created
	-- The simplifier should be able to cope ...

	(dbinders_s, dbinds, dfvs_s)
	   = unzip3 [(dbinders, dbind, dfvs) | DictBindDetails dbinders dbind dfvs _ <- dbind_details]

	full_fvs  = fvs `unionUniqSets` unionManyUniqSets dfvs_s
	fvs_outer = full_fvs `minusUniqSet` (mkUniqSet (concat dbinders_s))
    in
    returnSM (dbinds ++ binds, UsageDetails cis tycis [] fvs_outer)

  where
    spec_top_binds (first_bind:rest_binds)
      = specBindAndScope True {- top level -} first_bind (
	    spec_top_binds rest_binds `thenSM` \ (rest_binds, rest_uds) ->
	    returnSM (ItsABinds rest_binds, rest_uds)
        ) 			`thenSM` \ (first_binds, ItsABinds rest_binds, all_uds) ->
        returnSM (first_binds ++ rest_binds, all_uds)

    spec_top_binds []
      = returnSM ([], emptyUDs)
\end{code}

%************************************************************************
%*									*
\subsection[specExpr]{Specialising expressions}
%*									*
%************************************************************************

\begin{code}
specExpr :: PlainCoreExpr 
	 -> [PlainCoreArg]		-- The arguments: 
					--    TypeArgs are speced
					--    ValArgs are unprocessed
	 -> SpecM (PlainCoreExpr, 	-- Result expression with specialised versions installed
		   UsageDetails)	-- Details of usage of enclosing binders in the result
					-- expression.

specExpr (CoVar v) args
  = lookupId v			`thenSM` \ vlookup -> 
    case vlookup of
       Lifted vl vu
	     -> -- Binding has been lifted, need to extract un-lifted value
		-- NB: a function binding will never be lifted => args always null
		--     i.e. no call instance required or call to be constructed
		ASSERT (null args)
		returnSM (bindUnlift vl vu (CoVar vu), singleFvUDs (CoVarAtom vl))

       NoLift vatom@(CoVarAtom new_v)
	     -> mapSM specArg args		`thenSM` \ arg_info ->
		mkCallInstance v new_v arg_info	`thenSM` \ uds ->
    		mkCall new_v arg_info		`thenSM` \ call ->
    		returnSM (call, uds)

specExpr expr@(CoLit _) null_args
  = ASSERT (null null_args)
    returnSM (expr, emptyUDs)

specExpr (CoCon con tys args) null_args
  = ASSERT (null null_args)
    mapSM specTy tys 			`thenSM` \ tys ->
    mapAndUnzip3SM specAtom args	`thenSM` \ (args, args_uds_s, unlifts) ->
    mkTyConInstance con tys		`thenSM` \ con_uds ->
    returnSM (applyBindUnlifts unlifts (CoCon con tys args),
	      unionUDList args_uds_s `unionUDs` con_uds)

{- UNUSED: create specialised constructors in CoCon
specExpr (CoCon con tys args) null_args
  = ASSERT (null null_args)
    mapSM specTy tys 		`thenSM` \ tys ->
    mapAndUnzipSM specAtom args	`thenSM` \ (args, args_uds_s) ->
    mkTyConInstance con tys	`thenSM` \ con_con ->
    lookupId con		`thenSM` \ con ->
    mkConstrCall con tys	`thenSM` \ ~(spec_con, spec_tys) ->
    returnSM (CoCon spec_con spec_tys args,
	      unionUDList args_uds_s `unionUDs` con_uds)
-}

specExpr (CoPrim op@(CCallOp str is_asm may_gc arg_tys res_ty) tys args) null_args
  = ASSERT (null null_args)
    ASSERT (null tys)
    mapSM specTy arg_tys		`thenSM` \ arg_tys ->
    specTy res_ty			`thenSM` \ res_ty ->
    mapAndUnzip3SM specAtom args	`thenSM` \ (args, args_uds_s, unlifts) ->
    returnSM (applyBindUnlifts unlifts (CoPrim (CCallOp str is_asm may_gc arg_tys res_ty) tys args), 
	      unionUDList args_uds_s)

specExpr (CoPrim prim tys args) null_args
  = ASSERT (null null_args)
    mapSM specTy tys	 		`thenSM` \ tys ->
    mapAndUnzip3SM specAtom args	`thenSM` \ (args, args_uds_s, unlifts) ->
    -- specPrimOp prim tys		`thenSM` \ (prim, tys, prim_uds) ->
    returnSM (applyBindUnlifts unlifts (CoPrim prim tys args),
	      unionUDList args_uds_s {-`unionUDs` prim_uds-} )

{- ToDo: specPrimOp

specPrimOp :: PrimOp
	   -> [UniType]
	   -> SpecM (PrimOp,
		     [UniType],
	 	     UsageDetails)

-- Checks that PrimOp can handle (possibly unboxed) tys passed
--   and/or chooses PrimOp specialised to any unboxed tys
-- Errors are dealt with by returning a PrimOp call instance
--   which will result in a cis_errs message

-- ToDo: Deal with checkSpecTyApp for CoPrim in CoreLint
-}


specExpr (CoApp fun arg) args
  = 	-- Arg is passed on unprocessed
    specExpr fun (ValArg arg : args)	`thenSM` \ (expr,uds) ->
    returnSM (expr, uds)

specExpr (CoTyApp fun ty) args
  =	-- Spec the tyarg and pass it on
    specTy ty 				`thenSM` \ ty ->
    specExpr fun (TypeArg ty : args)

specExpr (CoLam bound_ids body)	args
  = specLam bound_ids body args

specExpr (CoTyLam tyvar body) (TypeArg ty : args)
  =	-- Type lambda with argument; argument already spec'd
    bindTyVar tyvar ty (
	specExpr body args
    )

specExpr (CoTyLam tyvar body) []
  = 	-- No arguments
    cloneTyVarSM tyvar 		`thenSM` \ new_tyvar ->
    bindTyVar tyvar (mkTyVarTy new_tyvar) (
	specExpr body []	`thenSM` \ (body, body_uds) ->
	let
	    (binds_here, final_uds) = dumpUDs body_uds [] [new_tyvar]
        in
	returnSM (CoTyLam new_tyvar (mkCoLetsNoUnboxed binds_here body), final_uds)
    )

specExpr (CoCase scrutinee alts) args
  = specExpr scrutinee []		`thenSM` \ (scrutinee, scrut_uds) ->
    specAlts alts scrutinee_type args	`thenSM` \ (alts, alts_uds) ->
    returnSM (CoCase scrutinee alts, scrut_uds `unionUDs`  alts_uds)
  where
    scrutinee_type = typeOfCoreExpr scrutinee


specExpr (CoLet bind body) args
  = specBindAndScope False {- not top level -} bind (
	specExpr body args 	`thenSM` \ (body, body_uds) ->
	returnSM (ItsAnExpr body, body_uds)
    )				`thenSM` \ (binds, ItsAnExpr body, all_uds) ->
    returnSM (mkCoLetsNoUnboxed binds body, all_uds)

specExpr (CoSCC cc expr) args
  = specExpr expr []		`thenSM` \ (expr, expr_uds) ->
    mapAndUnzip3SM specArg args	`thenSM` \ (args, args_uds_s, unlifts) -> 
    let
	scc_expr
	  = if squashableDictishCcExpr cc expr -- can toss the _scc_
	    then expr
	    else CoSCC cc expr
    in
    returnSM (applyBindUnlifts unlifts (applyToArgs scc_expr args),
	      unionUDList args_uds_s `unionUDs` expr_uds)

-- ToDo:DPH: add stuff here!
\end{code}

%************************************************************************
%*									*
\subsubsection{Specialising a lambda}
%*									*
%************************************************************************

\begin{code}
specLam :: [Id] -> PlainCoreExpr -> [PlainCoreArg]
	-> SpecM (PlainCoreExpr, UsageDetails)

specLam [] body args 
  = 	-- All lambdas saturated
    specExpr body args

specLam (binder:binders) body (ValArg arg : args)
  = 	-- Lambda with an unprocessed argument
    lookup_arg arg				`thenSM` \ arg ->
    bindId binder arg (
	specLam binders body args
    )
  where
    lookup_arg (CoLitAtom l) = returnSM (NoLift (CoLitAtom l))
    lookup_arg (CoVarAtom v) = lookupId v

specLam bound_ids body []
  = 	-- Lambda with no arguments
    specLambdaOrCaseBody bound_ids body [] 	`thenSM` \ (bound_ids, body, uds) ->
    returnSM (CoLam bound_ids body, uds)
\end{code}

\begin{code}
specLambdaOrCaseBody :: [Id]			-- The binders
		     -> PlainCoreExpr		-- The body
		     -> [PlainCoreArg]		-- Its args
		     -> SpecM ([Id],		-- New binders
			       PlainCoreExpr,	-- New body
			       UsageDetails)

specLambdaOrCaseBody bound_ids body args
 = cloneLambdaOrCaseBinders bound_ids 	`thenSM` \ (new_ids, clone_infos) ->
   bindIds bound_ids clone_infos (

	specExpr body args	`thenSM` \ (body, body_uds) ->

	let
	    -- Dump any dictionary bindings (and call instances) 
	    -- from the scope which mention things bound here
 	    (binds_here, final_uds) = dumpUDs body_uds new_ids []
	in
	returnSM (new_ids, mkCoLetsNoUnboxed binds_here body, final_uds)
   )

-- ToDo: Opportunity here to common-up dictionaries with same type,
-- thus avoiding recomputation.
\end{code}

A variable bound in a lambda or case is normally monomorphic so no
specialised versions will be required. This is just as well since we
do not know what code to specialise!

Unfortunately this is not always the case. For example a class Foo
with polymorphic methods gives rise to a dictionary with polymorphic
components as follows:

\begin{verbatim}
class Foo a where
  op1 :: a -> b -> a
  op2 :: a -> c -> a

instance Foo Int where
  op1 = op1Int
  op2 = op2Int

... op1 1 3# ...

==>

d.Foo.Int :: ( \/b . Int -> b -> Int, \/c . Int -> c -> Int )
d.Foo.Int = (op1_Int, op2_Int)

op1 = /\ a b -> \ dFoo -> case dFoo of (meth1, _) -> meth1 b
  
... op1 {Int Int#} d.Foo.Int 1 3# ...
\end{verbatim}

N.B. The type of the dictionary is not Hindley Milner!

Now we must specialise op1 at {* Int#} which requires a version of
meth1 at {Int#}. But since meth1 was extracted from a dictionary we do
not have access to its code to create the specialised version.


If we specialise on overloaded types as well we specialise op1 at
{Int Int#} d.Foo.Int:

op1_Int_Int# = case d.Foo.Int of (meth1, _) -> meth1 {Int#}

Though this is still invalid, after further simplification we get:

op1_Int_Int# = opInt1 {Int#}
  
Another round of specialisation will result in the specialised
version of op1Int being called directly.

For now we PANIC if a polymorphic lambda/case bound variable is found
in a call instance with an unboxed type. Other call instances, arising
from overloaded type arguments, are discarded since the unspecialised
version extracted from the method can be called as normal.

ToDo: Implement and test second round of specialisation.


%************************************************************************
%*									*
\subsubsection{Specialising case alternatives}
%*									*
%************************************************************************


\begin{code}
specAlts (CoAlgAlts alts deflt) scrutinee_ty args
  = mapSM specTy ty_args 			`thenSM` \ ty_args ->
    mapAndUnzipSM (specAlgAlt ty_args) alts	`thenSM` \ (alts, alts_uds_s) ->
    specDeflt deflt args			`thenSM` \ (deflt, deflt_uds) ->
    returnSM (CoAlgAlts alts deflt, 
	      unionUDList alts_uds_s `unionUDs` deflt_uds)

  where
    -- We use ty_args of scrutinee type to identify specialisation of alternatives
    (_, ty_args, _) = getUniDataTyCon scrutinee_ty

    specAlgAlt ty_args (con,binders,rhs) 
      = specLambdaOrCaseBody binders rhs args	`thenSM` \ (binders, rhs, rhs_uds) ->
	mkTyConInstance con ty_args    		`thenSM` \ con_uds ->
	returnSM ((con,binders,rhs), rhs_uds `unionUDs` con_uds)

{- UNUSED: creating specialised constructors in case alts
    specAlgAlt ty_args (con,binders,rhs)
      = specLambdaOrCaseBody binders rhs args	`thenSM` \ (binders, rhs, rhs_uds) ->
	mkTyConInstance con ty_args    		`thenSM` \ con_uds ->
	lookupId con			 	`thenSM` \ con ->
	mkConstrCall con ty_args		`thenSM` \ ~(spec_con, _) ->
	returnSM ((spec_con,binders,rhs), rhs_uds `unionUDs` con_uds)
-}

specAlts (CoPrimAlts alts deflt) scrutinee_ty args
  = mapAndUnzipSM specPrimAlt alts	`thenSM` \ (alts, alts_uds_s) ->
    specDeflt deflt args		`thenSM` \ (deflt, deflt_uds) ->
    returnSM (CoPrimAlts alts deflt, 
	      unionUDList alts_uds_s `unionUDs` deflt_uds)
  where
    specPrimAlt (lit,rhs) = specExpr rhs args	`thenSM` \ (rhs, uds) ->
			    returnSM ((lit,rhs), uds)


specDeflt CoNoDefault args = returnSM (CoNoDefault, emptyUDs)
specDeflt (CoBindDefault binder rhs) args 
 = specLambdaOrCaseBody [binder] rhs args	`thenSM` \ ([binder], rhs, uds) ->
   returnSM (CoBindDefault binder rhs, uds)
\end{code}


%************************************************************************
%*									*
\subsubsection{Specialising an atom}
%*									*
%************************************************************************

\begin{code}
specAtom :: PlainCoreAtom -> SpecM (PlainCoreAtom, UsageDetails,
				    PlainCoreExpr -> PlainCoreExpr)

specAtom (CoLitAtom lit)
  = returnSM (CoLitAtom lit, emptyUDs, id)

specAtom (CoVarAtom v)
  = lookupId v		`thenSM` \ vlookup ->
    case vlookup of 
      Lifted vl vu
	 -> returnSM (CoVarAtom vu, singleFvUDs (CoVarAtom vl), bindUnlift vl vu)

      NoLift vatom
	 -> returnSM (vatom, singleFvUDs vatom, id)


specArg :: PlainCoreArg -> SpecM (PlainCoreArg, UsageDetails,
				  PlainCoreExpr -> PlainCoreExpr)

specArg (ValArg arg)	-- unprocessed; spec the atom
  = specAtom arg	`thenSM` \ (arg, uds, unlift) ->
    returnSM (ValArg arg, uds, unlift)

specArg (TypeArg ty)	-- already speced; no action
  = returnSM (TypeArg ty, emptyUDs, id)
\end{code}


%************************************************************************
%*									*
\subsubsection{Specialising bindings}
%*									*
%************************************************************************

A classic case of when having a polymorphic recursive function would help!

\begin{code}
data BindsOrExpr = ItsABinds [PlainCoreBinding]
		 | ItsAnExpr PlainCoreExpr
\end{code}

\begin{code}
specBindAndScope 
	:: Bool					-- True <=> a top level group
	-> PlainCoreBinding			-- As yet unprocessed
	-> SpecM (BindsOrExpr, UsageDetails)	-- Something to do the scope of the bindings
	-> SpecM ([PlainCoreBinding],		-- Processed
		  BindsOrExpr, 			-- Combined result
		  UsageDetails)			-- Usage details of the whole lot

specBindAndScope is_top_level_group bind scopeM 
  = cloneLetrecBinders binders	`thenSM`	\ (new_binders, clone_infos) ->

	-- Two cases now: either this is a bunch of dictionaries, in
	-- which case we float them; or its a bunch of other values,
	-- in which case we see if they correspond to any
	-- call-instances we have in hand.

    if all (\id -> isDictTy (getIdUniType id) || isConstMethodId id) binders then
	-- Ha! A group of dictionary bindings, or constant methods.
	-- The reason for the latter is interesting.  Consider
	--
	--	dfun.Eq.Foo = /\a \ d -> ...
	--	
	--	constmeth1 = ...
	--	constmeth2 = ...
	--	dict = (constmeth1,constmeth2)
	--	
	--	...(dfun.Eq.Foo dict)...
	--
	-- Now, the defn of dict can't float above the constant-method
	-- decls, so the call-instance for dfun.Eq.Foo will be dropped.
	--
	-- Solution: float the constant methods in the same way as dictionaries
	--
	-- The other interesting bit is the test for dictionary-hood.
	-- Constant dictionaries, like dict above, are sometimes built
	-- as zero-arity dfuns, so isDictId alone won't work.

      bindIds binders clone_infos (

		-- Process the dictionary bindings themselves
	specBind new_binders bind	`thenSM` \ (bind, rhs_uds) ->

		-- Process their scope
	scopeM				`thenSM` \ (thing, scope_uds) ->
	let 
		-- Add the bindings to the current stuff
	    final_uds = addDictBinds new_binders bind rhs_uds scope_uds
	in
	returnSM ([], thing, final_uds)
      )
    else
	-- Ho! A group of ordinary (non-dict) bindings
      fixSM (\ ~(_, _, _, rec_spec_infos) ->

        bindSpecIds binders clone_infos rec_spec_infos (
		-- It's ok to have new binders in scope in
		-- non-recursive decls too, cos name shadowing is gone by now

		-- Do the scope of the bindings
	  scopeM				`thenSM` \ (thing, scope_uds) ->
	  let 
	     (call_insts_these_binders, gotci_scope_uds) = getCIs new_binders scope_uds
	  in

		-- Do the bindings themselves
	  specBind new_binders bind		`thenSM` \ (spec_bind, spec_uds) ->

		-- Create any necessary instances
	  instBind new_binders bind call_insts_these_binders
						`thenSM` \ (inst_binds, inst_uds, spec_infos) -> 

	  let
		-- Dump any dictionary bindings from the scope
		-- which mention things bound here
		(dict_binds, final_scope_uds) = dumpUDs gotci_scope_uds new_binders []
			-- The spec_ids can't appear anywhere in uds, because they only
			-- appear in SpecInfos.

		-- Build final binding group
		-- see note below about dependecies
		final_binds = [spec_bind,
			       CoRec (pairsFromCoreBinds (inst_binds ++ dict_binds))
			      ]

	  in
		-- Combine the results together
	  returnSM (final_binds,
		    thing, 
		    spec_uds `unionUDs` final_scope_uds `unionUDs` inst_uds, 
			-- inst_uds comes last, because there may be dict bindings
			-- floating outward in final_scope_uds which are mentioned 
			-- in the call-instances, and hence in spec_uds.
			-- This ordering makes sure that the precedence order
			-- among the dict bindings finally floated out is maintained.
		    spec_infos)
        )
      )			`thenSM` 	\ (binds, thing, final_uds, spec_infos) ->
      returnSM (binds, thing, final_uds)
  where
    binders = bindersOf bind
\end{code}

We place the spec_binds and dict_binds in a CoRec as there may be some
nasty dependencies. These don't actually require a CoRec, but its the
simplest solution. (The alternative would require some tricky dependency
analysis.) We leave it to the real dependency analyser to sort it all
out during a subsequent simplification pass.

Where do these dependencies arise?  Consider this case:

	data Foo a = ...

	{- instance Eq a => Eq (Foo a) where ... -}
	dfun.Eq.(Foo *) d.eq.a = <wurble>

	d2 = dfun.Eq.(Foo *) Char# d.Eq.Char#
	d1 = dfun.Eq.(Foo *) (Foo Char#) d2

Now, when specialising we must write the Char# instance of dfun.Eq.(Foo *) before
that for the (Foo Char#) instance:

	dfun.Eq.(Foo *) d.eq.a = <wurble>

	dfun.Eq.(Foo *)@Char# = <wurble>[d.Eq.Char#/d.eq.a]
	d2 = dfun.Eq.(Foo *)@Char# 

	dfun.Eq.(Foo *)@(Foo Char#) = <wurble>[d2/d.eq.a]
	d1 = dfun.Eq.(Foo *)@(Foo Char#)

The definition of dfun.Eq.(Foo *)@(Foo Char#) uses d2!!!  So it must
come after the definition of dfun.Eq.(Foo *)@Char#.
AAARGH!



\begin{code}
specBind :: [Id] -> PlainCoreBinding -> SpecM (PlainCoreBinding, UsageDetails)
	-- The UsageDetails returned has already had stuff to do with this group
	-- of binders deleted; that's why new_binders is passed in.
specBind new_binders (CoNonRec binder rhs) 
  = specOneBinding new_binders (binder,rhs)	`thenSM` \ ((binder,rhs), rhs_uds) ->
    returnSM (CoNonRec binder rhs, rhs_uds)

specBind new_binders (CoRec pairs)
  = mapAndUnzipSM (specOneBinding new_binders) pairs	`thenSM` \ (pairs, rhs_uds_s) ->
    returnSM (CoRec pairs, unionUDList rhs_uds_s)


specOneBinding :: [Id] -> (Id,PlainCoreExpr) -> SpecM ((Id,PlainCoreExpr), UsageDetails)

specOneBinding new_binders (binder, rhs)
  = lookupId binder		`thenSM` \ blookup ->
    specExpr rhs []		`thenSM` \ (rhs, rhs_uds) ->
    let
	specid_maybe_maybe  = isSpecPragmaId_maybe binder
	is_specid           = maybeToBool specid_maybe_maybe
	Just specinfo_maybe = specid_maybe_maybe
	specid_with_info    = maybeToBool specinfo_maybe
        Just spec_info      = specinfo_maybe

	pragma_uds
	  = if is_specid && specid_with_info then
	 	-- Have a SpecInfo stored in a SpecPragmaId binder
		-- This contains the SpecInfo for a specialisation pragma
		-- with an explicit SpecId specified
		-- We remove any cis for orig_id (there should only be one)
		-- and add the explicit ci to the usage details
		let
		    (SpecInfo spec_tys _ spec_id) = spec_info
		    Just (orig_id, _) = isSpecId_maybe spec_id
		in
		ASSERT(toplevelishId orig_id)     -- must not be cloned!
		explicitCI orig_id spec_tys spec_info
	    else
	        emptyUDs

	(binds_here, final_uds) = dumpUDs rhs_uds new_binders []
    in
    case blookup of
	Lifted lift_binder unlift_binder 
	  -> 	-- We may need to record an unboxed instance of 
		-- the _Lift data type in the usage details
	     mkTyConInstance liftDataCon [getIdUniType unlift_binder]
						`thenSM` \ lift_uds ->
	     returnSM ((lift_binder,
		        mkCoLetsNoUnboxed binds_here (liftExpr unlift_binder rhs)),
		       final_uds `unionUDs` pragma_uds `unionUDs` lift_uds)

	NoLift (CoVarAtom binder)
	  -> returnSM ((binder, mkCoLetsNoUnboxed binds_here rhs),
		       final_uds `unionUDs` pragma_uds)
\end{code}


%************************************************************************
%*									*
\subsection{@instBind@}
%*									*
%************************************************************************

\begin{code}
instBind main_ids@(first_binder:other_binders) bind call_insts_for_main_ids
 | all same_overloading other_binders
 = let
	-- Collect up identical call instances
	equiv_classes = equivClasses cmpCI_tys call_insts_for_main_ids 
   in
	-- For each equivalence class, build an instance
   mapAndUnzip3SM do_this_class equiv_classes	`thenSM` \ (inst_binds, inst_uds_s, spec_infos) ->

	-- Add in the remaining UDs
   returnSM (catMaybes inst_binds, 
   	     unionUDList inst_uds_s,
	     spec_infos
	    )

 | otherwise		-- Incompatible overloadings; see below by same_overloading
 = (if null (filter isUnboxedCI call_insts_for_main_ids)
    then (\ x y -> y) -- pprTrace "dumpCIs: not same overloading ... \n"
    else pprTrace "dumpCIs: not same overloading ... WITH UNBOXED TYPES!\n")
	     (ppHang (ppBesides [ppStr "{", ppr PprDebug main_ids, ppStr "}"])
   	           4 (ppAboves (map pprCI call_insts_for_main_ids)))
   (returnSM ([], emptyUDs, []))

 where
    (tyvar_tmpls, class_tyvar_pairs) = getIdOverloading first_binder
    tyvar_tmpl_tys = map mkTyVarTemplateTy tyvar_tmpls

    no_of_tyvars = length tyvar_tmpls
    no_of_dicts  = length class_tyvar_pairs

    do_this_class equiv_cis
      | not (null explicit_cis)
      = if (length main_ids > 1 || length explicit_cis > 1) then
	    -- ToDo: If this situation arose we would need to go through
	    --       checking cis for each main_id and only creating an
	    --       instantiation if we had no explicit_cis for that main_id
	    pprPanic "Specialise:instBind:explicit call instances\n"
		     (ppAboves [ppCat [ppStr "{", ppr PprDebug main_ids, ppStr "}"],
			        ppAboves (map pprCI equiv_cis)])
	else
    	    getSwitchCheckerSM		`thenSM` \ sw_chkr ->
    	    (if sw_chkr SpecialiseTrace then
	     let
		SpecInfo spec_tys _ spec_id = explicit_spec_info
             in
	     pprTrace "Specialising:"
	         (ppHang (ppBesides [ppStr "{", ppr PprDebug main_ids, ppStr "}"])
		       4 (ppAboves [
		          ppCat (ppStr "at types:" : [pprMaybeTy PprDebug ty | ty <- spec_tys]),
		          ppCat [ppStr "spec ids:", ppr PprDebug [spec_id], ppStr "(explicit)"]]))
	     else id) (

	    returnSM (Nothing, emptyUDs, [explicit_spec_info])
	    )
      | otherwise
      = mkOneInst (head equiv_cis) no_of_dicts main_ids bind
      where
        explicit_cis = filter isExplicitCI equiv_cis
	[CallInstance _ _ _ _ (Just explicit_spec_info)] = explicit_cis


	-- same_overloading tests whether the types of all the binders
	-- are "compatible"; ie have the same type and dictionary abstractions
	-- Almost always this is the case, because a recursive group is abstracted
	-- all together.  But, it can happen that it ain't the case, because of
	-- code generated from instance decls:
	--
	--	rec
	--	  dfun.Foo.Int :: (forall a. a -> Int, Int)
	--	  dfun.Foo.Int = (const.op1.Int, const.op2.Int)
	--
	--	  const.op1.Int :: forall a. a -> Int
	--	  const.op1.Int a = defm.Foo.op1 Int a dfun.Foo.Int
	--
	--	  const.op2.Int :: Int
	--	  const.op2.Int = 3
	--
	-- Note that the first two defns have different polymorphism, but they are
	-- mutually recursive!

    same_overloading :: Id -> Bool
    same_overloading id 
      = no_of_tyvars == length this_id_tyvars 					-- Same no of tyvars
	&&
	no_of_dicts == length this_id_class_tyvar_pairs				-- Same no of vdicts
	&&
	and (zipWith same_ov class_tyvar_pairs this_id_class_tyvar_pairs)	-- Same overloading
      where
	(this_id_tyvars, this_id_class_tyvar_pairs) = getIdOverloading id
  	tyvar_pairs = this_id_tyvars `zip` tyvar_tmpls

	same_ov (clas1,tyvar1) (clas2,tyvar2) 
	  = clas1  == clas2 &&
	    tyvar1 == assoc "same_overloading" tyvar_pairs tyvar2
\end{code}

OK, so we have:
	- a call instance				eg f [t1,t2,t3] [d1,d2]
	- the rhs of the function			eg orig_rhs
	- a constraint vector, saying which of 		eg [T,F,T]
	  the functions type args are constrained
	  (ie overloaded)

We return a new definition

	f@t1//t3 = /\a -> orig_rhs t1 a t3 d1 d2

The SpecInfo for f will be (the "2" indicates 2 dictionaries to eat)

	SpecInfo [Just t1, Nothing, Just t3] 2 f@t1//t3 

Based on this SpecInfo, a call instance of f

	...(f t1 t2 t3 d1 d2)...

should get replaced by

	...(f@t1//t3 t2)...

(But that is the business of @mkCall@.)

\begin{code}
mkOneInst :: CallInstance
	  -> Int				-- No of dicts to specialise
	  -> [Id]				-- New binders
	  -> PlainCoreBinding			-- Unprocessed
	  -> SpecM (Maybe PlainCoreBinding,	-- Instantiated version of input
		    UsageDetails,
		    [SpecInfo]			-- One for each id in the original binding
		   )

mkOneInst (CallInstance _ spec_tys dict_args _ _) no_of_dicts_to_specialise main_ids orig_bind
  = ASSERT (no_of_dicts_to_specialise == length dict_args)
    newSpecIds main_ids spec_tys no_of_dicts_to_specialise copy_inline_info
							`thenSM` \ spec_ids ->
    newTyVars (length [() | Nothing <- spec_tys])   	`thenSM` \ poly_tyvars ->
    let
	-- arg_tys is spec_tys with tyvars instead of the Nothing spec_tys
	-- which correspond to unspeciailsed args
	arg_tys  :: [UniType]
	(_,arg_tys) = mapAccumL do_the_wotsit poly_tyvars spec_tys

	args :: [PlainCoreArg]
	args = map TypeArg arg_tys ++ dict_args

	(one_spec_id:_) = spec_ids

	do_bind (CoNonRec binder rhs) 
	  = do_one_rhs rhs 	`thenSM` \ (rhs, rhs_uds) ->
	    returnSM (CoNonRec one_spec_id rhs, rhs_uds)

	do_bind (CoRec pairs)
	  = mapAndUnzipSM do_one_rhs [rhs | (_,rhs) <- pairs]	`thenSM` \ (rhss, rhss_uds_s) ->
	    returnSM (CoRec (spec_ids `zip` rhss), unionUDList rhss_uds_s)

	-- Apply the specialiser to (orig_rhs t1 a t3 d1 d2)
	do_one_rhs orig_rhs = specExpr orig_rhs args	`thenSM` \ (inst_rhs, inst_uds) ->
			      let 
				(binds_here, final_uds) = dumpUDs inst_uds main_ids []
				-- NB: main_ids!! not spec_ids!! Why? Because the free-var
				-- stuff knows nowt about spec_ids; it'll just have the
				-- original polymorphic main_ids as free.  Belgh
			      in
			      returnSM (mkCoLetsNoUnboxed binds_here (mkCoTyLam poly_tyvars inst_rhs), 
					final_uds)
    in
    getSwitchCheckerSM		`thenSM` \ sw_chkr ->
    (if sw_chkr SpecialiseTrace then
	pprTrace "Specialising:"
	         (ppHang (ppBesides [ppStr "{", ppr PprDebug main_ids, ppStr "}"])
		       4 (ppAboves [
		          ppBesides [ppStr "with args: ", ppInterleave ppNil (map pp_arg args)],
		          ppBesides [ppStr "spec ids: ", ppr PprDebug spec_ids]]))
     else id) (
	   
    do_bind orig_bind		`thenSM` \ (inst_bind, inst_uds) ->

    returnSM (Just inst_bind,
	      inst_uds,
	      [SpecInfo spec_tys no_of_dicts_to_specialise spec_id | spec_id <- spec_ids]
	      )
    )
  where
    -- debugging
    pp_arg (ValArg  a) = ppBesides [ppLparen, ppStr "ValArg ", ppr PprDebug a, ppRparen]
    pp_arg (TypeArg t) = ppBesides [ppLparen, ppStr "TypeArg ", ppr PprDebug t, ppRparen]

    do_the_wotsit (tyvar:tyvars) Nothing   = (tyvars, mkTyVarTy tyvar)
    do_the_wotsit tyvars         (Just ty) = (tyvars, ty)

    copy_inline_info new_id old_uf_info = addIdUnfolding new_id old_uf_info
\end{code}

%************************************************************************
%*									*
\subsection[Misc]{Miscellaneous junk}
%*									*
%************************************************************************

@getIdOverloading@ grabs the type of an Id, and returns a 
list of its polymorphic variables, and the initial segment of
its ThetaType, in which the classes constrain only type variables.
For example, if the Id's type is

	forall a,b,c. Eq a -> Ord [a] -> tau

we'll return

	([a,b,c], [(Eq,a)])

This seems curious at first.  For a start, the type above looks odd,
because we usually only have dictionary args whose types are of
the form (C a) where a is a type variable.  But this doesn't hold for
the functions arising from instance decls, which sometimes get 
arguements with types of form (C (T a)) for some type constructor T.

Should we specialise wrt this compound-type dictionary?  This is
a heuristic judgement, as indeed is the fact that we specialise wrt
only dictionaries.  We choose *not* to specialise wrt compound dictionaries
because at the moment the only place they show up is in instance decls,
where they are simply plugged into a returned dictionary.  So nothing is
gained by specialising wrt them.

\begin{code}
getIdOverloading :: Id
		 -> ([TyVarTemplate], [(Class,TyVarTemplate)])
getIdOverloading id
  = (tyvars, tyvar_part_of theta)
  where
    (tyvars, theta, _) = splitType (getIdUniType id)

    tyvar_part_of [] 		      = []
    tyvar_part_of ((clas,ty) : theta) = case getTyVarTemplateMaybe ty of
					    Nothing    -> []
					    Just tyvar -> (clas, tyvar) : tyvar_part_of theta
\end{code}

\begin{code}
mkCallInstance :: Id 
	       -> Id
	       -> [(PlainCoreArg, UsageDetails, PlainCoreExpr -> PlainCoreExpr)]
	       -> SpecM UsageDetails

mkCallInstance old_id new_id args
  = recordCallInst old_id args	`thenSM` \ record_call ->
    case record_call of
      Nothing   					-- No specialisation required
	-> -- pprTrace "NoSpecReqd:" 
	   --	    (ppCat [ppr PprDebug old_id, ppStr "at", ppCat (map (ppr PprDebug) args)])

	   (returnSM call_fv_uds)

      Just (True, spec_tys, dict_args, rest_args)	-- Requires specialisation: spec already exists
	-> -- pprTrace "SpecExists:" 
	   --	    (ppCat [ppr PprDebug old_id, ppStr " at ", ppCat (map (ppr PprDebug) args),
	   --		    ppBesides [ppStr "(", ppCat [pprMaybeTy PprDebug ty | ty <- spec_tys], 
	   --			                  ppCat [ppr PprDebug dict | dict <- dict_args],
	   --		               ppStr ")"]])

	   (returnSM call_fv_uds)

      Just (False, spec_tys, dict_args, rest_args)	-- Requires specialisation: record call-instance
	-> -- pprTrace "CallInst:"
	   --	    (ppCat [ppr PprDebug old_id, ppStr " at ", ppCat (map (ppr PprDebug) args),
	   --		    ppBesides [ppStr "(", ppCat [pprMaybeTy PprDebug ty | ty <- spec_tys], 
	   --			                  ppCat [ppr PprDebug dict | dict <- dict_args],
	   --		               ppStr ")"]])

	   (returnSM (singleCI new_id spec_tys dict_args `unionUDs` call_fv_uds))
  where
    call_fv_uds = singleFvUDs (CoVarAtom new_id) `unionUDs` unionUDList [uds | (_,uds,_) <- args]
\end{code}

\begin{code}
recordCallInst :: Id
	       -> [(PlainCoreArg, UsageDetails, PlainCoreExpr -> PlainCoreExpr)]
	       -> SpecM (Maybe (Bool, [Maybe UniType], [PlainCoreArg],
				[(PlainCoreArg, UsageDetails, PlainCoreExpr -> PlainCoreExpr)]))

recordCallInst id []		-- No args => no call instance
  = returnSM Nothing

recordCallInst id args
  | isBottomingId id		-- No specialised versions for "error" and friends are req'd.
  = returnSM Nothing		-- This is a special case in core lint etc.

	-- No call instances for Ids associated with a Class declaration,
        -- i.e. default methods, super-dict selectors and class ops.
        -- We rely on the instance declarations to provide suitable specialisations.
	-- These are dealt with in mkCall.

  | isDefaultMethodId id
  = returnSM Nothing	
			
  | maybeToBool (isSuperDictSelId_maybe id)
  = returnSM Nothing

  | isClassOpId id		
  = returnSM Nothing		

	-- Finally, the default case ...

  | otherwise
  = getSwitchCheckerSM		`thenSM` \ sw_chkr ->
    let
        spec_overloading = sw_chkr SpecialiseOverloaded
        spec_unboxed     = sw_chkr SpecialiseUnboxed
        spec_all	 = sw_chkr SpecialiseAll

	(tyvar_tmpls, class_tyvar_pairs) = getIdOverloading id
        constraint_vec = mkConstraintVector tyvar_tmpls class_tyvar_pairs

	arg_res = take_type_args tyvar_tmpls class_tyvar_pairs args
	enough_args = maybeToBool arg_res

	(Just (inst_tys, dict_args, rest_args)) = arg_res
	spec_tys = specialiseCallTys spec_all spec_unboxed spec_overloading
		                     constraint_vec inst_tys

	spec_exists = maybeToBool (lookupSpecEnv 
				     (getIdSpecialisation id) 
				     inst_tys)

	-- We record the call instance if there is some meaningful
	-- type which we want to specialise on ...
	record_spec = any (not . isTyVarTy) (catMaybes spec_tys)
    in
    if (not enough_args) then
	pprPanic "Specialise:recordCallInst: Unsaturated Type & Dict Application:\n\t"
		 (ppCat [ppr PprDebug id, ppr PprDebug [arg | (arg,_,_) <- args] ]) 
    else
    if record_spec then
	returnSM (Just (spec_exists, spec_tys, dict_args, rest_args))
    else
	returnSM Nothing


take_type_args (_:tyvars) class_tyvar_pairs ((TypeArg ty,_,_):args) 
	= case take_type_args tyvars class_tyvar_pairs args of
		Nothing 	          -> Nothing
		Just (tys, dicts, others) -> Just (ty:tys, dicts, others)
take_type_args (_:tyvars) class_tyvar_pairs []
	= Nothing
take_type_args [] class_tyvar_pairs args 
	= case take_dict_args class_tyvar_pairs args of
		Nothing              -> Nothing
		Just (dicts, others) -> Just ([], dicts, others)

take_dict_args (_:class_tyvar_pairs) ((dict@(ValArg _),_,_):args) 
	= case take_dict_args class_tyvar_pairs args of
		Nothing              -> Nothing
		Just (dicts, others) -> Just (dict:dicts, others)
take_dict_args (_:class_tyvar_pairs) []
	= Nothing
take_dict_args [] args
	= Just ([], args)
\end{code}

\begin{code}
mkCall :: Id
       -> [(PlainCoreArg, UsageDetails, PlainCoreExpr -> PlainCoreExpr)]
       -> SpecM PlainCoreExpr

mkCall main_id args
  | isDefaultMethodId main_id
    && any isUnboxedDataType ty_args
	-- No specialisations for default methods
	-- Unboxed calls to DefaultMethodIds should not occur
	-- The method should be specified in the instance declaration
    = panic "Specialise:mkCall:DefaultMethodId"

  | maybeToBool (isSuperDictSelId_maybe main_id)
    && any isUnboxedDataType ty_args
	-- No specialisations for super-dict selectors
	-- Specialise unboxed calls to SuperDictSelIds by extracting
	-- the super class dictionary directly form the super class
	-- NB: This should be dead code since all uses of this dictionary should
	--     have been specialised. We only do this to keep keep core-lint happy.
    = let
	 Just (_, super_class) = isSuperDictSelId_maybe main_id
         super_dict_id = case lookupClassInstAtSimpleType super_class (head ty_args) of
			 Nothing -> panic "Specialise:mkCall:SuperDictId"
			 Just id -> id
      in
      returnSM (CoVar super_dict_id)

  | otherwise
    = case lookupSpecEnv (getIdSpecialisation main_id) ty_args of
	Nothing -> checkUnspecOK main_id ty_args (
		   returnSM unspec_call
		   )

	Just (spec_id, tys_left, dicts_to_toss) 
		-> checkSpecOK main_id ty_args spec_id tys_left (
		   let
		       args_left = toss_dicts dicts_to_toss val_args
		   in

			-- The resulting spec_id may be an unboxed constant method
			--   eg: pi Double# d.Floating.Double# ==> pi.Double#
			-- Since it is a top level id pi.Double# will have been lifted.
			-- We must add code to unlift such a spec_id 

		   if isUnboxedDataType (getIdUniType spec_id) then
		       ASSERT (null tys_left && null args_left)
		       if isConstMethodId spec_id then
		 	   liftId spec_id 	`thenSM` \ (lifted_spec_id, unlifted_spec_id) ->
			   returnSM (bindUnlift lifted_spec_id unlifted_spec_id
						(CoVar unlifted_spec_id))
		       else
			   -- ToDo: Are there other cases where we have an unboxed spec_id ???
			   pprPanic "Specialise:mkCall: unboxed spec_id ...\n"
				    (ppCat [ppr PprDebug main_id,
				            ppInterleave ppNil (map (pprParendUniType PprDebug) ty_args),
					    ppStr "==>",
					    ppr PprDebug spec_id])
		   else		
		   let
		       (vals_left, _, unlifts_left) = unzip3 args_left
		       applied_tys  = mkCoTyApps (CoVar spec_id) tys_left
		       applied_vals = applyToArgs applied_tys vals_left
		   in
		   returnSM (applyBindUnlifts unlifts_left applied_vals)
		   )
  where
    (tys_and_vals, _, unlifts) = unzip3 args
    unspec_call = applyBindUnlifts unlifts (applyToArgs (CoVar main_id) tys_and_vals)


	-- ty_args is the types at the front of the arg list
	-- val_args is the rest of the arg-list

    (ty_args, val_args) = get args
      where
	get ((TypeArg ty,_,_) : args) = (ty : tys, rest) where (tys,rest) = get args
	get args		      = ([],       args)

	-- toss_dicts chucks away dict args, checking that they ain't types!
    toss_dicts 0 args 		         = args
    toss_dicts n ((ValArg _,_,_) : args) = toss_dicts (n-1) args
\end{code}

\begin{code}
checkUnspecOK :: Id -> [UniType] -> a -> a
checkUnspecOK check_id tys
  = if isLocallyDefined check_id && any isUnboxedDataType tys
    then pprPanic "Specialise:checkUnspecOK: unboxed instance for local id not found\n"
		  (ppCat [ppr PprDebug check_id,
			  ppInterleave ppNil (map (pprParendUniType PprDebug) tys)])
    else id

checkSpecOK :: Id -> [UniType] -> Id -> [UniType] -> a -> a
checkSpecOK check_id tys spec_id tys_left
  = if any isUnboxedDataType tys_left
    then pprPanic "Specialise:checkSpecOK: unboxed type args in specialised application\n"
		  (ppAboves [ppCat [ppr PprDebug check_id,
				    ppInterleave ppNil (map (pprParendUniType PprDebug) tys)],
			     ppCat [ppr PprDebug spec_id,
				    ppInterleave ppNil (map (pprParendUniType PprDebug) tys_left)]])
    else id
\end{code}

\begin{code}
mkTyConInstance :: Id
		-> [UniType]
   		-> SpecM UsageDetails
mkTyConInstance con tys
  = recordTyConInst con tys	`thenSM` \ record_inst ->
    case record_inst of
      Nothing				-- No TyCon instance
        -> -- pprTrace "NoTyConInst:" 
	   --	    (ppCat [ppr PprDebug tycon, ppStr "at",
	   --	            ppr PprDebug con, ppCat (map (ppr PprDebug) tys)])
	   (returnSM (singleConUDs con))

      Just spec_tys			-- Record TyCon instance
	-> -- pprTrace "TyConInst:"
	   --	    (ppCat [ppr PprDebug tycon, ppStr "at",
	   --		    ppr PprDebug con, ppCat (map (ppr PprDebug) tys),
	   --		    ppBesides [ppStr "(", 
	   --			       ppCat [pprMaybeTy PprDebug ty | ty <- spec_tys],
	   --			       ppStr ")"]])
	   (returnSM (singleTyConI tycon spec_tys `unionUDs` singleConUDs con))
  where
    tycon = getDataConTyCon con
\end{code}

\begin{code}
recordTyConInst :: Id
		-> [UniType]
		-> SpecM (Maybe [Maybe UniType])

recordTyConInst con tys
  = let
        spec_tys = specialiseConstrTys tys

	do_tycon_spec = maybeToBool (firstJust spec_tys)

        spec_exists = maybeToBool (lookupSpecEnv 
				      (getIdSpecialisation con) 
				      tys)
    in
    -- pprTrace "ConSpecExists?: "
    --	     (ppAboves [ppStr (if spec_exists then "True" else "False"),
    --		        ppr PprShowAll con, ppCat (map (ppr PprDebug) tys)])
    (if (not spec_exists && do_tycon_spec)
     then returnSM (Just spec_tys)
     else returnSM Nothing)
\end{code}

\begin{code}
{- UNUSED: create specilaised constructor calls in Core
mkConstrCall :: PlainCoreAtom -> [UniType] 	-- This constructor at these types
	     -> SpecM (Id, [UniType])		-- The specialised constructor and reduced types

mkConstrCall (CoVarAtom con_id) tys
  = case lookupSpecEnv (getIdSpecialisation con_id) tys of
	Nothing -> checkUnspecOK con_id tys (
		   returnSM (con_id, tys)
		   )
	Just (spec_id, tys_left, 0)
	        -> checkSpecOK con_id tys spec_id tys_left (
		   returnSM (spec_id, tys_left)
		   )
-}
\end{code}

%************************************************************************
%*									*
\subsection[monad-Specialise]{Monad used in specialisation}
%*									*
%************************************************************************

Monad has:

 inherited: control flags and
	    recordInst functions with flags cached

	    environment mapping tyvars to types 
	    environment mapping Ids to Atoms
 
 threaded in and out: unique supply

\begin{code}
type SpecM result
  =  (GlobalSwitch -> Bool)
  -> TypeEnv
  -> SpecIdEnv
  -> SplitUniqSupply
  -> result

initSM m sw_chker uniqs
  = m sw_chker nullTyVarEnv nullIdEnv uniqs

returnSM :: a -> SpecM a
thenSM	 :: SpecM a -> (a -> SpecM b) -> SpecM b
fixSM    :: (a -> SpecM a) -> SpecM a

thenSM m k sw_chkr tvenv idenv us
  = case splitUniqSupply us	   of { (s1, s2) ->
    case (m sw_chkr tvenv idenv s1) of { r ->
    k r sw_chkr tvenv idenv s2 }}

returnSM r sw_chkr tvenv idenv us = r

fixSM k sw_chkr tvenv idenv us
 = r
 where
   r = k r sw_chkr tvenv idenv us	-- Recursive in r!
\end{code}

\begin{code}
getSwitchCheckerSM sw_chkr tvenv idenv us = sw_chkr
\end{code}

The only interesting bit is figuring out the type of the SpecId!

\begin{code}
newSpecIds :: [Id]		-- The id of which to make a specialised version
	   -> [Maybe UniType]	-- Specialise to these types
	   -> Int		-- No of dicts to specialise
	   -> (Id -> UnfoldingDetails -> Id)  -- copies any arity info required
	   -> SpecM [Id]

newSpecIds main_ids maybe_tys dicts_to_ignore copy_id_info sw_chkr tvenv idenv us
  = spec_ids
  where
    uniqs = getSUniques (length main_ids) us
    spec_id_ty id = specialiseTy (getIdUniType id) maybe_tys dicts_to_ignore
    spec_ids = [ copy_id_info (mkSpecId uniq id maybe_tys (spec_id_ty id) noIdInfo) (getIdUnfolding id)
	       | (id,uniq) <- main_ids `zip` uniqs
	       ]

newTyVars :: Int -> SpecM [TyVar]
newTyVars n sw_chkr tvenv idenv us
 = map mkPolySysTyVar uniqs
 where
   uniqs = getSUniques n us
\end{code}

@cloneLambdaOrCaseBinders@ and @cloneLetrecBinders@ take a bunch of
binders, and build ``clones'' for them.  The clones differ from the
originals in three ways:

	(a) they have a fresh unique
	(b) they have the current type environment applied to their type
	(c) for letrec binders which have been specialised to unboxed values
	    the clone will have a lifted type

As well as returning the list of cloned @Id@s they also return a list of
@CloneInfo@s which the original binders should be bound to.
	    
\begin{code}
cloneLambdaOrCaseBinders :: [Id] 			-- Old binders
			 -> SpecM ([Id], [CloneInfo])	-- New ones

cloneLambdaOrCaseBinders old_ids sw_chkr tvenv idenv us
  = let
	uniqs = getSUniques (length old_ids) us
    in
    unzip (zipWith clone_it old_ids uniqs)
  where
    clone_it old_id uniq
      = (new_id, NoLift (CoVarAtom new_id))
      where
	new_id = applyTypeEnvToId tvenv (mkIdWithNewUniq old_id uniq)

cloneLetrecBinders :: [Id] 				-- Old binders
		   -> SpecM ([Id], [CloneInfo])	-- New ones

cloneLetrecBinders old_ids sw_chkr tvenv idenv us
  = let
	uniqs = getSUniques (2 * length old_ids) us
    in
    unzip (clone_them old_ids uniqs)
  where
    clone_them [] [] = []

    clone_them (old_id:olds) (u1:u2:uniqs)
      | toplevelishId old_id
	= (old_id,
	   NoLift (CoVarAtom old_id)) : clone_rest

	 -- Don't clone if it is a top-level thing. Why not?
	 -- (a) we don't want to change the uniques 
	 --     on such things (see TopLevId in Id.lhs)
	 -- (b) we don't have to be paranoid about name capture
	 -- (c) the thing is polymorphic so no need to subst

      | otherwise
	= if (isUnboxedDataType new_ty && not (isUnboxedDataType old_ty))
	  then (lifted_id,
		Lifted lifted_id unlifted_id) : clone_rest
	  else (new_id,
		NoLift (CoVarAtom new_id)) : clone_rest

      where 
	clone_rest = clone_them olds uniqs

	new_id = applyTypeEnvToId tvenv (mkIdWithNewUniq old_id u1)
	new_ty = getIdUniType new_id
	old_ty = getIdUniType old_id

	(lifted_id, unlifted_id) = mkLiftedId new_id u2


cloneTyVarSM :: TyVar -> SpecM TyVar

cloneTyVarSM old_tyvar sw_chkr tvenv idenv us
  = let
	uniq = getSUnique us
    in
    cloneTyVar old_tyvar uniq -- new_tyvar

bindId :: Id -> CloneInfo -> SpecM thing -> SpecM thing

bindId id val specm sw_chkr tvenv idenv us
 = specm sw_chkr tvenv (addOneToIdEnv idenv id val) us

bindIds :: [Id] -> [CloneInfo] -> SpecM thing -> SpecM thing

bindIds olds news specm sw_chkr tvenv idenv us
 = specm sw_chkr tvenv (growIdEnvList idenv (zip olds news)) us

bindSpecIds :: [Id]		-- Old
	    -> [(CloneInfo)]	-- New
	    -> [[SpecInfo]]	-- Corresponding specialisations
				-- Each sub-list corresponds to a different type,
				-- and contains one spec_info for each id
	    -> SpecM thing 
	    -> SpecM thing

bindSpecIds olds clones spec_infos specm sw_chkr tvenv idenv us
 = specm sw_chkr tvenv (growIdEnvList idenv old_to_clone) us
 where
   old_to_clone = mk_old_to_clone olds clones spec_infos

   -- The important thing here is that we are *lazy* in spec_infos
   mk_old_to_clone [] [] _ = []
   mk_old_to_clone (old:rest_olds) (clone:rest_clones) spec_infos
     = (old, add_spec_info clone) : 
       mk_old_to_clone rest_olds rest_clones spec_infos_rest
     where
       add_spec_info (NoLift (CoVarAtom new))
	 = NoLift (CoVarAtom (new `addIdSpecialisation`
			          (mkSpecEnv spec_infos_this_id)))
       add_spec_info lifted
	 = lifted		-- no specialised instances for unboxed lifted values

       spec_infos_this_id = map head spec_infos
       spec_infos_rest    = map tail spec_infos

{- UNUSED: creating specialised constructors
bindConIds :: [Id]		-- Old constructors
	   -> [[SpecInfo]]	-- Corresponding specialisations to be added
				-- Each sub-list corresponds to one constructor, and
				-- gives all its specialisations
	   -> SpecM thing 
	   -> SpecM thing

bindConIds ids spec_infos specm sw_chkr tvenv idenv us
 = specm sw_chkr tvenv (growIdEnvList idenv id_to_newspec) us
 where
   id_to_newspec = mk_id_to_newspec ids spec_infos

   -- The important thing here is that we are *lazy* in spec_infos
   mk_id_to_newspec [] _ = []
   mk_id_to_newspec (id:rest_ids) spec_infos
     = (id, CoVarAtom id_with_spec) : 
       mk_id_to_newspec rest_ids spec_infos_rest
     where
       id_with_spec = id `addIdSpecialisation` (mkSpecEnv spec_infos_this_id)
       spec_infos_this_id = head spec_infos
       spec_infos_rest    = tail spec_infos
-}

bindTyVar :: TyVar -> UniType -> SpecM thing -> SpecM thing

bindTyVar tyvar ty specm sw_chkr tvenv idenv us
 = specm sw_chkr (growTyVarEnvList tvenv [(tyvar,ty)]) idenv us
\end{code}

\begin{code}
lookupId :: Id -> SpecM CloneInfo

lookupId id sw_chkr tvenv idenv us 
  = case lookupIdEnv idenv id of
      Nothing   -> NoLift (CoVarAtom id)
      Just info -> info
\end{code}

\begin{code}
specTy :: UniType -> SpecM UniType	-- Apply the current type envt to the type

specTy ty sw_chkr tvenv idenv us 
  = applyTypeEnvToTy tvenv ty
\end{code}

\begin{code}
liftId :: Id -> SpecM (Id, Id)
liftId id sw_chkr tvenv idenv us
  = let
	uniq = getSUnique us
    in
    mkLiftedId id uniq
\end{code}

In other monads these @mapSM@ things are usually called @listM@.
I think @mapSM@ is a much better name.  The `2' and `3' variants are
when you want to return two or three results, and get at them
separately.  It saves you having to do an (unzip stuff) right after.

\begin{code}
mapSM  	       :: (a -> SpecM b)	    -> [a] -> SpecM [b]
mapAndUnzipSM  :: (a -> SpecM (b1, b2))	    -> [a] -> SpecM ([b1],[b2])
mapAndUnzip3SM :: (a -> SpecM (b1, b2, b3)) -> [a] -> SpecM ([b1],[b2],[b3])
mapAndUnzip4SM :: (a -> SpecM (b1, b2, b3, b4)) -> [a] -> SpecM ([b1],[b2],[b3],[b4])

mapSM f [] = returnSM []
mapSM f (x:xs) = f x  		`thenSM` \ r ->
		 mapSM f xs	`thenSM` \ rs ->
		 returnSM (r:rs)

mapAndUnzipSM f [] = returnSM ([],[])
mapAndUnzipSM f (x:xs) = f x 			`thenSM` \ (r1, r2) ->
			 mapAndUnzipSM f xs	`thenSM` \ (rs1,rs2) ->
			 returnSM ((r1:rs1),(r2:rs2))

mapAndUnzip3SM f [] = returnSM ([],[],[])
mapAndUnzip3SM f (x:xs) = f x 			`thenSM` \ (r1,r2,r3) ->
			  mapAndUnzip3SM f xs	`thenSM` \ (rs1,rs2,rs3) ->
			  returnSM ((r1:rs1),(r2:rs2),(r3:rs3))

mapAndUnzip4SM f [] = returnSM ([],[],[],[])
mapAndUnzip4SM f (x:xs) = f x 			`thenSM` \ (r1,r2,r3,r4) ->
			  mapAndUnzip4SM f xs	`thenSM` \ (rs1,rs2,rs3,rs4) ->
			  returnSM ((r1:rs1),(r2:rs2),(r3:rs3),(r4:rs4))
\end{code}
