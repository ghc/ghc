%
% (c) The GRASP/AQUA Project, Glasgow University, 1993-1996
%
\section[Specialise]{Stamping out overloading, and (optionally) polymorphism}

\begin{code}
#include "HsVersions.h"

module Specialise (
	specProgram,
	initSpecData,

	SpecialiseData(..)
    ) where

IMP_Ubiq(){-uitous-}
IMPORT_1_3(List(partition))

import Bag		( emptyBag, unitBag, isEmptyBag, unionBags,
			  partitionBag, listToBag, bagToList
			)
import Class		( GenClass{-instance Eq-} )
import CmdLineOpts	( opt_SpecialiseImports, opt_D_simplifier_stats,
			  opt_CompilingGhcInternals, opt_SpecialiseTrace,
			  opt_SpecialiseOverloaded, opt_SpecialiseUnboxed,
			  opt_SpecialiseAll
			)
import CoreLift		( mkLiftedId, liftExpr, bindUnlift, applyBindUnlifts )
import CoreSyn
import CoreUtils	( coreExprType, squashableDictishCcExpr )
import FiniteMap	( addListToFM_C, FiniteMap )
import Kind		( mkBoxedTypeKind )
import Id		( idType, isDefaultMethodId_maybe, toplevelishId,
			  isSuperDictSelId_maybe, isBottomingId,
			  isConstMethodId_maybe, isDataCon,
			  isImportedId, mkIdWithNewUniq,
			  dataConTyCon, applyTypeEnvToId,
			  nullIdEnv, addOneToIdEnv, growIdEnvList,
			  lookupIdEnv, SYN_IE(IdEnv),
			  emptyIdSet, mkIdSet, unitIdSet,
			  elementOfIdSet, minusIdSet,
			  unionIdSets, unionManyIdSets, SYN_IE(IdSet),
			  GenId{-instance Eq-}
			)
import Literal		( Literal{-instance Outputable-} )
import Maybes		( catMaybes, firstJust, maybeToBool )
import Name		( isLocallyDefined )
import Outputable	( interppSP, Outputable(..){-instance * []-} )
import PprStyle		( PprStyle(..) )
import PprType		( pprGenType, pprParendGenType, pprMaybeTy,
			  GenType{-instance Outputable-}, GenTyVar{-ditto-},
			  TyCon{-ditto-}
			)
import Pretty		( ppHang, ppCat, ppStr, ppAboves, ppBesides,
			  ppInt, ppSP, ppInterleave, ppNil, SYN_IE(Pretty)
			)
import PrimOp		( PrimOp(..) )
import SpecUtils
import Type		( mkTyVarTy, mkTyVarTys, isTyVarTy, getAppDataTyConExpandingDicts,
			  tyVarsOfTypes, applyTypeEnvToTy, isUnboxedType
			)
import TyCon		( TyCon{-instance Eq-} )
import TyVar		( cloneTyVar, mkSysTyVar,
			  elementOfTyVarSet, SYN_IE(TyVarSet),
			  nullTyVarEnv, growTyVarEnvList, SYN_IE(TyVarEnv),
			  GenTyVar{-instance Eq-}
			)
import TysWiredIn	( liftDataCon )
import Unique		( Unique{-instance Eq-} )
import UniqSet		( mkUniqSet, unionUniqSets, uniqSetToList )
import UniqSupply	( splitUniqSupply, getUniques, getUnique )
import Util		( equivClasses, mapAccumL, assoc, zipEqual, zipWithEqual,
			  thenCmp, panic, pprTrace, pprPanic, assertPanic
			)

infixr 9 `thenSM`

--ToDo:kill
data SpecInfo = SpecInfo [Maybe Type] Int Id
lookupSpecEnv = panic "Specialise.lookupSpecEnv (ToDo)"
addIdSpecialisation = panic "Specialise.addIdSpecialisation (ToDo)"
cmpUniTypeMaybeList = panic "Specialise.cmpUniTypeMaybeList (ToDo)"
getIdSpecialisation = panic "Specialise.getIdSpecialisation (ToDo)"
isClassOpId = panic "Specialise.isClassOpId (ToDo)"
isDictTy = panic "Specialise.isDictTy (ToDo)"
isLocalGenTyCon = panic "Specialise.isLocalGenTyCon (ToDo)"
isLocalSpecTyCon = panic "Specialise.isLocalSpecTyCon (ToDo)"
isSpecId_maybe = panic "Specialise.isSpecId_maybe (ToDo)"
isSpecPragmaId_maybe = panic "Specialise.isSpecPragmaId_maybe (ToDo)"
lookupClassInstAtSimpleType = panic "Specialise.lookupClassInstAtSimpleType (ToDo)"
mkSpecEnv = panic "Specialise.mkSpecEnv (ToDo)"
mkSpecId = panic "Specialise.mkSpecId (ToDo)"
selectIdInfoForSpecId = panic "Specialise.selectIdInfoForSpecId (ToDo)"
specialiseTy = panic "Specialise.specialiseTy (ToDo)"
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
substitute these in the Con calls.

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
		[Maybe Type] -- Instance types
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
type FreeVarsSet   = IdSet
type FreeTyVarsSet = TyVarSet

data CallInstance
  = CallInstance
		Id 		  -- This Id; *new* ie *cloned* id
		[Maybe Type]	  -- Specialised at these types (*new*, cloned)
				  -- Nothing => no specialisation on this type arg
				  --	      is required (flag dependent).
		[CoreArg]	  -- And these dictionaries; all ValArgs
		FreeVarsSet	  -- Free vars of the dict-args in terms of *new* ids
		(Maybe SpecInfo)  -- For specialisation with explicit SpecId
\end{code}

\begin{code}
pprCI :: CallInstance -> Pretty
pprCI (CallInstance id spec_tys dicts _ maybe_specinfo)
  = ppHang (ppCat [ppStr "Call inst for", ppr PprDebug id])
	 4 (ppAboves [ppCat (ppStr "types" : [pprMaybeTy PprDebug ty | ty <- spec_tys]),
		      case maybe_specinfo of
			Nothing -> ppCat (ppStr "dicts" : [ppr_arg PprDebug dict | dict <- dicts])
			Just (SpecInfo _ _ spec_id)
				-> ppCat [ppStr "Explicit SpecId", ppr PprDebug spec_id]
		     ])

-- ToDo: instance Outputable CoreArg?
ppr_arg sty (TyArg  t) = ppr sty t
ppr_arg sty (LitArg i) = ppr sty i
ppr_arg sty (VarArg v) = ppr sty v

isUnboxedCI :: CallInstance -> Bool
isUnboxedCI (CallInstance _ spec_tys _ _ _)
  = any isUnboxedType (catMaybes spec_tys)

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
  = cmp id1 id2 `thenCmp` cmpUniTypeMaybeList tys1 tys2

cmpCI_tys :: CallInstance -> CallInstance -> TAG_
cmpCI_tys (CallInstance _ tys1 _ _ _) (CallInstance _ tys2 _ _ _)
  = cmpUniTypeMaybeList tys1 tys2

eqCI_tys :: CallInstance -> CallInstance -> Bool
eqCI_tys c1 c2
  = case cmpCI_tys c1 c2 of { EQ_ -> True; other -> False }

isCIofTheseIds :: [Id] -> CallInstance -> Bool
isCIofTheseIds ids (CallInstance ci_id _ _ _ _)
  = any ((==) ci_id) ids

singleCI :: Id -> [Maybe Type] -> [CoreArg] -> UsageDetails
singleCI id tys dicts
  = UsageDetails (unitBag (CallInstance id tys dicts fv_set Nothing))
		 emptyBag [] emptyIdSet 0 0
  where
    fv_set = mkIdSet (id : [dict | (VarArg dict) <- dicts])

explicitCI :: Id -> [Maybe Type] -> SpecInfo -> UsageDetails
explicitCI id tys specinfo
  = UsageDetails (unitBag call_inst) emptyBag [] emptyIdSet 0 0
  where
    call_inst = CallInstance id tys dicts fv_set (Just specinfo)
    dicts  = panic "Specialise:explicitCI:dicts"
    fv_set = unitIdSet id

-- We do not process the CIs for top-level dfuns or defms
-- Instead we require an explicit SPEC inst pragma for dfuns
-- and an explict method within any instances for the defms

getCIids :: Bool -> [Id] -> [Id]
getCIids True ids = filter not_dict_or_defm ids
getCIids _    ids = ids

not_dict_or_defm id
  = not (isDictTy (idType id) || maybeToBool (isDefaultMethodId_maybe id))

getCIs :: Bool -> [Id] -> UsageDetails -> ([CallInstance], UsageDetails)
getCIs top_lev ids (UsageDetails cis tycon_cis dbs fvs c i)
  = let
	(cis_here, cis_not_here) = partitionBag (isCIofTheseIds (getCIids top_lev ids)) cis
	cis_here_list = bagToList cis_here
    in
    -- pprTrace "getCIs:"
    -- (ppHang (ppBesides [ppStr "{",
    --			   interppSP PprDebug ids,
    --			   ppStr "}"])
    --	     4 (ppAboves (map pprCI cis_here_list)))
    (cis_here_list, UsageDetails cis_not_here tycon_cis dbs fvs c i)

dumpCIs :: Bag CallInstance	-- The call instances
	-> Bool			-- True <=> top level bound Ids
	-> Bool			-- True <=> dict bindings to be floated (specBind only)
	-> [CallInstance]	-- Call insts for bound ids (instBind only)
	-> [Id]			-- Bound ids *new*
	-> [Id]			-- Full bound ids: includes dumped dicts
	-> Bag CallInstance	-- Kept call instances

   	-- CIs are dumped if:
	--   1) they are a CI for one of the bound ids, or
	--   2) they mention any of the dicts in a local unfloated binding
	--
	-- For top-level bindings we allow the call instances to
	-- float past a dict bind and place all the top-level binds
	-- in a *global* Rec.
	-- We leave it to the simplifier will sort it all out ...

dumpCIs cis top_lev floating inst_cis bound_ids full_ids
 = (if not (isEmptyBag cis_of_bound_id) &&
       not (isEmptyBag cis_of_bound_id_without_inst_cis)
    then
       pprTrace ("dumpCIs: dumping CI which was not instantiated ... \n" ++
		 "         (may be a non-HM recursive call)\n")
       (ppHang (ppBesides [ppStr "{",
			   interppSP PprDebug bound_ids,
			   ppStr "}"])
	     4 (ppAboves [ppStr "Dumping CIs:",
			  ppAboves (map pprCI (bagToList cis_of_bound_id)),
			  ppStr "Instantiating CIs:",
			  ppAboves (map pprCI inst_cis)]))
    else id) (
   if top_lev || floating then
       cis_not_bound_id
   else
       (if not (isEmptyBag cis_dump_unboxed)
	then pprTrace "dumpCIs: bound dictionary arg ... WITH UNBOXED TYPES!\n"
	     (ppHang (ppBesides [ppStr "{",
				 interppSP PprDebug full_ids,
				 ppStr "}"])
		   4 (ppAboves (map pprCI (bagToList cis_dump))))
	else id)
       cis_keep_not_bound_id
   )
 where
   (cis_of_bound_id, cis_not_bound_id)
      = partitionBag (isCIofTheseIds (getCIids top_lev bound_ids)) cis

   (cis_dump, cis_keep_not_bound_id)
      = partitionBag ok_to_dump_ci cis_not_bound_id

   ok_to_dump_ci (CallInstance _ _ _ fv_set _)
	= any (\ i -> i `elementOfIdSet` fv_set) full_ids

   (_, cis_of_bound_id_without_inst_cis) = partitionBag have_inst_ci cis_of_bound_id
   have_inst_ci ci = any (eqCI_tys ci) inst_cis

   (cis_dump_unboxed, _) = partitionBag isUnboxedCI cis_dump

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

We check this in dumpCIs by passing in all the instantiated call
instances (inst_cis) and reporting any dumped cis (cis_of_bound_id)
for which there is no such instance.

We also report CIs dumped due to a bound dictionary arg if they
contain unboxed types.

%************************************************************************
%*									*
\subsubsection[TyConInstances]{@TyConInstances@ data type}
%*									*
%************************************************************************

\begin{code}
data TyConInstance
  = TyConInstance TyCon			-- Type Constructor
		  [Maybe Type]	-- Applied to these specialising types

cmpTyConI :: TyConInstance -> TyConInstance -> TAG_
cmpTyConI (TyConInstance tc1 tys1) (TyConInstance tc2 tys2)
  = cmp tc1 tc2 `thenCmp` cmpUniTypeMaybeList tys1 tys2

cmpTyConI_tys :: TyConInstance -> TyConInstance -> TAG_
cmpTyConI_tys (TyConInstance _ tys1) (TyConInstance _ tys2)
  = cmpUniTypeMaybeList tys1 tys2

singleTyConI :: TyCon -> [Maybe Type] -> UsageDetails
singleTyConI ty_con spec_tys
  = UsageDetails emptyBag (unitBag (TyConInstance ty_con spec_tys)) [] emptyIdSet 0 0

isTyConIofThisTyCon :: TyCon -> TyConInstance -> Bool
isTyConIofThisTyCon ty_con (TyConInstance inst_ty_con _) = ty_con == inst_ty_con

isLocalSpecTyConI :: Bool -> TyConInstance -> Bool
isLocalSpecTyConI comp_prel (TyConInstance inst_ty_con _) = isLocalSpecTyCon comp_prel inst_ty_con

getLocalSpecTyConIs :: Bool -> UsageDetails -> ([TyConInstance], UsageDetails)
getLocalSpecTyConIs comp_prel (UsageDetails cis tycon_cis dbs fvs c i)
  = let
	(tycon_cis_local, tycon_cis_global)
	  = partitionBag (isLocalSpecTyConI comp_prel) tycon_cis
	tycon_cis_local_list = bagToList tycon_cis_local
    in
    (tycon_cis_local_list, UsageDetails cis tycon_cis_global dbs fvs c i)
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
	Int			-- no. of spec calls
	Int			-- no. of spec insts
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
	CoreBinding	-- Fully processed
	FreeVarsSet		-- Free in binding group (cloned)
	FreeTyVarsSet		-- Free in binding group
\end{code}

\begin{code}
emptyUDs    :: UsageDetails
unionUDs    :: UsageDetails -> UsageDetails -> UsageDetails
unionUDList :: [UsageDetails] -> UsageDetails

-- tickSpecCall :: Bool -> UsageDetails -> UsageDetails
tickSpecInsts :: UsageDetails -> UsageDetails

-- tickSpecCall found (UsageDetails cis ty_cis dbs fvs c i)
-- = UsageDetails cis ty_cis dbs fvs (c + (if found then 1 else 0)) i

tickSpecInsts (UsageDetails cis ty_cis dbs fvs c i)
 = UsageDetails cis ty_cis dbs fvs c (i+1)

emptyUDs = UsageDetails emptyBag emptyBag [] emptyIdSet 0 0

unionUDs (UsageDetails cis1 tycon_cis1 dbs1 fvs1 c1 i1) (UsageDetails cis2 tycon_cis2 dbs2 fvs2 c2 i2)
 = UsageDetails (unionBags cis1 cis2) (unionBags tycon_cis1 tycon_cis2)
		(dbs1 ++ dbs2) (fvs1 `unionIdSets` fvs2) (c1+c2) (i1+i2)
	-- The append here is really redundant, since the bindings don't
	-- scope over each other.  ToDo.

unionUDList = foldr unionUDs emptyUDs

singleFvUDs (VarArg v) | not (isImportedId v)
 = UsageDetails emptyBag emptyBag [] (unitIdSet v) 0 0
singleFvUDs other
 = emptyUDs

singleConUDs con = UsageDetails emptyBag emptyBag [] (unitIdSet con) 0 0

dumpDBs :: [DictBindDetails]
	-> Bool			-- True <=> top level bound Ids
	-> [TyVar]		-- TyVars being bound (cloned)
	-> [Id]			-- Ids being bound (cloned)
	-> FreeVarsSet		-- Fvs of body
	-> ([CoreBinding],	-- These ones have to go here
	    [DictBindDetails],	-- These can float further
	    [Id],		-- Incoming list + names of dicts bound here
	    FreeVarsSet		-- Incoming fvs + fvs of dicts bound here
	   )

 	-- It is just to complex to try to float top-level
	-- dict bindings with constant methods, inst methods,
	-- auxillary derived instance defns and user instance
	-- defns all getting in the way.
	-- So we dump all dbinds as soon as we get to the top
	-- level and place them in a *global* Rec.
	-- We leave it to the simplifier will sort it all out ...

dumpDBs [] top_lev bound_tyvars bound_ids fvs
  = ([], [], bound_ids, fvs)

dumpDBs ((db@(DictBindDetails dbinders dbind db_fvs db_ftv)):dbs)
	top_lev bound_tyvars bound_ids fvs
  | top_lev
    || any (\ i -> i `elementOfIdSet`    db_fvs) bound_ids
    || any (\ t -> t `elementOfTyVarSet` db_ftv) bound_tyvars
  = let		-- Ha!  Dump it!
	(dbinds_here, dbs_outer, full_bound_ids, full_fvs)
	   = dumpDBs dbs top_lev bound_tyvars (dbinders ++ bound_ids) (db_fvs `unionIdSets` fvs)
    in
    (dbind : dbinds_here, dbs_outer, full_bound_ids, full_fvs)

  | otherwise	-- This one can float out further
  = let
	(dbinds_here, dbs_outer, full_bound_ids, full_fvs)
	   = dumpDBs dbs top_lev bound_tyvars bound_ids fvs
    in
    (dbinds_here, db : dbs_outer, full_bound_ids, full_fvs)



dumpUDs :: UsageDetails
	-> Bool			-- True <=> top level bound Ids
	-> Bool			-- True <=> dict bindings to be floated (specBind only)
	-> [CallInstance]	-- Call insts for bound Ids (instBind only)
	-> [Id]			-- Ids which are just being bound; *new*
	-> [TyVar]		-- TyVars which are just being bound
	-> ([CoreBinding],	-- Bindings from UsageDetails which mention the ids
	    UsageDetails)	-- The above bindings removed, and
				-- any call-instances which mention the ids dumped too

dumpUDs (UsageDetails cis tycon_cis dbs fvs c i) top_lev floating inst_cis bound_ids tvs
  = let
	(dict_binds_here, dbs_outer, full_bound_ids, full_fvs)
		  = dumpDBs dbs top_lev tvs bound_ids fvs
	cis_outer = dumpCIs cis top_lev floating inst_cis bound_ids full_bound_ids
	fvs_outer = full_fvs `minusIdSet` (mkIdSet full_bound_ids)
    in
    (dict_binds_here, UsageDetails cis_outer tycon_cis dbs_outer fvs_outer c i)
\end{code}

\begin{code}
addDictBinds :: [Id] -> CoreBinding -> UsageDetails	-- Dict binding and RHS usage
	     -> UsageDetails	 				-- The usage to augment
	     -> UsageDetails
addDictBinds dbinders dbind (UsageDetails db_cis db_tycon_cis db_dbs db_fvs db_c db_i)
	 	            (UsageDetails cis    tycon_cis    dbs    fvs    c    i)
  = UsageDetails (db_cis `unionBags` cis)
		 (db_tycon_cis `unionBags` tycon_cis)
		 (db_dbs ++ [DictBindDetails dbinders dbind db_fvs db_ftvs] ++ dbs)
		 fvs c i
		 -- NB: We ignore counts from dictbinds since it is not user code
  where
	-- The free tyvars of the dictionary bindings should really be
	-- gotten from the RHSs, but I'm pretty sure it's good enough just
	-- to look at the type of the dictionary itself.
	-- Doing the proper job would entail keeping track of free tyvars as
	-- well as free vars, which would be a bore.
    db_ftvs = tyVarsOfTypes (map idType dbinders)
\end{code}

%************************************************************************
%*									*
\subsection[cloning-binders]{The Specialising IdEnv and CloneInfo}
%*									*
%************************************************************************

@SpecIdEnv@ maps old Ids to their new "clone". There are three cases:

1) (NoLift LitArg l) : an Id which is bound to a literal

2) (NoLift LitArg l) : an Id bound to a "new" Id
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
 = NoLift CoreArg	-- refers to cloned id or literal

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

	    (FiniteMap TyCon [(Bool, [Maybe Type])])
		-- TyCon specialisations to be generated
		-- We generate specialialised code (Bool=True) for data types
		-- defined in this module and any tuples used in this module
		-- The initial (and default) value is the specialisations
		-- requested by source-level SPECIALIZE data pragmas (Bool=True)
		-- and _SPECIALISE_ pragmas (Bool=False) in the interface files

	    (Bag (Id,[Maybe Type]))
		-- Imported specialisation errors
	    (Bag (Id,[Maybe Type]))
		-- Imported specialisation warnings
	    (Bag (TyCon,[Maybe Type]))
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
specProgram :: UniqSupply
	    -> [CoreBinding]	-- input ...
	    -> SpecialiseData
	    -> ([CoreBinding],	-- main result
		SpecialiseData)		-- result specialise data

specProgram uniqs binds
	   (SpecData False _ local_tycons _ init_specs init_errs init_warn init_tyerrs)
  = case (initSM (specTyConsAndScope (specTopBinds binds)) uniqs) of
      (final_binds, tycon_specs_list,
	UsageDetails import_cis import_tycis _ fvs spec_calls spec_insts)
	 -> let
		used_conids   = filter isDataCon (uniqSetToList fvs)
		used_tycons   = map dataConTyCon used_conids
		used_gen      = filter isLocalGenTyCon used_tycons
		gen_tycons    = uniqSetToList (mkUniqSet local_tycons `unionUniqSets` mkUniqSet used_gen)

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
				  && (not opt_SpecialiseImports || isEmptyBag cis_warn)
	    in
	    (if opt_D_simplifier_stats then
		pprTrace "\nSpecialiser Stats:\n" (ppAboves [
					ppBesides [ppStr "SpecCalls  ", ppInt spec_calls],
					ppBesides [ppStr "SpecInsts  ", ppInt spec_insts],
					ppSP])
	     else id)

	    (final_binds,
	     SpecData True no_errs local_tycons gen_tycons result_specs
				   cis_errs cis_warn tycis_errs)

specProgram uniqs binds (SpecData True _ _ _ _ _ _ _)
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

ToDo: Perhaps this collection should be done in CoreToStg to ensure no inconsistencies!

\begin{code}
specTyConsAndScope :: SpecM ([CoreBinding], UsageDetails)
	   	   -> SpecM ([CoreBinding], [(TyCon,[(Bool,[Maybe Type])])], UsageDetails)

specTyConsAndScope scopeM
  = scopeM			`thenSM` \ (binds, scope_uds) ->
    let
       (tycons_cis, gotci_scope_uds)
	 = getLocalSpecTyConIs opt_CompilingGhcInternals scope_uds

       tycon_specs_list = collectTyConSpecs tycons_cis
    in
    (if opt_SpecialiseTrace && not (null tycon_specs_list) then
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
	tycon_specs = [(False, spec_tys) | TyConInstance _ spec_tys <- uniq_cis]

    pp_specs (False, spec_tys) = ppInterleave ppNil [pprMaybeTy PprDebug spec_ty | spec_ty <- spec_tys]

\end{code}

%************************************************************************
%*									*
\subsection[specTopBinds]{Specialising top-level bindings}
%*									*
%************************************************************************

\begin{code}
specTopBinds :: [CoreBinding]
	     -> SpecM ([CoreBinding], UsageDetails)

specTopBinds binds
  = spec_top_binds binds    `thenSM`  \ (binds, UsageDetails cis tycis dbind_details fvs c i) ->
    let
	-- Add bindings for floated dbinds and collect fvs
	-- In actual fact many of these bindings are dead code since dict
	-- arguments are dropped when a specialised call is created
	-- The simplifier should be able to cope ...

	(dbinders_s, dbinds, dfvs_s)
	   = unzip3 [(dbinders, dbind, dfvs) | DictBindDetails dbinders dbind dfvs _ <- dbind_details]

	full_fvs  = fvs `unionIdSets` unionManyIdSets dfvs_s
	fvs_outer = full_fvs `minusIdSet` (mkIdSet (concat dbinders_s))

 	-- It is just to complex to try to sort out top-level dependencies
	-- So we just place all the top-level binds in a *global* Rec and
	-- leave it to the simplifier to sort it all out ...
    in
    ASSERT(null dbinds)
    returnSM ([Rec (pairsFromCoreBinds binds)], UsageDetails cis tycis [] fvs_outer c i)

  where
    spec_top_binds (first_bind:rest_binds)
      = specBindAndScope True first_bind (
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
specExpr :: CoreExpr
	 -> [CoreArg]		-- The arguments:
				--    TypeArgs are speced
				--    ValArgs are unprocessed
	 -> SpecM (CoreExpr, 	-- Result expression with specialised versions installed
		   UsageDetails)-- Details of usage of enclosing binders in the result
				-- expression.

specExpr (Var v) args
  = lookupId v			`thenSM` \ vlookup ->
    case vlookup of
       Lifted vl vu
	     -> -- Binding has been lifted, need to extract un-lifted value
		-- NB: a function binding will never be lifted => args always null
		--     i.e. no call instance required or call to be constructed
		ASSERT (null args)
		returnSM (bindUnlift vl vu (Var vu), singleFvUDs (VarArg vl))

       NoLift vatom@(VarArg new_v)
	     -> mapSM specOutArg args			`thenSM` \ arg_info ->
		mkCallInstance v new_v arg_info		`thenSM` \ call_uds ->
    		mkCall new_v arg_info			`thenSM` \ call ->
		let
		    uds = unionUDList [call_uds,
				       singleFvUDs vatom,
				       unionUDList [uds | (_,uds,_) <- arg_info]
				      ]
		in
    		returnSM (call, {- tickSpecCall speced -} uds)

specExpr expr@(Lit _) null_args
  = ASSERT (null null_args)
    returnSM (expr, emptyUDs)

specExpr (Con con args) null_args
  = ASSERT (null null_args)
    let
	(targs, vargs) = partition_args args
    in
    mapAndUnzipSM  specTyArg  targs `thenSM` \ (targs, tys) ->
    mapAndUnzip3SM specValArg vargs `thenSM` \ (vargs, args_uds_s, unlifts) ->
    mkTyConInstance con tys	    `thenSM` \ con_uds ->
    returnSM (applyBindUnlifts unlifts (Con con (targs ++ vargs)),
	      unionUDList args_uds_s `unionUDs` con_uds)

specExpr (Prim op@(CCallOp str is_asm may_gc arg_tys res_ty) args) null_args
  = ASSERT (null null_args)
    let
	(targs, vargs) = partition_args args
    in
    ASSERT (null targs)
    mapSM specTy arg_tys	    `thenSM` \ arg_tys ->
    specTy res_ty		    `thenSM` \ res_ty ->
    mapAndUnzip3SM specValArg vargs `thenSM` \ (vargs, args_uds_s, unlifts) ->
    returnSM (applyBindUnlifts unlifts (Prim (CCallOp str is_asm may_gc arg_tys res_ty) vargs),
	      unionUDList args_uds_s)

specExpr (Prim prim args) null_args
  = ASSERT (null null_args)
    let
	(targs, vargs) = partition_args args
    in
    mapAndUnzipSM  specTyArg  targs `thenSM` \ (targs, tys) ->
    mapAndUnzip3SM specValArg vargs `thenSM` \ (vargs, args_uds_s, unlifts) ->
    -- specPrimOp prim tys		`thenSM` \ (prim, tys, prim_uds) ->
    returnSM (applyBindUnlifts unlifts (Prim prim (targs ++ vargs)),
	      unionUDList args_uds_s {-`unionUDs` prim_uds-} )

{- ToDo: specPrimOp

specPrimOp :: PrimOp
	   -> [Type]
	   -> SpecM (PrimOp,
		     [Type],
	 	     UsageDetails)

-- Checks that PrimOp can handle (possibly unboxed) tys passed
--   and/or chooses PrimOp specialised to any unboxed tys
-- Errors are dealt with by returning a PrimOp call instance
--   which will result in a cis_errs message

-- ToDo: Deal with checkSpecTyApp for Prim in CoreLint
-}


specExpr (App fun arg) args
  = 	-- If TyArg, arg will be processed; otherwise, left alone
    preSpecArg arg 			`thenSM` \ new_arg    ->
    specExpr   fun (new_arg : args)	`thenSM` \ (expr,uds) ->
    returnSM (expr, uds)

specExpr (Lam (ValBinder binder) body) (arg : args) | isValArg arg
  = lookup_arg arg `thenSM` \ arg ->
    bindId binder arg (specExpr body args)
  where
    lookup_arg (LitArg l) = returnSM (NoLift (LitArg l))
    lookup_arg (VarArg v) = lookupId v

specExpr (Lam (ValBinder binder) body) []
  = specLambdaOrCaseBody [binder] body [] `thenSM` \ ([binder], body, uds) ->
    returnSM (Lam (ValBinder binder) body, uds)

specExpr (Lam (TyBinder tyvar) body) (TyArg ty : args)
  =	-- Type lambda with argument; argument already spec'd
    bindTyVar tyvar ty ( specExpr body args )

specExpr (Lam (TyBinder tyvar) body) []
  = 	-- No arguments
    cloneTyVarSM tyvar 		`thenSM` \ new_tyvar ->
    bindTyVar tyvar (mkTyVarTy new_tyvar) (
	specExpr body []	`thenSM` \ (body, body_uds) ->
	let
	    (binds_here, final_uds) = dumpUDs body_uds False False [] [] [new_tyvar]
	in
	returnSM (Lam (TyBinder new_tyvar)
		      (mkCoLetsNoUnboxed binds_here body),
		  final_uds)
    )

specExpr (Case scrutinee alts) args
  = specExpr scrutinee []		`thenSM` \ (scrutinee, scrut_uds) ->
    specAlts alts scrutinee_type args	`thenSM` \ (alts, alts_uds) ->
    returnSM (Case scrutinee alts, scrut_uds `unionUDs`  alts_uds)
  where
    scrutinee_type = coreExprType scrutinee

specExpr (Let bind body) args
  = specBindAndScope False bind (
	specExpr body args 	`thenSM` \ (body, body_uds) ->
	returnSM (ItsAnExpr body, body_uds)
    )				`thenSM` \ (binds, ItsAnExpr body, all_uds) ->
    returnSM (mkCoLetsUnboxedToCase binds body, all_uds)

specExpr (SCC cc expr) args
  = specExpr expr []		    `thenSM` \ (expr, expr_uds) ->
    mapAndUnzip3SM specOutArg args  `thenSM` \ (args, args_uds_s, unlifts) ->
    let
	scc_expr
	  = if squashableDictishCcExpr cc expr -- can toss the _scc_
	    then expr
	    else SCC cc expr
    in
    returnSM (applyBindUnlifts unlifts (mkGenApp scc_expr args),
	      unionUDList args_uds_s `unionUDs` expr_uds)

specExpr (Coerce _ _ _) args = panic "Specialise.specExpr:Coerce"

-- ToDo: This may leave some unspec'd dictionaries!!
\end{code}

%************************************************************************
%*									*
\subsubsection{Specialising a lambda}
%*									*
%************************************************************************

\begin{code}
specLambdaOrCaseBody :: [Id]			-- The binders
		     -> CoreExpr		-- The body
		     -> [CoreArg]		-- Its args
		     -> SpecM ([Id],		-- New binders
			       CoreExpr,	-- New body
			       UsageDetails)

specLambdaOrCaseBody bound_ids body args
 = cloneLambdaOrCaseBinders bound_ids 	`thenSM` \ (new_ids, clone_infos) ->
   bindIds bound_ids clone_infos (

	specExpr body args	`thenSM` \ (body, body_uds) ->

	let
	    -- Dump any dictionary bindings (and call instances)
	    -- from the scope which mention things bound here
 	    (binds_here, final_uds) = dumpUDs body_uds False False [] new_ids []
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
specAlts (AlgAlts alts deflt) scrutinee_ty args
  = mapSM specTy ty_args 			`thenSM` \ ty_args ->
    mapAndUnzipSM (specAlgAlt ty_args) alts	`thenSM` \ (alts, alts_uds_s) ->
    specDeflt deflt args			`thenSM` \ (deflt, deflt_uds) ->
    returnSM (AlgAlts alts deflt,
	      unionUDList alts_uds_s `unionUDs` deflt_uds)
  where
    -- We use ty_args of scrutinee type to identify specialisation of
    -- alternatives:

    (_, ty_args, _) = --trace "Specialise.specAlts:getAppData..." $
		      getAppDataTyConExpandingDicts scrutinee_ty

    specAlgAlt ty_args (con,binders,rhs)
      = specLambdaOrCaseBody binders rhs args	`thenSM` \ (binders, rhs, rhs_uds) ->
	mkTyConInstance con ty_args    		`thenSM` \ con_uds ->
	returnSM ((con,binders,rhs), rhs_uds `unionUDs` con_uds)

specAlts (PrimAlts alts deflt) scrutinee_ty args
  = mapAndUnzipSM specPrimAlt alts	`thenSM` \ (alts, alts_uds_s) ->
    specDeflt deflt args		`thenSM` \ (deflt, deflt_uds) ->
    returnSM (PrimAlts alts deflt,
	      unionUDList alts_uds_s `unionUDs` deflt_uds)
  where
    specPrimAlt (lit,rhs) = specExpr rhs args	`thenSM` \ (rhs, uds) ->
			    returnSM ((lit,rhs), uds)


specDeflt NoDefault args = returnSM (NoDefault, emptyUDs)
specDeflt (BindDefault binder rhs) args
 = specLambdaOrCaseBody [binder] rhs args	`thenSM` \ ([binder], rhs, uds) ->
   returnSM (BindDefault binder rhs, uds)
\end{code}


%************************************************************************
%*									*
\subsubsection{Specialising an atom}
%*									*
%************************************************************************

\begin{code}
partition_args :: [CoreArg] -> ([CoreArg], [CoreArg])
partition_args args
  = span is_ty_arg args
  where
    is_ty_arg (TyArg _) = True
    is_ty_arg _		= False

----------
preSpecArg :: CoreArg -> SpecM CoreArg -- diddle TyArgs, but nothing else

preSpecArg (TyArg ty)
  = specTy ty	`thenSM` \ new_ty ->
    returnSM (TyArg new_ty)

preSpecArg other = returnSM other

--------------------
specValArg :: CoreArg -> SpecM (CoreArg, UsageDetails,
				CoreExpr -> CoreExpr)

specValArg (LitArg lit)
  = returnSM (LitArg lit, emptyUDs, id)

specValArg (VarArg v)
  = lookupId v		`thenSM` \ vlookup ->
    case vlookup of
      Lifted vl vu
	 -> returnSM (VarArg vu, singleFvUDs (VarArg vl), bindUnlift vl vu)

      NoLift vatom
	 -> returnSM (vatom, singleFvUDs vatom, id)


------------------
specTyArg (TyArg ty)
  = specTy ty	`thenSM` \ new_ty ->
    returnSM (TyArg new_ty, new_ty)

--------------
specOutArg :: CoreArg -> SpecM (CoreArg, UsageDetails,
				  CoreExpr -> CoreExpr)

specOutArg (TyArg ty)	-- already speced; no action
  = returnSM (TyArg ty, emptyUDs, id)

specOutArg other_arg	-- unprocessed; spec the atom
  = specValArg other_arg
\end{code}


%************************************************************************
%*									*
\subsubsection{Specialising bindings}
%*									*
%************************************************************************

A classic case of when having a polymorphic recursive function would help!

\begin{code}
data BindsOrExpr = ItsABinds [CoreBinding]
		 | ItsAnExpr CoreExpr
\end{code}

\begin{code}
specBindAndScope
	:: Bool					-- True <=> a top level group
	-> CoreBinding			-- As yet unprocessed
	-> SpecM (BindsOrExpr, UsageDetails)	-- Something to do the scope of the bindings
	-> SpecM ([CoreBinding],		-- Processed
		  BindsOrExpr, 			-- Combined result
		  UsageDetails)			-- Usage details of the whole lot

specBindAndScope top_lev bind scopeM
  = cloneLetBinders top_lev (is_rec bind) binders
				`thenSM` \ (new_binders, clone_infos) ->

	-- Two cases now: either this is a bunch of local dictionaries,
	-- in which case we float them; or its a bunch of other values,
	-- in which case we see if they correspond to any call-instances
	-- we have from processing the scope

    if not top_lev && all (isDictTy . idType) binders
    then
	-- Ha! A group of local dictionary bindings

      bindIds binders clone_infos (

		-- Process the dictionary bindings themselves
	specBind False True new_binders [] bind	`thenSM` \ (bind, rhs_uds) ->

		-- Process their scope
	scopeM					`thenSM` \ (thing, scope_uds) ->
	let
		-- Add the bindings to the current stuff
	    final_uds = addDictBinds new_binders bind rhs_uds scope_uds
	in
	returnSM ([], thing, final_uds)
      )
    else
	-- Ho! A group of bindings

      fixSM (\ ~(_, _, _, rec_spec_infos) ->

	bindSpecIds binders clone_infos rec_spec_infos (
		-- It's ok to have new binders in scope in
		-- non-recursive decls too, cos name shadowing is gone by now

		-- Do the scope of the bindings
	  scopeM				`thenSM` \ (thing, scope_uds) ->
	  let
	     (call_insts, gotci_scope_uds) = getCIs top_lev new_binders scope_uds

	     equiv_ciss = equivClasses cmpCI_tys call_insts
	     inst_cis   = map head equiv_ciss
	  in

		-- Do the bindings themselves
	  specBind top_lev False new_binders inst_cis bind
						`thenSM` \ (spec_bind, spec_uds) ->

		-- Create any necessary instances
	  instBind top_lev new_binders bind equiv_ciss inst_cis
						`thenSM` \ (inst_binds, inst_uds, spec_infos) ->

	  let
		-- NB: dumpUDs only worries about new_binders since the free var
		--     stuff only records free new_binders
		--     The spec_ids only appear in SpecInfos and final speced calls

		-- Build final binding group and usage details
		(final_binds, final_uds)
		  = if top_lev then
			-- For a top-level binding we have to dumpUDs from
			-- spec_uds and inst_uds and scope_uds creating
			-- *global* dict bindings
			let
	    		    (scope_dict_binds, final_scope_uds)
		  	      = dumpUDs gotci_scope_uds True False [] new_binders []
			    (spec_dict_binds, final_spec_uds)
		  	      = dumpUDs spec_uds True False inst_cis new_binders []
			    (inst_dict_binds, final_inst_uds)
			      = dumpUDs inst_uds True False inst_cis new_binders []
			in
			([spec_bind] ++ inst_binds ++ scope_dict_binds
			   ++ spec_dict_binds ++ inst_dict_binds,
			 final_spec_uds `unionUDs` final_scope_uds `unionUDs` final_inst_uds)
		    else
			-- For a local binding we only have to dumpUDs from
			-- scope_uds since the UDs from spec_uds and inst_uds
			-- have already been dumped by specBind and instBind
			let
			    (scope_dict_binds, final_scope_uds)
		  	      = dumpUDs gotci_scope_uds False False [] new_binders []
			in
			([spec_bind] ++ inst_binds ++ scope_dict_binds,
			 spec_uds `unionUDs` final_scope_uds `unionUDs` inst_uds)

		-- inst_uds comes last, because there may be dict bindings
		-- floating outward in scope_uds which are mentioned
		-- in the call-instances, and hence in spec_uds.
		-- This ordering makes sure that the precedence order
		-- among the dict bindings finally floated out is maintained.
	  in
	  returnSM (final_binds, thing, final_uds, spec_infos)
	)
      )			`thenSM` 	\ (binds, thing, final_uds, spec_infos) ->
      returnSM (binds, thing, final_uds)
  where
    binders = bindersOf bind

    is_rec (NonRec _ _) = False
    is_rec _	          = True
\end{code}

\begin{code}
specBind :: Bool -> Bool -> [Id] -> [CallInstance]
	 -> CoreBinding
	 -> SpecM (CoreBinding, UsageDetails)
	-- The UsageDetails returned has already had stuff to do with this group
	-- of binders deleted; that's why new_binders is passed in.
specBind top_lev floating new_binders inst_cis (NonRec binder rhs)
  = specOneBinding top_lev floating new_binders inst_cis (binder,rhs)
							`thenSM` \ ((binder,rhs), rhs_uds) ->
    returnSM (NonRec binder rhs, rhs_uds)

specBind top_lev floating new_binders inst_cis (Rec pairs)
  = mapAndUnzipSM (specOneBinding top_lev floating new_binders inst_cis) pairs
							`thenSM` \ (pairs, rhs_uds_s) ->
    returnSM (Rec pairs, unionUDList rhs_uds_s)


specOneBinding :: Bool -> Bool -> [Id] -> [CallInstance]
	       -> (Id,CoreExpr)
	       -> SpecM ((Id,CoreExpr), UsageDetails)

specOneBinding top_lev floating new_binders inst_cis (binder, rhs)
  = lookupId binder		`thenSM` \ blookup ->
    specExpr rhs []		`thenSM` \ (rhs, rhs_uds) ->
    let
	specid_maybe_maybe  = isSpecPragmaId_maybe binder
	is_specid           = maybeToBool specid_maybe_maybe
	Just specinfo_maybe = specid_maybe_maybe
	specid_with_info    = maybeToBool specinfo_maybe
	Just spec_info      = specinfo_maybe

	-- If we have a SpecInfo stored in a SpecPragmaId binder
	-- it will contain a SpecInfo with an explicit SpecId
	-- We add the explicit ci to the usage details
	-- Any ordinary cis for orig_id (there should only be one)
	-- will be ignored later

	pragma_uds
	  = if is_specid && specid_with_info then
		let
		    (SpecInfo spec_tys _ spec_id) = spec_info
		    Just (orig_id, _) = isSpecId_maybe spec_id
		in
		ASSERT(toplevelishId orig_id)     -- must not be cloned!
		explicitCI orig_id spec_tys spec_info
	    else
		emptyUDs

	-- For a local binding we dump the usage details, creating
	-- any local dict bindings required
	-- At the top-level the uds will be dumped in specBindAndScope
	-- and the dict bindings made *global*

	(local_dict_binds, final_uds)
	  = if not top_lev then
		dumpUDs rhs_uds False floating inst_cis new_binders []
	    else
		([], rhs_uds)
    in
    case blookup of
	Lifted lift_binder unlift_binder
	  -> 	-- We may need to record an unboxed instance of
		-- the _Lift data type in the usage details
	     mkTyConInstance liftDataCon [idType unlift_binder]
						`thenSM` \ lift_uds ->
	     returnSM ((lift_binder,
			mkCoLetsNoUnboxed local_dict_binds (liftExpr unlift_binder rhs)),
		       final_uds `unionUDs` pragma_uds `unionUDs` lift_uds)

	NoLift (VarArg binder)
	  -> returnSM ((binder, mkCoLetsNoUnboxed local_dict_binds rhs),
		       final_uds `unionUDs` pragma_uds)
\end{code}


%************************************************************************
%*									*
\subsection{@instBind@}
%*									*
%************************************************************************

\begin{code}
instBind top_lev new_ids@(first_binder:other_binders) bind equiv_ciss inst_cis
 | null equiv_ciss
 = returnSM ([], emptyUDs, [])

 | all same_overloading other_binders
 = 	-- For each call_inst, build an instance
   mapAndUnzip3SM do_this_class equiv_ciss
	`thenSM` \ (inst_binds, inst_uds_s, spec_infos) ->

	-- Add in the remaining UDs
   returnSM (catMaybes inst_binds,
   	     unionUDList inst_uds_s,
	     spec_infos
	    )

 | otherwise		-- Incompatible overloadings; see below by same_overloading
 = (if not (null (filter isUnboxedCI (concat equiv_ciss)))
    then pprTrace "dumpCIs: not same overloading ... WITH UNBOXED TYPES!\n"
    else if top_lev
    then pprTrace "dumpCIs: not same overloading ... top level \n"
    else (\ x y -> y)
   ) (ppHang (ppBesides [ppStr "{",
			 interppSP PprDebug new_ids,
			 ppStr "}"])
   	   4 (ppAboves [ppAboves (map (pprGenType PprDebug . idType) new_ids),
			ppAboves (map pprCI (concat equiv_ciss))]))
   (returnSM ([], emptyUDs, []))

 where
    (tyvar_tmpls, class_tyvar_pairs) = getIdOverloading first_binder
    tyvar_tmpl_tys = mkTyVarTys tyvar_tmpls

    no_of_tyvars = length tyvar_tmpls
    no_of_dicts  = length class_tyvar_pairs

    do_this_class equiv_cis
      = mkOneInst do_cis explicit_cis no_of_dicts top_lev inst_cis new_ids bind
      where
	(explicit_cis, normal_cis) = partition isExplicitCI equiv_cis
	do_cis = head (normal_cis ++ explicit_cis)
	-- must choose a normal_cis in preference since dict_args will
	-- not be defined for an explicit_cis

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
      = no_of_tyvars == length this_id_tyvars
	-- Same no of tyvars
	&& no_of_dicts == length this_id_class_tyvar_pairs
	-- Same no of vdicts
	&& and (zipWith same_ov class_tyvar_pairs this_id_class_tyvar_pairs)
	&& length class_tyvar_pairs == length this_id_class_tyvar_pairs
	-- Same overloading
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
	  -> [CallInstance]			-- Any explicit cis for this inst
	  -> Int				-- No of dicts to specialise
	  -> Bool				-- Top level binders?
	  -> [CallInstance]			-- Instantiated call insts for binders
	  -> [Id]				-- New binders
	  -> CoreBinding			-- Unprocessed
	  -> SpecM (Maybe CoreBinding,	-- Instantiated version of input
		    UsageDetails,
		    [Maybe SpecInfo]		-- One for each id in the original binding
		   )

mkOneInst do_cis@(CallInstance _ spec_tys dict_args _ _) explicit_cis
	  no_of_dicts_to_specialise top_lev inst_cis new_ids orig_bind
  = newSpecIds new_ids spec_tys no_of_dicts_to_specialise
							`thenSM` \ spec_ids ->
    newTyVars (length [() | Nothing <- spec_tys])   	`thenSM` \ poly_tyvars ->
    let
	-- arg_tys is spec_tys with tyvars instead of the Nothing spec_tys
	-- which correspond to unspeciailsed args
	arg_tys  :: [Type]
	(_,arg_tys) = mapAccumL do_the_wotsit poly_tyvars spec_tys

	args :: [CoreArg]
	args = map TyArg arg_tys ++ dict_args

	(new_id:_) = new_ids
	(spec_id:_) = spec_ids

	do_bind (NonRec orig_id rhs)
	  = do_one_rhs (spec_id, new_id, (orig_id,rhs))
					`thenSM` \ (maybe_spec, rhs_uds, spec_info) ->
	    case maybe_spec of
		Just (spec_id, rhs) -> returnSM (Just (NonRec spec_id rhs), rhs_uds, [spec_info])
		Nothing 	    -> returnSM (Nothing, rhs_uds, [spec_info])

	do_bind (Rec pairs)
	  = mapAndUnzip3SM do_one_rhs (zip3 spec_ids new_ids pairs)
					`thenSM` \ (maybe_pairs, rhss_uds_s, spec_infos) ->
	    returnSM (Just (Rec (catMaybes maybe_pairs)),
		      unionUDList rhss_uds_s, spec_infos)

	do_one_rhs (spec_id, new_id, (orig_id, orig_rhs))

		-- Avoid duplicating a spec which has already been created ...
		-- This can arise in a Rec involving a dfun for which a
		-- a specialised instance has been created but specialisation
		-- "required" by one of the other Ids in the Rec
	  | top_lev && maybeToBool lookup_orig_spec
	  = (if opt_SpecialiseTrace
	     then trace_nospec "  Exists: " orig_id
	     else id) (

	    returnSM (Nothing, emptyUDs, Nothing)
	    )

		-- Check for a (single) explicit call instance for this id
	  | not (null explicit_cis_for_this_id)
	  = ASSERT (length explicit_cis_for_this_id == 1)
	    (if opt_SpecialiseTrace
	     then trace_nospec "  Explicit: " explicit_id
	     else id) (

	    returnSM (Nothing, tickSpecInsts emptyUDs, Just explicit_spec_info)
	    )

		-- Apply the specialiser to (orig_rhs t1 a t3 d1 d2)
	  | otherwise
	  = ASSERT (no_of_dicts_to_specialise == length dict_args)
	    specExpr orig_rhs args	`thenSM` \ (inst_rhs, inst_uds) ->
	    let
		-- For a local binding we dump the usage details, creating
		-- any local dict bindings required
		-- At the top-level the uds will be dumped in specBindAndScope
		-- and the dict bindings made *global*

		(local_dict_binds, final_uds)
		  = if not top_lev then
	    	        dumpUDs inst_uds False False inst_cis new_ids []
		    else
	    	        ([], inst_uds)

		spec_info = Just (SpecInfo spec_tys no_of_dicts_to_specialise spec_id)
	    in
	    if isUnboxedType (idType spec_id) then
		ASSERT (null poly_tyvars)
		liftId spec_id		`thenSM` \ (lift_spec_id, unlift_spec_id) ->
		mkTyConInstance liftDataCon [idType unlift_spec_id]
	    				`thenSM` \ lift_uds ->
		returnSM (Just (lift_spec_id,
				mkCoLetsNoUnboxed local_dict_binds (liftExpr unlift_spec_id inst_rhs)),
			  tickSpecInsts (final_uds `unionUDs` lift_uds), spec_info)
	    else
		returnSM (Just (spec_id,
	    	                mkCoLetsNoUnboxed local_dict_binds (mkTyLam poly_tyvars inst_rhs)),
			  tickSpecInsts final_uds, spec_info)
	  where
	    lookup_orig_spec = lookupSpecEnv (getIdSpecialisation orig_id) arg_tys

	    explicit_cis_for_this_id = filter (isCIofTheseIds [new_id]) explicit_cis
	    [CallInstance _ _ _ _ (Just explicit_spec_info)] = explicit_cis_for_this_id
	    SpecInfo _ _ explicit_id = explicit_spec_info

	    trace_nospec :: String -> Id -> a -> a
	    trace_nospec str spec_id
	      = pprTrace str
	     	(ppCat [ppr PprDebug new_id, ppInterleave ppNil (map pp_ty arg_tys),
			ppStr "==>", ppr PprDebug spec_id])
    in
    (if opt_SpecialiseTrace then
	pprTrace "Specialising:"
	(ppHang (ppBesides [ppStr "{",
			    interppSP PprDebug new_ids,
			    ppStr "}"])
	      4 (ppAboves [
		 ppBesides [ppStr "types: ", ppInterleave ppNil (map pp_ty arg_tys)],
		 if isExplicitCI do_cis then ppNil else
		 ppBesides [ppStr "dicts: ", ppInterleave ppNil (map pp_dict dict_args)],
		 ppBesides [ppStr "specs: ", ppr PprDebug spec_ids]]))
     else id) (

    do_bind orig_bind		`thenSM` \ (maybe_inst_bind, inst_uds, spec_infos) ->

    returnSM (maybe_inst_bind, inst_uds, spec_infos)
    )
  where
    pp_dict d = ppr_arg PprDebug d
    pp_ty t   = pprParendGenType PprDebug t

    do_the_wotsit (tyvar:tyvars) Nothing   = (tyvars, mkTyVarTy tyvar)
    do_the_wotsit tyvars         (Just ty) = (tyvars, ty)

\end{code}

%************************************************************************
%*									*
\subsection[Misc]{Miscellaneous junk}
%*									*
%************************************************************************

\begin{code}
mkCallInstance :: Id
	       -> Id
	       -> [(CoreArg, UsageDetails, CoreExpr -> CoreExpr)]
	       -> SpecM UsageDetails

mkCallInstance id new_id []
  = returnSM emptyUDs

mkCallInstance id new_id args

	-- No specialised versions for "error" and friends are req'd.
	-- This is a special case in core lint etc.

  | isBottomingId id
  = returnSM emptyUDs

	-- No call instances for SuperDictSelIds
	-- These are a special case in mkCall

  | maybeToBool (isSuperDictSelId_maybe id)
  = returnSM emptyUDs

	-- There are also no call instances for ClassOpIds
	-- However, we need to process it to get any second-level call
	-- instances for a ConstMethodId extracted from its SpecEnv

  | otherwise
  = let
	spec_overloading = opt_SpecialiseOverloaded
	spec_unboxed     = opt_SpecialiseUnboxed
	spec_all	 = opt_SpecialiseAll

	(tyvars, class_tyvar_pairs) = getIdOverloading id

	arg_res = take_type_args tyvars class_tyvar_pairs args
	enough_args = maybeToBool arg_res

	(Just (tys, dicts, rest_args)) = arg_res

	record_spec id tys
	  = (record, lookup, spec_tys)
	  where
	    spec_tys = specialiseCallTys spec_all spec_unboxed spec_overloading
					 (mkConstraintVector id) tys

	    record = any (not . isTyVarTy) (catMaybes spec_tys)

	    lookup = lookupSpecEnv (getIdSpecialisation id) tys
    in
    if (not enough_args) then
	pprPanic "Specialise:recordCallInst: Unsaturated Type & Dict Application:\n\t"
		 (ppCat (ppr PprDebug id : map (ppr_arg PprDebug) [arg | (arg,_,_) <- args]))
    else
    case record_spec id tys of
	(False, _, _)
	     -> -- pprTrace "CallInst:NotReqd\n"
	   	-- (ppCat [ppr PprDebug id, ppCat (map (ppr PprDebug) args)])
		(returnSM emptyUDs)

	(True, Nothing, spec_tys)
	     -> if isClassOpId id then	-- No CIs for class ops, dfun will give SPEC inst
		    returnSM emptyUDs
		else
		    -- pprTrace "CallInst:Reqd\n"
		    -- (ppAboves [ppCat [ppr PprDebug id, ppCat (map (ppr PprDebug) args)],
		    --	          ppCat [ppStr "CI", ppCat (map (pprMaybeTy PprDebug) spec_tys),
	   	    --		                     ppCat (map (ppr PprDebug) dicts)]])
		    (returnSM (singleCI new_id spec_tys dicts))

	(True, Just (spec_id, tys_left, toss), _)
	     -> if maybeToBool (isConstMethodId_maybe spec_id) then
			-- If we got a const method spec_id see if further spec required
			-- NB: const method is top-level so spec_id will not be cloned
		    case record_spec spec_id tys_left of
		      (False, _, _)
		    	-> -- pprTrace "CallInst:Exists\n"
		    	   -- (ppAboves [ppCat [ppr PprDebug id, ppCat (map (ppr PprDebug) args)],
		    	   --	         ppCat [ppStr "->", ppr PprDebug spec_id,
		    	   --		        ppr PprDebug (tys_left ++ drop toss dicts)]])
			   (returnSM emptyUDs)

		      (True, Nothing, spec_tys)
			-> -- pprTrace "CallInst:Exists:Reqd\n"
		    	   -- (ppAboves [ppCat [ppr PprDebug id, ppCat (map (ppr PprDebug) args)],
		    	   --	         ppCat [ppStr "->", ppr PprDebug spec_id,
		    	   --		        ppr PprDebug (tys_left ++ drop toss dicts)],
			   --	         ppCat [ppStr "CI", ppCat (map (pprMaybeTy PprDebug) spec_tys),
	   		   --		                    ppCat (map (ppr PprDebug) (drop toss dicts))]])
			   (returnSM (singleCI spec_id spec_tys (drop toss dicts)))

		      (True, Just (spec_spec_id, tys_left_left, toss_toss), _)
			-> -- pprTrace "CallInst:Exists:Exists\n"
		    	   -- (ppAboves [ppCat [ppr PprDebug id, ppCat (map (ppr PprDebug) args)],
		    	   --	         ppCat [ppStr "->", ppr PprDebug spec_id,
		    	   --		        ppr PprDebug (tys_left ++ drop toss dicts)],
		    	   --	         ppCat [ppStr "->", ppr PprDebug spec_spec_id,
		    	   --		        ppr PprDebug (tys_left_left ++ drop (toss + toss_toss) dicts)]])
			   (returnSM emptyUDs)

		else
		    -- pprTrace "CallInst:Exists\n"
		    -- (ppAboves [ppCat [ppr PprDebug id, ppCat (map (ppr PprDebug) args)],
		    --	          ppCat [ppStr "->", ppr PprDebug spec_id,
		    --		         ppr PprDebug (tys_left ++ drop toss dicts)]])
		    (returnSM emptyUDs)


take_type_args (_:tyvars) class_tyvar_pairs ((TyArg ty,_,_):args)
	= case (take_type_args tyvars class_tyvar_pairs args) of
	    Nothing 	          -> Nothing
	    Just (tys, dicts, others) -> Just (ty:tys, dicts, others)

take_type_args (_:tyvars) class_tyvar_pairs [] = Nothing

take_type_args [] class_tyvar_pairs args
	= case (take_dict_args class_tyvar_pairs args) of
	    Nothing              -> Nothing
	    Just (dicts, others) -> Just ([], dicts, others)

take_dict_args (_:class_tyvar_pairs) ((dict,_,_):args) | isValArg dict
	= case (take_dict_args class_tyvar_pairs args) of
	    Nothing              -> Nothing
	    Just (dicts, others) -> Just (dict:dicts, others)

take_dict_args (_:class_tyvar_pairs) [] = Nothing

take_dict_args [] args = Just ([], args)
\end{code}

\begin{code}
mkCall :: Id
       -> [(CoreArg, UsageDetails, CoreExpr -> CoreExpr)]
       -> SpecM CoreExpr

mkCall new_id arg_infos = returnSM (mkGenApp (Var new_id) [arg | (arg, _, _) <- arg_infos])

{- 
  | maybeToBool (isSuperDictSelId_maybe new_id)
    && any isUnboxedType ty_args
	-- No specialisations for super-dict selectors
	-- Specialise unboxed calls to SuperDictSelIds by extracting
	-- the super class dictionary directly form the super class
	-- NB: This should be dead code since all uses of this dictionary should
	--     have been specialised. We only do this to keep core-lint happy.
    = let
	 Just (_, super_class) = isSuperDictSelId_maybe new_id
	 super_dict_id = case lookupClassInstAtSimpleType super_class (head ty_args) of
			 Nothing -> panic "Specialise:mkCall:SuperDictId"
			 Just id -> id
      in
      returnSM (False, Var super_dict_id)

  | otherwise
    = case lookupSpecEnv (getIdSpecialisation new_id) ty_args of
	Nothing -> checkUnspecOK new_id ty_args (
		   returnSM (False, unspec_call)
		   )

	Just spec_1_details@(spec_id_1, tys_left_1, dicts_to_toss_1)
		-> let
			-- It may be necessary to specialsie a constant method spec_id again
		       (spec_id, tys_left, dicts_to_toss) =
			    case (maybeToBool (isConstMethodId_maybe spec_id_1),
				  lookupSpecEnv (getIdSpecialisation spec_id_1) tys_left_1) of
				 (False, _ )	 -> spec_1_details
				 (True, Nothing) -> spec_1_details
				 (True, Just (spec_id_2, tys_left_2, dicts_to_toss_2))
						 -> (spec_id_2, tys_left_2, dicts_to_toss_1 + dicts_to_toss_2)

		       args_left = toss_dicts dicts_to_toss val_args
		   in
		   checkSpecOK new_id ty_args spec_id tys_left (

			-- The resulting spec_id may be a top-level unboxed value
			-- This can arise for:
			-- 1) constant method values
			--    eq: class Num a where pi :: a
			--	  instance Num Double# where pi = 3.141#
			-- 2) specilised overloaded values
			--    eq: i1 :: Num a => a
			--	  i1 Int# d.Num.Int# ==> i1.Int#
			-- These top level defns should have been lifted.
			-- We must add code to unlift such a spec_id.

		   if isUnboxedType (idType spec_id) then
		       ASSERT (null tys_left && null args_left)
		       if toplevelishId spec_id then
		 	   liftId spec_id 	`thenSM` \ (lift_spec_id, unlift_spec_id) ->
			   returnSM (True, bindUnlift lift_spec_id unlift_spec_id
						      (Var unlift_spec_id))
		       else
			   pprPanic "Specialise:mkCall: unboxed spec_id not top-level ...\n"
				    (ppCat [ppr PprDebug new_id,
					    ppInterleave ppNil (map (pprParendGenType PprDebug) ty_args),
					    ppStr "==>",
					    ppr PprDebug spec_id])
		   else
		   let
		       (vals_left, _, unlifts_left) = unzip3 args_left
		       applied_tys  = mkTyApp (Var spec_id) tys_left
		       applied_vals = mkGenApp applied_tys vals_left
		   in
		   returnSM (True, applyBindUnlifts unlifts_left applied_vals)
		   )
  where
    (tys_and_vals, _, unlifts) = unzip3 args
    unspec_call = applyBindUnlifts unlifts (mkGenApp (Var new_id) tys_and_vals)


	-- ty_args is the types at the front of the arg list
	-- val_args is the rest of the arg-list

    (ty_args, val_args) = get args
      where
	get ((TyArg ty,_,_) : args) = (ty : tys, rest) where (tys,rest) = get args
	get args		    = ([],       args)


	-- toss_dicts chucks away dict args, checking that they ain't types!
    toss_dicts 0 args 		    = args
    toss_dicts n ((a,_,_) : args)
      | isValArg a		    = toss_dicts (n-1) args

\end{code}

\begin{code}
checkUnspecOK :: Id -> [Type] -> a -> a
checkUnspecOK check_id tys
  = if isLocallyDefined check_id && any isUnboxedType tys
    then pprPanic "Specialise:checkUnspecOK: unboxed instance for local id not found\n"
		  (ppCat [ppr PprDebug check_id,
			  ppInterleave ppNil (map (pprParendGenType PprDebug) tys)])
    else id

checkSpecOK :: Id -> [Type] -> Id -> [Type] -> a -> a
checkSpecOK check_id tys spec_id tys_left
  = if any isUnboxedType tys_left
    then pprPanic "Specialise:checkSpecOK: unboxed type args in specialised application\n"
		  (ppAboves [ppCat [ppr PprDebug check_id,
				    ppInterleave ppNil (map (pprParendGenType PprDebug) tys)],
			     ppCat [ppr PprDebug spec_id,
				    ppInterleave ppNil (map (pprParendGenType PprDebug) tys_left)]])
    else id
-}
\end{code}

\begin{code}
mkTyConInstance :: Id
		-> [Type]
   		-> SpecM UsageDetails
mkTyConInstance con tys
  = recordTyConInst con tys	`thenSM` \ record_inst ->
    case record_inst of
      Nothing				-- No TyCon instance
	-> -- pprTrace "NoTyConInst:"
	   -- (ppCat [ppr PprDebug tycon, ppStr "at",
	   --	      ppr PprDebug con, ppCat (map (ppr PprDebug) tys)])
	   (returnSM (singleConUDs con))

      Just spec_tys			-- Record TyCon instance
	-> -- pprTrace "TyConInst:"
	   -- (ppCat [ppr PprDebug tycon, ppStr "at",
	   --	      ppr PprDebug con, ppCat (map (ppr PprDebug) tys),
	   --	      ppBesides [ppStr "(",
	   --			 ppCat [pprMaybeTy PprDebug ty | ty <- spec_tys],
	   --			 ppStr ")"]])
	   (returnSM (singleTyConI tycon spec_tys `unionUDs` singleConUDs con))
  where
    tycon = dataConTyCon con
\end{code}

\begin{code}
recordTyConInst :: Id
		-> [Type]
		-> SpecM (Maybe [Maybe Type])

recordTyConInst con tys
  = let
	spec_tys = specialiseConstrTys tys

	do_tycon_spec = maybeToBool (firstJust spec_tys)

	spec_exists = maybeToBool (lookupSpecEnv
				      (getIdSpecialisation con)
				      tys)
    in
    -- pprTrace "ConSpecExists?: "
    -- (ppAboves [ppStr (if spec_exists then "True" else "False"),
    --		  ppr PprShowAll con, ppCat (map (ppr PprDebug) tys)])
    (if (not spec_exists && do_tycon_spec)
     then returnSM (Just spec_tys)
     else returnSM Nothing)
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
type TypeEnv = TyVarEnv Type

type SpecM result
  =  TypeEnv
  -> SpecIdEnv
  -> UniqSupply
  -> result

initSM m uniqs
  = m nullTyVarEnv nullIdEnv uniqs

returnSM :: a -> SpecM a
thenSM	 :: SpecM a -> (a -> SpecM b) -> SpecM b
fixSM    :: (a -> SpecM a) -> SpecM a

thenSM m k tvenv idenv us
  = case splitUniqSupply us	   of { (s1, s2) ->
    case (m tvenv idenv s1) of { r ->
    k r tvenv idenv s2 }}

returnSM r tvenv idenv us = r

fixSM k tvenv idenv us
 = r
 where
   r = k r tvenv idenv us	-- Recursive in r!
\end{code}

The only interesting bit is figuring out the type of the SpecId!

\begin{code}
newSpecIds :: [Id]		-- The id of which to make a specialised version
	   -> [Maybe Type]	-- Specialise to these types
	   -> Int		-- No of dicts to specialise
	   -> SpecM [Id]

newSpecIds new_ids maybe_tys dicts_to_ignore tvenv idenv us
  = [ mkSpecId uniq id maybe_tys (spec_id_ty id) (selectIdInfoForSpecId id)
      | (id,uniq) <- zipEqual "newSpecIds" new_ids uniqs ]
  where
    uniqs = getUniques (length new_ids) us
    spec_id_ty id = specialiseTy (idType id) maybe_tys dicts_to_ignore

newTyVars :: Int -> SpecM [TyVar]
newTyVars n tvenv idenv us 
  = [mkSysTyVar uniq mkBoxedTypeKind | uniq <- getUniques n us]
\end{code}

@cloneLambdaOrCaseBinders@ and @cloneLetBinders@ take a bunch of
binders, and build ``clones'' for them.  The clones differ from the
originals in three ways:

	(a) they have a fresh unique
	(b) they have the current type environment applied to their type
	(c) for Let binders which have been specialised to unboxed values
	    the clone will have a lifted type

As well as returning the list of cloned @Id@s they also return a list of
@CloneInfo@s which the original binders should be bound to.

\begin{code}
cloneLambdaOrCaseBinders :: [Id] 			-- Old binders
			 -> SpecM ([Id], [CloneInfo])	-- New ones

cloneLambdaOrCaseBinders old_ids tvenv idenv us
  = let
	uniqs = getUniques (length old_ids) us
    in
    unzip (zipWithEqual "cloneLambdaOrCaseBinders" clone_it old_ids uniqs)
  where
    clone_it old_id uniq
      = (new_id, NoLift (VarArg new_id))
      where
	new_id = applyTypeEnvToId tvenv (mkIdWithNewUniq old_id uniq)

cloneLetBinders :: Bool 			-- Top level ?
		-> Bool 			-- Recursice
		-> [Id] 			-- Old binders
		-> SpecM ([Id], [CloneInfo])	-- New ones

cloneLetBinders top_lev is_rec old_ids tvenv idenv us
  = let
	uniqs = getUniques (2 * length old_ids) us
    in
    unzip (clone_them old_ids uniqs)
  where
    clone_them [] [] = []

    clone_them (old_id:olds) (u1:u2:uniqs)
      | top_lev
	= (old_id,
	   NoLift (VarArg old_id)) : clone_rest

	 -- Don't clone if it is a top-level thing. Why not?
	 -- (a) we don't want to change the uniques
	 --     on such things (see TopLevId in Id.lhs)
	 -- (b) we don't have to be paranoid about name capture
	 -- (c) the thing is polymorphic so no need to subst

      | otherwise
	= if (is_rec && isUnboxedType new_ty && not (isUnboxedType old_ty))
	  then (lifted_id,
		Lifted lifted_id unlifted_id) : clone_rest
	  else (new_id,
		NoLift (VarArg new_id)) : clone_rest

      where
	clone_rest = clone_them olds uniqs

	new_id = applyTypeEnvToId tvenv (mkIdWithNewUniq old_id u1)
	new_ty = idType new_id
	old_ty = idType old_id

	(lifted_id, unlifted_id) = mkLiftedId new_id u2


cloneTyVarSM :: TyVar -> SpecM TyVar

cloneTyVarSM old_tyvar tvenv idenv us
  = let
	uniq = getUnique us
    in
    cloneTyVar old_tyvar uniq -- new_tyvar

bindId :: Id -> CloneInfo -> SpecM thing -> SpecM thing

bindId id val specm tvenv idenv us
 = specm tvenv (addOneToIdEnv idenv id val) us

bindIds :: [Id] -> [CloneInfo] -> SpecM thing -> SpecM thing

bindIds olds news specm tvenv idenv us
 = specm tvenv (growIdEnvList idenv (zip olds news)) us

bindSpecIds :: [Id]			-- Old
	    -> [(CloneInfo)]		-- New
	    -> [[Maybe SpecInfo]]	-- Corresponding specialisations
					-- Each sub-list corresponds to a different type,
					-- and contains one Maybe spec_info for each id
	    -> SpecM thing
	    -> SpecM thing

bindSpecIds olds clones spec_infos specm tvenv idenv us
 = specm tvenv (growIdEnvList idenv old_to_clone) us
 where
   old_to_clone = mk_old_to_clone olds clones spec_infos

   -- The important thing here is that we are *lazy* in spec_infos
   mk_old_to_clone [] [] _ = []
   mk_old_to_clone (old:rest_olds) (clone:rest_clones) spec_infos
     = (old, add_spec_info clone) :
       mk_old_to_clone rest_olds rest_clones spec_infos_rest
     where
       add_spec_info (NoLift (VarArg new))
	 = NoLift (VarArg (new `addIdSpecialisation`
				  (mkSpecEnv spec_infos_this_id)))
       add_spec_info lifted
	 = lifted		-- no specialised instances for unboxed lifted values

       spec_infos_this_id = catMaybes (map head spec_infos)
       spec_infos_rest    = map tail spec_infos


bindTyVar :: TyVar -> Type -> SpecM thing -> SpecM thing

bindTyVar tyvar ty specm tvenv idenv us
 = specm (growTyVarEnvList tvenv [(tyvar,ty)]) idenv us
\end{code}

\begin{code}
lookupId :: Id -> SpecM CloneInfo

lookupId id tvenv idenv us
  = case lookupIdEnv idenv id of
      Nothing   -> NoLift (VarArg id)
      Just info -> info
\end{code}

\begin{code}
specTy :: Type -> SpecM Type	-- Apply the current type envt to the type

specTy ty tvenv idenv us
  = applyTypeEnvToTy tvenv ty
\end{code}

\begin{code}
liftId :: Id -> SpecM (Id, Id)
liftId id tvenv idenv us
  = let
	uniq = getUnique us
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
