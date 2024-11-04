{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

{-
(c) The GRASP/AQUA Project, Glasgow University, 1993-1998

\section[Specialise]{Stamping out overloading, and (optionally) polymorphism}
-}

module GHC.Core.Opt.Specialise ( specProgram, specUnfolding ) where

import GHC.Prelude

import GHC.Driver.DynFlags
import GHC.Driver.Config
import GHC.Driver.Config.Diagnostic
import GHC.Driver.Config.Core.Rules ( initRuleOpts )

import GHC.Core.Type  hiding( substTy, substCo, extendTvSubst, zapSubst )
import GHC.Core.Multiplicity
import GHC.Core.SimpleOpt( defaultSimpleOpts, simpleOptExprWith )
import GHC.Core.Predicate
import GHC.Core.Coercion( Coercion )
import GHC.Core.Opt.Monad
import qualified GHC.Core.Subst as Core
import GHC.Core.Unfold.Make
import GHC.Core
import GHC.Core.Make      ( mkLitRubbish )
import GHC.Core.Unify     ( tcMatchTy )
import GHC.Core.Rules
import GHC.Core.Utils     ( exprIsTrivial, exprIsTopLevelBindable
                          , mkCast, exprType
                          , stripTicksTop, mkInScopeSetBndrs )
import GHC.Core.FVs
import GHC.Core.TyCo.FVs ( tyCoVarsOfTypeList )
import GHC.Core.Opt.Arity( collectBindersPushingCo )
-- import GHC.Core.Ppr( pprIds )

import GHC.Builtin.Types  ( unboxedUnitTy )

import GHC.Data.Maybe     ( maybeToList, isJust )
import GHC.Data.Bag
import GHC.Data.OrdList
import GHC.Data.List.SetOps

import GHC.Types.Basic
import GHC.Types.Unique.Supply
import GHC.Types.Unique.DFM
import GHC.Types.Name
import GHC.Types.Tickish
import GHC.Types.Id.Make  ( voidArgId, voidPrimId )
import GHC.Types.Var      ( PiTyBinder(..), isLocalVar, isInvisibleFunArg, mkLocalVar )
import GHC.Types.Var.Set
import GHC.Types.Var.Env
import GHC.Types.Id
import GHC.Types.Id.Info
import GHC.Types.Error

import GHC.Utils.Error ( mkMCDiagnostic )
import GHC.Utils.Monad    ( foldlM )
import GHC.Utils.Misc
import GHC.Utils.Outputable
import GHC.Utils.Panic

import GHC.Unit.Module( Module )
import GHC.Unit.Module.ModGuts
import GHC.Core.Unfold

import Data.List( partition )
import Data.List.NonEmpty ( NonEmpty (..) )
import GHC.Core.Subst (substTickish)

{-
************************************************************************
*                                                                      *
\subsection[notes-Specialise]{Implementation notes [SLPJ, Aug 18 1993]}
*                                                                      *
************************************************************************

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

        f x = f (x+x)           -- Yes I know its silly

becomes

        f a (d::Num a) = let p = +.sel a d
                         in
                         letrec fl (y::a) = fl (p y y)
                         in
                         fl

We still have recursion for non-overloaded functions which we
specialise, but the recursive call should get specialised to the
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
  +.sel could be floated outside the \y, to attempt to gain laziness.
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
at Int/Bool, because it's only instantiating the type variable "a" which
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
Ord!  (All the other specialisation has suitable dictionaries to hand
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


Requirements for the simplifier
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The simplifier has to be able to take advantage of the specialisation.

* When the simplifier finds an application of a polymorphic f, it looks in
f's IdInfo in case there is a suitable instance to call instead.  This converts

        f t1 t2 d1 d2   ===>   f_t1_t2

Note that the dictionaries get eaten up too!

* Dictionary selection operations on constant dictionaries must be
  short-circuited:

        +.sel Int d     ===>  +Int

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

        dfun.Eq.List Int d      ===> dEq.List_Int

Where does the Eq [Int] dictionary come from?  It is built in
response to a SPECIALIZE pragma on the Eq [a] instance decl.

In short, dfun Ids need IdInfo with a specialisation for each
constant instance of their instance declaration.

All this uses a single mechanism: the SpecEnv inside an Id


What does the specialisation IdInfo look like?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The SpecEnv of an Id maps a list of types (the template) to an expression

        [Type]  |->  Expr

For example, if f has this RuleInfo:

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
which sometimes get arguments with types of form (C (T a)) for some
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
        f :: Eq [(a,b)] => ...


Note [Specialisation and overlapping instances]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Here is at tricky case (see a comment in MR !8916):

    module A where
      class C a where
        meth :: a -> String
      instance {-# OVERLAPPABLE #-} C (Maybe a) where
        meth _ = "Maybe"

      {-# SPECIALISE f :: Maybe a -> Bool -> String #-}
      f :: C a => a -> Bool -> String
      f a True = f a False
      f a _    = meth a

    module B where
      import A

      instance C (Maybe Int) where
        meth _ = "Int"

      main = putStrLn $ f (Just 42 :: Maybe Int) True

Running main without optimisations yields "Int", the correct answer.
Activating optimisations yields "Maybe" due to a rewrite rule in module
A generated by the SPECIALISE pragma:

    RULE "USPEC f" forall a (d :: C a). f @a d = $sf

In B we get the call (f @(Maybe Int) (d :: C (Maybe Int))), and
that rewrites to $sf, but that isn't really right.

Overlapping instances mean that `C (Maybe Int)` is not a singleton
type: there two distinct dictionaries that have this type.  And that
spells trouble for specialistion, which really asssumes singleton
types.

For now, we just accept this problem, but it may bite us one day.
One solution would be to decline to expose any specialisation rules
to an importing module -- but that seems a bit drastic.


************************************************************************
*                                                                      *
\subsubsection{The new specialiser}
*                                                                      *
************************************************************************

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
  ... SPECIALISE f :: [Int] -> b -> b -> b ...

Hence, the invariant is this:

        *** no specialised version is overloaded ***


************************************************************************
*                                                                      *
\subsubsection{The exported function}
*                                                                      *
************************************************************************
-}

-- | Specialise calls to type-class overloaded functions occurring in a program.
specProgram :: ModGuts -> CoreM ModGuts
specProgram guts@(ModGuts { mg_module = this_mod
                          , mg_rules  = local_rules
                          , mg_binds  = binds })
  = do { dflags   <- getDynFlags
       ; rule_env <- initRuleEnv guts
                     -- See Note [Fire rules in the specialiser]

              -- We need to start with a Subst that knows all the things
              -- that are in scope, so that the substitution engine doesn't
              -- accidentally re-use a unique that's already in use
              -- Easiest thing is to do it all at once, as if all the top-level
              -- decls were mutually recursive
       ; let top_env = SE { se_subst = Core.mkEmptySubst $
                                        mkInScopeSetBndrs binds
                                      --    mkInScopeSetList $
                                      --  bindersOfBinds binds
                          , se_module = this_mod
                          , se_rules  = rule_env
                          , se_dflags = dflags }

             go []           = return ([], emptyUDs)
             go (bind:binds) = do (bind', binds', uds') <- specBind TopLevel top_env bind $ \_ ->
                                                           go binds
                                  return (bind' ++ binds', uds')

             -- Specialise the bindings of this module
       ; (binds', uds) <- runSpecM (go binds)

       ; (spec_rules, spec_binds) <- specImports top_env uds

       ; return (guts { mg_binds = spec_binds ++ binds'
                      , mg_rules = spec_rules ++ local_rules }) }

{-
Note [Wrap bindings returned by specImports]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
'specImports' returns a set of specialized bindings. However, these are lacking
necessary floated dictionary bindings, which are returned by
UsageDetails(ud_binds). These dictionaries need to be brought into scope with
'wrapDictBinds' before the bindings returned by 'specImports' can be used. See,
for instance, the 'specImports' call in 'specProgram'.


Note [Disabling cross-module specialisation]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Since GHC 7.10 we have performed specialisation of INLINABLE bindings living
in modules outside of the current module. This can sometimes uncover user code
which explodes in size when aggressively optimized. The
-fno-cross-module-specialise option was introduced to allow users to being
bitten by such instances to revert to the pre-7.10 behavior.

See #10491
-}


{- *********************************************************************
*                                                                      *
                   Specialising imported functions
*                                                                      *
********************************************************************* -}

{- Note [Specialising imported functions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
specImports specialises imported functions, based on calls in this module.

When -fspecialise-aggressively is on, we specialise any imported
function for which we have an unfolding.  The
-fspecialise-aggressively flag is usually off, because we risk lots of
orphan modules from over-vigorous specialisation.  (See Note [Orphans]
in GHC.Core.) However it's not a big deal: anything non-recursive with
an unfolding-template will probably have been inlined already.

When -fspecialise-aggressively is off, we are more selective about
specialisation (see canSpecImport):

(1) Without -fspecialise-aggressively, do not specialise
    DFunUnfoldings. Note [Do not specialise imported DFuns].

(2) Without -fspecialise-aggressively, specialise only imported things
    that have a /user-supplied/ INLINE or INLINABLE pragma (hence
    isAnyInlinePragma rather than isStableSource).

    In particular, we don't want to specialise workers created by
    worker/wrapper (for functions with no pragma) because they won't
    specialise usefully, and they generate quite a bit of useless code
    bloat.

    Specialise even INLINE things; it hasn't inlined yet, so perhaps
    it never will.  Moreover it may have calls inside it that we want
    to specialise

Wrinkle (W1): If we specialise an imported Id M.foo, we make a /local/
binding $sfoo.  But specImports may further specialise $sfoo. So we end up
with RULES for both M.foo (imported) and $sfoo (local).  Rules for local
Ids should be attached to the Ids themselves (see GHC.HsToCore
Note [Attach rules to local ids]); so we must partition the rules and
attach the local rules.  That is done in specImports, via addRulesToId.

Note [Glom the bindings if imported functions are specialised]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose we have an imported, *recursive*, INLINABLE function
   f :: Eq a => a -> a
   f = /\a \d x. ...(f a d)...
In the module being compiled we have
   g x = f (x::Int)
Now we'll make a specialised function
   f_spec :: Int -> Int
   f_spec = \x -> ...(f Int dInt)...
   {-# RULE  f Int _ = f_spec #-}
   g = \x. f Int dInt x
Note that f_spec doesn't look recursive
After rewriting with the RULE, we get
   f_spec = \x -> ...(f_spec)...
BUT since f_spec was non-recursive before it'll *stay* non-recursive.
The occurrence analyser never turns a NonRec into a Rec.  So we must
make sure that f_spec is recursive.  Easiest thing is to make all
the specialisations for imported bindings recursive.
-}

specImports :: SpecEnv
            -> UsageDetails
            -> CoreM ([CoreRule], [CoreBind])
specImports top_env (MkUD { ud_binds = dict_binds, ud_calls = calls })
  | not $ gopt Opt_CrossModuleSpecialise (se_dflags top_env)
    -- See Note [Disabling cross-module specialisation]
  = return ([], wrapDictBinds dict_binds [])

  | otherwise
  = do { let env_w_dict_bndrs = top_env `bringFloatedDictsIntoScope` dict_binds
       ; (_env, spec_rules, spec_binds) <- spec_imports env_w_dict_bndrs [] dict_binds calls

             -- Make a Rec: see Note [Glom the bindings if imported functions are specialised]
             --
             -- wrapDictBinds: don't forget to wrap the specialized bindings with
             --   bindings for the needed dictionaries.
             --   See Note [Wrap bindings returned by specImports]
             --
             -- addRulesToId: see Wrinkle (W1) in Note [Specialising imported functions]
             --               c.f. GHC.HsToCore.addExportFlagsAndRules
       ; let (rules_for_locals, rules_for_imps) = partition isLocalRule spec_rules
             local_rule_base = extendRuleBaseList emptyRuleBase rules_for_locals
             final_binds
               | null spec_binds = wrapDictBinds dict_binds []
               | otherwise       = [Rec $ mapFst (addRulesToId local_rule_base) $
                                          flattenBinds                          $
                                          wrapDictBinds dict_binds              $
                                          spec_binds]

       ; return (rules_for_imps, final_binds)
    }

-- | Specialise a set of calls to imported bindings
spec_imports :: SpecEnv          -- Passed in so that all top-level Ids are in scope
                                 ---In-scope set includes the FloatedDictBinds
             -> [Id]             -- Stack of imported functions being specialised
                                 -- See Note [specImport call stack]
             -> FloatedDictBinds -- Dict bindings, used /only/ for filterCalls
                                 -- See Note [Avoiding loops in specImports]
             -> CallDetails      -- Calls for imported things
             -> CoreM ( SpecEnv      -- Env contains the new rules
                      , [CoreRule]   -- New rules
                      , [CoreBind] ) -- Specialised bindings
spec_imports env callers dict_binds calls
  = do { let import_calls = dVarEnvElts calls
--       ; debugTraceMsg (text "specImports {" <+>
--                         vcat [ text "calls:" <+> ppr import_calls
--                              , text "dict_binds:" <+> ppr dict_binds ])
       ; (env, rules, spec_binds) <- go env import_calls
--       ; debugTraceMsg (text "End specImports }" <+> ppr import_calls)

       ; return (env, rules, spec_binds) }
  where
    go :: SpecEnv -> [CallInfoSet] -> CoreM (SpecEnv, [CoreRule], [CoreBind])
    go env [] = return (env, [], [])
    go env (cis : other_calls)
      = do { -- debugTraceMsg (text "specImport {" <+> ppr cis)
           ; (env, rules1, spec_binds1) <- spec_import env callers dict_binds cis
           ; -- debugTraceMsg (text "specImport }" <+> ppr cis)

           ; (env, rules2, spec_binds2) <- go env other_calls
           ; return (env, rules1 ++ rules2, spec_binds1 ++ spec_binds2) }

spec_import :: SpecEnv               -- Passed in so that all top-level Ids are in scope
                                     ---In-scope set includes the FloatedDictBinds
            -> [Id]                  -- Stack of imported functions being specialised
                                     -- See Note [specImport call stack]
            -> FloatedDictBinds      -- Dict bindings, used /only/ for filterCalls
                                     -- See Note [Avoiding loops in specImports]
            -> CallInfoSet           -- Imported function and calls for it
            -> CoreM ( SpecEnv
                     , [CoreRule]    -- New rules
                     , [CoreBind] )  -- Specialised bindings
spec_import env callers dict_binds cis@(CIS fn _)
  | isIn "specImport" fn callers
  = return (env, [], [])  -- No warning.  This actually happens all the time
                          -- when specialising a recursive function, because
                          -- the RHS of the specialised function contains a recursive
                          -- call to the original function

  | null good_calls
  = return (env, [], [])

  | Just rhs <- canSpecImport dflags fn
  = do {     -- Get rules from the external package state
             -- We keep doing this in case we "page-fault in"
             -- more rules as we go along
       ; eps_rules <- getExternalRuleBase
       ; let rule_env = se_rules env `updExternalPackageRules` eps_rules

--       ; debugTraceMsg (text "specImport1" <+> vcat
--           [ text "function:" <+> ppr fn
--           , text "good calls:" <+> ppr good_calls
--           , text "existing rules:" <+> ppr (getRules rule_env fn)
--           , text "rhs:" <+> ppr rhs
--           , text "dict_binds:" <+> ppr dict_binds ])

       ; (rules1, spec_pairs, MkUD { ud_binds = dict_binds1, ud_calls = new_calls })
            <- runSpecM $ specCalls True env (getRules rule_env fn) good_calls fn rhs

       ; let spec_binds1 = [NonRec b r | (b,r) <- spec_pairs]
             -- After the rules kick in, via fireRewriteRules, we may get recursion,
             -- but we rely on a global GlomBinds to sort that out later
             -- See Note [Glom the bindings if imported functions are specialised]
             -- Meanwhile, though, bring the binders into scope

             new_subst = se_subst env `Core.extendSubstInScopeList` map fst spec_pairs
             new_env   = env { se_rules = rule_env `addLocalRules` rules1
                             , se_subst = new_subst }
                         `bringFloatedDictsIntoScope` dict_binds1

       -- Now specialise any cascaded calls
--       ; debugTraceMsg (text "specImport 2" <+> vcat
--           [ text "function:" <+> ppr fn
--           , text "rules1:" <+> ppr rules1
--           , text "spec_binds1" <+> ppr spec_binds1
--           , text "dict_binds1" <+> ppr dict_binds1
--           , text "new_calls" <+> ppr new_calls ])

       ; (env, rules2, spec_binds2)
            <- spec_imports new_env (fn:callers)
                                    (dict_binds `thenFDBs` dict_binds1)
                                    new_calls

       ; let final_binds = wrapDictBinds dict_binds1 $
                           spec_binds2 ++ spec_binds1

       ; return (env, rules2 ++ rules1, final_binds) }

  | otherwise
  = do { tryWarnMissingSpecs dflags callers fn good_calls
       ; return (env, [], [])}

  where
    dflags = se_dflags env
    good_calls = filterCalls cis dict_binds
       -- SUPER IMPORTANT!  Drop calls that (directly or indirectly) refer to fn
       -- See Note [Avoiding loops in specImports]

canSpecImport :: DynFlags -> Id -> Maybe CoreExpr
canSpecImport dflags fn
  | isDataConWrapId fn
  = Nothing   -- Don't specialise data-con wrappers, even if they
              -- have dict args; there is no benefit.

  | CoreUnfolding { uf_tmpl = rhs } <- unf
    -- CoreUnfolding: see Note [Specialising imported functions] point (1).
  , isAnyInlinePragma (idInlinePragma fn)
    -- See Note [Specialising imported functions] point (2).
  = Just rhs

  | gopt Opt_SpecialiseAggressively dflags
  = maybeUnfoldingTemplate unf
    -- With -fspecialise-aggressively, specialise anything
    -- with an unfolding, stable or not, DFun or not

  | otherwise = Nothing
  where
    unf = realIdUnfolding fn   -- We want to see the unfolding even for loop breakers

-- | Returns whether or not to show a missed-spec warning.
-- If -Wall-missed-specializations is on, show the warning.
-- Otherwise, if -Wmissed-specializations is on, only show a warning
-- if there is at least one imported function being specialized,
-- and if all imported functions are marked with an inline pragma
-- Use the most specific warning as the reason.
tryWarnMissingSpecs :: DynFlags -> [Id] -> Id -> [CallInfo] -> CoreM ()
-- See Note [Warning about missed specialisations]
tryWarnMissingSpecs dflags callers fn calls_for_fn
  | isClassOpId fn = return () -- See Note [Missed specialisation for ClassOps]
  | wopt Opt_WarnMissedSpecs dflags
    && not (null callers)
    && allCallersInlined                  = doWarn $ WarningWithFlag Opt_WarnMissedSpecs
  | wopt Opt_WarnAllMissedSpecs dflags    = doWarn $ WarningWithFlag Opt_WarnAllMissedSpecs
  | otherwise                             = return ()
  where
    allCallersInlined = all (isAnyInlinePragma . idInlinePragma) callers
    diag_opts = initDiagOpts dflags
    doWarn reason =
      msg (mkMCDiagnostic diag_opts reason Nothing)
        (vcat [ hang (text ("Could not specialise imported function") <+> quotes (ppr fn))
                2 (vcat [ text "when specialising" <+> quotes (ppr caller)
                        | caller <- callers])
          , whenPprDebug (text "calls:" <+> vcat (map (pprCallInfo fn) calls_for_fn))
          , text "Probable fix: add INLINABLE pragma on" <+> quotes (ppr fn) ])

{- Note [Missed specialisation for ClassOps]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In #19592 I saw a number of missed specialisation warnings
which were the result of things like:

    case isJumpishInstr @X86.Instr $dInstruction_s7f8 eta3_a78C of { ...

where isJumpishInstr is part of the Instruction class and defined like
this:

    class Instruction instr where
        ...
        isJumpishInstr :: instr -> Bool
        ...

isJumpishInstr is a ClassOp which will select the right method
from within the dictionary via our built in rules. See also
Note [ClassOp/DFun selection] in GHC.Tc.TyCl.Instance.

We don't give these unfoldings, and as a result the specialiser
complains. But usually this doesn't matter. The simplifier will
apply the rule and we end up with

    case isJumpishInstrImplX86 eta3_a78C of { ...

Since isJumpishInstrImplX86 is defined for a concrete instance (given
by the dictionary) it is usually already well specialised!
Theoretically the implementation of a method could still be overloaded
over a different type class than what it's a method of. But I wasn't able
to make this go wrong, and SPJ thinks this should be fine as well.

So I decided to remove the warnings for failed specialisations on ClassOps
alltogether as they do more harm than good.
-}

{- Note [Do not specialise imported DFuns]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Ticket #18223 shows that specialising calls of DFuns is can cause a huge
and entirely unnecessary blowup in program size.  Consider a call to
    f @[[[[[[[[T]]]]]]]] d1 x
where df :: C a => C [a]
      d1 :: C [[[[[[[[T]]]]]]]] = dfC[] @[[[[[[[T]]]]]]] d1
      d2 :: C [[[[[[[T]]]]]]]   = dfC[] @[[[[[[T]]]]]] d3
      ...
Now we'll specialise f's RHS, which may give rise to calls to 'g',
also overloaded, which we will specialise, and so on.  However, if
we specialise the calls to dfC[], we'll generate specialised copies of
all methods of C, at all types; and the same for C's superclasses.

And many of these specialised functions will never be called.  We are
going to call the specialised 'f', and the specialised 'g', but DFuns
group functions into a tuple, many of whose elements may never be used.

With deeply-nested types this can lead to a simply overwhelming number
of specialisations: see #18223 for a simple example (from the wild).
I measured the number of specialisations for various numbers of calls
of `flip evalStateT ()`, and got this

                       Size after one simplification
  #calls    #SPEC rules    Terms     Types
      5         56          3100     10600
      9        108         13660     77206

The real tests case has 60+ calls, which blew GHC out of the water.

Solution: don't specialise DFuns.  The downside is that if we end
up with (h (dfun d)), /and/ we don't specialise 'h', then we won't
pass to 'h' a tuple of specialised functions.

However, the flag -fspecialise-aggressively (experimental, off by default)
allows DFuns to specialise as well.

Note [Avoiding loops in specImports]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We must take great care when specialising instance declarations
(DFuns like $fOrdList) lest we accidentally build a recursive
dictionary. See Note [Avoiding loops (DFuns)].

The basic strategy of Note [Avoiding loops (DFuns)] is to use filterCalls
to discard loopy specialisations.  But to do that we must ensure
that the in-scope dict-binds (passed to filterCalls) contains
all the needed dictionary bindings.  In particular, in the recursive
call to spec_imports in spec_import, we must include the dict-binds
from the parent.  Lacking this caused #17151, a really nasty bug.

Here is what happened.
* Class structure:
    Source is a superclass of Mut
    Index is a superclass of Source

* We started with these dict binds
    dSource = $fSourcePix @Int $fIndexInt
    dIndex  = sc_sel dSource
    dMut    = $fMutPix @Int dIndex
  and these calls to specialise
    $fMutPix @Int dIndex
    $fSourcePix @Int $fIndexInt

* We specialised the call ($fMutPix @Int dIndex)
  ==> new call ($fSourcePix @Int dIndex)
      (because Source is a superclass of Mut)

* We specialised ($fSourcePix @Int dIndex)
  ==> produces specialised dict $s$fSourcePix,
      a record with dIndex as a field
      plus RULE forall d. ($fSourcePix @Int d) = $s$fSourcePix
  *** This is the bogus step ***

* Now we decide not to specialise the call
    $fSourcePix @Int $fIndexInt
  because we alredy have a RULE that matches it

* Finally the simplifer rewrites
    dSource = $fSourcePix @Int $fIndexInt
    ==>  dSource = $s$fSourcePix

Disaster. Now we have

Rewrite dSource's RHS to $s$fSourcePix   Disaster
    dSource = $s$fSourcePix
    dIndex  = sc_sel dSource
    $s$fSourcePix = MkSource dIndex ...

Solution: filterCalls should have stopped the bogus step,
by seeing that dIndex transitively uses $fSourcePix. But
it can only do that if it sees all the dict_binds.  Wow.

--------------
Here's another example (#13429).  Suppose we have
  class Monoid v => C v a where ...

We start with a call
   f @ [Integer] @ Integer $fC[]Integer

Specialising call to 'f' gives dict bindings
   $dMonoid_1 :: Monoid [Integer]
   $dMonoid_1 = M.$p1C @ [Integer] $fC[]Integer

   $dC_1 :: C [Integer] (Node [Integer] Integer)
   $dC_1 = M.$fCvNode @ [Integer] $dMonoid_1

...plus a recursive call to
   f @ [Integer] @ (Node [Integer] Integer) $dC_1

Specialising that call gives
   $dMonoid_2  :: Monoid [Integer]
   $dMonoid_2  = M.$p1C @ [Integer] $dC_1

   $dC_2 :: C [Integer] (Node [Integer] Integer)
   $dC_2 = M.$fCvNode @ [Integer] $dMonoid_2

Now we have two calls to the imported function
  M.$fCvNode :: Monoid v => C v a
  M.$fCvNode @v @a m = C m some_fun

But we must /not/ use the call (M.$fCvNode @ [Integer] $dMonoid_2)
for specialisation, else we get:

  $dC_1 = M.$fCvNode @ [Integer] $dMonoid_1
  $dMonoid_2 = M.$p1C @ [Integer] $dC_1
  $s$fCvNode = C $dMonoid_2 ...
    RULE M.$fCvNode [Integer] _ _ = $s$fCvNode

Now use the rule to rewrite the call in the RHS of $dC_1
and we get a loop!


Note [specImport call stack]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When specialising an imports function 'f', we may get new calls
of an imported function 'g', which we want to specialise in turn,
and similarly specialising 'g' might expose a new call to 'h'.

We track the stack of enclosing functions. So when specialising 'h' we
have a specImport call stack of [g,f]. We do this for two reasons:
* Note [Warning about missed specialisations]
* Note [Avoiding recursive specialisation]

Note [Warning about missed specialisations]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose
 * In module Lib, you carefully mark a function 'foo' INLINABLE
 * Import Lib(foo) into another module M
 * Call 'foo' at some specialised type in M
Then you jolly well expect it to be specialised in M.  But what if
'foo' calls another function 'Lib.bar'.  Then you'd like 'bar' to be
specialised too.  But if 'bar' is not marked INLINABLE it may well
not be specialised.  The warning Opt_WarnMissedSpecs warns about this.

It's more noisy to warning about a missed specialisation opportunity
for /every/ overloaded imported function, but sometimes useful. That
is what Opt_WarnAllMissedSpecs does.

ToDo: warn about missed opportunities for local functions.

Note [Avoiding recursive specialisation]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When we specialise 'f' we may find new overloaded calls to 'g', 'h' in
'f's RHS.  So we want to specialise g,h.  But we don't want to
specialise f any more!  It's possible that f's RHS might have a
recursive yet-more-specialised call, so we'd diverge in that case.
And if the call is to the same type, one specialisation is enough.
Avoiding this recursive specialisation loop is one reason for the
'callers' stack passed to specImports and specImport.


************************************************************************
*                                                                      *
\subsubsection{@specExpr@: the main function}
*                                                                      *
************************************************************************
-}

data SpecEnv
  = SE { se_subst :: Core.Subst
             -- We carry a substitution down:
             -- a) we must clone any binding that might float outwards,
             --    to avoid name clashes
             -- b) we carry a type substitution to use when analysing
             --    the RHS of specialised bindings (no type-let!)

       , se_module :: Module
       , se_rules  :: RuleEnv  -- From the home package and this module
       , se_dflags :: DynFlags
     }

instance Outputable SpecEnv where
  ppr (SE { se_subst = subst })
    = text "SE" <+> braces (text "subst =" <+> ppr subst)

specVar :: SpecEnv -> InId -> SpecM (OutExpr, UsageDetails)
specVar env@(SE { se_subst = Core.Subst in_scope ids _ _ }) v
  | not (isLocalId v)                   = return (Var v, emptyUDs)
  | Just e  <- lookupVarEnv ids       v = specExpr (zapSubst env) e  -- Note (1)
  | Just v' <- lookupInScope in_scope v = return (Var v', emptyUDs)
  | otherwise = pprPanic "specVar" (ppr v $$ ppr in_scope)
  -- c.f. GHC.Core.Subst.lookupIdSubst
  -- Note (1): we recurse so we do the lookupInScope thing on any Vars in e
  --           probably has little effect, but it's the right thing.
  --           We need zapSubst because `e` is an OutExpr

specExpr :: SpecEnv -> CoreExpr -> SpecM (CoreExpr, UsageDetails)

---------------- First the easy cases --------------------
specExpr env (Var v)       = specVar env v
specExpr env (Type ty)     = return (Type     (substTy env ty), emptyUDs)
specExpr env (Coercion co) = return (Coercion (substCo env co), emptyUDs)
specExpr _   (Lit lit)     = return (Lit lit,                   emptyUDs)
specExpr env (Cast e co)
  = do { (e', uds) <- specExpr env e
       ; return ((mkCast e' (substCo env co)), uds) }
specExpr env (Tick tickish body)
  = do { (body', uds) <- specExpr env body
       ; return (Tick (specTickish env tickish) body', uds) }

---------------- Applications might generate a call instance --------------------
specExpr env expr@(App {})
  = do { let (fun_in, args_in) = collectArgs expr
       ; (args_out, uds_args) <- mapAndCombineSM (specExpr env) args_in
       ; let env_args = env `bringFloatedDictsIntoScope` ud_binds uds_args
                -- Some dicts may have floated out of args_in;
                -- they should be in scope for fireRewriteRules (#21689)
             (fun_in', args_out') = fireRewriteRules env_args fun_in args_out
       ; (fun_out', uds_fun) <- specExpr env fun_in'
       ; let uds_call = mkCallUDs env fun_out' args_out'
       ; return (fun_out' `mkApps` args_out', uds_fun `thenUDs` uds_call `thenUDs` uds_args) }

---------------- Lambda/case require dumping of usage details --------------------
specExpr env e@(Lam {})
  = specLam env' bndrs' body
  where
    (bndrs, body)  = collectBinders e
    (env', bndrs') = substBndrs env bndrs
        -- More efficient to collect a group of binders together all at once
        -- and we don't want to split a lambda group with dumped bindings

specExpr env (Case scrut case_bndr ty alts)
  = do { (scrut', scrut_uds) <- specExpr env scrut
       ; (scrut'', case_bndr', alts', alts_uds)
             <- specCase env scrut' case_bndr alts
--       ; pprTrace "specExpr:case" (vcat
--            [ text "scrut" <+> ppr scrut, text "scrut'" <+> ppr scrut'
--            , text "case_bndr'" <+> ppr case_bndr'
--            , text "alts_uds" <+> ppr alts_uds
--            ])
       ; return (Case scrut'' case_bndr' (substTy env ty) alts'
                , scrut_uds `thenUDs` alts_uds) }

---------------- Finally, let is the interesting case --------------------
specExpr env (Let bind body)
  = do { (binds', body', uds) <- specBind NotTopLevel env bind $ \body_env ->
                                 -- pprTrace "specExpr:let" (ppr (se_subst body_env) $$ ppr body) $
                                 specExpr body_env body
         -- All done
       ; return (foldr Let body' binds', uds) }

-- See Note [Specialisation modulo dictionary selectors]
--     Note [ClassOp/DFun selection]
--     Note [Fire rules in the specialiser]
fireRewriteRules :: SpecEnv -> InExpr -> [OutExpr] -> (InExpr, [OutExpr])
fireRewriteRules env (Var f) args
  | Just (rule, expr) <- specLookupRule env f args InitialPhase (getRules (se_rules env) f)
  , let rest_args    = drop (ruleArity rule) args -- See Note [Extra args in the target]
        zapped_subst = Core.zapSubst (se_subst env)
        expr'        = simpleOptExprWith defaultSimpleOpts zapped_subst expr
                       -- simplOptExpr needed because lookupRule returns
                       --   (\x y. rhs) arg1 arg2
  , (fun, args) <- collectArgs expr'
  = fireRewriteRules env fun (args++rest_args)
fireRewriteRules _ fun args = (fun, args)

--------------
specLam :: SpecEnv -> [OutBndr] -> InExpr -> SpecM (OutExpr, UsageDetails)
-- The binders have been substituted, but the body has not
specLam env bndrs body
  | null bndrs
  = specExpr env body
  | otherwise
  = do { (body', uds) <- specExpr env body
       ; let (free_uds, dumped_dbs) = dumpUDs bndrs uds
       ; return (mkLams bndrs (wrapDictBindsE dumped_dbs body'), free_uds) }

--------------
specTickish :: SpecEnv -> CoreTickish -> CoreTickish
specTickish (SE { se_subst = subst }) bp = substTickish subst bp

--------------
specCase :: SpecEnv
         -> OutExpr             -- Scrutinee, already done
         -> InId -> [InAlt]
         -> SpecM ( OutExpr     -- New scrutinee
                  , OutId
                  , [OutAlt]
                  , UsageDetails)
specCase env scrut' case_bndr [Alt con args rhs]
  | -- See Note [Floating dictionaries out of cases]
    interestingDict scrut' (idType case_bndr)
  , not (isDeadBinder case_bndr && null sc_args')
  = do { case_bndr_flt :| sc_args_flt <- mapM clone_me (case_bndr' :| sc_args')

       ; let case_bndr_flt' = case_bndr_flt `addDictUnfolding` scrut'
             scrut_bind     = mkDB (NonRec case_bndr_flt scrut')

             sc_args_flt' = zipWith addDictUnfolding sc_args_flt sc_rhss
             sc_rhss      = [ Case (Var case_bndr_flt') case_bndr' (idType sc_arg')
                                   [Alt con args' (Var sc_arg')]
                            | sc_arg' <- sc_args' ]
             cb_set       = unitVarSet case_bndr_flt'
             sc_binds     = [ DB { db_bind = NonRec sc_arg_flt sc_rhs, db_fvs  = cb_set }
                            | (sc_arg_flt, sc_rhs) <- sc_args_flt' `zip` sc_rhss ]

             flt_binds    = scrut_bind : sc_binds

             -- Extend the substitution for RHS to map the *original* binders
             -- to their floated versions.
             mb_sc_flts :: [Maybe DictId]
             mb_sc_flts = map (lookupVarEnv clone_env) args'
             clone_env  = zipVarEnv sc_args' sc_args_flt'

             subst_prs  = (case_bndr, Var case_bndr_flt)
                        : [ (arg, Var sc_flt)
                          | (arg, Just sc_flt) <- args `zip` mb_sc_flts ]
             subst'   = se_subst env_rhs
                        `Core.extendSubstInScopeList` (case_bndr_flt' : sc_args_flt')
                        `Core.extendIdSubstList`      subst_prs
             env_rhs' = env_rhs { se_subst = subst' }

       ; (rhs', rhs_uds)   <- specExpr env_rhs' rhs
       ; let (free_uds, dumped_dbs) = dumpUDs (case_bndr':args') rhs_uds
             all_uds = flt_binds `consDictBinds` free_uds
             alt'    = Alt con args' (wrapDictBindsE dumped_dbs rhs')
--       ; pprTrace "specCase" (ppr case_bndr $$ ppr scrut_bind) $
       ; return (Var case_bndr_flt, case_bndr', [alt'], all_uds) }
  where
    (env_rhs, (case_bndr':args')) = substBndrs env (case_bndr:args)
    sc_args' = filter is_flt_sc_arg args'

    clone_me bndr = do { uniq <- getUniqueM
                       ; return (mkUserLocalOrCoVar occ uniq wght ty loc) }
       where
         name = idName bndr
         wght = idMult bndr
         ty   = idType bndr
         occ  = nameOccName name
         loc  = getSrcSpan name

    arg_set = mkVarSet args'
    is_flt_sc_arg var =  isId var
                      && not (isDeadBinder var)
                      && isDictTy var_ty
                      && tyCoVarsOfType var_ty `disjointVarSet` arg_set
       where
         var_ty = idType var


specCase env scrut case_bndr alts
  = do { (alts', uds_alts) <- mapAndCombineSM spec_alt alts
       ; return (scrut, case_bndr', alts', uds_alts) }
  where
    (env_alt, case_bndr') = substBndr env case_bndr
    spec_alt (Alt con args rhs)
      = do { (rhs', uds) <- specExpr env_rhs rhs
           ; let (free_uds, dumped_dbs) = dumpUDs (case_bndr' : args') uds
--           ; unless (isNilOL dumped_dbs) $
--             pprTrace "specAlt" (vcat
--                 [text "case_bndr', args" <+> (ppr case_bndr' $$ ppr args)
--                 ,text "dumped" <+> ppr dumped_dbs ]) return ()
           ; return (Alt con args' (wrapDictBindsE dumped_dbs rhs'), free_uds) }
        where
          (env_rhs, args') = substBndrs env_alt args

{- Note [Fire rules in the specialiser]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider this (#21851)

    module A where
      f :: Num b => b -> (b, b)
      f x = (x + 1, snd (f x))
      {-# SPECIALIZE f :: Int -> (Int, Int) #-}

    module B (g') where
      import A

      g :: Num a => a -> a
      g x = fst (f x)
      {-# NOINLINE[99] g #-}

      h :: Int -> Int
      h = g

Note that `f` has the CPR property, and so will worker/wrapper.

The call to `g` in `h` will make us specialise `g @Int`. And the specialised
version of `g` will contain the call `f @Int`; but in the subsequent run of
the Simplifier, there will be a competition between:
* The user-supplied SPECIALISE rule for `f`
* The inlining of the wrapper for `f`
In fact, the latter wins -- see Note [Rewrite rules and inlining] in
GHC.Core.Opt.Simplify.Iteration.  However, it a bit fragile.

Moreover consider (test T21851_2):

    module A
      f :: (Ord a, Show b) => a -> b -> blah
      {-# RULE forall b. f @Int @b = wombat #-}

      wombat :: Show b => Int -> b -> blah
      wombat = blah

    module B
      import A
      g :: forall a. Ord a => blah
      g @a = ...g...f @a @Char....

      h = ....g @Int....

Now, in module B, GHC will specialise `g @Int`, which will lead to a
call `f @Int @Char`.  If we immediately (in the specialiser) rewrite
that to `womabat @Char`, we have a chance to specialise `wombat`.

Conclusion: it's treat if the Specialiser fires RULEs itself.
It's not hard to achieve: see `fireRewriteRules`. The only tricky bit is
making sure that we have a reasonably up to date EPS rule base. Currently
we load it up just once, in `initRuleEnv`, called at the beginning of
`specProgram`.

NB: you might wonder if running rules in the specialiser (this Note)
renders Note [Rewrite rules and inlining] in the Simplifier redundant.
That is, if we run rules in the specialiser, does it matter if we make
rules "win" over inlining in the Simplifier?  Yes, it does!  See the
discussion in #21851.

Note [Floating dictionaries out of cases]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
   g = \d. case d of { MkD sc ... -> ...(f sc)... }
Naively we can't float d2's binding out of the case expression,
because 'sc' is bound by the case, and that in turn means we can't
specialise f, which seems a pity.

So we invert the case, by floating out a binding
for 'sc_flt' thus:
    sc_flt = case d of { MkD sc ... -> sc }
Now we can float the call instance for 'f'.  Indeed this is just
what'll happen if 'sc' was originally bound with a let binding,
but case is more efficient, and necessary with equalities. So it's
good to work with both.

You might think that this won't make any difference, because the
call instance will only get nuked by the \d.  BUT if 'g' itself is
specialised, then transitively we should be able to specialise f.

In general, given
   case e of cb { MkD sc ... -> ...(f sc)... }
we transform to
   let cb_flt = e
       sc_flt = case cb_flt of { MkD sc ... -> sc }
   in
   case cb_flt of bg { MkD sc ... -> ....(f sc_flt)... }

The "_flt" things are the floated binds; we use the current substitution
to substitute sc -> sc_flt in the RHS

************************************************************************
*                                                                      *
                     Dealing with a binding
*                                                                      *
************************************************************************
-}

bringFloatedDictsIntoScope :: SpecEnv -> FloatedDictBinds -> SpecEnv
bringFloatedDictsIntoScope env (FDB { fdb_bndrs = dx_bndrs })
  = -- pprTrace "brought into scope" (ppr dx_bndrs) $
    env {se_subst=subst'}
  where
   subst' = se_subst env `Core.extendSubstInScopeSet` dx_bndrs

specBind :: TopLevelFlag
         -> SpecEnv    -- At top-level only, this env already has the
                       -- top level binders in scope
         -> InBind
         -> (SpecEnv -> SpecM (body, UsageDetails))    -- Process the body
         -> SpecM ( [OutBind]           -- New bindings
                  , body                -- Body
                  , UsageDetails)       -- And info to pass upstream

-- Returned UsageDetails:
--    No calls for binders of this bind
specBind top_lvl env (NonRec fn rhs) do_body
  = do { (rhs', rhs_uds) <- specExpr env rhs

       ; (body_env1, fn1) <- case top_lvl of
                               TopLevel    -> return (env, fn)
                               NotTopLevel -> cloneBndrSM env fn

       ; let fn2 | isStableUnfolding (idUnfolding fn1) = fn1
                 | otherwise = fn1 `setIdUnfolding` mkSimpleUnfolding defaultUnfoldingOpts rhs'
             -- Update the unfolding with the perhaps-simpler or more specialised rhs'
             -- This is important: see Note [Update unfolding after specialisation]
             -- And in any case cloneBndrSM discards non-Stable unfoldings

             fn3 = floatifyIdDemandInfo fn2
             -- We zap the demand info because the binding may float,
             -- which would invalidate the demand info (see #17810 for example).
             -- Destroying demand info is not terrible; specialisation is
             -- always followed soon by demand analysis.
             -- See Note [Floatifying demand info when floating] in GHC.Core.Opt.SetLevels

             body_env2 = body_env1 `bringFloatedDictsIntoScope` ud_binds rhs_uds
                                   `extendInScope` fn3
                                   -- bringFloatedDictsIntoScope: see #23567

       ; (body', body_uds) <- do_body body_env2

       ; (fn4, spec_defns, body_uds1) <- specDefn env body_uds fn3 rhs

       ; let (free_uds, dump_dbs, float_all) = dumpBindUDs [fn4] body_uds1
             all_free_uds                    = free_uds `thenUDs` rhs_uds

             pairs = spec_defns ++ [(fn4, rhs')]
                        -- fn4 mentions the spec_defns in its rules,
                        -- so put the latter first

             final_binds :: [DictBind]
             -- See Note [From non-recursive to recursive]
             final_binds | not (isNilOL dump_dbs)
                         , not (null spec_defns)
                         = [recWithDumpedDicts pairs dump_dbs]
                         | otherwise
                         = [mkDB $ NonRec b r | (b,r) <- pairs]
                           ++ fromOL dump_dbs

             can_float_this_one = exprIsTopLevelBindable rhs (idType fn)
             -- exprIsTopLevelBindable: see Note [Care with unlifted bindings]

       ; if float_all && can_float_this_one then
             -- Rather than discard the calls mentioning the bound variables
             -- we float this (dictionary) binding along with the others
              return ([], body', all_free_uds `snocDictBinds` final_binds)
         else
             -- No call in final_uds mentions bound variables,
             -- so we can just leave the binding here
              return (map db_bind final_binds, body', all_free_uds) }


specBind top_lvl env (Rec pairs) do_body
       -- Note [Specialising a recursive group]
  = do { let (bndrs,rhss) = unzip pairs

       ; (rec_env, bndrs1) <- case top_lvl of
                                 TopLevel    -> return (env, bndrs)
                                 NotTopLevel -> cloneRecBndrsSM env bndrs

       ; (rhss', rhs_uds)  <- mapAndCombineSM (specExpr rec_env) rhss
       ; (body', body_uds) <- do_body rec_env

       ; let scope_uds = body_uds `thenUDs` rhs_uds
                       -- Includes binds and calls arising from rhss

       ; (bndrs2, spec_defns2, uds2) <- specDefns rec_env scope_uds (bndrs1 `zip` rhss)
         -- bndrs2 is like bndrs1, but with RULES added

       ; (bndrs3, spec_defns3, uds3)
             <- if null spec_defns2  -- Common case: no specialisation
                then return (bndrs2, [], uds2)
                else do {            -- Specialisation occurred; do it again
                          (bndrs3, spec_defns3, uds3)
                              <- specDefns rec_env uds2 (bndrs2 `zip` rhss)
                        ; return (bndrs3, spec_defns3 ++ spec_defns2, uds3) }

       ; let (final_uds, dumped_dbs, float_all) = dumpBindUDs bndrs1 uds3
             final_bind = recWithDumpedDicts (spec_defns3 ++ zip bndrs3 rhss')
                                             dumped_dbs

       ; if float_all then
              return ([], body', final_uds `snocDictBind` final_bind)
         else
              return ([db_bind final_bind], body', final_uds) }


---------------------------
specDefns :: SpecEnv
          -> UsageDetails               -- Info on how it is used in its scope
          -> [(OutId,InExpr)]           -- The things being bound and their un-processed RHS
          -> SpecM ([OutId],            -- Original Ids with RULES added
                    [(OutId,OutExpr)],  -- Extra, specialised bindings
                    UsageDetails)       -- Stuff to fling upwards from the specialised versions

-- Specialise a list of bindings (the contents of a Rec), but flowing usages
-- upwards binding by binding.  Example: { f = ...g ...; g = ...f .... }
-- Then if the input CallDetails has a specialised call for 'g', whose specialisation
-- in turn generates a specialised call for 'f', we catch that in this one sweep.
-- But not vice versa (it's a fixpoint problem).

specDefns _env uds []
  = return ([], [], uds)
specDefns env uds ((bndr,rhs):pairs)
  = do { (bndrs1, spec_defns1, uds1) <- specDefns env uds  pairs
       ; (bndr1, spec_defns2, uds2)  <- specDefn  env uds1 bndr rhs
       ; return (bndr1 : bndrs1, spec_defns1 ++ spec_defns2, uds2) }

---------------------------
specDefn :: SpecEnv
         -> UsageDetails                -- Info on how it is used in its scope
         -> OutId -> InExpr             -- The thing being bound and its un-processed RHS
         -> SpecM (Id,                  -- Original Id with added RULES
                   [(Id,CoreExpr)],     -- Extra, specialised bindings
                   UsageDetails)        -- Stuff to fling upwards from the specialised versions

specDefn env body_uds fn rhs
  = do { let (body_uds_without_me, calls_for_me) = callsForMe fn body_uds
             rules_for_me = idCoreRules fn
             -- Bring into scope the binders from the floated dicts
             env_w_dict_bndrs = bringFloatedDictsIntoScope env (ud_binds body_uds)

       ; (rules, spec_defns, spec_uds) <- specCalls False env_w_dict_bndrs
                                                    rules_for_me calls_for_me fn rhs

       ; return ( fn `addIdSpecialisations` rules
                , spec_defns
                , body_uds_without_me `thenUDs` spec_uds) }
                -- It's important that the `thenUDs` is this way
                -- round, because body_uds_without_me may bind
                -- dictionaries that are used in calls_for_me passed
                -- to specDefn.  So the dictionary bindings in
                -- spec_uds may mention dictionaries bound in
                -- body_uds_without_me

---------------------------
specCalls :: Bool              -- True  =>  specialising imported fn
                               -- False =>  specialising local fn
          -> SpecEnv
          -> [CoreRule]        -- Existing RULES for the fn
          -> [CallInfo]
          -> OutId -> InExpr
          -> SpecM SpecInfo    -- New rules, specialised bindings, and usage details

-- This function checks existing rules, and does not create
-- duplicate ones. So the caller does not need to do this filtering.
-- See 'already_covered'

type SpecInfo = ( [CoreRule]       -- Specialisation rules
                , [(Id,CoreExpr)]  -- Specialised definition
                , UsageDetails )   -- Usage details from specialised RHSs

specCalls spec_imp env existing_rules calls_for_me fn rhs
        -- The first case is the interesting one
  |  notNull calls_for_me               -- And there are some calls to specialise
  && not (isNeverActive (idInlineActivation fn))
        -- Don't specialise NOINLINE things
        -- See Note [Auto-specialisation and RULES]
        --
        -- Don't specialise OPAQUE things, see Note [OPAQUE pragma].
        -- Since OPAQUE things are always never-active (see
        -- GHC.Parser.PostProcess.mkOpaquePragma) this guard never fires for
        -- OPAQUE things.

--   && not (certainlyWillInline (idUnfolding fn))      -- And it's not small
--      See Note [Inline specialisations] for why we do not
--      switch off specialisation for inline functions

  = -- pprTrace "specCalls: some" (vcat
    --   [ text "function" <+> ppr fn
    --   , text "calls:" <+> ppr calls_for_me
    --   , text "subst" <+> ppr (se_subst env) ]) $
    foldlM spec_call ([], [], emptyUDs) calls_for_me

  | otherwise   -- No calls or RHS doesn't fit our preconceptions
  = warnPprTrace (not (exprIsTrivial rhs) && notNull calls_for_me && not (isClassOpId fn))
          "Missed specialisation opportunity for" (ppr fn $$ trace_doc) $
          -- isClassOpId: class-op Ids never inline; we specialise them
          -- through fireRewriteRules. So don't complain about missed opportunities
          -- Note [Specialisation shape]
    -- pprTrace "specCalls: none" (ppr fn <+> ppr calls_for_me) $
    return ([], [], emptyUDs)
  where
    trace_doc = sep [ ppr rhs_bndrs, ppr (idInlineActivation fn) ]

    fn_type   = idType fn
    fn_arity  = idArity fn
    fn_unf    = realIdUnfolding fn  -- Ignore loop-breaker-ness here
    inl_prag  = idInlinePragma fn
    inl_act   = inlinePragmaActivation inl_prag
    is_local  = isLocalId fn
    is_dfun   = isDFunId fn
    dflags    = se_dflags env
    this_mod  = se_module env
        -- Figure out whether the function has an INLINE pragma
        -- See Note [Inline specialisations]

    (rhs_bndrs, rhs_body) = collectBindersPushingCo rhs
                            -- See Note [Account for casts in binding]

    already_covered :: SpecEnv -> [CoreRule] -> [CoreExpr] -> Bool
    already_covered env new_rules args      -- Note [Specialisations already covered]
       = isJust (specLookupRule env fn args (beginPhase inl_act)
                                (new_rules ++ existing_rules))
         -- Rules: we look both in the new_rules (generated by this invocation
         --   of specCalls), and in existing_rules (passed in to specCalls)
         -- inl_act: is the activation we are going to put in the new SPEC
         --   rule; so we want to see if it is covered by another rule with
         --   that same activation.

    ----------------------------------------------------------
        -- Specialise to one particular call pattern
    spec_call :: SpecInfo                         -- Accumulating parameter
              -> CallInfo                         -- Call instance
              -> SpecM SpecInfo
    spec_call spec_acc@(rules_acc, pairs_acc, uds_acc) _ci@(CI { ci_key = call_args })
      = -- See Note [Specialising Calls]
        do { let all_call_args | is_dfun   = saturating_call_args -- See Note [Specialising DFuns]
                               | otherwise = call_args
                 saturating_call_args = call_args ++ map mk_extra_dfun_arg (dropList call_args rhs_bndrs)
                 mk_extra_dfun_arg bndr | isTyVar bndr = UnspecType
                                        | otherwise = UnspecArg

           ; ( useful, rhs_env2, leftover_bndrs
             , rule_bndrs, rule_lhs_args
             , spec_bndrs1, dx_binds, spec_args) <- specHeader env rhs_bndrs all_call_args

--           ; pprTrace "spec_call" (vcat
--                [ text "fun:       "  <+> ppr fn
--                , text "call info: "  <+> ppr _ci
--                , text "useful:    "  <+> ppr useful
--                , text "rule_bndrs:"  <+> ppr rule_bndrs
--                , text "lhs_args:  "  <+> ppr rule_lhs_args
--                , text "spec_bndrs1:" <+> ppr spec_bndrs1
--                , text "leftover_bndrs:" <+> pprIds leftover_bndrs
--                , text "spec_args: "  <+> ppr spec_args
--                , text "dx_binds:  "  <+> ppr dx_binds
--                , text "rhs_bndrs"     <+> ppr rhs_bndrs
--                , text "rhs_body"     <+> ppr rhs_body
--                , text "rhs_env2:  "  <+> ppr (se_subst rhs_env2)
--                , ppr dx_binds ]) $
--             return ()

           ; if not useful  -- No useful specialisation
                || already_covered rhs_env2 rules_acc rule_lhs_args
             then return spec_acc
             else
        do { -- Run the specialiser on the specialised RHS
             -- The "1" suffix is before we maybe add the void arg
           ; (rhs_body', rhs_uds) <- specExpr rhs_env2 rhs_body
                -- Add the { d1' = dx1; d2' = dx2 } usage stuff
                -- to the rhs_uds; see Note [Specialising Calls]
           ; let rhs_uds_w_dx   = dx_binds `consDictBinds` rhs_uds
                 spec_rhs_bndrs = spec_bndrs1 ++ leftover_bndrs
                 (spec_uds, dumped_dbs) = dumpUDs spec_rhs_bndrs rhs_uds_w_dx
                 spec_rhs1 = mkLams spec_rhs_bndrs $
                             wrapDictBindsE dumped_dbs rhs_body'

                 spec_fn_ty1 = exprType spec_rhs1

                 -- Maybe add a void arg to the specialised function,
                 -- to avoid unlifted bindings
                 -- See Note [Specialisations Must Be Lifted]
                 -- C.f. GHC.Core.Opt.WorkWrap.Utils.needsVoidWorkerArg
                 add_void_arg = isUnliftedType spec_fn_ty1 && not (isJoinId fn)
                 (spec_bndrs, spec_rhs, spec_fn_ty)
                   | add_void_arg = ( voidPrimId : spec_bndrs1
                                    , Lam voidArgId spec_rhs1
                                    , mkVisFunTyMany unboxedUnitTy spec_fn_ty1)
                   | otherwise   = (spec_bndrs1, spec_rhs1, spec_fn_ty1)

                 join_arity_decr = length rule_lhs_args - length spec_bndrs

                 --------------------------------------
                 -- Add a suitable unfolding; see Note [Inline specialisations]
                 -- The wrap_unf_body applies the original unfolding to the specialised
                 -- arguments, not forgetting to wrap the dx_binds around the outside (#22358)
                 simpl_opts = initSimpleOpts dflags
                 wrap_unf_body body = foldr (Let . db_bind) (body `mkApps` spec_args) dx_binds
                 spec_unf = specUnfolding simpl_opts spec_bndrs wrap_unf_body
                                          rule_lhs_args fn_unf

                 --------------------------------------
                 -- Adding arity information just propagates it a bit faster
                 --      See Note [Arity decrease] in GHC.Core.Opt.Simplify
                 -- Copy InlinePragma information from the parent Id.
                 -- So if f has INLINE[1] so does spec_fn
                 arity_decr     = count isValArg rule_lhs_args - count isId spec_bndrs

                 spec_inl_prag
                   | not is_local     -- See Note [Specialising imported functions]
                   , isStrongLoopBreaker (idOccInfo fn) -- in GHC.Core.Opt.OccurAnal
                   = neverInlinePragma
                   | otherwise
                   = inl_prag

                 spec_fn_info
                   = vanillaIdInfo `setArityInfo`      max 0 (fn_arity - arity_decr)
                                   `setInlinePragInfo` spec_inl_prag
                                   `setUnfoldingInfo`  spec_unf

                 -- Compute the IdDetails of the specialise Id
                 -- See Note [Transfer IdDetails during specialisation]
                 spec_fn_details
                   = case idDetails fn of
                       JoinId join_arity _ -> JoinId (join_arity - join_arity_decr) Nothing
                       DFunId is_nt        -> DFunId is_nt
                       _                   -> VanillaId

           ; spec_fn <- newSpecIdSM (idName fn) spec_fn_ty spec_fn_details spec_fn_info
           ; let
                -- The rule to put in the function's specialisation is:
                --      forall x @b d1' d2'.
                --          f x @T1 @b @T2 d1' d2' = f1 x @b
                -- See Note [Specialising Calls]
                herald | spec_imp  = -- Specialising imported fn
                                     text "SPEC/" <> ppr this_mod
                       | otherwise = -- Specialising local fn
                                     text "SPEC"

                spec_rule = mkSpecRule dflags this_mod True inl_act
                                    herald fn rule_bndrs rule_lhs_args
                                    (mkVarApps (Var spec_fn) spec_bndrs)

                spec_f_w_arity = spec_fn

                _rule_trace_doc = vcat [ ppr fn <+> dcolon <+> ppr fn_type
                                       , ppr spec_fn  <+> dcolon <+> ppr spec_fn_ty
                                       , ppr rhs_bndrs, ppr call_args
                                       , ppr spec_rule
                                       ]

           ; -- pprTrace "spec_call: rule" _rule_trace_doc
             return ( spec_rule                  : rules_acc
                    , (spec_f_w_arity, spec_rhs) : pairs_acc
                    , spec_uds           `thenUDs` uds_acc
                    ) } }

-- Convenience function for invoking lookupRule from Specialise
-- The SpecEnv's InScopeSet should include all the Vars in the [CoreExpr]
specLookupRule :: SpecEnv -> Id -> [CoreExpr]
               -> CompilerPhase  -- Look up rules as if we were in this phase
               -> [CoreRule] -> Maybe (CoreRule, CoreExpr)
specLookupRule env fn args phase rules
  = lookupRule ropts in_scope_env is_active fn args rules
  where
    dflags       = se_dflags env
    in_scope     = substInScopeSet (se_subst env)
    in_scope_env = ISE in_scope (whenActiveUnfoldingFun is_active)
    ropts        = initRuleOpts dflags
    is_active    = isActive phase

{- Note [Specialising DFuns]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
DFuns have a special sort of unfolding (DFunUnfolding), and it is
hard to specialise a DFunUnfolding to give another DFunUnfolding
unless the DFun is fully applied (#18120).  So, in the case of DFunIds
we simply extend the CallKey with trailing UnspecTypes/UnspecArgs,
so that we'll generate a rule that completely saturates the DFun.

There is an ASSERT that checks this, in the DFunUnfolding case of
GHC.Core.Unfold.Make.specUnfolding.

Note [Transfer IdDetails during specialisation]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When specialising a function, `newSpecIdSM` comes up with a fresh Id the
specialised RHS will be bound to. It is critical that we get the `IdDetails` of
the specialised Id correct:

* JoinId: We want the specialised Id to be a join point, too.  But
  we have to carefully adjust the arity

* DFunId: It is crucial that we also make the new Id a DFunId.
  - First, because it obviously /is/ a DFun, having a DFunUnfolding and
    all that; see Note [Specialising DFuns]

  - Second, DFuns get very delicate special treatment in the demand analyser;
    see GHC.Core.Opt.DmdAnal.enterDFun.  If the specialised function isn't
    also a DFunId, this special treatment doesn't happen, so the demand
    analyser makes a too-strict DFun, and we get an infinite loop.  See Note
    [Do not strictify a DFun's parameter dictionaries] in GHC.Core.Opt.DmdAnal.
    #22549 describes the loop, and (lower down) a case where a /specialised/
    DFun caused a loop.

* WorkerLikeId: Introduced by WW, so after Specialise. Nevertheless, they come
  up when specialising imports. We must keep them as VanillaIds because WW
  will detect them as WorkerLikeIds again. That is, unless specialisation
  allows unboxing of all previous CBV args, in which case sticking to
  VanillaIds was the only correct choice to begin with.

* RecSelId, DataCon*Id, ClassOpId, PrimOpId, FCallId, CoVarId, TickBoxId:
  Never specialised.

Note [Specialisation Must Preserve Sharing]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider a function:

    f :: forall a. Eq a => a -> blah
    f =
      if expensive
         then f1
         else f2

As written, all calls to 'f' will share 'expensive'. But if we specialise 'f'
at 'Int', eg:

    $sfInt = SUBST[a->Int,dict->dEqInt] (if expensive then f1 else f2)

    RULE "SPEC f"
      forall (d :: Eq Int).
        f Int _ = $sfIntf

We've now lost sharing between 'f' and '$sfInt' for 'expensive'. Yikes!

To avoid this, we only generate specialisations for functions whose arity is
enough to bind all of the arguments we need to specialise.  This ensures our
specialised functions don't do any work before receiving all of their dicts,
and thus avoids the 'f' case above.

Note [Specialisations Must Be Lifted]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider a function 'f':

    f = forall a. Eq a => Array# a

used like

    case x of
      True -> ...f @Int dEqInt...
      False -> 0

Naively, we might generate an (expensive) specialisation

    $sfInt :: Array# Int

even in the case that @x = False@! Instead, we add a dummy 'Void#' argument to
the specialisation '$sfInt' ($sfInt :: Void# -> Array# Int) in order to
preserve laziness.

Note [Care with unlifted bindings]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider (#22998)
    f x = let x::ByteArray# = <some literal>
              n::Natural    = NB x
          in wombat @192827 (n |> co)
where
  co :: Natural ~ KnownNat 192827
  wombat :: forall (n:Nat). KnownNat n => blah

Left to itself, the specialiser would float the bindings for `x` and `n` to top
level, so we can specialise `wombat`.  But we can't have a top-level ByteArray#
(see Note [Core letrec invariant] in GHC.Core).  Boo.

This is pretty exotic, so we take a simple way out: in specBind (the NonRec
case) do not float the binding itself unless it satisfies exprIsTopLevelBindable.
This is conservative: maybe the RHS of `x` has a free var that would stop it
floating to top level anyway; but that is hard to spot (since we don't know what
the non-top-level in-scope binders are) and rare (since the binding must satisfy
Note [Core let-can-float invariant] in GHC.Core).


Note [Specialising Calls]
~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose we have a function with a complicated type:

    f :: forall a b c. Int -> Eq a => Show b => c -> Blah
    f @a @b @c i dEqA dShowA x = blah

and suppose it is called at:

    f 7 @T1 @T2 @T3 dEqT1 ($dfShow dShowT2) t3

This call is described as a 'CallInfo' whose 'ci_key' is:

    [ SpecType T1, SpecType T2, UnspecType, UnspecArg, SpecDict dEqT1
    , SpecDict ($dfShow dShowT2), UnspecArg ]

Why are 'a' and 'b' identified as 'SpecType', while 'c' is 'UnspecType'?
Because we must specialise the function on type variables that appear
free in its *dictionary* arguments; but not on type variables that do not
appear in any dictionaries, i.e. are fully polymorphic.

Because this call has dictionaries applied, we'd like to specialise
the call on any type argument that appears free in those dictionaries.
In this case, those are [a :-> T1, b :-> T2].

We also need to substitute the dictionary binders with their
specialised dictionaries. The simplest substitution would be
[dEqA :-> dEqT1, dShowA :-> $dfShow dShowT2], but this duplicates
work, since `$dfShow dShowT2` is a function application. Therefore, we
also want to *float the dictionary out* (via bindAuxiliaryDict),
creating a new dict binding

    dShow1 = $dfShow dShowT2

and the substitution [dEqA :-> dEqT1, dShowA :-> dShow1].

With the substitutions in hand, we can generate a specialised function:

    $sf :: forall c. Int -> c -> Blah
    $sf = SUBST[a :-> T1, b :-> T2, dEqA :-> dEqT1, dShowA :-> dShow1] (\@c i x -> blah)

Note that the substitution is applied to the whole thing.  This is
convenient, but just slightly fragile.  Notably:
  * There had better be no name clashes in a/b/c

We must construct a rewrite rule:

    RULE "SPEC f @T1 @T2 _"
      forall (@c :: Type) (i :: Int) (d1 :: Eq T1) (d2 :: Show T2).
        f @T1 @T2 @c i d1 d2 = $sf @c i

In the rule, d1 and d2 are just wildcards, not used in the RHS.  Note
additionally that 'x' isn't captured by this rule --- we bind only
enough etas in order to capture all of the *specialised* arguments.

Note [Drop dead args from specialisations]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When specialising a function, it’s possible some of the arguments may
actually be dead. For example, consider:

    f :: forall a. () -> Show a => a -> String
    f x y = show y ++ "!"

We might generate the following CallInfo for `f @Int`:

    [SpecType Int, UnspecArg, SpecDict $dShowInt, UnspecArg]

Normally we’d include both the x and y arguments in the
specialisation, since we’re not specialising on either of them. But
that’s silly, since x is actually unused! So we might as well drop it
in the specialisation:

    $sf :: Int -> String
    $sf y = show y ++ "!"

    {-# RULE "SPEC f @Int" forall x. f @Int x $dShow = $sf #-}

This doesn’t save us much, since the arg would be removed later by
worker/wrapper, anyway, but it’s easy to do.

Wrinkles

* Note that we only drop dead arguments if:
    1. We don’t specialise on them.
    2. They come before an argument we do specialise on.
  Doing the latter would require eta-expanding the RULE, which could
  make it match less often, so it’s not worth it. Doing the former could
  be more useful --- it would stop us from generating pointless
  specialisations --- but it’s more involved to implement and unclear if
  it actually provides much benefit in practice.

* If the function has a stable unfolding, specHeader has to come up with
  arguments to pass to that stable unfolding, when building the stable
  unfolding of the specialised function: this is the last field in specHeader's
  big result tuple.

  The right thing to do is to produce a LitRubbish; it should rapidly
  disappear.  Rather like GHC.Core.Opt.WorkWrap.Utils.mk_absent_let.

Note [Specialisation modulo dictionary selectors]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In #19644, we discovered that the ClassOp/DFun rules from
Note [ClassOp/DFun selection] inhibit transitive specialisation.
Example, inspired by T17966:

  class C a where
    m :: Show b => a -> b -> String
    dummy :: a -> () -- Force a datatype dictionary representation

  instance C Int where
    m a b = show a ++ show b
    dummy _ = ()

  f :: (C a, Show b) => a -> b -> String
  f a b = m a b ++ "!"
  {-# INLINABLE[0] f #-}

  main = putStrLn (f (42::Int) (True::Bool))

Here, we specialise `f` at `Int` and `Bool`, giving

  $dC = $fCInt
  $dShow = GHC.Show.$fShowBool
  $sf (a::Int) (b::Bool) =
        ... (m @Int $dC @Bool $dShow a b) ...

Here `m` is just a DictSel, so there is (apparently) nothing to specialise!
However, the next Simplifier run will expose the rewritten instance method:

  ... $fCInt_$cm @Bool $fShowBool a b ...

where $fCInt_$cm is the instance method for `m` in `instance C Int`:

   $fCInt_$cm :: forall b. Show b => Int -> b -> String
   $fCInt_$cm b d x y = show @Int $dShowInt x ++ show @b d y

We want to specialise this! How? By doing the method-selection rewrite in
the Specialiser. Hence

1. In the App case of 'specExpr', try to apply the ClassOp/DFun rule on the
   head of the application, repeatedly, via 'fireRewriteRules'.
2. Attach an unfolding to freshly-bound dictionary ids such as `$dC` and
   `$dShow` in `bindAuxiliaryDict`, so that we can exploit the unfolding
   in 'fireRewriteRules' to do the ClassOp/DFun rewrite.

NB: Without (2), (1) would be pointless, because 'lookupRule' wouldn't be able
to look into the RHS of `$dC` to see the DFun.

Note [Zap occ info in rule binders]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When we generate a specialisation RULE, we need to drop occurrence
info on the binders. If we don’t, things go wrong when we specialise a
function like

    f :: forall a. () -> Show a => a -> String
    f x y = show y ++ "!"

since we’ll generate a RULE like

    RULE "SPEC f @Int" forall x [Occ=Dead].
      f @Int x $dShow = $sf

and Core Lint complains, even though x only appears on the LHS (due to
Note [Drop dead args from specialisations]).

Why is that a Lint error? Because the arguments on the LHS of a rule
are syntactically expressions, not patterns, so Lint treats the
appearance of x as a use rather than a binding. Fortunately, the
solution is simple: we just make sure to zap the occ info before
using ids as wildcard binders in a rule.

Note [Account for casts in binding]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
   f :: Eq a => a -> IO ()
   {-# INLINABLE f
       StableUnf = (/\a \(d:Eq a) (x:a). blah) |> g
     #-}
   f = ...

In f's stable unfolding we have done some modest simplification which
has pushed the cast to the outside.  (I wonder if this is the Right
Thing, but it's what happens now; see GHC.Core.Opt.Simplify.Utils Note [Casts and
lambdas].)  Now that stable unfolding must be specialised, so we want
to push the cast back inside. It would be terrible if the cast
defeated specialisation!  Hence the use of collectBindersPushingCo.

Note [Evidence foralls]
~~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose (#12212) that we are specialising
   f :: forall a b. (Num a, F a ~ F b) => blah
with a=b=Int. Then the RULE will be something like
   RULE forall (d:Num Int) (g :: F Int ~ F Int).
        f Int Int d g = f_spec
But both varToCoreExpr (when constructing the LHS args), and the
simplifier (when simplifying the LHS args), will transform to
   RULE forall (d:Num Int) (g :: F Int ~ F Int).
        f Int Int d <F Int> = f_spec
by replacing g with Refl.  So now 'g' is unbound, which results in a later
crash. So we use Refl right off the bat, and do not forall-quantify 'g':
 * varToCoreExpr generates a Refl
 * exprsFreeIdsList returns the Ids bound by the args,
   which won't include g

You might wonder if this will match as often, but the simplifier replaces
complicated Refl coercions with Refl pretty aggressively.

Note [Orphans and auto-generated rules]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When we specialise an INLINABLE function, or when we have
-fspecialise-aggressively, we auto-generate RULES that are orphans.
We don't want to warn about these, or we'd generate a lot of warnings.
Thus, we only warn about user-specified orphan rules.

Indeed, we don't even treat the module as an orphan module if it has
auto-generated *rule* orphans.  Orphan modules are read every time we
compile, so they are pretty obtrusive and slow down every compilation,
even non-optimised ones.  (Reason: for type class instances it's a
type correctness issue.)  But specialisation rules are strictly for
*optimisation* only so it's fine not to read the interface.

What this means is that a SPEC rules from auto-specialisation in
module M will be used in other modules only if M.hi has been read for
some other reason, which is actually pretty likely.

Note [From non-recursive to recursive]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Even in the non-recursive case, if any dict-binds depend on 'fn' we might
have built a recursive knot

      f a d x = <blah>
      MkUD { ud_binds = NonRec d7  (MkD ..f..)
           , ud_calls = ...(f T d7)... }

The we generate

     Rec { fs x = <blah>[T/a, d7/d]
           f a d x = <blah>
               RULE f T _ = fs
           d7 = ...f... }

Here the recursion is only through the RULE.

However we definitely should /not/ make the Rec in this wildly common
case:
      d = ...
      MkUD { ud_binds = NonRec d7 (...d...)
           , ud_calls = ...(f T d7)... }

Here we want simply to add d to the floats, giving
      MkUD { ud_binds = NonRec d (...)
                        NonRec d7 (...d...)
           , ud_calls = ...(f T d7)... }

In general, we need only make this Rec if
  - there are some specialisations (spec_binds non-empty)
  - there are some dict_binds that depend on f (dump_dbs non-empty)

Note [Avoiding loops (DFuns)]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When specialising /dictionary functions/ we must be very careful to
avoid building loops. Here is an example that bit us badly, on
several distinct occasions.

Here is one: #3591
     class Eq a => C a
     instance Eq [a] => C [a]

This translates to
     dfun :: Eq [a] -> C [a]
     dfun a d = MkD a d (meth d)

     d4 :: Eq [T] = <blah>
     d2 ::  C [T] = dfun T d4
     d1 :: Eq [T] = $p1 d2
     d3 ::  C [T] = dfun T d1

None of these definitions is recursive. What happened was that we
generated a specialisation:
     RULE forall d. dfun T d = dT  :: C [T]
     dT = (MkD a d (meth d)) [T/a, d1/d]
        = MkD T d1 (meth d1)

But now we use the RULE on the RHS of d2, to get
    d2 = dT = MkD d1 (meth d1)
    d1 = $p1 d2

and now d1 is bottom!  The problem is that when specialising 'dfun' we
should first dump "below" the binding all floated dictionary bindings
that mention 'dfun' itself.  So d2 and d3 (and hence d1) must be
placed below 'dfun', and thus unavailable to it when specialising
'dfun'.  That in turn means that the call (dfun T d1) must be
discarded.  On the other hand, the call (dfun T d4) is fine, assuming
d4 doesn't mention dfun.

Solution:
  Discard all calls that mention dictionaries that depend
  (directly or indirectly) on the dfun we are specialising.
  This is done by 'filterCalls'

Note [Avoiding loops (non-DFuns)]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The whole Note [Avoiding loops (DFuns)] things applies only to DFuns.
It's important /not/ to apply filterCalls to non-DFuns. For example:

  class C a where { foo,bar :: [a] -> [a] }

  instance C Int where
     foo x = r_bar x
     bar xs = reverse xs

  r_bar :: C a => [a] -> [a]
  r_bar xs = bar (xs ++ xs)

That translates to:

    r_bar a (c::C a) (xs::[a]) = bar a d (xs ++ xs)

    Rec { $fCInt :: C Int = MkC foo_help reverse
          foo_help (xs::[Int]) = r_bar Int $fCInt xs }

The call (r_bar $fCInt) mentions $fCInt,
                        which mentions foo_help,
                        which mentions r_bar

But we DO want to specialise r_bar at Int:
    Rec { $fCInt :: C Int = MkC foo_help reverse
          foo_help (xs::[Int]) = r_bar Int $fCInt xs

          r_bar a (c::C a) (xs::[a]) = bar a d (xs ++ xs)
            RULE r_bar Int _ = r_bar_Int

          r_bar_Int xs = bar Int $fCInt (xs ++ xs)
           }

Note that, because of its RULE, r_bar joins the recursive
group.  (In this case it'll unravel a short moment later.)
See test simplCore/should_compile/T19599a.

Another example is #19599, which looked like this:

   class (Show a, Enum a) => MyShow a where
      myShow :: a -> String

   myShow_impl :: MyShow a => a -> String

   foo :: Int -> String
   foo = myShow_impl @Int $fMyShowInt

   Rec { $fMyShowInt = MkMyShowD $fEnumInt $fShowInt $cmyShow
       ; $cmyShow = myShow_impl @Int $fMyShowInt }

Here, we really do want to specialise `myShow_impl @Int $fMyShowInt`.


Note [Specialising a recursive group]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
    let rec { f x = ...g x'...
            ; g y = ...f y'.... }
    in f 'a'
Here we specialise 'f' at Char; but that is very likely to lead to
a specialisation of 'g' at Char.  We must do the latter, else the
whole point of specialisation is lost.

But we do not want to keep iterating to a fixpoint, because in the
presence of polymorphic recursion we might generate an infinite number
of specialisations.

So we use the following heuristic:
  * Arrange the rec block in dependency order, so far as possible
    (the occurrence analyser already does this)

  * Specialise it much like a sequence of lets

  * Then go through the block a second time, feeding call-info from
    the RHSs back in the bottom, as it were

In effect, the ordering maxmimises the effectiveness of each sweep,
and we do just two sweeps.   This should catch almost every case of
monomorphic recursion -- the exception could be a very knotted-up
recursion with multiple cycles tied up together.

This plan is implemented in the Rec case of specBindItself.

Note [Specialisations already covered]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We obviously don't want to generate two specialisations for the same
argument pattern.  There are two wrinkles

1. We do the already-covered test in specDefn, not when we generate
the CallInfo in mkCallUDs.  We used to test in the latter place, but
we now iterate the specialiser somewhat, and the Id at the call site
might therefore not have all the RULES that we can see in specDefn

2. What about two specialisations where the second is an *instance*
of the first?  If the more specific one shows up first, we'll generate
specialisations for both.  If the *less* specific one shows up first,
we *don't* currently generate a specialisation for the more specific
one.  (See the call to lookupRule in already_covered.)  Reasons:
  (a) lookupRule doesn't say which matches are exact (bad reason)
  (b) if the earlier specialisation is user-provided, it's
      far from clear that we should auto-specialise further

Note [Auto-specialisation and RULES]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider:
   g :: Num a => a -> a
   g = ...

   f :: (Int -> Int) -> Int
   f w = ...
   {-# RULE f g = 0 #-}

Suppose that auto-specialisation makes a specialised version of
g::Int->Int. That version won't appear in the LHS of the RULE for f.
So if the specialisation rule fires too early, the rule for f may
never fire.

It might be possible to add new rules, to "complete" the rewrite system.
Thus when adding
        RULE forall d. g Int d = g_spec
also add
        RULE f g_spec = 0

But that's a bit complicated.  For now we ask the programmer's help,
by *copying the INLINE activation pragma* to the auto-specialised
rule.  So if g says {-# NOINLINE[2] g #-}, then the auto-spec rule
will also not be active until phase 2.  And that's what programmers
should jolly well do anyway, even aside from specialisation, to ensure
that g doesn't inline too early.

This in turn means that the RULE would never fire for a NOINLINE
thing so not much point in generating a specialisation at all.

Note [Specialisation shape]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
We only specialise a function if it has visible top-level lambdas
corresponding to its overloading.  E.g. if
        f :: forall a. Eq a => ....
then its body must look like
        f = /\a. \d. ...

Reason: when specialising the body for a call (f ty dexp), we want to
substitute dexp for d, and pick up specialised calls in the body of f.

We do allow casts, however; see Note [Account for casts in binding].

This doesn't always work.  One example I came across was this:
        newtype Gen a = MkGen{ unGen :: Int -> a }

        choose :: Eq a => a -> Gen a
        choose n = MkGen (\r -> n)

        oneof = choose (1::Int)

It's a silly example, but we get
        choose = /\a. g `cast` co
where choose doesn't have any dict arguments.  Thus far I have not
tried to fix this (wait till there's a real example).

Mind you, then 'choose' will be inlined (since RHS is trivial) so
it doesn't matter.  This comes up with single-method classes

   class C a where { op :: a -> a }
   instance C a => C [a] where ....
==>
   $fCList :: C a => C [a]
   $fCList = $copList |> (...coercion>...)
   ....(uses of $fCList at particular types)...

So we suppress the WARN if the rhs is trivial.

Note [Inline specialisations]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Here is what we do with the InlinePragma of the original function

  * Activation/RuleMatchInfo: both inherited from the original function

  * InlineSpec: inherit from original function

  * Unfolding: transfer a StableUnfolding iff it is UnfWhen
               See GHC.Core.Unfold.Make.specUnfolding
               and its Note [Specialising unfoldings]

InlineSpec: you might wonder why we specialise INLINE functions at all.
After all they should be inlined, right?  Two reasons:

 * Even INLINE functions are sometimes not inlined, when they aren't
   applied to interesting arguments.  But perhaps the type arguments
   alone are enough to specialise (even though the args are too boring
   to trigger inlining), and it's certainly better to call the
   specialised version.

 * The RHS of an INLINE function might call another overloaded function,
   and we'd like to generate a specialised version of that function too.
   This actually happens a lot. Consider
      replicateM_ :: (Monad m) => Int -> m a -> m ()
      {-# INLINABLE replicateM_ #-}
      replicateM_ d x ma = ...
   The strictness analyser may transform to
      replicateM_ :: (Monad m) => Int -> m a -> m ()
      {-# INLINE replicateM_ #-}
      replicateM_ d x ma = case x of I# x' -> $wreplicateM_ d x' ma

      $wreplicateM_ :: (Monad m) => Int# -> m a -> m ()
      {-# INLINABLE $wreplicateM_ #-}
      $wreplicateM_ = ...
   Now an importing module has a specialised call to replicateM_, say
   (replicateM_ dMonadIO).  We certainly want to specialise $wreplicateM_!
   This particular example had a huge effect on the call to replicateM_
   in nofib/shootout/n-body.
-}

{- *********************************************************************
*                                                                      *
                   SpecArg, and specHeader
*                                                                      *
********************************************************************* -}

-- | An argument that we might want to specialise.
-- See Note [Specialising Calls] for the nitty gritty details.
data SpecArg
  =
    -- | Type arguments that should be specialised, due to appearing
    -- free in the type of a 'SpecDict'.
    SpecType Type

    -- | Type arguments that should remain polymorphic.
  | UnspecType

    -- | Dictionaries that should be specialised. mkCallUDs ensures
    -- that only "interesting" dictionary arguments get a SpecDict;
    -- see Note [Interesting dictionary arguments]
  | SpecDict DictExpr

    -- | Value arguments that should not be specialised.
  | UnspecArg

instance Outputable SpecArg where
  ppr (SpecType t) = text "SpecType" <+> ppr t
  ppr UnspecType   = text "UnspecType"
  ppr (SpecDict d) = text "SpecDict" <+> ppr d
  ppr UnspecArg    = text "UnspecArg"

specArgFreeIds :: SpecArg -> IdSet
specArgFreeIds (SpecType {}) = emptyVarSet
specArgFreeIds (SpecDict dx) = exprFreeIds dx
specArgFreeIds UnspecType    = emptyVarSet
specArgFreeIds UnspecArg     = emptyVarSet

specArgFreeVars :: SpecArg -> VarSet
specArgFreeVars (SpecType ty) = tyCoVarsOfType ty
specArgFreeVars (SpecDict dx) = exprFreeVars dx
specArgFreeVars UnspecType    = emptyVarSet
specArgFreeVars UnspecArg     = emptyVarSet

isSpecDict :: SpecArg -> Bool
isSpecDict (SpecDict {}) = True
isSpecDict _             = False

-- | Given binders from an original function 'f', and the 'SpecArg's
-- corresponding to its usage, compute everything necessary to build
-- a specialisation.
--
-- We will use the running example from Note [Specialising Calls]:
--
--     f :: forall a b c. Int -> Eq a => Show b => c -> Blah
--     f @a @b @c i dEqA dShowB x = blah
--
-- Suppose we decide to specialise it at the following pattern:
--
--     [ SpecType T1, SpecType T2, UnspecType, UnspecArg
--     , SpecDict dEqT1, SpecDict ($dfShow dShowT2), UnspecArg ]
--
-- We'd eventually like to build the RULE
--
--     RULE "SPEC f @T1 @T2 _"
--       forall (@c :: Type) (i :: Int) (d1 :: Eq T1) (d2 :: Show T2).
--         f @T1 @T2 @c i d1 d2 = $sf @c i
--
-- and the specialisation '$sf'
--
--     $sf :: forall c. Int -> c -> Blah
--     $sf = SUBST[a :-> T1, b :-> T2, dEqA :-> dEqT1, dShowB :-> dShow1] (\@c i x -> blah)
--
-- where dShow1 is a floated binding created by bindAuxiliaryDict.
--
-- The cases for 'specHeader' below are presented in the same order as this
-- running example. The result of 'specHeader' for this example is as follows:
--
--    ( -- Returned arguments
--      env + [a :-> T1, b :-> T2, dEqA :-> dEqT1, dShowB :-> dShow1]
--    , [x]
--
--      -- RULE helpers
--    , [c, i, d1, d2]
--    , [T1, T2, c, i, d1, d2]
--
--      -- Specialised function helpers
--    , [c, i, x]
--    , [dShow1 = $dfShow dShowT2]
--    , [T1, T2, c, i, dEqT1, dShow1]
--    )
specHeader
     :: SpecEnv
     -> [InBndr]    -- The binders from the original function 'f'
     -> [SpecArg]   -- From the CallInfo
     -> SpecM ( Bool     -- True <=> some useful specialisation happened
                         -- Not the same as any (isSpecDict args) because
                         -- the args might be longer than bndrs

                -- Returned arguments
              , SpecEnv      -- Substitution to apply to the body of 'f'
              , [OutBndr]    -- Leftover binders from the original function 'f'
                             --   that don’t have a corresponding SpecArg

                -- RULE helpers
              , [OutBndr]    -- Binders for the RULE
              , [OutExpr]    -- Args for the LHS of the rule

                -- Specialised function helpers
              , [OutBndr]    -- Binders for $sf
              , [DictBind]   -- Auxiliary dictionary bindings
              , [OutExpr]    -- Specialised arguments for unfolding
                             -- Same length as "Args for LHS of rule"
              )

-- We want to specialise on type 'T1', and so we must construct a substitution
-- 'a->T1', as well as a LHS argument for the resulting RULE and unfolding
-- details.
specHeader env (bndr : bndrs) (SpecType ty : args)
  = do { -- Find qvars, the type variables to add to the binders for the rule
         -- Namely those free in `ty` that aren't in scope
         -- See (MP2) in Note [Specialising polymorphic dictionaries]
         let in_scope = Core.substInScopeSet (se_subst env)
             qvars    = scopedSort $
                        filterOut (`elemInScopeSet` in_scope) $
                        tyCoVarsOfTypeList ty
             (env1, qvars') = substBndrs env qvars
             ty'            = substTy env1 ty
             env2           = extendTvSubst env1 bndr ty'
       ; (useful, env3, leftover_bndrs, rule_bs, rule_es, bs', dx, spec_args)
            <- specHeader env2 bndrs args
       ; pure ( useful
              , env3
              , leftover_bndrs
              , qvars' ++ rule_bs
              , Type ty' : rule_es
              , qvars' ++ bs'
              , dx
              , Type ty' : spec_args
              )
       }

-- Next we have a type that we don't want to specialise. We need to perform
-- a substitution on it (in case the type refers to 'a'). Additionally, we need
-- to produce a binder, LHS argument and RHS argument for the resulting rule,
-- /and/ a binder for the specialised body.
specHeader env (bndr : bndrs) (UnspecType : args)
  = do { let (env', bndr') = substBndr env bndr
       ; (useful, env'', leftover_bndrs, rule_bs, rule_es, bs', dx, spec_args)
            <- specHeader env' bndrs args
       ; pure ( useful
              , env''
              , leftover_bndrs
              , bndr' : rule_bs
              , varToCoreExpr bndr' : rule_es
              , bndr' : bs'
              , dx
              , varToCoreExpr bndr' : spec_args
              )
       }

-- Next we want to specialise the 'Eq a' dict away. We need to construct
-- a wildcard binder to match the dictionary (See Note [Specialising Calls] for
-- the nitty-gritty), as a LHS rule and unfolding details.
specHeader env (bndr : bndrs) (SpecDict d : args)
  | not (isDeadBinder bndr)
  , allVarSet (`elemInScopeSet` in_scope) (exprFreeVars d)
    -- See Note [Weird special case for SpecDict]
  = do { (env1, bndr') <- newDictBndr env bndr -- See Note [Zap occ info in rule binders]
       ; let (env2, dx_bind, spec_dict) = bindAuxiliaryDict env1 bndr bndr' d
       ; (_, env3, leftover_bndrs, rule_bs, rule_es, bs', dx, spec_args)
             <- specHeader env2 bndrs args
       ; pure ( True      -- Ha!  A useful specialisation!
              , env3
              , leftover_bndrs
              -- See Note [Evidence foralls]
              , exprFreeIdsList (varToCoreExpr bndr') ++ rule_bs
              , varToCoreExpr bndr' : rule_es
              , bs'
              , maybeToList dx_bind ++ dx
              , spec_dict : spec_args
              )
       }
   where
     in_scope = Core.substInScopeSet (se_subst env)

-- Finally, we don't want to specialise on this argument 'i':
--   - It's an UnSpecArg, or
--   - It's a dead dictionary
-- We need to produce a binder, LHS and RHS argument for the RULE, and
-- a binder for the specialised body.
--
-- NB: Calls to 'specHeader' will trim off any trailing 'UnspecArg's, which is
-- why 'i' doesn't appear in our RULE above. But we have no guarantee that
-- there aren't 'UnspecArg's which come /before/ all of the dictionaries, so
-- this case must be here.
specHeader env (bndr : bndrs) (_ : args)
    -- The "_" can be UnSpecArg, or SpecDict where the bndr is dead
  = do { -- see Note [Zap occ info in rule binders]
         let (env', bndr') = substBndr env (zapIdOccInfo bndr)
       ; (useful, env'', leftover_bndrs, rule_bs, rule_es, bs', dx, spec_args)
             <- specHeader env' bndrs args

       ; let bndr_ty = idType bndr'

             -- See Note [Drop dead args from specialisations]
             -- C.f. GHC.Core.Opt.WorkWrap.Utils.mk_absent_let
             (mb_spec_bndr, spec_arg)
                | isDeadBinder bndr
                , Just lit_expr <- mkLitRubbish bndr_ty
                = (Nothing, lit_expr)
                | otherwise
                = (Just bndr', varToCoreExpr bndr')

       ; pure ( useful
              , env''
              , leftover_bndrs
              , bndr' : rule_bs
              , varToCoreExpr bndr' : rule_es
              , case mb_spec_bndr of
                  Just b' -> b' : bs'
                  Nothing -> bs'
              , dx
              , spec_arg : spec_args
              )
       }

-- If we run out of binders, stop immediately
-- See Note [Specialisation Must Preserve Sharing]
specHeader env [] _ = pure (False, env, [], [], [], [], [], [])

-- Return all remaining binders from the original function. These have the
-- invariant that they should all correspond to unspecialised arguments, so
-- it's safe to stop processing at this point.
specHeader env bndrs []
  = pure (False, env', bndrs', [], [], [], [], [])
  where
    (env', bndrs') = substBndrs env bndrs


-- | Binds a dictionary argument to a fresh name, to preserve sharing
bindAuxiliaryDict
  :: SpecEnv
  -> InId -> OutId -> OutExpr -- Original dict binder, and the witnessing expression
  -> ( SpecEnv        -- Substitutes for orig_dict_id
     , Maybe DictBind -- Auxiliary dict binding, if any
     , OutExpr)       -- Witnessing expression (always trivial)
bindAuxiliaryDict env@(SE { se_subst = subst })
                  orig_dict_id fresh_dict_id dict_expr

  -- If the dictionary argument is trivial,
  -- don’t bother creating a new dict binding; just substitute
  | exprIsTrivial dict_expr
  = let env' = env { se_subst = Core.extendSubst subst orig_dict_id dict_expr }
    in -- pprTrace "bindAuxiliaryDict:trivial" (ppr orig_dict_id <+> ppr dict_id) $
       (env', Nothing, dict_expr)

  | otherwise  -- Non-trivial dictionary arg; make an auxiliary binding
  = let fresh_dict_id' = fresh_dict_id `addDictUnfolding` dict_expr

        dict_bind = mkDB (NonRec fresh_dict_id' dict_expr)
        env' = env { se_subst = Core.extendSubst subst orig_dict_id (Var fresh_dict_id')
                                `Core.extendSubstInScope` fresh_dict_id' }
                                -- Ensure the new unfolding is in the in-scope set
    in -- pprTrace "bindAuxiliaryDict:non-trivial" (ppr orig_dict_id <+> ppr fresh_dict_id') $
       (env', Just dict_bind, Var fresh_dict_id')

addDictUnfolding :: Id -> CoreExpr -> Id
-- Add unfolding for freshly-bound Ids: see Note [Make the new dictionaries interesting]
-- and Note [Specialisation modulo dictionary selectors]
addDictUnfolding id rhs
  = id `setIdUnfolding` mkSimpleUnfolding defaultUnfoldingOpts rhs

{-
Note [Make the new dictionaries interesting]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Important!  We're going to substitute dx_id1 for d
and we want it to look "interesting", else we won't gather *any*
consequential calls. E.g.
    f d = ...g d....
If we specialise f for a call (f (dfun dNumInt)), we'll get
a consequent call (g d') with an auxiliary definition
    d' = df dNumInt
We want that consequent call to look interesting; so we add an unfolding
in the dictionary Id.
-}


{- *********************************************************************
*                                                                      *
            UsageDetails and suchlike
*                                                                      *
********************************************************************* -}

data UsageDetails
  = MkUD { ud_binds :: !FloatedDictBinds
         , ud_calls :: !CallDetails }
    -- INVARIANT: suppose bs = fdb_bndrs ud_binds
    -- Then 'calls' may *mention* 'bs',
    -- but there should be no calls *for* bs

data FloatedDictBinds  -- See Note [Floated dictionary bindings]
  = FDB { fdb_binds :: !(OrdList DictBind)
               -- The order is important;
               -- in ds1 `appOL` ds2, bindings in ds2 can depend on those in ds1

        , fdb_bndrs :: !IdSet
    }          -- ^ The binders of 'fdb_binds'.
               -- Caches a superset of the expression
               --   `mkVarSet (bindersOfDictBinds fdb_binds))`
               -- for later addition to an InScopeSet

-- | A 'DictBind' is a binding along with a cached set containing its free
-- variables (both type variables and dictionaries). We need this set
-- in splitDictBinds, when filtering bindings to decide which are
-- captured by a binder
data DictBind = DB { db_bind :: CoreBind, db_fvs :: VarSet }

bindersOfDictBind :: DictBind -> [Id]
bindersOfDictBind = bindersOf . db_bind

bindersOfDictBinds :: Foldable f => f DictBind -> [Id]
bindersOfDictBinds = bindersOfBinds . foldr ((:) . db_bind) []

{- Note [Floated dictionary bindings]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We float out dictionary bindings for the reasons described under
"Dictionary floating" above.  But not /just/ dictionary bindings.
Consider

   f :: Eq a => blah
   f a d = rhs

   $c== :: T -> T -> Bool
   $c== x y = ...

   $df :: Eq T
   $df = Eq $c== ...

   gurgle = ...(f @T $df)...

We gather the call info for (f @T $df), and we don't want to drop it
when we come across the binding for $df.  So we add $df to the floats
and continue.  But then we have to add $c== to the floats, and so on.
These all float above the binding for 'f', and now we can
successfully specialise 'f'.

So the DictBinds in (ud_binds :: OrdList DictBind) may contain
non-dictionary bindings too.

Note [Specialising polymorphic dictionaries]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Note June 2023: This has proved to be quite a tricky optimisation to get right
see (#23469, #23109, #21229, #23445) so it is now guarded by a flag
`-fpolymorphic-specialisation`.


Consider
    class M a where { foo :: a -> Int }

    instance M (ST s) where ...
    -- dMST :: forall s. M (ST s)

    wimwam :: forall a. M a => a -> Int
    wimwam = /\a \(d::M a). body

    f :: ST s -> Int
    f = /\s \(x::ST s). wimwam @(ST s) (dMST @s) dx + 1

We'd like to specialise wimwam at (ST s), thus
    $swimwam :: forall s. ST s -> Int
    $swimwam = /\s. body[ST s/a, (dMST @s)/d]

    RULE forall s (d :: M (ST s)).
         wimwam @(ST s) d = $swimwam @s

Here are the moving parts:

(MP1) We must /not/ dump the CallInfo
        CIS wimwam (CI { ci_key = [@(ST s), dMST @s]
                       , ci_fvs = {dMST} })
      when we come to the /\s.  Instead, we simply let it continue to float
      upwards. Hence ci_fvs is an IdSet, listing the /Ids/ that
      are free in the call, but not the /TyVars/.  Hence using specArgFreeIds
      in singleCall.

  NB to be fully kosher we should explicitly quantifying the CallInfo
  over 's', but we don't bother.  This would matter if there was an
  enclosing binding of the same 's', which I don't expect to happen.

(MP2) When we come to specialise the call, we must remember to quantify
      over 's'.  That is done in the SpecType case of specHeader, where
      we add 's' (called qvars) to the binders of the RULE and the specialised
      function.

(MP3) If we have f :: forall m. Monoid m => blah, and two calls
        (f @(Endo b)      (d :: Monoid (Endo b))
        (f @(Endo (c->c)) (d :: Monoid (Endo (c->c)))
      we want to generate a specialisation only for the first.  The second
      is just a substitution instance of the first, with no greater specialisation.
      Hence the call to `remove_dups` in `filterCalls`.

All this arose in #13873, in the unexpected form that a SPECIALISE
pragma made the program slower!  The reason was that the specialised
function $sinsertWith arising from the pragma looked rather like `f`
above, and failed to specialise a call in its body like wimwam.
Without the pragma, the original call to `insertWith` was completely
monomorpic, and specialised in one go.

Wrinkles.

* See Note [Weird special case for SpecDict]

* With -XOverlappingInstances you might worry about this:
    class C a where ...
    instance C (Maybe Int) where ...   -- $df1 :: C (Maybe Int)
    instance C (Maybe a)   where ...   -- $df2 :: forall a. C (Maybe a)

    f :: C a => blah
    f = rhs

    g = /\a.  ...(f @(Maybe a) ($df2 a))...
    h = ...f @(Maybe Int) $df1

  There are two calls to f, but with different evidence.  This patch will
  combine them into one.  But it's OK: this code will never arise unless you
  use -XIncoherentInstances.  Even with -XOverlappingInstances, GHC tries hard
  to keep dictionaries as singleton types.  But that goes out of the window
  with -XIncoherentInstances -- and that is true even with ordianry type-class
  specialisation (at least if any inlining has taken place).

  GHC makes very few guarantees when you use -XIncoherentInstances, and its
  not worth crippling the normal case for the incoherent corner.  (The best
  thing might be to switch off specialisation altogether if incoherence is
  involved... but incoherence is a property of an instance, not a class, so
  it's a hard test to make.)

  But see Note [Specialisation and overlapping instances].

Note [Weird special case for SpecDict]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose we are trying to specialise for this this call:
   $wsplit @T (mkD @k @(a::k) :: C T)
where
   mkD :: forall k (a::k). C T
is a top-level dictionary-former.  This actually happened in #22459,
because of (MP1) of Note [Specialising polymorphic dictionaries].

How can we specialise $wsplit?  We might try

   RULE "SPEC" forall (d :: C T). $wsplit @T d = $s$wsplit

but then in the body of $s$wsplit what will we use for the dictionary
evidence?  We can't use (mkD @k @(a::k)) because k and a aren't in scope.
We could zap `k` to (Any @Type) and `a` to (Any @(Any @Type)), but that
is a lot of hard work for a very strange case.

So we simply refrain from specialising in this case; hence the guard
   allVarSet (`elemInScopeSet` in_scope) (exprFreeVars d)
in the SpecDict cased of specHeader.

How did this strange polymorphic mkD arise in the first place?
From GHC.Core.Opt.Utils.abstractFloats, which was abstracting
over too many type variables. But that too is now fixed;
see Note [Which type variables to abstract over] in that module.
-}

instance Outputable DictBind where
  ppr (DB { db_bind = bind, db_fvs = fvs })
    = text "DB" <+> braces (sep [ text "fvs: " <+> ppr fvs
                                , text "bind:" <+> ppr bind ])

instance Outputable UsageDetails where
  ppr (MkUD { ud_binds = dbs, ud_calls = calls })
        = text "MkUD" <+> braces (sep (punctuate comma
                [text "binds" <+> equals <+> ppr dbs,
                 text "calls" <+> equals <+> ppr calls]))

instance Outputable FloatedDictBinds where
  ppr (FDB { fdb_binds = binds }) = ppr binds

emptyUDs :: UsageDetails
emptyUDs = MkUD { ud_binds = emptyFDBs, ud_calls = emptyDVarEnv }


emptyFDBs :: FloatedDictBinds
emptyFDBs = FDB { fdb_binds = nilOL, fdb_bndrs = emptyVarSet }

------------------------------------------------------------
type CallDetails  = DIdEnv CallInfoSet
  -- The order of specialized binds and rules depends on how we linearize
  -- CallDetails, so to get determinism we must use a deterministic set here.
  -- See Note [Deterministic UniqFM] in GHC.Types.Unique.DFM

data CallInfoSet = CIS Id (Bag CallInfo)
  -- The list of types and dictionaries is guaranteed to
  -- match the type of f
  -- The Bag may contain duplicate calls (i.e. f @T and another f @T)
  -- These dups are eliminated by already_covered in specCalls

data CallInfo
  = CI { ci_key  :: [SpecArg]   -- All arguments
       , ci_fvs  :: IdSet       -- Free Ids of the ci_key call
                                -- /not/ including the main id itself, of course
                                -- NB: excluding tyvars:
                                --     See Note [Specialising polymorphic dictionaries]
    }

type DictExpr = CoreExpr

ciSetFilter :: (CallInfo -> Bool) -> CallInfoSet -> CallInfoSet
ciSetFilter p (CIS id a) = CIS id (filterBag p a)

instance Outputable CallInfoSet where
  ppr (CIS fn map) = hang (text "CIS" <+> ppr fn)
                        2 (ppr map)

pprCallInfo :: Id -> CallInfo -> SDoc
pprCallInfo fn (CI { ci_key = key })
  = ppr fn <+> ppr key

instance Outputable CallInfo where
  ppr (CI { ci_key = key, ci_fvs = _fvs })
    = text "CI" <> braces (sep (map ppr key))

unionCalls :: CallDetails -> CallDetails -> CallDetails
unionCalls c1 c2 = plusDVarEnv_C unionCallInfoSet c1 c2

unionCallInfoSet :: CallInfoSet -> CallInfoSet -> CallInfoSet
unionCallInfoSet (CIS f calls1) (CIS _ calls2) =
  CIS f (calls1 `unionBags` calls2)

callDetailsFVs :: CallDetails -> VarSet
callDetailsFVs calls =
  nonDetStrictFoldUDFM (unionVarSet . callInfoFVs) emptyVarSet calls
  -- It's OK to use nonDetStrictFoldUDFM here because we forget the ordering
  -- immediately by converting to a nondeterministic set.

callInfoFVs :: CallInfoSet -> VarSet
callInfoFVs (CIS _ call_info) =
  foldr (\(CI { ci_fvs = fv }) vs -> unionVarSet fv vs) emptyVarSet call_info

getTheta :: [PiTyBinder] -> [PredType]
getTheta = fmap piTyBinderType . filter isInvisiblePiTyBinder . filter isAnonPiTyBinder


------------------------------------------------------------
singleCall :: SpecEnv -> Id -> [SpecArg] -> UsageDetails
singleCall spec_env id args
  = MkUD {ud_binds = emptyFDBs,
          ud_calls = unitDVarEnv id $ CIS id $
                     unitBag (CI { ci_key  = args
                                 , ci_fvs  = call_fvs }) }
  where
    call_fvs =
      foldr (unionVarSet . free_var_fn) emptyVarSet args

    free_var_fn =
      if gopt Opt_PolymorphicSpecialisation (se_dflags spec_env)
        then specArgFreeIds
        else specArgFreeVars



        -- specArgFreeIds: we specifically look for free Ids, not TyVars
        --    see (MP1) in Note [Specialising polymorphic dictionaries]
        --
        -- We don't include the 'id' itself.

mkCallUDs :: SpecEnv -> OutExpr -> [OutExpr] -> UsageDetails
mkCallUDs env fun args
  | (_, Var f) <- stripTicksTop tickishFloatable fun -- See Note [Ticks on applications]
  = -- pprTraceWith "mkCallUDs" (\res -> vcat [ ppr f, ppr args, ppr res ]) $
    mkCallUDs' env f args
  | otherwise
  = emptyUDs

mkCallUDs' :: SpecEnv -> Id -> [OutExpr] -> UsageDetails
mkCallUDs' env f args
  | wantCallsFor env f    -- We want it, and...
  , not (null ci_key)     -- this call site has a useful specialisation
  = -- pprTrace "mkCallUDs: keeping" _trace_doc
    singleCall env f ci_key

  | otherwise  -- See also Note [Specialisations already covered]
  = -- pprTrace "mkCallUDs: discarding" _trace_doc
    emptyUDs

  where
    _trace_doc = vcat [ppr f, ppr args, ppr ci_key]
    pis                = fst $ splitPiTys $ idType f
    constrained_tyvars = tyCoVarsOfTypes $ getTheta pis

    ci_key :: [SpecArg]
    ci_key = dropWhileEndLE (not . isSpecDict) $
             zipWith mk_spec_arg args pis
             -- Drop trailing args until we get to a SpecDict
             -- In this way the RULE has as few args as possible,
             -- which broadens its applicability, since rules only
             -- fire when saturated

    mk_spec_arg :: OutExpr -> PiTyBinder -> SpecArg
    mk_spec_arg arg (Named bndr)
      |  binderVar bndr `elemVarSet` constrained_tyvars
      = case arg of
          Type ty -> SpecType ty
          _       -> pprPanic "ci_key" $ ppr arg
      |  otherwise = UnspecType

    -- For "invisibleFunArg", which are the type-class dictionaries,
    -- we decide on a case by case basis if we want to specialise
    -- on this argument; if so, SpecDict, if not UnspecArg
    mk_spec_arg arg (Anon pred af)
      | isInvisibleFunArg af
      , interestingDict arg (scaledThing pred)
              -- See Note [Interesting dictionary arguments]
      = SpecDict arg

      | otherwise = UnspecArg

{-
Note [Ticks on applications]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Ticks such as source location annotations can sometimes make their way
onto applications (see e.g. #21697). So if we see something like

    App (Tick _ f) e

we need to descend below the tick to find what the real function being
applied is.

The resulting RULE also has to be able to match this annotated use
site, so we only look through ticks that RULE matching looks through
(see Note [Tick annotations in RULE matching] in GHC.Core.Rules).
-}

wantCallsFor :: SpecEnv -> Id -> Bool
wantCallsFor _env _f = True
 -- We could reduce the size of the UsageDetails by being less eager
 -- about collecting calls for LocalIds: there is no point for
 -- ones that are lambda-bound.  We can't decide this by looking at
 -- the (absence of an) unfolding, because unfoldings for local
 -- functions are discarded by cloneBindSM, so no local binder will
 -- have an unfolding at this stage.  We'd have to keep a candidate
 -- set of let-binders.
 --
 -- Not many lambda-bound variables have dictionary arguments, so
 -- this would make little difference anyway.
 --
 -- For imported Ids we could check for an unfolding, but we have to
 -- do so anyway in canSpecImport, and it seems better to have it
 -- all in one place.  So we simply collect usage info for imported
 -- overloaded functions.

{- Note [Interesting dictionary arguments]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider this
         \a.\d:Eq a.  let f = ... in ...(f d)...
There really is not much point in specialising f wrt the dictionary d,
because the code for the specialised f is not improved at all, because
d is lambda-bound.  We simply get junk specialisations.

What is "interesting"?  Just that it has *some* structure.  But what about
variables?  We look in the variable's /unfolding/.  And that means
that we must be careful to ensure that dictionaries have unfoldings,

* cloneBndrSM discards non-Stable unfoldings
* specBind updates the unfolding after specialisation
  See Note [Update unfolding after specialisation]
* bindAuxiliaryDict adds an unfolding for an aux dict
  see Note [Specialisation modulo dictionary selectors]
* specCase adds unfoldings for the new bindings it creates

We accidentally lost accurate tracking of local variables for a long
time, because cloned variables didn't have unfoldings. But makes a
massive difference in a few cases, eg #5113. For nofib as a
whole it's only a small win: 2.2% improvement in allocation for ansi,
1.2% for bspt, but mostly 0.0!  Average 0.1% increase in binary size.

Note [Update unfolding after specialisation]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider (#21848)

  wombat :: Show b => Int -> b -> String
  wombat a b | a>0       = wombat (a-1) b
             | otherwise = show a ++ wombat a b

  class C a where
    meth :: Show b => a -> b -> String
    dummy :: a -> () -- Force a datatype dictionary representation

  instance C Int where
    meth = wombat
    dummy _ = ()

  class C a => D a   -- D has C as a superclass
  instance D Int

  f :: (D a, Show b) => a -> b -> String
  {-# INLINABLE[0] f #-}
  f a b = meth a b ++ "!" ++ meth a b

Now `f` turns into:

  f @a @b (dd :: D a) (ds :: Show b) a b
     = let dc :: D a = %p1 dd  -- Superclass selection
       in meth @a dc ....
          meth @a dc ....

When we specialise `f`, at a=Int say, that superclass selection can
nfire (via rewiteClassOps), but that info (that 'dc' is now a
particular dictionary `C`, of type `C Int`) must be available to
the call `meth @a dc`, so that we can fire the `meth` class-op, and
thence specialise `wombat`.

We deliver on this idea by updating the unfolding for the binder
in the NonRec case of specBind.  (This is too exotic to trouble with
the Rec case.)
-}

interestingDict :: CoreExpr -> Type -> Bool
-- A dictionary argument is interesting if it has *some* structure,
-- see Note [Interesting dictionary arguments]
-- NB: "dictionary" arguments include constraints of all sorts,
--     including equality constraints; hence the Coercion case
-- To make this work, we need to ensure that dictionaries have
-- unfoldings in them.
interestingDict arg arg_ty
  | not (typeDeterminesValue arg_ty) = False   -- See Note [Type determines value]
  | otherwise                        = go arg
  where
    go (Var v)               =  hasSomeUnfolding (idUnfolding v)
                             || isDataConWorkId v
    go (Type _)              = False
    go (Coercion _)          = False
    go (App fn (Type _))     = go fn
    go (App fn (Coercion _)) = go fn
    go (Tick _ a)            = go a
    go (Cast e _)            = go e
    go _                     = True

thenUDs :: UsageDetails -> UsageDetails -> UsageDetails
thenUDs (MkUD {ud_binds = db1, ud_calls = calls1})
        (MkUD {ud_binds = db2, ud_calls = calls2})
  = MkUD { ud_binds       = db1    `thenFDBs`   db2
         , ud_calls       = calls1 `unionCalls`  calls2 }

thenFDBs :: FloatedDictBinds -> FloatedDictBinds -> FloatedDictBinds
-- Combine FloatedDictBinds
-- In (dbs1 `thenFDBs` dbs2), dbs2 may mention dbs1 but not vice versa
thenFDBs (FDB { fdb_binds = dbs1, fdb_bndrs = bs1 })
         (FDB { fdb_binds = dbs2, fdb_bndrs = bs2 })
  = FDB { fdb_binds = dbs1 `appOL` dbs2
        , fdb_bndrs = bs1  `unionVarSet` bs2 }

-----------------------------
_dictBindBndrs :: OrdList DictBind -> [Id]
_dictBindBndrs dbs = foldr ((++) . bindersOf . db_bind) [] dbs

-- | Construct a 'DictBind' from a 'CoreBind'
mkDB :: CoreBind -> DictBind
mkDB bind = DB { db_bind = bind, db_fvs = bind_fvs bind }

-- | Identify the free variables of a 'CoreBind'
bind_fvs :: CoreBind -> VarSet
bind_fvs (NonRec bndr rhs) = pair_fvs (bndr,rhs)
bind_fvs (Rec prs)         = rhs_fvs `delVarSetList` (map fst prs)
                           where
                             rhs_fvs = unionVarSets (map pair_fvs prs)

pair_fvs :: (Id, CoreExpr) -> VarSet
pair_fvs (bndr, rhs) = exprSomeFreeVars interesting rhs
                       `unionVarSet` idFreeVars bndr
        -- idFreeVars: don't forget variables mentioned in
        -- the rules of the bndr.  C.f. OccAnal.addRuleUsage
        -- Also tyvars mentioned in its type; they may not appear
        -- in the RHS
        --      type T a = Int
        --      x :: T a = 3
  where
    interesting :: InterestingVarFun
    interesting v = isLocalVar v || (isId v && isDFunId v)
        -- Very important: include DFunIds /even/ if it is imported
        -- Reason: See Note [Avoiding loops in specImports], the #13429
        --         example involving an imported dfun.  We must know
        --         whether a dictionary binding depends on an imported
        --         DFun in case we try to specialise that imported DFun

-- | Flatten a set of "dumped" 'DictBind's, and some other binding
-- pairs, into a single recursive binding.
recWithDumpedDicts :: [(Id,CoreExpr)] -> OrdList DictBind -> DictBind
recWithDumpedDicts pairs dbs
  = DB { db_bind = Rec bindings
       , db_fvs = fvs `delVarSetList` map fst bindings }
  where
    (bindings, fvs) = foldr add ([], emptyVarSet)
                                (dbs `snocOL` mkDB (Rec pairs))
    add (DB { db_bind = bind, db_fvs = fvs }) (prs_acc, fvs_acc)
      = case bind of
          NonRec b r -> ((b,r) : prs_acc, fvs')
          Rec prs1   -> (prs1 ++ prs_acc, fvs')
      where
        fvs' = fvs_acc `unionVarSet` fvs

snocDictBind :: UsageDetails -> DictBind -> UsageDetails
snocDictBind uds@MkUD{ud_binds= FDB { fdb_binds = dbs, fdb_bndrs = bs }} db
  = uds { ud_binds = FDB { fdb_binds = dbs `snocOL` db
                         , fdb_bndrs = bs `extendVarSetList` bindersOfDictBind db } }

snocDictBinds :: UsageDetails -> [DictBind] -> UsageDetails
-- Add ud_binds to the tail end of the bindings in uds
snocDictBinds uds@MkUD{ud_binds=FDB{ fdb_binds = binds, fdb_bndrs = bs }} dbs
  = uds { ud_binds = FDB { fdb_binds = binds `appOL`        (toOL dbs)
                         , fdb_bndrs = bs    `extendVarSetList` bindersOfDictBinds dbs } }

consDictBinds :: [DictBind] -> UsageDetails -> UsageDetails
consDictBinds dbs uds@MkUD{ud_binds=FDB{fdb_binds = binds, fdb_bndrs = bs}}
  = uds { ud_binds = FDB{ fdb_binds = toOL dbs `appOL` binds
                        , fdb_bndrs = bs `extendVarSetList` bindersOfDictBinds dbs } }

wrapDictBinds :: FloatedDictBinds -> [CoreBind] -> [CoreBind]
wrapDictBinds (FDB { fdb_binds = dbs }) binds
  = foldr add binds dbs
  where
    add (DB { db_bind = bind }) binds = bind : binds

wrapDictBindsE :: OrdList DictBind -> CoreExpr -> CoreExpr
wrapDictBindsE dbs expr
  = foldr add expr dbs
  where
    add (DB { db_bind = bind }) expr = Let bind expr

----------------------
dumpUDs :: [CoreBndr] -> UsageDetails -> (UsageDetails, OrdList DictBind)
-- Used at a lambda or case binder; just dump anything mentioning the binder
dumpUDs bndrs uds@(MkUD { ud_binds = orig_dbs, ud_calls = orig_calls })
  | null bndrs = (uds, nilOL)  -- Common in case alternatives
  | otherwise  = -- pprTrace "dumpUDs" (ppr bndrs $$ ppr free_uds $$ ppr dump_dbs) $
                 (free_uds, dump_dbs)
  where
    free_uds = uds { ud_binds = free_dbs, ud_calls = free_calls }
    bndr_set = mkVarSet bndrs
    (free_dbs, dump_dbs, dump_set) = splitDictBinds orig_dbs bndr_set
    free_calls = deleteCallsMentioning dump_set $   -- Drop calls mentioning bndr_set on the floor
                 deleteCallsFor bndrs orig_calls    -- Discard calls for bndr_set; there should be
                                                    -- no calls for any of the dicts in dump_dbs

dumpBindUDs :: [CoreBndr] -> UsageDetails -> (UsageDetails, OrdList DictBind, Bool)
-- Used at a let(rec) binding.
-- We return a boolean indicating whether the binding itself is mentioned,
-- directly or indirectly, by any of the ud_calls; in that case we want to
-- float the binding itself;
-- See Note [Floated dictionary bindings]
dumpBindUDs bndrs (MkUD { ud_binds = orig_dbs, ud_calls = orig_calls })
  = -- pprTrace "dumpBindUDs" (ppr bndrs $$ ppr free_uds $$ ppr dump_dbs $$ ppr float_all) $
    (free_uds, dump_dbs, float_all)
  where
    free_uds = MkUD { ud_binds = free_dbs, ud_calls = free_calls }
    bndr_set = mkVarSet bndrs
    (free_dbs, dump_dbs, dump_set) = splitDictBinds orig_dbs bndr_set
    free_calls = deleteCallsFor bndrs orig_calls
    float_all = dump_set `intersectsVarSet` callDetailsFVs free_calls

callsForMe :: Id -> UsageDetails -> (UsageDetails, [CallInfo])
callsForMe fn uds@MkUD { ud_binds = orig_dbs, ud_calls = orig_calls }
  = -- pprTrace ("callsForMe")
    --          (vcat [ppr fn,
    --                 text "Orig dbs ="     <+> ppr (_dictBindBndrs orig_dbs),
    --                 text "Orig calls ="   <+> ppr orig_calls,
    --                 text "Calls for me =" <+> ppr calls_for_me]) $
    (uds_without_me, calls_for_me)
  where
    uds_without_me = uds { ud_calls = delDVarEnv orig_calls fn }
    calls_for_me = case lookupDVarEnv orig_calls fn of
                        Nothing -> []
                        Just cis -> filterCalls cis orig_dbs
         -- filterCalls: drop calls that (directly or indirectly)
         -- refer to fn.  See Note [Avoiding loops (DFuns)]

----------------------
filterCalls :: CallInfoSet -> FloatedDictBinds -> [CallInfo]
-- Remove dominated calls (Note [Specialising polymorphic dictionaries])
-- and loopy DFuns (Note [Avoiding loops (DFuns)])
filterCalls (CIS fn call_bag) (FDB { fdb_binds = dbs })
  | isDFunId fn  -- Note [Avoiding loops (DFuns)] applies only to DFuns
  = filter ok_call de_dupd_calls
  | otherwise         -- Do not apply it to non-DFuns
  = de_dupd_calls  -- See Note [Avoiding loops (non-DFuns)]
  where
    de_dupd_calls = remove_dups call_bag

    dump_set = foldl' go (unitVarSet fn) dbs
      -- This dump-set could also be computed by splitDictBinds
      --   (_,_,dump_set) = splitDictBinds dbs {fn}
      -- But this variant is shorter

    go so_far (DB { db_bind = bind, db_fvs = fvs })
       | fvs `intersectsVarSet` so_far
       = extendVarSetList so_far (bindersOf bind)
       | otherwise = so_far

    ok_call (CI { ci_fvs = fvs }) = fvs `disjointVarSet` dump_set

remove_dups :: Bag CallInfo -> [CallInfo]
-- Calls involving more generic instances beat more specific ones.
-- See (MP3) in Note [Specialising polymorphic dictionaries]
remove_dups calls = foldr add [] calls
  where
    add :: CallInfo -> [CallInfo] -> [CallInfo]
    add ci [] = [ci]
    add ci1 (ci2:cis) | ci2 `beats_or_same` ci1 = ci2:cis
                      | ci1 `beats_or_same` ci2 = ci1:cis
                      | otherwise               = ci2 : add ci1 cis

beats_or_same :: CallInfo -> CallInfo -> Bool
beats_or_same (CI { ci_key = args1 }) (CI { ci_key = args2 })
  = go args1 args2
  where
    go [] _ = True
    go (arg1:args1) (arg2:args2) = go_arg arg1 arg2 && go args1 args2
    go (_:_)        []           = False

    go_arg (SpecType ty1) (SpecType ty2) = isJust (tcMatchTy ty1 ty2)
    go_arg UnspecType     UnspecType     = True
    go_arg (SpecDict {})  (SpecDict {})  = True
    go_arg UnspecArg      UnspecArg      = True
    go_arg _              _              = False

----------------------
splitDictBinds :: FloatedDictBinds -> IdSet -> (FloatedDictBinds, OrdList DictBind, IdSet)
-- splitDictBinds dbs bndrs returns
--   (free_dbs, dump_dbs, dump_set)
-- where
--   * dump_dbs depends, transitively on bndrs
--   * free_dbs does not depend on bndrs
--   * dump_set = bndrs `union` bndrs(dump_dbs)
splitDictBinds (FDB { fdb_binds = dbs, fdb_bndrs = bs }) bndr_set
   = (FDB { fdb_binds = free_dbs
          , fdb_bndrs = bs `minusVarSet` dump_set }
     , dump_dbs, dump_set)
   where
    (free_dbs, dump_dbs, dump_set)
      = foldl' split_db (nilOL, nilOL, bndr_set) dbs
                -- Important that it's foldl' not foldr;
                -- we're accumulating the set of dumped ids in dump_set

    split_db (free_dbs, dump_dbs, dump_idset) db
        | DB { db_bind = bind, db_fvs = fvs } <- db
        , dump_idset `intersectsVarSet` fvs     -- Dump it
        = (free_dbs, dump_dbs `snocOL` db,
           extendVarSetList dump_idset (bindersOf bind))

        | otherwise     -- Don't dump it
        = (free_dbs `snocOL` db, dump_dbs, dump_idset)


----------------------
deleteCallsMentioning :: VarSet -> CallDetails -> CallDetails
-- Remove calls mentioning any Id in bndrs
-- NB: The call is allowed to mention TyVars in bndrs
--     Note [Specialising polymorphic dictionaries]
--     ci_fvs are just the free /Ids/
deleteCallsMentioning bndrs calls
  = mapDVarEnv (ciSetFilter keep_call) calls
  where
    keep_call (CI { ci_fvs = fvs }) = fvs `disjointVarSet` bndrs

deleteCallsFor :: [Id] -> CallDetails -> CallDetails
-- Remove calls *for* bndrs
deleteCallsFor bndrs calls = delDVarEnvList calls bndrs

{-
************************************************************************
*                                                                      *
\subsubsection{Boring helper functions}
*                                                                      *
************************************************************************
-}

type SpecM a = UniqSM a

runSpecM :: SpecM a -> CoreM a
runSpecM thing_inside
  = do { us <- getUniqueSupplyM
       ; return (initUs_ us thing_inside) }

mapAndCombineSM :: (a -> SpecM (b, UsageDetails)) -> [a] -> SpecM ([b], UsageDetails)
mapAndCombineSM _ []     = return ([], emptyUDs)
mapAndCombineSM f (x:xs) = do (y, uds1) <- f x
                              (ys, uds2) <- mapAndCombineSM f xs
                              return (y:ys, uds1 `thenUDs` uds2)

extendTvSubst :: SpecEnv -> TyVar -> Type -> SpecEnv
extendTvSubst env tv ty
  = env { se_subst = Core.extendTvSubst (se_subst env) tv ty }

extendInScope :: SpecEnv -> OutId -> SpecEnv
extendInScope env@(SE { se_subst = subst }) bndr
  = env { se_subst = subst `Core.extendSubstInScope` bndr }

zapSubst :: SpecEnv -> SpecEnv
zapSubst env@(SE { se_subst = subst })
  = env { se_subst = Core.zapSubst subst }

substTy :: SpecEnv -> Type -> Type
substTy env ty = substTyUnchecked (se_subst env) ty

substCo :: SpecEnv -> Coercion -> Coercion
substCo env co = Core.substCo (se_subst env) co

substBndr :: SpecEnv -> CoreBndr -> (SpecEnv, CoreBndr)
substBndr env bs = case Core.substBndr (se_subst env) bs of
                      (subst', bs') -> (env { se_subst = subst' }, bs')

substBndrs :: SpecEnv -> [CoreBndr] -> (SpecEnv, [CoreBndr])
substBndrs env bs = case Core.substBndrs (se_subst env) bs of
                      (subst', bs') -> (env { se_subst = subst' }, bs')

cloneBndrSM :: SpecEnv -> Id -> SpecM (SpecEnv, Id)
-- Clone the binders of the bind; return new bind with the cloned binders
-- Return the substitution to use for RHSs, and the one to use for the body
-- Discards non-Stable unfoldings
cloneBndrSM env@(SE { se_subst = subst }) bndr
  = do { us <- getUniqueSupplyM
       ; let (subst', bndr') = Core.cloneIdBndr subst us bndr
       ; return (env { se_subst = subst' }, bndr') }

cloneRecBndrsSM :: SpecEnv -> [Id] -> SpecM (SpecEnv, [Id])
cloneRecBndrsSM env@(SE { se_subst = subst }) bndrs
  = do { (subst', bndrs') <- Core.cloneRecIdBndrs subst bndrs
       ; let env' = env { se_subst = subst' }
       ; return (env', bndrs') }

newDictBndr :: SpecEnv -> CoreBndr -> SpecM (SpecEnv, CoreBndr)
-- Make up completely fresh binders for the dictionaries
-- Their bindings are going to float outwards
newDictBndr env@(SE { se_subst = subst }) b
  = do { uniq <- getUniqueM
       ; let n    = idName b
             ty'  = substTyUnchecked subst (idType b)
             b'   = mkUserLocal (nameOccName n) uniq ManyTy ty' (getSrcSpan n)
             env' = env { se_subst = subst `Core.extendSubstInScope` b' }
       ; pure (env', b') }

newSpecIdSM :: Name -> Type -> IdDetails -> IdInfo -> SpecM Id
    -- Give the new Id a similar occurrence name to the old one
newSpecIdSM old_name new_ty details info
  = do  { uniq <- getUniqueM
        ; let new_occ  = mkSpecOcc (nameOccName old_name)
              new_name = mkInternalName uniq new_occ  (getSrcSpan old_name)
        ; return (assert (not (isCoVarType new_ty)) $
                  mkLocalVar details new_name ManyTy new_ty info) }

{-
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

The benefit is that we have given the specialised "unboxed" values a
very simple lifted semantics and then leave it up to the simplifier to
optimise it --- knowing that the overheads will be removed in nearly
all cases.

In particular, the value will only be evaluated in the branches of the
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
-}
