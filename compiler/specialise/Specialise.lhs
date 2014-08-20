%
% (c) The GRASP/AQUA Project, Glasgow University, 1993-1998
%
\section[Specialise]{Stamping out overloading, and (optionally) polymorphism}

\begin{code}
{-# LANGUAGE CPP #-}

module Specialise ( specProgram ) where

#include "HsVersions.h"

import Id
import TcType hiding( substTy, extendTvSubstList )
import Type   hiding( substTy, extendTvSubstList )
import Coercion( Coercion )
import CoreMonad
import qualified CoreSubst
import CoreUnfold
import VarSet
import VarEnv
import CoreSyn
import Rules
import CoreUtils        ( exprIsTrivial, applyTypeToArgs )
import CoreFVs          ( exprFreeVars, exprsFreeVars, idFreeVars )
import UniqSupply
import Name
import MkId             ( voidArgId, voidPrimId )
import Maybes           ( catMaybes, isJust )
import BasicTypes
import HscTypes
import Bag
import DynFlags
import Util
import Outputable
import FastString
import State

import Control.Applicative (Applicative(..))
import Control.Monad
import Data.Map (Map)
import qualified Data.Map as Map
import qualified FiniteMap as Map
\end{code}

%************************************************************************
%*                                                                      *
\subsection[notes-Specialise]{Implementation notes [SLPJ, Aug 18 1993]}
%*                                                                      *
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


%************************************************************************
%*                                                                      *
\subsubsection{The new specialiser}
%*                                                                      *
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
%*                                                                      *
\subsubsection{The exported function}
%*                                                                      *
%************************************************************************

\begin{code}
specProgram :: ModGuts -> CoreM ModGuts
specProgram guts@(ModGuts { mg_rules = rules, mg_binds = binds })
  = do { hpt_rules <- getRuleBase
       ; dflags <- getDynFlags
       ; let local_rules = mg_rules guts
             rule_base = extendRuleBaseList hpt_rules rules

             -- Specialise the bindings of this module
       ; (binds', uds) <- runSpecM dflags (go binds)

             -- Specialise imported functions
       ; (new_rules, spec_binds) <- specImports dflags emptyVarSet rule_base uds

       ; let final_binds | null spec_binds = binds'
                         | otherwise       = Rec (flattenBinds spec_binds) : binds'
                   -- Note [Glom the bindings if imported functions are specialised]

       ; return (guts { mg_binds = final_binds
                      , mg_rules = new_rules ++ local_rules }) }
  where
        -- We need to start with a Subst that knows all the things
        -- that are in scope, so that the substitution engine doesn't
        -- accidentally re-use a unique that's already in use
        -- Easiest thing is to do it all at once, as if all the top-level
        -- decls were mutually recursive
    top_subst = SE { se_subst = CoreSubst.mkEmptySubst $ mkInScopeSet $ mkVarSet $
                                bindersOfBinds binds
                   , se_interesting = emptyVarSet }

    go []           = return ([], emptyUDs)
    go (bind:binds) = do (binds', uds) <- go binds
                         (bind', uds') <- specBind top_subst bind uds
                         return (bind' ++ binds', uds')

specImports :: DynFlags
            -> VarSet           -- Don't specialise these ones
                                -- See Note [Avoiding recursive specialisation]
            -> RuleBase         -- Rules from this module and the home package
                                -- (but not external packages, which can change)
            -> UsageDetails     -- Calls for imported things, and floating bindings
            -> CoreM ( [CoreRule]   -- New rules
                     , [CoreBind] ) -- Specialised bindings and floating bindings
-- See Note [Specialise imported INLINABLE things]
specImports dflags done rb uds
  = do { let import_calls = varEnvElts (ud_calls uds)
       ; (rules, spec_binds) <- go rb import_calls
       ; return (rules, wrapDictBinds (ud_binds uds) spec_binds) }
  where
    go _ [] = return ([], [])
    go rb (CIS fn calls_for_fn : other_calls)
      = do { (rules1, spec_binds1) <- specImport dflags done rb fn (Map.toList calls_for_fn)
           ; (rules2, spec_binds2) <- go (extendRuleBaseList rb rules1) other_calls
           ; return (rules1 ++ rules2, spec_binds1 ++ spec_binds2) }

specImport :: DynFlags
           -> VarSet                -- Don't specialise these
                                    -- See Note [Avoiding recursive specialisation]
           -> RuleBase              -- Rules from this module
           -> Id -> [CallInfo]      -- Imported function and calls for it
           -> CoreM ( [CoreRule]    -- New rules
                    , [CoreBind] )  -- Specialised bindings
specImport dflags done rb fn calls_for_fn
  | fn `elemVarSet` done
  = return ([], [])     -- No warning.  This actually happens all the time
                        -- when specialising a recursive function, because
                        -- the RHS of the specialised function contains a recursive
                        -- call to the original function

  | isInlinablePragma (idInlinePragma fn)
  , Just rhs <- maybeUnfoldingTemplate (realIdUnfolding fn)
  = do {     -- Get rules from the external package state
             -- We keep doing this in case we "page-fault in"
             -- more rules as we go along
       ; hsc_env <- getHscEnv
       ; eps <- liftIO $ hscEPS hsc_env
       ; let full_rb = unionRuleBase rb (eps_rule_base eps)
             rules_for_fn = getRules full_rb fn

       ; (rules1, spec_pairs, uds) <- runSpecM dflags $
              specCalls emptySpecEnv rules_for_fn calls_for_fn fn rhs
       ; let spec_binds1 = [NonRec b r | (b,r) <- spec_pairs]
             -- After the rules kick in we may get recursion, but
             -- we rely on a global GlomBinds to sort that out later
             -- See Note [Glom the bindings if imported functions are specialised]

              -- Now specialise any cascaded calls
       ; (rules2, spec_binds2) <- specImports dflags (extendVarSet done fn)
                                                     (extendRuleBaseList rb rules1)
                                                     uds

       ; return (rules2 ++ rules1, spec_binds2 ++ spec_binds1) }

  | otherwise
  = WARN( True, ptext (sLit "specImport discard") <+> ppr fn <+> ppr calls_for_fn )
    return ([], [])
\end{code}

Note [Specialise imported INLINABLE things]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We specialise INLINABLE things but not INLINE things.  The latter
should be inlined bodily, so not much point in specialising them.
Moreover, we risk lots of orphan modules from vigorous specialisation.

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


Note [Avoiding recursive specialisation]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When we specialise 'f' we may find new overloaded calls to 'g', 'h' in
'f's RHS.  So we want to specialise g,h.  But we don't want to
specialise f any more!  It's possible that f's RHS might have a
recursive yet-more-specialised call, so we'd diverge in that case.
And if the call is to the same type, one specialisation is enough.
Avoiding this recursive specialisation loop is the reason for the
'done' VarSet passed to specImports and specImport.

%************************************************************************
%*                                                                      *
\subsubsection{@specExpr@: the main function}
%*                                                                      *
%************************************************************************

\begin{code}
data SpecEnv 
  = SE { se_subst :: CoreSubst.Subst
             -- We carry a substitution down:
             -- a) we must clone any binding that might float outwards,
             --    to avoid name clashes
             -- b) we carry a type substitution to use when analysing
             --    the RHS of specialised bindings (no type-let!)


       , se_interesting :: VarSet 
             -- Dict Ids that we know something about
             -- and hence may be worth specialising against
             -- See Note [Interesting dictionary arguments]
     }

emptySpecEnv :: SpecEnv
emptySpecEnv = SE { se_subst = CoreSubst.emptySubst, se_interesting = emptyVarSet} 

specVar :: SpecEnv -> Id -> CoreExpr
specVar env v = CoreSubst.lookupIdSubst (text "specVar") (se_subst env) v

specExpr :: SpecEnv -> CoreExpr -> SpecM (CoreExpr, UsageDetails)

---------------- First the easy cases --------------------
specExpr env (Type ty)     = return (Type     (substTy env ty), emptyUDs)
specExpr env (Coercion co) = return (Coercion (substCo env co), emptyUDs)
specExpr env (Var v)       = return (specVar env v, emptyUDs)
specExpr _   (Lit lit)     = return (Lit lit,       emptyUDs)
specExpr env (Cast e co)
  = do { (e', uds) <- specExpr env e
       ; return ((Cast e' (substCo env co)), uds) }
specExpr env (Tick tickish body)
  = do { (body', uds) <- specExpr env body
       ; return (Tick (specTickish env tickish) body', uds) }

---------------- Applications might generate a call instance --------------------
specExpr env expr@(App {})
  = go expr []
  where
    go (App fun arg) args = do (arg', uds_arg) <- specExpr env arg
                               (fun', uds_app) <- go fun (arg':args)
                               return (App fun' arg', uds_arg `plusUDs` uds_app)

    go (Var f)       args = case specVar env f of
                                Var f' -> return (Var f', mkCallUDs env f' args)
                                e'     -> return (e', emptyUDs) -- I don't expect this!
    go other         _    = specExpr env other

---------------- Lambda/case require dumping of usage details --------------------
specExpr env e@(Lam _ _) = do
    (body', uds) <- specExpr env' body
    let (free_uds, dumped_dbs) = dumpUDs bndrs' uds
    return (mkLams bndrs' (wrapDictBindsE dumped_dbs body'), free_uds)
  where
    (bndrs, body) = collectBinders e
    (env', bndrs') = substBndrs env bndrs
        -- More efficient to collect a group of binders together all at once
        -- and we don't want to split a lambda group with dumped bindings

specExpr env (Case scrut case_bndr ty alts)
  = do { (scrut', scrut_uds) <- specExpr env scrut
       ; (scrut'', case_bndr', alts', alts_uds)
             <- specCase env scrut' case_bndr alts
       ; return (Case scrut'' case_bndr' (substTy env ty) alts'
                , scrut_uds `plusUDs` alts_uds) }

---------------- Finally, let is the interesting case --------------------
specExpr env (Let bind body)
  = do { -- Clone binders
         (rhs_env, body_env, bind') <- cloneBindSM env bind

         -- Deal with the body
       ; (body', body_uds) <- specExpr body_env body

        -- Deal with the bindings
      ; (binds', uds) <- specBind rhs_env bind' body_uds

        -- All done
      ; return (foldr Let body' binds', uds) }

specTickish :: SpecEnv -> Tickish Id -> Tickish Id
specTickish env (Breakpoint ix ids)
  = Breakpoint ix [ id' | id <- ids, Var id' <- [specVar env id]]
  -- drop vars from the list if they have a non-variable substitution.
  -- should never happen, but it's harmless to drop them anyway.
specTickish _ other_tickish = other_tickish

specCase :: SpecEnv
         -> CoreExpr            -- Scrutinee, already done
         -> Id -> [CoreAlt]
         -> SpecM ( CoreExpr    -- New scrutinee
                  , Id
                  , [CoreAlt]
                  , UsageDetails)
specCase env scrut' case_bndr [(con, args, rhs)]
  | isDictId case_bndr           -- See Note [Floating dictionaries out of cases]
  , interestingDict env scrut'
  , not (isDeadBinder case_bndr && null sc_args')
  = do { (case_bndr_flt : sc_args_flt) <- mapM clone_me (case_bndr' : sc_args')

       ; let sc_rhss = [ Case (Var case_bndr_flt) case_bndr' (idType sc_arg')
                              [(con, args', Var sc_arg')]
                       | sc_arg' <- sc_args' ]

             -- Extend the substitution for RHS to map the *original* binders
             -- to their floated verions.  
             mb_sc_flts :: [Maybe DictId]
             mb_sc_flts = map (lookupVarEnv clone_env) args'
             clone_env  = zipVarEnv sc_args' sc_args_flt
             subst_prs  = (case_bndr, Var case_bndr_flt)
                        : [ (arg, Var sc_flt)
                          | (arg, Just sc_flt) <- args `zip` mb_sc_flts ]
             env_rhs' = env_rhs { se_subst = CoreSubst.extendIdSubstList (se_subst env_rhs) subst_prs
                                , se_interesting = se_interesting env_rhs `extendVarSetList`
                                                   (case_bndr_flt : sc_args_flt) }

       ; (rhs', rhs_uds)   <- specExpr env_rhs' rhs
       ; let scrut_bind    = mkDB (NonRec case_bndr_flt scrut')
             case_bndr_set = unitVarSet case_bndr_flt
             sc_binds      = [(NonRec sc_arg_flt sc_rhs, case_bndr_set)
                             | (sc_arg_flt, sc_rhs) <- sc_args_flt `zip` sc_rhss ]
             flt_binds     = scrut_bind : sc_binds
             (free_uds, dumped_dbs) = dumpUDs (case_bndr':args') rhs_uds
             all_uds = flt_binds `addDictBinds` free_uds
             alt'    = (con, args', wrapDictBindsE dumped_dbs rhs')
       ; return (Var case_bndr_flt, case_bndr', [alt'], all_uds) }
  where
    (env_rhs, (case_bndr':args')) = substBndrs env (case_bndr:args)
    sc_args' = filter is_flt_sc_arg args'

    clone_me bndr = do { uniq <- getUniqueM
                       ; return (mkUserLocal occ uniq ty loc) }
       where
         name = idName bndr
         ty   = idType bndr
         occ  = nameOccName name
         loc  = getSrcSpan name

    arg_set = mkVarSet args'
    is_flt_sc_arg var =  isId var
                      && not (isDeadBinder var)
                      && isDictTy var_ty
                      && not (tyVarsOfType var_ty `intersectsVarSet` arg_set)
       where
         var_ty = idType var


specCase env scrut case_bndr alts
  = do { (alts', uds_alts) <- mapAndCombineSM spec_alt alts
       ; return (scrut, case_bndr', alts', uds_alts) }
  where
    (env_alt, case_bndr') = substBndr env case_bndr
    spec_alt (con, args, rhs) = do
          (rhs', uds) <- specExpr env_rhs rhs
          let (free_uds, dumped_dbs) = dumpUDs (case_bndr' : args') uds
          return ((con, args', wrapDictBindsE dumped_dbs rhs'), free_uds)
        where
          (env_rhs, args') = substBndrs env_alt args
\end{code}

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

%************************************************************************
%*                                                                      *
                     Dealing with a binding
%*                                                                      *
%************************************************************************

\begin{code}
specBind :: SpecEnv                     -- Use this for RHSs
         -> CoreBind
         -> UsageDetails                -- Info on how the scope of the binding
         -> SpecM ([CoreBind],          -- New bindings
                   UsageDetails)        -- And info to pass upstream

-- Returned UsageDetails:
--    No calls for binders of this bind
specBind rhs_env (NonRec fn rhs) body_uds
  = do { (rhs', rhs_uds) <- specExpr rhs_env rhs
       ; (fn', spec_defns, body_uds1) <- specDefn rhs_env body_uds fn rhs

       ; let pairs = spec_defns ++ [(fn', rhs')]
                        -- fn' mentions the spec_defns in its rules,
                        -- so put the latter first

             combined_uds = body_uds1 `plusUDs` rhs_uds
                -- This way round a call in rhs_uds of a function f
                -- at type T will override a call of f at T in body_uds1; and
                -- that is good because it'll tend to keep "earlier" calls
                -- See Note [Specialisation of dictionary functions]

             (free_uds, dump_dbs, float_all) = dumpBindUDs [fn] combined_uds
                -- See Note [From non-recursive to recursive]

             final_binds | isEmptyBag dump_dbs = [NonRec b r | (b,r) <- pairs]
                         | otherwise = [Rec (flattenDictBinds dump_dbs pairs)]

         ; if float_all then
             -- Rather than discard the calls mentioning the bound variables
             -- we float this binding along with the others
              return ([], free_uds `snocDictBinds` final_binds)
           else
             -- No call in final_uds mentions bound variables,
             -- so we can just leave the binding here
              return (final_binds, free_uds) }


specBind rhs_env (Rec pairs) body_uds
       -- Note [Specialising a recursive group]
  = do { let (bndrs,rhss) = unzip pairs
       ; (rhss', rhs_uds) <- mapAndCombineSM (specExpr rhs_env) rhss
       ; let scope_uds = body_uds `plusUDs` rhs_uds
                       -- Includes binds and calls arising from rhss

       ; (bndrs1, spec_defns1, uds1) <- specDefns rhs_env scope_uds pairs

       ; (bndrs3, spec_defns3, uds3)
             <- if null spec_defns1  -- Common case: no specialisation
                then return (bndrs1, [], uds1)
                else do {            -- Specialisation occurred; do it again
                          (bndrs2, spec_defns2, uds2)
                              <- specDefns rhs_env uds1 (bndrs1 `zip` rhss)
                        ; return (bndrs2, spec_defns2 ++ spec_defns1, uds2) }

       ; let (final_uds, dumped_dbs, float_all) = dumpBindUDs bndrs uds3
             bind = Rec (flattenDictBinds dumped_dbs $
                         spec_defns3 ++ zip bndrs3 rhss')

       ; if float_all then
              return ([], final_uds `snocDictBind` bind)
           else
              return ([bind], final_uds) }


---------------------------
specDefns :: SpecEnv
          -> UsageDetails               -- Info on how it is used in its scope
          -> [(Id,CoreExpr)]            -- The things being bound and their un-processed RHS
          -> SpecM ([Id],               -- Original Ids with RULES added
                    [(Id,CoreExpr)],    -- Extra, specialised bindings
                    UsageDetails)       -- Stuff to fling upwards from the specialised versions

-- Specialise a list of bindings (the contents of a Rec), but flowing usages
-- upwards binding by binding.  Example: { f = ...g ...; g = ...f .... }
-- Then if the input CallDetails has a specialised call for 'g', whose specialisation
-- in turn generates a specialised call for 'f', we catch that in this one sweep.
-- But not vice versa (it's a fixpoint problem).

specDefns _env uds []
  = return ([], [], uds)
specDefns env uds ((bndr,rhs):pairs)
  = do { (bndrs1, spec_defns1, uds1) <- specDefns env uds pairs
       ; (bndr1, spec_defns2, uds2)  <- specDefn env uds1 bndr rhs
       ; return (bndr1 : bndrs1, spec_defns1 ++ spec_defns2, uds2) }

---------------------------
specDefn :: SpecEnv
         -> UsageDetails                -- Info on how it is used in its scope
         -> Id -> CoreExpr              -- The thing being bound and its un-processed RHS
         -> SpecM (Id,                  -- Original Id with added RULES
                   [(Id,CoreExpr)],     -- Extra, specialised bindings
                   UsageDetails)        -- Stuff to fling upwards from the specialised versions

specDefn env body_uds fn rhs
  = do { let (body_uds_without_me, calls_for_me) = callsForMe fn body_uds
             rules_for_me = idCoreRules fn
       ; (rules, spec_defns, spec_uds) <- specCalls env rules_for_me
                                                    calls_for_me fn rhs
       ; return ( fn `addIdSpecialisations` rules
                , spec_defns
                , body_uds_without_me `plusUDs` spec_uds) }
                -- It's important that the `plusUDs` is this way
                -- round, because body_uds_without_me may bind
                -- dictionaries that are used in calls_for_me passed
                -- to specDefn.  So the dictionary bindings in
                -- spec_uds may mention dictionaries bound in
                -- body_uds_without_me

---------------------------
specCalls :: SpecEnv
          -> [CoreRule]                 -- Existing RULES for the fn
          -> [CallInfo]
          -> Id -> CoreExpr
          -> SpecM ([CoreRule],         -- New RULES for the fn
                    [(Id,CoreExpr)],    -- Extra, specialised bindings
                    UsageDetails)       -- New usage details from the specialised RHSs

-- This function checks existing rules, and does not create
-- duplicate ones. So the caller does not need to do this filtering.
-- See 'already_covered'

specCalls env rules_for_me calls_for_me fn rhs
        -- The first case is the interesting one
  |  rhs_tyvars `lengthIs`     n_tyvars -- Rhs of fn's defn has right number of big lambdas
  && rhs_ids    `lengthAtLeast` n_dicts -- and enough dict args
  && notNull calls_for_me               -- And there are some calls to specialise
  && not (isNeverActive (idInlineActivation fn))
        -- Don't specialise NOINLINE things
        -- See Note [Auto-specialisation and RULES]

--   && not (certainlyWillInline (idUnfolding fn))      -- And it's not small
--      See Note [Inline specialisation] for why we do not
--      switch off specialisation for inline functions

  = -- pprTrace "specDefn: some" (ppr fn $$ ppr calls_for_me $$ ppr rules_for_me) $
    do { stuff <- mapM spec_call calls_for_me
       ; let (spec_defns, spec_uds, spec_rules) = unzip3 (catMaybes stuff)
       ; return (spec_rules, spec_defns, plusUDList spec_uds) }

  | otherwise   -- No calls or RHS doesn't fit our preconceptions
  = WARN( not (exprIsTrivial rhs) && notNull calls_for_me, 
          ptext (sLit "Missed specialisation opportunity for")
                                 <+> ppr fn $$ _trace_doc )
          -- Note [Specialisation shape]
    -- pprTrace "specDefn: none" (ppr fn <+> ppr calls_for_me) $
    return ([], [], emptyUDs)
  where
    _trace_doc = sep [ ppr rhs_tyvars, ppr n_tyvars
                     , ppr rhs_ids, ppr n_dicts
                     , ppr (idInlineActivation fn) ]

    fn_type            = idType fn
    fn_arity           = idArity fn
    fn_unf             = realIdUnfolding fn     -- Ignore loop-breaker-ness here
    (tyvars, theta, _) = tcSplitSigmaTy fn_type
    n_tyvars           = length tyvars
    n_dicts            = length theta
    inl_prag           = idInlinePragma fn
    inl_act            = inlinePragmaActivation inl_prag
    is_local           = isLocalId fn

        -- Figure out whether the function has an INLINE pragma
        -- See Note [Inline specialisations]

    spec_arity = unfoldingArity fn_unf - n_dicts  -- Arity of the *specialised* inline rule

    (rhs_tyvars, rhs_ids, rhs_body) = collectTyAndValBinders rhs

    rhs_dict_ids = take n_dicts rhs_ids
    body         = mkLams (drop n_dicts rhs_ids) rhs_body
                -- Glue back on the non-dict lambdas

    already_covered :: DynFlags -> [CoreExpr] -> Bool
    already_covered dflags args      -- Note [Specialisations already covered]
       = isJust (lookupRule dflags 
                            (CoreSubst.substInScope (se_subst env), realIdUnfolding)
                            (const True) 
                            fn args rules_for_me)

    mk_ty_args :: [Maybe Type] -> [TyVar] -> [CoreExpr]
    mk_ty_args [] poly_tvs
      = ASSERT( null poly_tvs ) []
    mk_ty_args (Nothing : call_ts) (poly_tv : poly_tvs)
      = Type (mkTyVarTy poly_tv) : mk_ty_args call_ts poly_tvs
    mk_ty_args (Just ty : call_ts) poly_tvs
      = Type ty : mk_ty_args call_ts poly_tvs
    mk_ty_args (Nothing : _) [] = panic "mk_ty_args"

    ----------------------------------------------------------
        -- Specialise to one particular call pattern
    spec_call :: CallInfo                         -- Call instance
              -> SpecM (Maybe ((Id,CoreExpr),     -- Specialised definition
                               UsageDetails,      -- Usage details from specialised body
                               CoreRule))         -- Info for the Id's SpecEnv
    spec_call (CallKey call_ts, (call_ds, _))
      = ASSERT( call_ts `lengthIs` n_tyvars  && call_ds `lengthIs` n_dicts )

        -- Suppose f's defn is  f = /\ a b c -> \ d1 d2 -> rhs
        -- Supppose the call is for f [Just t1, Nothing, Just t3] [dx1, dx2]

        -- Construct the new binding
        --      f1 = SUBST[a->t1,c->t3, d1->d1', d2->d2'] (/\ b -> rhs)
        -- PLUS the usage-details
        --      { d1' = dx1; d2' = dx2 }
        -- where d1', d2' are cloned versions of d1,d2, with the type substitution
        -- applied.  These auxiliary bindings just avoid duplication of dx1, dx2
        --
        -- Note that the substitution is applied to the whole thing.
        -- This is convenient, but just slightly fragile.  Notably:
        --      * There had better be no name clashes in a/b/c
        do { let
                -- poly_tyvars = [b] in the example above
                -- spec_tyvars = [a,c]
                -- ty_args     = [t1,b,t3]
                spec_tv_binds = [(tv,ty) | (tv, Just ty) <- rhs_tyvars `zip` call_ts]
                spec_ty_args  = map snd spec_tv_binds
                env1          = extendTvSubstList env spec_tv_binds
                (rhs_env, poly_tyvars) = substBndrs env1
                                            [tv | (tv, Nothing) <- rhs_tyvars `zip` call_ts]

           ; (rhs_env2, inst_dict_ids, dx_binds) 
                  <- bindAuxiliaryDicts rhs_env (zipEqual "bindAux" rhs_dict_ids call_ds)
           ; let ty_args   = mk_ty_args call_ts poly_tyvars
                 inst_args = ty_args ++ map Var inst_dict_ids

           ; dflags <- getDynFlags
           ; if already_covered dflags inst_args then
                return Nothing
             else do
           {    -- Figure out the type of the specialised function
             let body_ty = applyTypeToArgs rhs fn_type inst_args
                 (lam_args, app_args)           -- Add a dummy argument if body_ty is unlifted
                   | isUnLiftedType body_ty     -- C.f. WwLib.mkWorkerArgs
                   = (poly_tyvars ++ [voidArgId], poly_tyvars ++ [voidPrimId])
                   | otherwise = (poly_tyvars, poly_tyvars)
                 spec_id_ty = mkPiTypes lam_args body_ty

           ; spec_f <- newSpecIdSM fn spec_id_ty
           ; (spec_rhs, rhs_uds) <- specExpr rhs_env2 (mkLams lam_args body)
           ; let
                -- The rule to put in the function's specialisation is:
                --      forall b, d1',d2'.  f t1 b t3 d1' d2' = f1 b
                rule_name = mkFastString ("SPEC " ++ showSDocDump dflags (ppr fn <+> ppr spec_ty_args))
                spec_env_rule = mkRule True {- Auto generated -} is_local
                                  rule_name
                                  inl_act       -- Note [Auto-specialisation and RULES]
                                  (idName fn)
                                  (poly_tyvars ++ inst_dict_ids)
                                  inst_args
                                  (mkVarApps (Var spec_f) app_args)

                -- Add the { d1' = dx1; d2' = dx2 } usage stuff
                final_uds = foldr consDictBind rhs_uds dx_binds

                --------------------------------------
                -- Add a suitable unfolding if the spec_inl_prag says so
                -- See Note [Inline specialisations]
                spec_inl_prag
                  | not is_local && isStrongLoopBreaker (idOccInfo fn)
                  = neverInlinePragma   -- See Note [Specialising imported functions] in OccurAnal
                  | otherwise
                  = case inl_prag of
                       InlinePragma { inl_inline = Inlinable }
                          -> inl_prag { inl_inline = EmptyInlineSpec }
                       _  -> inl_prag

                spec_unf
                  = case inlinePragmaSpec spec_inl_prag of
                      Inline    -> mkInlineUnfolding (Just spec_arity) spec_rhs
                      Inlinable -> mkInlinableUnfolding dflags spec_rhs
                      _         -> NoUnfolding

                --------------------------------------
                -- Adding arity information just propagates it a bit faster
                --      See Note [Arity decrease] in Simplify
                -- Copy InlinePragma information from the parent Id.
                -- So if f has INLINE[1] so does spec_f
                spec_f_w_arity = spec_f `setIdArity`      max 0 (fn_arity - n_dicts)
                                        `setInlinePragma` spec_inl_prag
                                        `setIdUnfolding`  spec_unf

           ; return (Just ((spec_f_w_arity, spec_rhs), final_uds, spec_env_rule)) } }

bindAuxiliaryDicts
        :: SpecEnv
        -> [(DictId,CoreExpr)]   -- (orig_dict, dx)
        -> SpecM (SpecEnv,             -- Substitute for all orig_dicts
                  [DictId],            -- Cloned dict Ids
                  [CoreBind])          -- Auxiliary bindings
-- Bind any dictionary arguments to fresh names, to preserve sharing
bindAuxiliaryDicts env@(SE { se_subst = subst, se_interesting = interesting }) 
                   dict_binds 
  = do { inst_dict_ids <- mapM (newDictBndr env . fst) dict_binds
                          -- Clone rhs_dicts, including instantiating their types
       ; let triples           = inst_dict_ids `zip` dict_binds
             (subst', binds)   = go subst [] triples
             interesting_dicts = mkVarSet [ dx_id | (dx_id, (_, dx)) <- triples
                                          , interestingDict env dx ]
                  -- See Note [Make the new dictionaries interesting]
             env' = env { se_subst = subst'
                        , se_interesting = interesting `unionVarSet` interesting_dicts }

       ; return (env', inst_dict_ids, binds) }
  where
    go subst binds []    = (subst, binds)
    go subst binds ((dx_id, (d, dx)) : triples)
      | exprIsTrivial dx = go (CoreSubst.extendIdSubst subst d dx) binds triples
      | otherwise        = go (CoreSubst.extendIdSubst subst d (Var dx_id))
                              (NonRec dx_id dx : binds) triples
             -- In the first case extend the substitution but not bindings;
             -- in the latter extend the bindings but not the substitution.
             -- For the former, note that we bind the *original* dict in the substitution,
             -- overriding any d->dx_id binding put there by substBndrs
\end{code}

Note [Make the new dictionaries interesting]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Important!  We're going to substitute dx_id1 for d
and we want it to look "interesting", else we won't gather *any*
consequential calls. E.g.
    f d = ...g d....
If we specialise f for a call (f (dfun dNumInt)), we'll get
a consequent call (g d') with an auxiliary definition
    d' = df dNumInt
We want that consequent call to look interesting


Note [From non-recursive to recursive]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Even in the non-recursive case, if any dict-binds depend on 'fn' we might
have built a recursive knot

      f a d x = <blah>
      MkUD { ud_binds = d7 = MkD ..f..
           , ud_calls = ...(f T d7)... }

The we generate

      Rec { fs x = <blah>[T/a, d7/d]
            f a d x = <blah>
               RULE f T _ = fs
            d7 = ...f... }

Here the recursion is only through the RULE.


Note [Specialisation of dictionary functions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Here is a nasty example that bit us badly: see Trac #3591

     class Eq a => C a
     instance Eq [a] => C [a]

---------------
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

But look at this:

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


Conclusion: we catch the nasty case using filter_dfuns in
callsForMe. To be honest I'm not 100% certain that this is 100%
right, but it works.  Sigh.


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
g::Int->Int That version won't appear in the LHS of the RULE for f.
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

This doesn't always work.  One example I came across was this:
        newtype Gen a = MkGen{ unGen :: Int -> a }

        choose :: Eq a => a -> Gen a
        choose n = MkGen (\r -> n)

        oneof = choose (1::Int)

It's a silly exapmle, but we get
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
  * Activation/RuleMatchInfo: both transferred to the
                              specialised function
  * InlineSpec:
       (a) An INLINE pragma is transferred
       (b) An INLINABLE pragma is *not* transferred

Why (a)? Previously the idea is that the point of INLINE was
precisely to specialise the function at its call site, and that's not
so important for the specialised copies.  But *pragma-directed*
specialisation now takes place in the typechecker/desugarer, with
manually specified INLINEs.  The specialiation here is automatic.
It'd be very odd if a function marked INLINE was specialised (because
of some local use), and then forever after (including importing
modules) the specialised version wasn't INLINEd.  After all, the
programmer said INLINE!

You might wonder why we don't just not-specialise INLINE functions.
It's because even INLINE functions are sometimes not inlined, when
they aren't applied to interesting arguments.  But perhaps the type
arguments alone are enough to specialise (even though the args are too
boring to trigger inlining), and it's certainly better to call the
specialised version.

Why (b)? See Trac #4874 for persuasive examples.  Suppose we have
    {-# INLINABLE f #-}
    f :: Ord a => [a] -> Int
    f xs = letrec f' = ...f'... in f'
Then, when f is specialised and optimised we might get
    wgo :: [Int] -> Int#
    wgo = ...wgo...
    f_spec :: [Int] -> Int
    f_spec xs = case wgo xs of { r -> I# r }
and we clearly want to inline f_spec at call sites.  But if we still
have the big, un-optimised of f (albeit specialised) captured in an
INLINABLE pragma for f_spec, we won't get that optimisation.

So we simply drop INLINABLE pragmas when specialising. It's not really
a complete solution; ignoring specalisation for now, INLINABLE functions
don't get properly strictness analysed, for example. But it works well
for examples involving specialisation, which is the dominant use of
INLINABLE.  See Trac #4874.


%************************************************************************
%*                                                                      *
\subsubsection{UsageDetails and suchlike}
%*                                                                      *
%************************************************************************

\begin{code}
data UsageDetails
  = MkUD {
        ud_binds :: !(Bag DictBind),
                        -- Floated dictionary bindings
                        -- The order is important;
                        -- in ds1 `union` ds2, bindings in ds2 can depend on those in ds1
                        -- (Remember, Bags preserve order in GHC.)

        ud_calls :: !CallDetails

        -- INVARIANT: suppose bs = bindersOf ud_binds
        -- Then 'calls' may *mention* 'bs',
        -- but there should be no calls *for* bs
    }

instance Outputable UsageDetails where
  ppr (MkUD { ud_binds = dbs, ud_calls = calls })
        = ptext (sLit "MkUD") <+> braces (sep (punctuate comma
                [ptext (sLit "binds") <+> equals <+> ppr dbs,
                 ptext (sLit "calls") <+> equals <+> ppr calls]))

type DictBind = (CoreBind, VarSet)
        -- The set is the free vars of the binding
        -- both tyvars and dicts

type DictExpr = CoreExpr

emptyUDs :: UsageDetails
emptyUDs = MkUD { ud_binds = emptyBag, ud_calls = emptyVarEnv }

------------------------------------------------------------
type CallDetails  = IdEnv CallInfoSet
newtype CallKey   = CallKey [Maybe Type]                        -- Nothing => unconstrained type argument

-- CallInfo uses a Map, thereby ensuring that
-- we record only one call instance for any key
--
-- The list of types and dictionaries is guaranteed to
-- match the type of f
data CallInfoSet = CIS Id (Map CallKey ([DictExpr], VarSet))
                        -- Range is dict args and the vars of the whole
                        -- call (including tyvars)
                        -- [*not* include the main id itself, of course]

type CallInfo = (CallKey, ([DictExpr], VarSet))

instance Outputable CallInfoSet where
  ppr (CIS fn map) = hang (ptext (sLit "CIS") <+> ppr fn)
                        2 (ppr map)

instance Outputable CallKey where
  ppr (CallKey ts) = ppr ts

-- Type isn't an instance of Ord, so that we can control which
-- instance we use.  That's tiresome here.  Oh well
instance Eq CallKey where
  k1 == k2 = case k1 `compare` k2 of { EQ -> True; _ -> False }

instance Ord CallKey where
  compare (CallKey k1) (CallKey k2) = cmpList cmp k1 k2
                where
                  cmp Nothing   Nothing   = EQ
                  cmp Nothing   (Just _)  = LT
                  cmp (Just _)  Nothing   = GT
                  cmp (Just t1) (Just t2) = cmpType t1 t2

unionCalls :: CallDetails -> CallDetails -> CallDetails
unionCalls c1 c2 = plusVarEnv_C unionCallInfoSet c1 c2

unionCallInfoSet :: CallInfoSet -> CallInfoSet -> CallInfoSet
unionCallInfoSet (CIS f calls1) (CIS _ calls2) = CIS f (calls1 `Map.union` calls2)

callDetailsFVs :: CallDetails -> VarSet
callDetailsFVs calls = foldVarEnv (unionVarSet . callInfoFVs) emptyVarSet calls

callInfoFVs :: CallInfoSet -> VarSet
callInfoFVs (CIS _ call_info) = Map.foldRight (\(_,fv) vs -> unionVarSet fv vs) emptyVarSet call_info

------------------------------------------------------------
singleCall :: Id -> [Maybe Type] -> [DictExpr] -> UsageDetails
singleCall id tys dicts
  = MkUD {ud_binds = emptyBag,
          ud_calls = unitVarEnv id $ CIS id $
                     Map.singleton (CallKey tys) (dicts, call_fvs) }
  where
    call_fvs = exprsFreeVars dicts `unionVarSet` tys_fvs
    tys_fvs  = tyVarsOfTypes (catMaybes tys)
        -- The type args (tys) are guaranteed to be part of the dictionary
        -- types, because they are just the constrained types,
        -- and the dictionary is therefore sure to be bound
        -- inside the binding for any type variables free in the type;
        -- hence it's safe to neglect tyvars free in tys when making
        -- the free-var set for this call
        -- BUT I don't trust this reasoning; play safe and include tys_fvs
        --
        -- We don't include the 'id' itself.

mkCallUDs :: SpecEnv -> Id -> [CoreExpr] -> UsageDetails
mkCallUDs env f args
  | not (want_calls_for f)  -- Imported from elsewhere
  || null theta             -- Not overloaded
  = emptyUDs

  |  not (all type_determines_value theta)
  || not (spec_tys `lengthIs` n_tyvars)
  || not ( dicts   `lengthIs` n_dicts)
  || not (any (interestingDict env) dicts)    -- Note [Interesting dictionary arguments]
  -- See also Note [Specialisations already covered]
  = -- pprTrace "mkCallUDs: discarding" _trace_doc
    emptyUDs    -- Not overloaded, or no specialisation wanted

  | otherwise
  = -- pprTrace "mkCallUDs: keeping" _trace_doc
    singleCall f spec_tys dicts
  where
    _trace_doc = vcat [ppr f, ppr args, ppr n_tyvars, ppr n_dicts
                      , ppr (map (interestingDict env) dicts)]
    (tyvars, theta, _) = tcSplitSigmaTy (idType f)
    constrained_tyvars = closeOverKinds (tyVarsOfTypes theta)
    n_tyvars           = length tyvars
    n_dicts            = length theta
   
    spec_tys = [mk_spec_ty tv ty | (tv, Type ty) <- tyvars `zip` args]
    dicts    = [dict_expr | (_, dict_expr) <- theta `zip` (drop n_tyvars args)]

    mk_spec_ty tyvar ty
        | tyvar `elemVarSet` constrained_tyvars = Just ty
        | otherwise                             = Nothing

    want_calls_for f = isLocalId f || isInlinablePragma (idInlinePragma f)

    type_determines_value pred    -- See Note [Type determines value]
        = case classifyPredType pred of
            ClassPred cls _ -> not (isIPClass cls)
            TuplePred ps    -> all type_determines_value ps
            EqPred {}       -> True
            IrredPred {}    -> True   -- Things like (D []) where D is a
                                      -- Constraint-ranged family; Trac #7785
\end{code}

Note [Type determines value]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Only specialise if all overloading is on non-IP *class* params,
because these are the ones whose *type* determines their *value*.  In
parrticular, with implicit params, the type args *don't* say what the
value of the implicit param is!  See Trac #7101

However, consider
         type family D (v::*->*) :: Constraint
         type instance D [] = ()
         f :: D v => v Char -> Int
If we see a call (f "foo"), we'll pass a "dictionary"
  () |> (g :: () ~ D [])
and it's good to specialise f at this dictionary.

So the question is: can an implicit parameter "hide inside" a
type-family constraint like (D a).  Well, no.  We don't allow
        type instance D Maybe = ?x:Int
Hence the IrredPred case in type_determines_value.
See Trac #7785.

Note [Interesting dictionary arguments]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider this
         \a.\d:Eq a.  let f = ... in ...(f d)...
There really is not much point in specialising f wrt the dictionary d,
because the code for the specialised f is not improved at all, because
d is lambda-bound.  We simply get junk specialisations.

What is "interesting"?  Just that it has *some* structure.  But what about
variables?  

 * A variable might be imported, in which case its unfolding
   will tell us whether it has useful structure

 * Local variables are cloned on the way down (to avoid clashes when
   we float dictionaries), and cloning drops the unfolding
   (cloneIdBndr).  Moreover, we make up some new bindings, and it's a
   nuisance to give them unfoldings.  So we keep track of the
   "interesting" dictionaries as a VarSet in SpecEnv.
   We have to take care to put any new interesting dictionary 
   bindings in the set.

We accidentally lost accurate tracking of local variables for a long
time, because cloned variables don't have unfoldings. But makes a 
massive difference in a few cases, eg Trac #5113. For nofib as a 
whole it's only a small win: 2.2% improvement in allocation for ansi,
1.2% for bspt, but mostly 0.0!  Average 0.1% increase in binary size.


\begin{code}
interestingDict :: SpecEnv -> CoreExpr -> Bool
-- A dictionary argument is interesting if it has *some* structure
interestingDict env (Var v) =  hasSomeUnfolding (idUnfolding v)
                            || isDataConWorkId v
                            || v `elemVarSet` se_interesting env
interestingDict _ (Type _)                = False
interestingDict _ (Coercion _)            = False
interestingDict env (App fn (Type _))     = interestingDict env fn
interestingDict env (App fn (Coercion _)) = interestingDict env fn
interestingDict env (Tick _ a)            = interestingDict env a
interestingDict env (Cast e _)            = interestingDict env e
interestingDict _ _                       = True
\end{code}

\begin{code}
plusUDs :: UsageDetails -> UsageDetails -> UsageDetails
plusUDs (MkUD {ud_binds = db1, ud_calls = calls1})
        (MkUD {ud_binds = db2, ud_calls = calls2})
  = MkUD { ud_binds = db1    `unionBags`   db2
         , ud_calls = calls1 `unionCalls`  calls2 }

plusUDList :: [UsageDetails] -> UsageDetails
plusUDList = foldr plusUDs emptyUDs

-----------------------------
_dictBindBndrs :: Bag DictBind -> [Id]
_dictBindBndrs dbs = foldrBag ((++) . bindersOf . fst) [] dbs

mkDB :: CoreBind -> DictBind
mkDB bind = (bind, bind_fvs bind)

bind_fvs :: CoreBind -> VarSet
bind_fvs (NonRec bndr rhs) = pair_fvs (bndr,rhs)
bind_fvs (Rec prs)         = foldl delVarSet rhs_fvs bndrs
                           where
                             bndrs = map fst prs
                             rhs_fvs = unionVarSets (map pair_fvs prs)

pair_fvs :: (Id, CoreExpr) -> VarSet
pair_fvs (bndr, rhs) = exprFreeVars rhs `unionVarSet` idFreeVars bndr
        -- Don't forget variables mentioned in the
        -- rules of the bndr.  C.f. OccAnal.addRuleUsage
        -- Also tyvars mentioned in its type; they may not appear in the RHS
        --      type T a = Int
        --      x :: T a = 3

flattenDictBinds :: Bag DictBind -> [(Id,CoreExpr)] -> [(Id,CoreExpr)]
flattenDictBinds dbs pairs
  = foldrBag add pairs dbs
  where
    add (NonRec b r,_) pairs = (b,r) : pairs
    add (Rec prs1, _)  pairs = prs1 ++ pairs

snocDictBinds :: UsageDetails -> [CoreBind] -> UsageDetails
-- Add ud_binds to the tail end of the bindings in uds
snocDictBinds uds dbs
  = uds { ud_binds = ud_binds uds `unionBags`
                     foldr (consBag . mkDB) emptyBag dbs }

consDictBind :: CoreBind -> UsageDetails -> UsageDetails
consDictBind bind uds = uds { ud_binds = mkDB bind `consBag` ud_binds uds }

addDictBinds :: [DictBind] -> UsageDetails -> UsageDetails
addDictBinds binds uds = uds { ud_binds = listToBag binds `unionBags` ud_binds uds }

snocDictBind :: UsageDetails -> CoreBind -> UsageDetails
snocDictBind uds bind = uds { ud_binds = ud_binds uds `snocBag` mkDB bind }

wrapDictBinds :: Bag DictBind -> [CoreBind] -> [CoreBind]
wrapDictBinds dbs binds
  = foldrBag add binds dbs
  where
    add (bind,_) binds = bind : binds

wrapDictBindsE :: Bag DictBind -> CoreExpr -> CoreExpr
wrapDictBindsE dbs expr
  = foldrBag add expr dbs
  where
    add (bind,_) expr = Let bind expr

----------------------
dumpUDs :: [CoreBndr] -> UsageDetails -> (UsageDetails, Bag DictBind)
-- Used at a lambda or case binder; just dump anything mentioning the binder
dumpUDs bndrs uds@(MkUD { ud_binds = orig_dbs, ud_calls = orig_calls })
  | null bndrs = (uds, emptyBag)  -- Common in case alternatives
  | otherwise  = -- pprTrace "dumpUDs" (ppr bndrs $$ ppr free_uds $$ ppr dump_dbs) $
                 (free_uds, dump_dbs)
  where
    free_uds = MkUD { ud_binds = free_dbs, ud_calls = free_calls }
    bndr_set = mkVarSet bndrs
    (free_dbs, dump_dbs, dump_set) = splitDictBinds orig_dbs bndr_set
    free_calls = deleteCallsMentioning dump_set $   -- Drop calls mentioning bndr_set on the floor
                 deleteCallsFor bndrs orig_calls    -- Discard calls for bndr_set; there should be
                                                    -- no calls for any of the dicts in dump_dbs

dumpBindUDs :: [CoreBndr] -> UsageDetails -> (UsageDetails, Bag DictBind, Bool)
-- Used at a lambda or case binder; just dump anything mentioning the binder
dumpBindUDs bndrs (MkUD { ud_binds = orig_dbs, ud_calls = orig_calls })
  = -- pprTrace "dumpBindUDs" (ppr bndrs $$ ppr free_uds $$ ppr dump_dbs) $
    (free_uds, dump_dbs, float_all)
  where
    free_uds = MkUD { ud_binds = free_dbs, ud_calls = free_calls }
    bndr_set = mkVarSet bndrs
    (free_dbs, dump_dbs, dump_set) = splitDictBinds orig_dbs bndr_set
    free_calls = deleteCallsFor bndrs orig_calls
    float_all = dump_set `intersectsVarSet` callDetailsFVs free_calls

callsForMe :: Id -> UsageDetails -> (UsageDetails, [CallInfo])
callsForMe fn (MkUD { ud_binds = orig_dbs, ud_calls = orig_calls })
  = -- pprTrace ("callsForMe")
    --         (vcat [ppr fn,
    --                text "Orig dbs ="     <+> ppr (_dictBindBndrs orig_dbs),
    --                text "Orig calls ="   <+> ppr orig_calls,
    --                text "Dep set ="      <+> ppr dep_set,
    --                text "Calls for me =" <+> ppr calls_for_me]) $
    (uds_without_me, calls_for_me)
  where
    uds_without_me = MkUD { ud_binds = orig_dbs, ud_calls = delVarEnv orig_calls fn }
    calls_for_me = case lookupVarEnv orig_calls fn of
                        Nothing -> []
                        Just (CIS _ calls) -> filter_dfuns (Map.toList calls)

    dep_set = foldlBag go (unitVarSet fn) orig_dbs
    go dep_set (db,fvs) | fvs `intersectsVarSet` dep_set
                        = extendVarSetList dep_set (bindersOf db)
                        | otherwise = dep_set

        -- Note [Specialisation of dictionary functions]
    filter_dfuns | isDFunId fn = filter ok_call
                 | otherwise   = \cs -> cs

    ok_call (_, (_,fvs)) = not (fvs `intersectsVarSet` dep_set)

----------------------
splitDictBinds :: Bag DictBind -> IdSet -> (Bag DictBind, Bag DictBind, IdSet)
-- Returns (free_dbs, dump_dbs, dump_set)
splitDictBinds dbs bndr_set
   = foldlBag split_db (emptyBag, emptyBag, bndr_set) dbs
                -- Important that it's foldl not foldr;
                -- we're accumulating the set of dumped ids in dump_set
   where
    split_db (free_dbs, dump_dbs, dump_idset) db@(bind, fvs)
        | dump_idset `intersectsVarSet` fvs     -- Dump it
        = (free_dbs, dump_dbs `snocBag` db,
           extendVarSetList dump_idset (bindersOf bind))

        | otherwise     -- Don't dump it
        = (free_dbs `snocBag` db, dump_dbs, dump_idset)


----------------------
deleteCallsMentioning :: VarSet -> CallDetails -> CallDetails
-- Remove calls *mentioning* bs
deleteCallsMentioning bs calls
  = mapVarEnv filter_calls calls
  where
    filter_calls :: CallInfoSet -> CallInfoSet
    filter_calls (CIS f calls) = CIS f (Map.filter keep_call calls)
    keep_call (_, fvs) = not (fvs `intersectsVarSet` bs)

deleteCallsFor :: [Id] -> CallDetails -> CallDetails
-- Remove calls *for* bs
deleteCallsFor bs calls = delVarEnvList calls bs
\end{code}


%************************************************************************
%*                                                                      *
\subsubsection{Boring helper functions}
%*                                                                      *
%************************************************************************

\begin{code}
newtype SpecM a = SpecM (State SpecState a)

data SpecState = SpecState {
                     spec_uniq_supply :: UniqSupply,
                     spec_dflags :: DynFlags
                 }

instance Functor SpecM where
    fmap = liftM

instance Applicative SpecM where
    pure = return
    (<*>) = ap

instance Monad SpecM where
    SpecM x >>= f = SpecM $ do y <- x
                               case f y of
                                   SpecM z ->
                                       z
    return x = SpecM $ return x
    fail str = SpecM $ fail str

instance MonadUnique SpecM where
    getUniqueSupplyM
        = SpecM $ do st <- get
                     let (us1, us2) = splitUniqSupply $ spec_uniq_supply st
                     put $ st { spec_uniq_supply = us2 }
                     return us1

    getUniqueM
        = SpecM $ do st <- get
                     let (u,us') = takeUniqFromSupply $ spec_uniq_supply st
                     put $ st { spec_uniq_supply = us' }
                     return u

instance HasDynFlags SpecM where
    getDynFlags = SpecM $ liftM spec_dflags get

runSpecM :: DynFlags -> SpecM a -> CoreM a
runSpecM dflags (SpecM spec)
    = do us <- getUniqueSupplyM
         let initialState = SpecState {
                                spec_uniq_supply = us,
                                spec_dflags = dflags
                            }
         return $ evalState spec initialState

mapAndCombineSM :: (a -> SpecM (b, UsageDetails)) -> [a] -> SpecM ([b], UsageDetails)
mapAndCombineSM _ []     = return ([], emptyUDs)
mapAndCombineSM f (x:xs) = do (y, uds1) <- f x
                              (ys, uds2) <- mapAndCombineSM f xs
                              return (y:ys, uds1 `plusUDs` uds2)

extendTvSubstList :: SpecEnv -> [(TyVar,Type)] -> SpecEnv
extendTvSubstList env tv_binds 
  = env { se_subst = CoreSubst.extendTvSubstList (se_subst env) tv_binds }

substTy :: SpecEnv -> Type -> Type
substTy env ty = CoreSubst.substTy (se_subst env) ty

substCo :: SpecEnv -> Coercion -> Coercion
substCo env co = CoreSubst.substCo (se_subst env) co

substBndr :: SpecEnv -> CoreBndr -> (SpecEnv, CoreBndr)
substBndr env bs = case CoreSubst.substBndr (se_subst env) bs of
                      (subst', bs') -> (env { se_subst = subst' }, bs')

substBndrs :: SpecEnv -> [CoreBndr] -> (SpecEnv, [CoreBndr])
substBndrs env bs = case CoreSubst.substBndrs (se_subst env) bs of
                      (subst', bs') -> (env { se_subst = subst' }, bs')

cloneBindSM :: SpecEnv -> CoreBind -> SpecM (SpecEnv, SpecEnv, CoreBind)
-- Clone the binders of the bind; return new bind with the cloned binders
-- Return the substitution to use for RHSs, and the one to use for the body
cloneBindSM env@(SE { se_subst = subst, se_interesting = interesting }) (NonRec bndr rhs)
  = do { us <- getUniqueSupplyM
       ; let (subst', bndr') = CoreSubst.cloneIdBndr subst us bndr
             interesting' | interestingDict env rhs 
                          = interesting `extendVarSet` bndr'
                          | otherwise = interesting
       ; return (env, env { se_subst = subst', se_interesting = interesting' }
                , NonRec bndr' rhs) }

cloneBindSM env@(SE { se_subst = subst, se_interesting = interesting }) (Rec pairs)
  = do { us <- getUniqueSupplyM
       ; let (subst', bndrs') = CoreSubst.cloneRecIdBndrs subst us (map fst pairs)
             env' = env { se_subst = subst'
                        , se_interesting = interesting `extendVarSetList` 
                                           [ v | (v,r) <- pairs, interestingDict env r ] }
       ; return (env', env', Rec (bndrs' `zip` map snd pairs)) }

newDictBndr :: SpecEnv -> CoreBndr -> SpecM CoreBndr
-- Make up completely fresh binders for the dictionaries
-- Their bindings are going to float outwards
newDictBndr env b = do { uniq <- getUniqueM
                       ; let n   = idName b
                             ty' = substTy env (idType b)
                       ; return (mkUserLocal (nameOccName n) uniq ty' (getSrcSpan n)) }

newSpecIdSM :: Id -> Type -> SpecM Id
    -- Give the new Id a similar occurrence name to the old one
newSpecIdSM old_id new_ty
  = do  { uniq <- getUniqueM
        ; let name    = idName old_id
              new_occ = mkSpecOcc (nameOccName name)
              new_id  = mkUserLocal new_occ uniq new_ty (getSrcSpan name)
        ; return new_id }
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

