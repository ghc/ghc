{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998
-}

{-# LANGUAGE CPP, DeriveDataTypeable, FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}

-- | CoreSyn holds all the main data types for use by for the Glasgow Haskell Compiler midsection
module CoreSyn (
        -- * Main data types
        Expr(..), Alt, Bind(..), AltCon(..), Arg,
        Tickish(..), TickishScoping(..), TickishPlacement(..),
        CoreProgram, CoreExpr, CoreAlt, CoreBind, CoreArg, CoreBndr,
        TaggedExpr, TaggedAlt, TaggedBind, TaggedArg, TaggedBndr(..), deTagExpr,

        -- * In/Out type synonyms
        InId, InBind, InExpr, InAlt, InArg, InType, InKind,
               InBndr, InVar, InCoercion, InTyVar, InCoVar,
        OutId, OutBind, OutExpr, OutAlt, OutArg, OutType, OutKind,
               OutBndr, OutVar, OutCoercion, OutTyVar, OutCoVar,

        -- ** 'Expr' construction
        mkLet, mkLets, mkLetNonRec, mkLetRec, mkLams,
        mkApps, mkTyApps, mkCoApps, mkVarApps, mkTyArg,

        mkIntLit, mkIntLitInt,
        mkWordLit, mkWordLitWord,
        mkWord64LitWord64, mkInt64LitInt64,
        mkCharLit, mkStringLit,
        mkFloatLit, mkFloatLitFloat,
        mkDoubleLit, mkDoubleLitDouble,

        mkConApp, mkConApp2, mkTyBind, mkCoBind,
        varToCoreExpr, varsToCoreExprs,

        isId, cmpAltCon, cmpAlt, ltAlt,

        -- ** Simple 'Expr' access functions and predicates
        bindersOf, bindersOfBinds, rhssOfBind, rhssOfAlts,
        collectBinders, collectTyBinders, collectTyAndValBinders,
        collectNBinders,
        collectArgs, collectArgsTicks, flattenBinds,

        exprToType, exprToCoercion_maybe,
        applyTypeToArg,

        isValArg, isTypeArg, isTyCoArg, valArgCount, valBndrCount,
        isRuntimeArg, isRuntimeVar,

        -- * Tick-related functions
        tickishCounts, tickishScoped, tickishScopesLike, tickishFloatable,
        tickishCanSplit, mkNoCount, mkNoScope,
        tickishIsCode, tickishPlace,
        tickishContains,

        -- * Unfolding data types
        Unfolding(..),  UnfoldingGuidance(..), UnfoldingSource(..),

        -- ** Constructing 'Unfolding's
        noUnfolding, bootUnfolding, evaldUnfolding, mkOtherCon,
        unSaturatedOk, needSaturated, boringCxtOk, boringCxtNotOk,

        -- ** Predicates and deconstruction on 'Unfolding'
        unfoldingTemplate, expandUnfolding_maybe,
        maybeUnfoldingTemplate, otherCons,
        isValueUnfolding, isEvaldUnfolding, isCheapUnfolding,
        isExpandableUnfolding, isConLikeUnfolding, isCompulsoryUnfolding,
        isStableUnfolding, isFragileUnfolding, hasSomeUnfolding,
        isBootUnfolding,
        canUnfold, neverUnfoldGuidance, isStableSource,

        -- * Annotated expression data types
        AnnExpr, AnnExpr'(..), AnnBind(..), AnnAlt,

        -- ** Operations on annotated expressions
        collectAnnArgs, collectAnnArgsTicks,

        -- ** Operations on annotations
        deAnnotate, deAnnotate', deAnnAlt, deAnnBind,
        collectAnnBndrs, collectNAnnBndrs,

        -- * Orphanhood
        IsOrphan(..), isOrphan, notOrphan, chooseOrphanAnchor,

        -- * Core rule data types
        CoreRule(..), RuleBase,
        RuleName, RuleFun, IdUnfoldingFun, InScopeEnv,
        RuleEnv(..), mkRuleEnv, emptyRuleEnv,

        -- ** Operations on 'CoreRule's
        ruleArity, ruleName, ruleIdName, ruleActivation,
        setRuleIdName, ruleModule,
        isBuiltinRule, isLocalRule, isAutoRule,

        -- * Core vectorisation declarations data type
        CoreVect(..)
    ) where

#include "HsVersions.h"

import GhcPrelude

import CostCentre
import VarEnv( InScopeSet )
import Var
import Type
import Coercion
import Name
import NameSet
import NameEnv( NameEnv, emptyNameEnv )
import Literal
import DataCon
import Module
import TyCon
import BasicTypes
import DynFlags
import Outputable
import Util
import UniqSet
import SrcLoc     ( RealSrcSpan, containsSpan )
import Binary

import Data.Data hiding (TyCon)
import Data.Int
import Data.Word

infixl 4 `mkApps`, `mkTyApps`, `mkVarApps`, `App`, `mkCoApps`
-- Left associative, so that we can say (f `mkTyApps` xs `mkVarApps` ys)

{-
************************************************************************
*                                                                      *
\subsection{The main data types}
*                                                                      *
************************************************************************

These data types are the heart of the compiler
-}

-- | This is the data type that represents GHCs core intermediate language. Currently
-- GHC uses System FC <https://www.microsoft.com/en-us/research/publication/system-f-with-type-equality-coercions/> for this purpose,
-- which is closely related to the simpler and better known System F <http://en.wikipedia.org/wiki/System_F>.
--
-- We get from Haskell source to this Core language in a number of stages:
--
-- 1. The source code is parsed into an abstract syntax tree, which is represented
--    by the data type 'HsExpr.HsExpr' with the names being 'RdrName.RdrNames'
--
-- 2. This syntax tree is /renamed/, which attaches a 'Unique.Unique' to every 'RdrName.RdrName'
--    (yielding a 'Name.Name') to disambiguate identifiers which are lexically identical.
--    For example, this program:
--
-- @
--      f x = let f x = x + 1
--            in f (x - 2)
-- @
--
--    Would be renamed by having 'Unique's attached so it looked something like this:
--
-- @
--      f_1 x_2 = let f_3 x_4 = x_4 + 1
--                in f_3 (x_2 - 2)
-- @
--    But see Note [Shadowing] below.
--
-- 3. The resulting syntax tree undergoes type checking (which also deals with instantiating
--    type class arguments) to yield a 'HsExpr.HsExpr' type that has 'Id.Id' as it's names.
--
-- 4. Finally the syntax tree is /desugared/ from the expressive 'HsExpr.HsExpr' type into
--    this 'Expr' type, which has far fewer constructors and hence is easier to perform
--    optimization, analysis and code generation on.
--
-- The type parameter @b@ is for the type of binders in the expression tree.
--
-- The language consists of the following elements:
--
-- *  Variables
--
-- *  Primitive literals
--
-- *  Applications: note that the argument may be a 'Type'.
--    See Note [CoreSyn let/app invariant]
--    See Note [Levity polymorphism invariants]
--
-- *  Lambda abstraction
--    See Note [Levity polymorphism invariants]
--
-- *  Recursive and non recursive @let@s. Operationally
--    this corresponds to allocating a thunk for the things
--    bound and then executing the sub-expression.
--
--    #top_level_invariant#
--    #letrec_invariant#
--
--    The right hand sides of all top-level and recursive @let@s
--    /must/ be of lifted type (see "Type#type_classification" for
--    the meaning of /lifted/ vs. /unlifted/). There is one exception
--    to this rule, top-level @let@s are allowed to bind primitive
--    string literals, see Note [CoreSyn top-level string literals].
--
--    See Note [CoreSyn let/app invariant]
--    See Note [Levity polymorphism invariants]
--
--    #type_let#
--    We allow a /non-recursive/ let to bind a type variable, thus:
--
--    > Let (NonRec tv (Type ty)) body
--
--    This can be very convenient for postponing type substitutions until
--    the next run of the simplifier.
--
--    At the moment, the rest of the compiler only deals with type-let
--    in a Let expression, rather than at top level.  We may want to revist
--    this choice.
--
-- *  Case expression. Operationally this corresponds to evaluating
--    the scrutinee (expression examined) to weak head normal form
--    and then examining at most one level of resulting constructor (i.e. you
--    cannot do nested pattern matching directly with this).
--
--    The binder gets bound to the value of the scrutinee,
--    and the 'Type' must be that of all the case alternatives
--
--    #case_invariants#
--    This is one of the more complicated elements of the Core language,
--    and comes with a number of restrictions:
--
--    1. The list of alternatives may be empty;
--       See Note [Empty case alternatives]
--
--    2. The 'DEFAULT' case alternative must be first in the list,
--       if it occurs at all.
--
--    3. The remaining cases are in order of increasing
--         tag  (for 'DataAlts') or
--         lit  (for 'LitAlts').
--       This makes finding the relevant constructor easy,
--       and makes comparison easier too.
--
--    4. The list of alternatives must be exhaustive. An /exhaustive/ case
--       does not necessarily mention all constructors:
--
--       @
--            data Foo = Red | Green | Blue
--       ... case x of
--            Red   -> True
--            other -> f (case x of
--                            Green -> ...
--                            Blue  -> ... ) ...
--       @
--
--       The inner case does not need a @Red@ alternative, because @x@
--       can't be @Red@ at that program point.
--
--    5. Floating-point values must not be scrutinised against literals.
--       See Trac #9238 and Note [Rules for floating-point comparisons]
--       in PrelRules for rationale.
--
-- *  Cast an expression to a particular type.
--    This is used to implement @newtype@s (a @newtype@ constructor or
--    destructor just becomes a 'Cast' in Core) and GADTs.
--
-- *  Notes. These allow general information to be added to expressions
--    in the syntax tree
--
-- *  A type: this should only show up at the top level of an Arg
--
-- *  A coercion

-- If you edit this type, you may need to update the GHC formalism
-- See Note [GHC Formalism] in coreSyn/CoreLint.hs
data Expr b
  = Var   Id
  | Lit   Literal
  | App   (Expr b) (Arg b)
  | Lam   b (Expr b)
  | Let   (Bind b) (Expr b)
  | Case  (Expr b) b Type [Alt b]       -- See #case_invariants#
  | Cast  (Expr b) Coercion
  | Tick  (Tickish Id) (Expr b)
  | Type  Type
  | Coercion Coercion
  deriving Data

-- | Type synonym for expressions that occur in function argument positions.
-- Only 'Arg' should contain a 'Type' at top level, general 'Expr' should not
type Arg b = Expr b

-- | A case split alternative. Consists of the constructor leading to the alternative,
-- the variables bound from the constructor, and the expression to be executed given that binding.
-- The default alternative is @(DEFAULT, [], rhs)@

-- If you edit this type, you may need to update the GHC formalism
-- See Note [GHC Formalism] in coreSyn/CoreLint.hs
type Alt b = (AltCon, [b], Expr b)

-- | A case alternative constructor (i.e. pattern match)

-- If you edit this type, you may need to update the GHC formalism
-- See Note [GHC Formalism] in coreSyn/CoreLint.hs
data AltCon
  = DataAlt DataCon   --  ^ A plain data constructor: @case e of { Foo x -> ... }@.
                      -- Invariant: the 'DataCon' is always from a @data@ type, and never from a @newtype@

  | LitAlt  Literal   -- ^ A literal: @case e of { 1 -> ... }@
                      -- Invariant: always an *unlifted* literal
                      -- See Note [Literal alternatives]

  | DEFAULT           -- ^ Trivial alternative: @case e of { _ -> ... }@
   deriving (Eq, Data)

-- This instance is a bit shady. It can only be used to compare AltCons for
-- a single type constructor. Fortunately, it seems quite unlikely that we'll
-- ever need to compare AltCons for different type constructors.
-- The instance adheres to the order described in [CoreSyn case invariants]
instance Ord AltCon where
  compare (DataAlt con1) (DataAlt con2) =
    ASSERT( dataConTyCon con1 == dataConTyCon con2 )
    compare (dataConTag con1) (dataConTag con2)
  compare (DataAlt _) _ = GT
  compare _ (DataAlt _) = LT
  compare (LitAlt l1) (LitAlt l2) = compare l1 l2
  compare (LitAlt _) DEFAULT = GT
  compare DEFAULT DEFAULT = EQ
  compare DEFAULT _ = LT

-- | Binding, used for top level bindings in a module and local bindings in a @let@.

-- If you edit this type, you may need to update the GHC formalism
-- See Note [GHC Formalism] in coreSyn/CoreLint.hs
data Bind b = NonRec b (Expr b)
            | Rec [(b, (Expr b))]
  deriving Data

{-
Note [Shadowing]
~~~~~~~~~~~~~~~~
While various passes attempt to rename on-the-fly in a manner that
avoids "shadowing" (thereby simplifying downstream optimizations),
neither the simplifier nor any other pass GUARANTEES that shadowing is
avoided. Thus, all passes SHOULD work fine even in the presence of
arbitrary shadowing in their inputs.

In particular, scrutinee variables `x` in expressions of the form
`Case e x t` are often renamed to variables with a prefix
"wild_". These "wild" variables may appear in the body of the
case-expression, and further, may be shadowed within the body.

So the Unique in a Var is not really unique at all.  Still, it's very
useful to give a constant-time equality/ordering for Vars, and to give
a key that can be used to make sets of Vars (VarSet), or mappings from
Vars to other things (VarEnv).   Moreover, if you do want to eliminate
shadowing, you can give a new Unique to an Id without changing its
printable name, which makes debugging easier.

Note [Literal alternatives]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
Literal alternatives (LitAlt lit) are always for *un-lifted* literals.
We have one literal, a literal Integer, that is lifted, and we don't
allow in a LitAlt, because LitAlt cases don't do any evaluation. Also
(see Trac #5603) if you say
    case 3 of
      S# x -> ...
      J# _ _ -> ...
(where S#, J# are the constructors for Integer) we don't want the
simplifier calling findAlt with argument (LitAlt 3).  No no.  Integer
literals are an opaque encoding of an algebraic data type, not of
an unlifted literal, like all the others.

Also, we do not permit case analysis with literal patterns on floating-point
types. See Trac #9238 and Note [Rules for floating-point comparisons] in
PrelRules for the rationale for this restriction.

-------------------------- CoreSyn INVARIANTS ---------------------------

Note [CoreSyn top-level invariant]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
See #toplevel_invariant#

Note [CoreSyn letrec invariant]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
See #letrec_invariant#

Note [CoreSyn top-level string literals]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
As an exception to the usual rule that top-level binders must be lifted,
we allow binding primitive string literals (of type Addr#) of type Addr# at the
top level. This allows us to share string literals earlier in the pipeline and
crucially allows other optimizations in the Core2Core pipeline to fire.
Consider,

  f n = let a::Addr# = "foo"#
        in \x -> blah

In order to be able to inline `f`, we would like to float `a` to the top.
Another option would be to inline `a`, but that would lead to duplicating string
literals, which we want to avoid. See Trac #8472.

The solution is simply to allow top-level unlifted binders. We can't allow
arbitrary unlifted expression at the top-level though, unlifted binders cannot
be thunks, so we just allow string literals.

It is important to note that top-level primitive string literals cannot be
wrapped in Ticks, as is otherwise done with lifted bindings. CoreToStg expects
to see just a plain (Lit (MachStr ...)) expression on the RHS of primitive
string bindings; anything else and things break. CoreLint checks this invariant.

Also see Note [Compilation plan for top-level string literals].

Note [Compilation plan for top-level string literals]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Here is a summary on how top-level string literals are handled by various
parts of the compilation pipeline.

* In the source language, there is no way to bind a primitive string literal
  at the top leve.

* In Core, we have a special rule that permits top-level Addr# bindings. See
  Note [CoreSyn top-level string literals]. Core-to-core passes may introduce
  new top-level string literals.

* In STG, top-level string literals are explicitly represented in the syntax
  tree.

* A top-level string literal may end up exported from a module. In this case,
  in the object file, the content of the exported literal is given a label with
  the _bytes suffix.

Note [CoreSyn let/app invariant]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The let/app invariant
     the right hand side of a non-recursive 'Let', and
     the argument of an 'App',
    /may/ be of unlifted type, but only if
    the expression is ok-for-speculation
    or the 'Let' is for a join point.

This means that the let can be floated around
without difficulty. For example, this is OK:

   y::Int# = x +# 1#

But this is not, as it may affect termination if the
expression is floated out:

   y::Int# = fac 4#

In this situation you should use @case@ rather than a @let@. The function
'CoreUtils.needsCaseBinding' can help you determine which to generate, or
alternatively use 'MkCore.mkCoreLet' rather than this constructor directly,
which will generate a @case@ if necessary

The let/app invariant is initially enforced by mkCoreLet and mkCoreApp in
coreSyn/MkCore.

Note [CoreSyn case invariants]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
See #case_invariants#

Note [Levity polymorphism invariants]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The levity-polymorphism invariants are these (as per "Levity Polymorphism",
PLDI '17):

* The type of a term-binder must not be levity-polymorphic,
  unless it is a let(rec)-bound join point
     (see Note [Invariants on join points])

* The type of the argument of an App must not be levity-polymorphic.

A type (t::TYPE r) is "levity polymorphic" if 'r' has any free variables.

For example
  \(r::RuntimeRep). \(a::TYPE r). \(x::a). e
is illegal because x's type has kind (TYPE r), which has 'r' free.

See Note [Levity polymorphism checking] in DsMonad to see where these
invariants are established for user-written code.

Note [CoreSyn let goal]
~~~~~~~~~~~~~~~~~~~~~~~
* The simplifier tries to ensure that if the RHS of a let is a constructor
  application, its arguments are trivial, so that the constructor can be
  inlined vigorously.

Note [Type let]
~~~~~~~~~~~~~~~
See #type_let#

Note [Empty case alternatives]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The alternatives of a case expression should be exhaustive.  But
this exhaustive list can be empty!

* A case expression can have empty alternatives if (and only if) the
  scrutinee is bound to raise an exception or diverge. When do we know
  this?  See Note [Bottoming expressions] in CoreUtils.

* The possiblity of empty alternatives is one reason we need a type on
  the case expression: if the alternatives are empty we can't get the
  type from the alternatives!

* In the case of empty types (see Note [Bottoming expressions]), say
    data T
  we do NOT want to replace
    case (x::T) of Bool {}   -->   error Bool "Inaccessible case"
  because x might raise an exception, and *that*'s what we want to see!
  (Trac #6067 is an example.) To preserve semantics we'd have to say
     x `seq` error Bool "Inaccessible case"
  but the 'seq' is just a case, so we are back to square 1.  Or I suppose
  we could say
     x |> UnsafeCoerce T Bool
  but that loses all trace of the fact that this originated with an empty
  set of alternatives.

* We can use the empty-alternative construct to coerce error values from
  one type to another.  For example

    f :: Int -> Int
    f n = error "urk"

    g :: Int -> (# Char, Bool #)
    g x = case f x of { 0 -> ..., n -> ... }

  Then if we inline f in g's RHS we get
    case (error Int "urk") of (# Char, Bool #) { ... }
  and we can discard the alternatives since the scrutinee is bottom to give
    case (error Int "urk") of (# Char, Bool #) {}

  This is nicer than using an unsafe coerce between Int ~ (# Char,Bool #),
  if for no other reason that we don't need to instantiate the (~) at an
  unboxed type.

* We treat a case expression with empty alternatives as trivial iff
  its scrutinee is (see CoreUtils.exprIsTrivial).  This is actually
  important; see Note [Empty case is trivial] in CoreUtils

* An empty case is replaced by its scrutinee during the CoreToStg
  conversion; remember STG is un-typed, so there is no need for
  the empty case to do the type conversion.

Note [Join points]
~~~~~~~~~~~~~~~~~~
In Core, a *join point* is a specially tagged function whose only occurrences
are saturated tail calls. A tail call can appear in these places:

  1. In the branches (not the scrutinee) of a case
  2. Underneath a let (value or join point)
  3. Inside another join point

We write a join-point declaration as
  join j @a @b x y = e1 in e2,
like a let binding but with "join" instead (or "join rec" for "let rec"). Note
that we put the parameters before the = rather than using lambdas; this is
because it's relevant how many parameters the join point takes *as a join
point.* This number is called the *join arity,* distinct from arity because it
counts types as well as values. Note that a join point may return a lambda! So
  join j x = x + 1
is different from
  join j = \x -> x + 1
The former has join arity 1, while the latter has join arity 0.

The identifier for a join point is called a join id or a *label.* An invocation
is called a *jump.* We write a jump using the jump keyword:

  jump j 3

The words *label* and *jump* are evocative of assembly code (or Cmm) for a
reason: join points are indeed compiled as labeled blocks, and jumps become
actual jumps (plus argument passing and stack adjustment). There is no closure
allocated and only a fraction of the function-call overhead. Hence we would
like as many functions as possible to become join points (see OccurAnal) and
the type rules for join points ensure we preserve the properties that make them
efficient.

In the actual AST, a join point is indicated by the IdDetails of the binder: a
local value binding gets 'VanillaId' but a join point gets a 'JoinId' with its
join arity.

For more details, see the paper:

  Luke Maurer, Paul Downen, Zena Ariola, and Simon Peyton Jones. "Compiling
  without continuations." Submitted to PLDI'17.

  https://www.microsoft.com/en-us/research/publication/compiling-without-continuations/

Note [Invariants on join points]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Join points must follow these invariants:

  1. All occurrences must be tail calls. Each of these tail calls must pass the
     same number of arguments, counting both types and values; we call this the
     "join arity" (to distinguish from regular arity, which only counts values).

  2. For join arity n, the right-hand side must begin with at least n lambdas.
     No ticks, no casts, just lambdas!  C.f. CoreUtils.joinRhsArity.

  2a. Moreover, this same constraint applies to any unfolding of the binder.
     Reason: if we want to push a continuation into the RHS we must push it
     into the unfolding as well.

  3. If the binding is recursive, then all other bindings in the recursive group
     must also be join points.

  4. The binding's type must not be polymorphic in its return type (as defined
     in Note [The polymorphism rule of join points]).

However, join points have simpler invariants in other ways

  5. A join point can have an unboxed type without the RHS being
     ok-for-speculation (i.e. drop the let/app invariant)
     e.g.  let j :: Int# = factorial x in ...

  6. A join point can have a levity-polymorphic RHS
     e.g.  let j :: r :: TYPE l = fail void# in ...
     This happened in an intermediate program Trac #13394

Examples:

  join j1  x = 1 + x in jump j (jump j x)  -- Fails 1: non-tail call
  join j1' x = 1 + x in if even a
                          then jump j1 a
                          else jump j1 a b -- Fails 1: inconsistent calls
  join j2  x = flip (+) x in j2 1 2        -- Fails 2: not enough lambdas
  join j2' x = \y -> x + y in j3 1         -- Passes: extra lams ok
  join j @a (x :: a) = x                   -- Fails 4: polymorphic in ret type

Invariant 1 applies to left-hand sides of rewrite rules, so a rule for a join
point must have an exact call as its LHS.

Strictly speaking, invariant 3 is redundant, since a call from inside a lazy
binding isn't a tail call. Since a let-bound value can't invoke a free join
point, then, they can't be mutually recursive. (A Core binding group *can*
include spurious extra bindings if the occurrence analyser hasn't run, so
invariant 3 does still need to be checked.) For the rigorous definition of
"tail call", see Section 3 of the paper (Note [Join points]).

Invariant 4 is subtle; see Note [The polymorphism rule of join points].

Core Lint will check these invariants, anticipating that any binder whose
OccInfo is marked AlwaysTailCalled will become a join point as soon as the
simplifier (or simpleOptPgm) runs.

Note [The type of a join point]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A join point has the same type it would have as a function. That is, if it takes
an Int and a Bool and its body produces a String, its type is `Int -> Bool ->
String`. Natural as this may seem, it can be awkward. A join point shouldn't be
thought to "return" in the same sense a function does---a jump is one-way. This
is crucial for understanding how case-of-case interacts with join points:

  case (join
          j :: Int -> Bool -> String
          j x y = ...
        in
          jump j z w) of
    "" -> True
    _  -> False

The simplifier will pull the case into the join point (see Note [Case-of-case
and join points] in Simplify):

  join
    j :: Int -> Bool -> Bool -- changed!
    j x y = case ... of "" -> True
                        _  -> False
  in
    jump j z w

The body of the join point now returns a Bool, so the label `j` has to have its
type updated accordingly. Inconvenient though this may be, it has the advantage
that 'CoreUtils.exprType' can still return a type for any expression, including
a jump.

This differs from the paper (see Note [Invariants on join points]). In the
paper, we instead give j the type `Int -> Bool -> forall a. a`. Then each jump
carries the "return type" as a parameter, exactly the way other non-returning
functions like `error` work:

  case (join
          j :: Int -> Bool -> forall a. a
          j x y = ...
        in
          jump j z w @String) of
    "" -> True
    _  -> False

Now we can move the case inward and we only have to change the jump:

  join
    j :: Int -> Bool -> forall a. a
    j x y = case ... of "" -> True
                        _  -> False
  in
    jump j z w @Bool

(Core Lint would still check that the body of the join point has the right type;
that type would simply not be reflected in the join id.)

Note [The polymorphism rule of join points]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Invariant 4 of Note [Invariants on join points] forbids a join point to be
polymorphic in its return type. That is, if its type is

  forall a1 ... ak. t1 -> ... -> tn -> r

where its join arity is k+n, none of the type parameters ai may occur free in r.
The most direct explanation is that given

  join j @a1 ... @ak x1 ... xn = e1 in e2

our typing rules require `e1` and `e2` to have the same type. Therefore the type
of `e1`---the return type of the join point---must be the same as the type of
e2. Since the type variables aren't bound in `e2`, its type can't include them,
and thus neither can the type of `e1`.

There's a deeper explanation in terms of the sequent calculus in Section 5.3 of
a previous paper:

  Paul Downen, Luke Maurer, Zena Ariola, and Simon Peyton Jones. "Sequent
  calculus as a compiler intermediate language." ICFP'16.

  https://www.microsoft.com/en-us/research/wp-content/uploads/2016/04/sequent-calculus-icfp16.pdf

The quick version: Consider the CPS term (the paper uses the sequent calculus,
but we can translate readily):

  \k -> join j @a1 ... @ak x1 ... xn = e1 k in e2 k

Since `j` is a join point, it doesn't bind a continuation variable but reuses
the variable `k` from the context. But the parameters `ai` are not in `k`'s
scope, and `k`'s type determines the return type of `j`; thus the `ai`s don't
appear in the return type of `j`. (Also, since `e1` and `e2` are passed the same
continuation, they must have the same type; hence the direct explanation above.)

************************************************************************
*                                                                      *
            In/Out type synonyms
*                                                                      *
********************************************************************* -}

{- Many passes apply a substitution, and it's very handy to have type
   synonyms to remind us whether or not the substitution has been applied -}

-- Pre-cloning or substitution
type InBndr     = CoreBndr
type InType     = Type
type InKind     = Kind
type InBind     = CoreBind
type InExpr     = CoreExpr
type InAlt      = CoreAlt
type InArg      = CoreArg
type InCoercion = Coercion

-- Post-cloning or substitution
type OutBndr     = CoreBndr
type OutType     = Type
type OutKind     = Kind
type OutCoercion = Coercion
type OutBind     = CoreBind
type OutExpr     = CoreExpr
type OutAlt      = CoreAlt
type OutArg      = CoreArg


{- *********************************************************************
*                                                                      *
              Ticks
*                                                                      *
************************************************************************
-}

-- | Allows attaching extra information to points in expressions

-- If you edit this type, you may need to update the GHC formalism
-- See Note [GHC Formalism] in coreSyn/CoreLint.hs
data Tickish id =
    -- | An @{-# SCC #-}@ profiling annotation, either automatically
    -- added by the desugarer as a result of -auto-all, or added by
    -- the user.
    ProfNote {
      profNoteCC    :: CostCentre, -- ^ the cost centre
      profNoteCount :: !Bool,      -- ^ bump the entry count?
      profNoteScope :: !Bool       -- ^ scopes over the enclosed expression
                                   -- (i.e. not just a tick)
    }

  -- | A "tick" used by HPC to track the execution of each
  -- subexpression in the original source code.
  | HpcTick {
      tickModule :: Module,
      tickId     :: !Int
    }

  -- | A breakpoint for the GHCi debugger.  This behaves like an HPC
  -- tick, but has a list of free variables which will be available
  -- for inspection in GHCi when the program stops at the breakpoint.
  --
  -- NB. we must take account of these Ids when (a) counting free variables,
  -- and (b) substituting (don't substitute for them)
  | Breakpoint
    { breakpointId     :: !Int
    , breakpointFVs    :: [id]  -- ^ the order of this list is important:
                                -- it matches the order of the lists in the
                                -- appropriate entry in HscTypes.ModBreaks.
                                --
                                -- Careful about substitution!  See
                                -- Note [substTickish] in CoreSubst.
    }

  -- | A source note.
  --
  -- Source notes are pure annotations: Their presence should neither
  -- influence compilation nor execution. The semantics are given by
  -- causality: The presence of a source note means that a local
  -- change in the referenced source code span will possibly provoke
  -- the generated code to change. On the flip-side, the functionality
  -- of annotated code *must* be invariant against changes to all
  -- source code *except* the spans referenced in the source notes
  -- (see "Causality of optimized Haskell" paper for details).
  --
  -- Therefore extending the scope of any given source note is always
  -- valid. Note that it is still undesirable though, as this reduces
  -- their usefulness for debugging and profiling. Therefore we will
  -- generally try only to make use of this property where it is
  -- necessary to enable optimizations.
  | SourceNote
    { sourceSpan :: RealSrcSpan -- ^ Source covered
    , sourceName :: String      -- ^ Name for source location
                                --   (uses same names as CCs)
    }

  deriving (Eq, Ord, Data)

-- | A "counting tick" (where tickishCounts is True) is one that
-- counts evaluations in some way.  We cannot discard a counting tick,
-- and the compiler should preserve the number of counting ticks as
-- far as possible.
--
-- However, we still allow the simplifier to increase or decrease
-- sharing, so in practice the actual number of ticks may vary, except
-- that we never change the value from zero to non-zero or vice versa.
tickishCounts :: Tickish id -> Bool
tickishCounts n@ProfNote{} = profNoteCount n
tickishCounts HpcTick{}    = True
tickishCounts Breakpoint{} = True
tickishCounts _            = False


-- | Specifies the scoping behaviour of ticks. This governs the
-- behaviour of ticks that care about the covered code and the cost
-- associated with it. Important for ticks relating to profiling.
data TickishScoping =
    -- | No scoping: The tick does not care about what code it
    -- covers. Transformations can freely move code inside as well as
    -- outside without any additional annotation obligations
    NoScope

    -- | Soft scoping: We want all code that is covered to stay
    -- covered.  Note that this scope type does not forbid
    -- transformations from happening, as as long as all results of
    -- the transformations are still covered by this tick or a copy of
    -- it. For example
    --
    --   let x = tick<...> (let y = foo in bar) in baz
    --     ===>
    --   let x = tick<...> bar; y = tick<...> foo in baz
    --
    -- Is a valid transformation as far as "bar" and "foo" is
    -- concerned, because both still are scoped over by the tick.
    --
    -- Note though that one might object to the "let" not being
    -- covered by the tick any more. However, we are generally lax
    -- with this - constant costs don't matter too much, and given
    -- that the "let" was effectively merged we can view it as having
    -- lost its identity anyway.
    --
    -- Also note that this scoping behaviour allows floating a tick
    -- "upwards" in pretty much any situation. For example:
    --
    --   case foo of x -> tick<...> bar
    --     ==>
    --   tick<...> case foo of x -> bar
    --
    -- While this is always leagl, we want to make a best effort to
    -- only make us of this where it exposes transformation
    -- opportunities.
  | SoftScope

    -- | Cost centre scoping: We don't want any costs to move to other
    -- cost-centre stacks. This means we not only want no code or cost
    -- to get moved out of their cost centres, but we also object to
    -- code getting associated with new cost-centre ticks - or
    -- changing the order in which they get applied.
    --
    -- A rule of thumb is that we don't want any code to gain new
    -- annotations. However, there are notable exceptions, for
    -- example:
    --
    --   let f = \y -> foo in tick<...> ... (f x) ...
    --     ==>
    --   tick<...> ... foo[x/y] ...
    --
    -- In-lining lambdas like this is always legal, because inlining a
    -- function does not change the cost-centre stack when the
    -- function is called.
  | CostCentreScope

  deriving (Eq)

-- | Returns the intended scoping rule for a Tickish
tickishScoped :: Tickish id -> TickishScoping
tickishScoped n@ProfNote{}
  | profNoteScope n        = CostCentreScope
  | otherwise              = NoScope
tickishScoped HpcTick{}    = NoScope
tickishScoped Breakpoint{} = CostCentreScope
   -- Breakpoints are scoped: eventually we're going to do call
   -- stacks, but also this helps prevent the simplifier from moving
   -- breakpoints around and changing their result type (see #1531).
tickishScoped SourceNote{} = SoftScope

-- | Returns whether the tick scoping rule is at least as permissive
-- as the given scoping rule.
tickishScopesLike :: Tickish id -> TickishScoping -> Bool
tickishScopesLike t scope = tickishScoped t `like` scope
  where NoScope         `like` _               = True
        _               `like` NoScope         = False
        SoftScope       `like` _               = True
        _               `like` SoftScope       = False
        CostCentreScope `like` _               = True

-- | Returns @True@ for ticks that can be floated upwards easily even
-- where it might change execution counts, such as:
--
--   Just (tick<...> foo)
--     ==>
--   tick<...> (Just foo)
--
-- This is a combination of @tickishSoftScope@ and
-- @tickishCounts@. Note that in principle splittable ticks can become
-- floatable using @mkNoTick@ -- even though there's currently no
-- tickish for which that is the case.
tickishFloatable :: Tickish id -> Bool
tickishFloatable t = t `tickishScopesLike` SoftScope && not (tickishCounts t)

-- | Returns @True@ for a tick that is both counting /and/ scoping and
-- can be split into its (tick, scope) parts using 'mkNoScope' and
-- 'mkNoTick' respectively.
tickishCanSplit :: Tickish id -> Bool
tickishCanSplit ProfNote{profNoteScope = True, profNoteCount = True}
                   = True
tickishCanSplit _  = False

mkNoCount :: Tickish id -> Tickish id
mkNoCount n | not (tickishCounts n)   = n
            | not (tickishCanSplit n) = panic "mkNoCount: Cannot split!"
mkNoCount n@ProfNote{}                = n {profNoteCount = False}
mkNoCount _                           = panic "mkNoCount: Undefined split!"

mkNoScope :: Tickish id -> Tickish id
mkNoScope n | tickishScoped n == NoScope  = n
            | not (tickishCanSplit n)     = panic "mkNoScope: Cannot split!"
mkNoScope n@ProfNote{}                    = n {profNoteScope = False}
mkNoScope _                               = panic "mkNoScope: Undefined split!"

-- | Return @True@ if this source annotation compiles to some backend
-- code. Without this flag, the tickish is seen as a simple annotation
-- that does not have any associated evaluation code.
--
-- What this means that we are allowed to disregard the tick if doing
-- so means that we can skip generating any code in the first place. A
-- typical example is top-level bindings:
--
--   foo = tick<...> \y -> ...
--     ==>
--   foo = \y -> tick<...> ...
--
-- Here there is just no operational difference between the first and
-- the second version. Therefore code generation should simply
-- translate the code as if it found the latter.
tickishIsCode :: Tickish id -> Bool
tickishIsCode SourceNote{} = False
tickishIsCode _tickish     = True  -- all the rest for now


-- | Governs the kind of expression that the tick gets placed on when
-- annotating for example using @mkTick@. If we find that we want to
-- put a tickish on an expression ruled out here, we try to float it
-- inwards until we find a suitable expression.
data TickishPlacement =

    -- | Place ticks exactly on run-time expressions. We can still
    -- move the tick through pure compile-time constructs such as
    -- other ticks, casts or type lambdas. This is the most
    -- restrictive placement rule for ticks, as all tickishs have in
    -- common that they want to track runtime processes. The only
    -- legal placement rule for counting ticks.
    PlaceRuntime

    -- | As @PlaceRuntime@, but we float the tick through all
    -- lambdas. This makes sense where there is little difference
    -- between annotating the lambda and annotating the lambda's code.
  | PlaceNonLam

    -- | In addition to floating through lambdas, cost-centre style
    -- tickishs can also be moved from constructors, non-function
    -- variables and literals. For example:
    --
    --   let x = scc<...> C (scc<...> y) (scc<...> 3) in ...
    --
    -- Neither the constructor application, the variable or the
    -- literal are likely to have any cost worth mentioning. And even
    -- if y names a thunk, the call would not care about the
    -- evaluation context. Therefore removing all annotations in the
    -- above example is safe.
  | PlaceCostCentre

  deriving (Eq)

-- | Placement behaviour we want for the ticks
tickishPlace :: Tickish id -> TickishPlacement
tickishPlace n@ProfNote{}
  | profNoteCount n        = PlaceRuntime
  | otherwise              = PlaceCostCentre
tickishPlace HpcTick{}     = PlaceRuntime
tickishPlace Breakpoint{}  = PlaceRuntime
tickishPlace SourceNote{}  = PlaceNonLam

-- | Returns whether one tick "contains" the other one, therefore
-- making the second tick redundant.
tickishContains :: Eq b => Tickish b -> Tickish b -> Bool
tickishContains (SourceNote sp1 n1) (SourceNote sp2 n2)
  = containsSpan sp1 sp2 && n1 == n2
    -- compare the String last
tickishContains t1 t2
  = t1 == t2

{-
************************************************************************
*                                                                      *
                Orphans
*                                                                      *
************************************************************************
-}

-- | Is this instance an orphan?  If it is not an orphan, contains an 'OccName'
-- witnessing the instance's non-orphanhood.
-- See Note [Orphans]
data IsOrphan
  = IsOrphan
  | NotOrphan OccName -- The OccName 'n' witnesses the instance's non-orphanhood
                      -- In that case, the instance is fingerprinted as part
                      -- of the definition of 'n's definition
    deriving Data

-- | Returns true if 'IsOrphan' is orphan.
isOrphan :: IsOrphan -> Bool
isOrphan IsOrphan = True
isOrphan _ = False

-- | Returns true if 'IsOrphan' is not an orphan.
notOrphan :: IsOrphan -> Bool
notOrphan NotOrphan{} = True
notOrphan _ = False

chooseOrphanAnchor :: NameSet -> IsOrphan
-- Something (rule, instance) is relate to all the Names in this
-- list. Choose one of them to be an "anchor" for the orphan.  We make
-- the choice deterministic to avoid gratuitious changes in the ABI
-- hash (Trac #4012).  Specifically, use lexicographic comparison of
-- OccName rather than comparing Uniques
--
-- NB: 'minimum' use Ord, and (Ord OccName) works lexicographically
--
chooseOrphanAnchor local_names
  | isEmptyNameSet local_names = IsOrphan
  | otherwise                  = NotOrphan (minimum occs)
  where
    occs = map nameOccName $ nonDetEltsUniqSet local_names
    -- It's OK to use nonDetEltsUFM here, see comments above

instance Binary IsOrphan where
    put_ bh IsOrphan = putByte bh 0
    put_ bh (NotOrphan n) = do
        putByte bh 1
        put_ bh n
    get bh = do
        h <- getByte bh
        case h of
            0 -> return IsOrphan
            _ -> do
                n <- get bh
                return $ NotOrphan n

{-
Note [Orphans]
~~~~~~~~~~~~~~
Class instances, rules, and family instances are divided into orphans
and non-orphans.  Roughly speaking, an instance/rule is an orphan if
its left hand side mentions nothing defined in this module.  Orphan-hood
has two major consequences

 * A module that contains orphans is called an "orphan module".  If
   the module being compiled depends (transitively) on an oprhan
   module M, then M.hi is read in regardless of whether M is oherwise
   needed. This is to ensure that we don't miss any instance decls in
   M.  But it's painful, because it means we need to keep track of all
   the orphan modules below us.

 * A non-orphan is not finger-printed separately.  Instead, for
   fingerprinting purposes it is treated as part of the entity it
   mentions on the LHS.  For example
      data T = T1 | T2
      instance Eq T where ....
   The instance (Eq T) is incorprated as part of T's fingerprint.

   In contrast, orphans are all fingerprinted together in the
   mi_orph_hash field of the ModIface.

   See MkIface.addFingerprints.

Orphan-hood is computed
  * For class instances:
      when we make a ClsInst
    (because it is needed during instance lookup)

  * For rules and family instances:
       when we generate an IfaceRule (MkIface.coreRuleToIfaceRule)
                     or IfaceFamInst (MkIface.instanceToIfaceInst)
-}

{-
************************************************************************
*                                                                      *
\subsection{Transformation rules}
*                                                                      *
************************************************************************

The CoreRule type and its friends are dealt with mainly in CoreRules,
but CoreFVs, Subst, PprCore, CoreTidy also inspect the representation.
-}

-- | Gathers a collection of 'CoreRule's. Maps (the name of) an 'Id' to its rules
type RuleBase = NameEnv [CoreRule]
        -- The rules are unordered;
        -- we sort out any overlaps on lookup

-- | A full rule environment which we can apply rules from.  Like a 'RuleBase',
-- but it also includes the set of visible orphans we use to filter out orphan
-- rules which are not visible (even though we can see them...)
data RuleEnv
    = RuleEnv { re_base          :: RuleBase
              , re_visible_orphs :: ModuleSet
              }

mkRuleEnv :: RuleBase -> [Module] -> RuleEnv
mkRuleEnv rules vis_orphs = RuleEnv rules (mkModuleSet vis_orphs)

emptyRuleEnv :: RuleEnv
emptyRuleEnv = RuleEnv emptyNameEnv emptyModuleSet

-- | A 'CoreRule' is:
--
-- * \"Local\" if the function it is a rule for is defined in the
--   same module as the rule itself.
--
-- * \"Orphan\" if nothing on the LHS is defined in the same module
--   as the rule itself
data CoreRule
  = Rule {
        ru_name :: RuleName,            -- ^ Name of the rule, for communication with the user
        ru_act  :: Activation,          -- ^ When the rule is active

        -- Rough-matching stuff
        -- see comments with InstEnv.ClsInst( is_cls, is_rough )
        ru_fn    :: Name,               -- ^ Name of the 'Id.Id' at the head of this rule
        ru_rough :: [Maybe Name],       -- ^ Name at the head of each argument to the left hand side

        -- Proper-matching stuff
        -- see comments with InstEnv.ClsInst( is_tvs, is_tys )
        ru_bndrs :: [CoreBndr],         -- ^ Variables quantified over
        ru_args  :: [CoreExpr],         -- ^ Left hand side arguments

        -- And the right-hand side
        ru_rhs   :: CoreExpr,           -- ^ Right hand side of the rule
                                        -- Occurrence info is guaranteed correct
                                        -- See Note [OccInfo in unfoldings and rules]

        -- Locality
        ru_auto :: Bool,   -- ^ @True@  <=> this rule is auto-generated
                           --               (notably by Specialise or SpecConstr)
                           --   @False@ <=> generated at the user's behest
                           -- See Note [Trimming auto-rules] in TidyPgm
                           -- for the sole purpose of this field.

        ru_origin :: !Module,   -- ^ 'Module' the rule was defined in, used
                                -- to test if we should see an orphan rule.

        ru_orphan :: !IsOrphan, -- ^ Whether or not the rule is an orphan.

        ru_local :: Bool        -- ^ @True@ iff the fn at the head of the rule is
                                -- defined in the same module as the rule
                                -- and is not an implicit 'Id' (like a record selector,
                                -- class operation, or data constructor).  This
                                -- is different from 'ru_orphan', where a rule
                                -- can avoid being an orphan if *any* Name in
                                -- LHS of the rule was defined in the same
                                -- module as the rule.
    }

  -- | Built-in rules are used for constant folding
  -- and suchlike.  They have no free variables.
  -- A built-in rule is always visible (there is no such thing as
  -- an orphan built-in rule.)
  | BuiltinRule {
        ru_name  :: RuleName,   -- ^ As above
        ru_fn    :: Name,       -- ^ As above
        ru_nargs :: Int,        -- ^ Number of arguments that 'ru_try' consumes,
                                -- if it fires, including type arguments
        ru_try   :: RuleFun
                -- ^ This function does the rewrite.  It given too many
                -- arguments, it simply discards them; the returned 'CoreExpr'
                -- is just the rewrite of 'ru_fn' applied to the first 'ru_nargs' args
    }
                -- See Note [Extra args in rule matching] in Rules.hs

type RuleFun = DynFlags -> InScopeEnv -> Id -> [CoreExpr] -> Maybe CoreExpr
type InScopeEnv = (InScopeSet, IdUnfoldingFun)

type IdUnfoldingFun = Id -> Unfolding
-- A function that embodies how to unfold an Id if you need
-- to do that in the Rule.  The reason we need to pass this info in
-- is that whether an Id is unfoldable depends on the simplifier phase

isBuiltinRule :: CoreRule -> Bool
isBuiltinRule (BuiltinRule {}) = True
isBuiltinRule _                = False

isAutoRule :: CoreRule -> Bool
isAutoRule (BuiltinRule {}) = False
isAutoRule (Rule { ru_auto = is_auto }) = is_auto

-- | The number of arguments the 'ru_fn' must be applied
-- to before the rule can match on it
ruleArity :: CoreRule -> Int
ruleArity (BuiltinRule {ru_nargs = n}) = n
ruleArity (Rule {ru_args = args})      = length args

ruleName :: CoreRule -> RuleName
ruleName = ru_name

ruleModule :: CoreRule -> Maybe Module
ruleModule Rule { ru_origin } = Just ru_origin
ruleModule BuiltinRule {} = Nothing

ruleActivation :: CoreRule -> Activation
ruleActivation (BuiltinRule { })       = AlwaysActive
ruleActivation (Rule { ru_act = act }) = act

-- | The 'Name' of the 'Id.Id' at the head of the rule left hand side
ruleIdName :: CoreRule -> Name
ruleIdName = ru_fn

isLocalRule :: CoreRule -> Bool
isLocalRule = ru_local

-- | Set the 'Name' of the 'Id.Id' at the head of the rule left hand side
setRuleIdName :: Name -> CoreRule -> CoreRule
setRuleIdName nm ru = ru { ru_fn = nm }

{-
************************************************************************
*                                                                      *
\subsection{Vectorisation declarations}
*                                                                      *
************************************************************************

Representation of desugared vectorisation declarations that are fed to the vectoriser (via
'ModGuts').
-}

data CoreVect = Vect      Id   CoreExpr
              | NoVect    Id
              | VectType  Bool TyCon (Maybe TyCon)
              | VectClass TyCon                     -- class tycon
              | VectInst  Id                        -- instance dfun (always SCALAR)  !!!FIXME: should be superfluous now

{-
************************************************************************
*                                                                      *
                Unfoldings
*                                                                      *
************************************************************************

The @Unfolding@ type is declared here to avoid numerous loops
-}

-- | Records the /unfolding/ of an identifier, which is approximately the form the
-- identifier would have if we substituted its definition in for the identifier.
-- This type should be treated as abstract everywhere except in "CoreUnfold"
data Unfolding
  = NoUnfolding        -- ^ We have no information about the unfolding.

  | BootUnfolding      -- ^ We have no information about the unfolding, because
                       -- this 'Id' came from an @hi-boot@ file.
                       -- See Note [Inlining and hs-boot files] in ToIface
                       -- for what this is used for.

  | OtherCon [AltCon]  -- ^ It ain't one of these constructors.
                       -- @OtherCon xs@ also indicates that something has been evaluated
                       -- and hence there's no point in re-evaluating it.
                       -- @OtherCon []@ is used even for non-data-type values
                       -- to indicated evaluated-ness.  Notably:
                       --
                       -- > data C = C !(Int -> Int)
                       -- > case x of { C f -> ... }
                       --
                       -- Here, @f@ gets an @OtherCon []@ unfolding.

  | DFunUnfolding {     -- The Unfolding of a DFunId
                        -- See Note [DFun unfoldings]
                        --     df = /\a1..am. \d1..dn. MkD t1 .. tk
                        --                                 (op1 a1..am d1..dn)
                        --                                 (op2 a1..am d1..dn)
        df_bndrs :: [Var],      -- The bound variables [a1..m],[d1..dn]
        df_con   :: DataCon,    -- The dictionary data constructor (never a newtype datacon)
        df_args  :: [CoreExpr]  -- Args of the data con: types, superclasses and methods,
    }                           -- in positional order

  | CoreUnfolding {             -- An unfolding for an Id with no pragma,
                                -- or perhaps a NOINLINE pragma
                                -- (For NOINLINE, the phase, if any, is in the
                                -- InlinePragInfo for this Id.)
        uf_tmpl       :: CoreExpr,        -- Template; occurrence info is correct
        uf_src        :: UnfoldingSource, -- Where the unfolding came from
        uf_is_top     :: Bool,          -- True <=> top level binding
        uf_is_value   :: Bool,          -- exprIsHNF template (cached); it is ok to discard
                                        --      a `seq` on this variable
        uf_is_conlike :: Bool,          -- True <=> applicn of constructor or CONLIKE function
                                        --      Cached version of exprIsConLike
        uf_is_work_free :: Bool,                -- True <=> doesn't waste (much) work to expand
                                        --          inside an inlining
                                        --      Cached version of exprIsCheap
        uf_expandable :: Bool,          -- True <=> can expand in RULE matching
                                        --      Cached version of exprIsExpandable
        uf_guidance   :: UnfoldingGuidance      -- Tells about the *size* of the template.
    }
  -- ^ An unfolding with redundant cached information. Parameters:
  --
  --  uf_tmpl: Template used to perform unfolding;
  --           NB: Occurrence info is guaranteed correct:
  --               see Note [OccInfo in unfoldings and rules]
  --
  --  uf_is_top: Is this a top level binding?
  --
  --  uf_is_value: 'exprIsHNF' template (cached); it is ok to discard a 'seq' on
  --     this variable
  --
  --  uf_is_work_free:  Does this waste only a little work if we expand it inside an inlining?
  --     Basically this is a cached version of 'exprIsWorkFree'
  --
  --  uf_guidance:  Tells us about the /size/ of the unfolding template


------------------------------------------------
data UnfoldingSource
  = -- See also Note [Historical note: unfoldings for wrappers]

    InlineRhs          -- The current rhs of the function
                       -- Replace uf_tmpl each time around

  | InlineStable       -- From an INLINE or INLINABLE pragma
                       --   INLINE     if guidance is UnfWhen
                       --   INLINABLE  if guidance is UnfIfGoodArgs/UnfoldNever
                       -- (well, technically an INLINABLE might be made
                       -- UnfWhen if it was small enough, and then
                       -- it will behave like INLINE outside the current
                       -- module, but that is the way automatic unfoldings
                       -- work so it is consistent with the intended
                       -- meaning of INLINABLE).
                       --
                       -- uf_tmpl may change, but only as a result of
                       -- gentle simplification, it doesn't get updated
                       -- to the current RHS during compilation as with
                       -- InlineRhs.
                       --
                       -- See Note [InlineStable]

  | InlineCompulsory   -- Something that *has* no binding, so you *must* inline it
                       -- Only a few primop-like things have this property
                       -- (see MkId.hs, calls to mkCompulsoryUnfolding).
                       -- Inline absolutely always, however boring the context.



-- | 'UnfoldingGuidance' says when unfolding should take place
data UnfoldingGuidance
  = UnfWhen {   -- Inline without thinking about the *size* of the uf_tmpl
                -- Used (a) for small *and* cheap unfoldings
                --      (b) for INLINE functions
                -- See Note [INLINE for small functions] in CoreUnfold
      ug_arity    :: Arity,     -- Number of value arguments expected

      ug_unsat_ok  :: Bool,     -- True <=> ok to inline even if unsaturated
      ug_boring_ok :: Bool      -- True <=> ok to inline even if the context is boring
                -- So True,True means "always"
    }

  | UnfIfGoodArgs {     -- Arose from a normal Id; the info here is the
                        -- result of a simple analysis of the RHS

      ug_args ::  [Int],  -- Discount if the argument is evaluated.
                          -- (i.e., a simplification will definitely
                          -- be possible).  One elt of the list per *value* arg.

      ug_size :: Int,     -- The "size" of the unfolding.

      ug_res :: Int       -- Scrutinee discount: the discount to substract if the thing is in
    }                     -- a context (case (thing args) of ...),
                          -- (where there are the right number of arguments.)

  | UnfNever        -- The RHS is big, so don't inline it
  deriving (Eq)

{-
Note [Historical note: unfoldings for wrappers]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We used to have a nice clever scheme in interface files for
wrappers. A wrapper's unfolding can be reconstructed from its worker's
id and its strictness. This decreased .hi file size (sometimes
significantly, for modules like GHC.Classes with many high-arity w/w
splits) and had a slight corresponding effect on compile times.

However, when we added the second demand analysis, this scheme lead to
some Core lint errors. The second analysis could change the strictness
signatures, which sometimes resulted in a wrapper's regenerated
unfolding applying the wrapper to too many arguments.

Instead of repairing the clever .hi scheme, we abandoned it in favor
of simplicity. The .hi sizes are usually insignificant (excluding the
+1M for base libraries), and compile time barely increases (~+1% for
nofib). The nicer upshot is that the UnfoldingSource no longer mentions
an Id, so, eg, substitutions need not traverse them.


Note [DFun unfoldings]
~~~~~~~~~~~~~~~~~~~~~~
The Arity in a DFunUnfolding is total number of args (type and value)
that the DFun needs to produce a dictionary.  That's not necessarily
related to the ordinary arity of the dfun Id, esp if the class has
one method, so the dictionary is represented by a newtype.  Example

     class C a where { op :: a -> Int }
     instance C a -> C [a] where op xs = op (head xs)

The instance translates to

     $dfCList :: forall a. C a => C [a]  -- Arity 2!
     $dfCList = /\a.\d. $copList {a} d |> co

     $copList :: forall a. C a => [a] -> Int  -- Arity 2!
     $copList = /\a.\d.\xs. op {a} d (head xs)

Now we might encounter (op (dfCList {ty} d) a1 a2)
and we want the (op (dfList {ty} d)) rule to fire, because $dfCList
has all its arguments, even though its (value) arity is 2.  That's
why we record the number of expected arguments in the DFunUnfolding.

Note that although it's an Arity, it's most convenient for it to give
the *total* number of arguments, both type and value.  See the use
site in exprIsConApp_maybe.
-}

-- Constants for the UnfWhen constructor
needSaturated, unSaturatedOk :: Bool
needSaturated = False
unSaturatedOk = True

boringCxtNotOk, boringCxtOk :: Bool
boringCxtOk    = True
boringCxtNotOk = False

------------------------------------------------
noUnfolding :: Unfolding
-- ^ There is no known 'Unfolding'
evaldUnfolding :: Unfolding
-- ^ This unfolding marks the associated thing as being evaluated

noUnfolding    = NoUnfolding
evaldUnfolding = OtherCon []

-- | There is no known 'Unfolding', because this came from an
-- hi-boot file.
bootUnfolding :: Unfolding
bootUnfolding = BootUnfolding

mkOtherCon :: [AltCon] -> Unfolding
mkOtherCon = OtherCon

isStableSource :: UnfoldingSource -> Bool
-- Keep the unfolding template
isStableSource InlineCompulsory   = True
isStableSource InlineStable       = True
isStableSource InlineRhs          = False

-- | Retrieves the template of an unfolding: panics if none is known
unfoldingTemplate :: Unfolding -> CoreExpr
unfoldingTemplate = uf_tmpl

-- | Retrieves the template of an unfolding if possible
-- maybeUnfoldingTemplate is used mainly wnen specialising, and we do
-- want to specialise DFuns, so it's important to return a template
-- for DFunUnfoldings
maybeUnfoldingTemplate :: Unfolding -> Maybe CoreExpr
maybeUnfoldingTemplate (CoreUnfolding { uf_tmpl = expr })
  = Just expr
maybeUnfoldingTemplate (DFunUnfolding { df_bndrs = bndrs, df_con = con, df_args = args })
  = Just (mkLams bndrs (mkApps (Var (dataConWorkId con)) args))
maybeUnfoldingTemplate _
  = Nothing

-- | The constructors that the unfolding could never be:
-- returns @[]@ if no information is available
otherCons :: Unfolding -> [AltCon]
otherCons (OtherCon cons) = cons
otherCons _               = []

-- | Determines if it is certainly the case that the unfolding will
-- yield a value (something in HNF): returns @False@ if unsure
isValueUnfolding :: Unfolding -> Bool
        -- Returns False for OtherCon
isValueUnfolding (CoreUnfolding { uf_is_value = is_evald }) = is_evald
isValueUnfolding _                                          = False

-- | Determines if it possibly the case that the unfolding will
-- yield a value. Unlike 'isValueUnfolding' it returns @True@
-- for 'OtherCon'
isEvaldUnfolding :: Unfolding -> Bool
        -- Returns True for OtherCon
isEvaldUnfolding (OtherCon _)                               = True
isEvaldUnfolding (CoreUnfolding { uf_is_value = is_evald }) = is_evald
isEvaldUnfolding _                                          = False

-- | @True@ if the unfolding is a constructor application, the application
-- of a CONLIKE function or 'OtherCon'
isConLikeUnfolding :: Unfolding -> Bool
isConLikeUnfolding (OtherCon _)                             = True
isConLikeUnfolding (CoreUnfolding { uf_is_conlike = con })  = con
isConLikeUnfolding _                                        = False

-- | Is the thing we will unfold into certainly cheap?
isCheapUnfolding :: Unfolding -> Bool
isCheapUnfolding (CoreUnfolding { uf_is_work_free = is_wf }) = is_wf
isCheapUnfolding _                                           = False

isExpandableUnfolding :: Unfolding -> Bool
isExpandableUnfolding (CoreUnfolding { uf_expandable = is_expable }) = is_expable
isExpandableUnfolding _                                              = False

expandUnfolding_maybe :: Unfolding -> Maybe CoreExpr
-- Expand an expandable unfolding; this is used in rule matching
--   See Note [Expanding variables] in Rules.hs
-- The key point here is that CONLIKE things can be expanded
expandUnfolding_maybe (CoreUnfolding { uf_expandable = True, uf_tmpl = rhs }) = Just rhs
expandUnfolding_maybe _                                                       = Nothing

isCompulsoryUnfolding :: Unfolding -> Bool
isCompulsoryUnfolding (CoreUnfolding { uf_src = InlineCompulsory }) = True
isCompulsoryUnfolding _                                             = False

isStableUnfolding :: Unfolding -> Bool
-- True of unfoldings that should not be overwritten
-- by a CoreUnfolding for the RHS of a let-binding
isStableUnfolding (CoreUnfolding { uf_src = src }) = isStableSource src
isStableUnfolding (DFunUnfolding {})               = True
isStableUnfolding _                                = False

-- | Only returns False if there is no unfolding information available at all
hasSomeUnfolding :: Unfolding -> Bool
hasSomeUnfolding NoUnfolding   = False
hasSomeUnfolding BootUnfolding = False
hasSomeUnfolding _             = True

isBootUnfolding :: Unfolding -> Bool
isBootUnfolding BootUnfolding = True
isBootUnfolding _             = False

neverUnfoldGuidance :: UnfoldingGuidance -> Bool
neverUnfoldGuidance UnfNever = True
neverUnfoldGuidance _        = False

isFragileUnfolding :: Unfolding -> Bool
-- An unfolding is fragile if it mentions free variables or
-- is otherwise subject to change.  A robust one can be kept.
-- See Note [Fragile unfoldings]
isFragileUnfolding (CoreUnfolding {}) = True
isFragileUnfolding (DFunUnfolding {}) = True
isFragileUnfolding _                  = False
  -- NoUnfolding, BootUnfolding, OtherCon are all non-fragile

canUnfold :: Unfolding -> Bool
canUnfold (CoreUnfolding { uf_guidance = g }) = not (neverUnfoldGuidance g)
canUnfold _                                   = False

{- Note [Fragile unfoldings]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
An unfolding is "fragile" if it mentions free variables (and hence would
need substitution) or might be affected by optimisation.  The non-fragile
ones are

   NoUnfolding, BootUnfolding

   OtherCon {}    If we know this binder (say a lambda binder) will be
                  bound to an evaluated thing, we want to retain that
                  info in simpleOptExpr; see Trac #13077.

We consider even a StableUnfolding as fragile, because it needs substitution.

Note [InlineStable]
~~~~~~~~~~~~~~~~~
When you say
      {-# INLINE f #-}
      f x = <rhs>
you intend that calls (f e) are replaced by <rhs>[e/x] So we
should capture (\x.<rhs>) in the Unfolding of 'f', and never meddle
with it.  Meanwhile, we can optimise <rhs> to our heart's content,
leaving the original unfolding intact in Unfolding of 'f'. For example
        all xs = foldr (&&) True xs
        any p = all . map p  {-# INLINE any #-}
We optimise any's RHS fully, but leave the InlineRule saying "all . map p",
which deforests well at the call site.

So INLINE pragma gives rise to an InlineRule, which captures the original RHS.

Moreover, it's only used when 'f' is applied to the
specified number of arguments; that is, the number of argument on
the LHS of the '=' sign in the original source definition.
For example, (.) is now defined in the libraries like this
   {-# INLINE (.) #-}
   (.) f g = \x -> f (g x)
so that it'll inline when applied to two arguments. If 'x' appeared
on the left, thus
   (.) f g x = f (g x)
it'd only inline when applied to three arguments.  This slightly-experimental
change was requested by Roman, but it seems to make sense.

See also Note [Inlining an InlineRule] in CoreUnfold.


Note [OccInfo in unfoldings and rules]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In unfoldings and rules, we guarantee that the template is occ-analysed,
so that the occurrence info on the binders is correct.  This is important,
because the Simplifier does not re-analyse the template when using it. If
the occurrence info is wrong
  - We may get more simplifier iterations than necessary, because
    once-occ info isn't there
  - More seriously, we may get an infinite loop if there's a Rec
    without a loop breaker marked


************************************************************************
*                                                                      *
                  AltCon
*                                                                      *
************************************************************************
-}

-- The Ord is needed for the FiniteMap used in the lookForConstructor
-- in SimplEnv.  If you declared that lookForConstructor *ignores*
-- constructor-applications with LitArg args, then you could get
-- rid of this Ord.

instance Outputable AltCon where
  ppr (DataAlt dc) = ppr dc
  ppr (LitAlt lit) = ppr lit
  ppr DEFAULT      = text "__DEFAULT"

cmpAlt :: (AltCon, a, b) -> (AltCon, a, b) -> Ordering
cmpAlt (con1, _, _) (con2, _, _) = con1 `cmpAltCon` con2

ltAlt :: (AltCon, a, b) -> (AltCon, a, b) -> Bool
ltAlt a1 a2 = (a1 `cmpAlt` a2) == LT

cmpAltCon :: AltCon -> AltCon -> Ordering
-- ^ Compares 'AltCon's within a single list of alternatives
-- DEFAULT comes out smallest, so that sorting by AltCon
-- puts alternatives in the order required by #case_invariants#
cmpAltCon DEFAULT      DEFAULT     = EQ
cmpAltCon DEFAULT      _           = LT

cmpAltCon (DataAlt d1) (DataAlt d2) = dataConTag d1 `compare` dataConTag d2
cmpAltCon (DataAlt _)  DEFAULT      = GT
cmpAltCon (LitAlt  l1) (LitAlt  l2) = l1 `compare` l2
cmpAltCon (LitAlt _)   DEFAULT      = GT

cmpAltCon con1 con2 = WARN( True, text "Comparing incomparable AltCons" <+>
                                  ppr con1 <+> ppr con2 )
                      LT

{-
************************************************************************
*                                                                      *
\subsection{Useful synonyms}
*                                                                      *
************************************************************************

Note [CoreProgram]
~~~~~~~~~~~~~~~~~~
The top level bindings of a program, a CoreProgram, are represented as
a list of CoreBind

 * Later bindings in the list can refer to earlier ones, but not vice
   versa.  So this is OK
      NonRec { x = 4 }
      Rec { p = ...q...x...
          ; q = ...p...x }
      Rec { f = ...p..x..f.. }
      NonRec { g = ..f..q...x.. }
   But it would NOT be ok for 'f' to refer to 'g'.

 * The occurrence analyser does strongly-connected component analysis
   on each Rec binding, and splits it into a sequence of smaller
   bindings where possible.  So the program typically starts life as a
   single giant Rec, which is then dependency-analysed into smaller
   chunks.
-}

-- If you edit this type, you may need to update the GHC formalism
-- See Note [GHC Formalism] in coreSyn/CoreLint.hs
type CoreProgram = [CoreBind]   -- See Note [CoreProgram]

-- | The common case for the type of binders and variables when
-- we are manipulating the Core language within GHC
type CoreBndr = Var
-- | Expressions where binders are 'CoreBndr's
type CoreExpr = Expr CoreBndr
-- | Argument expressions where binders are 'CoreBndr's
type CoreArg  = Arg  CoreBndr
-- | Binding groups where binders are 'CoreBndr's
type CoreBind = Bind CoreBndr
-- | Case alternatives where binders are 'CoreBndr's
type CoreAlt  = Alt  CoreBndr

{-
************************************************************************
*                                                                      *
\subsection{Tagging}
*                                                                      *
************************************************************************
-}

-- | Binders are /tagged/ with a t
data TaggedBndr t = TB CoreBndr t       -- TB for "tagged binder"

type TaggedBind t = Bind (TaggedBndr t)
type TaggedExpr t = Expr (TaggedBndr t)
type TaggedArg  t = Arg  (TaggedBndr t)
type TaggedAlt  t = Alt  (TaggedBndr t)

instance Outputable b => Outputable (TaggedBndr b) where
  ppr (TB b l) = char '<' <> ppr b <> comma <> ppr l <> char '>'

deTagExpr :: TaggedExpr t -> CoreExpr
deTagExpr (Var v)                   = Var v
deTagExpr (Lit l)                   = Lit l
deTagExpr (Type ty)                 = Type ty
deTagExpr (Coercion co)             = Coercion co
deTagExpr (App e1 e2)               = App (deTagExpr e1) (deTagExpr e2)
deTagExpr (Lam (TB b _) e)          = Lam b (deTagExpr e)
deTagExpr (Let bind body)           = Let (deTagBind bind) (deTagExpr body)
deTagExpr (Case e (TB b _) ty alts) = Case (deTagExpr e) b ty (map deTagAlt alts)
deTagExpr (Tick t e)                = Tick t (deTagExpr e)
deTagExpr (Cast e co)               = Cast (deTagExpr e) co

deTagBind :: TaggedBind t -> CoreBind
deTagBind (NonRec (TB b _) rhs) = NonRec b (deTagExpr rhs)
deTagBind (Rec prs)             = Rec [(b, deTagExpr rhs) | (TB b _, rhs) <- prs]

deTagAlt :: TaggedAlt t -> CoreAlt
deTagAlt (con, bndrs, rhs) = (con, [b | TB b _ <- bndrs], deTagExpr rhs)

{-
************************************************************************
*                                                                      *
\subsection{Core-constructing functions with checking}
*                                                                      *
************************************************************************
-}

-- | Apply a list of argument expressions to a function expression in a nested fashion. Prefer to
-- use 'MkCore.mkCoreApps' if possible
mkApps    :: Expr b -> [Arg b]  -> Expr b
-- | Apply a list of type argument expressions to a function expression in a nested fashion
mkTyApps  :: Expr b -> [Type]   -> Expr b
-- | Apply a list of coercion argument expressions to a function expression in a nested fashion
mkCoApps  :: Expr b -> [Coercion] -> Expr b
-- | Apply a list of type or value variables to a function expression in a nested fashion
mkVarApps :: Expr b -> [Var] -> Expr b
-- | Apply a list of argument expressions to a data constructor in a nested fashion. Prefer to
-- use 'MkCore.mkCoreConApps' if possible
mkConApp      :: DataCon -> [Arg b] -> Expr b

mkApps    f args = foldl App                       f args
mkCoApps  f args = foldl (\ e a -> App e (Coercion a)) f args
mkVarApps f vars = foldl (\ e a -> App e (varToCoreExpr a)) f vars
mkConApp con args = mkApps (Var (dataConWorkId con)) args

mkTyApps  f args = foldl (\ e a -> App e (mkTyArg a)) f args

mkConApp2 :: DataCon -> [Type] -> [Var] -> Expr b
mkConApp2 con tys arg_ids = Var (dataConWorkId con)
                            `mkApps` map Type tys
                            `mkApps` map varToCoreExpr arg_ids

mkTyArg :: Type -> Expr b
mkTyArg ty
  | Just co <- isCoercionTy_maybe ty = Coercion co
  | otherwise                        = Type ty

-- | Create a machine integer literal expression of type @Int#@ from an @Integer@.
-- If you want an expression of type @Int@ use 'MkCore.mkIntExpr'
mkIntLit      :: DynFlags -> Integer -> Expr b
-- | Create a machine integer literal expression of type @Int#@ from an @Int@.
-- If you want an expression of type @Int@ use 'MkCore.mkIntExpr'
mkIntLitInt   :: DynFlags -> Int     -> Expr b

mkIntLit    dflags n = Lit (mkMachInt dflags n)
mkIntLitInt dflags n = Lit (mkMachInt dflags (toInteger n))

-- | Create a machine word literal expression of type  @Word#@ from an @Integer@.
-- If you want an expression of type @Word@ use 'MkCore.mkWordExpr'
mkWordLit     :: DynFlags -> Integer -> Expr b
-- | Create a machine word literal expression of type  @Word#@ from a @Word@.
-- If you want an expression of type @Word@ use 'MkCore.mkWordExpr'
mkWordLitWord :: DynFlags -> Word -> Expr b

mkWordLit     dflags w = Lit (mkMachWord dflags w)
mkWordLitWord dflags w = Lit (mkMachWord dflags (toInteger w))

mkWord64LitWord64 :: Word64 -> Expr b
mkWord64LitWord64 w = Lit (mkMachWord64 (toInteger w))

mkInt64LitInt64 :: Int64 -> Expr b
mkInt64LitInt64 w = Lit (mkMachInt64 (toInteger w))

-- | Create a machine character literal expression of type @Char#@.
-- If you want an expression of type @Char@ use 'MkCore.mkCharExpr'
mkCharLit :: Char -> Expr b
-- | Create a machine string literal expression of type @Addr#@.
-- If you want an expression of type @String@ use 'MkCore.mkStringExpr'
mkStringLit :: String -> Expr b

mkCharLit   c = Lit (mkMachChar c)
mkStringLit s = Lit (mkMachString s)

-- | Create a machine single precision literal expression of type @Float#@ from a @Rational@.
-- If you want an expression of type @Float@ use 'MkCore.mkFloatExpr'
mkFloatLit :: Rational -> Expr b
-- | Create a machine single precision literal expression of type @Float#@ from a @Float@.
-- If you want an expression of type @Float@ use 'MkCore.mkFloatExpr'
mkFloatLitFloat :: Float -> Expr b

mkFloatLit      f = Lit (mkMachFloat f)
mkFloatLitFloat f = Lit (mkMachFloat (toRational f))

-- | Create a machine double precision literal expression of type @Double#@ from a @Rational@.
-- If you want an expression of type @Double@ use 'MkCore.mkDoubleExpr'
mkDoubleLit :: Rational -> Expr b
-- | Create a machine double precision literal expression of type @Double#@ from a @Double@.
-- If you want an expression of type @Double@ use 'MkCore.mkDoubleExpr'
mkDoubleLitDouble :: Double -> Expr b

mkDoubleLit       d = Lit (mkMachDouble d)
mkDoubleLitDouble d = Lit (mkMachDouble (toRational d))

-- | Bind all supplied binding groups over an expression in a nested let expression. Assumes
-- that the rhs satisfies the let/app invariant.  Prefer to use 'MkCore.mkCoreLets' if
-- possible, which does guarantee the invariant
mkLets        :: [Bind b] -> Expr b -> Expr b
-- | Bind all supplied binders over an expression in a nested lambda expression. Prefer to
-- use 'MkCore.mkCoreLams' if possible
mkLams        :: [b] -> Expr b -> Expr b

mkLams binders body = foldr Lam body binders
mkLets binds body   = foldr mkLet body binds

mkLet :: Bind b -> Expr b -> Expr b
-- The desugarer sometimes generates an empty Rec group
-- which Lint rejects, so we kill it off right away
mkLet (Rec []) body = body
mkLet bind     body = Let bind body

-- | @mkLetNonRec bndr rhs body@ wraps @body@ in a @let@ binding @bndr@.
mkLetNonRec :: b -> Expr b -> Expr b -> Expr b
mkLetNonRec b rhs body = Let (NonRec b rhs) body

-- | @mkLetRec binds body@ wraps @body@ in a @let rec@ with the given set of
-- @binds@ if binds is non-empty.
mkLetRec :: [(b, Expr b)] -> Expr b -> Expr b
mkLetRec [] body = body
mkLetRec bs body = Let (Rec bs) body

-- | Create a binding group where a type variable is bound to a type. Per "CoreSyn#type_let",
-- this can only be used to bind something in a non-recursive @let@ expression
mkTyBind :: TyVar -> Type -> CoreBind
mkTyBind tv ty      = NonRec tv (Type ty)

-- | Create a binding group where a type variable is bound to a type. Per "CoreSyn#type_let",
-- this can only be used to bind something in a non-recursive @let@ expression
mkCoBind :: CoVar -> Coercion -> CoreBind
mkCoBind cv co      = NonRec cv (Coercion co)

-- | Convert a binder into either a 'Var' or 'Type' 'Expr' appropriately
varToCoreExpr :: CoreBndr -> Expr b
varToCoreExpr v | isTyVar v = Type (mkTyVarTy v)
                | isCoVar v = Coercion (mkCoVarCo v)
                | otherwise = ASSERT( isId v ) Var v

varsToCoreExprs :: [CoreBndr] -> [Expr b]
varsToCoreExprs vs = map varToCoreExpr vs

{-
************************************************************************
*                                                                      *
   Getting a result type
*                                                                      *
************************************************************************

These are defined here to avoid a module loop between CoreUtils and CoreFVs

-}

applyTypeToArg :: Type -> CoreExpr -> Type
-- ^ Determines the type resulting from applying an expression with given type
-- to a given argument expression
applyTypeToArg fun_ty arg = piResultTy fun_ty (exprToType arg)

-- | If the expression is a 'Type', converts. Otherwise,
-- panics. NB: This does /not/ convert 'Coercion' to 'CoercionTy'.
exprToType :: CoreExpr -> Type
exprToType (Type ty)     = ty
exprToType _bad          = pprPanic "exprToType" empty

-- | If the expression is a 'Coercion', converts.
exprToCoercion_maybe :: CoreExpr -> Maybe Coercion
exprToCoercion_maybe (Coercion co) = Just co
exprToCoercion_maybe _             = Nothing

{-
************************************************************************
*                                                                      *
\subsection{Simple access functions}
*                                                                      *
************************************************************************
-}

-- | Extract every variable by this group
bindersOf  :: Bind b -> [b]
-- If you edit this function, you may need to update the GHC formalism
-- See Note [GHC Formalism] in coreSyn/CoreLint.hs
bindersOf (NonRec binder _) = [binder]
bindersOf (Rec pairs)       = [binder | (binder, _) <- pairs]

-- | 'bindersOf' applied to a list of binding groups
bindersOfBinds :: [Bind b] -> [b]
bindersOfBinds binds = foldr ((++) . bindersOf) [] binds

rhssOfBind :: Bind b -> [Expr b]
rhssOfBind (NonRec _ rhs) = [rhs]
rhssOfBind (Rec pairs)    = [rhs | (_,rhs) <- pairs]

rhssOfAlts :: [Alt b] -> [Expr b]
rhssOfAlts alts = [e | (_,_,e) <- alts]

-- | Collapse all the bindings in the supplied groups into a single
-- list of lhs\/rhs pairs suitable for binding in a 'Rec' binding group
flattenBinds :: [Bind b] -> [(b, Expr b)]
flattenBinds (NonRec b r : binds) = (b,r) : flattenBinds binds
flattenBinds (Rec prs1   : binds) = prs1 ++ flattenBinds binds
flattenBinds []                   = []

-- | We often want to strip off leading lambdas before getting down to
-- business. Variants are 'collectTyBinders', 'collectValBinders',
-- and 'collectTyAndValBinders'
collectBinders         :: Expr b   -> ([b],     Expr b)
collectTyBinders       :: CoreExpr -> ([TyVar], CoreExpr)
collectValBinders      :: CoreExpr -> ([Id],    CoreExpr)
collectTyAndValBinders :: CoreExpr -> ([TyVar], [Id], CoreExpr)
-- | Strip off exactly N leading lambdas (type or value). Good for use with
-- join points.
collectNBinders        :: Int -> Expr b -> ([b], Expr b)

collectBinders expr
  = go [] expr
  where
    go bs (Lam b e) = go (b:bs) e
    go bs e          = (reverse bs, e)

collectTyBinders expr
  = go [] expr
  where
    go tvs (Lam b e) | isTyVar b = go (b:tvs) e
    go tvs e                     = (reverse tvs, e)

collectValBinders expr
  = go [] expr
  where
    go ids (Lam b e) | isId b = go (b:ids) e
    go ids body               = (reverse ids, body)

collectTyAndValBinders expr
  = (tvs, ids, body)
  where
    (tvs, body1) = collectTyBinders expr
    (ids, body)  = collectValBinders body1

collectNBinders orig_n orig_expr
  = go orig_n [] orig_expr
  where
    go 0 bs expr      = (reverse bs, expr)
    go n bs (Lam b e) = go (n-1) (b:bs) e
    go _ _  _         = pprPanic "collectNBinders" $ int orig_n

-- | Takes a nested application expression and returns the the function
-- being applied and the arguments to which it is applied
collectArgs :: Expr b -> (Expr b, [Arg b])
collectArgs expr
  = go expr []
  where
    go (App f a) as = go f (a:as)
    go e         as = (e, as)

-- | Like @collectArgs@, but also collects looks through floatable
-- ticks if it means that we can find more arguments.
collectArgsTicks :: (Tickish Id -> Bool) -> Expr b
                 -> (Expr b, [Arg b], [Tickish Id])
collectArgsTicks skipTick expr
  = go expr [] []
  where
    go (App f a)  as ts = go f (a:as) ts
    go (Tick t e) as ts
      | skipTick t      = go e as (t:ts)
    go e          as ts = (e, as, reverse ts)


{-
************************************************************************
*                                                                      *
\subsection{Predicates}
*                                                                      *
************************************************************************

At one time we optionally carried type arguments through to runtime.
@isRuntimeVar v@ returns if (Lam v _) really becomes a lambda at runtime,
i.e. if type applications are actual lambdas because types are kept around
at runtime.  Similarly isRuntimeArg.
-}

-- | Will this variable exist at runtime?
isRuntimeVar :: Var -> Bool
isRuntimeVar = isId

-- | Will this argument expression exist at runtime?
isRuntimeArg :: CoreExpr -> Bool
isRuntimeArg = isValArg

-- | Returns @True@ for value arguments, false for type args
-- NB: coercions are value arguments (zero width, to be sure,
-- like State#, but still value args).
isValArg :: Expr b -> Bool
isValArg e = not (isTypeArg e)

-- | Returns @True@ iff the expression is a 'Type' or 'Coercion'
-- expression at its top level
isTyCoArg :: Expr b -> Bool
isTyCoArg (Type {})     = True
isTyCoArg (Coercion {}) = True
isTyCoArg _             = False

-- | Returns @True@ iff the expression is a 'Type' expression at its
-- top level.  Note this does NOT include 'Coercion's.
isTypeArg :: Expr b -> Bool
isTypeArg (Type {}) = True
isTypeArg _         = False

-- | The number of binders that bind values rather than types
valBndrCount :: [CoreBndr] -> Int
valBndrCount = count isId

-- | The number of argument expressions that are values rather than types at their top level
valArgCount :: [Arg b] -> Int
valArgCount = count isValArg

{-
************************************************************************
*                                                                      *
\subsection{Annotated core}
*                                                                      *
************************************************************************
-}

-- | Annotated core: allows annotation at every node in the tree
type AnnExpr bndr annot = (annot, AnnExpr' bndr annot)

-- | A clone of the 'Expr' type but allowing annotation at every tree node
data AnnExpr' bndr annot
  = AnnVar      Id
  | AnnLit      Literal
  | AnnLam      bndr (AnnExpr bndr annot)
  | AnnApp      (AnnExpr bndr annot) (AnnExpr bndr annot)
  | AnnCase     (AnnExpr bndr annot) bndr Type [AnnAlt bndr annot]
  | AnnLet      (AnnBind bndr annot) (AnnExpr bndr annot)
  | AnnCast     (AnnExpr bndr annot) (annot, Coercion)
                   -- Put an annotation on the (root of) the coercion
  | AnnTick     (Tickish Id) (AnnExpr bndr annot)
  | AnnType     Type
  | AnnCoercion Coercion

-- | A clone of the 'Alt' type but allowing annotation at every tree node
type AnnAlt bndr annot = (AltCon, [bndr], AnnExpr bndr annot)

-- | A clone of the 'Bind' type but allowing annotation at every tree node
data AnnBind bndr annot
  = AnnNonRec bndr (AnnExpr bndr annot)
  | AnnRec    [(bndr, AnnExpr bndr annot)]

-- | Takes a nested application expression and returns the the function
-- being applied and the arguments to which it is applied
collectAnnArgs :: AnnExpr b a -> (AnnExpr b a, [AnnExpr b a])
collectAnnArgs expr
  = go expr []
  where
    go (_, AnnApp f a) as = go f (a:as)
    go e               as = (e, as)

collectAnnArgsTicks :: (Tickish Var -> Bool) -> AnnExpr b a
                       -> (AnnExpr b a, [AnnExpr b a], [Tickish Var])
collectAnnArgsTicks tickishOk expr
  = go expr [] []
  where
    go (_, AnnApp f a)  as ts = go f (a:as) ts
    go (_, AnnTick t e) as ts | tickishOk t
                              = go e as (t:ts)
    go e                as ts = (e, as, reverse ts)

deAnnotate :: AnnExpr bndr annot -> Expr bndr
deAnnotate (_, e) = deAnnotate' e

deAnnotate' :: AnnExpr' bndr annot -> Expr bndr
deAnnotate' (AnnType t)           = Type t
deAnnotate' (AnnCoercion co)      = Coercion co
deAnnotate' (AnnVar  v)           = Var v
deAnnotate' (AnnLit  lit)         = Lit lit
deAnnotate' (AnnLam  binder body) = Lam binder (deAnnotate body)
deAnnotate' (AnnApp  fun arg)     = App (deAnnotate fun) (deAnnotate arg)
deAnnotate' (AnnCast e (_,co))    = Cast (deAnnotate e) co
deAnnotate' (AnnTick tick body)   = Tick tick (deAnnotate body)

deAnnotate' (AnnLet bind body)
  = Let (deAnnBind bind) (deAnnotate body)
deAnnotate' (AnnCase scrut v t alts)
  = Case (deAnnotate scrut) v t (map deAnnAlt alts)

deAnnAlt :: AnnAlt bndr annot -> Alt bndr
deAnnAlt (con,args,rhs) = (con,args,deAnnotate rhs)

deAnnBind  :: AnnBind b annot -> Bind b
deAnnBind (AnnNonRec var rhs) = NonRec var (deAnnotate rhs)
deAnnBind (AnnRec pairs) = Rec [(v,deAnnotate rhs) | (v,rhs) <- pairs]

-- | As 'collectBinders' but for 'AnnExpr' rather than 'Expr'
collectAnnBndrs :: AnnExpr bndr annot -> ([bndr], AnnExpr bndr annot)
collectAnnBndrs e
  = collect [] e
  where
    collect bs (_, AnnLam b body) = collect (b:bs) body
    collect bs body               = (reverse bs, body)

-- | As 'collectNBinders' but for 'AnnExpr' rather than 'Expr'
collectNAnnBndrs :: Int -> AnnExpr bndr annot -> ([bndr], AnnExpr bndr annot)
collectNAnnBndrs orig_n e
  = collect orig_n [] e
  where
    collect 0 bs body               = (reverse bs, body)
    collect n bs (_, AnnLam b body) = collect (n-1) (b:bs) body
    collect _ _  _                  = pprPanic "collectNBinders" $ int orig_n
