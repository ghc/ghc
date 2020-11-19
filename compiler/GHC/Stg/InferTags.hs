--
-- Copyright (c) 2019 Andreas Klebinger
--

{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-# OPTIONS_GHC -O2 -ddump-simpl -ddump-to-file -ddump-stg -ddump-cmm -ddump-asm -ddump-stg-final #-}
{-# OPTIONS_GHC -dsuppress-coercions -dno-suppress-type-signatures -dno-suppress-module-prefixes #-}
-- {-# OPTIONS_GHC -fprof-auto #-}
{-# OPTIONS_GHC -ticky -ticky-allocd -ticky-dyn-thunk #-}

module GHC.Stg.InferTags (findTags, EnterLattice) where

#include "HsVersions.h"

import GHC.Prelude

import GHC.Types.Basic
import GHC.Utils.Binary hiding (put, get)
import qualified GHC.Utils.Binary as B
import GHC.Core.DataCon
import GHC.Types.Id
import GHC.Utils.Outputable
import GHC.Core (AltCon(..))
import GHC.Unit.Types (Module)
import GHC.Core.TyCon (tyConDataCons)
import GHC.Core.Type
import GHC.Types.Unique.Supply
import GHC.Types.RepType
import GHC.Stg.Syntax as StgSyn hiding (AlwaysEnter)
import GHC.Stg.Utils

import GHC.StgToCmm.Types ( LambdaFormInfo(..) )
import GHC.Types.Name
import GHC.Builtin.Names
import GHC.Builtin.Types.Prim (addrPrimTy)

import GHC.Types.Demand ( Divergence ( Absent ), splitStrictSig )
import GHC.Types.Unique
import GHC.Types.Unique.FM
import GHC.Utils.Misc
-- import GHC.Utils.Monad.State -- See Note [Useless Bangs]
import GHC.Data.Maybe

import GHC.Utils.Monad
import GHC.Generics
import GHC.Stack

import GHC.Utils.Error
import GHC.Data.Graph.Directed

import GHC.Types.Var.Env

-- Fast comparisons
import GHC.Exts (reallyUnsafePtrEquality#, isTrue#)

-- Used for dumping nodes with -ddump-stg-tag-nodes
import GHC.Driver.Session

import Data.Ord (comparing)

-- import Data.Int
import Control.Applicative hiding (empty)
import Control.Monad

import Control.DeepSeq -- hiding (deepseq)
import System.IO.Unsafe

-- import Control.Monad.Trans.State.Strict

-- import GHC.Utils.IO.Unsafe (inlinePerformIO)
import GHC.Exts (State#, RealWorld, runRW# )
import GHC.IO (IO(..))

-- import System.Mem (getAllocationCounter)

-- import Data.Array.IArray as U
-- import Data.Array.MArray as M
import Data.Array.IO as IOA
import Data.IORef

import GHC.Exts (oneShot)
import GHC.Utils.Panic
import GHC.Driver.Ppr (pprTraceM)
{-

    Note [Debugging Taggedness]
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~

    In practice bugs happen. So I added a few ways to make issues with this code easier to debug.

    * There is a flag to dump the result of this analysis (ddump-stg-tag-nodes).
    I found this immensely helpful during developement.
    * There is a flag to emit runtime checks to confirm the results of taggedness (dtag-inference-checks).
      If we case on a value we expect to be tagged it adds a runtime check for the tag. If there is no tag
      your program will terminate immediately which makes it a lot easier to find the root cause.
    * The data flow graph can be extended with a description making it easier to work with.
      This is disabled by default, enabled by -DDEBUG, and easily changed by defining (or not)
      WITH_NODE_DESC. This has a significant performance impact hence is disabled by default.


    Note [Tag Inferrence - The basic idea]
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    This code has two goals:

    * Ensure constructors with strict fields are only
        allocated with tagged pointers in the strict fields.
    * Infer if a given evaluation site of an id is guaranteed to
        refer to a tagged pointer.

    Id's refered to by a tagged pointer do not have to be entered
    when used in an case expression which has massive performance
    benefits. If we can *infer* that the pointer is tagged without
    a runtime check we can improve performance further. In particular
    for traversals of strict data structures where repeated checks for
    the presence can carry a high cost.

    This Module contains the code for the actual inference and upholding
    the strict field invariant. See Note [The strict field invariant]
    Once computed the infered tag information is stored in the extension
    point of StgApp. This is then used in StgCmmExpr to determine if we
    need to enter an expression.

    This is essentially a form of 0CFA analysis. The correspondence is as follows:

    * Labeling expressions:
        Is done by creating a data flow node for the expression. Nodes are labeled
        by their id's. While labeling we also set up the initial dataflow state which
        is implicit encoded in the `node_result` field for each dataflow node.

        The data flow graph is created initially by nodesTopBinds.

    * Inference rules:
        Inference rules are encoded in the `node_update` field of nodes.
        These functions when executed update the dataflow state based on the existing
        dataflow state. The functions themselves are created in the various `node*`
        functions eg. `nodeLiteral`

        The simpler ones just return a constant result (and hence are only run once).
        More complicated ones might reference current values of other nodes to
        determine their result and can be iterated often.

        All node* functions have explicit documentation describing both how
        data flows between their inputs and outputs, as well as how new
        constraints are generated. But it's fairly informal.

    *  Solving of the constraints:
        Constraints are solved by iterating node_update functions until we reach
        a fixpoint or an iteration limit.

    All inference rules have been crafted such that intermediate results are safe to use.
    This means even if we fail to reach a fixpoint we can use the results gathered up to
    that point safely.

    The kind of information we track has it's own Note [Lattice for tag analysis].

    Once the dataflow analysis has run we extract relevant information from the constraints in
    the rewrite* (e.g. rewriteRhs) functions. This:
    + Updates the extension points where appropriate.
    + Inserts seq where required to uphold the strict field invariant.
      See Note [The strict field invariant].

    I found that commonly 0CFA is represented as having distinct maps
    for variables and labels. Implementation wise we instead
    assign all occurences refering to a local variable the same label.
    Which is done by keeping a mapping of variables to labels. This has
    the advantage that it works even in the presence of shadowing.
    This is beneficial for running the analysis. But does impose some
    overhead during creation of the data flow graph (but overall improves perf).
    Imported ids are simply referenced by their unique instead.


    Note [Useless Bangs]
    ~~~~~~~~~~~~~~~~~~~~

    Ghcs state monad is lazy. So to avoid space leaks I've added bangs
    very liberally in this module. Some are bound to be useless, but this
    still beats having space leaks.

    The only place in this module where we explicitly depend on lazyness is the
    (unused) ty for case alternatives. So there is no harm in excessive bang
    annotations, at least not compared to space leaks.


    Note [The strict field invariant]
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    The code in this module transforms the STG AST such
    that all strict fields will only contain tagged values.

    We do allow two exceptions to this invariant.
    * Functions.
    * Values representing an absentError.

    See also Note [Taggedness of absentError] for more information on
    the later.

    We uphold this invariant by inserting code for evaluation around
    constructor allocation where needed. In practice this means sometimes
    we will turn this RHS:

        StrictTuple foo bar

    into something like:

        case foo of taggedFoo ->
        case bar of taggedBar ->
            StrictTuple taggedFoo taggedBar

    The purpose of this invariant is that it allows us to eliminate
    the tag check when casing on values coming out of strict fields.

    In particular when traversing the strict spine of a data structure
    this eliminates a significant amount of code being executed leading
    to significant speedups. (10% and up for the traversal!)

    This, on it's own, would *not* be a performance improvement.
    However since for most strict constructor arguments we can infer
    if a tag is present or not in practice we do not add a lot of evaluation
    so the overhead is far lower than the payoff.

    Note [Taggedness of absentError]
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    WorkerWrapper -  under certain circumstances - will determine
    that a strict field in a constructor is not used and put a bottom
    expression in there as placeholder which is not supposed to be
    evaluated. See Note [Absent errors] in the WW code for why we do this.

    We have to take great care to avoid evaluating these absentError
    expresions when upholding the strict field invariant. Because
    evaluating them would trigger a panic in the runtime.

    For this purpose we treat bindings representing an absentError value
    as being tagged. We do this with the help of strictness analysis which
    will assign a special kind of divergence value (Absent) to such bindings.

    This is similar to the simplifier treating absentError as a constructor.
    See Note [aBSENT_ERROR_ID] for a description of the simplifier case.

    This is alright, because in the absence of bugs in the rest of GHC any such value
    should never be evaluated, and therefore can be treated as already
    evaluated without affecting correctness.

    This means we might end up with code like this:

        data T a b c = MkT a b !c

        $wf :: a -> b -> Result
        $wf a b = let c = absentError "ww_absent"
                  in  g (MkT a b c)

    Where we know g will not use the strict field c.

    We catch this kind of situation in two ways:
    * Checking the RHS for absentError applications in the current module.
    * Inspecting the strictness of imported ids.

    We check for absentError applications in the current module because:
        * It works with -O0 in which case the demand analyser isn't run.
        * It's more performant to inspect the functions unique than to
          compare the strictness.

    We have to rely on the strictness analysis for imported id's since not
    all ids provide us with an unfolding to inspect.

    If we come across a let binding of absentError we simply treat the binding as
    if it's represented by a tagged pointer.

    Checking the strictness properties for imported ids is important.
    What if we inline $wf into a module B, but don't do so for `c` which might
    get floated out? We have to ensure that c is still treated as tagged.

    Checking the RHS of local functions, and strictness of imported ones is sufficient
    to make this work.
    * Consider c (binding absentError) to be defined in module A
    * $wf to be inlined into module B from module A.

    This gives us four cases to consider:

    A & B not optimized:
        Since ww will not run on either, c will never be generated by the compiler.
    A & B optimized:
        A will be analyzed, the demand analyzer will recognise c as an absent error
        which allows us to treat `c` as tagged inside module B.
    A optimized, B not optimized:
        If B is not optimized then the body of $wf can't end up in B, so this works out.
    A not optimized, B not optimized:
        Since A is not optimised GHC won't expose any unfoldings, so again $wf and c can't
        end up in different modules and things work out.


    Note [nesting Limit]
    ~~~~~~~~~~~~~~~~~~~~

    When analysing a function like `f x = x : f x` we need to widen the result to
    ensure termination. We achieve this by analysing results only up to a depth of
    set by the nestingLimit variable.

    Analysing results nested past a few levels does not convey a real performance
    improvement, but it does affect compile times significantly for certain code.

-}


#if defined(DEBUG)
#define WITH_NODE_DESC
#endif

-- See Note [nesting Limit]
nestingLimit :: Int
nestingLimit = 10

-- Shortcut comparisons if two things reference the same object.
maybeEq :: a -> a -> Bool
maybeEq x1 x2 = isTrue# (reallyUnsafePtrEquality# x1 x2)

-- | Can we guarantee this RhsCon binding will remain a RhsCon?
data IsRhsCon
    = RhsCon        -- ^ Local bindings, nullary constructors.
    | MaybeClosure  -- ^ Top level bindings.
    deriving Eq

-- Grow them trees:

type instance BinderP       'InferTags = Id
type instance XRhsClosure   'InferTags = NoExtFieldSilent
-- Putting IsRhsCon into XRhsCon instead of the data flow node
-- is done for performance reasons.
type instance XRhsCon       'InferTags = (NodeId,IsRhsCon)
type instance XLet          'InferTags = NoExtFieldSilent
type instance XLetNoEscape  'InferTags = NoExtFieldSilent
type instance XStgApp       'InferTags = NodeId
type instance XStgConApp    'InferTags = NodeId

type InferStgTopBinding = GenStgTopBinding 'InferTags
type InferStgBinding    = GenStgBinding    'InferTags
type InferStgExpr       = GenStgExpr       'InferTags
type InferStgRhs        = GenStgRhs        'InferTags
type InferStgAlt        = GenStgAlt        'InferTags

data RecursionKind
    = NoMutRecursion -- ^ No mutual recursion.
    | OtherRecursion -- ^ Potentially mutual recursion
    | NoRecursion
    deriving Eq


{-
    Note [Lattice for tag analysis]
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# The EnterInfo Lattice

We use a lattice for the tag analysis.
The transfer function for nodes is monotonic (towards bot)

Lattice elements are cross products of two sub lattices.

One lattice encodes if we have to enter an object when
we case on it in the form of `case id of ...` and looks
like this:


            UndetEnterInfo
                  |
              MaybeEnter
               /  |  \
              /   |   \
      AlwaysEnter |  NeverEnter
               \  |  /
                \ | /
                  |
          NoValue (Lattice Bottom)


It is also called the "outer" info in some places.

What these values represent is the requirement of needing
to evaluate a binding 'val' in a context like

    case val of ...

Which for values coincides with a value being tagged.

For functions it doesn't currently matter (operationally) how we
treat them as they can not be entered only called.
However we try to assign functions a value of NeverEnter,
as it makes certain things more consistent in the code.

NoValue is a special value assigned to code paths who's result can't be
scrutinised at runtime. This is *different* from the values themselves
being bottom. Rather the expression producing them is bottom!

A common example is the tail recursive branch in a recursive function.
See Note [Recursive Functions] for why we need this.
If we end up assigning NoValue to a *binding's* enterInfo then
this binding represents a computation which won't return as it
will tail call itself forever.
This happens for example in `f x = f x`.

NeverEnter means the object referenced by the binding won't ever be
entered as a *value*. It might be called as a function when applied
to arguments.

AlwaysEnter implies something is a thunk of some form. However since GC
can also evaluate certain forms of thunks we do currently not utilize it.
As other wise we might redundantly enter thunks.

MaybeEnter represents the set of things for which one of these is true:
* We don't care about the enter behaviour
* We can't (easily) know the enter behaviour - e.g. function arguments
* We know it could be either enter or no-enter depending on
  branches taken at runtime.

# The FieldInfo Lattice

The second lattice represents information about values which are
in the fields of an id.
This is a mouthful so here is an example:

    let x = foo
    in case x of
        MyCon a b -> case a of
            <alts>

    Assume we have determined foo has the field info lattice:
        `FieldsProd [(NeverEnter,fi1), (MaybeEnter,fi2)]
    So naturally the same is true of the binding x.

    This means if we bind the second field of 'x' (here bound to 'a')
    then 'a' will have the information (NeverEnter,fi1) associated with it.

    This is independent of weither or not x needs to be entered, or even it's
    termination. As this information can only be used if x terminates.

If we have "foo = Just bar" then this lattice will
encode the information we have about bar, potentially also with
nested information itself. In practice we limit to nesting to a certain depth
for performance reasons but a few levels deep can be very useful.

This lattice has this shape:

           FieldsUndet
               |
         FieldsUnknown
               |
         FieldsUntyped
            /  |  \
   FieldsProd  |  FieldsSum
            \  |  /
          LatNoValues



It's very much analog to the one for enterInfo.

Again we have a placeholder for non-existing values used
for e.g. recursive tail calls.
See Note [Recursive Functions] for details about that.

Some of these constructors represent semantically a infinite number of fields
containing certain information:
* FieldsUndet   => each field contains a not yet determined values.
* FieldsUnknown => each field is a real top, we can't know anything about them yet.
* LatNoValues   => each field is the result of bottom expression returning. Also used
                   if no fields are present.
* FieldsProd/FieldsSum/FieldsUntyped encode varying information about a given
  number of fields. Fields not explicitly present in the list are assumed NoValue.

Again as example we might have

    x = Just Nothing.

With the enterInfo of 'x' being:

    enterInfo(x) = NeverEnter <FieldsSum [NeverEnter<LatNoValues>]>

When documenting this analysis we often omit the Fields* constructors for clarity.
We would then write x's enterInfo as NeverEnter < NeverEnter >.

    Note [The lattice element combinators]
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We do use lub to combine multiple branches and this behaves as expected.

For field infos we combine field information pointwise.

When combining results from different branches we allow
combinations of different field counts. Assuming NoValue for fields which
are not present. Usually promoting the field lattice to UntypedFields.

See also Note [Combining Branches].

Implementation wise we join branches by having the branching expression
take a special node as input, which then has all branches as inputs.
This means all other nodes can have a fixed number of inputs/outputs and
it's more obvious when multiple branches are combined when looking at the
data flow graph.


    Note [Combining Branches]
    ~~~~~~~~~~~~~~~~~~~~~~~~~

We use lub to combine the branches:

The enterInfo of a case expression is the combination
of all it's branches. Let's look at an easy case first:

    case exp of
        A1 -> alt1@(True,thunk)
        A2 -> alt2@(True,False)

Let's assume we already established that:

    enterInfo(alt1) = NeverEnter < NeverEnter, MaybeEnter >
    enterInfo(alt2) = NeverEnter < NeverEnter, NeverEnter >

We compute lub(alt1,alt2):

    lub_outer(NeverEnter,NeverEnter) = NeverEnter

Then we have to combine the fields of branches pointwise:

    lub_fields(FieldsProd<NeverEnter,MaybeEnter>, FieldsProd<NeverEnter,NeverEnter>) = ?

    => comparing the fields pointwise

    lub_field1(NeverEnter,NeverEnter) = NeverEnter
    lub_field2(NeverEnter,MaybeEnter) = MaybeEnter

    lub_fields(FieldsProd<NeverEnter,MaybeEnter>, FieldsProd<NeverEnter,NeverEnter>)
        = FieldsProd<NeverEnter,MaybeEnter>

So the overall result is:

    lub(alt1,alt2) = NeverEnter < NeverEnter, MaybeEnter >

If there are more than two branches than we simply compute

    alt1 `lub` alt2 `lub` ... `lub` altn

This is fairly straight forward so far.
However what about this case:

    case exp of
        A1 -> alt1@Nothing
        A2 -> alt2@Just True

We established:

    enterInfo(alt1) = NeverEnter
    enterInfo(alt2) = NeverEnter < NeverEnter >

and now we want to compute lub(alt1,alt2).
We first combine the outermost information:

    lub_outer(NeverEnter,NeverEnter) = NeverEnter

Then we have to combine the fields of branches pointwise:

    lub_fields(FieldsProd<NeverEnter>, FieldsNone) = ?

But we can't combine fields pointwise when the numbers of fields
doesn't match. To work around this we treat the branch with fewer fields
as having exactly the right number of additional fields for a pointwise
combination. All of the additional ones having an enterInfo of NoValue.

    lub_fields(FieldsProd<NeverEnter>, FieldsNone) = ?

    => Treat the second argument as having one NoValue field

    lub_fields(FieldsProd<NeverEnter>, FieldsNone<NoValue>) = ?
    lub_field1(NeverEnter, NoValue) = NeverEnter

    => Giving us the final result of:

    lub_fields(FieldsProd<NeverEnter>, FieldsNone<NoValue>)
        = FieldsUntyped<NeverEnter>)

    lub(alt1,alt2) = NeverEnter < NeverEnter >

This works because:

* NoValue represent the fact that we will never enter an expression with this enterInfo at runtime.
* We can never enter a non-existent field of a constructor => We can treat it as NoValue.

Why does this work?

The basic requirement for this to work is that non-existant fields are never
entered in correct code, and further can never be bound to a variable at runtime.


* NonExistant fields are never entered in well-typed code and never bound to variables =>
    + For a NoValue field to impact runtime behaviour there must be
      a constructor for which a field is combined with an assumed NoValue field.
      Otherwise the assumed field makes no difference to the result.

      So a construct of something along the lines of:

        let x = case e of
                  alt1 -> C1 e1
                  alt2 -> C2
        in
        ...
        case x of
            C1 f1 -> case f1 of ...
            C2    -> exp2

* If alt2 is taken enterInfo(f1) doesn't affect execution of the program.

* Since NoValue is the bottom of the lattice:
    enterInfo(f1) = lub(enterInfo e1, NoValue) = enterInfo(e1)

* If alt1 is taken GHC's code generation will:
    + Store the pointer representing e1 in a constructor C1, and bind it to x
    + When scrutinizing x the generated code will bind the pointer in x's first field
      to f1.
    + This retains the taggedness properties this analysis cares about. So enterInfo(e1) == enterInfo(e2)
      will hold.

Hopefully sheds some light on why it is safe to use this approach.

    Note [Recursive Functions]
    ~~~~~~~~~~~~~~~~~~~~~~~~~~

Consider this function:

    f x
        | x < 0 = Just $! Nothing
        | otherwise = f $ x-1

It's fairly obvious that it will return a value who's first
field will contain a tagged&evaluated value.

That is for `case (f foo) of x` we get enterInfo(x) == NeverEnter <NeverEnter>.

We deal with this by looking for branches in a functions body
which are recursive tail calls to the function itself.
We can treat these branches as not returning a value.
Indeed any value eventually produced by such a function must come from a
branch NOT consisting of a recursive tail call to itself.

For any of these tail calls we assign a enter info of NoValue.
When combining NoValue with other branches NoValue
always "gives way" to the result of the non-recursive branch(es).

This is correct as *when the function returns* it's result must be
from one of the non-tailcalled branches.

It even works our for silly things like f x = f x, we infer a value
of noValue for `f x` which means if `f` is called in a branch somewhere else
that branch too will give way to the terminating branches.

See also Note [Combining Branches] for more details on combining branches.

Beyond this we are cautios when combining the result of branches.
Since undetermined is the top of the lattice and we use lub
a combination of any number of branches will be undetermined until
we have approximated a more precise result for *all* branches.

Implementation wise we check for self tail calls by looking at the syntactic
context of function applications. This is implemented in `isRecTail`
and looks at join points (LNE) as well.

This does not solve the problem of mutual recursion however, in which
case we just throw our hands up in the air and simply assign the recursive
branch as undetermined.

TODO:
    We can surely do better on mutual recursion. After all for mutual recursion
    the eventual result will be that of a non-recursive branch so much of the same
    logic applies.
    But it's rare enough that it doesn't impact performance in a major way.

    Note [Functions producing infinite values]
    ------------------------------------------

We have to deal with functions without a fixpoint in our lattice.
These are primarily functions producing infinite values like (f x = x : f x)

To handle these cases we do two things:
* We limit processing data flow nodes for a certain depth of their fields
  using a widening operator.
* We stop running the analysis after a certain amount of time.
    + However the whole analysis is designed such that intermediate results of the
      analysis are *correct*. But they might be less precise.
    + This means even if the analysis doesn't terminate with a fixpoint we can
      use the results we derived so far.

Intermediate results (before a fixpoint is reached) might
be less precise than our analysis allows for. But any intermediate
result of the analysis will result in a safe approximation for the
program, as long as we have done at least one pass over each RHS.

Why one pass?

Consider this code:

    let x = StrictTuple (absentError foo) True

If we would stop after the initialization step then `x` would have
enterInfo(x) == UndetEnterInfo. To uphold the strict field invariant
we would then need to evaluate `absentError foo` before allocating the
strict tuple which would crash at runtime. See Note [The strict field invariant].

However it takes exactly one pass if we update nodes in dependency order
 - which we do - to infer all bindings binding absentErrors as NeverEnter.

This means as long as we do one pass over all data flow nodes we might be
less precise than we could be. But we don't predict invalid enterInfo and
therefore can use the result from the solved constraints, even if not all
have been resolved.


    Note [Infering recursive tail calls]
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

When creating the data flow graph we also keep track of
the context in which an expression occurs.
This is required to deal with shadowing of ids but also allows
us to infer if a function application is in a tail call position.

The rules are not that complicated and for the most part implemented
in isRecTail. They are similar to
the rules for join points.

It's not a tail call if:
    * If it's not saturated
    * If it's in a case scrutinee
    * If it's in a non recursive let definition
    * If it's defined in a potentially mutually recursive binding group.
It is a tail call if it's none of the above and in a tail call position in
    * the body of a let
    * the definition of a let-no-escape
    * a case alternative
    * a closure
-}

---------------------------------
--      The Lattice Types      --
---------------------------------


undetLat :: EnterLattice
undetLat = EnterLattice UndetEnterInfo FieldsUndet
maybeLat :: EnterLattice
maybeLat = EnterLattice MaybeEnter FieldsUnknown
noValue :: EnterLattice
noValue = EnterLattice NoValue FieldsNone

-- | Encode if a node needs to be entered or is already evaluated.
data EnterInfo
    = NoValue           -- ^ E.g. direct tail recursion, impossible fields.
    | AlwaysEnter       -- ^ WILL need to be entered
    | NeverEnter        -- ^ Does NOT need to be entered.
    | MaybeEnter        -- ^ Could be either
    | UndetEnterInfo    -- ^ Not yet determined.
    deriving (Eq,Ord,Show,Enum,Generic,NFData)

instance Binary EnterInfo where
    put_ bh info = putByte bh (fromIntegral $ fromEnum info) -- . (fromIntegral . fromEnum $ info :: Int8)
    get h = toEnum . fromIntegral <$> getByte h

instance Outputable EnterInfo where
    ppr UndetEnterInfo  = char '?'
    ppr NoValue         = text "noInfo"
    ppr AlwaysEnter     = text "ent"
    ppr MaybeEnter      = char 'm'
    ppr NeverEnter      = char 't'

{-  Note [Comparing Sums and Products]
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    At a glance it makes sense that we would never compare sum and product results.
    However consider this case:

    case v of
        True -> case x of prod -> Left prod
        False -> case y of sum -> Right sum

    Then we will infer taggedness of tagged<tagged>, which is a tagged
    result with the first field also being tagged.

    However the first field will be a prod type in one and
    a sum type in the other case. But this does not concern
    us as taggedness is value-level property so their types
    don't have to match.

    We could go even further still and compare the fields of
    `prod` and `sum` against each other. And we do!

    See Note [The lattice element combinators] for details.

-}

data EnterLattice = EnterLattice
    { enterInfo :: !EnterInfo
    , fieldInfo :: !FieldInfo
    }
    deriving (Eq, Generic, NFData)

instance Binary EnterLattice where
    put_ bh (EnterLattice enterInfo fieldInfo) = put_ bh enterInfo >> put_ bh fieldInfo
    get h = pure EnterLattice <*> B.get h <*> B.get h

-- Side note: Nullary constructors are assigned FieldsNone.

data FieldInfo
    -- | Direct tail recursion, "phantom" fields.
    = FieldsNone

    -- | The associated value has up to (length fields) fields we know something
    -- about. But the actual value at runtime can have less fields! Or more fields!
    -- See Note [Lattice for tag analysis].
    | FieldsUntyped [EnterLattice]

    -- Product result with up to (length fields) fields we know something about.
    | FieldsProd [EnterLattice]

    -- Sum with constructor the fields came out of
    | FieldsSum  !(Maybe DataCon) [EnterLattice]

    -- | At most we can say something about the tag of the value.
    --   The fields are impossible to known.
    | FieldsUnknown

    -- | We might find out more about the fields
    | FieldsUndet
    deriving (Generic)

instance Eq FieldInfo where
    -- x == y
    --     | maybeEq x y == True
    FieldsNone == FieldsNone = True
    FieldsUnknown == FieldsUnknown = True
    FieldsUndet == FieldsUndet = True
    (FieldsSum mb_con1 flds1) == (FieldsSum mb_con2 flds2)
        = mb_con1 == mb_con2 && eqEnterLattices flds1 flds2
    (FieldsUntyped flds1) == (FieldsUntyped flds2)
        = eqEnterLattices flds1 flds2
    (FieldsProd flds1) == (FieldsProd flds2)
        = eqEnterLattices flds1 flds2
    _ == _ = False

-- Relying on Eq [a] ends up not specializing which is quite
-- bad for performance :( So I handwrote this after the obvious
-- attempts failed.
eqEnterLattices :: [EnterLattice] -> [EnterLattice] -> Bool
eqEnterLattices [] [] = True
eqEnterLattices (x:xs) (y:ys) =
    x == y && eqEnterLattices xs ys
eqEnterLattices _ _ = False

instance NFData FieldInfo where
    rnf x = seq x ()

instance Outputable EnterLattice where
    ppr (EnterLattice enterInfo fieldInfo)
        = ppr enterInfo <> text " x " <> ppr fieldInfo

instance Outputable FieldInfo where
    ppr FieldsUnknown           = text "bot"
    ppr (FieldsUntyped fields)  = text "any" <> ppr fields
    ppr (FieldsProd fields)     = text "prod" <> ppr fields
    ppr (FieldsSum con fields)  = text "sum" <> char '<' <> ppr con <> char '>' <> ppr fields
    ppr FieldsNone              = text "none"
    ppr FieldsUndet             = text "undet"

{-  Note [FieldInfo Binary instance]
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Putting a data con into a interface file can cause non-trivial overhead
as it involves type checking at load time among other things.

So we convert all Sum field infos to untyped ones when serializing.
We currently do not take advantage of the con info so this does not
weaken the strength of the analysis.

-}
instance Binary FieldInfo where
    put_ bh FieldsNone              = putByte bh 0
    put_ bh (FieldsUntyped fields)  = putByte bh 1 >> put_ bh fields
    put_ bh (FieldsProd fields)     = putByte bh 2 >> put_ bh fields
    -- We turn FieldSum into FieldsUntyped here,
    -- While losing precision it means we don't have to save the con
    put_ bh (FieldsSum _con fields) = putByte bh 1 >> put_ bh fields
    put_ bh FieldsUnknown           = putByte bh 4
    put_ bh FieldsUndet             = putByte bh 5

    get bh = do
        con <- getByte bh
        case con of
            0 -> pure FieldsNone
            1 -> pure FieldsUntyped <*> B.get bh
            2 -> pure FieldsProd <*> B.get bh
            3 -> panic "get:FieldInfo - invalid byte"
            4 -> pure FieldsUnknown
            5 -> pure FieldsUndet
            _ -> panic "get:FieldInfo - invalid byte"

-- lub
combineEnterInfo :: EnterInfo -> EnterInfo -> EnterInfo
combineEnterInfo UndetEnterInfo _       = UndetEnterInfo
combineEnterInfo _ UndetEnterInfo       = UndetEnterInfo
combineEnterInfo MaybeEnter _           = MaybeEnter
combineEnterInfo _ MaybeEnter           = MaybeEnter
combineEnterInfo NeverEnter AlwaysEnter = MaybeEnter
combineEnterInfo AlwaysEnter NeverEnter = MaybeEnter
combineEnterInfo AlwaysEnter AlwaysEnter= AlwaysEnter
combineEnterInfo NeverEnter NeverEnter  = NeverEnter
combineEnterInfo NoValue x              = x
combineEnterInfo x NoValue              = x

combineFieldsUntyped :: [EnterLattice] -> [EnterLattice] -> [EnterLattice]
combineFieldsUntyped fields1 fields2 =
    go fields1 fields2
  where
    go (x:xs) (y:ys) = combineLattices x y : go xs ys
    go []     ys     = ys
    go xs     []      = xs


combineLattices :: EnterLattice -> EnterLattice -> EnterLattice
combineLattices x1 x2 | maybeEq x1 x2 || x1 == x2 = x1
combineLattices (EnterLattice ei1 fi1) (EnterLattice ei2 fi2)
    = EnterLattice (combineEnterInfo ei1 ei2) (combineFieldInfos fi1 fi2)

combineFieldInfos :: FieldInfo -> FieldInfo -> FieldInfo
combineFieldInfos FieldsUndet _ = FieldsUndet
combineFieldInfos _ FieldsUndet = FieldsUndet
combineFieldInfos (FieldsUnknown) _ = FieldsUnknown
combineFieldInfos _ (FieldsUnknown) = FieldsUnknown
combineFieldInfos FieldsNone x = x
combineFieldInfos x FieldsNone = x
-- Combine results of different constructors
-- See Note [Combining Branches]
combineFieldInfos (FieldsProd fs1) (FieldsSum _ fs2) =
    FieldsUntyped $ combineFieldsUntyped fs1 fs2
combineFieldInfos (FieldsSum _ fs1) (FieldsProd fs2) =
    FieldsUntyped $ combineFieldsUntyped fs1 fs2

combineFieldInfos (FieldsSum c1 fs1)  (FieldsSum c2 fs2)
    | c1 /= c2  = FieldsUntyped $ combineFieldsUntyped fs1 fs2
    | otherwise = FieldsSum c1 $
                  zipWithEqual "SumInfo:combine" combineLattices fs1 fs2
combineFieldInfos (FieldsProd fs1) (FieldsProd fs2)
    | l1 == l2 = FieldsProd $ combined
    -- We might combine different types. See Note [Combining Branches]
    | otherwise = FieldsProd $ combined ++ tail
    where
        combined = zipWith combineLattices fs1 fs2
        tail
          | l1 < l2 = drop l1 fs2
          | l1 > l2 = drop l2 fs1
          | otherwise = panic "combineFieldInfos: impossible"
        !l1 = length fs1
        !l2 = length fs2


-- untyped v untyped
combineFieldInfos (FieldsUntyped fs1) (FieldsUntyped fs2) =
    FieldsUntyped $ combineFieldsUntyped fs1 fs2
-- untyped v sum
combineFieldInfos (FieldsSum _ fs1) (FieldsUntyped fs2) =
    FieldsUntyped $ combineFieldsUntyped fs1 fs2
combineFieldInfos (FieldsUntyped fs1) (FieldsSum _ fs2) =
    FieldsUntyped $ combineFieldsUntyped fs1 fs2
-- untyped v prod
combineFieldInfos (FieldsProd fs1) (FieldsUntyped fs2) =
    FieldsUntyped $ combineFieldsUntyped fs1 fs2
combineFieldInfos (FieldsUntyped fs1) (FieldsProd fs2) =
    FieldsUntyped $ combineFieldsUntyped fs1 fs2

------------------------------------------------------------
--      Utility functions to deal with lattices           --
------------------------------------------------------------


-- Lattice of which we know, and can only know, the outer layer.
flatLattice :: EnterInfo -> EnterLattice
flatLattice x = EnterLattice x FieldsUnknown

-- Lattice where we know there are no inner values.
nullaryLattice :: EnterInfo -> EnterLattice
nullaryLattice enterInfo = EnterLattice enterInfo FieldsNone

-- Set (outermost) enterInfo
setEnterInfo :: HasDebugCallStack => EnterLattice -> EnterInfo -> EnterLattice
setEnterInfo lat@(EnterLattice enter fields) newEnter
    | enter == newEnter
    = lat
    | otherwise = EnterLattice newEnter fields

-- Lookup nth-field of the returned valued.
-- Defaulting towards undetLat
-- Zero indexed
indexField :: EnterLattice -> Int -> EnterLattice
indexField lat n =
    case fieldInfo lat of
        FieldsUndet -> undetLat
        FieldsUnknown -> maybeLat
        FieldsNone -> noValue
        FieldsSum  _ fields -> getField fields
        FieldsProd fields   -> getField fields
        FieldsUntyped fields -> getField fields
  where
    getField fields =
        case drop n fields of
            -- We treat [] equal to [undetLat, undetLat, undetLat, ...]
            [] -> undetLat
            (x:_xs) -> x

hasOuterTag :: EnterLattice -> Bool
hasOuterTag lat = enterInfo lat == NeverEnter

-- We use these to stop iterating on nodes which are already at the bot of the lattice.

hasFinalFields :: EnterLattice -> Bool
hasFinalFields lat =
    case (fieldInfo lat) of
        (FieldsUnknown )        -> True
        (FieldsNone  )          -> False
        (FieldsUndet)           -> False
        (FieldsProd fields)     -> all isFinalValue fields
        (FieldsSum  _ fields)   -> all isFinalValue fields
        (FieldsUntyped fields)  -> all isFinalValue fields

isFinalValue :: EnterLattice -> Bool
isFinalValue lat = enterInfo lat == MaybeEnter && hasFinalFields lat

nestingLevelOver :: EnterLattice -> Int -> Bool
nestingLevelOver _ 0 = True
nestingLevelOver (EnterLattice _ (FieldsProd fields)) n
    = any (`nestingLevelOver` (n-1)) fields
nestingLevelOver (EnterLattice _  (FieldsSum  _ fields)) n
    = any (`nestingLevelOver` (n-1)) fields
nestingLevelOver (EnterLattice _  (FieldsUntyped fields)) n
    = any (`nestingLevelOver` (n-1)) fields
nestingLevelOver _ _ = False

widenToNestingLevel :: Int -> EnterLattice -> EnterLattice
widenToNestingLevel n l
    | nestingLevelOver l n = -- pprTrace "capping" (ppr l) $
                             widenToNestingLevel' n l
    | otherwise = l

widenToNestingLevel' :: Int -> EnterLattice -> EnterLattice
widenToNestingLevel' _ l@(EnterLattice _ FieldsUnknown )  = l
widenToNestingLevel' _ l@(EnterLattice _ FieldsNone     ) = l
widenToNestingLevel' _ l@(EnterLattice _ FieldsUndet   )  = l
widenToNestingLevel' 0 _ = maybeLat
widenToNestingLevel' n (EnterLattice e (FieldsProd fields)) =
    EnterLattice e (FieldsProd $! (map (widenToNestingLevel' (n-1)) fields))
widenToNestingLevel' n (EnterLattice e (FieldsSum c fields)) =
    EnterLattice e (FieldsSum c $! map (widenToNestingLevel' (n-1)) fields)
widenToNestingLevel' n (EnterLattice e (FieldsUntyped fields)) =
    EnterLattice e (FieldsUntyped $! map (widenToNestingLevel' (n-1)) fields)


-- Node IDs are generally *just* uniques created during the creation
-- of the data flow graph.
newtype NodeId = NodeId { nid_unique :: Unique } -- ^ Other nodes
    deriving (Eq, Generic)

instance Ord NodeId where
    compare = comparing (getKey . nid_unique)

instance Outputable NodeId where
    ppr (NodeId  i) = ppr i

instance NFData NodeId where
    rnf x = seq x ()

instance Uniquable NodeId where
    getUnique = nid_unique

instance Uniquable FlowNode where
    getUnique = getUnique . node_id

data FlowState
    = FlowState
    { fs_idNodeMap :: !(UniqFM Id NodeId) -- ^ Map of imported id nodes (indexed by `Id`).
    , fs_uqNodeMap :: !(UniqFM NodeId FlowNode) -- ^ Transient results, index by `NodeId`
    , fs_doneNodes :: !(UniqFM NodeId FlowNode) -- ^ We can be sure these will no longer change, index by `NodeId`
    }

fromUEnv :: UFlowState -> FlowState
fromUEnv (# id_map, uq_map, done_map #) =
    FlowState {
        fs_idNodeMap = id_map,
        fs_uqNodeMap = uq_map,
        fs_doneNodes = done_map }

toUEnv :: FlowState -> UFlowState
toUEnv !env = (# fs_idNodeMap env, fs_uqNodeMap env, fs_doneNodes env #)

type UFlowState = (# (UniqFM Id NodeId), (UniqFM NodeId FlowNode), (UniqFM NodeId FlowNode) #)

newtype AM a = AM { runAM :: UFlowState -> State# RealWorld
                          -> (# a, UFlowState, State# RealWorld #)
                  } deriving Functor

instance Applicative AM where
    {-# INLINE pure #-}
    pure x   = AM $ oneShot $ \env s -> (# x, env, s #)
    {-# INLINE (<*>) #-}
    m <*> n  = AM $ oneShot $ \env s -> case runAM m env s of
                                (# f, env', s' #) -> case runAM n env' s' of
                                            (# x, env'', s'' #) -> (# f x, env'', s'' #)

instance Monad AM where
    {-# INLINE (>>=) #-}
    m >>= n  = AM $ oneShot $ \env s -> case runAM m env s of
                                (# !x, env', s' #) -> runAM (n x) env' s'
    {-# INLINE return #-}
    return = pure

get :: AM FlowState
get = AM $ oneShot $ \env s -> (# fromUEnv env, env, s #)

put :: FlowState -> AM ()
put !env = AM $ oneShot $ \_ s -> (# (), toUEnv env, s #)

evalAM :: FlowState -> AM a -> a
evalAM env (AM f) = runRW# $ \s ->
    case f (toUEnv env) s of
        (# x, _env', _s' #) -> x

instance MonadIO AM where
    liftIO (IO f) =
        AM $ oneShot $ \env s ->
                case f s of
                    (# s', !x #) -> (# x, env, s' #)


-- TODO: Add to list in UniqSupply
inferenceUniqueKey :: Char
inferenceUniqueKey = 't'

instance MonadUnique AM where
    getUniqueSupplyM = do
        liftIO $! mkSplitUniqSupply inferenceUniqueKey

    getUniqueM = do
        liftIO $! uniqFromMask inferenceUniqueKey

isDone :: Bool
isDone = True
notDone :: Bool
notDone = False

-- | Add new node, maybe mark it done.
addNode :: Bool -> FlowNode -> AM ()
addNode done node = do
    s <- get
    if done
        then put $! s { fs_doneNodes = addToUFM (fs_doneNodes s) (node_id node) node
                     , fs_uqNodeMap = delFromUFM (fs_uqNodeMap s) (node_id node) }
        else do
            ASSERTM( not <$> isMarkedDone (node_id node))
            put $! s { fs_uqNodeMap = addToUFM (fs_uqNodeMap s) (node_id node) node }

-- Update existing node
-- Kept separate in case we want to refactor later.
updateNode :: Bool -> FlowNode -> AM ()
updateNode done node = do
    addNode done node

-- | Move the node from the updateable to the finished set
markDone :: FlowNode -> AM ()
markDone node = do
    updateNode isDone node

-- | Pessimistic check, defaulting to False when it's not clear.
isMarkedDone :: HasDebugCallStack => NodeId -> AM Bool
isMarkedDone id = do
    s <- get
    return $! elemUFM id (fs_doneNodes s)

updateNodeResult :: NodeId -> EnterLattice -> AM ()
updateNodeResult id result = do
    node <- (getNode id)
    updateNode notDone (node {node_result = result})


getNode :: HasDebugCallStack => NodeId -> AM FlowNode
getNode node_id = do
    s <- get
    return $! fromMaybe
                   (pprPanic "Node not found" (ppr node_id))
                   (lookupUFM (fs_doneNodes s) node_id <|> lookupUFM (fs_uqNodeMap s) node_id)


-- We will never use this function to query for
-- non-existing nodes in the absence of bugs.
lookupNodeResult :: HasDebugCallStack => NodeId -> AM EnterLattice
lookupNodeResult node_id = do
    s <- get
    let node = (lookupUFM (fs_uqNodeMap s) node_id <|>
                lookupUFM (fs_doneNodes s) node_id)
    case node of
        Nothing -> do
            when debugIsOn $
                pprTraceM ("lookpupNodeResult: Nothing\n" ++ prettyCallStack callStack) (ppr node_id)
            return undetLat
        Just n  -> return $! node_result n

{-
    Note [Field information of function ids]
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Consider this code:

f x :: a -> (Maybe Int,a)
f x = (Just 1, a)

We can infer the taggedness of all but the second field of the tuple.

However if we have code like this:

f x :: a -> (Maybe Int,a)
f x = (Just 1, a)

g _ =
    let v = Just f
        ...
    in
    case v of
        Just f' -> f' True
        Nothing -> f False

If we keep the field information of f when stored inside a constructor then
we can eventually figure out that f' == f, so all branches have the same return taggedness info
which is the same as the one of f. So we could infer as the taggedness of g's result:

    enterInfo(g) = NeverEnter < NeverEnter < NeverEnter >, MaybeEnter>
                    ^ func        ^ Just       ^ 1           ^ "a"

However currently when when we bind a function to a variable we throw away any field information
of the functions result. We do not *have* to strip this information, it's merely done for performance.

TODO:
    This could be done, at the expense of compile time. We should benchmark this as future work
    to figure out of it's worth and do it if useful.

    Same thing for keeping track of the original function. Potentially removing some unknown calls.

-}

-- | If we use a *function* as an unapplied argument to a constructor we throw
-- away nested information and make do with NeverEnter Top for now.
-- See Note [Field information of function ids]
getConArgNodeId :: HasDebugCallStack => ContextStack -> StgArg -> AM NodeId
getConArgNodeId _    (StgLitArg _ ) = return litNodeId
getConArgNodeId ctxt (StgVarArg v )
    | isFunTy (unwrapType $ idType v)
    = return neverNodeId
    | otherwise
    = getIdNodeId ctxt v

-- Potential performance improvements.
-- TODO: We could put the result into it's own map of NodeId -> EnterLattice
--       or even an array. But that complicates the code somewhat and performance
--       doesn't seem to be an issue currently.
--
-- TODO: We could remove the node_id field completely as well. When allocating the node
--       we do know the node_id and capture it in a closure/the key of the map if needed.
--       Anyone querying the node also knows which id he queries for.
--       However adding an unused field seems to only increase allocations for *findTags*
--       by ~0,3% and helps a lot with debugging. So no good reason to do so currently.
data FlowNode
    = FlowNode
    { node_id :: {-# UNPACK  #-} !NodeId    -- ^ Node id
    , node_inputs :: [NodeId]               -- ^ Input dependencies
    , node_result :: !(EnterLattice)        -- ^ Cached result
    , node_update :: (AM EnterLattice)      -- ^ Calculates a new value for the node
                                            -- AND updates the value in the environment.
#if defined(WITH_NODE_DESC)
    , _node_desc :: SDoc -- ^ Debugging purposes
#endif
    }

node_desc :: FlowNode -> SDoc
#if defined(WITH_NODE_DESC)
node_desc n = _node_desc n
#else
node_desc _n = empty
#endif

instance NFData FlowNode where
    rnf (   FlowNode
                { node_id = _
                , node_inputs = node_inputs
                -- , node_done = _
                , node_result = node_result
                , node_update = _
#if defined(WITH_NODE_DESC)
                , _node_desc = _
#endif
                })  = deepseq (node_inputs,node_result) ()

instance Outputable FlowNode where
    ppr node =
        hang
            (text "node_" <> pprId node <-> pprDone node <-> (node_desc node) )
            2
            ( (ppr $ node_inputs node) <> parens (ppr $ node_result node) )
      where
        pprId node =
            case node_id node of
                NodeId uq -> ppr uq
        pprDone _node = empty
            -- if node_done node then text "done" else empty

data IsLNE = LNE | NotLNE deriving (Eq)

instance Outputable IsLNE where
    -- ppr :: IsLNE -> SDoc
    ppr NotLNE = empty
    ppr LNE = text "-LNE"

-- Syntactic context of the construct the node represents.
-- For most cases including a mapping
-- of in-scope ids to their data flow nodes/labels.
--
-- Primarily used to avoid shadowing. But CCaseScrut is also
-- needed to determine tail recursive calls.
-- See also Note [Recursive Functions]
data SynContext
    = CTopLevel     !(VarEnv NodeId)
    -- | letrec x = <here> in body
    | CLetRec       !(VarEnv NodeId) !IsLNE
    -- | letrec x = e in <here>
    | CLetRecBody   !(VarEnv NodeId) !IsLNE
    | CLet          !(VarEnv NodeId) !IsLNE
    | CLetBody      !(VarEnv NodeId) !IsLNE
    | CClosureBody  !(VarEnv NodeId)
    | CCaseScrut
    | CCaseBndr     !(VarEnv NodeId)
    | CAlt          !(VarEnv NodeId)
    deriving Eq

type ContextStack = [SynContext]

getCtxtIdMap :: SynContext -> Maybe (VarEnv NodeId)
getCtxtIdMap (CClosureBody m) = Just m
getCtxtIdMap (CCaseBndr m) = Just $ m
getCtxtIdMap (CCaseScrut) = Nothing
getCtxtIdMap (CAlt m) = Just m
getCtxtIdMap (CLetRec m _) = Just m
getCtxtIdMap (CLetRecBody m _) = Just m
getCtxtIdMap (CLet m _) = Just m
getCtxtIdMap (CLetBody m _) = Just m
getCtxtIdMap (CTopLevel m) = Just m

extendCtxt :: SynContext -> ContextStack -> ContextStack
extendCtxt c ctxt = c : ctxt

instance Outputable SynContext where
    ppr (CTopLevel map)       = text "CTop"        <> ppr map
    ppr (CAlt map)            = text "CAlt"        <> ppr map
    ppr CCaseScrut            = text "CCaseScrut"
    ppr (CCaseBndr map)       = text "CCaseBndr"   <> ppr map
    ppr (CClosureBody map)    = text "CClosure"    <> ppr map
    ppr (CLetRec     ids lne) = text "CLetRec"     <> ppr lne <> ppr ids
    ppr (CLetRecBody ids lne) = text "CLetRecBody" <> ppr lne <> ppr ids
    ppr (CLet id lne)         = text "CLet"        <> ppr lne <> ppr id
    ppr (CLetBody id lne)     = text "CLetBody"    <> ppr lne <> ppr id

-- | Is the given id mapped to a data flow node in the given context?
idMappedInCtxt :: Id -> ContextStack -> Maybe NodeId
idMappedInCtxt id ctxt
    = go ctxt
  where
    go (ctxt:_)
        | Just argMap <- getCtxtIdMap ctxt
        , Just node <- lookupVarEnv argMap id
        = Just $! node
    go (_:todo) = go todo
    go [] = Nothing

-- `isRecTail f ctxt`
-- Determine if f called in context ctxt is a recursive tail call.
-- If so it's result will be NoValue.
-- See Note [Recursive Functions]
isRecTail :: Id -> ContextStack -> Bool
isRecTail _f (CTopLevel _ : _) = False
isRecTail  f (CLetRec bnds _: _)
    | isSingleMapOf f bnds
    = True
isRecTail f (CLetRec _ LNE    : ctxt) = isRecTail f ctxt
isRecTail _f (CLetRec _ NotLNE : _   ) = False
isRecTail f (CLetRecBody bnds _ :ctxt)
    -- `let x = e in .. -> x`
    -- is not a recursive tail call
    | f `elemVarEnv` bnds = False
    | otherwise = isRecTail f ctxt
isRecTail f (CLet _ LNE    : ctxt) = isRecTail f ctxt
isRecTail _f (CLet _ NotLNE : _) = False
isRecTail f (CLetBody bnds _ : ctxt)
    | elemVarEnv f bnds = False
    | otherwise = isRecTail f ctxt
isRecTail f (CClosureBody args : ctxt)
    | f `elemVarEnv` args = False
    | otherwise = isRecTail f ctxt
isRecTail _f (CCaseScrut : _) = False
isRecTail f (CCaseBndr bnd : ctxt )
    | elemVarEnv f bnd
    = False
    | otherwise
    = isRecTail f ctxt
isRecTail f (CAlt bnds : ctxt)
    | f `elemVarEnv` bnds = False
    | otherwise = isRecTail f ctxt
isRecTail f x = pprPanic "Incomplete" $ ppr (f,x)

-- | isSingleMapOf v env == fromList [(v',_)] && v == v'
--
-- Used to determine recursive calls.
isSingleMapOf :: Id -> VarEnv NodeId -> Bool
isSingleMapOf v env =
    isSingletonUFM env && elemVarEnv v env

-- | Create a data flow note which combines multiple branches.
-- See Note [The lattice element combinators]
mkJoinNode :: [NodeId] -> AM NodeId
mkJoinNode []     = return unknownNodeId
mkJoinNode [node] = return node
mkJoinNode inputs = do
    node_id <- mkUniqueId
    let updater = do
            input_results <- mapM lookupNodeResult inputs
            let result = foldl1' combineLattices input_results
            if isFinalValue result
                then do
                    node <- getNode node_id
                    markDone $ node { node_result = result }
                else updateNodeResult node_id result
            return $! result

    addNode notDone $ FlowNode { node_id = node_id, node_result = undetLat
                       , node_inputs = inputs -- , node_done = False
                       , node_update = updater
#if defined(WITH_NODE_DESC)
                       , _node_desc = text "branches"
#endif
                       }
    return $! node_id

-- | Compute the taggedness result of applying a constructor to the given arguments
--   *and* applying the strict field invariant. Marking all strict fields as tagged.
mkOutConLattice :: DataCon -> EnterInfo -> [EnterLattice] -> EnterLattice
mkOutConLattice con outer fields
    | null fields   = EnterLattice outer $ FieldsNone
    | conCount == 1 = EnterLattice outer $ FieldsProd out_fields
    | conCount > 1  = EnterLattice outer $ FieldsSum (Just con) out_fields
    | otherwise = panic "mkOutConLattice"
  where
    out_fields = mapStrictConArgs con (`setEnterInfo` NeverEnter) fields
    conCount = length (tyConDataCons $ dataConTyCon con)

{-# NOINLINE findTags #-}
findTags :: DynFlags -> Module -> [StgTopBinding] -> ([TgStgTopBinding], [(Id,EnterLattice)])
-- findTags this_mod us binds = passTopBinds binds
findTags dflags this_mod binds =
    let state = FlowState {
            fs_idNodeMap = mempty,
            fs_uqNodeMap = emptyUFM,
            fs_doneNodes = emptyUFM }
    -- Run the analysis
        analysis :: AM ([TgStgTopBinding], [(Id, EnterLattice)])
        analysis = do
            addConstantNodes
            (binds',_mapping) <- {-# SCC "mkFlowNodes" #-}
                                nodesTopBinds this_mod binds
            {-# SCC "solveConstraints" #-} (solveConstraints dflags)
            -- exports <- exportTaggedness mapping
            exports <- return mempty -- Should probably remove this
            !finalBinds <- {-# SCC "rewriteAST" #-}
                           rewriteTopBinds binds'
            return (finalBinds, exports)
        (!binds', exports) = evalAM state $ analysis
    in  (seqTopBinds binds') `seq`
            -- pprTrace "foundBinds" (ppr this_mod)
                (binds',exports)

-- Constant mappings
addConstantNodes :: AM ()
addConstantNodes = do
    markDone litNode
    markDone addrNode
    markDone $ mkConstNode undetNodeId undetLat (text "undet")
    markDone $ mkConstNode unknownNodeId maybeLat (text "bot")
    markDone $ neverEnterNode
    markDone $ maybeEnterNode
    markDone $ alwaysEnterNode


mkConstNode :: NodeId -> EnterLattice -> SDoc -> FlowNode
mkConstNode id !val _desc =
    FlowNode
    { node_id = id
    , node_inputs = []
    --, node_done = True
    , node_result = val
    , node_update = (return $! val)
#if defined(WITH_NODE_DESC)
    , _node_desc = _desc
#endif

    }

-- Some nodes we can reuse.
litNodeId, undetNodeId, unknownNodeId, neverNodeId, maybeNodeId,
    alwaysNodeId, addrNodeId, nullaryConNodeId :: NodeId
litNodeId       = NodeId $ mkUnique 'c' 2
undetNodeId     = NodeId $ mkUnique 'c' 3 -- Always returns undetLat
unknownNodeId   = NodeId $ mkUnique 'c' 4
neverNodeId     = NodeId $ mkUnique 'c' 5
maybeNodeId     = NodeId $ mkUnique 'c' 6
alwaysNodeId    = NodeId $ mkUnique 'c' 7
addrNodeId      = NodeId $ mkUnique 'c' 8
nullaryConNodeId = NodeId $ mkUnique 'c' 9

alwaysEnterNode, maybeEnterNode, neverEnterNode, litNode, addrNode, nullaryConNode :: FlowNode
alwaysEnterNode = mkConstNode alwaysNodeId  (flatLattice AlwaysEnter) (text "always")
maybeEnterNode  = mkConstNode maybeNodeId   (flatLattice MaybeEnter) (text "maybe")
neverEnterNode  = mkConstNode neverNodeId   (flatLattice NeverEnter) (text "never")
litNode         = mkConstNode litNodeId     (nullaryLattice NeverEnter) (text "lit")
addrNode        = mkConstNode addrNodeId    (nullaryLattice NeverEnter) (text "c_str")
nullaryConNode  = mkConstNode nullaryConNodeId (nullaryLattice NeverEnter) (text "nullCon")

{-  Note [Imported Ids]
    ~~~~~~~~~~~~~~~~~~~

# Assigning data flow nodes to imported ids.

We want to keep our Ids a simple newtype around Unique.
This is "easy" for things brought into scope by the AST we work with.
We simply put a mapping from the Id to the NodeId into SynContext.
We can then map ids to their data flow nodes based on the SynContext
we are in.

However imported Id's can show up in any place in the AST and we want to
avoid traversing the whole AST twice just to gather them up beforehand.
We solve this by creating a Node and NodeId for each imported
id when we come across the id the first time.

The next time we come across the same id getIdNodeId will check
fs_idNodeMap, find the node we created earlier and return the
same node.

!! Note that the Unique of an ID, and it's corresponding NodeId !!
!! are not correlated.                                          !!

# Taggedness of imported ids

This is currently determined fully in addImportedNode since
the result of tag inference is not exported in interface files.

The rules are simply:
    * Field info is always Unknown

    Enterinfo is:
    * NeverEnter for functions with known Arity
    * NeverEnter for nullarry constructors
    * NeverEnter for ids with Absent divergence. (absentError expressions)
    * AlwaysEnter for Thunks - Technically the RTS might evaluate them so *always* is a lie here.
    * MaybeEnter otherwise.

    Note [Shadowing and NodeIds]
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Shadowing makes things more complex.

While constructing the data flow graph we have to be able to relate
every variable v to a data flow node describing v's value.
However the same variable can have different values in different because
of shadowing. For this reason we keep around a context which allows us to
map variables to nodes. Whenever a new variable comes into scope this context
is extended.

We then use getIdNodeId to get the node associated with an specific Id in
an specific context. It also takes care of imported Ids (See Note [Imported Ids]).

When we want to get the nodeId for a particular id which *must*
already present in a context we use getKnownIdNodeId.

-}

-- See Note [Shadowing and NodeIds]
getIdNodeId :: HasDebugCallStack => ContextStack -> Id -> AM NodeId
getIdNodeId ctxt id
    | Just node <- idMappedInCtxt id ctxt
    = return $! node
    | otherwise = do
        s <- get
        return $! fromMaybe (pprPanic "Unmapped id" (ppr id)) $
            lookupUFM (fs_idNodeMap s) id

-- See Note [Shadowing and NodeIds]
getKnownIdNodeId :: HasDebugCallStack => ContextStack -> Id -> NodeId
getKnownIdNodeId ctxt id
    | Just node <- idMappedInCtxt id ctxt
    = node
    | otherwise = pprPanic "Local id not mapped:" (ppr id)

mkUniqueId :: AM NodeId
mkUniqueId = NodeId <$> getUniqueM


-- | This adds a node containing information about an imported id.
--   Logic mimics somewhat what we do in StgCmmClosure.hs:mkLFImported
--   See also Note [Imported Ids]
addImportedNode :: Module -> Id -> AM ()
addImportedNode this_mod id
    -- Local id, it has to be mapped to an id via SynContext
    | nameIsLocalOrFrom this_mod (idName id) = return ()
    | otherwise = do
        -- Check if it is already cached.
        s <- get
        let idNodes = fs_idNodeMap s
        -- If not make up a new node.
        when (not $ elemUFM id idNodes) $ do
            new_node_id <- mkUniqueId
            let node
                    -- Functions tagged with arity are never entered, only applied.
                    | idFunRepArity id > 0
                    = set_if_desc neverEnterNode new_node_id (text "ext_func" <-> ppr id)

                    -- Known Nullarry constructors are also never entered
                    -- but for them it's important to preserve the information
                    -- of no fields.
                    | Just con <- (isDataConWorkId_maybe id)
                    , isNullaryRepDataCon con
                    = set_if_desc nullaryConNode new_node_id (text "ext_nullCon" <-> ppr id)

                    -- Imported binding of absentError
                    | (_, Absent) <- splitStrictSig (idStrictness id)
                    = set_if_desc neverEnterNode new_node_id (text "ext_absent_error" <-> ppr id)

                    | Just lf_info <- idLFInfo_maybe id
                    =   case lf_info of
                            -- Function, applied not entered.
                            LFReEntrant {}
                                -> set_if_desc neverEnterNode new_node_id (text "ext_lf_func" <-> ppr id)
                            -- Thunks need to be entered.
                            LFThunk {}
                                -> set_if_desc alwaysEnterNode new_node_id (text "ext_lf_thunk" <-> ppr id)
                            -- Constructors, already tagged.
                            LFCon {}
                                -- If we ever bind the fields we can infer the tags
                                -- based on the fields strictness. So a flat lattice
                                -- is fine.
                                -> set_if_desc neverEnterNode new_node_id (text "ext_lf_con" <-> ppr id)
                            LFUnknown {}
                                -> set_if_desc maybeEnterNode new_node_id (text "ext_lf_unknown" <-> ppr id)
                            LFUnlifted {}
                                -> set_if_desc neverEnterNode new_node_id (text "ext_lf_unlifted" <-> ppr id)
                            -- Shouldn't be possible. I don't think we can export letNoEscapes
                            LFLetNoEscape {}
                                -> set_if_desc maybeEnterNode new_node_id (text "ext_lf_lne" <-> ppr id)

                    -- General case, a potentially unevaluated imported id.
                    | not isFun
                    = set_if_desc maybeEnterNode new_node_id (text "ext_unknown_enter" <-> ppr id)

                    -- May or may not be entered.
                    | otherwise
                    = set_if_desc maybeEnterNode new_node_id (text "ext_unknown" <-> ppr id)
            put $!
                s { fs_idNodeMap = addToUFM (fs_idNodeMap s) id new_node_id
                , fs_doneNodes = addToUFM (fs_doneNodes s) new_node_id node }
  where
    isFun = isFunTy (unwrapType $ idType id)
    -- | When we don't use descriptions we can avoid creating
    --  a new node for e.g. each literal string. So we set the
    --  id only if descriptions are enabled.
    set_if_desc :: FlowNode -> NodeId -> SDoc -> FlowNode
#if defined(WITH_NODE_DESC)
    set_if_desc node node_id desc = node { _node_desc = desc, node_id = node_id}
#else
    set_if_desc node _id _desc    = node
#endif

-- | Returns the nodeId for a given imported Id.
importedFuncNode_Maybe :: Module -> Id -> AM (Maybe NodeId)
importedFuncNode_Maybe this_mod var_id
    -- Not an imported function
    | nameIsLocalOrFrom this_mod (idName var_id)
    = return Nothing
    | otherwise = do
        s <- get
        case lookupUFM (fs_idNodeMap s) var_id of
            Just node_id -> return $! Just node_id
            Nothing -> pprPanic "Imported id not mapped" (ppr var_id)

-- Get or make a nodeId for the given Id based on context.
mkCtxtEntry :: ContextStack -> Id -> AM (Id,NodeId)
mkCtxtEntry ctxt v
    | Just nodeId <- idMappedInCtxt v ctxt
    = return $! (v,nodeId)
    | otherwise
    = do
        !node_id <- mkUniqueId
        return $! (v, node_id)

{-# NOINLINE nodesTopBinds #-}
-- Note: We could expose the computed information about top level bindings
-- via interface files (or otherwise). But currently it's unused even though we return it.
nodesTopBinds :: Module -> [StgTopBinding] -> AM ([InferStgTopBinding], (VarEnv NodeId))
nodesTopBinds this_mod binds = do
    -- We preallocate node ids for the case where we must reference an node by id
    -- before we traversed the defining binding. (e.g. recursive groups)

    -- TODO: bindersOfTopBinds allocates an intermediate list, but we really
    -- shouldn't need to.
    let bind_ids = bindersOfTopBinds binds :: [Id]
    mappings <- foldM insertIdMapping mempty bind_ids :: AM (VarEnv NodeId)
    let topCtxt = CTopLevel mappings
    binds' <- mapM (nodesTop this_mod topCtxt) binds
    return (binds', mappings)
  where
    insertIdMapping :: VarEnv NodeId -> Id -> AM (VarEnv NodeId)
    insertIdMapping env v
        | idType v `eqType` addrPrimTy
        = return $! extendVarEnv env v addrNodeId
        | otherwise
        = extendVarEnv env v <$!> mkUniqueId

nodesTop :: Module -> SynContext -> StgTopBinding -> AM InferStgTopBinding
nodesTop _this_mod _ctxt (StgTopStringLit v str) = return (StgTopStringLit v str)
    -- String literals (and unlifted ids in general) are never entered.
    -- There is also no nested information so we can represent them all
    -- with a single preallocted data flow node. The ids are mapped to this
    -- node in `nodesTopBinds`

nodesTop this_mod ctxt (StgTopLifted bind)  = do
    bind' <- fst <$> nodesBind this_mod [ctxt] TopLevel NotLNE bind :: AM InferStgBinding
    return $! (StgTopLifted bind')

-- nodesBind creates the nodeIds for the bound rhs, the actual nodes are created in
-- nodeRhs. Returns the context including the let.
nodesBind :: Module -> ContextStack -> TopLevelFlag -> IsLNE -> StgBinding -> AM (InferStgBinding, ContextStack)
nodesBind this_mod ctxt bot lne (StgNonRec v rhs) = do
    boundId <- uncurry unitVarEnv <$> mkCtxtEntry ctxt v
    let ctxt' = ((CLet boundId lne) `extendCtxt` ctxt)
    rhs' <- (nodeRhs this_mod ctxt' bot v rhs)
    return $! (StgNonRec v rhs', (CLetBody boundId lne) `extendCtxt` ctxt)
nodesBind this_mod ctxt bot lne (StgRec binds) = do
    let ids = map fst binds
    boundIds <- mkVarEnv <$> mapM (mkCtxtEntry ctxt) ids :: AM (VarEnv NodeId)
    let ctxt' = (CLetRec boundIds lne) `extendCtxt` ctxt
    rhss' <- mapM (uncurry (nodeRhs this_mod ctxt' bot )) binds
    return $! (StgRec $ zip ids rhss', (CLetRecBody boundIds lne) `extendCtxt` ctxt)


{-  Note [RhsCon data flow]
    ~~~~~~~~~~~~~~~~~~~~~~~

Describes rules for lets like this:

    let x = Con args@[a1 .. an]

The data flow visually looks something like this.

+-----+      +------+
| con |      | args |
+--+--+      +--+---+
   |         |
   v         v
   +---------+
   | rhsNode |
   +---------+

The behaviour here is very similar to the one for
nodeConApp with a few alterations to account for the fact
that the result will be associated with a binding. The major
difference being that the rhsNode and the binding will be represented
by a single dataflow node.

The EnterLattices of the arguments are taken as is and are put
into the FieldInfo of the rhsNode.
The only exception is that we set strict fields to
NeverEnter because of the strict field invariant.
See also Note [The strict field invariant]

Doing this is implemented in mkOutConLattice.

The enterinfo is determined by a number of rules, required to uphold [The strict field invariant].
The main drivers are:
1) By default we infer tagged (NeverEnter) for all constructors allocated via a StgRhsCon because any
   regular constructor allocation results in a tagged pointer to the Constructor.
2) Top level constructor applications might turn into thunks if we need force any of their
   arguments to uphold the strict field invariant.
3) Recursive groups where strict arguments are defined in the group are currently
   considered MaybeEnter.

The enterInfo of the result is determined by checking these conditions
in order:

a.1) If the binding is not defined at the top level
   and is a non recursive binding:
->  NeverEnter, we can just wrap the constructor application in a Case.

a.2) If the binding is not defined at the top level
   and is in a recursive binding group, but all strict args
   are defined outside of the recursive group:
->  NeverEnter, we can just wrap the constructor application in a Case.

b) If there are no strict fields
->  NeverEnter, see 1)

c) If all strict field arguments are tagged (NeverEnter)
-> NeverEnter, see 1)

d) If any strict field arguments are UndetEnterInfo
-> UndetEnterInfo, if we don't know if a strict argument is already tagged
                   then we don't know if we need to wrap this application in
                   a case.

e) Otherwise
-> MaybeEnter

    Examples:
    ~~~~~~~~

Condition a.1) Unevaluated values in strict fields.

        data StrictLazy a b = SL !a b
        foo =
            ...
            let a1 = undefined
            let a2 = ...
            ...
            let x = SL a1 a2
            ...

    enterInfo(x) = NeverEnter <NeverEnter, enterInfo(a2)>

    Since we deal with a local let binding we will (after the analysis has run)
    push the allocation of the constructor past the evaluation of the arguments
    like this:

        foo =
            ...
            let a1 = undefined
            let a2 = True
            ...
            case a1 of a1'
                DEFAULT ->
                    let x = SL a1' a2
                    ...

    This is essentially what the Worker for the constructor would do
    as well. We can always do this for *any* local non-recursive let.

    Assinging NeverEnter to the first field will seem odd at first. But
    it makes sense once we consider that enterInfo(x) represents information
    about `x` *after* allocation of the constructor. Should arguments of
    strict fields be bottom then the constructor will not be allocated at
    all so there is no conflict.

    For this reason we can *always* set the information for strict fields
    to NeverEnter.

    Should there be multiple strict fields we simple generate more than
    one wrapping case expression.

Condition a.1) Tagged (NeverEnter) arguments to strict fields

    Rule a.1 also applies when we don't need a wrapping case expression.
    As is the case if the argument is already tagged. For example in this
    code:

        data StrictLazy a b = SL !a b
        foo =
            ...
            let a1 = True -- Nullary constructors are always unlifted/tagged.
            let x = SL a1 a2
            ...

    Since we know a1 is tagged we don't need to insert a case and
    we get the following enterInfo:

    enterInfo(x) == NeverEnter <NeverEnter, enterInfo(a2)>

    This also applies if x would be bound at the top level.


Condition a.2) wrapping local recursive groups:

        data StrictLazy a b = SL !a b
        foo =
            ...
            let a1 = undefined
            ...
            letrec {
                x = SL a1 y;
                y = Just x
            ...

    This can be wrapped just like the example for Rule a.1):

        foo =
            ...
            let a1 = undefined
            ...
            case a1 of a1'
                DEFAULT ->
                    letrec {
                        x = SL a1' y;
                        y = Just x
                    ...

    and results in enterInfo(x) = NeverEnter <NeverEnter, enterInfo(y)>

Condition b) Lazy constructors are always marked as NeverEnter.

        baz =
            ...
            let foo = Con1 a1 a2 a3
            ...

        bar = Con2 a1' a2'

    Both foo and bar get NeverEnter as enter info. The field info is
    exactly the same as the arguments. eg.

    enterInfo(foo) = NeverEnter<enterInfo(a1), enterInfo(a2), enterInfo(a3)>

    This works for both top level and local constructor bindings. This is because
    when we allocate a regular constructor the result is a tagged pointer
    (to the constructor). Since all fields are lazy we also are not in danger of
    having to wrap the constructor in a case (which would turn it into a Thunk).

Condition c) Strict constructors with tagged arguments.

        foo =
            let a1 = True
            let a2 = False
            ...
            let x = StrictPair a1 a2
            ...

    We can trivially infer that a1/a2 will be tagged (as they are nullary constructors).
    With this information available we can infer that there is no danger of the rhs
    turning into a thunk and as such we can infer enterInfo(x) = NeverEnter <NeverEnter, NeverEnter>

Condition d) Strict field undetermined

    Assuming we have infered so far that:

        enterInfo(a1) = UndetEnterInfo
        enterInfo(a2) = UndetEnterInfo

    and analyse this snippet:

        foo = StrictPair a1 a2

    We don't know if the arguments are tagged. As consequence we
    don't know if Case wrapping is required.

    Should we require Case wrapping, the binding will turn into a thunk.
    Should we not require wrapping, the binding will be tagged (NeverEnter).

    But since we can't determine this from the current state we the infered result will
    be:
        enterInfo(foo) = UndetEnterInfo <NeverEnter, NeverEnter>

    Keep in mind that the enterInfo for the fields of `foo` is NeverEnter since strict
    fields always get a enterInfo of NeverEnter.

Condition e) Default: Fall back to MaybeEnter.

    If none of the other conditions matched *then* we fall back to MaybeEnter.

    For us to reach e) there must be strict fields and
    they must be applied to a binder with enterInfo of AlwaysEnter/MaybeEnter/NoValue.

    If there is a value of MaybeEnter/AlwaysEnter for one of the strict arguments
    we need to evaluate this argument before allocation. In order to do this we will turn
    this RhsCon into a RhsClosure. Turning the rhs into a thunk.

    For example we have inferred:

        enterInfo(thunk) = AlwaysEnter

    and are looking at this code:

        foo = StrictJust thunk

    We will infer enterInfo(foo) = MaybeEnter <NeverEnter> and rewrite the binding to

        foo = case thunk of x -> StrictJust x

    Which turns foo into a thunk.

    Note that this case also handles the situations where we apply bottoming bindings
    to strict constructors. For example if enterInfo(thunk) is NoValue this
    represents storing the result of a computation which will not return into a
    strict field. We can safely treat this the same as the AlwaysEnter/MaybeEnter case.

    For example we might have:

        loop x = loop x
        thunk = loop ()

        foo = StrictJust thunk

    Then we infer enterInfo(foo) = MaybeEnter <NeverEnter> and rewrite the AST
    to

        loop x = loop x
        thunk = loop ()

        foo = case thunk of x -> StrictJust x

    In practice hitting this condition is quite rare. But it can make code slightly worse
    as some constructor applications at the top level turn into thunks.

-}

-- | Dealing with let bound rhss.
--  We pass in the id to which the RHS is bound. This allows us to check
--  if the RHS is part of a recursive group.
nodeRhs :: HasDebugCallStack => Module -> ContextStack -> TopLevelFlag
        -> Id -> StgRhs
        -> AM (InferStgRhs)
nodeRhs this_mod ctxt topFlag binding (StgRhsCon _ ccs con args)
  | null args = do
        -- pprTraceM "RhsConNullary" (ppr con <+> ppr node_id <+> ppr ctxt)
        let node = mkConstNode node_id (EnterLattice NeverEnter FieldsNone)
                                       (ppr binding <-> text "rhsConNullary")
        markDone $ node
        return $! (StgRhsCon (node_id,RhsCon) ccs con args)
  | otherwise = do

        mapM_ (addImportedNode this_mod ) [v | StgVarArg v <- args]
        node_inputs <- mapM (getConArgNodeId ctxt) args :: AM [NodeId]
        -- pprTraceM "RhsCon" (ppr con <+> ppr node_id <+> ppr args <+> ppr node_inputs <+> ppr ctxt)
        let node =  FlowNode
                        { node_id = node_id
                        , node_inputs = node_inputs
                        --, node_done   = False
                        , node_result = undetLat
                        , node_update = node_update node_id node_inputs
#if defined(WITH_NODE_DESC)
                        , _node_desc = (ppr binding <-> text "rhsCon")
#endif
                        }
        addNode notDone node

        return $! (StgRhsCon (node_id,remainsConRhs) ccs con args)
  where
    node_id = getKnownIdNodeId ctxt binding
    !remainsConRhs
        | isTopLevel topFlag            = MaybeClosure
        -- a.1)
        | (CLet _ _ : _) <- ctxt        = RhsCon
        -- a.2)
        | (CLetRec binds _ : _) <- ctxt
        , not (binding `elemUFM` binds) = RhsCon
        -- strict argument defined in recursive group
        | otherwise                     = MaybeClosure

    node_update this_id node_inputs = do
        fieldResults <- mapM (lookupNodeResult) node_inputs
        let strictResults = getStrictConArgs con fieldResults
        let strictOuter = map enterInfo strictResults :: [EnterInfo]
        -- pprTraceM "RhsCon" (ppr con <+> ppr this_id <+> ppr fieldResults)
        -- See Note [RhsCon data flow]
        let outerTag
                -- a) If it's never turned into a closure it's always tagged.
                | remainsConRhs == RhsCon =
                    NeverEnter

                -- b) nothing to force
                | not $ any isMarkedStrict $ dataConRepStrictness con
                =   NeverEnter

                -- c) If all of the strict inputs are tagged so is the output.
                | all (==NeverEnter) strictOuter
                = NeverEnter

                -- d) Taggedness depends on the taggedness of the arguments.
                | any (== UndetEnterInfo) strictOuter
                = UndetEnterInfo

                -- e)
                | otherwise
                = MaybeEnter


        -- Strict fields need to marked as neverEnter here, even if their inputs are not.
        -- This is because once we scrutinise the result of this rhs they will have been tagged.
        let result = mkOutConLattice con outerTag fieldResults
        let cappedResult = widenToNestingLevel nestingLimit result
        updateNodeResult this_id cappedResult
        return $! cappedResult




{-

TODO: As future work we could try to analyze which arguments a function is called with and use this to
      enhance the results of the analysis. But for now we don't do so.

TODO: Partial applications

* Currently we don't really try to retain field information of partial applications.
* But it might be worth doing so. If we then eventually fully apply the thunk we
  might bind the fields of the result and use the field information at that point.
  But it's not a big win so I haven't spent the time.


    Note [RhsClosure data flow]
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~

    +---------+
    | rhsNode |  = <body>
    +---------+

This is rather simple:

We take enterInfo(body) and set the outermost enterInfo as follows:

1) NeverEnter iff <body> represents an absentError value. See [Taggedness of absentError]
2) AlwaysEnter iff there are no arguments. (It's a thunk).
3) NeverEnter otherwise. As functions aren't entered just applied.

We currently do not consider arguments at all, this would require a
guarantee that there are no call sites outside of this module.
Something currently not tracked by GHC.

nodeRhs really *only* modifies the outer enterInfo. Some examples:

Rule 1: Absent error:

    x = absentError foo
    => enterInfo(x) = NeverEnter

    ---

    bar = absentError foo
    x = bar
    => enterInfo(x) = absentError foo

Rule 2: AlwaysEnter:

    x = body@(case someThunk of y -> y)
    => enterInfo(x) = AlwaysEnter < enterInfo_ofFields(body)>

    That is we take whatever the result of analyzing body is, but we replace the
    outermost enterInfo with AlwaysEnter. This is the case for all thunks
    which are not absentError no matter how exactly the body looks like.

Rule 3: NeverEnter:

    f :: Int -> Int -> (Int,Int)
    f x y = body@(case x + y of bndr -> (bndr, bndr))
    => enterInfo(f) = NeverEnter < NeverEnter, NeverEnter >
                        ^ function    ^ bndr        ^ bndr

    The outermust enterInfo represents the (evaluated) function. We *omit* the enterInfo
    for the outermost enterInfo of the *value*. Because the result of a function is always
    evaluated so there is no need.

    For another example:

    f :: Int -> Int -> Maybe Int
    f x y = body@(Nothing)
    => enterInfo(f) = NeverEnter
                        ^ function, no fields so no more information than that.

-}

nodeRhs this_mod ctxt _topFlag binding (StgRhsClosure _ext _ccs _flag args body) = do
    (body', body_id) <- nodeExpr this_mod ctxt' body
    let node = FlowNode { node_id = node_id
                        , node_inputs = [body_id]
                        -- ^ We might infer things about nested fields once evaluated.
                        -- , node_done   = False
                        , node_result = EnterLattice enterInfo FieldsUndet
                        , node_update = node_update node_id body_id
#if defined(WITH_NODE_DESC)
                        , _node_desc = node_desc
#endif
                        }
    addNode notDone node
    return $! (StgRhsClosure _ext _ccs _flag args body')

  where
    node_id = getKnownIdNodeId ctxt binding
#if defined(WITH_NODE_DESC)
    node_desc
        | null args = text "rhsThunk:" <> (ppr binding)
        | otherwise = text "rhsFunc:" <> (ppr binding)
#endif
    -- We know nothing about the arguments.
    varMap = mkVarEnv (zip args (replicate arity unknownNodeId))
    ctxt' = (CClosureBody varMap `extendCtxt` ctxt)
    arity = length args
    enterInfo
        | isAbsentExpr body = NeverEnter
        | null args = AlwaysEnter
        | otherwise = NeverEnter      -- Thunks with arity > 0
                                    -- are only entered when applied.
    node_update this_id body_id = do
        bodyInfo <- lookupNodeResult body_id
        let result = setEnterInfo bodyInfo enterInfo
        let cappedResult = widenToNestingLevel nestingLimit result
        if hasFinalFields cappedResult
            then do
                node <- getNode this_id
                markDone $ node { node_result = cappedResult }
            else updateNodeResult this_id cappedResult
        return $! cappedResult

-- Constraints for possible STG expressions. Most are delegated to helper functions.
nodeExpr :: Module -> ContextStack -> StgExpr -> AM (InferStgExpr, NodeId)
nodeExpr this_mod ctxt (e@StgCase {})          = nodeCase this_mod ctxt e
nodeExpr this_mod ctxt (e@StgLet {})           = nodeLet this_mod ctxt e
nodeExpr this_mod ctxt (e@StgLetNoEscape {})   = nodeLetNoEscape this_mod ctxt e
nodeExpr this_mod ctxt (StgTick t e)           = do
    (e',nodeId) <- nodeExpr this_mod ctxt e
    return $! (StgTick t e', nodeId)
nodeExpr this_mod ctxt e@(StgConApp {})        = nodeConApp this_mod ctxt e
nodeExpr this_mod ctxt e@(StgApp {})           = nodeApp this_mod ctxt e
-- Do the boring ones right here
nodeExpr _ _ctxt  (StgLit lit)              = return $! (StgLit lit, litNodeId)
-- Not currently analysed, mostly deal with unlifted values anyway.
nodeExpr _ _ctxt  (StgOpApp op args res_ty) = return $! (StgOpApp op args res_ty, unknownNodeId)
nodeExpr _ _ctxt  (StgLam {})               = error "Invariant violated: No lambdas in STG representation."

{-  Note [Case Data Flow Nodes]
    ~~~~~~~~~~~~~~~~~~~~~~~~~~

Case expressions result in a few control flow nodes and constraints.

Information between the different nodes for a case expression
is best shown by example:

    case e1 of bndr
    {   C1 f1 f2 -> alt1;
        C2       -> alt2; }

This result in data flow nodes and data flow between them
as shown below.

                              Always NoEnter
+-----------+                   +--------+
| Scrutinee +---- fieldInfo --->+  bndr  |
+-----+-----+                   +--------+
      |
      +
  fieldInfo
      +     bind field info
      |      to alt bndrs
      v          |
      |    +-----------+         +--------+
      |    |           |         |        |
      +--->+  f1  f2   +-------->+  alt1  +--------+
      |    |           |         |        |        |
      |    +-----------+         +--------+        v            +-----------+
      |                              |        +----+----+       |           |
      v                    result of alt rhs  | Combine +------>+ Case Node |
      |                              |        +----+----+       |           |
      |                          +--------+        ^            +-----------+
      |                          |        |        |
      +--------->------->------->+  alt2  +--------+
                                 |        |
                                 +--------+

Enumerating the case constraints:

1) Case Binder:
   enterInfo(bndr) = setOuter(NeverEnter, enterInfo(scrutinee))
   Binders are like the scrutinee but evaluated, that is outermost enterInfo = NeverEnter.

2) Field binders:
   enterInfo(field_n)
     | isStrictField = setOuter(NeverEnter, field_n(enterInfo((scrutinee)))
     | otherwise = field_n(enterInfo(scrutinee))
   Bound fields get their info from the scrutinees fieldInfo, except for strict fields whos
   outermost enterInfo get's set to NeverEnter.

3) Case expression:
    enterInfo(caseExpr) = lub(enterInfo(alt1), enterInfo(alt2), ... ,enterInfo(altn))

How branches are combined is explained in the Note [The lattice element combinators]
and Note [Combining Branches].

Note that the enterInfo of case alternatives can depend on case binders/field binders
if they reference them. But they are determined just as any other expression otherwise.



Examples:

    data StrictLazy a b = SL !a b

    ... case e of e'
            StrictLazy strict lazy -> lazy :: Int
            Nothing -> alt2 :: Int

    Let's assume enterInfo(x) = MaybeEnter if we apply the constraints we get:

1) Case Binder:
    enterInfo(e') = setOuter(NeverEnter, enterInfo(e))
    => setOuter(NeverEnter, MayberEnter)
    => NeverEnter

2) Field binders:

    For the lazy field:

    enterInfo(lazy) = field_n(enterInfo(scrutinee))
    => field_2(MaybeEnter)
    => UndetEnterInfo  -- See Note [Lattice for tag analysis]

    For the strict field:

    enterInfo(strict) = setOuter(NeverEnter, field_n(enterInfo(scrutinee)))
    => setOuter(NeverEnter, field_1(MaybeEnter))
    => setOuter(NeverEnter, MaybeEnter)
    => NeverEnter -- Because of the strict field invariant.


3)  Case expression:

    let's assume enterInfo(lazy) = MaybeEnter, enterInfo(alt2) = NeverEnter

    enterInfo(caseExpression) = lub(alt1, alt2)
    => lub(lazy, alt2)
    => lub(MaybeEnter, NeverEnter)
    => lub(MaybeEnter)

-}

-- See Note [Case Data Flow Node]
nodeCase :: Module -> ContextStack -> StgExpr -> AM (InferStgExpr, NodeId)
nodeCase this_mod ctxt (StgCase scrut bndr alt_type alts) = do
    (scrut',scrutNodeId) <- nodeExpr this_mod (CCaseScrut `extendCtxt` ctxt) scrut
    bndrNodeId <- nodeCaseBndr scrutNodeId bndr
    let ctxt' = CCaseBndr (unitVarEnv bndr bndrNodeId) `extendCtxt` ctxt
    (alts', altNodeIds) <- unzip <$> mapM (nodeAlt this_mod ctxt' scrutNodeId) alts
    !caseNodeId <- mkJoinNode altNodeIds
    -- pprTraceM "Scrut, alts, rhss" $ ppr (scrut, scrutNodeId, altNodeIds, altsId)
    return $! (StgCase scrut' bndr alt_type alts' , caseNodeId)
nodeCase _ _ _ = panic "Impossible: nodeCase"


-- Take the result of the scrutinee and mark it as tagged.
nodeCaseBndr :: NodeId -> Id -> AM NodeId
nodeCaseBndr scrutNodeId _bndr = do
    !bndrNodeId <- mkUniqueId
    addNode notDone $ FlowNode
                        { node_id = bndrNodeId
                        , node_inputs = [scrutNodeId] --, node_done = False
                        , node_result = undetLat, node_update = updater bndrNodeId
#if defined(WITH_NODE_DESC)
                        , _node_desc = text "caseBndr" <-> parens (ppr scrutNodeId) <-> ppr _bndr
#endif
                        }
    return bndrNodeId
      where
        updater bndrNodeId = do
            scrutResult <- lookupNodeResult scrutNodeId
            let result = setEnterInfo scrutResult NeverEnter
            if hasFinalFields result
                then do
                    node <- getNode bndrNodeId
                    markDone $ node { node_result = result }
                else
                    updateNodeResult bndrNodeId result
            return $! result

nodeAlt :: HasDebugCallStack => Module -> ContextStack -> NodeId -> StgAlt -> AM (InferStgAlt, NodeId)
nodeAlt this_mod ctxt scrutNodeId (altCon, bndrs, rhs)
  | otherwise = do
    bndrMappings <- mkVarEnv <$> zipWithM mkAltBndrNode [0..] bndrs
    let ctxt' = (CAlt bndrMappings) `extendCtxt` ctxt
    (!rhs', !rhs_id) <- nodeExpr this_mod ctxt' rhs
    return $! ((altCon, bndrs, rhs'), rhs_id)

    where
        strictBnds :: [Id]
        strictBnds
          | DataAlt con <- altCon
          = getStrictConArgs con bndrs
          | otherwise = []

        -- Result for ONE of the bindings bound by the alt.
        -- Eg for an StgAlt of (Just, [foo], expr) we call mkAltBndrNode 0 foo
        mkAltBndrNode :: Int -> Id -> AM (Id,NodeId)
        mkAltBndrNode n bndr
          | isUnliftedType bndrTy
          , not (isUnboxedTupleType bndrTy)
          , not (isUnboxedSumType bndrTy)
          = do
                !node_id <- mkUniqueId
                addNode isDone litNode { node_id = node_id }
                return $! (bndr,node_id)
          | otherwise = do
                node_id <- mkUniqueId --Shadows existing binds
                let updater = do
                        scrut_res <- lookupNodeResult scrutNodeId :: AM EnterLattice
                        let bndr_res = (indexField scrut_res n)
                        let is_strict_field = elem bndr strictBnds
                        let result
                                | is_strict_field
                                -- Tag things coming out of strict binds
                                = setEnterInfo bndr_res NeverEnter
                                | otherwise = bndr_res
                        -- pprTraceM "Updating altBndr:" (ppr (node_id, result) $$
                        --         text "Input:" <+> ppr scrutNodeId $$
                        --         text "scrut_res" <+> ppr scrut_res $$
                        --         text "bndr_res" <+> ppr bndr_res )
                        let finalFields = hasFinalFields result
                        if (is_strict_field && finalFields) || (finalFields && enterInfo result == MaybeEnter)
                            then do
                                node <- getNode node_id
                                markDone $ node { node_result = result }
                            else
                                updateNodeResult node_id result
                        return $! result
                addNode notDone FlowNode
                    { node_id = node_id
                    , node_result = undetLat
                    -- , node_done = False
                    , node_inputs = [scrutNodeId]
                    , node_update = updater
#if defined(WITH_NODE_DESC)
                    , _node_desc = text "altBndr" <-> ppr altCon <-> ppr bndr
#endif
                    }
                return $! (bndr,node_id)
            where
                bndrTy = idType bndr

(<->) :: SDoc -> SDoc -> SDoc
(<->) a b = a <> char '_' <> b

-- Note [Let bindings and their context]

-- If we analyse a binding of the form:

--     let f x = e in body

-- then we analyze `e` in the context of CLet[Rec]
-- and `body` in the context of CLet[Rec]Body.

-- In each case the context carries the *same* mapping
-- of binding ids to node ids, however we use different
-- constructors in order to be able to differentiate between tail
-- call branches and regular references to an id.
-- See Note [Recursive Functions] for the details.

{-  Note [Let/ Data Flow]
    ~~~~~~~~~~~~~~~~~~~~

This is rather simple. For a construct like:

    expr@(let x = rhs in body)

We bind the result of the rhs to the variable (x) which binds it.
The result of the whole expression is equivalent to the result of
the `body` expression.

The variable can be referenced from the body or the rhs itself
for let recs.

That is for a let expression of the form

    letExpr@(let var = rhs in body)

We have only the constraints that:

1) enterInfo(var) = enterInfo(rhs)
2) enterInfo(letExpr) = enterInfo(body)

Implementation wise the body/rhs can reference the data flow node
for the bound variable in the SynContext.

-}


nodeLet :: Module -> ContextStack -> StgExpr -> AM (InferStgExpr, NodeId)
nodeLet this_mod ctxt (StgLet ext bind expr) = do
    (bind',ctxt') <- nodesBind this_mod ctxt NotTopLevel NotLNE bind
    (expr',node) <- nodeExpr this_mod ctxt' expr
    return $! (StgLet ext bind' expr', node)
nodeLet _ _ _ = panic "Impossible"

nodeLetNoEscape :: Module -> ContextStack -> StgExpr -> AM (InferStgExpr, NodeId)
nodeLetNoEscape this_mod ctxt (StgLetNoEscape ext bind expr) = do
    (bind',ctxt') <- nodesBind this_mod ctxt NotTopLevel LNE bind
    (expr',node) <- nodeExpr this_mod ctxt' expr
    return $! (StgLetNoEscape ext bind' expr', node)
nodeLetNoEscape _ _ _ = panic "Impossible"

{-  Note [ConApp Data Flow]
    ~~~~~~~~~~~~~~~~~~~~~~~

Information from the constructor (strict fields)
and arguments is used to determine the result.

The information from both the constructor and the given arguments "flow"
into the final node to determine the result.

+-----+       +------+
| con |       | args |
+--+--+       +--+---+
   |             |
   v             v
   +-------------+
   | ConApp node |
   +-------------+

The enterInfo from this node is never used directly as it only appears
in expression contexts. (Rhs of case alternative, closures or lets).
In cases like (StgRhsClosure ... expr), expr == StgConApp
we determine the enterInfo of the rhs based on the fact that it's a RhsClosure.
In other contexts it's enterInfo will never be used by another node.

So if we have an ConApp expression of the form: app@(Con arg1 arg2 ... argn) we
solve the constraint:

    enterInfo(app) == MaybeEnter < fieldInfo(arg1), fieldInfo(arg2), ... , fieldInfo(argn) >

    where fieldInfo(arg_n) is defined as:

    fieldInfo(field_n)
        | isStrictField(field_n) = setOuter(NeverEnter, enterInfo(arg_n))
        | otherwise = enterInfo(arg_n)


We set strict field to NeverEnter because of the strict field invariant.
See also Note [The strict field invariant].

Doing this is implemented in mkOutConLattice.

Example:

    Consider this snippet and already derived information:

    data StrictLazy a b = SL !a b

        ...
        let thunk_x = ... :: Bool
        let thunk_y = ... :: Bool

        ... app@(SL thunk_x thunk_y)

        enterInfo(thunk_x) = MaybeEnter
        enterInfo(thunk_y) = MaybeEnter


    Here we get enterInfo(app) = MaybeEnter   < NeverEnter,   MaybeEnter>
                                  ^from ConApp  ^from strict field  ^from enterInfo(thunk_y)
                                                 invariant.
-}
nodeConApp :: HasDebugCallStack => Module -> ContextStack -> StgExpr -> AM (InferStgExpr, NodeId)
nodeConApp this_mod ctxt (StgConApp _ext con args tys) = do
    node_id <- mkUniqueId
    mapM_ (addImportedNode this_mod) [v | StgVarArg v <- args]
    inputs <- mapM (getConArgNodeId ctxt) args :: AM [NodeId]
    let updater = do
            fieldResults <- mapM lookupNodeResult inputs :: AM [EnterLattice]
            let result = mkOutConLattice con MaybeEnter fieldResults
            -- pprTraceM "UpdateConApp:" $ ppr (node_id,result) <+> text "inputs:" <> ppr inputs
            updateNodeResult node_id result
            return $! result

    addNode notDone FlowNode
        { node_id = node_id
        , node_result = undetLat
        , node_inputs = inputs
        -- , node_done = False
        , node_update = updater
#if defined(WITH_NODE_DESC)
        , _node_desc = text "conApp"
#endif
        }

    return $! (StgConApp node_id con args tys, node_id)
nodeConApp _ _ _ = panic "Impossible"

{-  Note [App Data Flow]
    ~~~~~~~~~~~~~~~~~~~~

This is one of the more involved data flow constructs.
The actual flow if information is rather simple:

    `StgApp f arg`

Induces this data flow

+---+     +------+
| f |     | args |
+-+-+     +--+---+
  |          |
  v          v
  +----------+
  | app node |
  +----------+

However there are a lot of rules which go into how the "app node"
actually uses the information give. We check these conditions in order:

1) If f is imported:
->  We compute it's enterInfo as described by Note [Imported Ids]

2) If f is an absent expression:
->  We treat it as NeverEnter.

3) If f is a simple recursive tail call:
->  We mark the result as such: NoValue x RecFields
    See [Recursive Functions] for details.

4) If f is part of mutual recursive binds.
or is a unsaturated function call:
->  We throw up our hands and determine we know nothing.

5) If f is a unsaturated function call:
-> we also give up and infer no information.

6) If f is a saturated function:
->  We determine setOuter(MaybeEnter, enterInfo(f))

7) If f is not a function, and has no args:
->  We reuse the information of f

8)
In any other case:
->  We throw up our hands and determine we know nothing.

Examples:

1) Imported ids:

    e@(head xs)
    => enterInfo(e) = importedInfo(head)
    => NeverEnter

    Where importedInfo is implemented by `addImportedNode`

2) Absent error:

    e@(absentError foo)
    => enterInfo(e) = NeverEnter

4) Self-recursive calls:

    letrec {
        f x = ...
        g x = ...
        h x = ... expr@(g foo) ...
        }

    Here g is part of the recursive group so rule 7 triggers and given the context we derive:
        enterInfo(g foo) = MaybeEnter

5) Unsaturated functions:

    e@(f x), arity(f) > 1:

    enterInfo(f x) = MaybeEnter

    We don't retain field information of f. Since there is likely
    little gain for too much complexity. But we could revisit this.

6) Saturated function call:

    Given:

    f x = case x of _ -> (True,False)
    enterInfo(f) = NeverEnter < NeverEnter, NeverEnter >

    If we have an expression `e@(f x)` we derive:

    enterInfo(e)
    => setOuter(MaybeEnter, enterInfo(f))
    => setOuter(MaybeEnter, NeverEnter < NeverEnter, NeverEnter >)
    => MaybeEnter < NeverEnter, NeverEnter >

7) Variable expressions:

    For any variable expressions (e.g. expr@v) we derive enterInfo(expr) = enterInfo(v).

8) Other cases: We derive no information, that is for any other application we derive
    enterInfo(expr) = MaybeEnter

-}

nodeApp :: HasDebugCallStack => Module -> ContextStack -> StgExpr -> AM (InferStgExpr, NodeId)
nodeApp this_mod ctxt expr@(StgApp _ f args) = do
    mapM_ (addImportedNode this_mod) (f:[v | StgVarArg v <- args])
    maybeImportedFunc <- importedFuncNode_Maybe this_mod f
    case () of
        _
            | Just node_id <- maybeImportedFunc
            ->  return $! (StgApp node_id f args, node_id)
            | otherwise -> do
                node_id <- mkUniqueId
                let updater = do
                        !result <- mkResult

                        -- pprTraceM "Updating " (ppr node_id)
                        -- Try to peek into the function being applied
                        -- node <- getNode node_id
                        -- !input_nodes <- mapM getNode inputs
                        -- pprTraceM "AppFields" (ppr (f, result) <+> ppr node $$
                        --     text "inputs:" <+> ppr inputs $$
                        --     ppr input_nodes
                        --     )
                        if (null inputs || isFinalValue result )
                            -- We have collected the final result
                            then do
                                -- pprTraceM "Limiting nesting for " (ppr node_id)
                                node <- getNode node_id
                                markDone $ node { node_result = result }
                                return $! result
                            else do
                                updateNodeResult node_id result
                                return $! result

                addNode notDone $ FlowNode
                    { node_id = node_id, node_result = undetLat
                    , node_inputs = inputs
                    -- , node_done = False
                    , node_update = updater
#if defined(WITH_NODE_DESC)
                    , _node_desc = text "app" <-> ppr f <> ppr args
#endif
                    }

                return $! (StgApp node_id f args, node_id)
  where
    inputs
        | isAbsentExpr expr = []
        | isFun && (not isSat) = []
        | recTail = []
        | isFun && isSat = [f_node_id]
        | otherwise = [f_node_id]

    -- See Note [App Data Flow]
    mkResult :: AM EnterLattice
    mkResult
        | isAbsent =
            -- pprTrace "Absent:" (ppr f) $
            return $! nullaryLattice NeverEnter

        | isFun && (not isSat) = return $! maybeLat

        -- App in a direct self-recursive tail call context, returns nothing
        | recTail = return $! nullaryLattice NoValue

        | OtherRecursion <- recursionKind
        =   lookupNodeResult f_node_id

        | NoMutRecursion <- recursionKind =
            -- pprTrace "simpleRec" (ppr f) $
            lookupNodeResult f_node_id

        | isFun && isSat = (`setEnterInfo` MaybeEnter) <$!> lookupNodeResult f_node_id


        {- TODO: If we build a pap, but keep track of the field values we should
            be able to use these if it's fully applied later in the body. eg:

            case f x of pap ->
                let res = pap y in (resulting in tagged fields)
                if cond then Just <taggedThing> else res

            But we currently don't do so.
        -}
        | not isFun
        , null args
        = lookupNodeResult f_node_id

        | otherwise
        = return $! maybeLat

    recTail = recursionKind == NoMutRecursion && isRecTail f ctxt
    isFun = isFunTy (unwrapType $ idType f)
    arity = idFunRepArity f
    isSat = arity > 0 && (length args == arity)
    isAbsent = isAbsentExpr expr

    -- We check if f is imported using importedFuncNode_Maybe so this
    -- is guarantedd to be not imported when demanded.
    f_node_id = getKnownIdNodeId ctxt f

    recursionKind = getRecursionKind ctxt

    getRecursionKind [] = NoRecursion
    getRecursionKind ((CLetRec ids _) : _) | f `elemVarEnv` ids =
                if sizeUFM ids == 1 then NoMutRecursion else OtherRecursion
    getRecursionKind (_ : todo) = getRecursionKind todo
nodeApp _ _ _ = panic "Impossible"

-- Dep-sorting nodes is good for performance.
depSortNodes :: UniqFM NodeId FlowNode -> [FlowNode]
depSortNodes in_nodes = reversePayload [] . topologicalSortG $ graphFromEdgedVerticesUniq vertices
  where
    vertices = foldUFM mkVertex [] in_nodes :: [Node NodeId FlowNode]

    mkVertex :: FlowNode -> [Node NodeId FlowNode] -> [Node NodeId FlowNode]
    mkVertex n xs = (DigraphNode n (node_id n) (node_inputs n)) : xs

    reversePayload :: [FlowNode] -> [Node NodeId FlowNode] -> [FlowNode]
    reversePayload acc []       =  acc
    reversePayload acc (x:xs)   =
        let !x' = node_payload x
        in reversePayload (x':acc) xs

type NodeArray = IOArray Int FlowNode
type FlagArray = IOUArray Int Bool

solveConstraints :: HasDebugCallStack => DynFlags -> AM ()
solveConstraints dflags = do
        todos <- fs_uqNodeMap <$> get
        -- pprTraceM "sortSize" (ppr $ sizeUFM todos)
        -- let dep_sorted_nodes = nonDetEltsUFM todos
        let !dep_sorted_nodes = {-# SCC "nodeSorting" #-}
                                (depSortNodes todos) :: [FlowNode]
        iterate dep_sorted_nodes (undefined,undefined) 1

        uqList <- map snd . nonDetUFMToList . fs_uqNodeMap <$> get
        doneList <- map snd . nonDetUFMToList . fs_doneNodes <$> get
        let resultNodes =  (uqList ++ doneList)
        seq (unsafePerformIO $ GHC.Utils.Error.dumpIfSet_dyn dflags
                Opt_D_dump_stg_tag_nodes "STG Infered tags" FormatText
                (vcat $ map ppr resultNodes)) (return ())
        -- mapM_ (pprTraceM "node:" . ppr) resultNodes
        return ()
  where
    iterate :: [FlowNode] -> (NodeArray,FlagArray) -> Int -> AM ()
    iterate xs (arr, doneFlags) n = do
        pprTraceM "Pass:" $ (ppr (length xs)) <+> text "nodes remaining."
        !change <- liftIO $ newIORef False
        !xs' <- runUpdates change False xs
        progress <- liftIO $ readIORef change

        if (not progress)
            then return ()
            --max iterations
            else if (n > 5)
                then -- pprTraceM "Warning:" (text "Aborting at" <+> ppr n <+> text "iterations") >>
                     return ()
                else iterate xs' (arr,doneFlags) (n+1)

runUpdates :: IORef Bool -> Bool -> [FlowNode] -> AM [FlowNode]
runUpdates !_change _some_change [] = return []
runUpdates change some_change (node:nodes) = do
    -- !_ <- get
    !node_changed <- update node
    -- Avoid a write if we can
    when (not some_change && node_changed) $ do
        liftIO $ writeIORef change True

    node_done <- isMarkedDone (node_id node)
    if node_done
        then runUpdates change (some_change || node_changed) nodes
        else do
            pure (node:) <*> runUpdates change (some_change || node_changed) nodes

    where
        -- True <=> something changed.
        update :: FlowNode -> AM Bool
        update node = do
            let old_result = node_result node
            result <- node_update node
            done <- and <$> (mapM isMarkedDone (node_inputs node))
            let node' = node { node_result = result }
            when (done || result `nestingLevelOver` 12) (markDone node')
            if (result == old_result)
                -- Nothing to do this round
                then return False
                else do
                    return True



{-
------------------------------------------------------------
    Add cases around strict fields where required.
------------------------------------------------------------
-}

rewriteTopBinds :: [InferStgTopBinding] -> AM [TgStgTopBinding]
rewriteTopBinds binds = mapM (rewriteTop) binds

rewriteTop :: InferStgTopBinding -> AM TgStgTopBinding
rewriteTop (StgTopStringLit v s) = return $! (StgTopStringLit v s)
rewriteTop      (StgTopLifted bind)  = do
    (StgTopLifted . fst) <$!> (rewriteBinds bind)

-- For bot level binds, the wrapper is guaranteed to be `id`
rewriteBinds :: InferStgBinding -> AM (TgStgBinding, TgStgExpr -> TgStgExpr)
rewriteBinds (StgNonRec v rhs) = do
        (!rhs, wrapper) <-  rewriteRhs v rhs
        return $! (StgNonRec v rhs, wrapper)
rewriteBinds (StgRec binds) =do
        (rhss, wrappers) <- unzip <$> mapM (uncurry rewriteRhs) binds
        let wrapper = foldl1 (.) wrappers
        return $! (mkRec rhss, wrapper)
  where
    mkRec :: [TgStgRhs] -> TgStgBinding
    mkRec rhss = StgRec (zip (map fst binds) rhss)

-- | When dealing with a let bound rhs passing the id in allows us the shortcut the
--  the rule for the rhs tag to flow to the id
rewriteRhs :: Id -> InferStgRhs -> AM (TgStgRhs, TgStgExpr -> TgStgExpr)
rewriteRhs _binding (StgRhsCon (node_id,rewriteFlag) ccs con args) = do
    node <- getNode node_id
    fieldInfos <- mapM lookupNodeResult (node_inputs node)
    -- tagInfo <- lookupNodeResult node_id
    -- pprTraceM "rewriteRhsCon" $ ppr _binding <+> ppr tagInfo
    -- pprTraceM "rewriteConApp" $ ppr con <+> vcat [
    --     text "args" <+> ppr args,
    --     text "tagInfo" <+> ppr tagInfo,
    --     text "fieldInfos" <+> ppr fieldInfos
    --     -- text "strictIndices" <+> ppr strictIndices,
    --     -- text "needsEval" <+> ppr needsEval,
    --     -- text "evalArgs" <+> ppr evalArgs
    --     ]

    -- TODO: use zip3
    let strictIndices = getStrictConArgs con (zip [0..] fieldInfos) :: [(Int,EnterLattice)]
    let needsEval = map fst . filter (not . hasOuterTag . snd) $ strictIndices :: [Int]
    -- TODO: selectIndices is not a performant solution, fix that.
    let evalArgs = [v | StgVarArg !v <- selectIndices needsEval args] :: [Id]

    if (null evalArgs)
        then return $! (StgRhsCon noExtFieldSilent ccs con args, id)
        else do
            -- tagInfo <- lookupNodeResult node_id
            -- pprTraceM "Creating seqs (wrapped) for " $ ppr _binding <+> ppr node_id

            evaldArgs <- mapM mkLocalArgId evalArgs -- Create case binders
            let varMap = zip evalArgs evaldArgs -- Match them up with original ids
            let updateArg (StgLitArg lit) = (StgLitArg lit)
                updateArg (StgVarArg v)
                    | Just v' <- lookup v varMap
                    = StgVarArg v'
                    | otherwise = StgVarArg v
            let evaldConArgs = map updateArg args
            -- At this point iff, we have possibly untagged arguments
            -- and MaybeClosure as flag, we turn the result into a closure.
            if rewriteFlag == MaybeClosure
                then do
                    conExpr <- mkSeqs evalArgs con args (panic "mkSeqs should not need to provide types")
                    return $! (StgRhsClosure noExtFieldSilent ccs ReEntrant [] $! conExpr, id)
                else do
                    let evalExpr expr = foldr (\(v, vEvald) e -> mkSeq v vEvald e) expr varMap
                    return $! ((StgRhsCon noExtFieldSilent ccs con evaldConArgs), evalExpr)
rewriteRhs _binding (StgRhsClosure ext ccs flag args body) = do
    pure (,) <*>
        (StgRhsClosure ext ccs flag args <$> rewriteExpr False body) <*>
        pure id

type IsScrut = Bool

rewriteExpr :: IsScrut -> InferStgExpr -> AM TgStgExpr
rewriteExpr _ (e@StgCase {})          = rewriteCase e
rewriteExpr _ (e@StgLet {})           = rewriteLet e
rewriteExpr _ (e@StgLetNoEscape {})   = rewriteLetNoEscape e
rewriteExpr isScrut (StgTick t e)     = StgTick t <$!> rewriteExpr isScrut e
rewriteExpr _ e@(StgConApp {})        = rewriteConApp e

rewriteExpr isScrut e@(StgApp {})     = rewriteApp isScrut e
rewriteExpr _ (StgLit lit)           = return $! (StgLit lit)
rewriteExpr _ (StgOpApp op args res_ty) = return $! (StgOpApp op args res_ty)
rewriteExpr _ (StgLam {}) = error "Invariant violated: No lambdas in STG representation."

rewriteCase :: InferStgExpr -> AM TgStgExpr
rewriteCase (StgCase scrut bndr alt_type alts) =
    pure StgCase <*>
        rewriteExpr True scrut <*>
        pure bndr <*>
        pure alt_type <*>
        mapM rewriteAlt alts

rewriteCase _ = panic "Impossible: nodeCase"

rewriteAlt :: InferStgAlt -> AM TgStgAlt
rewriteAlt (altCon, bndrs, rhs) = do
    !rhs' <- rewriteExpr False rhs
    return $! (altCon, bndrs, rhs')

rewriteLet :: InferStgExpr -> AM TgStgExpr
rewriteLet (StgLet xt bind expr) = do
    (!bind', !wrapper) <- rewriteBinds bind
    !expr' <- rewriteExpr False expr
    return $! wrapper (StgLet xt bind' expr')
rewriteLet _ = panic "Impossible"

rewriteLetNoEscape :: InferStgExpr -> AM TgStgExpr
rewriteLetNoEscape (StgLetNoEscape xt bind expr) = do
    (!bind', wrapper) <- rewriteBinds bind
    !expr' <- rewriteExpr False expr
    return $! wrapper (StgLetNoEscape xt bind' expr')
rewriteLetNoEscape _ = panic "Impossible"

rewriteConApp :: InferStgExpr -> AM TgStgExpr
rewriteConApp (StgConApp nodeId con args tys) = do
    node <- getNode nodeId
    -- We look at the INPUT because the output of this node will always have tagged
    -- strict fields in the end.
    fieldInfos <- mapM lookupNodeResult (node_inputs node)
    let strictIndices = getStrictConArgs con (zip3 [(0 :: Int) ..] fieldInfos args) :: [(Int,EnterLattice, StgArg)]
    let needsEval = map fstOf3 . filter (not . hasOuterTag . sndOf3) $ strictIndices :: [Int]
    let evalArgs = [v | StgVarArg v <- selectIndices needsEval args] :: [Id]
    if (not $ null evalArgs)
        then do
            -- pprTraceM "Creating conAppSeqs for " $ ppr nodeId <+> parens ( ppr evalArgs ) -- <+> parens ( ppr fieldInfos )
            mkSeqs evalArgs con args tys
        else return $! (StgConApp noExtFieldSilent con args tys)

rewriteConApp _ = panic "Impossible"

rewriteApp :: IsScrut -> InferStgExpr -> AM TgStgExpr
rewriteApp True (StgApp nodeId f args)
    | null args = do
    tagInfo <- lookupNodeResult nodeId
    let !enter = (extInfo $ enterInfo tagInfo)
    return $! StgApp enter f args
  where
    extInfo AlwaysEnter       = -- pprTrace "alwaysEnter" (ppr f)
                                --   StgSyn.AlwaysEnter
                                -- Reenters evaluated closures too often
                                  StgSyn.MayEnter
    extInfo NeverEnter        = StgSyn.NoEnter
    extInfo MaybeEnter        = StgSyn.MayEnter
    extInfo NoValue          = StgSyn.MayEnter
    extInfo UndetEnterInfo    = StgSyn.MayEnter

rewriteApp _ (StgApp _ f args) = return $ StgApp MayEnter f args -- TODO? Also apply here?
rewriteApp _ _ = panic "Impossible"

----------------------------------------------
-- Deal with exporting tagging information

_exportTaggedness :: [(Id,NodeId)] -> AM [(Id, EnterLattice)]
_exportTaggedness xs = mapMaybeM export xs
    where
        export (v,nid)
            | isInternalName (idName v)
            = return Nothing
            | isUnliftedType (idType v)
            = return Nothing
            | otherwise
            = do
                !res <- lookupNodeResult nid
                return $ Just (v,res)

-- We would ideally replace ALL references to the evaluatee with the evaluted binding.
-- But for now we don't.
mkSeq :: Id -> Id -> TgStgExpr -> TgStgExpr
mkSeq id bndr !expr =
    -- pprTrace "mkSeq" (ppr (id,bndr)) $
    let altTy = mkStgAltType bndr [(DEFAULT, [], panic "Not used")]
    in
    StgCase (StgApp MayEnter id []) bndr altTy [(DEFAULT, [], expr)]

-- Create a ConApp which is guaranteed to evaluate the given ids.
mkSeqs :: [Id] -> DataCon -> [StgArg] -> [Type] -> AM TgStgExpr
mkSeqs untaggedIds con args tys = do
    argMap <- mapM (\arg -> (arg,) <$> mkLocalArgId arg ) untaggedIds :: AM [(InId, OutId)]
    -- mapM_ (pprTraceM "Forcing strict args before allocation:" . ppr) argMap
    let taggedArgs
            = map   (\v -> case v of
                        StgVarArg v' -> StgVarArg $ fromMaybe v' $ lookup v' argMap
                        lit -> lit)
                    args

    let conBody = StgConApp noExtFieldSilent con taggedArgs tys
    let body = foldr (\(v,bndr) expr -> mkSeq v bndr expr) conBody argMap
    return $! body

mkLocalArgId :: Id -> AM Id
mkLocalArgId id = do
    u <- getUniqueM
    return $! setIdUnique (localiseId id) u

-- These are inserted by the WW transformation and we treat them semantically as tagged.
-- This avoids us seqing them when we shouldn't.
-- See [Taggedness of absentError]
isAbsentExpr :: GenStgExpr p -> Bool
isAbsentExpr (StgTick _t e) = isAbsentExpr e
isAbsentExpr (StgApp _ f _)
  | idUnique f == absentErrorIdKey = True
  -- I'm not convinced that this via strictness is required for module-internal functions.
  -- But it's hard to proof otherwise so we just accept this overhead.
  | (_, Absent) <- splitStrictSig (idStrictness f)
  = True
isAbsentExpr _ = False
