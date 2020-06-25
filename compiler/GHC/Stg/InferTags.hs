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

{-# OPTIONS_GHC -O2 -ddump-simpl -ddump-to-file -ddump-stg -ddump-cmm -ddump-asm #-}

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
import GHC.Unit.Types --Module
import GHC.Types.Var.Set
import GHC.Core.TyCon (tyConDataCons)
import GHC.Core.Type
import GHC.Types.Unique.Supply
import GHC.Types.RepType
import GHC.Stg.Syntax as StgSyn hiding (AlwaysEnter)
import GHC.Stg.Utils

import GHC.Types.Name
import GHC.Builtin.Names


import GHC.Types.Unique
import GHC.Types.Unique.FM
import GHC.Types.Unique.Set (nonDetEltsUniqSet)
import GHC.Utils.Misc
import GHC.Utils.Monad.State -- See Note [Useless Bangs]
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

{-
    Note [Useless Bangs]
    ~~~~~~~~~~~~~~~~~~~~

    Ghcs state monad is lazy. So to avoid space leaks I've added bangs
    very liberally in this module. Some are bound to be useless, but this
    still beats having space leaks.

    The only place in this module where we explicitly depend on lazyness is the
    (unused) ty for case alternatives. So there is no harm in excessive bang
    annotations, at least not compared to space leaks.


    Note [Tag Inferrence - The basic idea]
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This code has two goals:
    * Ensure constructors with strict fields are only
      allocated with tagged pointers in the strict fields.
    * Infer if a given use site of an id is guaranteed to
      refer to a tagged pointer.

Id's represent by a tagged pointer do not have to be entered
when used in an case expression like `case id of ...` which
can have massive performance benefits for traversals of strict
data structures.

This Module contains the code for the actual inference and upholding
the strict field invariant. See Note [The strict field invariant]
Once computed the infered taggedness is stored in the extension
point of StgApp. This is then used in StgCmmExpr to determine if we
need to enter an expression.


The inference code has three major parts.

* Building an data flow graph based on the STG AST. This is done by the
  node* family of functions.
* Running the analysis on the data flow graph.
    + The driver of this is solveConstraints.
    + The actual update function for each node type is defined
      in it's respective node* function.
* Updating the AST based on the computed information. This is implemented
    by the rewrite* functions.
    + We update the extension points.
    + We also wrap allocations for constructors in
      seq statements if required to uphold the taggedness of strict
      fields.


    Note [The strict field invariant]
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The code in this module transforms the STG AST such
that all strict fields will only contain tagged values
with the exception of functions and absentError values.

This is done by adding seq's where required around allocation
of constructors with strict fields.

The purpose of this invariant is that it allows us to eliminate
the tag check when casing on values coming out of strict fields.

In particular when traversing the strict spine of a data structure
this eliminates a significant amount of code being executed leading
to significant speedups. (10% and up for the traversal!)

    Note [Absent error in strict fields]
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Note [Taggedness of absentError]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

WW under certain circumstances will determine that a strict
field is not used and allocate a temporary constructor with
an absentError inside the field.

So we have to take great care to avoid reentering them when
upholding the strict field invariant.

For this purpose we treat bindings binding such an expression
as being tagged in all cases.

This happens because when GHC reboxes a constructor, sometimes we
don't need to recreate the full value and can stub out fields
which are known to be unused.

For lifted types GHC places an absentError thunk in those fields
so if anything goes wrong we will get a runtime error instead of
unpredictable behaviour.

See Note [Absent errors] for the details on why we do this.

The bad news is this means that technically speaking the strict
field invariant is a lie. To avoid disaster striking when we
evaluate such values we check for these explicitly.

After all in the absence of bugs in the rest of GHC any such value
should never be evaluated, and therefore can be treated as already
evaluated without affecting correctness.

This means we might end up with code like this:

    $wf :: a -> b -> Result
    $wf a b = let c = absentError "ww_absent"
            in  g (MkT a b c)

Where we known g will not use the field c.

Since STG is in ANF any such binding will be a function application
of the known `absentError` id. Which makes it easy to check for this.

We then treat any binding of an absentError application as already evaluated.
Again this is safe as these bindings can't be evaluated in the absence of bugs
in the rest of GHC.

In the presence of bugs in the rest of GHC a debug build will still catch these
error when confirming validity of predicated taggedness. So this seems reasonable.

One issue arises with imported worker functions. What if we inline $wf
into a module B, but don't do so for c we have to ensure  that c
is still treated as tagged.

This might seem tricky, as we really want to expose this information from/to
optimized builds. But we do not want to force it on unoptimized ones.

Consider c to be defined in module A, and $wf to be inlined into B.
This gives us four cases to consider:

A & B not optimized:
    Since ww will not run on either, there is no way for this kind of reboxing to happen.
A & B optimized:
    A will be analyzed, and export taggedness info about c, B will import this info and
    treat c accordingly.
A optimized, B not optimized:
    If B is not optimized then the body of $wf can't end up in B, so this works out.
A not optimized, B not optimized:
    Since A is not optimised GHC won't expose any unfoldings, so again $wf and c can't
    end up in different modules and things work out.

-}


#define WITH_NODE_DESC

-- Shortcut comparisons if two things reference the same object.
maybeEq :: a -> a -> Bool
maybeEq x1 x2 = isTrue# (reallyUnsafePtrEquality# x1 x2)

-- Grow them trees:

-- Once all is said and done, will this still be a RhsCon?
data IsRhsCon = RhsCon | MaybeClosure deriving Eq

type instance BinderP       'InferTags = Id
type instance XRhsClosure   'InferTags = NoExtFieldSilent
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

We use a lattice for the tag analysis, with each update of a node
being monotonic towards the top of the lattice.

Lattice elements are cross products of two values.
Both of which are lattices themselves.

One lattice encodes if we have to enter an object when
we case on it in the form of `case id of ...` and looks
like this:

               NoValue
                  |
               /  |  \
              /   |   \
      AlwaysEnter |  NeverEnter
               \  |  /
                \ | /
              MaybeEnter
                  |
            UndetEnterInfo

It is also called the "outer" infor about a binding in places.

What these values represent is the requirement of needing
to evaluate a binding 'val' in a context like

    case val of
        True -> foo
        False -> bar

Which for values coincides with a value being tagged.

For functions it doesn't matter (operationally) how we treat them
as they can not be entered only called. However we try to assign
functions a value of NeverEnter, as it makes certain things more
consistent in the code.

NoValue is a special value assigned to expressions assigned to
expressions which can't be scrutinised at runtime.
A common example is the tail recursive branch in a recursive function.
See Note [Recursive Functions] for why we need this.
If we end up assigning NoValue to a *bindings* enterInfo then
this binding represents a computation which won't return as it
will tail call itself forever.
This happens for example for `f x = f x`.

NoValue is also a fixpoint in the lattice. Once a data flow node
has been assigned enterInfo NoValue it's enterInfo won't change
anymore, as the only way to arrive at this value in practice is
through nodes representing tail recursion, or by having a node
which only depends on such nodes.

NeverEnter means the object referenced by the binding won't ever be
entered as a *value*. It might be called as a function when applied
to arguments.

AlwaysEnter implies something is a thunk of some form. However since GC
can also evaluate certain forms of thunks we do currently not utilize it.

MaybeEnter represents the union of things where:
* We don't care about the enter behaviour
* We know we can't know the enter behaviour - function arguments
* We know it could be either enter or no-enter depending on
  branches taken at runtime.

# The FieldInfo Lattice

The second lattice represents information about bindings which are
bound to the fields of an id.
This is a mouthful so here is an example:

    let x = foo
    in case x of
        MyCon a b -> case a of
            <alts>

    We have determined foo has the field info lattice:
        `FieldsProd [(NeverEnter,fi1), (MaybeEnter,fi2)]
    So naturally the same is true of x.

    This means if we bind the second field of 'x' (here bound to 'a')
    then 'a' will have the information (NeverEnter,fi1) associated with it.

    This is independent of weither or not x needs to be entered, or even it's
    termination. As this information can only be used if x terminates.

So if we have "foo = Just bar" then this lattice will
encode the information we have about bar, potentially also with
nested information itself. In practice we limit to nesting to a certain depth
for performance reasons but a few levels deep can be very useful.

This lattice has this shape:

          LatNoValues
            /  |  \
   FieldsProd  |  FieldsSum
            \  |  /
         FieldsUntyped
               |
         FieldsUnknown
               |
           FieldsUndet

It's very much analog to the one for enterInfo.

Again we have a placeholder for lack of a value used
for recursive tail calls.
See Note [Recursive Functions] for details about that.

Some of these constructors represent an infinite number of fields
containing certain information:
* FieldsUndet represents a infinite number of undetermined values.
* FieldsUnknown represents a infinite number of values we can't know anything about.
* LatNoValues represents a infinite number of non-values.

* FieldsProd/FieldsSum/FieldsUntyped encode known information about fields.
  They are represented as a determined list of information. Fields not represented
  in the list have semantically the value noValue.

Again as example we might have

    x = Just Nothing.

The FieldLattice for 'x' would semantically be:

    FieldsSum ((NeverEnter,LatNoValues) : repeat (noValues))

However 'repeat (noValues)' is only implicitly encoded in our branch combinators.

This scheme is incredible useful when combining branches of constructors with a different
number of fields.

    Note [The lattice element combinators]
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We do use glb to combine multiple branches and this behaves as expected.

For field infos we combine field information pointwise.

When combining results from different branches we allow
combinations of different field counts. Usually promoting the field
info constructor to UntypedFields

    Combining branches with different constructors/types.
    -----------------------------------------------------

Eg we combine `Just False` and Nothing into the result
tagged<tagged>.

This might seem odd at a glance but is safe.

Consider for example two constructors C1 and C2 as rhs
in two branches alt1 and alt2.
C1 having n1 fields, C2 having n2 fields, and
n1 < n2.

For the first n1 fields we combine the lattices of the
results, which is safe. If they contradict each other
we simply assume a safe value on the lattice (MayEnter).

So this can not result in an error, and is in fact the same
behaviour as when comparing two branches consisting of the same
outermost constructor.

Now for the fields n1+1 .. n2 we assign them the value from
C2's branch. This is safe.

* A branch not matching on any constructor can't bind any fields.
  So this is trivially safe.
* Any branch matching on C1 will only bind up to n1 arguments.
  This means there is no chance of any of the values n1+1 ... n2
  to be scrutinzed in this branch.
* Any branch matching on C2 might bind the fields n+1 ... n2.
  But this is also safe.
  + If there are other branches with as many defined fields we would
    already have combined them to a safe element of the lattice.
  + If alt2 is the only such branch then we will always scrutinize the
    value created in this branch, so using the field results of alt for
    n1+1 .. n2 is also safe.
* Any other constellation can be again reduced to this case.

    Note [Recursive Functions]
    ~~~~~~~~~~~~~~~~~~~~~~~~~~

Consider this function:

    f x
        | x < 0 = Just $! Nothing
        | otherwise = f $ x-1

It's fairly obvious that it will return a value whos first
field will contain a tagged&evaluated value.

We deal with this by looking for branches in a functions body
which are recursive tail calls to the function itself.
In our domain we can pretend these branches do not return a value.
Indeed any value eventually produced by a function must come from a
branch NOT consisting of a recursive tail call.

For any of these tail calls we mark the result with the special
noValue element of the lattice. When combining these values they
always "give way" to the result of the non-recursive branches.

This is correct as after all we can approximate the result of the recursive
branch as a combination of all non-recursive branches.

It even works our for silly things like f x = f x, we infer a value
of noValue for `f x` which means if `f` is called in a branch somewhere else
that branch too will give way to the terminating branches.

Beyond this we are cautios when combining the result of branches.
bot/top combined with any other value will result in bot or top so
the result of multiple branches will only be inferred once all the
branches have been analyzed. As otherwise the not yet analyzed branches
will pull the result down to undetermined as well.

Implementation wise we check for tail calls by looking at the syntactic
context of function applications. This is implemented in `isRecTail`
and looks through join points (LNE) as well.

This does not solve the problem of mutual recursion however, in which
case we just throw our hands up in the air and simply assign the recursive
branch as undetermined.

TODO: Can we simply assign noValue here as well?

    We use glb to combine the branches:
    ------------

If we use glb to combine the two branches the following
happens:
* We initialize all nodes to bot.
* We compute the info tagged<tagged> for the non-recursive branch.
* We compute the info noValue for the tail recursive branch.
* Combining that with tagged from the other branch will result
  in the value tagged<tagged>.

We have reached the fixpoint bot at this point.

If we process the nodes in a different order then it might take
multiple iterations. But we make an effort to process nodes in
dependency order.

    Non tail recursive functions.
    -----------------------------

We have to deal with functions without a fixpoint in our lattice.
These are primarily functions producing infinite values like (f x = x : f x)

To handle these cases we do three things:
* We stop processing data flow nodes once
  their fields go beyond a certain depth.
* We process each node a limited number of times.
* We make sure that ANY intermediate result of the
  analysis is safe.

Intermediate results (before a fixpoint is reached) might
be less precise than our analysis allows for. But any state
will result in a correct program transformation as we never
overestimate enter behaviour.
That is we never predict we don't have to enter a binding if
we do.


    Note [Infering recursive tail calls]
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

When creating the data flow graph we also keep track of
the context in which an expression occurs.
This is required to deal with shadowing of ids but also allows
us to infer if a function application is in a tail call position.

The rules are not that complicated and for the most part implemented
in isRecTail, except for saturation of the call. They are similar to
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


bot :: EnterLattice
bot = EnterLattice UndetEnterInfo FieldsUndet
top :: EnterLattice
top = EnterLattice MaybeEnter FieldsUnknown
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

    Then we will infer taggedness of ![!], which is a tagged
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
    -- | Direct tail recursion, fields without arguments, the works.
    = FieldsNone

    -- | The associated value has up to (length fields) fields we know something
    -- about. But can have less fields! Or more fields!
    -- See Note [Lattice for tag analysis].
    | FieldsUntyped [EnterLattice]


    | FieldsProd [EnterLattice]

    -- Constructor the fields came out of
    | FieldsSum  !(Maybe DataCon) [EnterLattice]

    -- | At most we can say something about the tag of the value itself.
    --   The fields are impossible to known.
    | FieldsUnknown

    -- | We might find out more about the fields
    | FieldsUndet
    deriving (Eq,Generic)

instance NFData FieldInfo where
    rnf x = seq x ()

instance Outputable EnterLattice where
    ppr (EnterLattice enterInfo fieldInfo)
        = ppr enterInfo <> text " x " <> ppr fieldInfo

instance Outputable FieldInfo where
    ppr FieldsUnknown           = text "top"
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

combineEnterInfo :: EnterInfo -> EnterInfo -> EnterInfo
combineEnterInfo UndetEnterInfo _       = UndetEnterInfo
combineEnterInfo _ UndetEnterInfo       = UndetEnterInfo
combineEnterInfo MaybeEnter _           = MaybeEnter
combineEnterInfo _ MaybeEnter           = MaybeEnter
combineEnterInfo NoValue x              = x
combineEnterInfo x NoValue              = x
combineEnterInfo NeverEnter AlwaysEnter = MaybeEnter
combineEnterInfo AlwaysEnter NeverEnter = MaybeEnter
combineEnterInfo AlwaysEnter AlwaysEnter= AlwaysEnter
combineEnterInfo NeverEnter NeverEnter  = NeverEnter

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
-- We currently do combine results of different constructors
combineFieldInfos (FieldsProd fs1) (FieldsSum _ fs2) =
    FieldsUntyped $ combineFieldsUntyped fs1 fs2
combineFieldInfos (FieldsSum _ fs1) (FieldsProd fs2) =
    FieldsUntyped $ combineFieldsUntyped fs1 fs2

combineFieldInfos (FieldsSum c1 fs1)  (FieldsSum c2 fs2)
    | c1 /= c2  = FieldsUntyped $ combineFieldsUntyped fs1 fs2
    | otherwise = FieldsSum c1 $
                  zipWithEqual "SumInfo:combine" combineLattices fs1 fs2
combineFieldInfos (FieldsProd fs1) (FieldsProd fs2) =
    FieldsProd $ zipWithEqual "ProdInfo:combine" combineLattices fs1 fs2

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


-- Lattice when we know, and can only know, the outer layer.
flatLattice :: EnterInfo -> EnterLattice
flatLattice x = EnterLattice x FieldsUnknown

setEnterInfo :: HasDebugCallStack => EnterLattice -> EnterInfo -> EnterLattice
setEnterInfo lat@(EnterLattice enter fields) newEnter
    | enter == newEnter
    = lat
    | otherwise = EnterLattice newEnter fields

-- Lookup field of the returned valued.
-- Defaulting towards bot
-- Zero indexed
indexField :: EnterLattice -> Int -> EnterLattice
indexField lat n =
    case fieldInfo lat of
        FieldsUndet -> bot
        FieldsUnknown -> top
        FieldsNone -> noValue
        FieldsSum  _ fields -> getField fields
        FieldsProd fields   -> getField fields
        FieldsUntyped fields -> getField fields
  where
    getField fields =
        case drop n fields of
            -- We treat [] equal to [bot, bot, bot, ...]
            [] -> bot
            (x:_xs) -> x

hasOuterTag :: EnterLattice -> Bool
hasOuterTag lat = enterInfo lat == NeverEnter

-- We use these to stop iterating on nodes which are already at the top of the lattice.

hasTopFields :: EnterLattice -> Bool
hasTopFields lat = areTopFields $ fieldInfo lat

areTopFields :: FieldInfo -> Bool
areTopFields (FieldsUnknown ) = True
areTopFields (FieldsNone  )    = False
areTopFields (FieldsUndet)    = False
areTopFields (FieldsProd fields)  = all isTopValue fields
areTopFields (FieldsSum  _ fields) = all isTopValue fields
areTopFields (FieldsUntyped fields) = all isTopValue fields

isTopValue :: EnterLattice -> Bool
isTopValue lat = enterInfo lat == MaybeEnter && hasTopFields lat

nestingLevelOver :: EnterLattice -> Int -> Bool
nestingLevelOver _ 0 = True
nestingLevelOver (EnterLattice _ (FieldsProd fields)) n
    = any (`nestingLevelOver` (n-1)) fields
nestingLevelOver (EnterLattice _  (FieldsSum  _ fields)) n
    = any (`nestingLevelOver` (n-1)) fields
nestingLevelOver (EnterLattice _  (FieldsUntyped fields)) n
    = any (`nestingLevelOver` (n-1)) fields
nestingLevelOver _ _ = False

capAtLevel :: Int -> EnterLattice -> EnterLattice
capAtLevel _ l@(EnterLattice _ FieldsUnknown ) = l
capAtLevel _ l@(EnterLattice _ FieldsNone     ) = l
capAtLevel _ l@(EnterLattice _ FieldsUndet   ) = l
capAtLevel 0 _ = top
capAtLevel n (EnterLattice e (FieldsProd fields)) =
    EnterLattice e (FieldsProd (map (capAtLevel (n-1)) fields))
capAtLevel n (EnterLattice e (FieldsSum c fields)) =
    EnterLattice e (FieldsSum c $ map (capAtLevel (n-1)) fields)
capAtLevel n (EnterLattice e (FieldsUntyped fields)) =
    EnterLattice e (FieldsUntyped $ map (capAtLevel (n-1)) fields)


{-

Note [Taggedness of let bound constructors]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

By default a let bound StgRhsCon WILL result in a tagged binding.
However there are some exceptions:

* Imported non-nullary constructors.

    We don't store the tag in the Interface so can't recreate it - not tagged.

* Top level RhsCon with strict untagged arguments.

    In order these will only contain tagged references we have to turn them into
    functions who evaluate the possibly untagged arguments.

-}

-- | Nodes identified by their id have the result mapped back the STG
--   all other nodes get an unique and are only there for the analysis.
--   We also map certain ids to uniqe based id's if they might be shadowed.
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

-- TODO: The UniqSupply is only used during creation of the data flow nodes
--       so could be pulled out if performance becomes an issue.
data FlowState
    = FlowState
    { fs_us :: !UniqSupply
    , fs_idNodeMap :: !(UniqFM NodeId) -- ^ Map of imported id nodes (indexed by `Id`).
    , fs_uqNodeMap :: !(UniqFM FlowNode) -- ^ Transient results, index by `NodeId`
    , fs_doneNodes :: !(UniqFM FlowNode) -- ^ We can be sure these will no longer change, index by `NodeId`
    }

type AM = State FlowState

instance MonadUnique AM where
    getUniqueSupplyM = do
        s <- get
        let (us1,us2) = splitUniqSupply $ fs_us s
        put $! s {fs_us = us1}
        return us2
    getUniqueM = do
        s <- get
        let (!u,!us) = takeUniqFromSupply $! fs_us s
        put $! s {fs_us = us}
        return $ u

isDone :: Bool
isDone = True
notDone :: Bool
notDone = False

-- | Add new node, maybe mark it done.
addNode :: Bool -> FlowNode -> AM ()
addNode done node = do
    s <- get
    if done
        then put $! s { fs_doneNodes = addToUFM (fs_doneNodes s) node node
                     , fs_uqNodeMap = delFromUFM (fs_uqNodeMap s) node }
        else do
            ASSERTM( not <$> isMarkedDone (node_id node))
            put $! s { fs_uqNodeMap = addToUFM (fs_uqNodeMap s) node node }

-- | Move the node from the updateable to the finished set
markDone :: FlowNode -> AM ()
markDone node = do
    addNode isDone node

-- | Pessimistic check, defaulting to False when it's not clear.
isMarkedDone :: HasDebugCallStack => NodeId -> AM Bool
isMarkedDone id = do
    s <- get
    return $! elemUFM id (fs_doneNodes s)

updateNodeResult :: NodeId -> EnterLattice -> AM ()
updateNodeResult id result = do
    node <- (getNode id)
    addNode notDone (node {node_result = result})


getNode :: HasDebugCallStack => NodeId -> AM FlowNode
getNode node_id = do
    s <- get
    return $! fromMaybe
                   (pprPanic "Node not found" (ppr node_id))
                   (lookupUFM (fs_doneNodes s) node_id <|> lookupUFM (fs_uqNodeMap s) node_id)


-- TODO: Can we make sure we never try to query non-existing nodes?
lookupNodeResult :: HasDebugCallStack => NodeId -> AM EnterLattice
lookupNodeResult node_id = do
    s <- get
    let node = (lookupUFM (fs_uqNodeMap s) node_id <|>
                lookupUFM (fs_doneNodes s) node_id)
    case node of
        Nothing -> -- pprTraceM ("loopupNodeResult: Nothing\n" ++ prettyCallStack callStack) (ppr node_id) >>
                   return bot
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
    tagged< -- (,)
        tagged<tagged<top>>, -- Just (I# lit)
        unknown -- a
    >

However currently we do not keep nested field information inside constructors if the
values stored is a function, so this does not work.

TODO: This could be done, at the expense of compile time. Figure out of it's worth and do it if useful.

-}

-- | If we use a *function* as an unapplied argument to a constructor we throw
-- away nested information and make do with NeverEnter Top for now.
-- See Note [Field information of function ids]
getConArgNodeId :: HasDebugCallStack => [SynContext] -> StgArg -> AM NodeId
getConArgNodeId _    (StgLitArg _ ) = return litNodeId
getConArgNodeId ctxt (StgVarArg v )
    | isFunTy (unwrapType $ idType v)
    = return neverNodeId
    | otherwise
    = mkIdNodeId ctxt v

-- TODO: We could put the result into it's own map of NodeId -> EnterLattice
--       or even an array. But that complicates the code somewhat and performance
--       doesn't seem to be an issue currently.
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

set_desc :: FlowNode -> SDoc -> FlowNode
#if defined(WITH_NODE_DESC)
set_desc n desc = n { _node_desc = desc}
#else
set_desc n _ = n
#endif

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

-- Syntactic context of a node, potentially including a mapping
-- of in-scope ids to their data flow nodes.
data SynContext
    = CTopLevel     !(VarEnv NodeId)
    | CLetRec       !(VarEnv NodeId) !IsLNE
    | CLetRecBody   !(VarEnv NodeId) !IsLNE
    | CLet          !(VarEnv NodeId) !IsLNE
    | CLetBody      !(VarEnv NodeId) !IsLNE
    | CClosureBody  !(VarEnv NodeId)
    | CCaseScrut
    | CCaseBndr     !(VarEnv NodeId)
    | CAlt          !(VarEnv NodeId)
    | CNone
    deriving Eq

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
getCtxtIdMap (CNone) = Nothing

-- | isSingleMapOf v env == fromList [(v',_)] && v == v'
isSingleMapOf :: Id -> VarEnv NodeId -> Bool
isSingleMapOf v env =
    sizeUFM env == 1 && elemVarEnv v env

instance Outputable SynContext where
    ppr CNone                 = text "CNone"
    ppr (CTopLevel map)       = text "CTop"        <> ppr map
    ppr (CAlt map)            = text "CAlt"        <> ppr map
    ppr CCaseScrut            = text "CCaseScrut"
    ppr (CCaseBndr map)       = text "CCaseBndr"   <> ppr map
    ppr (CClosureBody map)    = text "CClosure"    <> ppr map
    ppr (CLetRec     ids lne) = text "CLetRec"     <> ppr lne <> ppr ids
    ppr (CLetRecBody ids lne) = text "CLetRecBody" <> ppr lne <> ppr ids
    ppr (CLet id lne)         = text "CLet"        <> ppr lne <> ppr id
    ppr (CLetBody id lne)     = text "CLetBody"    <> ppr lne <> ppr id

-- | Is the given id mapped to a data flow node by it's context?
idMappedInCtxt :: Id -> [SynContext] -> Maybe NodeId
idMappedInCtxt id ctxt
    = go ctxt
  where
    go (ctxt:_)
        | Just argMap <- getCtxtIdMap ctxt
        , Just node <- lookupVarEnv argMap id
        = Just $! node
    go (_:todo) = go todo
    go [] = Nothing

-- | Lub like operator between all input node
-- See Note [The lattice element combinators]
mkJoinNode :: [NodeId] -> AM NodeId
mkJoinNode []     = return unknownNodeId
mkJoinNode [node] = return node
mkJoinNode inputs = do
    node_id <- mkUniqueId
    let updater = do
            input_results <- mapM lookupNodeResult inputs
            let result = foldl1' combineLattices input_results
            if isTopValue result
                then do
                    node <- getNode node_id
                    markDone $ node { node_result = result }
                else updateNodeResult node_id result
            return $! result

    addNode notDone $ FlowNode { node_id = node_id, node_result = bot
                       , node_inputs = inputs -- , node_done = False
                       , node_update = updater
#if defined(WITH_NODE_DESC)
                       , _node_desc = text "branches"
#endif
                       }
    return $! node_id

-- Gives the lattice for evaluating con with arguments of the given taggedness.
-- | Take a lattice argument per constructor argument to simplify things.
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
findTags :: Module -> UniqSupply -> [StgTopBinding] -> ([TgStgTopBinding], [(Id,EnterLattice)])
-- findTags this_mod us binds = passTopBinds binds
findTags this_mod us binds =
    -- pprTrace "findTags" (ppr this_mod) $
    let state = FlowState {
            fs_us = us,
            fs_idNodeMap = mempty,
            fs_uqNodeMap = emptyUFM,
            fs_doneNodes = emptyUFM }
    -- Run the analysis
        (!binds', exports) = (flip evalState) state $ do
            addConstantNodes
            (binds',mapping) <- nodesTopBinds this_mod binds
            solveConstraints
            exports <- exportTaggedness mapping
            !finalBinds <- rewriteTopBinds binds'
            return (finalBinds, exports)
    in  (seqTopBinds binds') `seq`
            -- pprTrace "foundBinds" (ppr this_mod)
                (binds',exports)

-- passTopBinds :: [StgTopBinding] -> [TgStgTopBinding]
-- passTopBinds binds = map (passTop) binds

-- passTop :: StgTopBinding -> TgStgTopBinding
-- passTop (StgTopStringLit v s)   = StgTopStringLit v s
-- passTop (StgTopLifted bind)     = StgTopLifted (passBinds bind)

-- passBinds :: StgBinding -> TgStgBinding
-- passBinds (StgNonRec v rhs) = StgNonRec v (passRhs rhs)
-- passBinds (StgRec pairs)    = StgRec $ map (\(v,rhs) -> (v, passRhs rhs)) pairs

-- -- For top level lets we have to turn lets into closures.
-- passRhs :: StgRhs -> TgStgRhs
-- passRhs (StgRhsCon node_id ccs con args) = (StgRhsCon noExtFieldSilent ccs con args)
-- passRhs (StgRhsClosure ext ccs flag args body) =
--     StgRhsClosure ext ccs flag args $ passExpr body

-- passExpr :: StgExpr -> TgStgExpr
-- passExpr (StgCase scrut bndr ty alts) = StgCase (passExpr scrut) bndr ty (map passAlt alts)
-- passExpr (StgLet _ binds body)          = StgLet noExtFieldSilent (passBinds binds) (passExpr body)
-- passExpr (StgLetNoEscape _ binds body)  = StgLetNoEscape noExtFieldSilent (passBinds binds) (passExpr body)
-- passExpr (StgTick t e)     = StgTick t $ passExpr e
-- passExpr (StgConApp _ con args tys)     = StgConApp noExtFieldSilent con args tys

-- passExpr (StgApp _ f args)              =  StgApp MayEnter f args
-- passExpr (StgLit lit)                   = (StgLit lit)
-- passExpr (StgOpApp op args res_ty)      = (StgOpApp op args res_ty)
-- passExpr (StgLam {}) = error "Invariant violated: No lambdas in STG representation."

-- passAlt :: StgAlt -> TgStgAlt
-- passAlt (altCon, bndrs, rhs) = (altCon, bndrs, passExpr rhs)

-- Constant mappings
addConstantNodes :: AM ()
addConstantNodes = do
    markDone litNode
    markDone $ mkConstNode undetNodeId bot
    markDone $ mkConstNode unknownNodeId top
    markDone $ mkConstNode neverNodeId (flatLattice NeverEnter)
    markDone $ mkConstNode maybeNodeId (flatLattice MaybeEnter)
    markDone $ mkConstNode alwaysNodeId (flatLattice AlwaysEnter)


mkConstNode :: NodeId -> EnterLattice -> FlowNode
mkConstNode id !val =
    FlowNode
    { node_id = id
    , node_inputs = []
    --, node_done = True
    , node_result = val
    , node_update = (return $! val)
#if defined(WITH_NODE_DESC)
    , _node_desc = (text "const")
#endif

    }

-- Some nodes we can reuse.
litNodeId, undetNodeId, unknownNodeId, neverNodeId, maybeNodeId, alwaysNodeId :: NodeId
litNodeId       = NodeId $ mkUnique 'c' 2
undetNodeId     = NodeId $ mkUnique 'c' 3 -- Always returns bot
unknownNodeId   = NodeId $ mkUnique 'c' 4
neverNodeId     = NodeId $ mkUnique 'c' 5
maybeNodeId     = NodeId $ mkUnique 'c' 6
alwaysNodeId    = NodeId $ mkUnique 'c' 7


litNode :: FlowNode
litNode =
    (mkConstNode litNodeId (flatLattice NeverEnter))
#if defined(WITH_NODE_DESC)
        { _node_desc = text "lit" }
#endif

{-  Note [Imported Ids]
    ~~~~~~~~~~~~~~~~~~~

# Assigning data flow nodes to imported ids.

We want to keep our Ids a simple newtype around Unique.
This is "easy" for things brought into scope by AST nodes,
we put a mapping from the Id to the NodeId into SynContext
which we can use to look up the actual node.

However imported Id's can show up in any place in the AST.
We solve this by creating a Node and NodeId for each imported
id when we come across the id the first time.

The next time we come across the same id mkIdNodeId will check
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
    * MaybeEnter otherwise.

TODO: Once we can export taggedness in the Interface file we
will want to use the imported tag info instead of these rules.


    Note [Shadowing and NodeIds]
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Shadowing makes things slightly more complex.

For constructs potentially introducing a shadowing id like
case binders, or the binders of case alternatives we create
a new NodeId when traversing the AST. When we want to get the
nodeId for a particular id given a context we use mkLocalIdNodeId.

We use mkIdNodeId if the id might be imported, which runs in our
monad and adds it to a map of ids which maps Id -> NodeId.

-}

-- See Note [Shadowing and NodeIds]
mkIdNodeId :: HasDebugCallStack => [SynContext] -> Id -> AM NodeId
mkIdNodeId ctxt id
    | Just node <- idMappedInCtxt id ctxt
    = return $! node
    | otherwise = do
        s <- get
        return $! fromMaybe (pprPanic "Unmapped id" (ppr id)) $
            lookupUFM (fs_idNodeMap s) id

-- See Note [Shadowing and NodeIds]
mkLocalIdNodeId :: HasDebugCallStack => [SynContext] -> Id -> NodeId
mkLocalIdNodeId ctxt id
    | Just node <- idMappedInCtxt id ctxt
    = node
    | otherwise = pprPanic "Local id not mapped:" (ppr id)

mkUniqueId :: AM NodeId
mkUniqueId = NodeId <$> getUniqueM




-- | This adds nodes with information we can figure out about imported ids into the env.
--   Mimics somewhat what we do in StgCmmClosure.hs:mkLFImported
--   See also Note [Imported Ids]
addImportedNode :: Module -> Id -> AM ()
addImportedNode this_mod id
    -- Local id, it has to be mapped to an id via SynContext
    | nameIsLocalOrFrom this_mod (idName id) = return ()
    | otherwise = do
        node_id <- mkUniqueId
        s <- get
        let idNodes = fs_idNodeMap s
        -- Check if it is already cached.
        when (not $ elemUFM id idNodes) $ do
            let node
                    -- Functions tagged with arity are never entered as atoms.
                    | idFunRepArity id > 0
                    = (mkConstNode node_id (flatLattice NeverEnter))
                                `set_desc` (text "ext_func" <-> ppr id)

                    -- Known Nullarry constructors are also never entered
                    | Just con <- (isDataConWorkId_maybe id)
                    , isNullaryRepDataCon con
                    = (mkConstNode node_id (EnterLattice NeverEnter FieldsNone))
                                `set_desc` (text "ext_nullCon" <-> ppr id)

                    -- General case, a potentially unevaluated imported id.
                    | not isFun
                    = (mkConstNode node_id (flatLattice MaybeEnter))
                                `set_desc` (text "ext_unknown_enter" <-> ppr id)

                    -- May or may not be entered.
                    | otherwise
                    = (mkConstNode node_id (flatLattice MaybeEnter))
                                `set_desc` (text "ext_unknown" <-> ppr id)
            put $!
                s { fs_idNodeMap = addToUFM (fs_idNodeMap s) id node_id
                , fs_doneNodes = addToUFM (fs_doneNodes s) node node }
  where
    isFun = isFunTy (unwrapType $ idType id)

-- | Returns the nodeId for a given imported Id.
importedFuncNode :: Module -> Id -> AM (Maybe NodeId)
importedFuncNode this_mod var_id
    -- Not an imported function
    | nameIsLocalOrFrom this_mod (idName var_id)
    = return Nothing
    | otherwise = do
        s <- get
        case lookupUFM (fs_idNodeMap s) var_id of
            Just node_id -> return $! Just node_id
            Nothing -> pprPanic "Imported id not mapped" (ppr var_id)

mkCtxtEntry :: [SynContext] -> Id -> AM (Id,NodeId)
mkCtxtEntry ctxt v
    | Just nodeId <- idMappedInCtxt v ctxt
    = return $! (v,nodeId)
    | otherwise
    = do
        !node_id <- mkUniqueId
        return $! (v, node_id)

{-# NOINLINE nodesTopBinds #-}
nodesTopBinds :: Module -> [StgTopBinding] -> AM ([InferStgTopBinding], [(Id,NodeId)])
nodesTopBinds this_mod binds = do
    let top_level_binds = mkVarSet (bindersOfTopBinds binds) :: IdSet
    -- We preallocate node ids for the case where we must reference an node by id
    -- before we traversed the defining binding.
    let bind_ids = (nonDetEltsUniqSet top_level_binds)
    mappings <- mapM (mkCtxtEntry [CNone]) bind_ids :: AM [(Id,NodeId)]
    let topCtxt = CTopLevel $ mkVarEnv mappings
    binds' <- mapM (nodesTop this_mod topCtxt) binds
    return (binds', mappings)

nodesTop :: Module -> SynContext -> StgTopBinding -> AM InferStgTopBinding
nodesTop _this_mod ctxt (StgTopStringLit v str) = do
    -- String literals are never entered.
    let node_id = mkLocalIdNodeId [ctxt] v
    let node = mkConstNode node_id (flatLattice NeverEnter)
                    `set_desc` text "c_str"
    markDone node
    return $! (StgTopStringLit v str)
nodesTop this_mod ctxt (StgTopLifted bind)  = do
    bind' <- fst <$> nodesBind this_mod [ctxt] TopLevel NotLNE bind :: AM InferStgBinding
    return $! (StgTopLifted bind')

-- nodesBind creates the nodeIds for the bound rhs, the actual nodes are created in
-- nodeRhs. Returns the context including the let
nodesBind :: Module -> [SynContext] -> TopLevelFlag -> IsLNE -> StgBinding -> AM (InferStgBinding, [SynContext])
nodesBind this_mod ctxt top lne (StgNonRec v rhs) = do
    boundId <- uncurry unitVarEnv <$> mkCtxtEntry ctxt v
    let ctxt' = ((CLet boundId lne):ctxt)
    rhs' <- (nodeRhs this_mod ctxt' top v rhs)
    return $! (StgNonRec v rhs', (CLetBody boundId lne):ctxt)
nodesBind this_mod ctxt top lne (StgRec binds) = do
    let ids = map fst binds
    boundIds <- mkVarEnv <$> mapM (mkCtxtEntry ctxt) ids :: AM (VarEnv NodeId)
    let ctxt' = (CLetRec boundIds lne) : ctxt
    rhss' <- mapM (uncurry (nodeRhs this_mod ctxt' top )) binds
    return $! (StgRec $ zip ids rhss', CLetRecBody boundIds lne: ctxt)



{-
TODO: If we have a recursive let, but non of the recursive ids are in strict fields
      we and should can still tag the resulting let.

    For example:

    data Foo a = Foo Foo !a
    ...
    let x = Foo y bla
        y = Foo x blub
    in expr
    ...
    Should result in x and y being tagged with a wrapper like this:

    case bla of bla' ->
    case blub of blub' ->
        let x = Foo y bla'
            y = Foo x blub'
        in expr

-}

{-  Note [RhsCon data flow]
    ~~~~~~~~~~~~~~~~~~~~~~~

+-----+      +------+
| con |      | args |
+--+--+      +--+---+
   |         |         implicit
   v         v            |
   +---------+    +-------------+
   | rhsNode | == | bindingNode |
   +---------+    +-------------+

The behaviour here is very similar to the one for
nodeConApp with a few alterations to account for the fact
that the result will be associated with a binding.

The EnterLattices of the arguments are stored in the FieldInfo of the result.
The only exception is that we set the enterInfo for strict fields to
NeverEnter because of the strict field invariant.
See also Note [The strict field invariant]

Doing this is implemented in mkOutConLattice.

The enterinfo is determined by a number of rules.
The main drivers are:
1) By default we can tag all constructors allocated via a StgRhsCon
2) Top level constructors might turn into thunks if we need to wrap them in seqs
3) Recursive groups are not well supported yet in the first iteration

The enterInfo of the result is determined by checking these conditions
in order:

a.1) If the binding is not defined at the top level
   and is a non recursive binding:
->  NeverEnter, we can just wrap the expression in a Case.

a.2) If the binding is not defined at the top level
   and is in a recursive binding, but all strict args
   are defined outside of the recursive group:
->  NeverEnter, we can just wrap the expression in a Case.



b) If there are no strict fields
->  NeverEnter, see 1)

c) If all strict field arguments are tagged
-> NeverEnter, see 1)

d) If any strict field arguments are undetermined
-> UndetEnterInfo, if we don't know if a strict argument is already tagged
                   then we don't know if we need to wrap this allocation in
                   a seq.

e) Otherwise
-> MaybeEnter

For us to reach e) the strict fields must contain one of AlwaysEnter/MaybeEnter/NoValue.

If there is a value of MaybeEnter/NeverEnter for one of the strict arguments
we need to evaluate this argument before allocation.
In order to do this we will turn this RhsCon into a RhsClosure.
Removing the ability to tag the binding as it will turn into a thunk.

If there is a NoValue this represents storing the result of a computation
in a strict field which will not return.
We treat this the same as the AlwaysEnter/MaybeEnter/NoValue case.
Mostly since it means the constructr will never be allocated and as such
wrappting it in a seq is of little consequence.

-}

-- | When dealing with a let bound rhs passing the id in allows us the shortcut the
--  the rule for the rhs tag to flow to the id
nodeRhs :: HasDebugCallStack => Module -> [SynContext] -> TopLevelFlag
        -> Id -> StgRhs
        -> AM (InferStgRhs)
nodeRhs this_mod ctxt topFlag binding (StgRhsCon _ ccs con args)
  | null args = do
        -- pprTraceM "RhsConNullary" (ppr con <+> ppr node_id <+> ppr ctxt)
        let node = mkConstNode node_id (EnterLattice NeverEnter FieldsNone)
        markDone $ node `set_desc` (ppr binding <-> text "rhsConNullary")
        return $! (StgRhsCon (node_id,RhsCon) ccs con args)
  | otherwise = do

        mapM_ (addImportedNode this_mod ) [v | StgVarArg v <- args]
        node_inputs <- mapM (getConArgNodeId ctxt) args :: AM [NodeId]
        -- pprTraceM "RhsCon" (ppr con <+> ppr node_id <+> ppr args <+> ppr node_inputs <+> ppr ctxt)
        let node =  FlowNode
                        { node_id = node_id
                        , node_inputs = node_inputs
                        --, node_done   = False
                        , node_result = bot
                        , node_update = node_update node_id node_inputs
#if defined(WITH_NODE_DESC)
                        , _node_desc = (ppr binding <-> text "rhsCon")
#endif
                        }
        addNode notDone node

        return $! (StgRhsCon (node_id,remainsConRhs) ccs con args)
  where
    node_id = mkLocalIdNodeId ctxt binding
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
        let cappedResult = capAtLevel 10 result
        updateNodeResult this_id cappedResult
        return $! cappedResult




{- TODO:
    Is it worth to instantiate local thunks with their actual arguments
    or an approximation (lub) of them?

    This would require a notion of "internal" ideas beyond the concept of top level bindings.

TODO: Partial applications

* RhsCon is never partially applied
* If we can tell a RhsClosures is partially applied we know it's arity.
* This means we can assign the field info EVEN for partial results,
  we just have to make sure to only use field info for fully applied
  results.


    Note [RhsClosure data flow]
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~

    +--------------+
    |              |
    | closure expr | (case, let, Con, app, lit)
    |              |
    +--------------+
    |
    |                   implicit
    v                      |
    +---------+    +-------------+
    | rhsNode | == | bindingNode |
    +---------+    +-------------+

This is rather simple:

We assign the fields of the closures body to the rhsNode.

We mark it's enterInfo as:
    NeverEnter if it represents an absentError.
    AlwaysEnter if it takes no arguments.
    NeverEnter otherwise - Functions are called, not entered like values.

We currently do not consider arguments at all, this would require a
guarantee that there are no call sites outside of this module.
Something currently not tracked by GHC.

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
    node_id = mkLocalIdNodeId ctxt binding
#if defined(WITH_NODE_DESC)
    node_desc
        | null args = text "rhsThunk:" <> (ppr binding)
        | otherwise = text "rhsFunc:" <> (ppr binding)
#endif
    -- We know nothing about the arguments.
    varMap = mkVarEnv (zip args (replicate arity unknownNodeId))
    ctxt' = (CClosureBody varMap :ctxt)
    arity = length args
    enterInfo
        | isAbsentExpr body = NeverEnter
        | null args = AlwaysEnter
        | otherwise = NeverEnter      -- Thunks with arity > 0
                                    -- are only entered when applied.
    node_update this_id body_id = do
        bodyInfo <- lookupNodeResult body_id
        let result = setEnterInfo bodyInfo enterInfo
        let cappedResult = capAtLevel 10 result
        if hasTopFields cappedResult
            then do
                node <- getNode this_id
                markDone $ node { node_result = cappedResult }
            else updateNodeResult this_id cappedResult
        return $! cappedResult

nodeExpr :: Module -> [SynContext] -> StgExpr -> AM (InferStgExpr, NodeId)
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
-- Not currently analysed
nodeExpr _ _ctxt  (StgOpApp op args res_ty) = return $! (StgOpApp op args res_ty, unknownNodeId)
nodeExpr _ _ctxt  (StgLam {})               = error "Invariant violated: No lambdas in STG representation."

{-  Note [Case Data Flow Nodes]
    ~~~~~~~~~~~~~~~~~~~~~~~~~~

Case expressions result in a few control flow nodes.

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

This behaves mostly as expected.
A noteworthy detail is that for any alt binder coming out of a strict field
we set enterInfo to NeverEnter.
This is useful as it allows avoiding enter code for these bindings even when
the scrutinee is opaque to our analysis, like for example mutual recursive
functions.

How branches are combined is explained in Note [The lattice element combinators].

-}

-- See Note [Case Data Flow Node]
nodeCase :: Module -> [SynContext] -> StgExpr -> AM (InferStgExpr, NodeId)
nodeCase this_mod ctxt (StgCase scrut bndr alt_type alts) = do
    (scrut',scrutNodeId) <- nodeExpr this_mod (CCaseScrut:ctxt) scrut
    bndrNodeId <- nodeCaseBndr scrutNodeId bndr
    let ctxt' = CCaseBndr (unitVarEnv bndr bndrNodeId) : ctxt
    (alts', altNodeIds) <- unzip <$> mapM (nodeAlt this_mod ctxt' scrutNodeId) alts
    !caseNodeId <- mkJoinNode altNodeIds
    -- pprTraceM "Scrut, alts, rhss" $ ppr (scrut, scrutNodeId, altNodeIds, altsId)
    return $! (StgCase scrut' bndr alt_type alts' , caseNodeId)
nodeCase _ _ _ = panic "Impossible: nodeCase"


-- Take the result of the scrutinee and mark it as tagged.
nodeCaseBndr :: NodeId -> Id -> AM NodeId
nodeCaseBndr scrutNodeId _bndr = do
    !bndrNodeId <- mkUniqueId
    addNode notDone $ FlowNode  { node_id = bndrNodeId
                        , node_inputs = [scrutNodeId] --, node_done = False
                        , node_result = bot, node_update = updater bndrNodeId
#if defined(WITH_NODE_DESC)
                        , _node_desc = text "caseBndr" <-> parens (ppr scrutNodeId) <-> ppr _bndr
#endif
                        }
    return bndrNodeId
      where
        updater bndrNodeId = do
            scrutResult <- lookupNodeResult scrutNodeId
            let result = setEnterInfo scrutResult NeverEnter
            if hasTopFields result
                then do
                    node <- getNode bndrNodeId
                    markDone $ node { node_result = result }
                else
                    updateNodeResult bndrNodeId result
            return $! result

nodeAlt :: HasDebugCallStack => Module -> [SynContext] -> NodeId -> StgAlt -> AM (InferStgAlt, NodeId)
nodeAlt this_mod ctxt scrutNodeId (altCon, bndrs, rhs)
  | otherwise = do
    bndrMappings <- mkVarEnv <$> zipWithM mkAltBndrNode [0..] bndrs
    let ctxt' = (CAlt bndrMappings):ctxt
    (!rhs', !rhs_id) <- nodeExpr this_mod ctxt' rhs
    return $! ((altCon, bndrs, rhs'), rhs_id)

    where
        strictBnds
          | DataAlt con <- altCon
          = getStrictConArgs con bndrs
          | otherwise = []

        -- Result for ONE of the bindings bound by the alt.
        -- Eg for (Just, foo, expr) we call mkAltBndrNode 0 foo
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
                        let topFields = hasTopFields result
                        if (is_strict_field && topFields) || (topFields && enterInfo result == MaybeEnter)
                            then do
                                node <- getNode node_id
                                markDone $ node { node_result = result }
                            else
                                updateNodeResult node_id result
                        return $! result
                addNode notDone FlowNode
                    { node_id = node_id
                    , node_result = bot
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

This is rather simple:
We bind the result of the RHS to the variable which binds the rhs.
This can be referenced from the body or the rhs itself
for let recs. The overall result of the let expression
is the result of the body.

        +-------+
    +-> +  rhs  |
    |   +---+---+
            |
    ^       | result available
            | via the bindings id
    |       v
        +---+----+
    +-- +  var   |
        +---+----+
            v
        +---+----+    +----------+
        |  body  | == | let node |
        +--------+    +----------+

The interesting things happen inside the rhs.

-}


nodeLet :: Module -> [SynContext] -> StgExpr -> AM (InferStgExpr, NodeId)
nodeLet this_mod ctxt (StgLet ext bind expr) = do
    (bind',ctxt') <- nodesBind this_mod ctxt NotTopLevel NotLNE bind
    (expr',node) <- nodeExpr this_mod ctxt' expr
    return $! (StgLet ext bind' expr', node)
nodeLet _ _ _ = panic "Impossible"

nodeLetNoEscape :: Module -> [SynContext] -> StgExpr -> AM (InferStgExpr, NodeId)
nodeLetNoEscape this_mod ctxt (StgLetNoEscape ext bind expr) = do
    (bind',ctxt') <- nodesBind this_mod ctxt NotTopLevel LNE bind
    (expr',node) <- nodeExpr this_mod ctxt' expr
    return $! (StgLetNoEscape ext bind' expr', node)
nodeLetNoEscape _ _ _ = panic "Impossible"

{-  Note [ConApp Data Flow]
    ~~~~~~~~~~~~~~~~~~~~~~~

Information from the constructor used (strict fields)
and arguments is used to determine the result.

+-----+       +------+
| con |       | args |
+--+--+       +--+---+
   |             |
   v             v
   +-------------+
   | ConApp node |
   +-------------+

The enterInfo from this node is never used directly as it appears
in expression contexts. In cases like (StgRhsClosure ... expr), expr == StgConApp
we determine the enterInfo of the rhs based on the fact that it's a RhsClosure.
In other contexts it's enterInfo will never be used by another node.

But to keep the code simple we use MaybeEnter here.

As far as data flow rules are concerned this is rather simple.

The EnterLattices of the arguments are stored in the FieldInfo of the result.
The only exception is that we set the enterInfo for strict fields to
NeverEnter because of the strict field invariant.
See also Note [The strict field invariant]

Doing this is implemented in mkOutConLattice.




-}
nodeConApp :: HasDebugCallStack => Module -> [SynContext] -> StgExpr -> AM (InferStgExpr, NodeId)
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
        , node_result = bot
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

This is one of the more complicated data flow constructs.
The actual data flow is rather simple:

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
actually uses the information give.

If f is imported:
->  We use it's information unconditionally (See Note [Imported Ids])

If f is an absent expression:
->  We treat it as NeverEnter with Unknown field information.

If f is a simple recursive tail call:
->  We mark the result as such: NoValue x RecFields
    See [Recursive Functions] for details.

If f is potentially part of mutual recursive binds
or   is a unsaturated function call:
->  We throw up our hands and determine we know nothing.

If f is a saturated function:
->  We determine MaybeEnter x fieldInfoOf(f)

If f is not a function, and has no args:
->  We reuse the information of f

TODO: Under what circumstances can this happen?
In any other case:
->  We throw up our hands and determine we know nothing.

-}

nodeApp :: HasDebugCallStack => Module -> [SynContext] -> StgExpr -> AM (InferStgExpr, NodeId)
nodeApp this_mod ctxt expr@(StgApp _ f args) = do
    mapM_ (addImportedNode this_mod) (f:[v | StgVarArg v <- args])
    maybeImportedFunc <- importedFuncNode this_mod f
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
                        if (null inputs || isTopValue result )
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
                    { node_id = node_id, node_result = bot
                    , node_inputs = inputs
                    -- , node_done = False
                    , node_update = updater
#if defined(WITH_NODE_DESC)
                    , _node_desc = text "app" <-> ppr f <> ppr args
#endif
                    }

                return $! (StgApp node_id f args, node_id)
  where
    -- Determine if f in this position is a recursive tail call
    -- and as such safe to set to NoValue
    -- See Note [Recursive Functions]
    isRecTail :: [SynContext] -> Bool
    isRecTail (CTopLevel _ : _) = False
    isRecTail (CLetRec bnds _: _)
        | isSingleMapOf f bnds
        = True
    isRecTail (CLetRec _ LNE    : ctxt) = isRecTail ctxt
    isRecTail (CLetRec _ NotLNE : _   ) = False
    isRecTail (CLetRecBody bnds _ :ctxt)
        | f `elemVarEnv` bnds = False
        | otherwise = isRecTail ctxt
    isRecTail (CLet _ LNE    : ctxt) = isRecTail ctxt
    isRecTail (CLet _ NotLNE : _) = False
    isRecTail (CLetBody bnds _ : ctxt)
        | elemVarEnv f bnds = False
        | otherwise = isRecTail ctxt
    isRecTail (CClosureBody args : ctxt)
        | f `elemVarEnv` args = False
        | otherwise = isRecTail ctxt
    isRecTail (CCaseScrut : _) = False
    isRecTail (CCaseBndr bnd : ctxt )
        | elemVarEnv f bnd
        = False
        | otherwise
        = isRecTail ctxt
    isRecTail (CAlt bnds : ctxt)
        | f `elemVarEnv` bnds = False
        | otherwise = isRecTail ctxt
    isRecTail (CNone : _) = panic "impossible"
    isRecTail x = pprPanic "Incomplete" $ ppr x

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
            return $! flatLattice NeverEnter

        | isFun && (not isSat) = return $! top

        -- App in a direct self-recursive tail call context, returns nothing
        | recTail = return $! EnterLattice NoValue FieldsNone

        | OtherRecursion <- recursionKind
        =   lookupNodeResult f_node_id

        | NoMutRecursion <- recursionKind =
            -- pprTrace "simpleRec" (ppr f) $
            lookupNodeResult f_node_id

        | isFun && isSat = (`setEnterInfo` MaybeEnter) <$!> lookupNodeResult f_node_id


        -- TODO: If we build a pap, but keep track of the field values we should
        -- be able to use these if it's fully applied later in the body.
        {- eg:
            case f x of pap ->
            let res = pap y (resulting in tagged fields)
            if cond then Just <taggedThing> else res
        -}
        | not isFun
        --, ASSERT null args -- Otherwise data is applied to arguments.
        , null args
        = lookupNodeResult f_node_id

        | otherwise
        =   -- pprTrace "Unsat?" (ppr (f,args)) $
            return $! top

    recTail = recursionKind == NoMutRecursion && isRecTail ctxt
    isFun = isFunTy (unwrapType $ idType f)
    arity = idFunRepArity f
    isSat = arity > 0 && (length args == arity)
    isAbsent = isAbsentExpr expr

    -- We check if f is imported using importedFuncNode so this
    -- is guarantedd to be not imported when demanded.
    f_node_id = mkLocalIdNodeId ctxt f

    recursionKind = getRecursionKind ctxt

    getRecursionKind [] = NoRecursion
    getRecursionKind ((CLetRec ids _) : _) | f `elemVarEnv` ids =
                if sizeUFM ids == 1 then NoMutRecursion else OtherRecursion
    getRecursionKind (_ : todo) = getRecursionKind todo
nodeApp _ _ _ = panic "Impossible"

sccNodes :: [FlowNode] -> [FlowNode]
-- sccNodes in_nodes = in_nodes
sccNodes in_nodes = reverse . map node_payload . topologicalSortG $ graphFromEdgedVerticesUniq vertices
  where
    vertices = map mkVertex in_nodes :: [Node NodeId FlowNode]
    mkVertex :: FlowNode -> Node NodeId FlowNode
    mkVertex n = DigraphNode (n) (node_id n) (node_inputs n)

solveConstraints :: HasDebugCallStack => AM ()
solveConstraints = do
        -- uqCount <- sizeUFM . fs_uqNodeMap <$> get
        -- doneCount <- sizeUFM . fs_doneNodes <$> get

        -- idList <- map snd . nonDetUFMToList . fs_idNodeMap <$> get
        -- uqList <- map snd . nonDetUFMToList . fs_uqNodeMap <$> get
        -- doneList <- map snd . nonDetUFMToList . fs_doneNodes <$> get
        -- -- mapM_ (pprTraceM "node:" . ppr) (idList ++ uqList ++ doneList)
        -- pprTraceM "Initial: (uqList, doneList)" (ppr (uqCount, doneCount))
        -- pprTraceM "IterateStart" empty
        iterate 1
        -- iterate (-11)

        -- uqCount <- sizeUFM . fs_uqNodeMap <$> get
        -- doneCount <- sizeUFM . fs_doneNodes <$> get
        -- pprTraceM "ListLengthsFinal" $ ppr (uqCount, doneCount)

        -- remainingNodes <- nonDetEltsUFM . fs_uqNodeMap <$> get
        -- mapM_ (pprTraceM "UnfinishedNodes:" . ppr) $ remainingNodes


        uqList <- map snd . nonDetUFMToList . fs_uqNodeMap <$> get
        doneList <- map snd . nonDetUFMToList . fs_doneNodes <$> get
        let resultNodes =  (uqList ++ doneList)
        seq (unsafePerformIO $ GHC.Utils.Error.dumpIfSet_dyn unsafeGlobalDynFlags
                Opt_D_dump_stg_tag_nodes "STG Infered tags" FormatText
                (vcat $ map ppr resultNodes)) (return ())
        -- mapM_ (pprTraceM "node:" . ppr) resultNodes
        return ()
  where
    iterate :: Int -> AM ()
    iterate n = do
        -- pprTraceM "iterate - pass " (ppr n)
        uqNodes <- fs_uqNodeMap <$> get
        -- return $! seqEltsUFM rnf uqNodes
        -- pprTraceM "IterateUndone:" $ ppr (sizeUFM uqNodes)

        progress <- or <$> (mapM update (sccNodes . nonDetEltsUFM $ uqNodes)) :: AM Bool
        if (not progress)
            then return ()
            --max iterations
            else if (n > 5)
                then -- pprTraceM "Warning:" (text "Aborting at" <+> ppr n <+> text "iterations") >>
                     return ()
                else iterate (n+1)

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

-- For top level binds, the wrapper is guaranteed to be `id`
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

exportTaggedness :: [(Id,NodeId)] -> AM [(Id, EnterLattice)]
exportTaggedness xs = mapMaybeM export xs
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

-- rewriteTopBinds :: [InferStgTopBinding] -> AM [TgStgTopBinding]
-- rewriteTopBinds binds = mapM (rewriteTop) binds

-- rewriteTop :: InferStgTopBinding -> AM TgStgTopBinding
-- rewriteTop (StgTopStringLit v s) = return $! (StgTopStringLit v s)
-- rewriteTop (StgTopLifted bind)  = do
--     let ids = stgBindIds bind
--     let node_ids =


--     (StgTopLifted . fst) <$!> (rewriteBinds bind)

-- -- For top level binds, the wrapper is guaranteed to be `id`
-- rewriteBinds :: InferStgBinding -> AM (TgStgBinding, TgStgExpr -> TgStgExpr)
-- rewriteBinds (StgNonRec v rhs) = do
--         (!rhs, wrapper) <-  rewriteRhs v rhs
--         return $! (StgNonRec v rhs, wrapper)
-- rewriteBinds (StgRec binds) =do
--         (rhss, wrappers) <- unzip <$> mapM (uncurry rewriteRhs) binds
--         let wrapper = foldl1 (.) wrappers
--         return $! (mkRec rhss, wrapper)
--   where
--     mkRec :: [TgStgRhs] -> TgStgBinding
--     mkRec rhss = StgRec (zip (map fst binds) rhss)

----------------------------------------------
-- Utilities for rewriting RhsCon to ConClosure

-- We should really replace ALL references to the evaluatee with the evaluted binding.
-- Not just in the constructor args.

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
-- This avoids us seqing them again.
isAbsentExpr :: GenStgExpr p -> Bool
isAbsentExpr (StgTick _t e) = isAbsentExpr e
isAbsentExpr (StgApp _ f _)
  | idUnique f == absentErrorIdKey = True
isAbsentExpr _ = False
