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

module GHC.Stg.InferTags.Types

where

#include "HsVersions.h"

#if defined(DEBUG)
#define WITH_NODE_DESC
#endif

import GHC.Prelude


import GHC.Core.DataCon
import GHC.Types.Id
import GHC.Types.Unique.Supply
import GHC.Types.Unique
import GHC.Types.Unique.FM
import GHC.Types.Var.Env

import GHC.Utils.Binary hiding (put, get)
import qualified GHC.Utils.Binary as B

import GHC.Utils.Outputable
import GHC.Utils.Misc
import GHC.Utils.Monad
import GHC.Utils.Panic
import GHC.Data.Maybe

import GHC.Stg.Syntax as StgSyn hiding (AlwaysEnter)

import GHC.Generics
import GHC.Stack

import GHC.Exts (reallyUnsafePtrEquality#, isTrue#, -- Shortcutting comparisons
                 State#, RealWorld, runRW#, oneShot )
import GHC.IO (IO(..))
import Data.Ord (comparing)
import Control.Applicative hiding (empty)
import Control.Monad
import Control.DeepSeq

import GHC.Driver.Ppr (pprTraceM)

-----------------------------------------------------------
--  Various utilities that should probably live elsewhere
-----------------------------------------------------------

-- | a <-> b == a <> char ' ' <> char b
(<->) :: SDoc -> SDoc -> SDoc
(<->) a b = a <> char '_' <> b

-- | TODO: Does this fire often enough to be worthwhile?
maybeEq :: a -> a -> Bool
maybeEq x1 x2 = isTrue# (reallyUnsafePtrEquality# x1 x2)

-----------------------------------------------------------
--  Required extensions to the STG AST
-----------------------------------------------------------

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

-----------------------------------------------------------
--  Types used by and for the Analysis
-----------------------------------------------------------

-- | Can we guarantee this RhsCon binding will remain a RhsCon?
data IsRhsCon
    = RhsCon        -- ^ Local bindings, nullary constructors.
    | MaybeClosure  -- ^ Top level bindings.
    deriving Eq

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

    This is independent of weither or not x needs to be entered, or even its
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
of all its branches. Let's look at an easy case first:

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

This is correct as *when the function returns* its result must be
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
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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

{-
Note [Comparing Sums and Products]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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

{-
Note [FieldInfo Binary instance]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
TODO: Either put this info into interface files, or remove the instances.

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

-- Returns true if the lattice element represents a known-tagged value.
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
nestingLevelOver lat depth
    | depth <= 0 = True
    | otherwise = case lat of
        EnterLattice _ fieldLattice ->
            case fieldLattice of
                FieldsProd    fields -> any (`nestingLevelOver` (depth-1)) fields
                FieldsSum   _ fields -> any (`nestingLevelOver` (depth-1)) fields
                FieldsUntyped fields -> any (`nestingLevelOver` (depth-1)) fields
                FieldsNone           -> False
                FieldsUndet          -> False
                FieldsUnknown        -> False


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


-----------------------------------------------------------
-- Data flow state, Monad and Nodes
-----------------------------------------------------------

isDone :: Bool
isDone = True
notDone :: Bool
notDone = False



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

setNodeDesc :: SDoc -> FlowNode -> FlowNode
#if defined(WITH_NODE_DESC)
setNodeDesc doc n = n { _node_desc = doc }
#else
setNodeDesc _ n = n
#endif

hasNodeDesc :: Bool
#if defined(WITH_NODE_DESC)
hasNodeDesc = True
#else
hasNodeDesc = False
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

-- TODO: It would be nice to use .dot syntax here to allow graphviz renderings
-- of the data flow graph.
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

dumpNodes :: [FlowNode] -> SDoc
dumpNodes nodes =
    text "digraph tag-flow-nodes {" <> char '\n' <>
    vcat (map ppr_node_deps nodes) <>
    text "}"
    where
        ppr_node_deps node =
            ppr (node_id node) <> text "[label=\"" <> ppr node <> text "\"];" <> char '\n' <>
                vcat ( map (mk_dep (node_id node)) (node_inputs node))
        mk_dep node_id in_id = ppr in_id <> text " -> " <> ppr node_id <> char ';';

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



-----------------------------------------------------------
-- Data flow state, Monad and Nodes
-----------------------------------------------------------

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

mkUniqueId :: AM NodeId
mkUniqueId = NodeId <$> getUniqueM

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

