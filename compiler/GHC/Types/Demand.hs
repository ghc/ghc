
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE PatternSynonyms #-}

{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998
-}

-- | A language to express the evaluation context of an expression as a
-- 'Demand' and track how an expression evaluates free variables and arguments
-- in turn as a 'DmdType'.
--
-- Lays out the abstract domain for "GHC.Core.Opt.DmdAnal".
module GHC.Types.Demand (
    -- * Demands
    Boxity(..),
    Card(C_00, C_01, C_0N, C_10, C_11, C_1N), CardNonAbs, CardNonOnce,
    Demand(AbsDmd, BotDmd, (:*)),
    SubDemand(Prod, Poly), mkProd, viewProd,
    -- ** Algebra
    absDmd, topDmd, botDmd, seqDmd, topSubDmd,
    -- *** Least upper bound
    lubCard, lubDmd, lubSubDmd,
    -- *** Greatest lower bound
    glbCard,
    -- *** Plus
    plusCard, plusDmd, plusSubDmd,
    -- *** Multiply
    multCard, multDmd, multSubDmd,
    -- ** Predicates on @Card@inalities and @Demand@s
    isAbs, isAtMostOnce, isStrict,
    isAbsDmd, isAtMostOnceDmd, isStrUsedDmd, isStrictDmd,
    isTopDmd, isWeakDmd, onlyBoxedArguments,
    -- ** Special demands
    evalDmd,
    -- *** Demands used in PrimOp signatures
    lazyApply1Dmd, lazyApply2Dmd, strictOnceApply1Dmd, strictManyApply1Dmd,
    -- ** Other @Demand@ operations
    oneifyCard, oneifyDmd, strictifyDmd, strictifyDictDmd, lazifyDmd, floatifyDmd,
    peelCallDmd, peelManyCalls, mkCalledOnceDmd, mkCalledOnceDmds, strictCallArity,
    mkWorkerDemand, subDemandIfEvaluated,
    -- ** Extracting one-shot information
    callCards, argOneShots, argsOneShots, saturatedByOneShots,
    -- ** Manipulating Boxity of a Demand
    unboxDeeplyDmd,

    -- * Divergence
    Divergence(..), topDiv, botDiv, exnDiv, lubDivergence, isDeadEndDiv,

    -- * Demand environments
    DmdEnv(..), addVarDmdEnv, mkTermDmdEnv, nopDmdEnv, plusDmdEnv, plusDmdEnvs,
    multDmdEnv, reuseEnv,

    -- * Demand types
    DmdType(..), dmdTypeDepth,
    -- ** Algebra
    nopDmdType, botDmdType,
    lubDmdType, plusDmdType, multDmdType, discardArgDmds,
    -- ** Other operations
    peelFV, findIdDemand, addDemand, splitDmdTy, deferAfterPreciseException,

    -- * Demand signatures
    DmdSig(..), mkDmdSigForArity, mkClosedDmdSig, mkVanillaDmdSig,
    splitDmdSig, dmdSigDmdEnv, hasDemandEnvSig,
    nopSig, botSig, isNopSig, isBottomingSig, isDeadEndSig, isDeadEndAppSig,
    trimBoxityDmdSig, transferArgBoxityDmdSig,

    -- ** Handling arity adjustments
    prependArgsDmdSig, etaConvertDmdSig,

    -- * Demand transformers from demand signatures
    DmdTransformer, dmdTransformSig, dmdTransformDataConSig, dmdTransformDictSelSig,

    -- * Trim to a type shape
    TypeShape(..), trimToType, trimBoxity,

    -- * @seq@ing stuff
    seqDemand, seqDemandList, seqDmdType, seqDmdSig,

    -- * Zapping usage information
    zapUsageDemand, zapDmdEnvSig, zapUsedOnceDemand, zapUsedOnceSig
  ) where

import GHC.Prelude

import GHC.Types.Var
import GHC.Types.Var.Env
import GHC.Types.Unique.FM
import GHC.Types.Basic
import GHC.Data.Maybe   ( orElse )

import GHC.Core.Type    ( Type, isTerminatingType )
import GHC.Core.DataCon ( splitDataProductType_maybe, StrictnessMark, isMarkedStrict )
import GHC.Core.Multiplicity    ( scaledThing )

import GHC.Utils.Binary
import GHC.Utils.Misc
import GHC.Utils.Outputable
import GHC.Utils.Panic

import Data.Coerce (coerce)
import Data.Function

{-
************************************************************************
*                                                                      *
           Boxity: Whether the box of something is used
*                                                                      *
************************************************************************
-}

{- Note [Strictness and Unboxing]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If an argument is used strictly by the function body, we may use use
call-by-value instead of call-by-need for that argument. What's more, we may
unbox an argument that is used strictly, discarding the box at the call site.
This can reduce allocations of the program drastically if the box really isn't
needed in the function body. Here's an example:
```
even :: Int -> Bool
even (I# 0) = True
even (I# 1) = False
even (I# n) = even (I# (n -# 2))
```
All three code paths of 'even' are (a) strict in the argument, and (b)
immediately discard the boxed 'Int'. Now if we have a call site like
`even (I# 42)`, then it would be terrible to allocate the 'I#' box for the
argument only to tear it apart immediately in the body of 'even'! Hence,
worker/wrapper will allocate a wrapper for 'even' that not only uses
call-by-value for the argument (e.g., `case I# 42 of b { $weven b }`), but also
*unboxes* the argument, resulting in
```
even :: Int -> Bool
even (I# n) = $weven n
$weven :: Int# -> Bool
$weven 0 = True
$weven 1 = False
$weven n = $weven (n -# 2)
```
And now the box in `even (I# 42)` will cancel away after inlining the wrapper.

As far as the permission to unbox is concerned, *evaluatedness* of the argument
is the important trait. Unboxing implies eager evaluation of an argument and
we don't want to change the termination properties of the function. One way
to ensure that is to unbox strict arguments only, but strictness is only a
sufficient condition for evaluatedness.
See Note [Unboxing evaluated arguments] in "GHC.Core.Opt.DmdAnal", where
we manage to unbox *strict fields* of unboxed arguments that the function is not
actually strict in, simply by realising that those fields have to be evaluated.

Note [Boxity analysis]
~~~~~~~~~~~~~~~~~~~~~~
Alas, we don't want to unbox *every* strict argument
(as Note [Strictness and Unboxing] might suggest).
Here's an example (from T19871):
```
data Huge = H Bool Bool ... Bool
ann :: Huge -> (Bool, Huge)
ann h@(Huge True _ ... _) = (False, h)
ann h                     = (True,  h)
```
Unboxing 'h' yields
```
$wann :: Bool -> Bool -> ... -> Bool -> (Bool, Huge)
$wann True b2 ... bn = (False, Huge True b2 ... bn)
$wann b1   b2 ... bn = (True,  Huge b1   b2 ... bn)
```
The pair constructor really needs its fields boxed. But '$wann' doesn't get
passed 'h' anymore, only its components! Ergo it has to reallocate the 'Huge'
box, in a process called "reboxing". After w/w, call sites like
`case ... of Just h -> ann h` pay for the allocation of the additional box.
In earlier versions of GHC we simply accepted that reboxing would sometimes
happen, but we found some cases where it made a big difference: #19407, for
example.

We therefore perform a simple syntactic boxity analysis that piggy-backs on
demand analysis in order to determine whether the box of a strict argument is
always discarded in the function body, in which case we can pass it unboxed
without risking regressions such as in 'ann' above. But as soon as one use needs
the box, we want Boxed to win over any Unboxed uses.

The demand signature (cf. Note [Demand notation]) will say whether it uses
its arguments boxed or unboxed. Indeed it does so for every sub-component of
the argument demand. Here's an example:
```
f :: (Int, Int) -> Bool
f (a, b) = even (a + b) -- demand signature: <1!P(1!L,1!L)>
```
The '!' indicates places where we want to unbox, the lack thereof indicates the
box is used by the function. Boxity flags are part of the 'Poly' and 'Prod'
'SubDemand's, see Note [Why Boxity in SubDemand and not in Demand?].
The given demand signature says "Unbox the pair and then nestedly unbox its
two fields". By contrast, the demand signature of 'ann' above would look like
<1P(1L,L,...,L)>, lacking any '!'.

A demand signature like <1P(1!L)> -- Boxed outside but Unboxed in the field --
doesn't make a lot of sense, as we can never unbox the field without unboxing
the containing record. See Note [Finalising boxity for demand signatures] in
"GHC.Core.Opt.DmdAnal" for how we avoid to spread this and other kinds of
misinformed boxities.

Due to various practical reasons, Boxity Analysis is not conservative at times.
Here are reasons for too much optimism:

 * Note [Function body boxity and call sites] is an observation about when it is
   beneficial to unbox a parameter that is returned from a function.
   Note [Unboxed demand on function bodies returning small products] derives
   a heuristic from the former Note, pretending that all call sites of a
   function need returned small products Unboxed.
 * Note [Boxity for bottoming functions] in DmdAnal makes all bottoming
   functions unbox their arguments, incurring reboxing in code paths that will
   diverge anyway. In turn we get more unboxing in hot code paths.

Boxity analysis fixes a number of issues: #19871, #19407, #4267, #16859, #18907, #13331

Note [Function body boxity and call sites]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider (from T5949)
```
f n p = case n of
  0 -> p :: (a, b)
  _ -> f (n-1) p
-- Worker/wrapper split if we decide to unbox:
$wf n x y = case n of
  0 -> (# x, y #)
  _ -> $wf (n-1) x y
f n (x,y) = case $wf n x y of (# r, s #) -> (r,s)
```
When is it better to /not/ to unbox 'p'? That depends on the callers of 'f'!
If all call sites

 1. Wouldn't need to allocate fresh boxes for 'p', and
 2. Needed the result pair of 'f' boxed

Only then we'd see an increase in allocation resulting from unboxing. But as
soon as only one of (1) or (2) holds, it really doesn't matter if 'f' unboxes
'p' (and its result, it's important that CPR follows suit). For example
```
res = ... case f m (field t) of (r1,r2) -> ...  -- (1) holds
arg = ... [ f m (x,y) ] ...                     -- (2) holds
```
Because one of the boxes in the call site can cancel away:
```
res = ... case field1 t of (x1,x2) ->
          case field2 t of (y1,y2) ->
          case $wf x1 x2 y1 y2 of (#r1,r2#) -> ...
arg = ... [ case $wf x1 x2 y1 y2 of (#r1,r2#) -> (r1,r2) ] ...
```
And when call sites neither have arg boxes (1) nor need the result boxed (2),
then hesitating to unbox means /more/ allocation in the call site because of the
need for fresh argument boxes.

Summary: If call sites that satisfy both (1) and (2) occur more often than call
sites that satisfy neither condition, then it's best /not/ to unbox 'p'.

Note [Unboxed demand on function bodies returning small products]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Note [Boxity analysis] achieves its biggest wins when we avoid reboxing huge
records. But when we return small products from a function, we often get faster
programs by pretending that the caller unboxes the result. Long version:

Observation: Big record arguments (e.g., DynFlags) tend to be modified much less
             frequently than small records (e.g., Int).
Result:      Big records tend to be passed around boxed (unmodified) much more
             frequently than small records.
Consequence:  The larger the record, the more likely conditions (1) and (2) from
             Note [Function body boxity and call sites] are met, in which case
             unboxing returned parameters leads to reboxing.

So we put an Unboxed demand on function bodies returning small products and a
Boxed demand on the others. What is regarded a small product is controlled by
the -fdmd-unbox-width flag.

This also manages to unbox functions like
```
sum z      []          = z
sum (I# n) ((I# x):xs) = sum (I# (n +# x)) xs
```
where we can unbox 'z' on the grounds that it's but a small box anyway. That in
turn means that the I# allocation in the recursive call site can cancel away and
we get a non-allocating loop, nice and tight.
Note that this is the typical case in "Observation" above: A small box is
unboxed, modified, the result reboxed for the recursive call.

Originally, this came up in binary-trees' check' function and #4267 which
(similarly) features a strict fold over a tree. We'd also regress in join004 and
join007 if we didn't assume an optimistic Unboxed demand on the function body.
T17932 features a (non-recursive) function that returns a large record, e.g.,
```
flags (Options f x) = <huge> `seq` f
```
and here we won't unbox 'f' because it has 5 fields (which is larger than the
default -fdmd-unbox-width threshold).

Why not focus on putting Unboxed demands on *all recursive* function?
Then we'd unbox
```
flags 0 (Options f x) = <huge> `seq` f
flags n o             = flags (n-1) o
```
and that seems hardly useful.
(NB: Similar to 'f' from Note [Preserving Boxity of results is rarely a win],
but there we only had 2 fields.)

What about the Boxity of *fields* of a small, returned box? Consider
```
sumIO :: Int -> Int -> IO Int
sumIO 0 !z = return z     -- What DmdAnal sees: sumIO 0 z s = z `seq` (# s, z #)
sumIO n !z = sumIO (n-1) (z+n)
```
We really want 'z' to unbox here. Yet its use in the returned unboxed pair
is fundamentally a Boxed one! CPR would manage to unbox it, but DmdAnal runs
before that. There is an Unboxed use in the recursive call to 'go' though.
But 'IO Int' returns a small product, and 'Int' is a small product itself.
So we'll put the RHS of 'sumIO' under sub-demand '!P(L,L!P(L))', indicating that
*if* we evaluate 'z', we don't need the box later on. And indeed the bang will
evaluate `z`, so we conclude with a total demand of `1!P(L)` on `z` and unbox
it.

Unlike for recursive functions, where we can often speed up the loop by
unboxing at the cost of a bit of reboxing in the base case, the wins for
non-recursive functions quickly turn into losses when unboxing too deeply.
That happens in T11545, T18109 and T18174. Therefore, we deeply unbox recursive
function bodies but only shallowly unbox non-recursive function bodies (governed
by the max_depth variable).

The implementation is in 'GHC.Core.Opt.DmdAnal.unboxWhenSmall'. It is quite
vital, guarding for regressions in test cases like #2387, #3586, #16040, #5075
and #19871.

Note that this is fundamentally working around a phase problem, namely that the
results of boxity analysis depend on CPR analysis (and vice versa, of course).

Note [unboxedWins]
~~~~~~~~~~~~~~~~~~
We used to use '_unboxedWins' below in 'lubBoxity', which was too optimistic.

While it worked around some shortcomings of the phase separation between Boxity
analysis and CPR analysis, it was a gross hack which caused regressions itself
that needed all kinds of fixes and workarounds. Examples (from #21119):

  * As #20767 says, L and B were no longer top and bottom of our lattice
  * In #20746 we unboxed huge Handle types that were never needed boxed in the
    first place. See Note [deferAfterPreciseException].
  * It also caused unboxing of huge records where we better shouldn't, for
    example in T19871.absent.
  * It became impossible to work with when implementing !7599, mostly due to the
    chaos that results from #20767.

Conclusion: We should use 'boxedWins' in 'lubBoxity', #21119.
Fortunately, we could come up with a number of better mechanisms to make up for
the sometimes huge regressions that would have otherwise incured:

1. A beefed up Note [Unboxed demand on function bodies returning small products]
   that works recursively fixes most regressions. It's a bit unsound, but
   pretty well-behaved.
2. We saw bottoming functions spoil boxity in some less severe cases and
   countered that with Note [Boxity for bottoming functions].

-}

boxedWins :: Boxity -> Boxity -> Boxity
boxedWins Unboxed Unboxed = Unboxed
boxedWins _       !_      = Boxed

_unboxedWins :: Boxity -> Boxity -> Boxity
-- See Note [unboxedWins]
_unboxedWins Boxed Boxed = Boxed
_unboxedWins _     !_    = Unboxed

lubBoxity :: Boxity -> Boxity -> Boxity
-- See Note [Boxity analysis] for the lattice.
lubBoxity = boxedWins

{-
************************************************************************
*                                                                      *
           Card: Combining Strictness and Usage
*                                                                      *
************************************************************************
-}

{- Note [Evaluation cardinalities]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The demand analyser uses an (abstraction of) /evaluation cardinality/ of type
Card, to specify how many times a term is evaluated. A Card C_lu
represents an /interval/ of possible cardinalities [l..u], meaning

* Evaluated /at least/ 'l' times (strictness).
  Hence 'l' is either 0 (lazy)
                   or 1 (strict)

* Evaluated /at most/ 'u' times (usage).
  Hence 'u' is either 0 (not used at all),
                   or 1 (used at most once)
                   or n (no information)

Intervals describe sets, so the underlying lattice is the powerset lattice.

Usually l<=u, but we also have C_10, the interval [1,0], the empty interval,
denoting the empty set.   This is the bottom element of the lattice.

See Note [Demand notation] for the notation we use for each of the constructors.

Note [Bit vector representation for Card]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
While the 6 inhabitants of Card admit an efficient representation as an
enumeration, implementing operations such as lubCard, plusCard and multCard
leads to unreasonably bloated code. This was the old defn for lubCard, for
example:

  -- Handle C_10 (bot)
  lubCard C_10 n    = n    -- bot
  lubCard n    C_10 = n    -- bot
  -- Handle C_0N (top)
  lubCard C_0N _    = C_0N -- top
  lubCard _    C_0N = C_0N -- top
  -- Handle C_11
  lubCard C_00 C_11 = C_01 -- {0} ∪ {1} = {0,1}
  lubCard C_11 C_00 = C_01 -- {0} ∪ {1} = {0,1}
  lubCard C_11 n    = n    -- {1} is a subset of all other intervals
  lubCard n    C_11 = n    -- {1} is a subset of all other intervals
  -- Handle C_1N
  lubCard C_1N C_1N = C_1N -- reflexivity
  lubCard _    C_1N = C_0N -- {0} ∪ {1,n} = top
  lubCard C_1N _    = C_0N -- {0} ∪ {1,n} = top
  -- Handle C_01
  lubCard C_01 _    = C_01 -- {0} ∪ {0,1} = {0,1}
  lubCard _    C_01 = C_01 -- {0} ∪ {0,1} = {0,1}
  -- Handle C_00
  lubCard C_00 C_00 = C_00 -- reflexivity

There's a much more compact way to encode these operations if Card is
represented not as distinctly denoted intervals, but as the subset of the set
of all cardinalities {0,1,n} instead. We represent such a subset as a bit vector
of length 3 (which fits in an Int). That's actually pretty common for such
powerset lattices.
There's one bit per denoted cardinality that is set iff that cardinality is part
of the denoted set, with n being the most significand bit (index 2) and 0 being
represented by the least significand bit (index 0).

How does that help? Well, for one, lubCard just becomes

  lubCard (Card a) (Card b) = Card (a .|. b)

The other operations, 'plusCard' and 'multCard', become significantly more
tricky, but immensely more compact. It's all straight-line code with a few bit
twiddling instructions now!

Note [Algebraic specification for plusCard and multCard]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The representation change in Note [Bit vector representation for Card] admits
very dense definitions of 'plusCard' and 'multCard' in terms of bit twiddling,
but the connection to the algebraic operations they implement is lost.
It's helpful to have a written specification of what 'plusCard' and 'multCard'
here that says what they should compute.

  * plusCard: a@[l1,u1] + b@[l2,u2] = r@[l1+l2,u1+u2].
      - In terms of sets, 0 ∈ r iff 0 ∈ a and 0 ∈ b.
        Examples: set in C_00 + C_00, C_01 + C_0N, but not in C_10 + C_00
      - In terms of sets, 1 ∈ r iff 1 ∈ a or 1 ∈ b.
        Examples: set in C_01 + C_00, C_0N + C_0N, but not in C_10 + C_00
      - In terms of sets, n ∈ r iff n ∈ a or n ∈ b, or (1 ∈ a and 1 ∈ b),
        so not unlike add with carry.
        Examples: set in C_01 + C_01, C_01 + C_0N, but not in C_10 + C_01
      - Handy special cases:
          o 'plusCard C_10' bumps up the strictness of its argument, just like
            'lubCard C_00' lazifies it, without touching upper bounds.
            See also 'strictifyCard'
          o Similarly, 'plusCard C_0N' discards usage information
            (incl. absence) but leaves strictness alone.

  * multCard: a@[l1,u1] * b@[l2,u2] = r@[l1*l2,u1*u2].
      - In terms of sets, 0 ∈ r iff 0 ∈ a or 0 ∈ b.
        Examples: set in C_00 * C_10, C_01 * C_1N, but not in C_10 * C_1N
      - In terms of sets, 1 ∈ r iff 1 ∈ a and 1 ∈ b.
        Examples: set in C_01 * C_01, C_01 * C_1N, but not in C_11 * C_10
      - In terms of sets, n ∈ r iff 1 ∈ r and (n ∈ a or n ∈ b).
        Examples: set in C_1N * C_01, C_1N * C_0N, but not in C_10 * C_1N
      - Handy special cases:
          o 'multCard C_1N c' is the same as 'plusCard c c' and
            drops used-once info. But unlike 'plusCard C_0N', it leaves absence
            and strictness.
          o 'multCard C_01' drops strictness info, like 'lubCard C_00'.
          o 'multCard C_0N' does both; it discards all strictness and used-once
            info and retains only absence info.
-}


-- | Describes an interval of /evaluation cardinalities/.
-- See Note [Evaluation cardinalities]
-- See Note [Bit vector representation for Card]
newtype Card = Card Int
  deriving Eq

-- | A subtype of 'Card' for which the upper bound is never 0 (no 'C_00' or
-- 'C_10'). The only four inhabitants are 'C_01', 'C_0N', 'C_11', 'C_1N'.
-- Membership can be tested with 'isCardNonAbs'.
-- See 'D' and 'Call' for use sites and explanation.
type CardNonAbs = Card

-- | A subtype of 'Card' for which the upper bound is never 1 (no 'C_01' or
-- 'C_11'). The only four inhabitants are 'C_00', 'C_0N', 'C_10', 'C_1N'.
-- Membership can be tested with 'isCardNonOnce'.
-- See 'Poly' for use sites and explanation.
type CardNonOnce = Card

-- | Absent, {0}. Pretty-printed as A.
pattern C_00 :: Card
pattern C_00 = Card 0b001
-- | Bottom, {}. Pretty-printed as A.
pattern C_10 :: Card
pattern C_10 = Card 0b000
-- | Strict and used once, {1}. Pretty-printed as 1.
pattern C_11 :: Card
pattern C_11 = Card 0b010
-- | Used at most once, {0,1}. Pretty-printed as M.
pattern C_01 :: Card
pattern C_01 = Card 0b011
-- | Strict and used (possibly) many times, {1,n}. Pretty-printed as S.
pattern C_1N :: Card
pattern C_1N = Card 0b110
-- | Every possible cardinality; the top element, {0,1,n}. Pretty-printed as L.
pattern C_0N :: Card
pattern C_0N = Card 0b111

{-# COMPLETE C_00, C_01, C_0N, C_10, C_11, C_1N :: Card #-}

_botCard, topCard :: Card
_botCard = C_10
topCard = C_0N

-- | True <=> lower bound is 1.
isStrict :: Card -> Bool
-- See Note [Bit vector representation for Card]
isStrict (Card c) = c .&. 0b001 == 0 -- simply check 0 bit is not set

-- | True <=> upper bound is 0.
isAbs :: Card -> Bool
-- See Note [Bit vector representation for Card]
isAbs (Card c) = c .&. 0b110 == 0 -- simply check 1 and n bit are not set

-- | True <=> upper bound is 1.
isAtMostOnce :: Card -> Bool
-- See Note [Bit vector representation for Card]
isAtMostOnce (Card c) = c .&. 0b100 == 0 -- simply check n bit is not set

-- | Is this a 'CardNonAbs'?
isCardNonAbs :: Card -> Bool
isCardNonAbs = not . isAbs

-- | Is this a 'CardNonOnce'?
isCardNonOnce :: Card -> Bool
isCardNonOnce n = isAbs n || not (isAtMostOnce n)

-- | Intersect with [0,1].
oneifyCard :: Card -> Card
oneifyCard = glbCard C_01

-- | Intersect with [1,n]. The same as @'plusCard' 'C_10'@.
strictifyCard :: Card -> Card
strictifyCard = glbCard C_1N

-- | Denotes '∪' on 'Card'.
lubCard :: Card -> Card -> Card
-- See Note [Bit vector representation for Card]
lubCard (Card a) (Card b) = Card (a .|. b) -- main point of the bit-vector encoding!

-- | Denotes '∩' on 'Card'.
glbCard :: Card -> Card -> Card
-- See Note [Bit vector representation for Card]
glbCard (Card a) (Card b) = Card (a .&. b)

-- | Denotes '+' on lower and upper bounds of 'Card'.
plusCard :: Card -> Card -> Card
-- See Note [Algebraic specification for plusCard and multCard]
plusCard (Card a) (Card b)
  = Card (bit0 .|. bit1 .|. bitN)
  where
    bit0 =  (a .&. b)                         .&. 0b001
    bit1 =  (a .|. b)                         .&. 0b010
    bitN = ((a .|. b) .|. shiftL (a .&. b) 1) .&. 0b100

-- | Denotes '*' on lower and upper bounds of 'Card'.
multCard :: Card -> Card -> Card
-- See Note [Algebraic specification for plusCard and multCard]
multCard (Card a) (Card b)
  = Card (bit0 .|. bit1 .|. bitN)
  where
    bit0 = (a .|. b)                   .&. 0b001
    bit1 = (a .&. b)                   .&. 0b010
    bitN = (a .|. b) .&. shiftL bit1 1 .&. 0b100

{-
************************************************************************
*                                                                      *
           Demand: Evaluation contexts
*                                                                      *
************************************************************************
-}

-- | A demand describes
--
--   * How many times a variable is evaluated, via a 'Card'inality, and
--   * How deep its value was evaluated in turn, via a 'SubDemand'.
--
-- See also Note [Demand notation]
-- and Note [Demand examples].
--
-- This data type is quite similar to `'Scaled' 'SubDemand'`, but it's scaled
-- by 'Card', which is an /interval/ on 'Multiplicity', the upper bound of
-- which could be used to infer uniqueness types. Also we treat 'AbsDmd' and
-- 'BotDmd' specially, as the concept of a 'SubDemand' doesn't apply when there
-- isn't any evaluation at all. If you don't care, simply use '(:*)'.
data Demand
  = BotDmd
  -- ^ A bottoming demand, produced by a diverging function ('C_10'), hence there is no
  -- 'SubDemand' that describes how it was evaluated.

  | AbsDmd
  -- ^ An absent demand: Evaluated exactly 0 times ('C_00'), hence there is no
  -- 'SubDemand' that describes how it was evaluated.

  | D !CardNonAbs !SubDemand
  -- ^ Don't use this internal data constructor; use '(:*)' instead.
  -- Since BotDmd deals with 'C_10' and AbsDmd deals with 'C_00', the
  -- cardinality component is CardNonAbs
  deriving Eq

-- | Only meant to be used in the pattern synonym below!
viewDmdPair :: Demand -> (Card, SubDemand)
viewDmdPair BotDmd   = (C_10, botSubDmd)
viewDmdPair AbsDmd   = (C_00, botSubDmd)
viewDmdPair (D n sd) = (n, sd)

-- | @c :* sd@ is a demand that says \"evaluated @c@ times, and any trace in
-- which it is evaluated will evaluate at least as deep as @sd@\".
--
-- Matching on this pattern synonym is a complete match.
-- If the matched demand was 'AbsDmd', it will match as @C_00 :* seqSubDmd@.
-- If the matched demand was 'BotDmd', it will match as @C_10 :* botSubDmd@.
-- The builder of this pattern synonym simply /discards/ the 'SubDemand' if the
-- 'Card' was absent and returns 'AbsDmd' or 'BotDmd' instead. It will assert
-- that the discarded sub-demand was 'seqSubDmd' and 'botSubDmd', respectively.
--
-- Call sites should consider whether they really want to look at the
-- 'SubDemand' of an absent demand and match on 'AbsDmd' and/or 'BotDmd'
-- otherwise. Really, any other 'SubDemand' would be allowed and
-- might work better, depending on context.
pattern (:*) :: HasDebugCallStack => Card -> SubDemand -> Demand
pattern n :* sd <- (viewDmdPair -> (n, sd)) where
  C_10 :* sd = BotDmd & assertPpr (sd == botSubDmd) (text "B /=" <+> ppr sd)
  C_00 :* sd = AbsDmd & assertPpr (sd == botSubDmd) (text "A /=" <+> ppr sd)
  n    :* sd = D n sd & assertPpr (isCardNonAbs n)  (ppr n $$ ppr sd)
{-# COMPLETE (:*) #-}

-- | A sub-demand describes an /evaluation context/ (in the sense of an
-- operational semantics), e.g. how deep the denoted thing is going to be
-- evaluated. See 'Demand' for examples.
--
-- See Note [SubDemand denotes at least one evaluation] for a more detailed
-- description of what a sub-demand means.
--
-- See Note [Demand notation] for the extensively used short-hand notation.
-- See also Note [Why Boxity in SubDemand and not in Demand?].
data SubDemand
  = Poly !Boxity !CardNonOnce
  -- ^ Polymorphic demand, the denoted thing is evaluated arbitrarily deep,
  -- with the specified cardinality at every level. The 'Boxity' applies only
  -- to the outer evaluation context as well as all inner evaluation context.
  -- See Note [Boxity in Poly] for why we want it to carry 'Boxity'.
  -- Expands to 'Call' via 'viewCall' and to 'Prod' via 'viewProd'.
  --
  -- @Poly b n@ is semantically equivalent to @Prod b [n :* Poly b n, ...]
  -- or @Call n (Poly Boxed n)@. 'viewCall' and 'viewProd' do these rewrites.
  --
  -- In Note [Demand notation]: @L  === P(L,L,...)@  and @L  === C(L)@,
  --                            @B  === P(B,B,...)@  and @B  === C(B)@,
  --                            @!A === !P(A,A,...)@ and @!A === C(A)@,
  --                            and so on.
  --
  -- We'll only see 'Poly' with 'C_10' (B), 'C_00' (A), 'C_0N' (L) and sometimes
  -- 'C_1N' (S) through 'plusSubDmd', never 'C_01' (M) or 'C_11' (1) (grep the
  -- source code). Hence 'CardNonOnce', which is closed under 'lub' and 'plus'.
  --
  -- Why doesn't this constructor simply carry a 'Demand' instead of its fields?
  -- See Note [Call SubDemand vs. evaluation Demand].
  | Call !CardNonAbs !SubDemand
  -- ^ @Call n sd@ describes the evaluation context of @n@ function
  -- applications (with one argument), where the result of each call is
  -- evaluated according to @sd@.
  -- @sd@ describes program traces in which the denoted thing was called at all,
  -- see Note [SubDemand denotes at least one evaluation].
  -- That Note also explains why it doesn't make sense for @n@ to be absent,
  -- hence we forbid it with 'CardNonAbs'. Absent call demands can still be
  -- expressed with 'Poly'.
  -- Used only for values of function type. Use the smart constructor 'mkCall'
  -- whenever possible!
  | Prod !Boxity ![Demand]
  -- ^ @Prod b ds@ describes the evaluation context of a case scrutinisation
  -- on an expression of product type, where the product components are
  -- evaluated according to @ds@. The 'Boxity' @b@ says whether or not the box
  -- of the product was used.

-- | We have to respect Poly rewrites through 'viewCall' and 'viewProd'.
instance Eq SubDemand where
  d1 == d2 = case d1 of
    Prod b1 ds1
      | Just (b2, ds2) <- viewProd (length ds1) d2 -> b1 == b2 && ds1 == ds2
    Call n1 sd1
      | Just (n2, sd2) <- viewCall d2              -> n1 == n2 && sd1 == sd2
    Poly b1 n1
      | Poly b2 n2 <- d2                           -> b1 == b2 && n1 == n2
    _                                              -> False

topSubDmd, botSubDmd, seqSubDmd :: SubDemand
topSubDmd = Poly   Boxed C_0N
botSubDmd = Poly Unboxed C_10
seqSubDmd = Poly Unboxed C_00

-- | The uniform field demand when viewing a 'Poly' as a 'Prod', as in
-- 'viewProd'.
polyFieldDmd :: Boxity -> CardNonOnce -> Demand
polyFieldDmd _     C_00 = AbsDmd
polyFieldDmd _     C_10 = BotDmd
polyFieldDmd Boxed C_0N = topDmd
polyFieldDmd b     n    = n :* Poly b n & assertPpr (isCardNonOnce n) (ppr n)

-- | A smart constructor for 'Prod', applying rewrite rules along the semantic
-- equality @Prod b [n :* Poly Boxed n, ...] === Poly b n@, simplifying to
-- 'Poly' 'SubDemand's when possible. Examples:
--
--   * Rewrites @P(L,L)@ (e.g., arguments @Boxed@, @[L,L]@) to @L@
--   * Rewrites @!P(L!L,L!L)@ (e.g., arguments @Unboxed@, @[L!L,L!L]@) to @!L@
--   * Does not rewrite @P(1L)@, @P(L!L)@, @!P(L)@ or @P(L,A)@
--
mkProd :: Boxity -> [Demand] -> SubDemand
mkProd b ds
  | all (== AbsDmd) ds = Poly b C_00
  | all (== BotDmd) ds = Poly b C_10
  | dmd@(n :* Poly b2 m):_ <- ds
  , n == m           -- don't rewrite P(SL)  to S
  , b == b2          -- don't rewrite P(S!S) to !S
  , all (== dmd) ds  -- don't rewrite P(L,A) to L
  = Poly b n
  | otherwise          = Prod b ds

-- | @viewProd n sd@ interprets @sd@ as a 'Prod' of arity @n@, expanding 'Poly'
-- demands as necessary.
viewProd :: Arity -> SubDemand -> Maybe (Boxity, [Demand])
-- It's quite important that this function is optimised well;
-- it is used by lubSubDmd and plusSubDmd.
viewProd n (Prod b ds)
  | ds `lengthIs` n = Just (b, ds)
-- Note the strict application to replicate: This makes sure we don't allocate
-- a thunk for it, inlines it and lets case-of-case fire at call sites.
viewProd n (Poly b card)
  | let !ds = replicate n $! polyFieldDmd b card
  = Just (b, ds)
viewProd _ _
  = Nothing
{-# INLINE viewProd #-} -- we want to fuse away the replicate and the allocation
                        -- for Arity. Otherwise, #18304 bites us.

-- | A smart constructor for 'Call', applying rewrite rules along the semantic
-- equality @Call C_0N (Poly C_0N) === Poly C_0N@, simplifying to 'Poly' 'SubDemand's
-- when possible.
mkCall :: CardNonAbs -> SubDemand -> SubDemand
--mkCall C_1N sd@(Poly Boxed C_1N) = sd -- NO! #21085 strikes. See Note [mkCall and plusSubDmd]
mkCall C_0N sd@(Poly Boxed C_0N) = sd
mkCall n    sd                   = assertPpr (isCardNonAbs n) (ppr n $$ ppr sd) $
                                   Call n sd

-- | @viewCall sd@ interprets @sd@ as a 'Call', expanding 'Poly' subdemands as
-- necessary.
viewCall :: SubDemand -> Maybe (Card, SubDemand)
viewCall (Call n sd) = Just (n :: Card, sd)
viewCall (Poly _ n)
  | isAbs n          = Just (n :: Card, botSubDmd)
  | otherwise        = Just (n :: Card, Poly Boxed n)
viewCall _           = Nothing

topDmd, absDmd, botDmd, seqDmd :: Demand
topDmd = C_0N :* topSubDmd
absDmd = AbsDmd
botDmd = BotDmd
seqDmd = C_11 :* seqSubDmd

-- | Sets 'Boxity' to 'Unboxed' for non-'Call' sub-demands and recurses into 'Prod'.
unboxDeeplySubDmd :: SubDemand -> SubDemand
unboxDeeplySubDmd (Poly _ n)  = Poly Unboxed n
unboxDeeplySubDmd (Prod _ ds) = mkProd Unboxed (strictMap unboxDeeplyDmd ds)
unboxDeeplySubDmd call@Call{} = call

-- | Sets 'Boxity' to 'Unboxed' for the 'Demand', recursing into 'Prod's.
-- Don't recurse into lazy arguments; see GHC.Core.Opt.DmdAnal
--    Note [No lazy, Unboxed demands in demand signature]
unboxDeeplyDmd :: Demand -> Demand
unboxDeeplyDmd AbsDmd   = AbsDmd
unboxDeeplyDmd BotDmd   = BotDmd
unboxDeeplyDmd dmd@(D n sd) | isStrict n = D n (unboxDeeplySubDmd sd)
                            | otherwise  = dmd


multDmd :: Card -> Demand -> Demand
multDmd C_11 dmd       = dmd -- An optimisation
-- The following four lines make sure that we rewrite to AbsDmd and BotDmd
-- whenever the leading cardinality is absent (C_00 or C_10).
-- Otherwise it may happen that the SubDemand is not 'botSubDmd', triggering
-- the assertion in `:*`.
-- Example: `multDmd B 1L = BA`, so with an inner `seqSubDmd`. Our lattice
-- allows us to always rewrite this to proper BotDmd and we maintain the
-- invariant that this is indeed the case.
multDmd C_00 _        = AbsDmd
multDmd _    AbsDmd   = AbsDmd
multDmd C_10 (D n _)  = if isStrict n then BotDmd else AbsDmd
multDmd n    BotDmd   = if isStrict n then BotDmd else AbsDmd
-- See Note [SubDemand denotes at least one evaluation] for the strictifyCard
multDmd n    (D m sd) = multCard n m :* multSubDmd (strictifyCard n) sd

multSubDmd :: Card -> SubDemand -> SubDemand
multSubDmd C_11 sd           = sd -- An optimisation, for when sd is a deep Prod
-- The following three equations don't have an impact on Demands, only on
-- Boxity. They are needed so that we don't trigger the assertions in `:*`
-- when called from `multDmd`.
multSubDmd C_00 _            = seqSubDmd -- Otherwise `multSubDmd A L == A /= !A`
multSubDmd C_10 (Poly _ n)   = if isStrict n then botSubDmd else seqSubDmd -- Otherwise `multSubDmd B L == B /= !B`
multSubDmd C_10 (Call n _)   = if isStrict n then botSubDmd else seqSubDmd -- Otherwise we'd call `mkCall` with absent cardinality
multSubDmd n    (Poly b m)   = Poly b (multCard n m)
multSubDmd n    (Call n' sd) = mkCall (multCard n n') sd
multSubDmd n    (Prod b ds)  = mkProd b (strictMap (multDmd n) ds)

lazifyIfStrict :: Card -> SubDemand -> SubDemand
lazifyIfStrict n sd = multSubDmd (glbCard C_01 n) sd

-- | Denotes '∪' on 'Demand'.
lubDmd :: Demand -> Demand -> Demand
lubDmd BotDmd      dmd2        = dmd2
lubDmd dmd1        BotDmd      = dmd1
lubDmd (n1 :* sd1) (n2 :* sd2) = -- pprTraceWith "lubDmd" (\it -> ppr (n1:*sd1) $$ ppr (n2:*sd2) $$ ppr it) $
  lubCard n1 n2 :* lubSubDmd sd1 sd2

lubSubDmd :: SubDemand -> SubDemand -> SubDemand
-- Shortcuts for neutral and absorbing elements.
-- Below we assume that Boxed always wins.
lubSubDmd (Poly Unboxed C_10)  sd                   = sd
lubSubDmd sd                   (Poly Unboxed C_10)  = sd
lubSubDmd sd@(Poly Boxed C_0N) _                    = sd
lubSubDmd _                    sd@(Poly Boxed C_0N) = sd
-- Handle Prod
lubSubDmd (Prod b1 ds1) (Poly b2 n2)
  | let !d = polyFieldDmd b2 n2
  = mkProd (lubBoxity b1 b2) (strictMap (lubDmd d) ds1)
lubSubDmd (Prod b1 ds1) (Prod b2 ds2)
  | equalLength ds1 ds2
  = mkProd (lubBoxity b1 b2) (strictZipWith lubDmd ds1 ds2)
-- Handle Call
lubSubDmd (Call n1 sd1) (viewCall -> Just (n2, sd2)) =
  mkCall (lubCard n1 n2) (lubSubDmd sd1 sd2)
-- Handle Poly
lubSubDmd (Poly b1 n1) (Poly b2 n2) = Poly (lubBoxity b1 b2) (lubCard n1 n2)
-- Other Poly case by commutativity
lubSubDmd sd1@Poly{}   sd2          = lubSubDmd sd2 sd1
-- Otherwise (Call `lub` Prod) return Top
lubSubDmd _            _            = topSubDmd

-- | Denotes '+' on 'Demand'.
plusDmd :: Demand -> Demand -> Demand
plusDmd AbsDmd      dmd2        = dmd2
plusDmd dmd1        AbsDmd      = dmd1
plusDmd (n1 :* sd1) (n2 :* sd2) = -- pprTraceWith "plusDmd" (\it -> ppr (n1:*sd1) $$ ppr (n2:*sd2) $$ ppr it) $
  -- Why lazify? See Note [SubDemand denotes at least one evaluation]
  -- and also Note [Unrealised opportunity in plusDmd] which applies when both
  -- n1 and n2 are lazy already
  plusCard n1 n2 :* plusSubDmd (lazifyIfStrict n1 sd1) (lazifyIfStrict n2 sd2)

plusSubDmd :: SubDemand -> SubDemand -> SubDemand
-- Shortcuts for neutral and absorbing elements.
-- Below we assume that Boxed always wins.
plusSubDmd (Poly Unboxed C_00)  sd                   = sd
plusSubDmd sd                   (Poly Unboxed C_00)  = sd
plusSubDmd sd@(Poly Boxed C_1N) _                    = sd
plusSubDmd _                    sd@(Poly Boxed C_1N) = sd
-- Handle Prod
plusSubDmd (Prod b1 ds1) (Poly b2 n2)
  | let !d = polyFieldDmd b2 n2
  = mkProd (lubBoxity b1 b2) (strictMap (plusDmd d) ds1)
plusSubDmd (Prod b1 ds1) (Prod b2 ds2)
  | equalLength ds1 ds2
  = mkProd (lubBoxity b1 b2) (strictZipWith plusDmd ds1 ds2)
-- Handle Call
plusSubDmd (Call n1 sd1) (viewCall -> Just (n2, sd2)) =
  mkCall (plusCard n1 n2) (lubSubDmd sd1 sd2)
-- Handle Poly
plusSubDmd (Poly b1 n1) (Poly b2 n2) = Poly (lubBoxity b1 b2) (plusCard n1 n2)
-- Other Poly case by commutativity
plusSubDmd sd1@Poly{}   sd2          = plusSubDmd sd2 sd1
-- Otherwise (Call `plus` Prod) return Top
plusSubDmd _            _            = topSubDmd

-- | Used to suppress pretty-printing of an uninformative demand
isTopDmd :: Demand -> Bool
isTopDmd dmd = dmd == topDmd

isAbsDmd :: Demand -> Bool
isAbsDmd (n :* _) = isAbs n

-- | Contrast with isStrictUsedDmd. See Note [Strict demands]
isStrictDmd :: Demand -> Bool
isStrictDmd (n :* _) = isStrict n

-- | Not absent and used strictly. See Note [Strict demands]
isStrUsedDmd :: Demand -> Bool
isStrUsedDmd (n :* _) = isStrict n && not (isAbs n)

-- | Is the value used at most once?
isAtMostOnceDmd :: Demand -> Bool
isAtMostOnceDmd (n :* _) = isAtMostOnce n

-- | We try to avoid tracking weak free variable demands in strictness
-- signatures for analysis performance reasons.
-- See Note [Lazy and unleashable free variables] in "GHC.Core.Opt.DmdAnal".
isWeakDmd :: Demand -> Bool
isWeakDmd dmd@(n :* _) = not (isStrict n) && is_plus_idem_dmd dmd
  where
    -- @is_plus_idem_* thing@ checks whether @thing `plus` thing = thing@,
    -- e.g. if @thing@ is idempotent wrt. to @plus@.
    -- is_plus_idem_card n = plusCard n n == n
    is_plus_idem_card = isCardNonOnce
    -- is_plus_idem_dmd dmd = plusDmd dmd dmd == dmd
    is_plus_idem_dmd AbsDmd    = True
    is_plus_idem_dmd BotDmd    = True
    is_plus_idem_dmd (n :* sd) = is_plus_idem_card n && is_plus_idem_sub_dmd sd
    -- is_plus_idem_sub_dmd sd = plusSubDmd sd sd == sd
    is_plus_idem_sub_dmd (Poly _ n)  = assert (isCardNonOnce n) True
    is_plus_idem_sub_dmd (Prod _ ds) = all is_plus_idem_dmd ds
    is_plus_idem_sub_dmd (Call n _)  = is_plus_idem_card n

evalDmd :: Demand
evalDmd = C_1N :* topSubDmd

-- | First argument of 'GHC.Exts.maskAsyncExceptions#': @1C(1,L)@.
-- Called exactly once.
strictOnceApply1Dmd :: Demand
strictOnceApply1Dmd = C_11 :* mkCall C_11 topSubDmd

-- | First argument of 'GHC.Exts.atomically#': @SC(S,L)@.
-- Called at least once, possibly many times.
strictManyApply1Dmd :: Demand
strictManyApply1Dmd = C_1N :* mkCall C_1N topSubDmd

-- | First argument of catch#: @MC(1,L)@.
-- Evaluates its arg lazily, but then applies it exactly once to one argument.
lazyApply1Dmd :: Demand
lazyApply1Dmd = C_01 :* mkCall C_11 topSubDmd

-- | Second argument of catch#: @MC(1,C(1,L))@.
-- Evaluates its arg lazily, but then applies it exactly once to two arguments.
lazyApply2Dmd :: Demand
lazyApply2Dmd = C_01 :* mkCall C_11 (mkCall C_11 topSubDmd)

-- | Make a 'Demand' evaluated at-most-once.
oneifyDmd :: Demand -> Demand
oneifyDmd AbsDmd    = AbsDmd
oneifyDmd BotDmd    = BotDmd
oneifyDmd (n :* sd) = oneifyCard n :* sd

-- | Make a 'Demand' evaluated at-least-once (e.g. strict).
strictifyDmd :: Demand -> Demand
strictifyDmd = plusDmd seqDmd

-- | If the argument is a guaranteed-terminating type
--   (i.e. a non-newtype dictionary) give it strict demand.
--   This is sound because terminating types can't be bottom:
--         See GHC.Core Note [NON-BOTTOM-DICTS invariant]
-- Also split the product type & demand and recur in order to similarly
-- strictify the argument's contained used non-newtype superclass dictionaries.
-- We use the demand as our recursive measure to guarantee termination.
strictifyDictDmd :: Type -> Demand -> Demand
strictifyDictDmd ty (n :* Prod b ds)
  | not (isAbs n)
  , Just field_tys <- as_non_newtype_dict ty
  = C_1N :* mkProd b (zipWith strictifyDictDmd field_tys ds)
      -- main idea: ensure it's strict
  where
    -- Return a TyCon and a list of field types if the given
    -- type is a non-newtype dictionary type
    as_non_newtype_dict ty
      | isTerminatingType ty
      , Just (_tc, _arg_tys, _data_con, field_tys) <- splitDataProductType_maybe ty
      = Just (map scaledThing field_tys)
      | otherwise
      = Nothing
strictifyDictDmd _  dmd = dmd

-- | Make a 'Demand' lazy.
lazifyDmd :: Demand -> Demand
lazifyDmd = multDmd C_01

-- | Adjust the demand on a binding that may float outwards
-- See Note [Floatifying demand info when floating]
floatifyDmd :: Demand -> Demand
floatifyDmd = multDmd C_0N

-- | Wraps the 'SubDemand' with a one-shot call demand: @d@ -> @C(1,d)@.
mkCalledOnceDmd :: SubDemand -> SubDemand
mkCalledOnceDmd sd = mkCall C_11 sd

-- | @mkCalledOnceDmds n d@ returns @C(1,C1...C(1,d))@ where there are @n@ @C1@'s.
mkCalledOnceDmds :: Arity -> SubDemand -> SubDemand
mkCalledOnceDmds arity sd = iterate mkCalledOnceDmd sd !! arity

-- | Peels one call level from the sub-demand, and also returns how many
-- times we entered the lambda body.
peelCallDmd :: SubDemand -> (Card, SubDemand)
peelCallDmd sd = viewCall sd `orElse` (topCard, topSubDmd)

-- Peels multiple nestings of 'Call' sub-demands and also returns
-- whether it was unsaturated in the form of a 'Card'inality, denoting
-- how many times the lambda body was entered.
-- See Note [Demands from unsaturated function calls].
peelManyCalls :: Arity -> SubDemand -> (Card, SubDemand)
peelManyCalls k sd = go k C_11 sd
  where
    go 0 !n !sd                        = (n, sd)
    go k !n (viewCall -> Just (m, sd)) = go (k-1) (n `multCard` m) sd
    go _ _  _                          = (topCard, topSubDmd)
{-# INLINE peelManyCalls #-} -- so that the pair cancels away in a `fst _` context

strictCallArity :: SubDemand -> Arity
strictCallArity sd = go 0 sd
  where
    go n (Call card sd) | isStrict card = go (n+1) sd
    go n _                              = n

-- | Extract the 'SubDemand' of a 'Demand'.
-- PRECONDITION: The SubDemand must be used in a context where the expression
-- denoted by the Demand is under evaluation.
subDemandIfEvaluated :: Demand -> SubDemand
subDemandIfEvaluated (_ :* sd) = sd

-- See Note [Demand on the worker] in GHC.Core.Opt.WorkWrap
mkWorkerDemand :: Int -> Demand
mkWorkerDemand n = C_01 :* go n
  where go 0 = topSubDmd
        go n = mkCall C_01 $ go (n-1)

argsOneShots :: DmdSig -> Arity -> [[OneShotInfo]]
-- ^ See Note [Computing one-shot info]
argsOneShots (DmdSig (DmdType _ arg_ds)) n_val_args
  | unsaturated_call = []
  | otherwise = go arg_ds
  where
    unsaturated_call = arg_ds `lengthExceeds` n_val_args

    go []               = []
    go (arg_d : arg_ds) = argOneShots arg_d `cons` go arg_ds

    -- Avoid list tail like [ [], [], [] ]
    cons [] [] = []
    cons a  as = a:as

argOneShots :: Demand          -- ^ depending on saturation
            -> [OneShotInfo]
-- ^ See Note [Computing one-shot info]
argOneShots AbsDmd    = [] -- This defn conflicts with 'saturatedByOneShots',
argOneShots BotDmd    = [] -- according to which we should return
                           -- @repeat OneShotLam@ here...
argOneShots (_ :* sd) = map go (callCards sd)
  where
    go n | isAtMostOnce n = OneShotLam
         | otherwise      = NoOneShotInfo

-- | See Note [Computing one-shot info]
callCards :: SubDemand -> [Card]
callCards (Call n sd) = n : callCards sd
callCards (Poly _ _n) = [] -- n is never C_01 or C_11 so we may as well stop here
callCards Prod{}      = []

-- |
-- @saturatedByOneShots n C(M,C(M,...)) = True@
--   <=>
-- There are at least n nested C(M,..) calls.
-- See Note [Demand on the worker] in GHC.Core.Opt.WorkWrap
saturatedByOneShots :: Int -> Demand -> Bool
saturatedByOneShots _ AbsDmd    = True
saturatedByOneShots _ BotDmd    = True
saturatedByOneShots n (_ :* sd) = isAtMostOnce $ fst $ peelManyCalls n sd

{- Note [Strict demands]
~~~~~~~~~~~~~~~~~~~~~~~~
'isStrUsedDmd' returns true only of demands that are
   both strict
   and  used

In particular, it is False for <B> (i.e. strict and not used,
cardinality C_10), which can and does arise in, say (#7319)
   f x = raise# <some exception>
Then 'x' is not used, so f gets strictness <B> -> .
Now the w/w generates
   fx = let x <B> = absentError "unused"
        in raise <some exception>
At this point we really don't want to convert to
   fx = case absentError "unused" of x -> raise <some exception>
Since the program is going to diverge, this swaps one error for another,
but it's really a bad idea to *ever* evaluate an absent argument.
In #7319 we get
   T7319.exe: Oops!  Entered absent arg w_s1Hd{v} [lid] [base:GHC.Base.String{tc 36u}]

Note [SubDemand denotes at least one evaluation]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider a demand `n :* sd` on a binding `let x = e in <body>`.
(Similarly, a call sub-demand `Cn(sd)` on a lambda `\_. e`).
While `n` describes how *often* `x` had been evaluated in <body>,
the sub-demand `sd` describes how *deep* `e` has been evaluated, under the
following

  PREMISE: *for all program traces where `x` had been evaluated at all*

That is, `sd` disregards all program traces where `x` had not been evaluated,
because it can't describe the depth of an evaluation that never happened.
NB: The Premise only makes a difference for lower bounds/strictness.
Upper bounds/usage are unaffected by adding or leaving out evaluations that
never happen.

The Premise comes into play when we have lazy Demands. For example, if `x` was
demanded with `LP(SL,A)`, so perhaps the full expression was
  let x = (e1, e2) in (x `seq` fun y `seq` case x of (a,b) -> a, True)
then `x` will be evaluated lazily, but in any trace in which `x` is evaluated,
the pair in its RHS will ultimately be evaluated deeply with sub-demand
`P(SL,A)`. That means that `e1` is ultimately evaluated strictly, even though
evaluation of the field does not directly follow the eval of `x` due to the
intermittent call `fun y`.

How does the additional strictness help? The long version is the list of
examples at the end of this Note (as procured in #21081 and #18903).
The short version is

  * We get to take advantage of call-by-value/let-to-case in more situations,
    as for e1 above. See example "More let-to-case" below.
  * Note [Eta reduction based on evaluation context] applies in more situations.
    See example "More eta reduction" below.
  * We get to unbox more results, see example "More CPR" below.

It seems like we don't give up anything in return. Indeed that is the case:

  * If we dropped the Premise, then a lazy `n` in `nP(m..)` would always force
    `m` to be lazy, too. That is quite redundant! It seems wasteful not to use
    the lower bound of `m` for something more useful. So indeed we give up on
    nothing in return for some nice wins.
  * Even if `n` is absent (so the Premise does hold for no trace whatsoever),
    it's pretty easy to describe how `e` was evaluated. Answer: 'botSubDmd'.
    We use it when expanding 'Absent' and 'Bottom' demands in 'viewDmdPair' as
    well as when expanding absent 'Poly's to 'Call' sub-demands in 'viewCall'.

Of course, we now have to maintain the Premise when we unpack and rebuild
Demands. For strict demands, we know that the Premise indeed always holds for
any program trace abstracted over, whereas we have to be careful for lazy
demands.

In particular, when doing `plusDmd` we have to *lazify* the nested SubDemand
if the outer cardinality is lazy. E.g.,
  LP(SL) + SP(L) = (L+S)P((M*SL)+L) = SP(L+L) = SP(L)
Multiplying with `M`/`C_01` is the "lazify" part here and is implemented in
`lazifyIfStrict`. Example proving that point:
  d2 :: <LP(SL)><SP(A)>
  d2 x y = y `seq` (case x of (a,b) -> a, True)
  -- What is the demand on x in (d2 x x)? NOT SP(SL)!!

We used to apply the same reasoning to Call SubDemands `Cn(sd)` in `plusSubDmd`,
but that led to #21717, because different calls return different heap objects.
See Note [Call SubDemand vs. evaluation Demand].

There are a couple more examples that improve in T21081.
Here is a selection of those examples demonstrating the usefulness of The
Premise:

  * "More let-to-case" (from testcase T21081):
    ```hs
    f :: (Bool, Bool) -> (Bool, Bool)
    f pr = (case pr of (a,b) -> a /= b, True)
    g :: Int -> (Bool, Bool)
    g x = let y = let z = odd x in (z,z) in f y
    ```
    Although `f` is lazy in `pr`, we could case-bind `z` because it is always
    evaluated when `y` is evaluated. So we give `pr` demand `LP(SL,SL)`
    (most likely with better upper bounds/usage) and demand analysis then
    infers a strict demand for `z`.

  * "More eta reduction" (from testcase T21081):
    ```hs
    myfoldl :: (a -> b -> a) -> a -> [b] -> a
    myfoldl f z [] = z
    myfoldl f !z (x:xs) = myfoldl (\a b -> f a b) (f z x) xs
    ```
    Here, we can give `f` a demand of `LC(S,C(1,L))` (instead of the lazier
    `LC(L,C(1,L))`) which says "Whenever `f` is evaluated (lazily), it is also
    called with two arguments".
    And Note [Eta reduction based on evaluation context] means we can rewrite
    `\a b -> f a b` to `f` in the call site of `myfoldl`. Nice!

  * "More CPR" (from testcase T18903):
    ```hs
    h :: Int -> Int
    h m =
      let g :: Int -> (Int,Int)
          g 1 = (m, 0)
          g n = (2 * n, 2 `div` n)
          {-# NOINLINE g #-}
      in case m of
        1 -> 0
        2 -> snd (g m)
        _ -> uncurry (+) (g m)
    ```
    We want to give `g` the demand `MC(1,P(MP(L),1P(L)))`, so we see that in each
    call site of `g`, we are strict in the second component of the returned
    pair. That in turn means that Nested CPR can unbox the result of the
    division even though it might throw.

Note [Unrealised opportunity in plusDmd]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Recall the lazification of SubDemands happening in `plusDmd` as described in
Note [SubDemand denotes at least one evaluation].

We *could* do better when both Demands are lazy already. Example
  (fun 1, fun 2)
Both args put Demand SC(S,L) on `fun`. The lazy pair arg context lazifies
this to LC(S,L), and it would be reasonable to report this Demand on `fun` for
the entire pair expression; after all, `fun` is called whenever it is evaluated.
But our definition of `plusDmd` will compute
  LC(S,L) + LC(S,L) = (L+L)(M*C(S,L) + M*C(S,L)) = L(C(L,L)) = L
Which is clearly less precise.
Doing better here could mean to `lub` when both demands are lazy, e.g.,
  LC(S,L) + LC(S,L) = (L+L)(C(S,L) ⊔ C(S,L)) = L(C(S,L))
Indeed that's what we did at one point between 9.4 and 9.6 after !7599, but it
means that we need a function `lubPlusSubDmd` that lubs on lower bounds but
plus'es upper bounds, implying maintenance challenges and complicated
explanations.

Plus, NoFib says that this special case doesn't bring all that much
(geom. mean +0.0% counted instructions), so we don't bother anymore.

Note [Call SubDemand vs. evaluation Demand]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Although both evaluation Demands and Call SubDemands carry a (Card,SubDemand)
pair, their interpretation is quite different. Example:

  f x = fst x * snd x
    -- f :: <SP(1L,1L)>, because 1P(1L,A)+1P(A,1L) = SP(1L,1L)
  g x = fst (x 1) * snd (x 2)
    -- g :: <SC(S,P(ML,ML))>, because 1C(1,P(1L,A))+1C(1,P(A,1L)) = SC(S,P(ML,ML))

The point about this example is that both demands have P(A,1L)/P(1L,A) as
sub-expressions, but when these sub-demands occur

  1. under an evaluation demand, we combine with `plusSubDmd`
  2. whereas under a Call sub-demand, we combine with `lubSubDmd`

And thus (1) yields a stricter demand on the pair components than (2).

In #21717 we saw that we really need lub in (2), because otherwise we make an
unsound prediction in `g (\n -> if n == 1 then (1,1) else (bot,2))`; we'd say
that the `bot` expression is always evaluated, when it clearly is not.
Operationally, every call to `g` gives back a potentially distinct,
heap-allocated pair with potentially different contents, and we must `lubSubDmd`
over all such calls to approximate how any of those pairs might be used.

That is in stark contrast to f's argument `x`: Operationally, every eval of
`x` must yield the same pair and `f` evaluates both components of that pair.
The theorem "every eval of `x` returns the same heap object" is a very strong
MUST-alias property and we capitalise on that by using `plusSubDmd` in (1).

And indeed we *must* use `plusSubDmd` in (1) for sound upper bounds in an
analysis that assumes call-by-need (as opposed to the weaker call-by-name) for
let bindings. Consider

  h x = fst x * fst x
    -- h :: <SP(SL,A)>

And the expression `let a=1; p=(a,a)} in h p`. Here, *although* the RHS of `p`
is only evaluated once under call-by-need, `a` is still evaluated twice.
If we had used `lubSubDmd`, we'd see SP(1L,A) and the 1L unsoundly says "exactly
once".

If the analysis had assumed call-by-name, it would be sound to say "a is used
once in p": p is used multiple times and hence so would a, as if p was a
function. So using `plusSubDmd` does not only yield better strictness, it is
also "holding up the other end of the bargain" of the call-by-need assumption
for upper bounds.

(To SG's knowledge, the distinction between call-by-name and call-by-need does
not matter for strictness analysis/lower bounds, thus it would be sound to use
`lubSubDmd` all the time there.)

Note [mkCall and plusSubDmd]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We never rewrite a strict, non-absent Call sub-demand like C(S,S) to a
polymorphic sub-demand like S, otherwise #21085 strikes. Consider the
following inequality (would also for M and 1 instead of L and S, but we forbid
such Polys):

  L+S = S = C(S,S) < C(S,L) = C(L,L)+C(S,S)

Note that L=C(L,L). If we also had S=C(S,S), we'd be in trouble: Now
`plusSubDmd` would no longer maintain the equality relation on sub-demands,
much less monotonicity. Bad!

Clearly, `n <= Cn(n)` is unproblematic, as is `n >= Cn(n)` for any `n`
except 1 and S. But `C(S,S) >= S` would mean trouble, because then we'd get
the problematic `C(S,S) = S`. We have just established that `S < C(S,S)`!
As such, the rewrite C(S,S) to S is anti-monotone and we forbid it, first
and foremost in `mkCall` (which is the only place that rewrites Cn(n) to n).

Crisis and #21085 averted!

Note [Computing one-shot info]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider a call
    f (\pqr. e1) (\xyz. e2) e3
where f has usage signature
    <C(M,C(L,C(M,L)))><C(M,L)><L>
Then argsOneShots returns a [[OneShotInfo]] of
    [[OneShot,NoOneShotInfo,OneShot],  [OneShot]]
The occurrence analyser propagates this one-shot infor to the
binders \pqr and \xyz;
see Note [Sources of one-shot information] in GHC.Core.Opt.OccurAnal.

Note [Boxity in Poly]
~~~~~~~~~~~~~~~~~~~~~
To support Note [Boxity analysis], it makes sense that 'Prod' carries a
'Boxity'. But why does 'Poly' have to carry a 'Boxity', too? Shouldn't all
'Poly's be 'Boxed'? Couldn't we simply use 'Prod Unboxed' when we need to
express an unboxing demand?

'botSubDmd' (B) needs to be the bottom of the lattice, so it needs to be an
Unboxed demand (and deeply, at that). Similarly, 'seqSubDmd' (A) is an Unboxed
demand. So why not say that Polys with absent cardinalities have Unboxed boxity?
That doesn't work, because we also need the boxed equivalents. Here's an example
for A (function 'absent' in T19871):
```
f _ True  = 1
f a False = a `seq` 2
  -- demand on a: MA, the A is short for `Poly Boxed C_00`

g a = a `seq` f a True
  -- demand on a: SA, which is `Poly Boxed C_00`

h True  p       = g p -- SA on p (inherited from g)
h False p@(x,y) = x+y -- S!P(1!L,1!L) on p
```
If A is treated as Unboxed, we get reboxing in the call site to 'g'.
So we obviously would need a Boxed variant of A. Rather than introducing a lot
of special cases, we just carry the Boxity in 'Poly'. Plus, we could most likely
find examples like the above for any other cardinality.

Note [Why Boxity in SubDemand and not in Demand?]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In #19871, we started out by storing 'Boxity' in 'SubDemand', in the 'Prod'
constructor only. But then we found that we weren't able to express the unboxing
'seqSubDmd', because that one really is a `Poly C_00` sub-demand.
We then tried to store the Boxity in 'Demand' instead, for these reasons:

  1. The whole boxity-of-seq business comes to a satisfying conclusion
  2. Putting Boxity in the SubDemand is weird to begin with, because it
     describes the box and not its fields, just as the evaluation cardinality
     of a Demand describes how often the box is used. It makes more sense that
     Card and Boxity travel together. Also the alternative would have been to
     store Boxity with Poly, which is even weirder and more redundant.

But then we regressed in T7837 (grep #19871 for boring specifics), which needed
to transfer an ambient unboxed *demand* on a dictionary selector to its argument
dictionary, via a 'Call' sub-demand `C(1,sd)`, as
Note [Demand transformer for a dictionary selector] explains. Annoyingly,
the boxity info has to be stored in the *sub-demand* `sd`! There's no demand
to store the boxity in. So we bit the bullet and now we store Boxity in
'SubDemand', both in 'Prod' *and* 'Poly'. See also Note [Boxity in Poly].

Note [Demand transformer for data constructors]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider the expression (x,y) with sub-demand P(SL,A).  What is the demand on
x,y?  Obviously `x` is used strictly, and `y` not at all. So we want to
decompose a product demand, and feed its components demands into the
arguments.  That is the job of dmdTransformDataConSig.  More precisely,

 * it gets the demand on the data constructor itself;
   in the above example that is C(1,C(1,P(SL,A)))
 * it returns the demands on the arguments;
   in the above example that is [SL, A]

When the data constructor worker has strict fields, an additional seq
will be inserted for each field (see (SFC3) in Note [Strict fields in Core]).
Hence we add an additional `seqDmd` for each strict field to emulate
field eval insertion.

For example, consider `data SP a b = MkSP !a !b` and expression `MkSP x y`,
with the same sub-demand P(SL,A).
The strict fields bump up the strictness; we'd get [SL,1!A] for the field
demands. Note that the first demand was unaffected by the seq, whereas
the second, previously absent demand became `seqDmd` exactly.
-}

{- *********************************************************************
*                                                                      *
                 Divergence: Whether evaluation surely diverges
*                                                                      *
********************************************************************* -}

-- | 'Divergence' characterises whether something surely diverges.
-- Models a subset lattice of the following exhaustive set of divergence
-- results:
--
-- [n] nontermination (e.g. loops)
-- [i] throws imprecise exception
-- [p] throws precise exception
-- [c] converges (reduces to WHNF).
--
-- The different lattice elements correspond to different subsets, indicated by
-- juxtaposition of indicators (e.g. __nc__ definitely doesn't throw an
-- exception, and may or may not reduce to WHNF).
--
-- @
--             Dunno (nipc)
--                  |
--            ExnOrDiv (nip)
--                  |
--            Diverges (ni)
-- @
--
-- As you can see, we don't distinguish __n__ and __i__.
-- See Note [Precise exceptions and strictness analysis] for why __p__ is so
-- special compared to __i__.
data Divergence
  = Diverges -- ^ Definitely throws an imprecise exception or diverges.
  | ExnOrDiv -- ^ Definitely throws a *precise* exception, an imprecise
             --   exception or diverges. Never converges, hence 'isDeadEndDiv'!
             --   See scenario 1 in Note [Precise exceptions and strictness analysis].
  | Dunno    -- ^ Might diverge, throw any kind of exception or converge.
  deriving Eq

lubDivergence :: Divergence -> Divergence -> Divergence
lubDivergence Diverges div      = div
lubDivergence div      Diverges = div
lubDivergence ExnOrDiv ExnOrDiv = ExnOrDiv
lubDivergence _        _        = Dunno
-- This needs to commute with defaultFvDmd, i.e.
-- defaultFvDmd (r1 `lubDivergence` r2) = defaultFvDmd r1 `lubDmd` defaultFvDmd r2
-- (See Note [Default demand on free variables and arguments] for why)

-- | See Note [Asymmetry of plusDmdType], which concludes that 'plusDivergence'
-- needs to be symmetric.
-- Strictly speaking, we should have @plusDivergence Dunno Diverges = ExnOrDiv@.
-- But that regresses in too many places (every infinite loop, basically) to be
-- worth it and is only relevant in higher-order scenarios
-- (e.g. Divergence of @f (throwIO blah)@).
-- So 'plusDivergence' currently is 'glbDivergence', really.
plusDivergence :: Divergence -> Divergence -> Divergence
plusDivergence Dunno    Dunno    = Dunno
plusDivergence Diverges _        = Diverges
plusDivergence _        Diverges = Diverges
plusDivergence _        _        = ExnOrDiv

-- | In a non-strict scenario, we might not force the Divergence, in which case
-- we might converge, hence Dunno.
multDivergence :: Card -> Divergence -> Divergence
multDivergence n _ | not (isStrict n) = Dunno
multDivergence _ d                    = d

topDiv, exnDiv, botDiv :: Divergence
topDiv = Dunno
exnDiv = ExnOrDiv
botDiv = Diverges

-- | True if the 'Divergence' indicates that evaluation will not return.
-- See Note [Dead ends].
isDeadEndDiv :: Divergence -> Bool
isDeadEndDiv Diverges = True
isDeadEndDiv ExnOrDiv = True
isDeadEndDiv Dunno    = False

-- See Notes [Default demand on free variables and arguments]
-- and Scenario 1 in [Precise exceptions and strictness analysis]
defaultFvDmd :: Divergence -> Demand
defaultFvDmd Dunno    = absDmd
defaultFvDmd ExnOrDiv = absDmd -- This is the whole point of ExnOrDiv!
defaultFvDmd Diverges = botDmd -- Diverges

defaultArgDmd :: Divergence -> Demand
-- TopRes and BotRes are polymorphic, so that
--      BotRes === (Bot -> BotRes) === ...
--      TopRes === (Top -> TopRes) === ...
-- This function makes that concrete
-- Also see Note [Default demand on free variables and arguments]
defaultArgDmd Dunno    = topDmd
-- NB: not botDmd! We don't want to mask the precise exception by forcing the
-- argument. But it is still absent.
defaultArgDmd ExnOrDiv = absDmd
defaultArgDmd Diverges = botDmd

{- Note [Precise vs imprecise exceptions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
An exception is considered to be /precise/ when it is thrown by the 'raiseIO#'
primop. It follows that all other primops (such as 'raise#' or
division-by-zero) throw /imprecise/ exceptions. Note that the actual type of
the exception thrown doesn't have any impact!

GHC undertakes some effort not to apply an optimisation that would mask a
/precise/ exception with some other source of nontermination, such as genuine
divergence or an imprecise exception, so that the user can reliably
intercept the precise exception with a catch handler before and after
optimisations.

See also the wiki page on precise exceptions:
https://gitlab.haskell.org/ghc/ghc/wikis/exceptions/precise-exceptions
Section 5 of "Tackling the awkward squad" talks about semantic concerns.
Imprecise exceptions are actually more interesting than precise ones (which are
fairly standard) from the perspective of semantics. See the paper "A Semantics
for Imprecise Exceptions" for more details.

Note [Dead ends]
~~~~~~~~~~~~~~~~
We call an expression that either diverges or throws a precise or imprecise
exception a "dead end". We used to call such an expression just "bottoming",
but with the measures we take to preserve precise exception semantics
(see Note [Precise exceptions and strictness analysis]), that is no longer
accurate: 'exnDiv' is no longer the bottom of the Divergence lattice.

Yet externally to demand analysis, we mostly care about being able to drop dead
code etc., which is all due to the property that such an expression never
returns, hence we consider throwing a precise exception to be a dead end.
See also 'isDeadEndDiv'.

Note [Precise exceptions and strictness analysis]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We have to take care to preserve precise exception semantics in strictness
analysis (#17676). There are two scenarios that need careful treatment.

The fixes were discussed at
https://gitlab.haskell.org/ghc/ghc/wikis/fixing-precise-exceptions

Recall that raiseIO# raises a *precise* exception, in contrast to raise# which
raises an *imprecise* exception. See Note [Precise vs imprecise exceptions].

Scenario 1: Precise exceptions in case alternatives
---------------------------------------------------
Unlike raise# (which returns botDiv), we want raiseIO# to return exnDiv.
Here's why. Consider this example from #13380 (similarly #17676):
  f x y | x>0       = raiseIO# Exc
        | y>0       = return 1
        | otherwise = return 2
Is 'f' strict in 'y'? One might be tempted to say yes! But that plays fast and
loose with the precise exception; after optimisation, (f 42 (error "boom"))
turns from throwing the precise Exc to throwing the imprecise user error
"boom". So, the defaultFvDmd of raiseIO# should be lazy (topDmd), which can be
achieved by giving it divergence exnDiv.
See Note [Default demand on free variables and arguments].

Why don't we just give it topDiv instead of introducing exnDiv?
Because then the simplifier will fail to discard raiseIO#'s continuation in
  case raiseIO# x s of { (# s', r #) -> <BIG> }
which we'd like to optimise to
  case raiseIO# x s of {}
Hence we came up with exnDiv. The default FV demand of exnDiv is lazy (and
its default arg dmd is absent), but otherwise (in terms of 'isDeadEndDiv') it
behaves exactly as botDiv, so that dead code elimination works as expected.
This is tracked by T13380b.

Scenario 2: Precise exceptions in case scrutinees
-------------------------------------------------
Consider (more complete examples in #148, #1592, testcase strun003)

  case foo x s of { (# s', r #) -> y }

Is this strict in 'y'? Often not! If @foo x s@ might throw a precise exception
(ultimately via raiseIO#), then we must not force 'y', which may fail to
terminate or throw an imprecise exception, until we have performed @foo x s@.

So we have to 'deferAfterPreciseException' (which 'lub's with 'exnDmdType' to
model the exceptional control flow) when @foo x s@ may throw a precise
exception. Motivated by T13380{d,e,f}.
See Note [Which scrutinees may throw precise exceptions] in "GHC.Core.Opt.DmdAnal".

We have to be careful not to discard dead-end Divergence from case
alternatives, though (#18086):

  m = putStrLn "foo" >> error "bar"

'm' should still have 'exnDiv', which is why it is not sufficient to lub with
'nopDmdType' (which has 'topDiv') in 'deferAfterPreciseException'.

Historical Note: This used to be called the "IO hack". But that term is rather
a bad fit because
1. It's easily confused with the "State hack", which also affects IO.
2. Neither "IO" nor "hack" is a good description of what goes on here, which
   is deferring strictness results after possibly throwing a precise exception.
   The "hack" is probably not having to defer when we can prove that the
   expression may not throw a precise exception (increasing precision of the
   analysis), but that's just a favourable guess.

Note [Side-effects and strictness]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Due to historic reasons and the continued effort not to cause performance
regressions downstream, Strictness Analysis is currently prone to discarding
observable side-effects (other than precise exceptions, see
Note [Precise exceptions and strictness analysis]) in some cases. For example,
  f :: MVar () -> Int -> IO Int
  f mv x = putMVar mv () >> (x `seq` return x)
The call to `putMVar` is an observable side-effect. Yet, Strictness Analysis
currently concludes that `f` is strict in `x` and uses call-by-value.
That means `f mv (error "boom")` will error out with the imprecise exception
rather performing the side-effect.

This is a conscious violation of the semantics described in the paper
"a semantics for imprecise exceptions"; so it would be great if we could
identify the offending primops and extend the idea in
Note [Which scrutinees may throw precise exceptions] to general side-effects.

Unfortunately, the existing has-side-effects classification for primops is
too conservative, listing `writeMutVar#` and even `readMutVar#` as
side-effecting. That is due to #3207. A possible way forward is described in
#17900, but no effort has been so far towards a resolution.

Note [Exceptions and strictness]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We used to smart about catching exceptions, but we aren't anymore.
See #14998 for the way it's resolved at the moment.

Here's a historic breakdown:

Apparently, exception handling prim-ops didn't use to have any special
strictness signatures, thus defaulting to nopSig, which assumes they use their
arguments lazily. Joachim was the first to realise that we could provide richer
information. Thus, in 0558911f91c (Dec 13), he added signatures to
primops.txt.pp indicating that functions like `catch#` and `catchRetry#` call
their argument, which is useful information for usage analysis. Still with a
'Lazy' strictness demand (i.e. 'lazyApply1Dmd'), though, and the world was fine.

In 7c0fff4 (July 15), Simon argued that giving `catch#` et al. a
'strictApply1Dmd' leads to substantial performance gains. That was at the cost
of correctness, as #10712 proved. So, back to 'lazyApply1Dmd' in
28638dfe79e (Dec 15).

Motivated to reproduce the gains of 7c0fff4 without the breakage of #10712,
Ben opened #11222. Simon made the demand analyser "understand catch" in
9915b656 (Jan 16) by adding a new 'catchArgDmd', which basically said to call
its argument strictly, but also swallow any thrown exceptions in
'multDivergence'. This was realized by extending the 'Str' constructor of
'ArgStr' with a 'ExnStr' field, indicating that it catches the exception, and
adding a 'ThrowsExn' constructor to the 'Divergence' lattice as an element
between 'Dunno' and 'Diverges'. Then along came #11555 and finally #13330,
so we had to revert to 'lazyApply1Dmd' again in 701256df88c (Mar 17).

This left the other variants like 'catchRetry#' having 'catchArgDmd', which is
where #14998 picked up. Item 1 was concerned with measuring the impact of also
making `catchRetry#` and `catchSTM#` have 'lazyApply1Dmd'. The result was that
there was none. We removed the last usages of 'catchArgDmd' in 00b8ecb7
(Apr 18). There was a lot of dead code resulting from that change, that we
removed in ef6b283 (Jan 19): We got rid of 'ThrowsExn' and 'ExnStr' again and
removed any code that was dealing with the peculiarities.

Where did the speed-ups vanish to? In #14998, item 3 established that
turning 'catch#' strict in its first argument didn't bring back any of the
alleged performance benefits. Item 2 of that ticket finally found out that it
was entirely due to 'catchException's new (since #11555) definition, which
was simply

    catchException !io handler = catch io handler

While 'catchException' is arguably the saner semantics for 'catch', it is an
internal helper function in "GHC.IO". Its use in
"GHC.IO.Handle.Internals.do_operation" made for the huge allocation differences:
Remove the bang and you find the regressions we originally wanted to avoid with
'catchArgDmd'. See also #exceptions_and_strictness# in "GHC.IO".

So history keeps telling us that the only possibly correct strictness annotation
for the first argument of 'catch#' is 'lazyApply1Dmd', because 'catch#' really
is not strict in its argument: Just try this in GHCi

  :set -XScopedTypeVariables
  import Control.Exception
  catch undefined (\(_ :: SomeException) -> putStrLn "you'll see this")

Any analysis that assumes otherwise will be broken in some way or another
(beyond `-fno-pedantic-bottoms`).

But then #13380 and #17676 suggest (in Mar 20) that we need to re-introduce a
subtly different variant of `ThrowsExn` (which we call `ExnOrDiv` now) that is
only used by `raiseIO#` in order to preserve precise exceptions by strictness
analysis, while not impacting the ability to eliminate dead code.
See Note [Precise exceptions and strictness analysis].

Note [Default demand on free variables and arguments]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Free variables not mentioned in the environment of a 'DmdType'
are demanded according to the demand type's Divergence:
  * In a Diverges (botDiv) context, that demand is botDmd
    (strict and absent).
  * In all other contexts, the demand is absDmd (lazy and absent).
This is recorded in 'defaultFvDmd'.

Similarly, we can eta-expand demand types to get demands on excess arguments
not accounted for in the type, by consulting 'defaultArgDmd':
  * In a Diverges (botDiv) context, that demand is again botDmd.
  * In a ExnOrDiv (exnDiv) context, that demand is absDmd: We surely diverge
    before evaluating the excess argument, but don't want to eagerly evaluate
    it (cf. Note [Precise exceptions and strictness analysis]).
  * In a Dunno context (topDiv), the demand is topDmd, because
    it's perfectly possible to enter the additional lambda and evaluate it
    in unforeseen ways (so, not absent).

Note [Bottom CPR iff Dead-Ending Divergence]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Both CPR analysis and Demand analysis handle recursive functions by doing
fixed-point iteration. To find the *least* (e.g., most informative) fixed-point,
iteration starts with the bottom element of the semantic domain. Diverging
functions generally have the bottom element as their least fixed-point.

One might think that CPR analysis and Demand analysis then agree in when a
function gets a bottom denotation. E.g., whenever it has 'botCpr', it should
also have 'botDiv'. But that is not the case, because strictness analysis has to
be careful around precise exceptions, see Note [Precise vs imprecise exceptions].

So Demand analysis gives some diverging functions 'exnDiv' (which is *not* the
bottom element) when the CPR signature says 'botCpr', and that's OK. Here's an
example (from #18086) where that is the case:

ioTest :: IO ()
ioTest = do
  putStrLn "hi"
  undefined

However, one can loosely say that we give a function 'botCpr' whenever its
'Divergence' is 'exnDiv' or 'botDiv', i.e., dead-ending. But that's just
a consequence of fixed-point iteration, it's not important that they agree.

************************************************************************
*                                                                      *
           Demand environments and types
*                                                                      *
************************************************************************
-}

-- Subject to Note [Default demand on free variables and arguments]
-- | Captures the result of an evaluation of an expression, by
--
--   * Listing how the free variables of that expression have been evaluated
--     ('de_fvs')
--   * Saying whether or not evaluation would surely diverge ('de_div')
--
-- See Note [Demand env Equality].
data DmdEnv = DE { de_fvs :: !(VarEnv Demand), de_div :: !Divergence }

instance Eq DmdEnv where
  DE fv1 div1 == DE fv2 div2
    = div1 == div2 && canonicalise div1 fv1 == canonicalise div2 fv2
    where
      canonicalise div fv = filterUFM (/= defaultFvDmd div) fv

mkEmptyDmdEnv :: Divergence -> DmdEnv
mkEmptyDmdEnv div = DE emptyVarEnv div

-- | Build a potentially terminating 'DmdEnv' from a finite map that says what
-- has been evaluated so far
mkTermDmdEnv :: VarEnv Demand -> DmdEnv
mkTermDmdEnv fvs = DE fvs topDiv

nopDmdEnv :: DmdEnv
nopDmdEnv = mkEmptyDmdEnv topDiv

botDmdEnv :: DmdEnv
botDmdEnv = mkEmptyDmdEnv botDiv

exnDmdEnv :: DmdEnv
exnDmdEnv = mkEmptyDmdEnv exnDiv

lubDmdEnv :: DmdEnv -> DmdEnv -> DmdEnv
lubDmdEnv (DE fv1 d1) (DE fv2 d2) = DE lub_fv lub_div
  where
    -- See Note [Demand env Equality]
    lub_fv  = plusVarEnv_CD lubDmd fv1 (defaultFvDmd d1) fv2 (defaultFvDmd d2)
    lub_div = lubDivergence d1 d2

addVarDmdEnv :: DmdEnv -> Id -> Demand -> DmdEnv
addVarDmdEnv env@(DE fvs div) id dmd
  = DE (extendVarEnv fvs id (dmd `plusDmd` lookupDmdEnv env id)) div

plusDmdEnv :: DmdEnv -> DmdEnv -> DmdEnv
plusDmdEnv (DE fv1 d1) (DE fv2 d2)
  -- In contrast to Note [Asymmetry of plusDmdType], this function is symmetric.
  | isEmptyVarEnv fv2, defaultFvDmd d2 == absDmd
  = DE fv1 (d1 `plusDivergence` d2) -- a very common case that is much more efficient
  | isEmptyVarEnv fv1, defaultFvDmd d1 == absDmd
  = DE fv2 (d1 `plusDivergence` d2) -- another very common case that is much more efficient
  | otherwise
  = DE (plusVarEnv_CD plusDmd fv1 (defaultFvDmd d1) fv2 (defaultFvDmd d2))
       (d1 `plusDivergence` d2)

-- | 'DmdEnv' is a monoid via 'plusDmdEnv' and 'nopDmdEnv'; this is its 'msum'
plusDmdEnvs :: [DmdEnv] -> DmdEnv
plusDmdEnvs []   = nopDmdEnv
plusDmdEnvs pdas = foldl1' plusDmdEnv pdas

multDmdEnv :: Card -> DmdEnv -> DmdEnv
multDmdEnv C_11 env          = env
multDmdEnv C_00 _            = nopDmdEnv
multDmdEnv n    (DE fvs div) = DE (mapVarEnv (multDmd n) fvs) (multDivergence n div)

reuseEnv :: DmdEnv -> DmdEnv
reuseEnv = multDmdEnv C_1N

lookupDmdEnv :: DmdEnv -> Id -> Demand
-- See Note [Default demand on free variables and arguments]
lookupDmdEnv (DE fv div) id = lookupVarEnv fv id `orElse` defaultFvDmd div

delDmdEnv :: DmdEnv -> Id -> DmdEnv
delDmdEnv (DE fv div) id = DE (fv `delVarEnv` id) div

-- | Characterises how an expression
--
--    * Evaluates its free variables ('dt_env') including divergence info
--    * Evaluates its arguments ('dt_args')
--
data DmdType
  = DmdType
  { dt_env  :: !DmdEnv     -- ^ Demands on free variables.
                           -- See Note [Demand type Divergence]
  , dt_args :: ![Demand]   -- ^ Demand on arguments
  }

-- | See Note [Demand env Equality].
instance Eq DmdType where
  DmdType env1 ds1 == DmdType env2 ds2
    = ds1 == ds2 -- cheap checks first
      && env1 == env2

-- | Compute the least upper bound of two 'DmdType's elicited /by the same
-- incoming demand/!
lubDmdType :: DmdType -> DmdType -> DmdType
lubDmdType d1 d2 = DmdType lub_fv lub_ds
  where
    n = max (dmdTypeDepth d1) (dmdTypeDepth d2)
    (DmdType fv1 ds1) = etaExpandDmdType n d1
    (DmdType fv2 ds2) = etaExpandDmdType n d2
    lub_ds  = zipWithEqual "lubDmdType" lubDmd ds1 ds2
    lub_fv = lubDmdEnv fv1 fv2

discardArgDmds :: DmdType -> DmdEnv
discardArgDmds (DmdType fv _) = fv

plusDmdType :: DmdType -> DmdEnv -> DmdType
plusDmdType (DmdType fv ds) fv'
  -- See Note [Asymmetry of plusDmdType]
  -- 'DmdEnv' forms a (monoidal) action on 'DmdType' via this operation.
  = DmdType (plusDmdEnv fv fv') ds

botDmdType :: DmdType
botDmdType = DmdType botDmdEnv []

-- | The demand type of doing nothing (lazy, absent, no Divergence
-- information). Note that it is ''not'' the top of the lattice (which would be
-- "may use everything"), so it is (no longer) called topDmdType.
nopDmdType :: DmdType
nopDmdType = DmdType nopDmdEnv []

-- | The demand type of an unspecified expression that is guaranteed to
-- throw a (precise or imprecise) exception or diverge.
exnDmdType :: DmdType
exnDmdType = DmdType exnDmdEnv []

dmdTypeDepth :: DmdType -> Arity
dmdTypeDepth = length . dt_args

-- | This makes sure we can use the demand type with n arguments after eta
-- expansion, where n must not be lower than the demand types depth.
-- It appends the argument list with the correct 'defaultArgDmd'.
etaExpandDmdType :: Arity -> DmdType -> DmdType
etaExpandDmdType n d@DmdType{dt_args = ds, dt_env = env}
  | n == depth = d
  | n >  depth = d{dt_args = inc_ds}
  | otherwise  = pprPanic "etaExpandDmdType: arity decrease" (ppr n $$ ppr d)
  where depth = length ds
        -- Arity increase:
        --  * Demands on FVs are still valid
        --  * Demands on args also valid, plus we can extend with defaultArgDmd
        --    as appropriate for the given Divergence
        --  * Divergence is still valid:
        --    - A dead end after 2 arguments stays a dead end after 3 arguments
        --    - The remaining case is Dunno, which is already topDiv
        inc_ds = take n (ds ++ repeat (defaultArgDmd (de_div env)))

-- | A conservative approximation for a given 'DmdType' in case of an arity
-- decrease. Currently, it's just nopDmdType.
decreaseArityDmdType :: DmdType -> DmdType
decreaseArityDmdType _ = nopDmdType

splitDmdTy :: DmdType -> (Demand, DmdType)
-- Split off one function argument
-- We already have a suitable demand on all
-- free vars, so no need to add more!
splitDmdTy ty@DmdType{dt_args=dmd:args} = (dmd, ty{dt_args=args})
splitDmdTy ty@DmdType{dt_env=env}       = (defaultArgDmd (de_div env), ty)

multDmdType :: Card -> DmdType -> DmdType
multDmdType C_11 dmd_ty = dmd_ty -- a vital optimisation for T25196
multDmdType n    (DmdType fv args)
  = -- pprTrace "multDmdType" (ppr n $$ ppr fv $$ ppr (multDmdEnv n fv)) $
    DmdType (multDmdEnv n fv)
            (strictMap (multDmd n) args)

peelFV :: DmdType -> Var -> (DmdType, Demand)
peelFV (DmdType fv ds) id = -- pprTrace "rfv" (ppr id <+> ppr dmd $$ ppr fv)
                            (DmdType fv' ds, dmd)
  where
  -- Force these arguments so that old `Env` is not retained.
  !fv' = fv `delDmdEnv` id
  !dmd = lookupDmdEnv fv id

addDemand :: Demand -> DmdType -> DmdType
addDemand dmd (DmdType fv ds) = DmdType fv (dmd:ds)

findIdDemand :: DmdType -> Var -> Demand
findIdDemand (DmdType fv _) id = lookupDmdEnv fv id

-- | When e is evaluated after executing an IO action that may throw a precise
-- exception, we act as if there is an additional control flow path that is
-- taken if e throws a precise exception. The demand type of this control flow
-- path
--   * is lazy and absent ('topDmd') and boxed in all free variables and arguments
--   * has 'exnDiv' 'Divergence' result
-- See Note [Precise exceptions and strictness analysis]
--
-- So we can simply take a variant of 'nopDmdType', 'exnDmdType'.
-- Why not 'nopDmdType'? Because then the result of 'e' can never be 'exnDiv'!
-- That means failure to drop dead-ends, see #18086.
deferAfterPreciseException :: DmdType -> DmdType
deferAfterPreciseException = lubDmdType exnDmdType

{- Note [deferAfterPreciseException]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The big picture is in Note [Precise exceptions and strictness analysis]
The idea is that we want to treat
   case <I/O operation> of (# s', r #) -> rhs

as if it was
   case <I/O operation> of
      Just (# s', r #) -> rhs
      Nothing          -> error

That is, the I/O operation might throw an exception, so that 'rhs' never
gets reached.  For example, we don't want to be strict in the strict free
variables of 'rhs'.

So we have the simple definition
  deferAfterPreciseException = lubDmdType (DmdType emptyDmdEnv [] exnDiv)

Historically, when we had `lubBoxity = _unboxedWins` (see Note [unboxedWins]),
we had a more complicated definition for deferAfterPreciseException to make sure
it preserved boxity in its argument. That was needed for code like
   case <I/O operation> of
      (# s', r) -> f x

which uses `x` *boxed*. If we `lub`bed it with `(DmdType emptyDmdEnv [] exnDiv)`
we'd get an *unboxed* demand on `x` (because we let Unboxed win),
which led to #20746.  Nowadays with `lubBoxity = boxedWins` we don't need
the complicated definition.

Note [Demand type Divergence]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In contrast to DmdSigs, DmdTypes are elicited under a specific incoming demand.
This is described in detail in Note [Understanding DmdType and DmdSig].
Here, we'll focus on what that means for a DmdType's Divergence in a higher-order
scenario.

Consider
  err x y = x `seq` y `seq` error (show x)
this has a strictness signature of
  <1L><1L>b
meaning that we don't know what happens when we call err in weaker contexts than
C(1,C(1,L)), like @err `seq` ()@ (1A) and @err 1 `seq` ()@ (C(S,A)). We
may not unleash the botDiv, hence assume topDiv. Of course, in
@err 1 2 `seq` ()@ the incoming demand C(S,C(S,A)) is strong enough and we see
that the expression diverges.

Now consider a function
  f g = g 1 2
with signature <C(1,C(1,L))>, and the expression
  f err `seq` ()
now f puts a strictness demand of C(1,C(1,L)) onto its argument, which is unleashed
on err via the App rule. In contrast to weaker head strictness, this demand is
strong enough to unleash err's signature and hence we see that the whole
expression diverges!

Note [Demand env Equality]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
What is the difference between the Demand env {x->A} and {}?
Answer: There is none! They have the exact same semantics, because any var that
is not mentioned in 'de_fvs' implicitly has demand 'defaultFvDmd', based on
the divergence of the demand env 'de_div'.
Similarly, b{x->B, y->A} is the same as b{y->A}, because the default FV
demand of BotDiv is B. But neither is equal to b{}, because y has demand B in
the latter, not A as before.

The Eq instance of DmdEnv must reflect that, otherwise we can get into monotonicity
issues during fixed-point iteration ({x->A} /= {} /= {x->A} /= ...).
It does so by filtering out any default FV demands prior to comparing 'de_fvs'.

Note that 'lubDmdEnv' maintains this kind of equality by using 'plusVarEnv_CD',
involving 'defaultFvDmd' for any entries present in one 'de_fvs' but not the
other.

Note [Asymmetry of plusDmdType]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
'plus' for DmdTypes is *asymmetrical*, because there can only one
be one type contributing argument demands!  For example, given (e1 e2), we get
a DmdType dt1 for e1, use its arg demand to analyse e2 giving dt2, and then do
(dt1 `plusType` dt2). Similarly with
  case e of { p -> rhs }
we get dt_scrut from the scrutinee and dt_rhs from the RHS, and then
compute (dt_rhs `plusType` dt_scrut).

We
 1. combine the information on the free variables,
 2. take the demand on arguments from the first argument
 3. combine the termination results, as in plusDivergence.

Since we don't use argument demands of the second argument anyway, 'plus's
second argument is just a 'PlusDmdType'.

But note that the argument demand types are not guaranteed to be observed in
left to right order. For example, analysis of a case expression will pass the
demand type for the alts as the left argument and the type for the scrutinee as
the right argument. Also, it is not at all clear if there is such an order;
consider the LetUp case, where the RHS might be forced at any point while
evaluating the let body.
Therefore, it is crucial that 'plusDivergence' is symmetric!

Note [Demands from unsaturated function calls]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider a demand transformer d1 -> d2 -> r for f.
If a sufficiently detailed demand is fed into this transformer,
e.g <C(1,C(1,L))> arising from "f x1 x2" in a strict, use-once context,
then d1 and d2 is precisely the demand unleashed onto x1 and x2 (similar for
the free variable environment) and furthermore the result information r is the
one we want to use.

An anonymous lambda is also an unsaturated function all (needs one argument,
none given), so this applies to that case as well.

But the demand fed into f might be less than C(1,C(1,L)). Then we have to
'multDmdType' the announced demand type. Examples:
 * Not strict enough, e.g. C(1,C(1,L)):
   - We have to multiply all argument and free variable demands with C_01,
     zapping strictness.
   - We have to multiply divergence with C_01. If r says that f Diverges for sure,
     then this holds when the demand guarantees that two arguments are going to
     be passed. If the demand is lower, we may just as well converge.
     If we were tracking definite convergence, than that would still hold under
     a weaker demand than expected by the demand transformer.
 * Used more than once, e.g. C(S,C(1,L)):
   - Multiply with C_1N. Even if f puts a used-once demand on any of its argument
     or free variables, if we call f multiple times, we may evaluate this
     argument or free variable multiple times.

In dmdTransformSig, we call peelManyCalls to find out the 'Card'inality with
which we have to multiply and then call multDmdType with that.

Similarly, dmdTransformDictSelSig and dmdAnal, when analyzing a Lambda, use
peelCallDmd, which peels only one level, but also returns the demand put on the
body of the function.
-}


{-
************************************************************************
*                                                                      *
                     Demand signatures
*                                                                      *
************************************************************************

Note [DmdSig: demand signatures, and demand-sig arity]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
See also
  * Note [Demand signatures semantically]
  * Note [Understanding DmdType and DmdSig]

In a let-bound Id we record its demand signature.
In principle, this demand signature is a demand transformer, mapping
a demand on the Id into a DmdType, which gives
        a) the free vars of the Id's value
        b) the Id's arguments
        c) an indication of the result of applying
           the Id to its arguments

However, in fact we store in the Id an extremely emasculated demand
transformer, namely
        a single DmdType
(Nevertheless we dignify DmdSig as a distinct type.)

The DmdSig for an Id is a semantic thing.  Suppose a function `f` has a DmdSig of
  DmdSig (DmdType (fv_dmds,res) [d1..dn])
Here `n` is called the "demand-sig arity" of the DmdSig.  The signature means:
  * If you apply `f` to n arguments (the demand-sig-arity)
  * then you can unleash demands d1..dn on the arguments
  * and demands fv_dmds on the free variables.
Also see Note [Demand type Divergence] for the meaning of a Divergence in a
demand signature.

If `f` is applied to fewer value arguments than its demand-sig arity, it means
that the demand on the function at a call site is weaker than the vanilla call
demand, used for signature inference. Therefore we place a top demand on all
arguments.

For example, the demand transformer described by the demand signature
        DmdSig (DmdType {x -> <1L>} <A><1P(L,L)>)
says that when the function is applied to two arguments, it
unleashes demand 1L on the free var x, A on the first arg,
and 1P(L,L) on the second.

If this same function is applied to one arg, all we can say is that it
uses x with 1L, and its arg with demand 1P(L,L).

Note [Demand signatures semantically]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Demand analysis interprets expressions in the abstract domain of demand
transformers. Given a (sub-)demand that denotes the evaluation context, the
abstract transformer of an expression gives us back a demand type denoting
how other things (like arguments and free vars) were used when the expression
was evaluated. Here's an example:

  f x y =
    if x + expensive
      then \z -> z + y * ...
      else \z -> z * ...

The abstract transformer (let's call it F_e) of the if expression (let's
call it e) would transform an incoming (undersaturated!) head sub-demand A
into a demand type like {x-><1L>,y-><L>}<L>. In pictures:

     SubDemand ---F_e---> DmdType
     <A>                  {x-><1L>,y-><L>}<L>

Let's assume that the demand transformers we compute for an expression are
correct wrt. to some concrete semantics for Core. How do demand signatures fit
in? They are strange beasts, given that they come with strict rules when to
it's sound to unleash them.

Fortunately, we can formalise the rules with Galois connections. Consider
f's strictness signature, {}<1L><L>. It's a single-point approximation of
the actual abstract transformer of f's RHS for arity 2. So, what happens is that
we abstract *once more* from the abstract domain we already are in, replacing
the incoming Demand by a simple lattice with two elements denoting incoming
arity: A_2 = {<2, >=2} (where '<2' is the top element and >=2 the bottom
element). Here's the diagram:

     A_2 -----f_f----> DmdType
      ^                   |
      | α               γ |
      |                   v
  SubDemand --F_f----> DmdType

With
  α(C(1,C(1,_))) = >=2
  α(_)         =  <2
  γ(ty)        =  ty
and F_f being the abstract transformer of f's RHS and f_f being the abstracted
abstract transformer computable from our demand signature simply by

  f_f(>=2) = {}<1L><L>
  f_f(<2)  = multDmdType C_0N {}<1L><L>

where multDmdType makes a proper top element out of the given demand type.

In practice, the A_n domain is not just a simple Bool, but a Card, which is
exactly the Card with which we have to multDmdType. The Card for arity n
is computed by calling @peelManyCalls n@, which corresponds to α above.

Note [Understanding DmdType and DmdSig]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Demand types are sound approximations of an expression's semantics relative to
the incoming demand we put the expression under. Consider the following
expression:

    \x y -> x `seq` (y, 2*x)

Here is a table with demand types resulting from different incoming demands we
put that expression under. Note the monotonicity; a stronger incoming demand
yields a more precise demand type:

    incoming sub-demand   |  demand type
    --------------------------------
    P(A)                  |  <L><L>{}
    C(1,C(1,P(L)))        |  <1P(L)><L>{}
    C(1,C(1,1P(1P(L),A))) |  <1P(A)><A>{}

Note that in the first example, the depth of the demand type was *higher* than
the arity of the incoming call demand due to the anonymous lambda.
The converse is also possible and happens when we unleash demand signatures.
In @f x y@, the incoming call demand on f has arity 2. But if all we have is a
demand signature with depth 1 for @f@ (which we can safely unleash, see below),
the demand type of @f@ under a call demand of arity 2 has a *lower* depth of 1.

So: Demand types are elicited by putting an expression under an incoming (call)
demand, the arity of which can be lower or higher than the depth of the
resulting demand type.
In contrast, a demand signature summarises a function's semantics *without*
immediately specifying the incoming demand it was produced under. Despite StrSig
being a newtype wrapper around DmdType, it actually encodes two things:

  * The threshold (i.e., minimum arity) to unleash the signature
  * A demand type that is sound to unleash when the minimum arity requirement is
    met.

Here comes the subtle part: The threshold is encoded in the demand-sig arity!
So in mkDmdSigForArity we make sure to trim the list of argument demands to the
given threshold arity. Call sites will make sure that this corresponds to the
arity of the call demand that elicited the wrapped demand type. See also
Note [DmdSig: demand signatures, and demand-sig arity]
-}

-- | The depth of the wrapped 'DmdType' encodes the arity at which it is safe
-- to unleash. Better construct this through 'mkDmdSigForArity'.
-- See Note [Understanding DmdType and DmdSig]
newtype DmdSig
  = DmdSig DmdType
  deriving Eq

-- | Turns a 'DmdType' computed for the particular 'Arity' into a 'DmdSig'
-- unleashable at that arity. See Note [Understanding DmdType and DmdSig].
mkDmdSigForArity :: Arity -> DmdType -> DmdSig
mkDmdSigForArity threshold_arity dmd_ty@(DmdType fvs args)
  | threshold_arity < dmdTypeDepth dmd_ty
  = DmdSig $ DmdType (fvs { de_div = topDiv }) (take threshold_arity args)
  | otherwise
  = DmdSig (etaExpandDmdType threshold_arity dmd_ty)

mkClosedDmdSig :: [Demand] -> Divergence -> DmdSig
mkClosedDmdSig ds div = mkDmdSigForArity (length ds) (DmdType (mkEmptyDmdEnv div) ds)

mkVanillaDmdSig :: Arity -> Divergence -> DmdSig
mkVanillaDmdSig ar div = mkClosedDmdSig (replicate ar topDmd) div

splitDmdSig :: DmdSig -> ([Demand], Divergence)
splitDmdSig (DmdSig (DmdType env dmds)) = (dmds, de_div env)

dmdSigDmdEnv :: DmdSig -> DmdEnv
dmdSigDmdEnv (DmdSig (DmdType env _)) = env

hasDemandEnvSig :: DmdSig -> Bool
hasDemandEnvSig = not . isEmptyVarEnv . de_fvs . dmdSigDmdEnv

botSig :: DmdSig
botSig = DmdSig botDmdType

nopSig :: DmdSig
nopSig = DmdSig nopDmdType

isNopSig :: DmdSig -> Bool
isNopSig (DmdSig ty) = ty == nopDmdType

-- | True if the signature diverges or throws an exception in a saturated call.
-- See Note [Dead ends].
isDeadEndSig :: DmdSig -> Bool
isDeadEndSig (DmdSig (DmdType env _)) = isDeadEndDiv (de_div env)

-- | True if the signature diverges or throws an imprecise exception in a saturated call.
-- NB: In constrast to 'isDeadEndSig' this returns False for 'exnDiv'.
-- See Note [Dead ends]
-- and Note [Precise vs imprecise exceptions].
isBottomingSig :: DmdSig -> Bool
isBottomingSig (DmdSig (DmdType env _)) = de_div env == botDiv

-- | True when the signature indicates all arguments are boxed
onlyBoxedArguments :: DmdSig -> Bool
onlyBoxedArguments (DmdSig (DmdType _ dmds)) = all demandIsBoxed dmds
 where
   demandIsBoxed BotDmd    = True
   demandIsBoxed AbsDmd    = True
   demandIsBoxed (_ :* sd) = subDemandIsboxed sd

   subDemandIsboxed (Poly Unboxed _) = False
   subDemandIsboxed (Poly _ _)       = True
   subDemandIsboxed (Call _ sd)      = subDemandIsboxed sd
   subDemandIsboxed (Prod Unboxed _) = False
   subDemandIsboxed (Prod _ ds)      = all demandIsBoxed ds

-- | Returns true if an application to n value args would diverge or throw an
-- exception.
--
-- If a function having 'botDiv' is applied to a less number of arguments than
-- its syntactic arity, we cannot say for sure that it is going to diverge.
-- Hence this function conservatively returns False in that case.
-- See Note [Dead ends].
isDeadEndAppSig :: DmdSig -> Int -> Bool
isDeadEndAppSig (DmdSig (DmdType env ds)) n
  = isDeadEndDiv (de_div env) && not (lengthExceeds ds n)

trimBoxityDmdEnv :: DmdEnv -> DmdEnv
trimBoxityDmdEnv (DE fvs div) = DE (mapVarEnv trimBoxity fvs) div

trimBoxityDmdType :: DmdType -> DmdType
trimBoxityDmdType (DmdType env ds) =
  DmdType (trimBoxityDmdEnv env) (map trimBoxity ds)

trimBoxityDmdSig :: DmdSig -> DmdSig
trimBoxityDmdSig = coerce trimBoxityDmdType

-- | Transfers the boxity of the left arg to the demand structure of the right
-- arg. This only makes sense if applied to new and old demands of the same
-- value.
transferBoxity :: Demand -> Demand -> Demand
transferBoxity from to = go_dmd from to
  where
    go_dmd (from_n :* from_sd) to_dmd@(to_n :* to_sd)
      | isAbs from_n || isAbs to_n = to_dmd
      | otherwise = case (from_sd, to_sd) of
          (Poly from_b _, Poly _ to_c) ->
            to_n :* Poly from_b to_c
          (_, Prod _ to_ds)
            | Just (from_b, from_ds) <- viewProd (length to_ds) from_sd
            -> to_n :* mkProd from_b (strictZipWith go_dmd from_ds to_ds)
          (Prod from_b from_ds, _)
            | Just (_, to_ds) <- viewProd (length from_ds) to_sd
            -> to_n :* mkProd from_b (strictZipWith go_dmd from_ds to_ds)
          _ -> trimBoxity to_dmd

transferArgBoxityDmdType :: DmdType -> DmdType -> DmdType
transferArgBoxityDmdType _from@(DmdType _ from_ds) to@(DmdType to_env to_ds)
  | equalLength from_ds to_ds
  = -- pprTraceWith "transfer" (\r -> ppr _from $$ ppr to $$ ppr r) $
    DmdType to_env -- Only arg boxity! See Note [Don't change boxity without worker/wrapper]
            (zipWith transferBoxity from_ds to_ds)
  | otherwise
  = trimBoxityDmdType to

transferArgBoxityDmdSig :: DmdSig -> DmdSig -> DmdSig
transferArgBoxityDmdSig = coerce transferArgBoxityDmdType

prependArgsDmdSig :: Int -> DmdSig -> DmdSig
-- ^ Add extra ('topDmd') arguments to a strictness signature.
-- In contrast to 'etaConvertDmdSig', this /prepends/ additional argument
-- demands. This is used by FloatOut.
prependArgsDmdSig new_args sig@(DmdSig dmd_ty@(DmdType env dmds))
  | new_args == 0        = sig
  | dmd_ty == nopDmdType = sig
  | otherwise            = DmdSig (DmdType env dmds')
  where
    dmds' = assertPpr (new_args > 0) (ppr new_args) $
            replicate new_args topDmd ++ dmds

etaConvertDmdSig :: Arity -> DmdSig -> DmdSig
-- ^ We are expanding (\x y. e) to (\x y z. e z) or reducing from the latter to
-- the former (when the Simplifier identifies a new join points, for example).
-- In contrast to 'prependArgsDmdSig', this /appends/ extra arg demands if
-- necessary.
-- This works by looking at the 'DmdType' (which was produced under a call
-- demand for the old arity) and trying to transfer as many facts as we can to
-- the call demand of new arity.
-- An arity increase (resulting in a stronger incoming demand) can retain much
-- of the info, while an arity decrease (a weakening of the incoming demand)
-- must fall back to a conservative default.
etaConvertDmdSig arity (DmdSig dmd_ty)
  | arity < dmdTypeDepth dmd_ty = DmdSig $ decreaseArityDmdType dmd_ty
  | otherwise                   = DmdSig $ etaExpandDmdType arity dmd_ty

{-
************************************************************************
*                                                                      *
                     Demand transformers
*                                                                      *
************************************************************************
-}

-- | A /demand transformer/ is a monotone function from an incoming evaluation
-- context ('SubDemand') to a 'DmdType', describing how the denoted thing
-- (i.e. expression, function) uses its arguments and free variables, and
-- whether it diverges.
--
-- See Note [Understanding DmdType and DmdSig]
-- and Note [DmdSig: demand signatures, and demand-sig arity]
type DmdTransformer = SubDemand -> DmdType

-- | Extrapolate a demand signature ('DmdSig') into a 'DmdTransformer'.
--
-- Given a function's 'DmdSig' and a 'SubDemand' for the evaluation context,
-- return how the function evaluates its free variables and arguments.
dmdTransformSig :: DmdSig -> DmdTransformer
dmdTransformSig (DmdSig dmd_ty@(DmdType _ arg_ds)) sd
  = multDmdType (fst $ peelManyCalls (length arg_ds) sd) dmd_ty
    -- see Note [Demands from unsaturated function calls]
    -- and Note [DmdSig: demand signatures, and demand-sig arity]

-- | A special 'DmdTransformer' for data constructors that feeds product
-- demands into the constructor arguments.
dmdTransformDataConSig :: [StrictnessMark] -> DmdTransformer
-- See Note [Demand transformer for data constructors]
dmdTransformDataConSig str_marks sd = case viewProd arity body_sd of
  Just (_, dmds) -> mk_body_ty n dmds
  Nothing        -> nopDmdType
  where
    arity = length str_marks
    (n, body_sd) = peelManyCalls arity sd
    mk_body_ty n dmds = DmdType nopDmdEnv (zipWith (bump n) str_marks dmds)
    bump n str dmd | isMarkedStrict str = multDmd n (plusDmd str_field_dmd dmd)
                   | otherwise          = multDmd n dmd
    str_field_dmd = seqDmd -- See the bit about strict fields
                           -- in Note [Demand transformer for data constructors]

-- | A special 'DmdTransformer' for dictionary selectors that feeds the demand
-- on the result into the indicated dictionary component (if saturated).
-- See Note [Demand transformer for a dictionary selector].
dmdTransformDictSelSig :: DmdSig -> DmdTransformer
-- NB: This currently doesn't handle newtype dictionaries.
-- It should simply apply call_sd directly to the dictionary, I suppose.
dmdTransformDictSelSig (DmdSig (DmdType _ [_ :* prod])) call_sd
   | (n, sd') <- peelCallDmd call_sd
   , Prod _ sig_ds <- prod
   = multDmdType n $
     DmdType nopDmdEnv [C_11 :* mkProd Unboxed (map (enhance sd') sig_ds)]
   | otherwise
   = nopDmdType -- See Note [Demand transformer for a dictionary selector]
  where
    enhance _  AbsDmd   = AbsDmd
    enhance _  BotDmd   = BotDmd
    enhance sd _dmd_var = C_11 :* sd  -- This is the one!
                                      -- C_11, because we multiply with n above
dmdTransformDictSelSig sig sd = pprPanic "dmdTransformDictSelSig: no args" (ppr sig $$ ppr sd)

{-
Note [Demand transformer for a dictionary selector]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose we have a superclass selector 'sc_sel' and a class method
selector 'op_sel', and a function that uses both, like this

-- Strictness sig: 1P(1,A)
sc_sel (x,y) = x

-- Strictness sig: 1P(A,1)
op_sel (p,q)= q

f d v = op_sel (sc_sel d) v

What do we learn about the demand on 'd'?  Alas, we see only the
demand from 'sc_sel', namely '1P(1,A)'.  We /don't/ see that 'd' really has a nested
demand '1P(1P(A,1C(1,1)),A)'.  On the other hand, if we inlined the two selectors
we'd have

f d x = case d of (x,_) ->
        case x of (_,q) ->
        q v

If we analyse that, we'll get a richer, nested demand on 'd'.

We want to behave /as if/ we'd inlined 'op_sel' and 'sc_sel'. We can do this
easily by building a richer demand transformer for dictionary selectors than
is expressible by a regular demand signature.
And that is what 'dmdTransformDictSelSig' does: it transforms the demand on the
result to a demand on the (single) argument.

How does it do that?
If we evaluate (op dict-expr) under demand 'd', then we can push the demand 'd'
into the appropriate field of the dictionary. What *is* the appropriate field?
We just look at the strictness signature of the class op, which will be
something like: P(AAA1AAAAA). Then replace the '1' (or any other non-absent
demand, really) by the demand 'd'. The '1' acts as if it was a demand variable,
the whole signature really means `\d. P(AAAdAAAAA)` for any incoming
demand 'd'.

For single-method classes, which are represented by newtypes the signature
of 'op' won't look like P(...), so matching on Prod will fail.
That's fine: if we are doing strictness analysis we are also doing inlining,
so we'll have inlined 'op' into a cast.  So we can bale out in a conservative
way, returning nopDmdType. SG: Although we then probably want to apply the eval
demand 'd' directly to 'op' rather than turning it into 'topSubDmd'...

It is (just.. #8329) possible to be running strictness analysis *without*
having inlined class ops from single-method classes.  Suppose you are using
ghc --make; and the first module has a local -O0 flag.  So you may load a class
without interface pragmas, ie (currently) without an unfolding for the class
ops.   Now if a subsequent module in the --make sweep has a local -O flag
you might do strictness analysis, but there is no inlining for the class op.
This is weird, so I'm not worried about whether this optimises brilliantly; but
it should not fall over.
-}

zapDmdEnv :: DmdEnv -> DmdEnv
zapDmdEnv (DE _ div) = mkEmptyDmdEnv div

-- | Remove the demand environment from the signature.
zapDmdEnvSig :: DmdSig -> DmdSig
zapDmdEnvSig (DmdSig (DmdType env ds)) = DmdSig (DmdType (zapDmdEnv env) ds)

zapUsageDemand :: Demand -> Demand
-- Remove the usage info, but not the strictness info, from the demand
zapUsageDemand = kill_usage $ KillFlags
    { kf_abs         = True
    , kf_used_once   = True
    , kf_called_once = True
    }

-- | Remove all `C_01 :*` info (but not `CM` sub-demands) from the demand
zapUsedOnceDemand :: Demand -> Demand
zapUsedOnceDemand = kill_usage $ KillFlags
    { kf_abs         = False
    , kf_used_once   = True
    , kf_called_once = False
    }

-- | Remove all `C_01 :*` info (but not `CM` sub-demands) from the strictness
--   signature
zapUsedOnceSig :: DmdSig -> DmdSig
zapUsedOnceSig (DmdSig (DmdType env ds))
    = DmdSig (DmdType env (map zapUsedOnceDemand ds))

data KillFlags = KillFlags
    { kf_abs         :: Bool
    , kf_used_once   :: Bool
    , kf_called_once :: Bool
    }

kill_usage_card :: KillFlags -> Card -> Card
kill_usage_card kfs C_00 | kf_abs kfs       = C_0N
kill_usage_card kfs C_10 | kf_abs kfs       = C_1N
kill_usage_card kfs C_01 | kf_used_once kfs = C_0N
kill_usage_card kfs C_11 | kf_used_once kfs = C_1N
kill_usage_card _   n                       = n

kill_usage :: KillFlags -> Demand -> Demand
kill_usage _   AbsDmd    = AbsDmd
kill_usage _   BotDmd    = BotDmd
kill_usage kfs (n :* sd) = kill_usage_card kfs n :* kill_usage_sd kfs sd

kill_usage_sd :: KillFlags -> SubDemand -> SubDemand
kill_usage_sd kfs (Call n sd)
  | kf_called_once kfs        = mkCall (lubCard C_1N n) (kill_usage_sd kfs sd)
  | otherwise                 = mkCall n                (kill_usage_sd kfs sd)
kill_usage_sd kfs (Prod b ds) = mkProd b (map (kill_usage kfs) ds)
kill_usage_sd _   sd          = sd

{- *********************************************************************
*                                                                      *
               TypeShape and demand trimming
*                                                                      *
********************************************************************* -}


data TypeShape -- See Note [Trimming a demand to a type]
               --     in GHC.Core.Opt.DmdAnal
  = TsFun TypeShape
  | TsProd [TypeShape]
  | TsUnk

trimToType :: Demand -> TypeShape -> Demand
-- See Note [Trimming a demand to a type] in GHC.Core.Opt.DmdAnal
trimToType AbsDmd    _  = AbsDmd
trimToType BotDmd    _  = BotDmd
trimToType (n :* sd) ts
  = n :* go sd ts
  where
    go (Prod b ds) (TsProd tss)
      | equalLength ds tss    = mkProd b (zipWith trimToType ds tss)
    go (Call n sd) (TsFun ts) = mkCall n (go sd ts)
    go sd@Poly{}   _          = sd
    go _           _          = topSubDmd

-- | Drop all boxity
trimBoxity :: Demand -> Demand
trimBoxity AbsDmd    = AbsDmd
trimBoxity BotDmd    = BotDmd
trimBoxity (n :* sd) = n :* go sd
  where
    go (Poly _ n)  = Poly Boxed n
    go (Prod _ ds) = mkProd Boxed (map trimBoxity ds)
    go (Call n sd) = mkCall n $ go sd

{-
************************************************************************
*                                                                      *
                     'seq'ing demands
*                                                                      *
************************************************************************
-}

seqDemand :: Demand -> ()
seqDemand AbsDmd    = ()
seqDemand BotDmd    = ()
seqDemand (_ :* sd) = seqSubDemand sd

seqSubDemand :: SubDemand -> ()
seqSubDemand (Prod _ ds) = seqDemandList ds
seqSubDemand (Call _ sd) = seqSubDemand sd
seqSubDemand (Poly _ _)  = ()

seqDemandList :: [Demand] -> ()
seqDemandList = foldr (seq . seqDemand) ()

seqDmdType :: DmdType -> ()
seqDmdType (DmdType env ds) =
  seqDmdEnv env `seq` seqDemandList ds `seq` ()

seqDmdEnv :: DmdEnv -> ()
seqDmdEnv (DE fvs _) = seqEltsUFM seqDemand fvs

seqDmdSig :: DmdSig -> ()
seqDmdSig (DmdSig ty) = seqDmdType ty

{-
************************************************************************
*                                                                      *
                     Outputable and Binary instances
*                                                                      *
************************************************************************
-}

-- Just for debugging purposes.
instance Show Card where
  show C_00 = "C_00"
  show C_01 = "C_01"
  show C_0N = "C_0N"
  show C_10 = "C_10"
  show C_11 = "C_11"
  show C_1N = "C_1N"

{- Note [Demand notation]
~~~~~~~~~~~~~~~~~~~~~~~~~
This Note should be kept up to date with the documentation of `-fstrictness`
in the user's guide.

For pretty-printing demands, we use quite a compact notation with some
abbreviations. Here's the BNF:

  card ::= B                        {}
        |  A                        {0}
        |  M                        {0,1}
        |  L                        {0,1,n}
        |  1                        {1}
        |  S                        {1,n}

  box  ::= !                        Unboxed
        |  <empty>                  Boxed

  d    ::= card sd                  The :* constructor, just juxtaposition
        |  card                     abbreviation: Same as "card card"

  sd   ::= box card                 @Poly box card@
        |  box P(d,d,..)            @Prod box [d1,d2,..]@
        |  Ccard(sd)                @Call card sd@

So, L can denote a 'Card', polymorphic 'SubDemand' or polymorphic 'Demand',
but it's always clear from context which "overload" is meant. It's like
return-type inference of e.g. 'read'.

An example of the demand syntax is 1!P(1!L,A), the demand of fst's argument.
See Note [Demand examples] for more examples and their semantics.

This is the syntax for demand signatures:

  div ::= <empty>      topDiv
       |  x            exnDiv
       |  b            botDiv

  sig ::= {x->dx,y->dy,z->dz...}<d1><d2><d3>...<dn>div
                  ^              ^   ^   ^      ^   ^
                  |              |   |   |      |   |
                  |              \---+---+------/   |
                  |                  |              |
             demand on free        demand on      divergence
               variables           arguments      information
           (omitted if empty)                     (omitted if
                                                no information)

Note [Demand examples]
~~~~~~~~~~~~~~~~~~~~~~
Here are some examples of the demand notation, specified in Note [Demand notation],
in action. In each case we give the demand on the variable `x`.

Demand on x    Example            Explanation
  1!A           seq x y             Evaluates `x` exactly once (`1`), but not
                                    any deeper (`A`), and discards the box (`!`).
  S!A           seq x (seq x y)     Twice the previous demand; hence eval'd
                                    more than once (`S` for strict).
  1!P(1!L,A)    fst x               Evaluates pair `x` exactly once, first
                                    component exactly once. No info that (`L`).
                                    Second component is absent. Discards boxes (`!`).
  1P(1L,A)      opq_fst x           Like fst, but all boxes are retained.
  SP(1!L,A)     opq_seq x (fst x)   Two evals of x but exactly one of its first component.
                                    Box of x retained, but box of first component discarded.
  1!C(1,L)      x $ 3               Evals x exactly once ( 1 ) and calls it
                                    exactly once ( C(1,_) ). No info on how the
                                    result is evaluated ( L ).
  MC(M,L)       maybe y x           Evals x at most once ( 1 ) and calls it at
                                    most once ( C(1,_) ). No info on how the
                                    result is evaluated ( L ).
  LP(SL,A)      map (+ fst x)       Evals x lazily and multiple times ( L ),
                                    but when it is evaluated, the first
                                    component is evaluated (strictly) as well.

In the examples above, `opq_fst` is an opaque wrapper around `fst`, i.e.

  opq_fst = fst
  {-# OPAQUE opq_fst #-}

Similarly for `seq`. The effect of an OPAQUE pragma is that it discards any
boxity flags in the demand signature, as described in Note [OPAQUE pragma].
-}

-- | See Note [Demand notation]
-- Current syntax was discussed in #19016.
instance Outputable Card where
  ppr C_00 = char 'A' -- "Absent"
  ppr C_01 = char 'M' -- "Maybe"
  ppr C_0N = char 'L' -- "Lazy"
  ppr C_11 = char '1' -- "exactly 1"
  ppr C_1N = char 'S' -- "Strict"
  ppr C_10 = char 'B' -- "Bottom"

-- | See Note [Demand notation]
instance Outputable Demand where
  ppr AbsDmd                    = char 'A'
  ppr BotDmd                    = char 'B'
  ppr (C_0N :* Poly Boxed C_0N) = char 'L' -- Print LL as just L
  ppr (C_1N :* Poly Boxed C_1N) = char 'S' -- Dito SS
  ppr (n :* sd)                 = ppr n <> ppr sd

-- | See Note [Demand notation]
instance Outputable SubDemand where
  ppr (Poly b n)  = pp_boxity b <> ppr n
  ppr (Call n sd) = char 'C' <> parens (ppr n <> comma <> ppr sd)
  ppr (Prod b ds) = pp_boxity b <> char 'P' <> parens (fields ds)
    where
      fields []     = empty
      fields [x]    = ppr x
      fields (x:xs) = ppr x <> char ',' <> fields xs

pp_boxity :: Boxity -> SDoc
pp_boxity Unboxed = char '!'
pp_boxity _       = empty

instance Outputable Divergence where
  ppr Diverges = char 'b' -- for (b)ottom
  ppr ExnOrDiv = char 'x' -- for e(x)ception
  ppr Dunno    = empty

instance Outputable DmdEnv where
  ppr (DE fvs div)
    = ppr div <> if null fv_elts then empty
                 else braces (fsep (map pp_elt fv_elts))
    where
      pp_elt (uniq, dmd) = ppr uniq <> text "->" <> ppr dmd
      fv_elts = nonDetUFMToList fvs
        -- It's OK to use nonDetUFMToList here because we only do it for
        -- pretty printing

instance Outputable DmdType where
  ppr (DmdType fv ds)
    = hcat (map (angleBrackets . ppr) ds) <> ppr fv

instance Outputable DmdSig where
   ppr (DmdSig ty) = ppr ty

instance Outputable TypeShape where
  ppr TsUnk        = text "TsUnk"
  ppr (TsFun ts)   = text "TsFun" <> parens (ppr ts)
  ppr (TsProd tss) = parens (hsep $ punctuate comma $ map ppr tss)

instance Binary Card where
  put_ bh C_00 = putByte bh 0
  put_ bh C_01 = putByte bh 1
  put_ bh C_0N = putByte bh 2
  put_ bh C_11 = putByte bh 3
  put_ bh C_1N = putByte bh 4
  put_ bh C_10 = putByte bh 5
  get bh = do
    h <- getByte bh
    case h of
      0 -> return C_00
      1 -> return C_01
      2 -> return C_0N
      3 -> return C_11
      4 -> return C_1N
      5 -> return C_10
      _ -> pprPanic "Binary:Card" (ppr (fromIntegral h :: Int))

instance Binary Demand where
  put_ bh (n :* sd) = put_ bh n *> case n of
    C_00 -> return ()
    C_10 -> return ()
    _    -> put_ bh sd
  get bh = get bh >>= \n -> case n of
    C_00 -> return AbsDmd
    C_10 -> return BotDmd
    _    -> (n :*) <$> get bh

instance Binary SubDemand where
  put_ bh (Poly b sd) = putByte bh 0 *> put_ bh b *> put_ bh sd
  put_ bh (Call n sd) = putByte bh 1 *> put_ bh n *> put_ bh sd
  put_ bh (Prod b ds) = putByte bh 2 *> put_ bh b *> put_ bh ds
  get bh = do
    h <- getByte bh
    case h of
      0 -> Poly <$> get bh <*> get bh
      1 -> mkCall <$> get bh <*> get bh
      2 -> Prod <$> get bh <*> get bh
      _ -> pprPanic "Binary:SubDemand" (ppr (fromIntegral h :: Int))

instance Binary Divergence where
  put_ bh Dunno    = putByte bh 0
  put_ bh ExnOrDiv = putByte bh 1
  put_ bh Diverges = putByte bh 2
  get bh = do
    h <- getByte bh
    case h of
      0 -> return Dunno
      1 -> return ExnOrDiv
      2 -> return Diverges
      _ -> pprPanic "Binary:Divergence" (ppr (fromIntegral h :: Int))

instance Binary DmdEnv where
  -- Ignore VarEnv when spitting out the DmdType
  put_ bh (DE _ d) = put_ bh d
  get bh = DE emptyVarEnv <$> get bh

instance Binary DmdType where
  put_ bh (DmdType fv ds) = put_ bh fv *> put_ bh ds
  get bh = DmdType <$> get bh <*> get bh

instance Binary DmdSig where
  put_ bh (DmdSig aa) = put_ bh aa
  get bh = DmdSig <$> get bh
