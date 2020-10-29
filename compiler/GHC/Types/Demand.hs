{-# LANGUAGE CPP                  #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE ViewPatterns         #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

\section[Demand]{@Demand@: A decoupled implementation of a demand domain}
-}

module GHC.Types.Demand (
        Card(..), Demand(..), CleanDemand(Prod), viewProd,
        oneifyDmd, oneifyCard,
        absDmd, topDmd, botDmd, seqDmd,
        lubCard, lubDmd, lubCleanDmd,
        plusCard, plusDmd, plusCleanDmd,
        multCard, multDmd, multCleanDmd,
        lazyApply1Dmd, lazyApply2Dmd, strictApply1Dmd,
        isAbs, isUsedOnce, isStrict, isAbsDmd, isUsedOnceDmd, isStrictDmd,
        isTopDmd, isSeqDmd,
        strictenDmd,
        addCaseBndrDmd,

        DmdType(..), dmdTypeDepth, lubDmdType, plusDmdType,
        BothDmdArg, mkBothDmdArg, toBothDmdArg,
        nopDmdType, botDmdType, addDemand,

        DmdEnv, emptyDmdEnv, keepAliveDmdEnv,
        peelFV, findIdDemand,

        Divergence(..), lubDivergence, isDeadEndDiv,
        topDiv, botDiv, exnDiv,
        appIsDeadEnd, isDeadEndSig, pprIfaceStrictSig,
        StrictSig(..), mkStrictSigForArity, mkClosedStrictSig,
        nopSig, botSig,
        isTopSig, hasDemandEnvSig,
        splitStrictSig, strictSigDmdEnv,
        prependArgsStrictSig, etaConvertStrictSig,

        seqDemand, seqDemandList, seqDmdType, seqStrictSig,

        evalDmd, cleanEvalDmd, cleanEvalProdDmd,
        splitDmdTy, isWeakDmd, deferAfterPreciseException,
        multUnsat, multDmdType,

        peelCallDmd, peelManyCalls, mkCallDmd, mkCallDmds,
        mkWorkerDemand, dmdTransformSig, dmdTransformDataConSig,
        dmdTransformDictSelSig, argOneShots, argsOneShots, saturatedByOneShots,
        TypeShape(..), trimToType,

        reuseEnv,
        zapUsageDemand, zapUsageEnvSig,
        zapUsedOnceDemand, zapUsedOnceSig,
        strictifyDictDmd, strictifyDmd

     ) where

#include "HsVersions.h"

import GHC.Prelude

import GHC.Types.Var ( Var, Id )
import GHC.Types.Var.Env
import GHC.Types.Var.Set
import GHC.Types.Unique.FM
import GHC.Types.Basic
import GHC.Data.Maybe   ( orElse )

import GHC.Core.Type    ( Type )
import GHC.Core.TyCon   ( isNewTyCon, isClassTyCon )
import GHC.Core.DataCon ( splitDataProductType_maybe )
import GHC.Core.Multiplicity    ( scaledThing )

import GHC.Utils.Binary
import GHC.Utils.Misc
import GHC.Utils.Outputable
import GHC.Utils.Panic

{-
************************************************************************
*                                                                      *
        Joint domain for Strictness and Absence
*                                                                      *
************************************************************************
-}

{-
************************************************************************
*                                                                      *
            Strictness domain
*                                                                      *
************************************************************************

          Lazy
           |
        HeadStr
        /     \
    SCall      SProd
        \     /
        HyperStr

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
(beyond `-fno-pendantic-bottoms`).

But then #13380 and #17676 suggest (in Mar 20) that we need to re-introduce a
subtly different variant of `ThrowsExn` (which we call `ExnOrDiv` now) that is
only used by `raiseIO#` in order to preserve precise exceptions by strictness
analysis, while not impacting the ability to eliminate dead code.
See Note [Precise exceptions and strictness analysis].

-}

{-
************************************************************************
*                                                                      *
            Absence domain
*                                                                      *
************************************************************************

         Used
         /   \
     UCall   UProd
         \   /
         UHead
          |
  Count x -
        |
       Abs
-}

addCaseBndrDmd :: Demand    -- On the case binder
               -> [Demand]  -- On the components of the constructor
               -> [Demand]  -- Final demands for the components of the constructor
-- See Note [Demand on case-alternative binders]
addCaseBndrDmd (n :* cd) alt_dmds
  | isAbs n   = alt_dmds
  | otherwise = zipWith plusDmd alt_dmds ds
  where
    Just ds = viewProd (length alt_dmds) cd -- Guaranteed not to be a call

{- Note [Demand on case-alternative binders]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The demand on a binder in a case alternative comes
  (a) From the demand on the binder itself
  (b) From the demand on the case binder
Forgetting (b) led directly to #10148.

Example. Source code:
  f x@(p,_) = if p then foo x else True

  foo (p,True) = True
  foo (p,q)    = foo (q,p)

After strictness analysis:
  f = \ (x_an1 [Dmd=<S(SL),1*U(U,1*U)>] :: (Bool, Bool)) ->
      case x_an1
      of wild_X7 [Dmd=<L,1*U(1*U,1*U)>]
      { (p_an2 [Dmd=<S,1*U>], ds_dnz [Dmd=<L,A>]) ->
      case p_an2 of _ {
        False -> GHC.Types.True;
        True -> foo wild_X7 }

It's true that ds_dnz is *itself* absent, but the use of wild_X7 means
that it is very much alive and demanded.  See #10148 for how the
consequences play out.

This is needed even for non-product types, in case the case-binder
is used but the components of the case alternative are not.

Note [Don't optimise UProd(Used) to Used]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
These two UseDmds:
   UProd [Used, Used]   and    Used
are semantically equivalent, but we do not turn the former into
the latter, for a regrettable-subtle reason.  Suppose we did.
then
  f (x,y) = (y,x)
would get
  StrDmd = Str  = SProd [Lazy, Lazy]
  UseDmd = Used = UProd [Used, Used]
But with the joint demand of <Str, Used> doesn't convey any clue
that there is a product involved, and so the worthSplittingFun
will not fire.  (We'd need to use the type as well to make it fire.)
Moreover, consider
  g h p@(_,_) = h p
This too would get <Str, Used>, but this time there really isn't any
point in w/w since the components of the pair are not used at all.

So the solution is: don't aggressively collapse UProd [Used,Used] to
Used; instead leave it as-is. In effect we are using the UseDmd to do a
little bit of boxity analysis.  Not very nice.

Note [Used should win]
~~~~~~~~~~~~~~~~~~~~~~
Both in lubUse and plusUse we want (Used `plus` UProd us) to be Used.
Why?  Because Used carries the implication the whole thing is used,
box and all, so we don't want to w/w it.  If we use it both boxed and
unboxed, then we are definitely using the box, and so we are quite
likely to pay a reboxing cost.  So we make Used win here.

Example is in the Buffer argument of GHC.IO.Handle.Internals.writeCharBuffer

Baseline: (A) Not making Used win (UProd wins)
Compare with: (B) making Used win for lub and both

            Min          -0.3%     -5.6%    -10.7%    -11.0%    -33.3%
            Max          +0.3%    +45.6%    +11.5%    +11.5%     +6.9%
 Geometric Mean          -0.0%     +0.5%     +0.3%     +0.2%     -0.8%

Baseline: (B) Making Used win for both lub and both
Compare with: (C) making Used win for plus, but UProd win for lub

            Min          -0.1%     -0.3%     -7.9%     -8.0%     -6.5%
            Max          +0.1%     +1.0%    +21.0%    +21.0%     +0.5%
 Geometric Mean          +0.0%     +0.0%     -0.0%     -0.1%     -0.1%
-}

{-
************************************************************************
*                                                                      *
         Clean demand for Strictness and Usage
*                                                                      *
************************************************************************

This domain differst from JointDemand in the sense that pure absence
is taken away, i.e., we deal *only* with non-absent demands.

Note [Strict demands]
~~~~~~~~~~~~~~~~~~~~~
isStrictDmd returns true only of demands that are
   both strict
   and  used
In particular, it is False for <HyperStr, Abs>, which can and does
arise in, say (#7319)
   f x = raise# <some exception>
Then 'x' is not used, so f gets strictness <HyperStr,Abs> -> .
Now the w/w generates
   fx = let x <HyperStr,Abs> = absentError "unused"
        in raise <some exception>
At this point we really don't want to convert to
   fx = case absentError "unused" of x -> raise <some exception>
Since the program is going to diverge, this swaps one error for another,
but it's really a bad idea to *ever* evaluate an absent argument.
In #7319 we get
   T7319.exe: Oops!  Entered absent arg w_s1Hd{v} [lid] [base:GHC.Base.String{tc 36u}]

Note [Dealing with call demands]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Call demands are constructed and deconstructed coherently for
strictness and absence. For instance, the strictness signature for the
following function

f :: (Int -> (Int, Int)) -> (Int, Bool)
f g = (snd (g 3), True)

should be: <L,C(U(AU))>m
-}

-- | Evaluated strictly, and used arbitrarily deeply
evalDmd :: Demand
evalDmd = C_1N :* cleanEvalDmd

-- | Wraps the 'CleanDemand' with a one-shot call demand: @d@ -> @C1(d)@.
mkCallDmd :: CleanDemand -> CleanDemand
mkCallDmd cd = Call C_11 cd

-- | @mkCallDmds n d@ returns @C1(C1...(C1 d))@ where there are @n@ @C1@'s.
mkCallDmds :: Arity -> CleanDemand -> CleanDemand
mkCallDmds arity cd = iterate mkCallDmd cd !! arity

-- See Note [Demand on the worker] in GHC.Core.Opt.WorkWrap
mkWorkerDemand :: Int -> Demand
mkWorkerDemand n = C_01 :* go n
  where go 0 = topCleanDmd
        go n = Call C_01 $ go (n-1)

cleanEvalDmd :: CleanDemand
cleanEvalDmd = topCleanDmd

cleanEvalProdDmd :: Arity -> CleanDemand
cleanEvalProdDmd n = Prod (replicate n topDmd)

{-
************************************************************************
*                                                                      *
           Demand: Combining Strictness and Usage
*                                                                      *
************************************************************************
-}

data Card
  = C_00 -- ^ {0}
  | C_01 -- ^ {0,1}
  | C_0N -- ^ {0,1,n} Every possible cardinality; the top element.
  | C_11 -- ^ {1,1}
  | C_1N -- ^ {1,n}   TODO: Think about whether this cardinality is of any
         --                 practical relevance. If we are strict, we can
         --                 assume that it is used at most once because of
         --                 call-by-value. Ah yes, it's relevant for call
         --                 demands.
  | C_10 -- ^ {}      The empty interval; the bottom element of the powerset lattice.
  deriving ( Eq )

instance Show Card where
  show C_00 = "A"
  show C_01 = "1"
  show C_0N = "U"
  show C_11 = "S"
  show C_1N = "S"
  show C_10 = "B"

_botCard, topCard :: Card
_botCard = C_10
topCard = C_0N

-- | True <=> lower bound is 1.
isStrict :: Card -> Bool
isStrict C_10 = True
isStrict C_11 = True
isStrict C_1N = True
isStrict _    = False

-- | True <=> upper bound is 0.
isAbs :: Card -> Bool
isAbs C_00 = True
isAbs C_10 = True -- Bottom cardinality is also absent
isAbs _    = False

-- | True <=> upper bound is 1.
isUsedOnce :: Card -> Bool
isUsedOnce C_0N = False
isUsedOnce C_1N = False
isUsedOnce _    = True

-- | Intersect with [0,1].
oneifyCard :: Card -> Card
oneifyCard C_0N = C_01
oneifyCard C_1N = C_11
oneifyCard c    = c

-- | Denotes '∪' on 'Card'.
lubCard :: Card -> Card -> Card
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
lubCard _    C_1N = C_0N -- {0} ∪ {1,n} = top
lubCard C_1N _    = C_0N -- {0} ∪ {1,n} = top
-- Handle C_01
lubCard C_01 _    = C_01 -- {0} ∪ {0,1} = {0,1}
lubCard _    C_01 = C_01 -- {0} ∪ {0,1} = {0,1}
-- Handle C_00
lubCard C_00 C_00 = C_00 -- reflexivity

-- | Denotes '+' on 'Card'.
plusCard :: Card -> Card -> Card
-- Handle C_00
plusCard C_00 n    = n    -- {0}+n = n
plusCard n    C_00 = n    -- {0}+n = n
-- Handle C_10
plusCard C_10 C_01 = C_11 -- These follow by applying + to lower and upper
plusCard C_10 C_0N = C_1N -- bounds individually
plusCard C_10 n    = n
plusCard C_01 C_10 = C_11
plusCard C_0N C_10 = C_1N
plusCard n    C_10 = n
-- Handle the rest (C_01, C_0N, C_11, C_1N)
plusCard C_01 C_01 = C_0N -- The upper bound is at least 1, so upper bound of
plusCard C_01 C_0N = C_0N -- the result must be 1+1 ~= N.
plusCard C_0N C_01 = C_0N -- But for the lower bound we have 4 cases where
plusCard C_0N C_0N = C_0N -- 0+0 ~= 0 (as opposed to 1), so we match on these.
plusCard _    _    = C_1N -- Otherwise we return topCard

-- | Denotes '*' on 'Card'.
multCard :: Card -> Card -> Card
-- Handle C_11 (neutral element)
multCard C_11 c    = c
multCard c    C_11 = c
-- Handle C_00 (annihilating element)
multCard C_00 _    = C_00
multCard _    C_00 = C_00
-- Handle C_10
multCard C_10 c    = if isStrict c then C_10 else C_00
multCard c    C_10 = if isStrict c then C_10 else C_00
-- Handle reflexive C_1N, C_01
multCard C_1N C_1N = C_1N
multCard C_01 C_01 = C_01
-- Handle C_0N and the rest (C_01, C_1N):
multCard _    _    = C_0N

-- It's similar to @'Scaled' 'CleanDemand'@, but it's scaled by 'Card', which
-- is an interval on 'Multiplicity'.
data Demand = !Card :* !CleanDemand
  deriving ( Eq, Show )

data CleanDemand
  = Poly !Card     -- ^ Polymorphic head demand with nested evaluation
                   -- cardinalities.

  | Call !Card !CleanDemand -- ^ Call demand
                          -- Used only for values of function type

  | Prod ![Demand]       -- ^ Product
                        -- Used only for values of product type
                        -- Invariant: not all components are HyperDmd (use HyperDmd)
                        --            not all components are Lazy     (use HeadStr)

  deriving ( Eq, Show )

poly00, poly01, poly0N, poly11, poly1N, poly10 :: CleanDemand
topCleanDmd, _botCleanDmd, seqCleanDmd :: CleanDemand
poly00 = Poly C_00
poly01 = Poly C_01
poly0N = Poly C_0N
poly11 = Poly C_11
poly1N = Poly C_1N
poly10 = Poly C_10
topCleanDmd = poly0N
_botCleanDmd = poly10
seqCleanDmd = poly00

polyDmd :: Card -> Demand
polyDmd C_00 = C_00 :* poly00
polyDmd C_01 = C_01 :* poly01
polyDmd C_0N = C_0N :* poly0N
polyDmd C_11 = C_11 :* poly11
polyDmd C_1N = C_1N :* poly1N
polyDmd C_10 = C_10 :* poly10

topDmd, absDmd, botDmd, seqDmd :: Demand
strictApply1Dmd, lazyApply1Dmd, lazyApply2Dmd :: Demand
topDmd = polyDmd C_0N
absDmd = polyDmd C_00
botDmd = polyDmd C_10
seqDmd = C_11 :* seqCleanDmd
strictApply1Dmd = C_1N :* Call C_1N topCleanDmd
lazyApply1Dmd   = C_01 :* Call C_01 topCleanDmd
-- | Second argument of catch#:
--    uses its arg at most once, applies it once
--    but is lazy (might not be called at all)
lazyApply2Dmd = C_01 :* Call C_01 (Call C_01 topCleanDmd)

viewProd :: Arity -> CleanDemand -> Maybe [Demand]
viewProd n (Prod ds)     | ds `lengthIs` n = Just ds
viewProd n (Poly card)                     = Just (replicate n (polyDmd card))
viewProd _ _                               = Nothing

viewCall :: CleanDemand -> Maybe (Card, CleanDemand)
viewCall (Call n cd)    = Just (n, cd)
viewCall cd@(Poly card) = Just (card, cd)
viewCall _              = Nothing

lubCleanDmd :: CleanDemand -> CleanDemand -> CleanDemand
-- Handle Prod
lubCleanDmd (Prod ds1) (viewProd (length ds1) -> Just ds2) =
  Prod $ zipWith lubDmd ds1 ds2 -- TODO: What about Note [Used should win]?
-- Handle Call
lubCleanDmd (Call n1 d1) (viewCall -> Just (n2, d2)) =
  Call (lubCard n1 n2) (lubCleanDmd d1 d2)
-- Handle Poly
lubCleanDmd (Poly n1)  (Poly n2) = Poly (lubCard n1 n2)
-- Make use of reflexivity (so we'll match the Prod or Call cases again).
lubCleanDmd cd1@Poly{} cd2       = lubCleanDmd cd2 cd1
-- Otherwise (Call `lub` Prod) return Top
lubCleanDmd _          _         = topCleanDmd

lubDmd :: Demand -> Demand -> Demand
lubDmd (n1 :* cd1) (n2 :* cd2) = lubCard n1 n2 :* lubCleanDmd cd1 cd2

plusCleanDmd :: CleanDemand -> CleanDemand -> CleanDemand
-- Handle Prod
plusCleanDmd (Prod ds1) (viewProd (length ds1) -> Just ds2) =
  Prod $ zipWith plusDmd ds1 ds2
-- Handle Call
-- TODO: Exciting special treatment of inner demand for call demands:
--    use `lubUse` instead of `plusUse`!
plusCleanDmd (Call n1 d1) (viewCall -> Just (n2, d2)) =
  Call (plusCard n1 n2) (lubCleanDmd d1 d2)
-- Handle Poly
plusCleanDmd (Poly n1)  (Poly n2) = Poly (plusCard n1 n2)
-- Make use of reflexivity (so we'll match the Prod or Call cases again).
plusCleanDmd cd1@Poly{} cd2       = plusCleanDmd cd2 cd1
-- Otherwise (Call `lub` Prod) return Top
plusCleanDmd _          _         = topCleanDmd

plusDmd :: Demand -> Demand -> Demand
plusDmd (n1 :* cd1) (n2 :* cd2) = plusCard n1 n2 :* plusCleanDmd cd1 cd2

oneifyDmd :: Demand -> Demand
oneifyDmd (n :* cd) = oneifyCard n :* cd

isTopDmd :: Demand -> Bool
-- ^ Used to suppress pretty-printing of an uninformative demand
isTopDmd dmd = dmd == topDmd

isAbsDmd :: Demand -> Bool
isAbsDmd (n :* _) = isAbs n

isStrictDmd :: Demand -> Bool
-- See Note [Strict demands]
isStrictDmd (n :* _) = isStrict n

isSeqDmd :: Demand -> Bool
isSeqDmd (C_11 :* cd) = cd == seqCleanDmd
isSeqDmd (C_1N :* cd) = cd == seqCleanDmd -- I wonder if we need this case.
isSeqDmd _            = False

-- | Is the value used at most once?
isUsedOnceDmd :: Demand -> Bool
isUsedOnceDmd (n :* _) = isUsedOnce n

-- More utility functions for strictness
seqDemand :: Demand -> ()
seqDemand (_ :* Prod ds) = seqDemandList ds
seqDemand _              = ()

seqDemandList :: [Demand] -> ()
seqDemandList = foldr (seq . seqDemand) ()

{- Note [Scaling demands]
~~~~~~~~~~~~~~~~~~~~~~~~~
If a demand is used multiple times (/reused/), for example the argument in an
unsaturated function call, then any upper bound of 1 mentioned that is not
protected by a Call (See Note [Scaling Call demands]), has to be relaxed to an
upper bound of n.
Essentially, the cardinality in all demands are /scaled/ by a factor >1.

Since
  * 'plus*' roughly amounts to + on the involved cardinalities
  * scaling by n is the same as performing + n times, meaning we can scale by
    doing n 'plus*'s
  * 'plus*' is idempotent, e.g. @a `plus` a `plus` a == a `plus` a@
A simple specification for scaling @a@ is by doing @a `plus` a@.
In practice, we implement this operation by the 'scale*' family of
functions, which is a bit more optimised.

Additionally, we provide predicates 'isScaleInvariant*' that are satisfied
exactly iff they are invariant under 'scale*'.

Note [Scaling Call demands]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
Scaling a 'Call' only concerns the outer call information, because the nested
'CleanDemand' is implicitly scaled by the outer cardinality. E.g., reusing
       C1(C1(U)) ("Called once with at least two arguments")
yields C(C1(U))  ("Called multiple times, but each time with at least two
                   arguments"),
*not*  C(C(U))   ("Called multiple times with one argument, and the resulting
                   PAP is also called multiple times with one argument").

This also follows from the specification
  scaleCleanDmd cd = plusCleanDmd cd cd,
which dictates that
  scaleCleanDmd (Call n cd) = Call (scaleCard n) (lubCleanDmd cd cd)
and 'lubCleanDmd' is reflexive, hence
  scaleCleanDmd (Call n cd) = Call (scaleCard n) cd.
-}

-- | Whether the given 'Card' is invariant to scaling, as if it was used
-- multiple times.
-- See Note [Scaling demands].
isScaleInvariantCard :: Card -> Bool
-- I double-checked that this will generate optimal code
isScaleInvariantCard c = plusCard c c == c

-- | See Note [Scaling demands].
isScaleInvariantDmd :: Demand -> Bool
-- isScaleInvariantDmd dmd = scaleDmd dmd == dmd
isScaleInvariantDmd (n :* cd) = isScaleInvariantCard n && isScaleInvariantCleanDmd cd

-- | See Note [Scaling demands].
isScaleInvariantCleanDmd :: CleanDemand -> Bool
-- isScaleInvariantCleanDmd cd = scaleCleanDmd cd == cd
isScaleInvariantCleanDmd (Poly n)   = isScaleInvariantCard n
isScaleInvariantCleanDmd (Prod ds)  = all isScaleInvariantDmd ds
isScaleInvariantCleanDmd (Call n _) = isScaleInvariantCard n -- See Note [Scaling Call demands]

-- | We try to avoid tracking weak free variable demands in strictness
-- signatures for analysis performance reasons. FVs with weak demands
-- provide next to no information when unleashed, so they are unleashed
-- once, upon leaving the scope of the binding whose strictness signature
-- was incomplete.
-- See Note [Lazy and unleashable free variables] in "GHC.Core.Opt.DmdAnal".
isWeakDmd :: Demand -> Bool
isWeakDmd (n :* cd) = not (isStrict n) && isScaleInvariantCleanDmd cd

keepAliveDmdEnv :: DmdEnv -> IdSet -> DmdEnv
-- (keepAliveDmdType dt vs) makes sure that the Ids in vs have
-- /some/ usage in the returned demand types -- they are not Absent
-- See Note [Absence analysis for stable unfoldings and RULES]
--     in GHC.Core.Opt.DmdAnal
keepAliveDmdEnv env vs
  = nonDetStrictFoldVarSet add env vs
  where
    add :: Id -> DmdEnv -> DmdEnv
    add v env = extendVarEnv_C add_dmd env v topDmd

    add_dmd :: Demand -> Demand -> Demand
    -- If the existing usage is Absent, make it used
    -- Otherwise leave it alone
    add_dmd dmd _ | isAbsDmd dmd = topDmd
                  | otherwise    = dmd

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
trimToType (n :* cd) ts
  = n :* go cd ts
  where
    go (Prod ds)   (TsProd tss)
      | equalLength ds tss    = Prod (zipWith trimToType ds tss)
    go (Call n cd) (TsFun ts) = Call n (go cd ts)
    go cd@Poly{}   _          = cd
    go _           _          = topCleanDmd

instance Outputable TypeShape where
  ppr TsUnk        = text "TsUnk"
  ppr (TsFun ts)   = text "TsFun" <> parens (ppr ts)
  ppr (TsProd tss) = parens (hsep $ punctuate comma $ map ppr tss)

{- *********************************************************************
*                                                                      *
                   Termination
*                                                                      *
********************************************************************* -}

-- | Divergence lattice. Models a subset lattice of the following exhaustive
-- set of divergence results:
--
-- [n] nontermination (e.g. loops)
-- [i] throws imprecise exception
-- [p] throws precise exception
-- [c] converges (reduces to WHNF)
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
  deriving( Eq, Show )

lubDivergence :: Divergence -> Divergence -> Divergence
lubDivergence Diverges div      = div
lubDivergence div      Diverges = div
lubDivergence ExnOrDiv ExnOrDiv = ExnOrDiv
lubDivergence _        _        = Dunno
-- This needs to commute with defaultFvDmd, i.e.
-- defaultFvDmd (r1 `lubDivergence` r2) = defaultFvDmd r1 `lubDmd` defaultFvDmd r2
-- (See Note [Default demand on free variables and arguments] for why)

plusDivergence :: Divergence -> Divergence -> Divergence
-- See Note [Asymmetry of 'plus*'], which concludes that 'plusDivergence' needs
-- to be symmetric.
-- Strictly speaking, we should have @plusDivergence Dunno Diverges = ExnOrDiv@.
-- But that regresses in too many places (every infinite loop, basically) to be
-- worth it and is only relevant in higher-order scenarios
-- (e.g. Divergence of @f (throwIO blah)@).
-- So 'plusDivergence' currently is 'glbDivergence', really.
plusDivergence Dunno    Dunno    = Dunno
plusDivergence Diverges _        = Diverges
plusDivergence _        Diverges = Diverges
plusDivergence _        _        = ExnOrDiv

instance Outputable Divergence where
  ppr Diverges = char 'b' -- for (b)ottom
  ppr ExnOrDiv = char 'x' -- for e(x)ception
  ppr Dunno    = empty

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
-}

------------------------------------------------------------------------
-- Combined demand result                                             --
------------------------------------------------------------------------

topDiv, exnDiv, botDiv :: Divergence
topDiv = Dunno
exnDiv = ExnOrDiv
botDiv = Diverges

-- | True if the result indicates that evaluation will not return.
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

{- Note [Default demand on free variables and arguments]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Free variables not mentioned in the environment of a 'DmdType'
are demanded according to the demand type's Divergence:
  * In a Diverges (botDiv) context, that demand is botDmd
    (HyperStr and Absent).
  * In all other contexts, the demand is absDmd (Lazy and Absent).
This is recorded in 'defaultFvDmd'.

Similarly, we can eta-expand demand types to get demands on excess arguments
not accounted for in the type, by consulting 'defaultArgDmd':
  * In a Diverges (botDiv) context, that demand is again botDmd.
  * In a ExnOrDiv (exnDiv) context, that demand is absDmd: We surely diverge
    before evaluating the excess argument, but don't want to eagerly evaluate
    it (cf. Note [Precise exceptions and strictness analysis]).
  * In a Dunno context (topDiv), the demand is topDmd, because
    it's perfectly possible to enter the additional lambda and evaluate it
    in unforeseen ways (so, not Absent).


************************************************************************
*                                                                      *
           Demand environments and types
*                                                                      *
************************************************************************
-}

type DmdEnv = VarEnv Demand   -- See Note [Default demand on free variables and arguments]

data DmdType = DmdType
                  DmdEnv        -- Demand on explicitly-mentioned
                                --      free variables
                  [Demand]      -- Demand on arguments
                  Divergence     -- See [Demand type Divergence]

{-
Note [Demand type Divergence]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In contrast to StrictSigs, DmdTypes are elicited under a specific incoming demand.
This is described in detail in Note [Understanding DmdType and StrictSig].
Here, we'll focus on what that means for a DmdType's Divergence in a higher-order
scenario.

Consider
  err x y = x `seq` y `seq` error (show x)
this has a strictness signature of
  <S><S>b
meaning that we don't know what happens when we call errin weaker contexts than
C(C(S)), like @err `seq` ()@ (S) and @err 1 `seq` ()@ (C(S)). We may not unleash
the botDiv, hence assume topDiv. Of course, in @err 1 2 `seq` ()@ the incoming
demand C(C(S)) is strong enough and we see that the expression diverges.

Now consider a function
  f g = g 1 2
with signature <C(S)>, and the expression
  f err `seq` ()
now f puts a strictness demand of C(C(S)) onto its argument, which is unleashed
on err via the App rule. In contrast to weaker head strictness, this demand is
strong enough to unleash err's signature and hence we see that the whole
expression diverges!

Note [Asymmetry of 'plus*']
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
second argument is just a 'BothDmdType'.

But note that the argument demand types are not guaranteed to be observed in
left to right order. For example, analysis of a case expression will pass the
demand type for the alts as the left argument and the type for the scrutinee as
the right argument. Also, it is not at all clear if there is such an order;
consider the LetUp case, where the RHS might be forced at any point while
evaluating the let body.
Therefore, it is crucial that 'plusDivergence' is symmetric!
-}

-- Equality needed for fixpoints in GHC.Core.Opt.DmdAnal
instance Eq DmdType where
  (==) (DmdType fv1 ds1 div1)
       (DmdType fv2 ds2 div2) = nonDetUFMToList fv1 == nonDetUFMToList fv2
         -- It's OK to use nonDetUFMToList here because we're testing for
         -- equality and even though the lists will be in some arbitrary
         -- Unique order, it is the same order for both
                              && ds1 == ds2 && div1 == div2

-- | Compute the least upper bound of two 'DmdType's elicited /by the same
-- incoming demand/!
lubDmdType :: DmdType -> DmdType -> DmdType
lubDmdType d1 d2
  = DmdType lub_fv lub_ds lub_div
  where
    n = max (dmdTypeDepth d1) (dmdTypeDepth d2)
    (DmdType fv1 ds1 r1) = etaExpandDmdType n d1
    (DmdType fv2 ds2 r2) = etaExpandDmdType n d2

    lub_fv  = plusVarEnv_CD lubDmd fv1 (defaultFvDmd r1) fv2 (defaultFvDmd r2)
    lub_ds  = zipWithEqual "lubDmdType" lubDmd ds1 ds2
    lub_div = lubDivergence r1 r2

type BothDmdArg = (DmdEnv, Divergence)

mkBothDmdArg :: DmdEnv -> BothDmdArg
mkBothDmdArg env = (env, topDiv)

toBothDmdArg :: DmdType -> BothDmdArg
toBothDmdArg (DmdType fv _ r) = (fv, r)

plusDmdType :: DmdType -> BothDmdArg -> DmdType
plusDmdType (DmdType fv1 ds1 r1) (fv2, t2)
    -- See Note [Asymmetry of 'plus*']
    -- 'plus' takes the argument/result info from its *first* arg,
    -- using its second arg just for its free-var info.
  = DmdType (plusVarEnv_CD plusDmd fv1 (defaultFvDmd r1) fv2 (defaultFvDmd t2))
            ds1
            (r1 `plusDivergence` t2)

instance Outputable DmdType where
  ppr (DmdType fv ds res)
    = hsep [hcat (map (angleBrackets . ppr) ds) <> ppr res,
            if null fv_elts then empty
            else braces (fsep (map pp_elt fv_elts))]
    where
      pp_elt (uniq, dmd) = ppr uniq <> text "->" <> ppr dmd
      fv_elts = nonDetUFMToList fv
        -- It's OK to use nonDetUFMToList here because we only do it for
        -- pretty printing

emptyDmdEnv :: VarEnv Demand
emptyDmdEnv = emptyVarEnv

botDmdType :: DmdType
botDmdType = DmdType emptyDmdEnv [] botDiv

-- | The demand type of doing nothing (lazy, absent, no Divergence
-- information). Note that it is ''not'' the top of the lattice (which would be
-- "may use everything"), so it is (no longer) called topDmdType.
-- (SG: I agree, but why is it still 'topDmd' then?)
nopDmdType :: DmdType
nopDmdType = DmdType emptyDmdEnv [] topDiv

isTopDmdType :: DmdType -> Bool
isTopDmdType (DmdType env args div)
  = div == topDiv && null args && isEmptyVarEnv env

-- | The demand type of an unspecified expression that is guaranteed to
-- throw a (precise or imprecise) exception or diverge.
exnDmdType :: DmdType
exnDmdType = DmdType emptyDmdEnv [] exnDiv

dmdTypeDepth :: DmdType -> Arity
dmdTypeDepth (DmdType _ ds _) = length ds

-- | This makes sure we can use the demand type with n arguments after eta
-- expansion, where n must not be lower than the demand types depth.
-- It appends the argument list with the correct 'defaultArgDmd'.
etaExpandDmdType :: Arity -> DmdType -> DmdType
etaExpandDmdType n d
  | n == depth = d
  | n >  depth = DmdType fv inc_ds div
  | otherwise  = pprPanic "etaExpandDmdType: arity decrease" (ppr n $$ ppr d)
  where depth = dmdTypeDepth d
        DmdType fv ds div = d
        -- Arity increase:
        --  * Demands on FVs are still valid
        --  * Demands on args also valid, plus we can extend with defaultArgDmd
        --    as appropriate for the given Divergence
        --  * Divergence is still valid:
        --    - A dead end after 2 arguments stays a dead end after 3 arguments
        --    - The remaining case is Dunno, which is already topDiv
        inc_ds  = take n (ds ++ repeat (defaultArgDmd div))

-- | A conservative approximation for a given 'DmdType' in case of an arity
-- decrease. Currently, it's just nopDmdType.
decreaseArityDmdType :: DmdType -> DmdType
decreaseArityDmdType _ = nopDmdType

seqDmdType :: DmdType -> ()
seqDmdType (DmdType env ds res) =
  seqDmdEnv env `seq` seqDemandList ds `seq` res `seq` ()

seqDmdEnv :: DmdEnv -> ()
seqDmdEnv env = seqEltsUFM seqDemandList env

splitDmdTy :: DmdType -> (Demand, DmdType)
-- Split off one function argument
-- We already have a suitable demand on all
-- free vars, so no need to add more!
splitDmdTy (DmdType fv (dmd:dmds) res_ty) = (dmd, DmdType fv dmds res_ty)
splitDmdTy ty@(DmdType _ [] res_ty)       = (defaultArgDmd res_ty, ty)

-- | When e is evaluated after executing an IO action that may throw a precise
-- exception, we act as if there is an additional control flow path that is
-- taken if e throws a precise exception. The demand type of this control flow
-- path
--   * is lazy and absent ('topDmd') in all free variables and arguments
--   * has 'exnDiv' 'Divergence' result
-- So we can simply take a variant of 'nopDmdType', 'exnDmdType'.
-- Why not 'nopDmdType'? Because then the result of 'e' can never be 'exnDiv'!
-- That means failure to drop dead-ends, see #18086.
-- See Note [Precise exceptions and strictness analysis]
deferAfterPreciseException :: DmdType -> DmdType
deferAfterPreciseException = lubDmdType exnDmdType

strictenDmd :: Demand -> Demand
strictenDmd (n :* cd) = plusCard C_10 n :* cd

-- This is used in dmdAnalStar when post-processing
-- a function's argument demand. So we only care about what
-- does to free variables, and whether it terminates.
-- see Note [Asymmetry of 'plus*']
multDmdType :: Card -> DmdType -> BothDmdArg
multDmdType n (DmdType fv _ res_ty)
    = -- pprTrace "multDmdType" (ppr n $$ ppr fv $$ ppr (multDmdEnv n fv)) $
      (multDmdEnv n fv, multDivergence n res_ty)

-- | In a non-strict scenario, we might not force the Divergence, in which case
-- we might converge, hence Dunno.
multDivergence :: Card -> Divergence -> Divergence
multDivergence n _ | not (isStrict n) = Dunno
multDivergence _ d                    = d

multDmdEnv :: Card -> DmdEnv -> DmdEnv
multDmdEnv n env
  | isAbs n   = emptyDmdEnv
    -- In this case (multDmd ds) == id; avoid a redundant rebuild
    -- of the environment. Be careful, bad things will happen if this doesn't
    -- match multDmd (see #13977).
  | n == C_11 = env
  | otherwise = mapVarEnv (multDmd n) env
  -- For the Absent case just discard alC_ sage information
  -- We only processed the thing at all to analyse the body
  -- See Note [Always analyse in virgin pass]

-- | See Note [Scaling demands]
reuseEnv :: DmdEnv -> DmdEnv
reuseEnv = multDmdEnv C_1N

-- | The trivial cases of the @mult*@ functions.
-- If @multTrivial n abs a = ma@, we have the following outcomes
-- depending on @n@:
--
--   * 'C_11' => multiply by one, @ma = Just a@
--   * 'C_00', 'C_10' (e.g. @'isAbs' n@) => return the absent thing,
--      @ma = Just abs@
--   * Otherwise ('C_01', 'C_*N') it's not a trivial case, @ma = Nothing@.
multTrivial :: Card -> a -> a -> Maybe a
multTrivial C_11 _   a           = Just a
multTrivial n    abs _ | isAbs n = Just abs
multTrivial _    _   _           = Nothing

multUnsat :: Card -> DmdType -> DmdType
multUnsat n (DmdType fv args res_ty)
  = -- pprTrace "multUnsat" (ppr n $$ ppr fv $$ ppr (multDmdEnv n fv)) $
    DmdType (multDmdEnv n fv)
            (map (multDmd n) args)
            (multDivergence n res_ty)

multCleanDmd :: Card -> CleanDemand -> CleanDemand
multCleanDmd n cd
  | Just cd' <- multTrivial n seqCleanDmd cd = cd'
multCleanDmd n (Poly n')    = Poly (multCard n n')
multCleanDmd n (Call n' cd) = Call (multCard n n') cd -- TODO Note
multCleanDmd n (Prod ds)    = Prod (map (multDmd n) ds)

multDmd :: Card -> Demand -> Demand
multDmd n    dmd
  | Just dmd' <- multTrivial n absDmd dmd = dmd'
multDmd n (m :* dmd) = multCard n m :* multCleanDmd n dmd

-- | Peels one call level from the demand, and also returns how many times we
-- entered the lambda body.
peelCallDmd :: CleanDemand -> (Card, CleanDemand)
peelCallDmd cd = viewCall cd `orElse` (topCard, topCleanDmd)

-- Peels that multiple nestings of calls clean demand and also returns
-- whether it was unsaturated (separately for strictness and usage
-- see Note [Demands from unsaturated function calls]
peelManyCalls :: Int -> CleanDemand -> Card
peelManyCalls 0 _                          = C_11
peelManyCalls n (viewCall -> Just (m, cd)) = m `multCard` peelManyCalls (n-1) cd
peelManyCalls _ _                          = C_0N

{-
Note [Demands from unsaturated function calls]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider a demand transformer d1 -> d2 -> r for f.
If a sufficiently detailed demand is fed into this transformer,
e.g <C(C(S)), C1(C1(S))> arising from "f x1 x2" in a strict, use-once context,
then d1 and d2 is precisely the demand unleashed onto x1 and x2 (similar for
the free variable environment) and furthermore the result information r is the
one we want to use.

An anonymous lambda is also an unsaturated function all (needs one argument,
none given), so this applies to that case as well.

But the demand fed into f might be less than <C(C(S)), C1(C1(S))>. There are a few cases:
 * Not enough demand on the strictness side:
   - In that case, we need to zap all strictness in the demand on arguments and
     free variables.
   - And finally Divergence information: If r says that f Diverges for sure,
     then this holds when the demand guarantees that two arguments are going to
     be passed. If the demand is lower, we may just as well converge.
     If we were tracking definite convegence, than that would still hold under
     a weaker demand than expected by the demand transformer.
 * Not enough demand from the usage side: The missing usage can be expanded
   using UCall Many, therefore this is subsumed by the third case:
 * At least one of the uses has a cardinality of Many.
   - Even if f puts a One demand on any of its argument or free variables, if
     we call f multiple times, we may evaluate this argument or free variable
     multiple times. So forget about any occurrence of "One" in the demand.

In dmdTransformSig, we call peelManyCalls to find out if we are in any of these
cases, and then call multUnsat to reduce the demand appropriately.

Similarly, dmdTransformDictSelSig and dmdAnal, when analyzing a Lambda, use
peelCallDmd, which peels only one level, but also returns the demand put on the
body of the function.
-}

peelFV :: DmdType -> Var -> (DmdType, Demand)
peelFV (DmdType fv ds res) id = -- pprTrace "rfv" (ppr id <+> ppr dmd $$ ppr fv)
                               (DmdType fv' ds res, dmd)
  where
  fv' = fv `delVarEnv` id
  -- See Note [Default demand on free variables and arguments]
  dmd  = lookupVarEnv fv id `orElse` defaultFvDmd res

addDemand :: Demand -> DmdType -> DmdType
addDemand dmd (DmdType fv ds res) = DmdType fv (dmd:ds) res

findIdDemand :: DmdType -> Var -> Demand
findIdDemand (DmdType fv _ res) id
  = lookupVarEnv fv id `orElse` defaultFvDmd res

{-
Note [Always analyse in virgin pass]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Tricky point: make sure that we analyse in the 'virgin' pass. Consider
   rec { f acc x True  = f (...rec { g y = ...g... }...)
         f acc x False = acc }
In the virgin pass for 'f' we'll give 'f' a very strict (bottom) type.
That might mean that we analyse the sub-expression containing the
E = "...rec g..." stuff in a bottom demand.  Suppose we *didn't analyse*
E, but just returned botType.

Then in the *next* (non-virgin) iteration for 'f', we might analyse E
in a weaker demand, and that will trigger doing a fixpoint iteration
for g.  But *because it's not the virgin pass* we won't start g's
iteration at bottom.  Disaster.  (This happened in $sfibToList' of
nofib/spectral/fibheaps.)

So in the virgin pass we make sure that we do analyse the expression
at least once, to initialise its signatures.

Note [Analyzing with lazy demand and lambdas]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The insight for analyzing lambdas follows from the fact that for
strictness S = C(L). This polymorphic expansion is critical for
cardinality analysis of the following example:

{-# NOINLINE build #-}
build g = (g (:) [], g (:) [])

h c z = build (\x ->
                let z1 = z ++ z
                 in if c
                    then \y -> x (y ++ z1)
                    else \y -> x (z1 ++ y))

One can see that `build` assigns to `g` demand <L,C(C1(U))>.
Therefore, when analyzing the lambda `(\x -> ...)`, we
expect each lambda \y -> ... to be annotated as "one-shot"
one. Therefore (\x -> \y -> x (y ++ z)) should be analyzed with a
demand <C(C(..), C(C1(U))>.

This is achieved by, first, converting the lazy demand L into the
strict S by the second cC_ase of the analysis.

Note [Analysing with absent demand]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose we analyse an expression with demand <L,A>.  The "A" means
"absent", so this expression will never be needed.  What should happen?
There are several wrinkles:

* We *do* want to analyse the expression regardless.
  Reason: Note [Always analyse in virgin pass]

  But we can post-process the results to ignore all the usage
  demands coming back. This is done by multDmdType.

* In a previous incarnation of GHC we needed to be extra careful in the
  case of an *unlifted type*, because unlifted values are evaluated
  even if they are not used.  Example (see #9254):
     f :: (() -> (# Int#, () #)) -> ()
          -- Strictness signature is
          --    <C(S(LS)), 1*C1(U(A,1*U()))>
          -- I.e. calls k, but discards first component of result
     f k = case k () of (# _, r #) -> r

     g :: Int -> ()
     g y = f (\n -> (# case y of I# y2 -> y2, n #))

  Here f's strictness signature says (correctly) that it calls its
  argument function and ignores the first component of its result.
  This is correct in the sense that it'd be fine to (say) modify the
  function so that always returned 0# in the first component.

  But in function g, we *will* evaluate the 'case y of ...', because
  it has type Int#.  So 'y' will be evaluated.  So we must record this
  usage of 'y', else 'g' will say 'y' is absent, and will w/w so that
  'y' is bound to an aBSENT_ERROR thunk.

  However, the argument of toCleanDmd always satisfies the let/app
  invariant; so if it is unlifted it is also okForSpeculation, and so
  can be evaluated in a short finite time -- and that rules out nasty
  cases like the one above.  (I'm not quite sure why this was a
  problem in an earlier version of GHC, but it isn't now.)
-}

{- *********************************************************************
*                                                                      *
                     Demand signatures
*                                                                      *
************************************************************************

In a let-bound Id we record its strictness info.
In principle, this strictness info is a demand transformer, mapping
a demand on the Id into a DmdType, which gives
        a) the free vars of the Id's value
        b) the Id's arguments
        c) an indication of the result of applying
           the Id to its arguments

However, in fact we store in the Id an extremely emascuated demand
transfomer, namely

                a single DmdType
(Nevertheless we dignify StrictSig as a distinct type.)

This DmdType gives the demands unleashed by the Id when it is applied
to as many arguments as are given in by the arg demands in the DmdType.
Also see Note [Demand type Divergence] for the meaning of a Divergence in a
strictness signature.

If an Id is applied to less arguments than its arity, it means that
the demand on the function at a call site is weaker than the vanilla
call demand, used for signature inference. Therefore we place a top
demand on all arguments. Otherwise, the demand is specified by Id's
signature.

For example, the demand transformer described by the demand signature
        StrictSig (DmdType {x -> <S,1*U>} <L,A><C_,(U,U)>m)
says that when the function is applied to two arguments, it
unleashes demand <S,1*U> on the free var x, <L,A> on the first arg,
and <C_,(U,U)> on the second, then returning a constructor.

If this same function is applied to one arg, all we can say is that it
uses x with <C_,>, and its arg with demand <C_,>.

Note [Understanding DmdType and StrictSig]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Demand types are sound approximations of an expression's semantics relative to
the incoming demand we put the expression under. Consider the following
expression:

    \x y -> x `seq` (y, 2*x)

Here is a table with demand types resulting from different incoming demands we
put that expression under. Note the monotonicity; a stronger incoming demand
yields a more precise demand type:

    incoming demand                  |  demand type
    ----------------------------------------------------
    <S           ,HU              >  |  <C_,><C_,>{}
    <C(C(S     )),C1(C1(U       ))>  |  <S,U><C_,>{}
    <C(C(S(S,L))),C1(C1(U(1*U,A)))>  |  <S,1*HU><L,A>{}

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

Here comes the subtle part: The threshold is encoded in the wrapped demand
type's depth! So in mkStrictSigForArity we make sure to trim the list of
argument demands to the given threshold arity. Call sites will make sure that
this corresponds to the arity of the call demand that elicited the wrapped
demand type. See also Note [What are demand signatures?] in GHC.Core.Opt.DmdAnal.
-}

-- | The depth of the wrapped 'DmdType' encodes the arity at which it is safe
-- to unleash. Better construct this through 'mkStrictSigForArity'.
-- See Note [Understanding DmdType and StrictSig]
newtype StrictSig = StrictSig DmdType
                  deriving( Eq )

instance Outputable StrictSig where
   ppr (StrictSig ty) = ppr ty

-- Used for printing top-level strictness pragmas in interface files
pprIfaceStrictSig :: StrictSig -> SDoc
pprIfaceStrictSig = ppr

-- | Turns a 'DmdType' computed for the particular 'Arity' into a 'StrictSig'
-- unleashable at that arity. See Note [Understanding DmdType and StrictSig]
mkStrictSigForArity :: Arity -> DmdType -> StrictSig
mkStrictSigForArity arity dmd_ty@(DmdType fvs args div)
  | arity < dmdTypeDepth dmd_ty = StrictSig (DmdType fvs (take arity args) div)
  | otherwise                   = StrictSig (etaExpandDmdType arity dmd_ty)

mkClosedStrictSig :: [Demand] -> Divergence -> StrictSig
mkClosedStrictSig ds res = mkStrictSigForArity (length ds) (DmdType emptyDmdEnv ds res)

splitStrictSig :: StrictSig -> ([Demand], Divergence)
splitStrictSig (StrictSig (DmdType _ dmds res)) = (dmds, res)

prependArgsStrictSig :: Int -> StrictSig -> StrictSig
-- ^ Add extra ('topDmd') arguments to a strictness signature.
-- In contrast to 'etaConvertStrictSig', this /prepends/ additional argument
-- demands. This is used by FloatOut.
prependArgsStrictSig new_args sig@(StrictSig dmd_ty@(DmdType env dmds res))
  | new_args == 0       = sig
  | isTopDmdType dmd_ty = sig
  | new_args < 0        = pprPanic "prependArgsStrictSig: negative new_args"
                                   (ppr new_args $$ ppr sig)
  | otherwise           = StrictSig (DmdType env dmds' res)
  where
    dmds' = replicate new_args topDmd ++ dmds

etaConvertStrictSig :: Arity -> StrictSig -> StrictSig
-- ^ We are expanding (\x y. e) to (\x y z. e z) or reducing from the latter to
-- the former (when the Simplifier identifies a new join points, for example).
-- In contrast to 'prependArgsStrictSig', this /appends/ extra arg demands if
-- necessary.
-- This works by looking at the 'DmdType' (which was produced under a call
-- demand for the old arity) and trying to transfer as many facts as we can to
-- the call demand of new arity.
-- An arity increase (resulting in a stronger incoming demand) can retain much
-- of the info, while an arity decrease (a weakening of the incoming demand)
-- must fall back to a conservative default.
etaConvertStrictSig arity (StrictSig dmd_ty)
  | arity < dmdTypeDepth dmd_ty = StrictSig $ decreaseArityDmdType dmd_ty
  | otherwise                   = StrictSig $ etaExpandDmdType arity dmd_ty

isTopSig :: StrictSig -> Bool
isTopSig (StrictSig ty) = isTopDmdType ty

hasDemandEnvSig :: StrictSig -> Bool
hasDemandEnvSig (StrictSig (DmdType env _ _)) = not (isEmptyVarEnv env)

strictSigDmdEnv :: StrictSig -> DmdEnv
strictSigDmdEnv (StrictSig (DmdType env _ _)) = env

-- | True if the signature diverges or throws an exception in a saturated call.
-- See Note [Dead ends].
isDeadEndSig :: StrictSig -> Bool
isDeadEndSig (StrictSig (DmdType _ _ res)) = isDeadEndDiv res

botSig :: StrictSig
botSig = StrictSig botDmdType

nopSig :: StrictSig
nopSig = StrictSig nopDmdType

seqStrictSig :: StrictSig -> ()
seqStrictSig (StrictSig ty) = seqDmdType ty

dmdTransformSig :: StrictSig -> CleanDemand -> DmdType
-- (dmdTransformSig fun_sig dmd) considers a call to a function whose
-- signature is fun_sig, with demand dmd.  We return the demand
-- that the function places on its context (eg its args)
dmdTransformSig (StrictSig dmd_ty@(DmdType _ arg_ds _)) cd
  = multUnsat (peelManyCalls (length arg_ds) cd) dmd_ty
    -- see Note [Demands from unsaturated function calls]

dmdTransformDataConSig :: Arity -> CleanDemand -> DmdType
-- Same as dmdTransformSig but for a data constructor (worker),
-- which has a special kind of demand transformer.
-- If the constructor is saturated, we feed the demand on
-- the result into the constructor arguments.
dmdTransformDataConSig arity cd = case go arity cd of
  Just dmds -> DmdType emptyDmdEnv dmds topDiv
  Nothing   -> nopDmdType -- Not saturated
  where
    go 0 cd                            = viewProd arity cd
    go n (viewCall -> Just (C_11, cd)) = go (n-1) cd  -- strict calls only!
    go _ _                             = Nothing

dmdTransformDictSelSig :: StrictSig -> CleanDemand -> DmdType
-- Like dmdTransformDataConSig, we have a special demand transformer
-- for dictionary selectors.  If the selector is saturated (ie has one
-- argument: the dictionary), we feed the demand on the result into
-- the indicated dictionary component.
-- NB: This currently doesn't handle newtype dictionaries and it's unclear how
-- it could without additional parameters.
dmdTransformDictSelSig (StrictSig (DmdType _ [(_ :* sig_cd)] _)) call_cd
   | (n, cd') <- peelCallDmd call_cd
   , Prod sig_ds  <- sig_cd
   = multUnsat n $
     DmdType emptyDmdEnv [C_11 :* Prod (map (enhance cd') sig_ds)] topDiv
   | otherwise
   = nopDmdType -- See Note [Demand transformer for a dictionary selector]
  where
    enhance cd old | isAbsDmd old = old
                   | otherwise    = C_11 :* cd  -- This is the one!

dmdTransformDictSelSig sig cd = pprPanic "dmdTransformDictSelSig: no args" (ppr sig $$ ppr cd)

{-
Note [Demand transformer for a dictionary selector]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If we evaluate (op dict-expr) under demand 'd', then we can push the demand 'd'
into the appropriate field of the dictionary. What *is* the appropriate field?
We just look at the strictness signature of the class op, which will be
something like: U(AAASAAAAA).  Then replace the 'S' by the demand 'd'.

For single-method classes, which are represented by newtypes the signature
of 'op' won't look like U(...), so matching on Prod will fail.
That's fine: if we are doing strictness analysis we are also doing inlining,
so we'll have inlined 'op' into a cast.  So we can bale out in a conservative
way, returning nopDmdType.

It is (just.. #8329) possible to be running strictness analysis *without*
having inlined class ops from single-method classes.  Suppose you are using
ghc --make; and the first module has a local -O0 flag.  So you may load a class
without interface pragmas, ie (currently) without an unfolding for the class
ops.   Now if a subsequent module in the --make sweep has a local -O flag
you might do strictness analysis, but there is no inlining for the class op.
This is weird, so I'm not worried about whether this optimises brilliantly; but
it should not fall over.
-}

argsOneShots :: StrictSig -> Arity -> [[OneShotInfo]]
-- ^ See Note [Computing one-shot info]
argsOneShots (StrictSig (DmdType _ arg_ds _)) n_val_args
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
argOneShots (_ :* cd) = go cd
  where
    go (Call n cd)
      | isUsedOnce n = OneShotLam    : go cd
      | otherwise    = NoOneShotInfo : go cd
    go _    = []

-- saturatedByOneShots n C1(C1(...)) = True,
--   <=>
-- there are at least n nested C1(..) calls
-- See Note [Demand on the worker] in GHC.Core.Opt.WorkWrap
saturatedByOneShots :: Int -> Demand -> Bool
saturatedByOneShots n (_ :* cd) = isUsedOnce (peelManyCalls n cd)

{- Note [Computing one-shot info]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider a call
    f (\pqr. e1) (\xyz. e2) e3
where f has usage signature
    C1(C(C1(U))) C1(U) U
Then argsOneShots returns a [[OneShotInfo]] of
    [[OneShot,NoOneShotInfo,OneShot],  [OneShot]]
The occurrence analyser propagates this one-shot infor to the
binders \pqr and \xyz; see Note [Use one-shot information] in "GHC.Core.Opt.OccurAnal".
-}

-- | Returns true if an application to n args would diverge or throw an
-- exception. See Note [Unsaturated applications] and Note [Dead ends].
appIsDeadEnd :: StrictSig -> Int -> Bool
appIsDeadEnd (StrictSig (DmdType _ ds res)) n
  = isDeadEndDiv res && not (lengthExceeds ds n)

{-
Note [Unsaturated applications]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If a function having bottom as its demand result is applied to a less
number of arguments than its syntactic arity, we cannot say for sure
that it is going to diverge. This is the reason why we use the
function appIsDeadEnd, which, given a strictness signature and a number
of arguments, says conservatively if the function is never going to return.
See Note [Dead ends].
-}

zapUsageEnvSig :: StrictSig -> StrictSig
-- Remove the usage environment from the demand
zapUsageEnvSig (StrictSig (DmdType _ ds r)) = mkClosedStrictSig ds r

zapUsageDemand :: Demand -> Demand
-- Remove the usage info, but not the strictness info, from the demand
zapUsageDemand = kill_usage $ KillFlags
    { kf_abs         = True
    , kf_used_once   = True
    , kf_called_once = True
    }

-- | Remove all 1* information (but not C1 information) from the demand
zapUsedOnceDemand :: Demand -> Demand
zapUsedOnceDemand = kill_usage $ KillFlags
    { kf_abs         = False
    , kf_used_once   = True
    , kf_called_once = False
    }

-- | Remove all 1* information (but not C1 information) from the strictness
--   signature
zapUsedOnceSig :: StrictSig -> StrictSig
zapUsedOnceSig (StrictSig (DmdType env ds r))
    = StrictSig (DmdType env (map zapUsedOnceDemand ds) r)

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
kill_usage kfs (n :* cd) = kill_usage_card kfs n :* kill_usage_cd kfs cd

kill_usage_cd :: KillFlags -> CleanDemand -> CleanDemand
kill_usage_cd kfs (Call n cd)
  | kf_called_once kfs      = Call (lubCard C_1N n) (kill_usage_cd kfs cd)
  | otherwise               = Call n                (kill_usage_cd kfs cd)
kill_usage_cd kfs (Prod ds) = Prod (map (kill_usage kfs) ds)
kill_usage_cd _   cd        = cd

-- | If the argument is a used non-newtype dictionary, give it strict demand.
-- Also split the product type & demand and recur in order to similarly
-- strictify the argument's contained used non-newtype superclass dictionaries.
-- We use the demand as our recursive measure to guarantee termination.
strictifyDictDmd :: Type -> Demand -> Demand
strictifyDictDmd ty (n :* Prod ds)
  | not (isAbs n)
  , Just field_tys <- as_non_newtype_dict ty
  = C_1N :* -- main idea: ensure it's strict
      if all (not . isAbsDmd) ds
        then topCleanDmd -- abstract to strict w/ arbitrary component use,
                         -- since this smells like reboxing; results in CBV
                         -- boxed
                         --
                         -- TODO revisit this if we ever do boxity analysis
        else Prod (zipWith strictifyDictDmd field_tys ds)
  where
    -- | Return a TyCon and a list of field types if the given
    -- type is a non-newtype dictionary type
    as_non_newtype_dict ty
      | Just (tycon, _arg_tys, _data_con, map scaledThing -> inst_con_arg_tys)
          <- splitDataProductType_maybe ty
      , not (isNewTyCon tycon)
      , isClassTyCon tycon
      = Just inst_con_arg_tys
      | otherwise
      = Nothing
strictifyDictDmd _  dmd = dmd

strictifyDmd :: Demand -> Demand
strictifyDmd (n :* cd) = plusCard n C_10 :* cd

{-
Note [HyperStr and Use demands]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
TODO: Irrelevant?!
The information "HyperStr" needs to be in the strictness signature, and not in
the demand signature, because we still want to know about the demand on things. Consider

    f (x,y) True  = error (show x)
    f (x,y) False = x+1

The signature of f should be <S(SL),1*U(1*U(U),A)><S,1*U>m. If we were not
distinguishing the uses on x and y in the True case, we could either not figure
out how deeply we can unpack x, or that we do not have to pass y.
-}

instance Outputable Card where
  ppr = text . show

instance Outputable Demand where
  ppr dmd@(n :* cd)
    | isAbs n          = ppr n
    | dmd == polyDmd n = ppr n
    | otherwise        = ppr n <> ppr cd

instance Outputable CleanDemand where
  ppr (Poly cd)   = ppr cd
  ppr (Call n cd) = char 'C' <> ppr n <> parens (ppr cd)
  ppr (Prod ds)   = parens (fields ds)
    where
      fields []     = empty
      fields [x]    = ppr x
      fields (x:xs) = ppr x <> char ',' <> fields xs

{-
************************************************************************
*                                                                      *
                     Serialisation
*                                                                      *
************************************************************************
-}

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
  put_ bh (n :* cd) = put_ bh n *> put_ bh cd
  get bh = (:*) <$> get bh <*> get bh

instance Binary CleanDemand where
  put_ bh (Poly cd)   = putByte bh 0 *> put_ bh cd
  put_ bh (Call n cd) = putByte bh 1 *> put_ bh n *> put_ bh cd
  put_ bh (Prod ds)   = putByte bh 2 *> put_ bh ds
  get bh = do
    h <- getByte bh
    case h of
      0 -> Poly <$> get bh
      1 -> Call <$> get bh <*> get bh
      2 -> Prod <$> get bh
      _ -> pprPanic "Binary:CleanDemand" (ppr (fromIntegral h :: Int))

instance Binary StrictSig where
  put_ bh (StrictSig aa) = put_ bh aa
  get bh = StrictSig <$> get bh

instance Binary DmdType where
  -- Ignore DmdEnv when spitting out the DmdType
  put_ bh (DmdType _ ds dr) = put_ bh ds *> put_ bh dr
  get bh = DmdType emptyDmdEnv <$> get bh <*> get bh

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
