module Supercompile.StaticFlags where

import Data.Char (toLower)

import FastString
import StaticFlags


parseEnum :: String -> a -> [(String, a)] -> a
parseEnum prefix def opts = maybe def parse $ lookup_str prefix
  where parse = maybe (error "parseEnum: unknown option") id . flip lookup opts . map toLower


-- The StaticFlagsParser admits any option beginning with -fsupercompiler

-- | The situations in which will demand a SUPERINLINABLE annotation is present
data Superinlinability = ForEverything | ForRecursion | ForNothing

sUPERINLINABILITY :: Superinlinability
sUPERINLINABILITY = parseEnum "-fsupercompiler-superinlinability" ForRecursion [("", ForRecursion), ("recursion", ForRecursion), ("everything", ForEverything), ("nothing", ForNothing)]

data InstanceMatching = NoInstances | InstancesOfGeneralised | AllInstances

-- I've decided that allowing arbitrary tiebacks to any ancestor state overlaps too much with the combination
-- of MSG-based generalisation+rollback, and has the potential to lose more useful optimisation than that combo does.
-- Matching back to generalised stuff is still a good idea, but we need to propagate generalised flags more agressively (FIXME)
iNSTANCE_MATCHING :: InstanceMatching
iNSTANCE_MATCHING = parseEnum "-fsupercompiler-instance-matching" InstancesOfGeneralised [("full", AllInstances), ("generalised", InstancesOfGeneralised), ("none", NoInstances)]

-- This is not remotely safe:
fLOAT_TO_MATCH :: Bool
fLOAT_TO_MATCH = False

-- This is also not safe as implemented. It could be made safe with some fiddling, but it's not worth it:
rEDUCE_BEFORE_MATCH :: Bool
rEDUCE_BEFORE_MATCH = False

-- At the moment, this HAS to be on for termination to be assured:
eAGER_SPLIT_VALUES :: Bool
eAGER_SPLIT_VALUES = not $ lookUp $ fsLit "-fsupercompiler-no-eager-split-values"

rEFINE_ALTS :: Bool
rEFINE_ALTS = not $ lookUp $ fsLit "-fsupercompiler-no-refine-alts"
--rEFINE_ALTS = False

dEEDS :: Bool
dEEDS = lookUp $ fsLit "-fsupercompiler-deeds"
--dEEDS = True

bOUND_STEPS :: Bool
bOUND_STEPS = lookUp $ fsLit "-fsupercompiler-bound-steps"
--bOUND_STEPS = True

-- For debugging very long-running supercompilation
dEPTH_LIIMT :: Maybe Int
dEPTH_LIIMT = Just (lookup_def_int "-fsupercompiler-depth-limit" maxBound)
--dEPTH_LIIMT = Just 10

pOSITIVE_INFORMATION :: Bool
pOSITIVE_INFORMATION = lookUp $ fsLit "-fsupercompiler-positive-information"
--pOSITIVE_INFORMATION = True

-- In favour of preinitialization:
--  1. We can reuse the normal speculation mechanism of the supercompiler to expose
--     more almost-cheap top-level bindings
--  2. No need to mark some heap bindings as "let" bindings, so things are simpler
--  3. None of the complications associated with speculating "let" marked bindings (see below)
--  4. Probably more reliable at finding reuse opportunities: "let"-marked bindings basically
--     only work for expressions like (Just map) or (map), though they are very good at
--     preventing lambda-abstraction over names we don't have unfoldings for.
--     Preinitialization with eta-expansion is particularly good at finding (rare) lambdas
--     in the arguments of data/casts within other lambdas. (FIXME: data not implemented)
--
-- Against preinitialization:
--  1. If an unfolding is bound at top level but non-cheap, we need to:
--    a) Prevent inlining it (so we don't duplicate work)
--    b) Still refer to it with a simple free variable rather than via a lambda
--   It is hard to accomplish either of these things with preinit, especially
--   if we delay cheapness detection to the normal speculation mechanism! (FIXME: I'm not doing either..)
--  2. Have to "eta-expand" to get better tieback chance. Speculation is needed for "accurate" eta-expansion.
--  3. The memo table is much larger (in benchmarks matching is not a major hotspot, though)
--  4. It is convenient to hide let-bound bindings in the pretty-printer (perhaps we could do
--     something else though -- e.g. the speculator should mark bindings as "do-not-print" so
--     only new bindings from reduce are printed each time)
--
--
-- NB: it *is* important that we speculate the unfoldings, even though you would think that
-- GHC would have already done its best to make them into manifest values. Sample things that can be
-- improved by this are:
--
-- 1. $d = let x = (y |> co) in D x
--    GHC doesn't let-float the x binding since it originated from ANFing. See also:
--    i = let i# = 1# in I# i#
--
-- 2. GHC.Base.$fMonadIO_$c>>= = bindIO
--    GHC doesn't necessarily eliminate a trivial equality like this if both names are exported.
--
-- 3. choice [parser_1, parser_2, parser_3] :: Parser
--    This expression has arity 1 but GHC can't see that because it is unwilling to push
--    the case-scrutinisation of the list input to "choice" under a lambda. Our evaluator
--    eliminates that scrutinisation entirely so we don't have any such problem.
--
-- GHC is pretty good at discovering all the obvious eta-expansions by itself before this point.
--
--
-- NB: speculating unfoldings is not as trivial as you might think! Consider the first
-- example above:
--   $d = let x = (y |> co) in D x
--
-- If we speculate this we presumably want to let-float:
--   x = (y |> co)
--   $d = D x
--
-- But there is no "real" top-level x binding we can just refer to as a free variable of the
-- supercompiled term.
--
-- Here are some possible responses:
--
-- 0. Manually create some appropriate top-level bindings in the current module.
--    Of course, this is only appropriate if *every* one of the new bindings are values, or we risk
--    work duplication. If even one is not, as in:
--     x = f_with_arity_4 1 2 (fib 100)
--
--    Then we cannot provide an unfolding for x in the state's heap since doing so cannot help but
--    lose the shared (fib 100) work. (Perhaps we could make an exception for saturated datacons
--    where we can sometimes extract the shared work thunk by case analysis, but this is complex.)
--
-- 1. We could normalise each unfolding to something of the form
--      letrec x1 = u1; ...; xn = un in v [|> co]
--    Where:
--      u ::= v [|> co]
--          | x [|> co]
--   
--    Such that x \elem {x1,..,xn} OR is the name of another succesfully-normalised unfolding.
--    With unfoldings of this form, we can just copy in the whole thing whenever we want to inline
--    the unfolding at a use site.
--
-- 2. We could guarantee that any let-marked thing in the heap is available to duplicate. In this case,
--    I think it is safe to allow the evaluator to create actual update frames for them. This will
--    allow the actual unfolding to be done as a normal part of evaluation.
--
--    When the update frames are popped the new bindings should be added to the heap as simple internal
--    bindings, probably. NB: this does lose the benefits of being a let-binding! Alternatively we could
--    add them as let bindings as long as it won't refer to any non-let bindings, which gets the benefit
--    of let bindings in almost all cases where it is possible.
--
--    In this scheme, we can speculate to ensure that we get the right form of binding and then throw
--    away the work of the speculator.
--
-- 3. We could speculate and then throw away any unfoldings that refer to newly-created let bindings.
--    This precludes use of the speculation mechanism for examples 1 and 3 above, but OK for 2.
--
--    Of course, if this only solves 2 then perhaps a simple renaming pass would be just as good and easier!
--
--    This solution is good for finding partial applications as well (at least for those partial applications
--    to arguments that have already been floated out by the previous GHC invocation), but this isn't useful
--    in practice.

pREINITALIZE_MEMO_TABLE :: Bool
pREINITALIZE_MEMO_TABLE = lookUp $ fsLit "-fsupercompiler-preinitalize"

data DeedsPolicy = FCFS | Proportional
                 deriving (Read)

dEEDS_POLICY :: DeedsPolicy
dEEDS_POLICY = parseEnum "-fsupercompiler-deeds-policy" Proportional [("fcfs", FCFS), ("proportional", Proportional)]

bLOAT_FACTOR :: Int
--bLOAT_FACTOR = fromMaybe 10 $ listToMaybe [read val | arg <- aRGS, Just val <- [stripPrefix "--bloat=" arg]]
bLOAT_FACTOR = lookup_def_int "-fsupercompiler-bloat-factor" 10
 -- NB: need a bloat factor of at least 5 to get append/append fusion to work. The critical point is:
 --
 --  let (++) = ...
 --  in case (case xs of []     -> ys
 --                      (x:xs) -> x : (xs ++ ys)) of
 --    []     -> zs
 --    (x:xs) -> x : (xs ++ zs)
 --
 -- We need to duplicate the case continuation into each branch, so at one time we will have:
 --  1) Two copies of (++) in the [] branch of the inner case
 --    a) One in the heap
 --    b) One from the stack (from [_] ++ zs)
 --  2) Similarly two copies in the (:) branch of the inner case
 --  3) One copy manifested in the residual branch of xs
 --
 -- Total = 5 copies (due to tiebacks, the residual program will do better than this)
 --
 -- 
 -- Unfortunately, my implementation doesn't tie back as eagerly as you might like, so we actually peel the loop once and
 -- hence need a bloat factor of 8 here (5 + 3 other case statements derived from (++))
 -- TODO: figure out how to reduce this number.


cALL_BY_NAME :: Bool
cALL_BY_NAME = lookUp $ fsLit "-fsupercompiler-call-by-name"


dUPLICATE_VALUES_EVALUATOR, dUPLICATE_VALUES_SPLITTER :: Bool
dUPLICATE_VALUES_EVALUATOR = lookUp $ fsLit "-fsupercompiler-duplicate-values-evaluator"
dUPLICATE_VALUES_SPLITTER = lookUp $ fsLit "-fsupercompiler-duplicate-values-splitter"


data TagBagType = TBT { tagBagPairwiseGrowth :: Bool }
                deriving (Show)

tAG_COLLECTION :: TagBagType
tAG_COLLECTION = parseEnum "-fsupercompiler-tag-collection" (TBT False) [("bags", TBT False), ("bags-strong", TBT True)]


data GeneralisationType = NoGeneralisation | AllEligible | DependencyOrder Bool | StackFirst

gENERALISATION :: GeneralisationType
gENERALISATION = parseEnum "-fsupercompiler-generalisation" StackFirst [("none", NoGeneralisation), ("all-eligible", AllEligible), ("first-reachable", DependencyOrder True), ("last-reachable", DependencyOrder False), ("stack-first", StackFirst)]

oCCURRENCE_GENERALISATION :: Bool
oCCURRENCE_GENERALISATION = not $ lookUp $ fsLit "-fsupercompiler-no-occurrence-generalisation"

tYPE_GEN :: Bool
tYPE_GEN = True

eVALUATE_PRIMOPS :: Bool
eVALUATE_PRIMOPS = not $ lookUp $ fsLit "-fsupercompiler-no-primops"

sPECULATION :: Bool
sPECULATION = not $ lookUp $ fsLit "-fsupercompiler-no-speculation"

lOCAL_TIEBACKS :: Bool
lOCAL_TIEBACKS = lookUp $ fsLit "-fsupercompiler-local-tiebacks"

rEFINE_FULFILMENT_FVS :: Bool
rEFINE_FULFILMENT_FVS = not $ lookUp $ fsLit "-fsupercompiler-no-refine-fulfilment-fvs"


hISTORY_TREE :: Bool
hISTORY_TREE = not $ lookUp $ fsLit "-fsupercompiler-no-history-tree"

rEDUCE_ROLLBACK :: Bool
rEDUCE_ROLLBACK = not $ lookUp $ fsLit "-fsupercompiler-no-reduce-rollback"

sC_ROLLBACK :: Bool
sC_ROLLBACK = not $ lookUp $ fsLit "-fsupercompiler-no-sc-rollback"


tRACE :: Bool
tRACE = lookUp $ fsLit "-fsupercompiler-trace"
