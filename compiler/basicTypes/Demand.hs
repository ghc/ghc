{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

\section[Demand]{@Demand@: A decoupled implementation of a demand domain}
-}

{-# LANGUAGE CPP, FlexibleInstances, TypeSynonymInstances, RecordWildCards #-}

module Demand (
        StrDmd, UseDmd(..), Count,

        Demand, DmdShell, CleanDemand, getStrDmd, getUseDmd,
        mkProdDmd, mkOnceUsedDmd, mkManyUsedDmd, mkHeadStrict, oneifyDmd,
        toCleanDmd,
        absDmd, topDmd, botDmd, seqDmd,
        lubDmd, bothDmd,
        lazyApply1Dmd, lazyApply2Dmd, strictApply1Dmd,
        catchArgDmd,
        isTopDmd, isAbsDmd, isSeqDmd,
        peelUseCall, cleanUseDmd_maybe, strictenDmd, bothCleanDmd,
        addCaseBndrDmd,

        DmdType(..), dmdTypeDepth, lubDmdType, bothDmdType,
        nopDmdType, botDmdType, mkDmdType,
        addDemand, removeDmdTyArgs,
        BothDmdArg, mkBothDmdArg, toBothDmdArg,

        DmdEnv, emptyDmdEnv,
        peelFV, findIdDemand,

        DmdResult, CPRResult,
        isBotRes, isTopRes,
        topRes, botRes, exnRes, cprProdRes,
        vanillaCprProdRes, cprSumRes,
        appIsBottom, isBottomingSig, pprIfaceStrictSig,
        trimCPRInfo, returnsCPR_maybe,
        StrictSig(..), mkStrictSig, mkClosedStrictSig,
        nopSig, botSig, exnSig, cprProdSig,
        isTopSig, hasDemandEnvSig,
        splitStrictSig, strictSigDmdEnv,
        increaseStrictSigArity, etaExpandStrictSig,

        seqDemand, seqDemandList, seqDmdType, seqStrictSig,

        evalDmd, cleanEvalDmd, cleanEvalProdDmd, isStrictDmd,
        splitDmdTy, splitFVs,
        deferAfterIO,
        postProcessUnsat, postProcessDmdType,

        splitProdDmd_maybe, peelCallDmd, peelManyCalls, mkCallDmd,
        mkWorkerDemand, dmdTransformSig, dmdTransformDataConSig,
        dmdTransformDictSelSig, argOneShots, argsOneShots, saturatedByOneShots,
        trimToType, TypeShape(..),

        useCount, isUsedOnce, reuseEnv,
        killUsageDemand, killUsageSig, zapUsageDemand, zapUsageEnvSig,
        zapUsedOnceDemand, zapUsedOnceSig,
        strictifyDictDmd, strictifyDmd

     ) where

#include "HsVersions.h"

import GhcPrelude

import DynFlags
import Outputable
import Var ( Var )
import VarEnv
import UniqFM
import Util
import BasicTypes
import Binary
import Maybes           ( orElse )

import Type            ( Type )
import TyCon           ( isNewTyCon, isClassTyCon )
import DataCon         ( splitDataProductType_maybe )

{-
************************************************************************
*                                                                      *
        Joint domain for Strictness and Absence
*                                                                      *
************************************************************************
-}

data JointDmd s u = JD { sd :: s, ud :: u }
  deriving ( Eq, Show )

getStrDmd :: JointDmd s u -> s
getStrDmd = sd

getUseDmd :: JointDmd s u -> u
getUseDmd = ud

-- Pretty-printing
instance (Outputable s, Outputable u) => Outputable (JointDmd s u) where
  ppr (JD {sd = s, ud = u}) = angleBrackets (ppr s <> char ',' <> ppr u)

-- Well-formedness preserving constructors for the joint domain
mkJointDmd :: s -> u -> JointDmd s u
mkJointDmd s u = JD { sd = s, ud = u }

mkJointDmds :: [s] -> [u] -> [JointDmd s u]
mkJointDmds ss as = zipWithEqual "mkJointDmds" mkJointDmd ss as


{-
************************************************************************
*                                                                      *
            Strictness domain
*                                                                      *
************************************************************************

        Lazy
         |
  ExnStr x -
           |
        HeadStr
        /     \
    SCall      SProd
        \      /
        HyperStr

Note [Exceptions and strictness]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Exceptions need rather careful treatment, especially because of 'catch'
('catch#'), 'catchSTM' ('catchSTM#'), and 'orElse' ('catchRetry#').
See Trac #11555, #10712 and #13330, and for some more background, #11222.

There are three main pieces.

* The Termination type includes ThrowsExn, meaning "under the given
  demand this expression either diverges or throws an exception".

  This is relatively uncontroversial. The primops raise# and
  raiseIO# both return ThrowsExn; nothing else does.

* An ArgStr has an ExnStr flag to say how to process the Termination
  result of the argument.  If the ExnStr flag is ExnStr, we squash
  ThrowsExn to topRes.  (This is done in postProcessDmdResult.)

Here is the key example

    catchRetry# (\s -> retry# s) blah

We analyse the argument (\s -> retry# s) with demand
    Str ExnStr (SCall HeadStr)
i.e. with the ExnStr flag set.
  - First we analyse the argument with the "clean-demand" (SCall
    HeadStr), getting a DmdResult of ThrowsExn from the saturated
    application of retry#.
  - Then we apply the post-processing for the shell, squashing the
    ThrowsExn to topRes.

This also applies uniformly to free variables.  Consider

    let r = \st -> retry# st
    in catchRetry# (\s -> ...(r s')..) handler st

If we give the first argument of catch a strict signature, we'll get a demand
'C(S)' for 'r'; that is, 'r' is definitely called with one argument, which
indeed it is.  But when we post-process the free-var demands on catchRetry#'s
argument (in postProcessDmdEnv), we'll give 'r' a demand of (Str ExnStr (SCall
HeadStr)); and if we feed that into r's RHS (which would be reasonable) we'll
squash the retry just as if we'd inlined 'r'.

* We don't try to get clever about 'catch#' and 'catchSTM#' at the moment. We
previously (#11222) tried to take advantage of the fact that 'catch#' calls its
first argument eagerly. See especially commit
9915b6564403a6d17651e9969e9ea5d7d7e78e7f. We analyzed that first argument with
a strict demand, and then performed a post-processing step at the end to change
ThrowsExn to TopRes.  The trouble, I believe, is that to use this approach
correctly, we'd need somewhat different information about that argument.
Diverges, ThrowsExn (i.e., diverges or throws an exception), and Dunno are the
wrong split here.  In order to evaluate part of the argument speculatively,
we'd need to know that it *does not throw an exception*. That is, that it
either diverges or succeeds. But we don't currently have a way to talk about
that. Abstractly and approximately,

catch# m f s = case ORACLE m s of
  DivergesOrSucceeds -> m s
  Fails exc -> f exc s

where the magical ORACLE determines whether or not (m s) throws an exception
when run, and if so which one. If we want, we can safely consider (catch# m f s)
strict in anything that both branches are strict in (by performing demand
analysis for 'catch#' in the same way we do for case). We could also safely
consider it strict in anything demanded by (m s) that is guaranteed not to
throw an exception under that demand, but I don't know if we have the means
to express that.

My mind keeps turning to this model (not as an actual change to the type, but
as a way to think about what's going on in the analysis):

newtype IO a = IO {unIO :: State# s -> (# s, (# SomeException | a #) #)}
instance Monad IO where
  return a = IO $ \s -> (# s, (# | a #) #)
  IO m >>= f = IO $ \s -> case m s of
    (# s', (# e | #) #) -> (# s', e #)
    (# s', (# | a #) #) -> unIO (f a) s
raiseIO# e s = (# s, (# e | #) #)
catch# m f s = case m s of
  (# s', (# e | #) #) -> f e s'
  res -> res

Thinking about it this way seems likely to be productive for analyzing IO
exception behavior, but imprecise exceptions and asynchronous exceptions remain
quite slippery beasts. Can we incorporate them? I think we can. We can imagine
applying 'seq#' to evaluate @m s@, determining whether it throws an imprecise
or asynchronous exception or whether it succeeds or throws an IO exception.
This confines the peculiarities to 'seq#', which is indeed rather essentially
peculiar.
-}

-- | Vanilla strictness domain
data StrDmd
  = HyperStr             -- ^ Hyper-strict (bottom of the lattice).
                         -- See Note [HyperStr and Use demands]

  | SCall StrDmd         -- ^ Call demand
                         -- Used only for values of function type

  | SProd [ArgStr]       -- ^ Product
                         -- Used only for values of product type
                         -- Invariant: not all components are HyperStr (use HyperStr)
                         --            not all components are Lazy     (use HeadStr)

  | HeadStr              -- ^ Head-Strict
                         -- A polymorphic demand: used for values of all types,
                         --                       including a type variable

  deriving ( Eq, Show )

-- | Strictness of a function argument.
type ArgStr = Str StrDmd

-- | Strictness demand.
data Str s = Lazy         -- ^ Lazy (top of the lattice)
           | Str ExnStr s -- ^ Strict
  deriving ( Eq, Show )

-- | How are exceptions handled for strict demands?
data ExnStr  -- See Note [Exceptions and strictness]
  = VanStr   -- ^ "Vanilla" case, ordinary strictness

  | ExnStr   -- ^ @Str ExnStr d@ means be strict like @d@ but then degrade
             -- the 'Termination' info 'ThrowsExn' to 'Dunno'.
             -- e.g. the first argument of @catch@ has this strictness.
  deriving( Eq, Show )

-- Well-formedness preserving constructors for the Strictness domain
strBot, strTop :: ArgStr
strBot = Str VanStr HyperStr
strTop = Lazy

mkSCall :: StrDmd -> StrDmd
mkSCall HyperStr = HyperStr
mkSCall s        = SCall s

mkSProd :: [ArgStr] -> StrDmd
mkSProd sx
  | any isHyperStr sx = HyperStr
  | all isLazy     sx = HeadStr
  | otherwise         = SProd sx

isLazy :: ArgStr -> Bool
isLazy Lazy     = True
isLazy (Str {}) = False

isHyperStr :: ArgStr -> Bool
isHyperStr (Str _ HyperStr) = True
isHyperStr _                = False

-- Pretty-printing
instance Outputable StrDmd where
  ppr HyperStr      = char 'B'
  ppr (SCall s)     = char 'C' <> parens (ppr s)
  ppr HeadStr       = char 'S'
  ppr (SProd sx)    = char 'S' <> parens (hcat (map ppr sx))

instance Outputable ArgStr where
  ppr (Str x s)     = (case x of VanStr -> empty; ExnStr -> char 'x')
                      <> ppr s
  ppr Lazy          = char 'L'

lubArgStr :: ArgStr -> ArgStr -> ArgStr
lubArgStr Lazy        _           = Lazy
lubArgStr _           Lazy        = Lazy
lubArgStr (Str x1 s1) (Str x2 s2) = Str (x1 `lubExnStr` x2) (s1 `lubStr` s2)

lubExnStr :: ExnStr -> ExnStr -> ExnStr
lubExnStr VanStr VanStr = VanStr
lubExnStr _      _      = ExnStr   -- ExnStr is lazier

lubStr :: StrDmd -> StrDmd -> StrDmd
lubStr HyperStr s              = s
lubStr (SCall s1) HyperStr     = SCall s1
lubStr (SCall _)  HeadStr      = HeadStr
lubStr (SCall s1) (SCall s2)   = SCall (s1 `lubStr` s2)
lubStr (SCall _)  (SProd _)    = HeadStr
lubStr (SProd sx) HyperStr     = SProd sx
lubStr (SProd _)  HeadStr      = HeadStr
lubStr (SProd s1) (SProd s2)
    | s1 `equalLength` s2      = mkSProd (zipWith lubArgStr s1 s2)
    | otherwise                = HeadStr
lubStr (SProd _) (SCall _)     = HeadStr
lubStr HeadStr   _             = HeadStr

bothArgStr :: ArgStr -> ArgStr -> ArgStr
bothArgStr Lazy        s           = s
bothArgStr s           Lazy        = s
bothArgStr (Str x1 s1) (Str x2 s2) = Str (x1 `bothExnStr` x2) (s1 `bothStr` s2)

bothExnStr :: ExnStr -> ExnStr -> ExnStr
bothExnStr ExnStr ExnStr = ExnStr
bothExnStr _      _      = VanStr

bothStr :: StrDmd -> StrDmd -> StrDmd
bothStr HyperStr _             = HyperStr
bothStr HeadStr s              = s
bothStr (SCall _)  HyperStr    = HyperStr
bothStr (SCall s1) HeadStr     = SCall s1
bothStr (SCall s1) (SCall s2)  = SCall (s1 `bothStr` s2)
bothStr (SCall _)  (SProd _)   = HyperStr  -- Weird

bothStr (SProd _)  HyperStr    = HyperStr
bothStr (SProd s1) HeadStr     = SProd s1
bothStr (SProd s1) (SProd s2)
    | s1 `equalLength` s2      = mkSProd (zipWith bothArgStr s1 s2)
    | otherwise                = HyperStr  -- Weird
bothStr (SProd _) (SCall _)    = HyperStr

-- utility functions to deal with memory leaks
seqStrDmd :: StrDmd -> ()
seqStrDmd (SProd ds)   = seqStrDmdList ds
seqStrDmd (SCall s)    = seqStrDmd s
seqStrDmd _            = ()

seqStrDmdList :: [ArgStr] -> ()
seqStrDmdList [] = ()
seqStrDmdList (d:ds) = seqArgStr d `seq` seqStrDmdList ds

seqArgStr :: ArgStr -> ()
seqArgStr Lazy      = ()
seqArgStr (Str x s) = x `seq` seqStrDmd s

-- Splitting polymorphic demands
splitArgStrProdDmd :: Int -> ArgStr -> Maybe [ArgStr]
splitArgStrProdDmd n Lazy      = Just (replicate n Lazy)
splitArgStrProdDmd n (Str _ s) = splitStrProdDmd n s

splitStrProdDmd :: Int -> StrDmd -> Maybe [ArgStr]
splitStrProdDmd n HyperStr   = Just (replicate n strBot)
splitStrProdDmd n HeadStr    = Just (replicate n strTop)
splitStrProdDmd n (SProd ds) = WARN( not (ds `lengthIs` n),
                                     text "splitStrProdDmd" $$ ppr n $$ ppr ds )
                               Just ds
splitStrProdDmd _ (SCall {}) = Nothing
      -- This can happen when the programmer uses unsafeCoerce,
      -- and we don't then want to crash the compiler (Trac #9208)

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

-- | Domain for genuine usage
data UseDmd
  = UCall Count UseDmd   -- ^ Call demand for absence.
                         -- Used only for values of function type

  | UProd [ArgUse]       -- ^ Product.
                         -- Used only for values of product type
                         -- See Note [Don't optimise UProd(Used) to Used]
                         --
                         -- Invariant: Not all components are Abs
                         -- (in that case, use UHead)

  | UHead                -- ^ May be used but its sub-components are
                         -- definitely *not* used.  For product types, UHead
                         -- is equivalent to U(AAA); see mkUProd.
                         --
                         -- UHead is needed only to express the demand
                         -- of 'seq' and 'case' which are polymorphic;
                         -- i.e. the scrutinised value is of type 'a'
                         -- rather than a product type. That's why we
                         -- can't use UProd [A,A,A]
                         --
                         -- Since (UCall _ Abs) is ill-typed, UHead doesn't
                         -- make sense for lambdas

  | Used                 -- ^ May be used and its sub-components may be used.
                         -- (top of the lattice)
  deriving ( Eq, Show )

-- Extended usage demand for absence and counting
type ArgUse = Use UseDmd

data Use u
  = Abs             -- Definitely unused
                    -- Bottom of the lattice

  | Use Count u     -- May be used with some cardinality
  deriving ( Eq, Show )

-- | Abstract counting of usages
data Count = One | Many
  deriving ( Eq, Show )

-- Pretty-printing
instance Outputable ArgUse where
  ppr Abs           = char 'A'
  ppr (Use Many a)   = ppr a
  ppr (Use One  a)   = char '1' <> char '*' <> ppr a

instance Outputable UseDmd where
  ppr Used           = char 'U'
  ppr (UCall c a)    = char 'C' <> ppr c <> parens (ppr a)
  ppr UHead          = char 'H'
  ppr (UProd as)     = char 'U' <> parens (hcat (punctuate (char ',') (map ppr as)))

instance Outputable Count where
  ppr One  = char '1'
  ppr Many = text ""

useBot, useTop :: ArgUse
useBot     = Abs
useTop     = Use Many Used

mkUCall :: Count -> UseDmd -> UseDmd
--mkUCall c Used = Used c
mkUCall c a  = UCall c a

mkUProd :: [ArgUse] -> UseDmd
mkUProd ux
  | all (== Abs) ux    = UHead
  | otherwise          = UProd ux

lubCount :: Count -> Count -> Count
lubCount _ Many = Many
lubCount Many _ = Many
lubCount x _    = x

lubArgUse :: ArgUse -> ArgUse -> ArgUse
lubArgUse Abs x                   = x
lubArgUse x Abs                   = x
lubArgUse (Use c1 a1) (Use c2 a2) = Use (lubCount c1 c2) (lubUse a1 a2)

lubUse :: UseDmd -> UseDmd -> UseDmd
lubUse UHead       u               = u
lubUse (UCall c u) UHead           = UCall c u
lubUse (UCall c1 u1) (UCall c2 u2) = UCall (lubCount c1 c2) (lubUse u1 u2)
lubUse (UCall _ _) _               = Used
lubUse (UProd ux) UHead            = UProd ux
lubUse (UProd ux1) (UProd ux2)
     | ux1 `equalLength` ux2       = UProd $ zipWith lubArgUse ux1 ux2
     | otherwise                   = Used
lubUse (UProd {}) (UCall {})       = Used
-- lubUse (UProd {}) Used             = Used
lubUse (UProd ux) Used             = UProd (map (`lubArgUse` useTop) ux)
lubUse Used       (UProd ux)       = UProd (map (`lubArgUse` useTop) ux)
lubUse Used _                      = Used  -- Note [Used should win]

-- `both` is different from `lub` in its treatment of counting; if
-- `both` is computed for two used, the result always has
--  cardinality `Many` (except for the inner demands of UCall demand -- [TODO] explain).
--  Also,  x `bothUse` x /= x (for anything but Abs).

bothArgUse :: ArgUse -> ArgUse -> ArgUse
bothArgUse Abs x                   = x
bothArgUse x Abs                   = x
bothArgUse (Use _ a1) (Use _ a2)   = Use Many (bothUse a1 a2)


bothUse :: UseDmd -> UseDmd -> UseDmd
bothUse UHead       u               = u
bothUse (UCall c u) UHead           = UCall c u

-- Exciting special treatment of inner demand for call demands:
--    use `lubUse` instead of `bothUse`!
bothUse (UCall _ u1) (UCall _ u2)   = UCall Many (u1 `lubUse` u2)

bothUse (UCall {}) _                = Used
bothUse (UProd ux) UHead            = UProd ux
bothUse (UProd ux1) (UProd ux2)
      | ux1 `equalLength` ux2       = UProd $ zipWith bothArgUse ux1 ux2
      | otherwise                   = Used
bothUse (UProd {}) (UCall {})       = Used
-- bothUse (UProd {}) Used             = Used  -- Note [Used should win]
bothUse Used (UProd ux)             = UProd (map (`bothArgUse` useTop) ux)
bothUse (UProd ux) Used             = UProd (map (`bothArgUse` useTop) ux)
bothUse Used _                      = Used  -- Note [Used should win]

peelUseCall :: UseDmd -> Maybe (Count, UseDmd)
peelUseCall (UCall c u)   = Just (c,u)
peelUseCall _             = Nothing

addCaseBndrDmd :: Demand    -- On the case binder
               -> [Demand]  -- On the components of the constructor
               -> [Demand]  -- Final demands for the components of the constructor
-- See Note [Demand on case-alternative binders]
addCaseBndrDmd (JD { sd = ms, ud = mu }) alt_dmds
  = case mu of
     Abs     -> alt_dmds
     Use _ u -> zipWith bothDmd alt_dmds (mkJointDmds ss us)
             where
                Just ss = splitArgStrProdDmd arity ms  -- Guaranteed not to be a call
                Just us = splitUseProdDmd      arity u   -- Ditto
  where
    arity = length alt_dmds

{- Note [Demand on case-alternative binders]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The demand on a binder in a case alternative comes
  (a) From the demand on the binder itself
  (b) From the demand on the case binder
Forgetting (b) led directly to Trac #10148.

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
that it is very much alive and demanded.  See Trac #10148 for how the
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
Used; intead leave it as-is. In effect we are using the UseDmd to do a
little bit of boxity analysis.  Not very nice.

Note [Used should win]
~~~~~~~~~~~~~~~~~~~~~~
Both in lubUse and bothUse we want (Used `both` UProd us) to be Used.
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
Compare with: (C) making Used win for both, but UProd win for lub

            Min          -0.1%     -0.3%     -7.9%     -8.0%     -6.5%
            Max          +0.1%     +1.0%    +21.0%    +21.0%     +0.5%
 Geometric Mean          +0.0%     +0.0%     -0.0%     -0.1%     -0.1%
-}

-- If a demand is used multiple times (i.e. reused), than any use-once
-- mentioned there, that is not protected by a UCall, can happen many times.
markReusedDmd :: ArgUse -> ArgUse
markReusedDmd Abs         = Abs
markReusedDmd (Use _ a)   = Use Many (markReused a)

markReused :: UseDmd -> UseDmd
markReused (UCall _ u)      = UCall Many u   -- No need to recurse here
markReused (UProd ux)       = UProd (map markReusedDmd ux)
markReused u                = u

isUsedMU :: ArgUse -> Bool
-- True <=> markReusedDmd d = d
isUsedMU Abs          = True
isUsedMU (Use One _)  = False
isUsedMU (Use Many u) = isUsedU u

isUsedU :: UseDmd -> Bool
-- True <=> markReused d = d
isUsedU Used           = True
isUsedU UHead          = True
isUsedU (UProd us)     = all isUsedMU us
isUsedU (UCall One _)  = False
isUsedU (UCall Many _) = True  -- No need to recurse

-- Squashing usage demand demands
seqUseDmd :: UseDmd -> ()
seqUseDmd (UProd ds)   = seqArgUseList ds
seqUseDmd (UCall c d)  = c `seq` seqUseDmd d
seqUseDmd _            = ()

seqArgUseList :: [ArgUse] -> ()
seqArgUseList []     = ()
seqArgUseList (d:ds) = seqArgUse d `seq` seqArgUseList ds

seqArgUse :: ArgUse -> ()
seqArgUse (Use c u)  = c `seq` seqUseDmd u
seqArgUse _          = ()

-- Splitting polymorphic Maybe-Used demands
splitUseProdDmd :: Int -> UseDmd -> Maybe [ArgUse]
splitUseProdDmd n Used        = Just (replicate n useTop)
splitUseProdDmd n UHead       = Just (replicate n Abs)
splitUseProdDmd n (UProd ds)  = WARN( not (ds `lengthIs` n),
                                      text "splitUseProdDmd" $$ ppr n
                                                             $$ ppr ds )
                                Just ds
splitUseProdDmd _ (UCall _ _) = Nothing
      -- This can happen when the programmer uses unsafeCoerce,
      -- and we don't then want to crash the compiler (Trac #9208)

useCount :: Use u -> Count
useCount Abs         = One
useCount (Use One _) = One
useCount _           = Many


{-
************************************************************************
*                                                                      *
         Clean demand for Strictness and Usage
*                                                                      *
************************************************************************

This domain differst from JointDemand in the sence that pure absence
is taken away, i.e., we deal *only* with non-absent demands.

Note [Strict demands]
~~~~~~~~~~~~~~~~~~~~~
isStrictDmd returns true only of demands that are
   both strict
   and  used
In particular, it is False for <HyperStr, Abs>, which can and does
arise in, say (Trac #7319)
   f x = raise# <some exception>
Then 'x' is not used, so f gets strictness <HyperStr,Abs> -> .
Now the w/w generates
   fx = let x <HyperStr,Abs> = absentError "unused"
        in raise <some exception>
At this point we really don't want to convert to
   fx = case absentError "unused" of x -> raise <some exception>
Since the program is going to diverge, this swaps one error for another,
but it's really a bad idea to *ever* evaluate an absent argument.
In Trac #7319 we get
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

type CleanDemand = JointDmd StrDmd UseDmd
     -- A demand that is at least head-strict

bothCleanDmd :: CleanDemand -> CleanDemand -> CleanDemand
bothCleanDmd (JD { sd = s1, ud = a1}) (JD { sd = s2, ud = a2})
  = JD { sd = s1 `bothStr` s2, ud = a1 `bothUse` a2 }

mkHeadStrict :: CleanDemand -> CleanDemand
mkHeadStrict cd = cd { sd = HeadStr }

mkOnceUsedDmd, mkManyUsedDmd :: CleanDemand -> Demand
mkOnceUsedDmd (JD {sd = s,ud = a}) = JD { sd = Str VanStr s, ud = Use One a }
mkManyUsedDmd (JD {sd = s,ud = a}) = JD { sd = Str VanStr s, ud = Use Many a }

evalDmd :: Demand
-- Evaluated strictly, and used arbitrarily deeply
evalDmd = JD { sd = Str VanStr HeadStr, ud = useTop }

mkProdDmd :: [Demand] -> CleanDemand
mkProdDmd dx
  = JD { sd = mkSProd $ map getStrDmd dx
       , ud = mkUProd $ map getUseDmd dx }

mkCallDmd :: CleanDemand -> CleanDemand
mkCallDmd (JD {sd = d, ud = u})
  = JD { sd = mkSCall d, ud = mkUCall One u }

-- See Note [Demand on the worker] in WorkWrap
mkWorkerDemand :: Int -> Demand
mkWorkerDemand n = JD { sd = Lazy, ud = Use One (go n) }
  where go 0 = Used
        go n = mkUCall One $ go (n-1)

cleanEvalDmd :: CleanDemand
cleanEvalDmd = JD { sd = HeadStr, ud = Used }

cleanEvalProdDmd :: Arity -> CleanDemand
cleanEvalProdDmd n = JD { sd = HeadStr, ud = UProd (replicate n useTop) }


{-
************************************************************************
*                                                                      *
           Demand: combining stricness and usage
*                                                                      *
************************************************************************
-}

type Demand = JointDmd ArgStr ArgUse

lubDmd :: Demand -> Demand -> Demand
lubDmd (JD {sd = s1, ud = a1}) (JD {sd = s2, ud = a2})
 = JD { sd = s1 `lubArgStr` s2
      , ud = a1 `lubArgUse` a2 }

bothDmd :: Demand -> Demand -> Demand
bothDmd (JD {sd = s1, ud = a1}) (JD {sd = s2, ud = a2})
 = JD { sd = s1 `bothArgStr` s2
      , ud = a1 `bothArgUse` a2 }

lazyApply1Dmd, lazyApply2Dmd, strictApply1Dmd, catchArgDmd :: Demand

strictApply1Dmd = JD { sd = Str VanStr (SCall HeadStr)
                     , ud = Use Many (UCall One Used) }

-- First argument of catchRetry# and catchSTM#:
--    uses its arg once, applies it once
--    and catches exceptions (the ExnStr) part
catchArgDmd = JD { sd = Str ExnStr (SCall HeadStr)
                 , ud = Use One (UCall One Used) }

lazyApply1Dmd = JD { sd = Lazy
                   , ud = Use One (UCall One Used) }

-- Second argument of catch#:
--    uses its arg at most once, applies it once
--    but is lazy (might not be called at all)
lazyApply2Dmd = JD { sd = Lazy
                   , ud = Use One (UCall One (UCall One Used)) }

absDmd :: Demand
absDmd = JD { sd = Lazy, ud = Abs }

topDmd :: Demand
topDmd = JD { sd = Lazy, ud = useTop }

botDmd :: Demand
botDmd = JD { sd = strBot, ud = useBot }

seqDmd :: Demand
seqDmd = JD { sd = Str VanStr HeadStr, ud = Use One UHead }

oneifyDmd :: JointDmd s (Use u) -> JointDmd s (Use u)
oneifyDmd (JD { sd = s, ud = Use _ a }) = JD { sd = s, ud = Use One a }
oneifyDmd jd                            = jd

isTopDmd :: Demand -> Bool
-- Used to suppress pretty-printing of an uninformative demand
isTopDmd (JD {sd = Lazy, ud = Use Many Used}) = True
isTopDmd _                                    = False

isAbsDmd :: JointDmd (Str s) (Use u) -> Bool
isAbsDmd (JD {ud = Abs}) = True   -- The strictness part can be HyperStr
isAbsDmd _               = False  -- for a bottom demand

isSeqDmd :: Demand -> Bool
isSeqDmd (JD {sd = Str VanStr HeadStr, ud = Use _ UHead}) = True
isSeqDmd _                                                = False

isUsedOnce :: JointDmd (Str s) (Use u) -> Bool
isUsedOnce (JD { ud = a }) = case useCount a of
                               One  -> True
                               Many -> False

-- More utility functions for strictness
seqDemand :: Demand -> ()
seqDemand (JD {sd = s, ud = u}) = seqArgStr s `seq` seqArgUse u

seqDemandList :: [Demand] -> ()
seqDemandList [] = ()
seqDemandList (d:ds) = seqDemand d `seq` seqDemandList ds

isStrictDmd :: JointDmd (Str s) (Use u) -> Bool
-- See Note [Strict demands]
isStrictDmd (JD {ud = Abs})  = False
isStrictDmd (JD {sd = Lazy}) = False
isStrictDmd _                = True

isWeakDmd :: Demand -> Bool
isWeakDmd (JD {sd = s, ud = a}) = isLazy s && isUsedMU a

cleanUseDmd_maybe :: Demand -> Maybe UseDmd
cleanUseDmd_maybe (JD { ud = Use _ u }) = Just u
cleanUseDmd_maybe _                     = Nothing

splitFVs :: Bool   -- Thunk
         -> DmdEnv -> (DmdEnv, DmdEnv)
splitFVs is_thunk rhs_fvs
  | is_thunk  = nonDetFoldUFM_Directly add (emptyVarEnv, emptyVarEnv) rhs_fvs
                -- It's OK to use nonDetFoldUFM_Directly because we
                -- immediately forget the ordering by putting the elements
                -- in the envs again
  | otherwise = partitionVarEnv isWeakDmd rhs_fvs
  where
    add uniq dmd@(JD { sd = s, ud = u }) (lazy_fv, sig_fv)
      | Lazy <- s = (addToUFM_Directly lazy_fv uniq dmd, sig_fv)
      | otherwise = ( addToUFM_Directly lazy_fv uniq (JD { sd = Lazy, ud = u })
                    , addToUFM_Directly sig_fv  uniq (JD { sd = s,    ud = Abs }) )

data TypeShape = TsFun TypeShape
               | TsProd [TypeShape]
               | TsUnk

instance Outputable TypeShape where
  ppr TsUnk        = text "TsUnk"
  ppr (TsFun ts)   = text "TsFun" <> parens (ppr ts)
  ppr (TsProd tss) = parens (hsep $ punctuate comma $ map ppr tss)

trimToType :: Demand -> TypeShape -> Demand
-- See Note [Trimming a demand to a type]
trimToType (JD { sd = ms, ud = mu }) ts
  = JD (go_ms ms ts) (go_mu mu ts)
  where
    go_ms :: ArgStr -> TypeShape -> ArgStr
    go_ms Lazy      _  = Lazy
    go_ms (Str x s) ts = Str x (go_s s ts)

    go_s :: StrDmd -> TypeShape -> StrDmd
    go_s HyperStr    _            = HyperStr
    go_s (SCall s)   (TsFun ts)   = SCall (go_s s ts)
    go_s (SProd mss) (TsProd tss)
      | equalLength mss tss       = SProd (zipWith go_ms mss tss)
    go_s _           _            = HeadStr

    go_mu :: ArgUse -> TypeShape -> ArgUse
    go_mu Abs _ = Abs
    go_mu (Use c u) ts = Use c (go_u u ts)

    go_u :: UseDmd -> TypeShape -> UseDmd
    go_u UHead       _          = UHead
    go_u (UCall c u) (TsFun ts) = UCall c (go_u u ts)
    go_u (UProd mus) (TsProd tss)
      | equalLength mus tss      = UProd (zipWith go_mu mus tss)
    go_u _           _           = Used

{-
Note [Trimming a demand to a type]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider this:

  f :: a -> Bool
  f x = case ... of
          A g1 -> case (x |> g1) of (p,q) -> ...
          B    -> error "urk"

where A,B are the constructors of a GADT.  We'll get a U(U,U) demand
on x from the A branch, but that's a stupid demand for x itself, which
has type 'a'. Indeed we get ASSERTs going off (notably in
splitUseProdDmd, Trac #8569).

Bottom line: we really don't want to have a binder whose demand is more
deeply-nested than its type.  There are various ways to tackle this.
When processing (x |> g1), we could "trim" the incoming demand U(U,U)
to match x's type.  But I'm currently doing so just at the moment when
we pin a demand on a binder, in DmdAnal.findBndrDmd.


Note [Threshold demands]
~~~~~~~~~~~~~~~~~~~~~~~~
Threshold usage demand is generated to figure out if
cardinality-instrumented demands of a binding's free variables should
be unleashed. See also [Aggregated demand for cardinality].

Note [Replicating polymorphic demands]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Some demands can be considered as polymorphic. Generally, it is
applicable to such beasts as tops, bottoms as well as Head-Used and
Head-stricts demands. For instance,

S ~ S(L, ..., L)

Also, when top or bottom is occurred as a result demand, it in fact
can be expanded to saturate a callee's arity.
-}

splitProdDmd_maybe :: Demand -> Maybe [Demand]
-- Split a product into its components, iff there is any
-- useful information to be extracted thereby
-- The demand is not necessarily strict!
splitProdDmd_maybe (JD { sd = s, ud = u })
  = case (s,u) of
      (Str _ (SProd sx), Use _ u) | Just ux <- splitUseProdDmd (length sx) u
                                  -> Just (mkJointDmds sx ux)
      (Str _ s, Use _ (UProd ux)) | Just sx <- splitStrProdDmd (length ux) s
                                  -> Just (mkJointDmds sx ux)
      (Lazy,    Use _ (UProd ux)) -> Just (mkJointDmds (replicate (length ux) Lazy) ux)
      _ -> Nothing

{-
************************************************************************
*                                                                      *
                   Demand results
*                                                                      *
************************************************************************


DmdResult:     Dunno CPRResult
               /
           ThrowsExn
             /
        Diverges


CPRResult:         NoCPR
                   /    \
            RetProd    RetSum ConTag


Product constructors return (Dunno (RetProd rs))
In a fixpoint iteration, start from Diverges
We have lubs, but not glbs; but that is ok.
-}

------------------------------------------------------------------------
-- Constructed Product Result
------------------------------------------------------------------------

data Termination r
  = Diverges    -- Definitely diverges
  | ThrowsExn   -- Definitely throws an exception or diverges
  | Dunno r     -- Might diverge or converge
  deriving( Eq, Show )

type DmdResult = Termination CPRResult

data CPRResult = NoCPR          -- Top of the lattice
               | RetProd        -- Returns a constructor from a product type
               | RetSum ConTag  -- Returns a constructor from a data type
               deriving( Eq, Show )

lubCPR :: CPRResult -> CPRResult -> CPRResult
lubCPR (RetSum t1) (RetSum t2)
  | t1 == t2                       = RetSum t1
lubCPR RetProd     RetProd     = RetProd
lubCPR _ _                     = NoCPR

lubDmdResult :: DmdResult -> DmdResult -> DmdResult
lubDmdResult Diverges       r              = r
lubDmdResult ThrowsExn      Diverges       = ThrowsExn
lubDmdResult ThrowsExn      r              = r
lubDmdResult (Dunno c1)     Diverges       = Dunno c1
lubDmdResult (Dunno c1)     ThrowsExn      = Dunno c1
lubDmdResult (Dunno c1)     (Dunno c2)     = Dunno (c1 `lubCPR` c2)
-- This needs to commute with defaultDmd, i.e.
-- defaultDmd (r1 `lubDmdResult` r2) = defaultDmd r1 `lubDmd` defaultDmd r2
-- (See Note [Default demand on free variables] for why)

bothDmdResult :: DmdResult -> Termination () -> DmdResult
-- See Note [Asymmetry of 'both' for DmdType and DmdResult]
bothDmdResult _ Diverges   = Diverges
bothDmdResult r ThrowsExn  = case r of { Diverges -> r; _ -> ThrowsExn }
bothDmdResult r (Dunno {}) = r
-- This needs to commute with defaultDmd, i.e.
-- defaultDmd (r1 `bothDmdResult` r2) = defaultDmd r1 `bothDmd` defaultDmd r2
-- (See Note [Default demand on free variables] for why)

instance Outputable r => Outputable (Termination r) where
  ppr Diverges      = char 'b'
  ppr ThrowsExn     = char 'x'
  ppr (Dunno c)     = ppr c

instance Outputable CPRResult where
  ppr NoCPR        = empty
  ppr (RetSum n)   = char 'm' <> int n
  ppr RetProd      = char 'm'

seqDmdResult :: DmdResult -> ()
seqDmdResult Diverges  = ()
seqDmdResult ThrowsExn = ()
seqDmdResult (Dunno c) = seqCPRResult c

seqCPRResult :: CPRResult -> ()
seqCPRResult NoCPR        = ()
seqCPRResult (RetSum n)   = n `seq` ()
seqCPRResult RetProd      = ()


------------------------------------------------------------------------
-- Combined demand result                                             --
------------------------------------------------------------------------

-- [cprRes] lets us switch off CPR analysis
-- by making sure that everything uses TopRes
topRes, exnRes, botRes :: DmdResult
topRes = Dunno NoCPR
exnRes = ThrowsExn
botRes = Diverges

cprSumRes :: ConTag -> DmdResult
cprSumRes tag = Dunno $ RetSum tag

cprProdRes :: [DmdType] -> DmdResult
cprProdRes _arg_tys = Dunno $ RetProd

vanillaCprProdRes :: Arity -> DmdResult
vanillaCprProdRes _arity = Dunno $ RetProd

isTopRes :: DmdResult -> Bool
isTopRes (Dunno NoCPR) = True
isTopRes _             = False

isBotRes :: DmdResult -> Bool
-- True if the result diverges or throws an exception
isBotRes Diverges   = True
isBotRes ThrowsExn  = True
isBotRes (Dunno {}) = False

trimCPRInfo :: Bool -> Bool -> DmdResult -> DmdResult
trimCPRInfo trim_all trim_sums res
  = trimR res
  where
    trimR (Dunno c) = Dunno (trimC c)
    trimR res       = res

    trimC (RetSum n)   | trim_all || trim_sums = NoCPR
                       | otherwise             = RetSum n
    trimC RetProd      | trim_all  = NoCPR
                       | otherwise = RetProd
    trimC NoCPR = NoCPR

returnsCPR_maybe :: DmdResult -> Maybe ConTag
returnsCPR_maybe (Dunno c) = retCPR_maybe c
returnsCPR_maybe _         = Nothing

retCPR_maybe :: CPRResult -> Maybe ConTag
retCPR_maybe (RetSum t)  = Just t
retCPR_maybe RetProd     = Just fIRST_TAG
retCPR_maybe NoCPR       = Nothing

-- See Notes [Default demand on free variables]
-- and [defaultDmd vs. resTypeArgDmd]
defaultDmd :: Termination r -> Demand
defaultDmd (Dunno {}) = absDmd
defaultDmd _          = botDmd  -- Diverges or ThrowsExn

resTypeArgDmd :: Termination r -> Demand
-- TopRes and BotRes are polymorphic, so that
--      BotRes === (Bot -> BotRes) === ...
--      TopRes === (Top -> TopRes) === ...
-- This function makes that concrete
-- Also see Note [defaultDmd vs. resTypeArgDmd]
resTypeArgDmd (Dunno _) = topDmd
resTypeArgDmd _         = botDmd   -- Diverges or ThrowsExn

{-
Note [defaultDmd and resTypeArgDmd]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

These functions are similar: They express the demand on something not
explicitly mentioned in the environment resp. the argument list. Yet they are
different:
 * Variables not mentioned in the free variables environment are definitely
   unused, so we can use absDmd there.
 * Further arguments *can* be used, of course. Hence topDmd is used.


************************************************************************
*                                                                      *
           Demand environments and types
*                                                                      *
************************************************************************
-}

type DmdEnv = VarEnv Demand   -- See Note [Default demand on free variables]

data DmdType = DmdType
                  DmdEnv        -- Demand on explicitly-mentioned
                                --      free variables
                  [Demand]      -- Demand on arguments
                  DmdResult     -- See [Nature of result demand]

{-
Note [Nature of result demand]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A DmdResult contains information about termination (currently distinguishing
definite divergence and no information; it is possible to include definite
convergence here), and CPR information about the result.

The semantics of this depends on whether we are looking at a DmdType, i.e. the
demand put on by an expression _under a specific incoming demand_ on its
environment, or at a StrictSig describing a demand transformer.

For a
 * DmdType, the termination information is true given the demand it was
   generated with, while for
 * a StrictSig it holds after applying enough arguments.

The CPR information, though, is valid after the number of arguments mentioned
in the type is given. Therefore, when forgetting the demand on arguments, as in
dmdAnalRhs, this needs to be considere (via removeDmdTyArgs).

Consider
  b2 x y = x `seq` y `seq` error (show x)
this has a strictness signature of
  <S><S>b
meaning that "b2 `seq` ()" and "b2 1 `seq` ()" might well terminate, but
for "b2 1 2 `seq` ()" we get definite divergence.

For comparison,
  b1 x = x `seq` error (show x)
has a strictness signature of
  <S>b
and "b1 1 `seq` ()" is known to terminate.

Now consider a function h with signature "<C(S)>", and the expression
  e1 = h b1
now h puts a demand of <C(S)> onto its argument, and the demand transformer
turns it into
  <S>b
Now the DmdResult "b" does apply to us, even though "b1 `seq` ()" does not
diverge, and we do not anything being passed to b.

Note [Asymmetry of 'both' for DmdType and DmdResult]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
'both' for DmdTypes is *asymmetrical*, because there is only one
result!  For example, given (e1 e2), we get a DmdType dt1 for e1, use
its arg demand to analyse e2 giving dt2, and then do (dt1 `bothType` dt2).
Similarly with
  case e of { p -> rhs }
we get dt_scrut from the scrutinee and dt_rhs from the RHS, and then
compute (dt_rhs `bothType` dt_scrut).

We
 1. combine the information on the free variables,
 2. take the demand on arguments from the first argument
 3. combine the termination results, but
 4. take CPR info from the first argument.

3 and 4 are implementd in bothDmdResult.
-}

-- Equality needed for fixpoints in DmdAnal
instance Eq DmdType where
  (==) (DmdType fv1 ds1 res1)
       (DmdType fv2 ds2 res2) = nonDetUFMToList fv1 == nonDetUFMToList fv2
         -- It's OK to use nonDetUFMToList here because we're testing for
         -- equality and even though the lists will be in some arbitrary
         -- Unique order, it is the same order for both
                              && ds1 == ds2 && res1 == res2

lubDmdType :: DmdType -> DmdType -> DmdType
lubDmdType d1 d2
  = DmdType lub_fv lub_ds lub_res
  where
    n = max (dmdTypeDepth d1) (dmdTypeDepth d2)
    (DmdType fv1 ds1 r1) = ensureArgs n d1
    (DmdType fv2 ds2 r2) = ensureArgs n d2

    lub_fv  = plusVarEnv_CD lubDmd fv1 (defaultDmd r1) fv2 (defaultDmd r2)
    lub_ds  = zipWithEqual "lubDmdType" lubDmd ds1 ds2
    lub_res = lubDmdResult r1 r2

{-
Note [The need for BothDmdArg]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Previously, the right argument to bothDmdType, as well as the return value of
dmdAnalStar via postProcessDmdType, was a DmdType. But bothDmdType only needs
to know about the free variables and termination information, but nothing about
the demand put on arguments, nor cpr information. So we make that explicit by
only passing the relevant information.
-}

type BothDmdArg = (DmdEnv, Termination ())

mkBothDmdArg :: DmdEnv -> BothDmdArg
mkBothDmdArg env = (env, Dunno ())

toBothDmdArg :: DmdType -> BothDmdArg
toBothDmdArg (DmdType fv _ r) = (fv, go r)
  where
    go (Dunno {}) = Dunno ()
    go ThrowsExn  = ThrowsExn
    go Diverges   = Diverges

bothDmdType :: DmdType -> BothDmdArg -> DmdType
bothDmdType (DmdType fv1 ds1 r1) (fv2, t2)
    -- See Note [Asymmetry of 'both' for DmdType and DmdResult]
    -- 'both' takes the argument/result info from its *first* arg,
    -- using its second arg just for its free-var info.
  = DmdType (plusVarEnv_CD bothDmd fv1 (defaultDmd r1) fv2 (defaultDmd t2))
            ds1
            (r1 `bothDmdResult` t2)

instance Outputable DmdType where
  ppr (DmdType fv ds res)
    = hsep [hcat (map ppr ds) <> ppr res,
            if null fv_elts then empty
            else braces (fsep (map pp_elt fv_elts))]
    where
      pp_elt (uniq, dmd) = ppr uniq <> text "->" <> ppr dmd
      fv_elts = nonDetUFMToList fv
        -- It's OK to use nonDetUFMToList here because we only do it for
        -- pretty printing

emptyDmdEnv :: VarEnv Demand
emptyDmdEnv = emptyVarEnv

-- nopDmdType is the demand of doing nothing
-- (lazy, absent, no CPR information, no termination information).
-- Note that it is ''not'' the top of the lattice (which would be "may use everything"),
-- so it is (no longer) called topDmd
nopDmdType, botDmdType, exnDmdType :: DmdType
nopDmdType = DmdType emptyDmdEnv [] topRes
botDmdType = DmdType emptyDmdEnv [] botRes
exnDmdType = DmdType emptyDmdEnv [] exnRes

cprProdDmdType :: Arity -> DmdType
cprProdDmdType arity
  = DmdType emptyDmdEnv [] (vanillaCprProdRes arity)

isTopDmdType :: DmdType -> Bool
isTopDmdType (DmdType env [] res)
  | isTopRes res && isEmptyVarEnv env = True
isTopDmdType _                        = False

mkDmdType :: DmdEnv -> [Demand] -> DmdResult -> DmdType
mkDmdType fv ds res = DmdType fv ds res

dmdTypeDepth :: DmdType -> Arity
dmdTypeDepth (DmdType _ ds _) = length ds

-- Remove any demand on arguments. This is used in dmdAnalRhs on the body
removeDmdTyArgs :: DmdType -> DmdType
removeDmdTyArgs = ensureArgs 0

-- This makes sure we can use the demand type with n arguments,
-- It extends the argument list with the correct resTypeArgDmd
-- It also adjusts the DmdResult: Divergence survives additional arguments,
-- CPR information does not (and definite converge also would not).
ensureArgs :: Arity -> DmdType -> DmdType
ensureArgs n d | n == depth = d
               | otherwise  = DmdType fv ds' r'
  where depth = dmdTypeDepth d
        DmdType fv ds r = d

        ds' = take n (ds ++ repeat (resTypeArgDmd r))
        r' = case r of    -- See [Nature of result demand]
              Dunno _ -> topRes
              _       -> r


seqDmdType :: DmdType -> ()
seqDmdType (DmdType env ds res) =
  seqDmdEnv env `seq` seqDemandList ds `seq` seqDmdResult res `seq` ()

seqDmdEnv :: DmdEnv -> ()
seqDmdEnv env = seqEltsUFM seqDemandList env

splitDmdTy :: DmdType -> (Demand, DmdType)
-- Split off one function argument
-- We already have a suitable demand on all
-- free vars, so no need to add more!
splitDmdTy (DmdType fv (dmd:dmds) res_ty) = (dmd, DmdType fv dmds res_ty)
splitDmdTy ty@(DmdType _ [] res_ty)       = (resTypeArgDmd res_ty, ty)

-- When e is evaluated after executing an IO action, and d is e's demand, then
-- what of this demand should we consider, given that the IO action can cleanly
-- exit?
-- * We have to kill all strictness demands (i.e. lub with a lazy demand)
-- * We can keep usage information (i.e. lub with an absent demand)
-- * We have to kill definite divergence
-- * We can keep CPR information.
-- See Note [IO hack in the demand analyser] in DmdAnal
deferAfterIO :: DmdType -> DmdType
deferAfterIO d@(DmdType _ _ res) =
    case d `lubDmdType` nopDmdType of
        DmdType fv ds _ -> DmdType fv ds (defer_res res)
  where
  defer_res r@(Dunno {}) = r
  defer_res _            = topRes  -- Diverges and ThrowsExn

strictenDmd :: Demand -> CleanDemand
strictenDmd (JD { sd = s, ud = u})
  = JD { sd = poke_s s, ud = poke_u u }
  where
    poke_s Lazy      = HeadStr
    poke_s (Str _ s) = s
    poke_u Abs       = UHead
    poke_u (Use _ u) = u

-- Deferring and peeling

type DmdShell   -- Describes the "outer shell"
                -- of a Demand
   = JointDmd (Str ()) (Use ())

toCleanDmd :: Demand -> (DmdShell, CleanDemand)
-- Splits a Demand into its "shell" and the inner "clean demand"
toCleanDmd (JD { sd = s, ud = u })
  = (JD { sd = ss, ud = us }, JD { sd = s', ud = u' })
    -- See Note [Analyzing with lazy demand and lambdas]
    -- See Note [Analysing with absent demand]
  where
    (ss, s') = case s of
                Str x s' -> (Str x (), s')
                Lazy     -> (Lazy,     HeadStr)

    (us, u') = case u of
                 Use c u' -> (Use c (), u')
                 Abs      -> (Abs,      Used)

-- This is used in dmdAnalStar when post-processing
-- a function's argument demand. So we only care about what
-- does to free variables, and whether it terminates.
-- see Note [The need for BothDmdArg]
postProcessDmdType :: DmdShell -> DmdType -> BothDmdArg
postProcessDmdType du@(JD { sd = ss }) (DmdType fv _ res_ty)
    = (postProcessDmdEnv du fv, term_info)
    where
       term_info = case postProcessDmdResult ss res_ty of
                     Dunno _   -> Dunno ()
                     ThrowsExn -> ThrowsExn
                     Diverges  -> Diverges

postProcessDmdResult :: Str () -> DmdResult -> DmdResult
postProcessDmdResult Lazy           _         = topRes
postProcessDmdResult (Str ExnStr _) ThrowsExn = topRes  -- Key point!
-- Note that only ThrowsExn results can be caught, not Diverges
postProcessDmdResult _              res       = res

postProcessDmdEnv :: DmdShell -> DmdEnv -> DmdEnv
postProcessDmdEnv ds@(JD { sd = ss, ud = us }) env
  | Abs <- us       = emptyDmdEnv
    -- In this case (postProcessDmd ds) == id; avoid a redundant rebuild
    -- of the environment. Be careful, bad things will happen if this doesn't
    -- match postProcessDmd (see #13977).
  | Str VanStr _ <- ss
  , Use One _ <- us = env
  | otherwise       = mapVarEnv (postProcessDmd ds) env
  -- For the Absent case just discard all usage information
  -- We only processed the thing at all to analyse the body
  -- See Note [Always analyse in virgin pass]

reuseEnv :: DmdEnv -> DmdEnv
reuseEnv = mapVarEnv (postProcessDmd
                        (JD { sd = Str VanStr (), ud = Use Many () }))

postProcessUnsat :: DmdShell -> DmdType -> DmdType
postProcessUnsat ds@(JD { sd = ss }) (DmdType fv args res_ty)
  = DmdType (postProcessDmdEnv ds fv)
            (map (postProcessDmd ds) args)
            (postProcessDmdResult ss res_ty)

postProcessDmd :: DmdShell -> Demand -> Demand
postProcessDmd (JD { sd = ss, ud = us }) (JD { sd = s, ud = a})
  = JD { sd = s', ud = a' }
  where
    s' = case ss of
           Lazy         -> Lazy
           Str ExnStr _ -> markExnStr s
           Str VanStr _ -> s
    a' = case us of
           Abs        -> Abs
           Use Many _ -> markReusedDmd a
           Use One  _ -> a

markExnStr :: ArgStr -> ArgStr
markExnStr (Str VanStr s) = Str ExnStr s
markExnStr s              = s

-- Peels one call level from the demand, and also returns
-- whether it was unsaturated (separately for strictness and usage)
peelCallDmd :: CleanDemand -> (CleanDemand, DmdShell)
-- Exploiting the fact that
-- on the strictness side      C(B) = B
-- and on the usage side       C(U) = U
peelCallDmd (JD {sd = s, ud = u})
  = (JD { sd = s', ud = u' }, JD { sd = ss, ud = us })
  where
    (s', ss) = case s of
                 SCall s' -> (s',       Str VanStr ())
                 HyperStr -> (HyperStr, Str VanStr ())
                 _        -> (HeadStr,  Lazy)
    (u', us) = case u of
                 UCall c u' -> (u',   Use c    ())
                 _          -> (Used, Use Many ())
       -- The _ cases for usage includes UHead which seems a bit wrong
       -- because the body isn't used at all!
       -- c.f. the Abs case in toCleanDmd

-- Peels that multiple nestings of calls clean demand and also returns
-- whether it was unsaturated (separately for strictness and usage
-- see Note [Demands from unsaturated function calls]
peelManyCalls :: Int -> CleanDemand -> DmdShell
peelManyCalls n (JD { sd = str, ud = abs })
  = JD { sd = go_str n str, ud = go_abs n abs }
  where
    go_str :: Int -> StrDmd -> Str ()  -- True <=> unsaturated, defer
    go_str 0 _          = Str VanStr ()
    go_str _ HyperStr   = Str VanStr () -- == go_str (n-1) HyperStr, as HyperStr = Call(HyperStr)
    go_str n (SCall d') = go_str (n-1) d'
    go_str _ _          = Lazy

    go_abs :: Int -> UseDmd -> Use ()      -- Many <=> unsaturated, or at least
    go_abs 0 _              = Use One ()   --          one UCall Many in the demand
    go_abs n (UCall One d') = go_abs (n-1) d'
    go_abs _ _              = Use Many ()

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
   - Furthermore, we remove CPR information. It could be left, but given the incoming
     demand is not enough to evaluate so far we just do not bother.
   - And finally termination information: If r says that f diverges for sure,
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
cases, and then call postProcessUnsat to reduce the demand appropriately.

Similarly, dmdTransformDictSelSig and dmdAnal, when analyzing a Lambda, use
peelCallDmd, which peels only one level, but also returns the demand put on the
body of the function.
-}

peelFV :: DmdType -> Var -> (DmdType, Demand)
peelFV (DmdType fv ds res) id = -- pprTrace "rfv" (ppr id <+> ppr dmd $$ ppr fv)
                               (DmdType fv' ds res, dmd)
  where
  fv' = fv `delVarEnv` id
  -- See Note [Default demand on free variables]
  dmd  = lookupVarEnv fv id `orElse` defaultDmd res

addDemand :: Demand -> DmdType -> DmdType
addDemand dmd (DmdType fv ds res) = DmdType fv (dmd:ds) res

findIdDemand :: DmdType -> Var -> Demand
findIdDemand (DmdType fv _ res) id
  = lookupVarEnv fv id `orElse` defaultDmd res

{-
Note [Default demand on free variables]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If the variable is not mentioned in the environment of a demand type,
its demand is taken to be a result demand of the type.
    For the stricness component,
     if the result demand is a Diverges, then we use HyperStr
                                         else we use Lazy
    For the usage component, we use Absent.
So we use either absDmd or botDmd.

Also note the equations for lubDmdResult (resp. bothDmdResult) noted there.

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
strict S by the second clause of the analysis.

Note [Analysing with absent demand]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose we analyse an expression with demand <L,A>.  The "A" means
"absent", so this expression will never be needed.  What should happen?
There are several wrinkles:

* We *do* want to analyse the expression regardless.
  Reason: Note [Always analyse in virgin pass]

  But we can post-process the results to ignore all the usage
  demands coming back. This is done by postProcessDmdType.

* In a previous incarnation of GHC we needed to be extra careful in the
  case of an *unlifted type*, because unlifted values are evaluated
  even if they are not used.  Example (see Trac #9254):
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


************************************************************************
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
Also see Note [Nature of result demand] for the meaning of a DmdResult in a
strictness signature.

If an Id is applied to less arguments than its arity, it means that
the demand on the function at a call site is weaker than the vanilla
call demand, used for signature inference. Therefore we place a top
demand on all arguments. Otherwise, the demand is specified by Id's
signature.

For example, the demand transformer described by the demand signature
        StrictSig (DmdType {x -> <S,1*U>} <L,A><L,U(U,U)>m)
says that when the function is applied to two arguments, it
unleashes demand <S,1*U> on the free var x, <L,A> on the first arg,
and <L,U(U,U)> on the second, then returning a constructor.

If this same function is applied to one arg, all we can say is that it
uses x with <L,U>, and its arg with demand <L,U>.
-}

newtype StrictSig = StrictSig DmdType
                  deriving( Eq )

instance Outputable StrictSig where
   ppr (StrictSig ty) = ppr ty

-- Used for printing top-level strictness pragmas in interface files
pprIfaceStrictSig :: StrictSig -> SDoc
pprIfaceStrictSig (StrictSig (DmdType _ dmds res))
  = hcat (map ppr dmds) <> ppr res

mkStrictSig :: DmdType -> StrictSig
mkStrictSig dmd_ty = StrictSig dmd_ty

mkClosedStrictSig :: [Demand] -> DmdResult -> StrictSig
mkClosedStrictSig ds res = mkStrictSig (DmdType emptyDmdEnv ds res)

splitStrictSig :: StrictSig -> ([Demand], DmdResult)
splitStrictSig (StrictSig (DmdType _ dmds res)) = (dmds, res)

increaseStrictSigArity :: Int -> StrictSig -> StrictSig
-- Add extra arguments to a strictness signature
increaseStrictSigArity arity_increase sig@(StrictSig dmd_ty@(DmdType env dmds res))
  | isTopDmdType dmd_ty = sig
  | arity_increase <= 0 = sig
  | otherwise           = StrictSig (DmdType env dmds' res)
  where
    dmds' = replicate arity_increase topDmd ++ dmds

etaExpandStrictSig :: Arity -> StrictSig -> StrictSig
-- We are expanding (\x y. e) to (\x y z. e z)
-- Add exta demands to the /end/ of the arg demands if necessary
etaExpandStrictSig arity sig@(StrictSig dmd_ty@(DmdType env dmds res))
  | isTopDmdType dmd_ty = sig
  | arity_increase <= 0 = sig
  | otherwise           = StrictSig (DmdType env dmds' res)
  where
    arity_increase = arity - length dmds
    dmds' = dmds ++ replicate arity_increase topDmd

isTopSig :: StrictSig -> Bool
isTopSig (StrictSig ty) = isTopDmdType ty

hasDemandEnvSig :: StrictSig -> Bool
hasDemandEnvSig (StrictSig (DmdType env _ _)) = not (isEmptyVarEnv env)

strictSigDmdEnv :: StrictSig -> DmdEnv
strictSigDmdEnv (StrictSig (DmdType env _ _)) = env

isBottomingSig :: StrictSig -> Bool
-- True if the signature diverges or throws an exception
isBottomingSig (StrictSig (DmdType _ _ res)) = isBotRes res

nopSig, botSig, exnSig :: StrictSig
nopSig = StrictSig nopDmdType
botSig = StrictSig botDmdType
exnSig = StrictSig exnDmdType

cprProdSig :: Arity -> StrictSig
cprProdSig arity = StrictSig (cprProdDmdType arity)

seqStrictSig :: StrictSig -> ()
seqStrictSig (StrictSig ty) = seqDmdType ty

dmdTransformSig :: StrictSig -> CleanDemand -> DmdType
-- (dmdTransformSig fun_sig dmd) considers a call to a function whose
-- signature is fun_sig, with demand dmd.  We return the demand
-- that the function places on its context (eg its args)
dmdTransformSig (StrictSig dmd_ty@(DmdType _ arg_ds _)) cd
  = postProcessUnsat (peelManyCalls (length arg_ds) cd) dmd_ty
    -- see Note [Demands from unsaturated function calls]

dmdTransformDataConSig :: Arity -> StrictSig -> CleanDemand -> DmdType
-- Same as dmdTransformSig but for a data constructor (worker),
-- which has a special kind of demand transformer.
-- If the constructor is saturated, we feed the demand on
-- the result into the constructor arguments.
dmdTransformDataConSig arity (StrictSig (DmdType _ _ con_res))
                             (JD { sd = str, ud = abs })
  | Just str_dmds <- go_str arity str
  , Just abs_dmds <- go_abs arity abs
  = DmdType emptyDmdEnv (mkJointDmds str_dmds abs_dmds) con_res
                -- Must remember whether it's a product, hence con_res, not TopRes

  | otherwise   -- Not saturated
  = nopDmdType
  where
    go_str 0 dmd        = splitStrProdDmd arity dmd
    go_str n (SCall s') = go_str (n-1) s'
    go_str n HyperStr   = go_str (n-1) HyperStr
    go_str _ _          = Nothing

    go_abs 0 dmd            = splitUseProdDmd arity dmd
    go_abs n (UCall One u') = go_abs (n-1) u'
    go_abs _ _              = Nothing

dmdTransformDictSelSig :: StrictSig -> CleanDemand -> DmdType
-- Like dmdTransformDataConSig, we have a special demand transformer
-- for dictionary selectors.  If the selector is saturated (ie has one
-- argument: the dictionary), we feed the demand on the result into
-- the indicated dictionary component.
dmdTransformDictSelSig (StrictSig (DmdType _ [dict_dmd] _)) cd
   | (cd',defer_use) <- peelCallDmd cd
   , Just jds <- splitProdDmd_maybe dict_dmd
   = postProcessUnsat defer_use $
     DmdType emptyDmdEnv [mkOnceUsedDmd $ mkProdDmd $ map (enhance cd') jds] topRes
   | otherwise
   = nopDmdType              -- See Note [Demand transformer for a dictionary selector]
  where
    enhance cd old | isAbsDmd old = old
                   | otherwise    = mkOnceUsedDmd cd  -- This is the one!

dmdTransformDictSelSig _ _ = panic "dmdTransformDictSelSig: no args"

{-
Note [Demand transformer for a dictionary selector]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If we evaluate (op dict-expr) under demand 'd', then we can push the demand 'd'
into the appropriate field of the dictionary. What *is* the appropriate field?
We just look at the strictness signature of the class op, which will be
something like: U(AAASAAAAA).  Then replace the 'S' by the demand 'd'.

For single-method classes, which are represented by newtypes the signature
of 'op' won't look like U(...), so the splitProdDmd_maybe will fail.
That's fine: if we are doing strictness analysis we are also doing inlining,
so we'll have inlined 'op' into a cast.  So we can bale out in a conservative
way, returning nopDmdType.

It is (just.. Trac #8329) possible to be running strictness analysis *without*
having inlined class ops from single-method classes.  Suppose you are using
ghc --make; and the first module has a local -O0 flag.  So you may load a class
without interface pragmas, ie (currently) without an unfolding for the class
ops.   Now if a subsequent module in the --make sweep has a local -O flag
you might do strictness analysis, but there is no inlining for the class op.
This is weird, so I'm not worried about whether this optimises brilliantly; but
it should not fall over.
-}

argsOneShots :: StrictSig -> Arity -> [[OneShotInfo]]
-- See Note [Computing one-shot info]
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

-- saturatedByOneShots n C1(C1(...)) = True,
--   <=>
-- there are at least n nested C1(..) calls
-- See Note [Demand on the worker] in WorkWrap
saturatedByOneShots :: Int -> Demand -> Bool
saturatedByOneShots n (JD { ud = usg })
  = case usg of
      Use _ arg_usg -> go n arg_usg
      _             -> False
  where
    go 0 _             = True
    go n (UCall One u) = go (n-1) u
    go _ _             = False

argOneShots :: Demand          -- depending on saturation
            -> [OneShotInfo]
argOneShots (JD { ud = usg })
  = case usg of
      Use _ arg_usg -> go arg_usg
      _             -> []
  where
    go (UCall One  u) = OneShotLam : go u
    go (UCall Many u) = NoOneShotInfo : go u
    go _              = []

{- Note [Computing one-shot info]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider a call
    f (\pqr. e1) (\xyz. e2) e3
where f has usage signature
    C1(C(C1(U))) C1(U) U
Then argsOneShots returns a [[OneShotInfo]] of
    [[OneShot,NoOneShotInfo,OneShot],  [OneShot]]
The occurrence analyser propagates this one-shot infor to the
binders \pqr and \xyz; see Note [Use one-shot information] in OccurAnal.
-}

-- appIsBottom returns true if an application to n args
-- would diverge or throw an exception
-- See Note [Unsaturated applications]
appIsBottom :: StrictSig -> Int -> Bool
appIsBottom (StrictSig (DmdType _ ds res)) n
            | isBotRes res                   = not $ lengthExceeds ds n
appIsBottom _                              _ = False

{-
Note [Unsaturated applications]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If a function having bottom as its demand result is applied to a less
number of arguments than its syntactic arity, we cannot say for sure
that it is going to diverge. This is the reason why we use the
function appIsBottom, which, given a strictness signature and a number
of arguments, says conservatively if the function is going to diverge
or not.

Zap absence or one-shot information, under control of flags

Note [Killing usage information]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The flags -fkill-one-shot and -fkill-absence let you switch off the generation
of absence or one-shot information altogether.  This is only used for performance
tests, to see how important they are.
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

killUsageDemand :: DynFlags -> Demand -> Demand
-- See Note [Killing usage information]
killUsageDemand dflags dmd
  | Just kfs <- killFlags dflags = kill_usage kfs dmd
  | otherwise                    = dmd

killUsageSig :: DynFlags -> StrictSig -> StrictSig
-- See Note [Killing usage information]
killUsageSig dflags sig@(StrictSig (DmdType env ds r))
  | Just kfs <- killFlags dflags = StrictSig (DmdType env (map (kill_usage kfs) ds) r)
  | otherwise                    = sig

data KillFlags = KillFlags
    { kf_abs         :: Bool
    , kf_used_once   :: Bool
    , kf_called_once :: Bool
    }

killFlags :: DynFlags -> Maybe KillFlags
-- See Note [Killing usage information]
killFlags dflags
  | not kf_abs && not kf_used_once = Nothing
  | otherwise                      = Just (KillFlags {..})
  where
    kf_abs         = gopt Opt_KillAbsence dflags
    kf_used_once   = gopt Opt_KillOneShot dflags
    kf_called_once = kf_used_once

kill_usage :: KillFlags -> Demand -> Demand
kill_usage kfs (JD {sd = s, ud = u}) = JD {sd = s, ud = zap_musg kfs u}

zap_musg :: KillFlags -> ArgUse -> ArgUse
zap_musg kfs Abs
  | kf_abs kfs = useTop
  | otherwise  = Abs
zap_musg kfs (Use c u)
  | kf_used_once kfs = Use Many (zap_usg kfs u)
  | otherwise        = Use c    (zap_usg kfs u)

zap_usg :: KillFlags -> UseDmd -> UseDmd
zap_usg kfs (UCall c u)
    | kf_called_once kfs = UCall Many (zap_usg kfs u)
    | otherwise          = UCall c    (zap_usg kfs u)
zap_usg kfs (UProd us)   = UProd (map (zap_musg kfs) us)
zap_usg _   u            = u

-- If the argument is a used non-newtype dictionary, give it strict
-- demand. Also split the product type & demand and recur in order to
-- similarly strictify the argument's contained used non-newtype
-- superclass dictionaries. We use the demand as our recursive measure
-- to guarantee termination.
strictifyDictDmd :: Type -> Demand -> Demand
strictifyDictDmd ty dmd = case getUseDmd dmd of
  Use n _ |
    Just (tycon, _arg_tys, _data_con, inst_con_arg_tys)
      <- splitDataProductType_maybe ty,
    not (isNewTyCon tycon), isClassTyCon tycon -- is a non-newtype dictionary
    -> seqDmd `bothDmd` -- main idea: ensure it's strict
       case splitProdDmd_maybe dmd of
         -- superclass cycles should not be a problem, since the demand we are
         -- consuming would also have to be infinite in order for us to diverge
         Nothing -> dmd -- no components have interesting demand, so stop
                        -- looking for superclass dicts
         Just dmds
           | all (not . isAbsDmd) dmds -> evalDmd
             -- abstract to strict w/ arbitrary component use, since this
             -- smells like reboxing; results in CBV boxed
             --
             -- TODO revisit this if we ever do boxity analysis
           | otherwise -> case mkProdDmd $ zipWith strictifyDictDmd inst_con_arg_tys dmds of
               JD {sd = s,ud = a} -> JD (Str VanStr s) (Use n a)
             -- TODO could optimize with an aborting variant of zipWith since
             -- the superclass dicts are always a prefix
  _ -> dmd -- unused or not a dictionary

strictifyDmd :: Demand -> Demand
strictifyDmd dmd@(JD { sd = str })
  = dmd { sd = str `bothArgStr` Str VanStr HeadStr }

{-
Note [HyperStr and Use demands]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The information "HyperStr" needs to be in the strictness signature, and not in
the demand signature, because we still want to know about the demand on things. Consider

    f (x,y) True  = error (show x)
    f (x,y) False = x+1

The signature of f should be <S(SL),1*U(1*U(U),A)><S,1*U>m. If we were not
distinguishing the uses on x and y in the True case, we could either not figure
out how deeply we can unpack x, or that we do not have to pass y.


************************************************************************
*                                                                      *
                     Serialisation
*                                                                      *
************************************************************************
-}

instance Binary StrDmd where
  put_ bh HyperStr     = do putByte bh 0
  put_ bh HeadStr      = do putByte bh 1
  put_ bh (SCall s)    = do putByte bh 2
                            put_ bh s
  put_ bh (SProd sx)   = do putByte bh 3
                            put_ bh sx
  get bh = do
         h <- getByte bh
         case h of
           0 -> do return HyperStr
           1 -> do return HeadStr
           2 -> do s  <- get bh
                   return (SCall s)
           _ -> do sx <- get bh
                   return (SProd sx)

instance Binary ExnStr where
  put_ bh VanStr = putByte bh 0
  put_ bh ExnStr = putByte bh 1

  get bh = do h <- getByte bh
              return (case h of
                        0 -> VanStr
                        _ -> ExnStr)

instance Binary ArgStr where
    put_ bh Lazy         = do
            putByte bh 0
    put_ bh (Str x s)    = do
            putByte bh 1
            put_ bh x
            put_ bh s

    get  bh = do
            h <- getByte bh
            case h of
              0 -> return Lazy
              _ -> do x <- get bh
                      s  <- get bh
                      return $ Str x s

instance Binary Count where
    put_ bh One  = do putByte bh 0
    put_ bh Many = do putByte bh 1

    get  bh = do h <- getByte bh
                 case h of
                   0 -> return One
                   _ -> return Many

instance Binary ArgUse where
    put_ bh Abs          = do
            putByte bh 0
    put_ bh (Use c u)    = do
            putByte bh 1
            put_ bh c
            put_ bh u

    get  bh = do
            h <- getByte bh
            case h of
              0 -> return Abs
              _ -> do c  <- get bh
                      u  <- get bh
                      return $ Use c u

instance Binary UseDmd where
    put_ bh Used         = do
            putByte bh 0
    put_ bh UHead        = do
            putByte bh 1
    put_ bh (UCall c u)    = do
            putByte bh 2
            put_ bh c
            put_ bh u
    put_ bh (UProd ux)   = do
            putByte bh 3
            put_ bh ux

    get  bh = do
            h <- getByte bh
            case h of
              0 -> return $ Used
              1 -> return $ UHead
              2 -> do c <- get bh
                      u <- get bh
                      return (UCall c u)
              _ -> do ux <- get bh
                      return (UProd ux)

instance (Binary s, Binary u) => Binary (JointDmd s u) where
    put_ bh (JD { sd = x, ud = y }) = do put_ bh x; put_ bh y
    get  bh = do
              x <- get bh
              y <- get bh
              return $ JD { sd = x, ud = y }

instance Binary StrictSig where
    put_ bh (StrictSig aa) = do
            put_ bh aa
    get bh = do
          aa <- get bh
          return (StrictSig aa)

instance Binary DmdType where
  -- Ignore DmdEnv when spitting out the DmdType
  put_ bh (DmdType _ ds dr)
       = do put_ bh ds
            put_ bh dr
  get bh
      = do ds <- get bh
           dr <- get bh
           return (DmdType emptyDmdEnv ds dr)

instance Binary DmdResult where
  put_ bh (Dunno c)     = do { putByte bh 0; put_ bh c }
  put_ bh ThrowsExn     = putByte bh 1
  put_ bh Diverges      = putByte bh 2

  get bh = do { h <- getByte bh
              ; case h of
                  0 -> do { c <- get bh; return (Dunno c) }
                  1 -> return ThrowsExn
                  _ -> return Diverges }

instance Binary CPRResult where
    put_ bh (RetSum n)   = do { putByte bh 0; put_ bh n }
    put_ bh RetProd      = putByte bh 1
    put_ bh NoCPR        = putByte bh 2

    get  bh = do
            h <- getByte bh
            case h of
              0 -> do { n <- get bh; return (RetSum n) }
              1 -> return RetProd
              _ -> return NoCPR
