%
% (c) The University of Glasgow 2006
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[Demand]{@Demand@: A decoupled implementation of a demand domain}

\begin{code}

module Demand (
        StrDmd, UseDmd(..), Count(..), 
        countOnce, countMany,   -- cardinality

        Demand, CleanDemand, 
        mkProdDmd, mkOnceUsedDmd, mkManyUsedDmd, mkHeadStrict, oneifyDmd,
        getUsage, toCleanDmd, 
        absDmd, topDmd, botDmd, seqDmd,
        lubDmd, bothDmd, apply1Dmd, apply2Dmd, 
        isTopDmd, isBotDmd, isAbsDmd, isSeqDmd, 
        peelUseCall, cleanUseDmd_maybe, strictenDmd, bothCleanDmd,

        DmdType(..), dmdTypeDepth, lubDmdType, bothDmdType,
        nopDmdType, botDmdType, mkDmdType,
        addDemand,
        BothDmdArg, mkBothDmdArg, toBothDmdArg,

        DmdEnv, emptyDmdEnv,
        peelFV,

        DmdResult, CPRResult,
        isBotRes, isTopRes,
        topRes, botRes, cprProdRes, vanillaCprProdRes, cprSumRes,
        appIsBottom, isBottomingSig, pprIfaceStrictSig, 
        trimCPRInfo, returnsCPR, returnsCPR_maybe,
        StrictSig(..), mkStrictSig, mkClosedStrictSig, nopSig, botSig, cprProdSig,
        isNopSig, splitStrictSig, increaseStrictSigArity,

        seqDemand, seqDemandList, seqDmdType, seqStrictSig, 

        evalDmd, cleanEvalDmd, cleanEvalProdDmd, isStrictDmd, 
        splitDmdTy, splitFVs,
        deferAfterIO,
        postProcessUnsat, postProcessDmdTypeM,

        splitProdDmd, splitProdDmd_maybe, peelCallDmd, mkCallDmd,
        dmdTransformSig, dmdTransformDataConSig, dmdTransformDictSelSig,
        argOneShots, argsOneShots,

        isSingleUsed, reuseEnv, zapDemand, zapStrictSig,

        strictifyDictDmd

     ) where

#include "HsVersions.h"

import StaticFlags
import DynFlags
import Outputable
import Var ( Var )
import VarEnv
import UniqFM
import Util
import BasicTypes
import Binary
import Maybes           ( isJust, orElse )

import Type            ( Type )
import TyCon           ( isNewTyCon, isClassTyCon )
import DataCon         ( splitDataProductType_maybe )
\end{code}

%************************************************************************
%*                                                                      *
\subsection{Strictness domain}
%*                                                                      *
%************************************************************************

        Lazy
         |
      HeadStr
      /     \
  SCall      SProd
      \      /
      HyperStr

\begin{code}

-- Vanilla strictness domain
data StrDmd
  = HyperStr             -- Hyper-strict 
                         -- Bottom of the lattice
                         -- Note [HyperStr and Use demands]

  | SCall StrDmd         -- Call demand
                         -- Used only for values of function type

  | SProd [MaybeStr]     -- Product
                         -- Used only for values of product type
                         -- Invariant: not all components are HyperStr (use HyperStr)
                         --            not all components are Lazy     (use HeadStr)

  | HeadStr              -- Head-Strict
                         -- A polymorphic demand: used for values of all types,
                         --                       including a type variable

  deriving ( Eq, Show )

data MaybeStr = Lazy            -- Lazy
                                -- Top of the lattice
              | Str StrDmd
  deriving ( Eq, Show )

-- Well-formedness preserving constructors for the Strictness domain
strBot, strTop :: MaybeStr
strBot = Str HyperStr
strTop = Lazy

mkSCall :: StrDmd -> StrDmd
mkSCall HyperStr = HyperStr
mkSCall s        = SCall s

mkSProd :: [MaybeStr] -> StrDmd
mkSProd sx
  | any isHyperStr sx = HyperStr
  | all isLazy     sx = HeadStr
  | otherwise         = SProd sx

isLazy :: MaybeStr -> Bool
isLazy Lazy    = True
isLazy (Str _) = False

isHyperStr :: MaybeStr -> Bool
isHyperStr (Str HyperStr) = True
isHyperStr _              = False

-- Pretty-printing
instance Outputable StrDmd where
  ppr HyperStr      = char 'B'
  ppr (SCall s)     = char 'C' <> parens (ppr s)
  ppr HeadStr       = char 'S'
  ppr (SProd sx)    = char 'S' <> parens (hcat (map ppr sx))

instance Outputable MaybeStr where
  ppr (Str s)       = ppr s
  ppr Lazy          = char 'L'

lubMaybeStr :: MaybeStr -> MaybeStr -> MaybeStr
lubMaybeStr Lazy     _        = Lazy
lubMaybeStr _        Lazy     = Lazy
lubMaybeStr (Str s1) (Str s2) = Str (s1 `lubStr` s2)

lubStr :: StrDmd -> StrDmd -> StrDmd
lubStr HyperStr s              = s
lubStr (SCall s1) HyperStr     = SCall s1
lubStr (SCall _)  HeadStr      = HeadStr
lubStr (SCall s1) (SCall s2)   = SCall (s1 `lubStr` s2)
lubStr (SCall _)  (SProd _)    = HeadStr
lubStr (SProd sx) HyperStr     = SProd sx
lubStr (SProd _)  HeadStr      = HeadStr
lubStr (SProd s1) (SProd s2)
    | length s1 == length s2   = mkSProd (zipWith lubMaybeStr s1 s2)
    | otherwise                = HeadStr
lubStr (SProd _) (SCall _)     = HeadStr
lubStr HeadStr   _             = HeadStr

bothMaybeStr :: MaybeStr -> MaybeStr -> MaybeStr
bothMaybeStr Lazy     s           = s
bothMaybeStr s        Lazy        = s 
bothMaybeStr (Str s1) (Str s2) = Str (s1 `bothStr` s2)

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
    | length s1 == length s2   = mkSProd (zipWith bothMaybeStr s1 s2)
    | otherwise                = HyperStr  -- Weird
bothStr (SProd _) (SCall _)    = HyperStr

-- utility functions to deal with memory leaks
seqStrDmd :: StrDmd -> ()
seqStrDmd (SProd ds)   = seqStrDmdList ds
seqStrDmd (SCall s)     = s `seq` () 
seqStrDmd _            = ()

seqStrDmdList :: [MaybeStr] -> ()
seqStrDmdList [] = ()
seqStrDmdList (d:ds) = seqMaybeStr d `seq` seqStrDmdList ds

seqMaybeStr :: MaybeStr -> ()
seqMaybeStr Lazy    = ()
seqMaybeStr (Str s) = seqStrDmd s

-- Splitting polymorphic demands
splitStrProdDmd :: Int -> StrDmd -> [MaybeStr]
splitStrProdDmd n HyperStr     = replicate n strBot
splitStrProdDmd n HeadStr      = replicate n strTop
splitStrProdDmd n (SProd ds)   = ASSERT( ds `lengthIs` n) ds
splitStrProdDmd _ d@(SCall {}) = pprPanic "attempt to prod-split strictness call demand" (ppr d)
\end{code}

%************************************************************************
%*                                                                      *
\subsection{Absence domain}
%*                                                                      *
%************************************************************************

      Used
      /   \
  UCall   UProd
      \   /
      UHead
       |
      Abs

\begin{code}

-- Domain for genuine usage
data UseDmd
  = UCall Count UseDmd   -- Call demand for absence
                         -- Used only for values of function type

  | UProd [MaybeUsed]     -- Product 
                         -- Used only for values of product type
                         -- See Note [Don't optimise UProd(Used) to Used]
                         -- [Invariant] Not all components are Abs
                         --             (in that case, use UHead)

  | UHead                -- May be used; but its sub-components are 
                         -- definitely *not* used.  Roughly U(AAA)
                         -- Eg the usage of x in x `seq` e
                         -- A polymorphic demand: used for values of all types,
                         --                       including a type variable
                         -- Since (UCall _ Abs) is ill-typed, UHead doesn't
                         -- make sense for lambdas

  | Used                 -- May be used; and its sub-components may be used
                         -- Top of the lattice
  deriving ( Eq, Show )

-- Extended usage demand for absence and counting
data MaybeUsed
  = Abs                  -- Definitely unused
                         -- Bottom of the lattice

  | Use Count UseDmd     -- May be used with some cardinality 
  deriving ( Eq, Show )

-- Abstract counting of usages
data Count = One | Many
  deriving ( Eq, Show )     

-- Pretty-printing
instance Outputable MaybeUsed where
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

-- Well-formedness preserving constructors for the Absence domain
countOnce, countMany :: Count
countOnce = One
countMany = Many

useBot, useTop :: MaybeUsed
useBot     = Abs
useTop     = Use Many Used

mkUCall :: Count -> UseDmd -> UseDmd
--mkUCall c Used = Used c 
mkUCall c a  = UCall c a

mkUProd :: [MaybeUsed] -> UseDmd
mkUProd ux 
  | all (== Abs) ux    = UHead
  | otherwise          = UProd ux

lubCount :: Count -> Count -> Count
lubCount _ Many = Many
lubCount Many _ = Many
lubCount x _    = x 

lubMaybeUsed :: MaybeUsed -> MaybeUsed -> MaybeUsed
lubMaybeUsed Abs x                   = x
lubMaybeUsed x Abs                   = x
lubMaybeUsed (Use c1 a1) (Use c2 a2) = Use (lubCount c1 c2) (lubUse a1 a2)

lubUse :: UseDmd -> UseDmd -> UseDmd
lubUse UHead       u               = u
lubUse (UCall c u) UHead           = UCall c u
lubUse (UCall c1 u1) (UCall c2 u2) = UCall (lubCount c1 c2) (lubUse u1 u2)
lubUse (UCall _ _) _               = Used
lubUse (UProd ux) UHead            = UProd ux 
lubUse (UProd ux1) (UProd ux2)
     | length ux1 == length ux2    = UProd $ zipWith lubMaybeUsed ux1 ux2
     | otherwise                   = Used
lubUse (UProd {}) (UCall {})       = Used
-- lubUse (UProd {}) Used             = Used
lubUse (UProd ux) Used             = UProd (map (`lubMaybeUsed` useTop) ux)
lubUse Used       (UProd ux)       = UProd (map (`lubMaybeUsed` useTop) ux)
lubUse Used _                      = Used  -- Note [Used should win]

-- `both` is different from `lub` in its treatment of counting; if
-- `both` is computed for two used, the result always has
--  cardinality `Many` (except for the inner demands of UCall demand -- [TODO] explain).  
--  Also,  x `bothUse` x /= x (for anything but Abs).

bothMaybeUsed :: MaybeUsed -> MaybeUsed -> MaybeUsed
bothMaybeUsed Abs x                   = x
bothMaybeUsed x Abs                   = x
bothMaybeUsed (Use _ a1) (Use _ a2)   = Use Many (bothUse a1 a2)


bothUse :: UseDmd -> UseDmd -> UseDmd
bothUse UHead       u               = u
bothUse (UCall c u) UHead           = UCall c u

-- Exciting special treatment of inner demand for call demands: 
--    use `lubUse` instead of `bothUse`!
bothUse (UCall _ u1) (UCall _ u2)   = UCall Many (u1 `lubUse` u2)

bothUse (UCall {}) _                = Used
bothUse (UProd ux) UHead            = UProd ux 
bothUse (UProd ux1) (UProd ux2)
      | length ux1 == length ux2    = UProd $ zipWith bothMaybeUsed ux1 ux2
      | otherwise                   = Used
bothUse (UProd {}) (UCall {})       = Used
-- bothUse (UProd {}) Used             = Used  -- Note [Used should win]
bothUse Used (UProd ux)             = UProd (map (`bothMaybeUsed` useTop) ux)
bothUse (UProd ux) Used             = UProd (map (`bothMaybeUsed` useTop) ux)
bothUse Used _                      = Used  -- Note [Used should win]

peelUseCall :: UseDmd -> Maybe (Count, UseDmd)
peelUseCall (UCall c u)   = Just (c,u)
peelUseCall _             = Nothing
\end{code}

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


\begin{code}
-- If a demand is used multiple times (i.e. reused), than any use-once
-- mentioned there, that is not protected by a UCall, can happen many times.
markReusedDmd :: MaybeUsed -> MaybeUsed
markReusedDmd Abs         = Abs
markReusedDmd (Use _ a)   = Use Many (markReused a)

markReused :: UseDmd -> UseDmd
markReused (UCall _ u)      = UCall Many u   -- No need to recurse here
markReused (UProd ux)       = UProd (map markReusedDmd ux)
markReused u                = u

isUsedMU :: MaybeUsed -> Bool
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
seqUseDmd (UProd ds)   = seqMaybeUsedList ds
seqUseDmd (UCall c d)  = c `seq` seqUseDmd d
seqUseDmd _            = ()

seqMaybeUsedList :: [MaybeUsed] -> ()
seqMaybeUsedList []     = ()
seqMaybeUsedList (d:ds) = seqMaybeUsed d `seq` seqMaybeUsedList ds

seqMaybeUsed :: MaybeUsed -> ()
seqMaybeUsed (Use c u)  = c `seq` seqUseDmd u
seqMaybeUsed _          = ()

-- Splitting polymorphic Maybe-Used demands
splitUseProdDmd :: Int -> UseDmd -> [MaybeUsed]
splitUseProdDmd n Used          = replicate n useTop
splitUseProdDmd n UHead         = replicate n Abs
splitUseProdDmd n (UProd ds)    = ASSERT2( ds `lengthIs` n, ppr n $$ ppr ds ) ds
splitUseProdDmd _ d@(UCall _ _) = pprPanic "attempt to prod-split usage call demand" (ppr d)
\end{code}
  
%************************************************************************
%*                                                                      *
\subsection{Joint domain for Strictness and Absence}
%*                                                                      *
%************************************************************************

\begin{code}

data JointDmd = JD { strd :: MaybeStr, absd :: MaybeUsed } 
  deriving ( Eq, Show )

-- Pretty-printing
instance Outputable JointDmd where
  ppr (JD {strd = s, absd = a}) = angleBrackets (ppr s <> char ',' <> ppr a)

-- Well-formedness preserving constructors for the joint domain
mkJointDmd :: MaybeStr -> MaybeUsed -> JointDmd
mkJointDmd s a = JD { strd = s, absd = a }

mkJointDmds :: [MaybeStr] -> [MaybeUsed] -> [JointDmd]
mkJointDmds ss as = zipWithEqual "mkJointDmds" mkJointDmd ss as
     
absDmd :: JointDmd
absDmd = mkJointDmd Lazy Abs

apply1Dmd, apply2Dmd :: Demand
-- C1(U), C1(C1(U)) respectively
apply1Dmd = JD { strd = Lazy, absd = Use Many (UCall One Used) }
apply2Dmd = JD { strd = Lazy, absd = Use Many (UCall One (UCall One Used)) }

topDmd :: JointDmd
topDmd = mkJointDmd Lazy useTop

seqDmd :: JointDmd
seqDmd = mkJointDmd (Str HeadStr) (Use One UHead)

botDmd :: JointDmd
botDmd = mkJointDmd strBot useBot

lubDmd :: JointDmd -> JointDmd -> JointDmd
lubDmd (JD {strd = s1, absd = a1}) 
       (JD {strd = s2, absd = a2}) = mkJointDmd (s1 `lubMaybeStr` s2) (a1 `lubMaybeUsed` a2)

bothDmd :: JointDmd -> JointDmd -> JointDmd
bothDmd (JD {strd = s1, absd = a1}) 
        (JD {strd = s2, absd = a2}) = mkJointDmd (s1 `bothMaybeStr` s2) (a1 `bothMaybeUsed` a2)

isTopDmd :: JointDmd -> Bool
isTopDmd (JD {strd = Lazy, absd = Use Many Used}) = True
isTopDmd _                                        = False 

isBotDmd :: JointDmd -> Bool
isBotDmd (JD {strd = Str HyperStr, absd = Abs}) = True
isBotDmd _                                      = False 
  
isAbsDmd :: JointDmd -> Bool
isAbsDmd (JD {absd = Abs})  = True   -- The strictness part can be HyperStr 
isAbsDmd _                  = False  -- for a bottom demand

isSeqDmd :: JointDmd -> Bool
isSeqDmd (JD {strd=Str HeadStr, absd=Use _ UHead}) = True
isSeqDmd _                                         = False

-- More utility functions for strictness
seqDemand :: JointDmd -> ()
seqDemand (JD {strd = x, absd = y}) = seqMaybeStr x `seq` seqMaybeUsed y `seq` ()

seqDemandList :: [JointDmd] -> ()
seqDemandList [] = ()
seqDemandList (d:ds) = seqDemand d `seq` seqDemandList ds

isStrictDmd :: Demand -> Bool
-- See Note [Strict demands]
isStrictDmd (JD {absd = Abs})  = False
isStrictDmd (JD {strd = Lazy}) = False
isStrictDmd _                  = True

isWeakDmd :: Demand -> Bool
isWeakDmd (JD {strd = s, absd = a}) = isLazy s && isUsedMU a

cleanUseDmd_maybe :: JointDmd -> Maybe UseDmd
cleanUseDmd_maybe (JD { absd = Use _ ud }) = Just ud
cleanUseDmd_maybe _                        = Nothing

splitFVs :: Bool   -- Thunk
         -> DmdEnv -> (DmdEnv, DmdEnv)
splitFVs is_thunk rhs_fvs
  | is_thunk  = foldUFM_Directly add (emptyVarEnv, emptyVarEnv) rhs_fvs
  | otherwise = partitionVarEnv isWeakDmd rhs_fvs
  where
    add uniq dmd@(JD { strd = s, absd = u }) (lazy_fv, sig_fv)
      | Lazy <- s = (addToUFM_Directly lazy_fv uniq dmd, sig_fv)
      | otherwise = ( addToUFM_Directly lazy_fv uniq (JD { strd = Lazy, absd = u })
                    , addToUFM_Directly sig_fv  uniq (JD { strd = s,    absd = Abs }) )
\end{code}

%************************************************************************
%*                                                                      *
\subsection{Clean demand for Strictness and Usage}
%*                                                                      *
%************************************************************************

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


\begin{code}

data CleanDemand = CD { sd :: StrDmd, ud :: UseDmd } 
  deriving ( Eq, Show )

instance Outputable CleanDemand where
  ppr (CD {sd = s, ud = a}) = angleBrackets (ppr s <> comma <> ppr a)

mkCleanDmd :: StrDmd -> UseDmd -> CleanDemand
mkCleanDmd s a = CD { sd = s, ud = a }

bothCleanDmd :: CleanDemand -> CleanDemand -> CleanDemand
bothCleanDmd (CD { sd = s1, ud = a1}) (CD { sd = s2, ud = a2}) 
  = CD { sd = s1 `bothStr` s2, ud = a1 `bothUse` a2 }

mkHeadStrict :: CleanDemand -> CleanDemand
mkHeadStrict (CD { ud = a }) = mkCleanDmd HeadStr a

oneifyDmd :: JointDmd -> JointDmd
oneifyDmd (JD { strd = s, absd = Use _ a }) = JD { strd = s, absd = Use One a }
oneifyDmd jd                                = jd

mkOnceUsedDmd, mkManyUsedDmd :: CleanDemand -> JointDmd
mkOnceUsedDmd (CD {sd = s,ud = a}) = mkJointDmd (Str s) (Use One a)
mkManyUsedDmd (CD {sd = s,ud = a}) = mkJointDmd (Str s) (Use Many a)

getUsage :: CleanDemand -> UseDmd
getUsage = ud

evalDmd :: JointDmd
-- Evaluated strictly, and used arbitrarily deeply
evalDmd = mkJointDmd (Str HeadStr) useTop

mkProdDmd :: [JointDmd] -> CleanDemand
mkProdDmd dx 
  = mkCleanDmd sp up 
  where
    sp = mkSProd $ map strd dx
    up = mkUProd $ map absd dx   

mkCallDmd :: CleanDemand -> CleanDemand
mkCallDmd (CD {sd = d, ud = u}) 
  = mkCleanDmd (mkSCall d) (mkUCall One u)

cleanEvalDmd :: CleanDemand
cleanEvalDmd = mkCleanDmd HeadStr Used

cleanEvalProdDmd :: Arity -> CleanDemand
cleanEvalProdDmd n = mkCleanDmd HeadStr (UProd (replicate n useTop))

isSingleUsed :: JointDmd -> Bool
isSingleUsed (JD {absd=a}) = is_used_once a
  where
    is_used_once Abs         = True
    is_used_once (Use One _) = True
    is_used_once _           = False
\end{code}

Note [Threshold demands]
~~~~~~~~~~~~~~~~~~~~~~~~
Threshold usage demand is generated to figure out if
cardinality-instrumented demands of a binding's free variables should
be unleashed. See also [Aggregated demand for cardinality].

Note [Replicating polymorphic demands]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Some demands can be considered as polymorphic. Generally, it is
applicable to such beasts as tops, bottoms as well as Head-Used adn
Head-stricts demands. For instance,

S ~ S(L, ..., L)

Also, when top or bottom is occurred as a result demand, it in fact
can be expanded to saturate a callee's arity. 


\begin{code}
splitProdDmd :: Arity -> JointDmd -> [JointDmd]
splitProdDmd n (JD {strd = s, absd = u})
  = mkJointDmds (split_str s) (split_abs u)
  where
    split_str Lazy    = replicate n Lazy
    split_str (Str s) = splitStrProdDmd n s

    split_abs Abs       = replicate n Abs
    split_abs (Use _ u) = splitUseProdDmd n u

splitProdDmd_maybe :: JointDmd -> Maybe [JointDmd]
-- Split a product into its components, iff there is any
-- useful information to be extracted thereby
-- The demand is not necessarily strict!
splitProdDmd_maybe (JD {strd = s, absd = u})
  = case (s,u) of
      (Str (SProd sx), Use _ u)          -> Just (mkJointDmds sx (splitUseProdDmd (length sx) u))
      (Str s,          Use _ (UProd ux)) -> Just (mkJointDmds (splitStrProdDmd (length ux) s) ux)
      (Lazy,           Use _ (UProd ux)) -> Just (mkJointDmds (replicate (length ux) Lazy)    ux)
      _                                  -> Nothing
\end{code}

%************************************************************************
%*                                                                      *
                   Demand results
%*                                                                      *
%************************************************************************


DmdResult:     Dunno CPRResult
               /
        Diverges


CPRResult:         NoCPR
                   /    \
            RetProd    RetSum ConTag


Product contructors return (Dunno (RetProd rs))
In a fixpoint iteration, start from Diverges
We have lubs, but not glbs; but that is ok.


\begin{code}
------------------------------------------------------------------------
-- Constructed Product Result                                             
------------------------------------------------------------------------

data Termination r = Diverges    -- Definitely diverges
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
lubDmdResult (Dunno c1)     Diverges       = Dunno c1
lubDmdResult (Dunno c1)     (Dunno c2)     = Dunno (c1 `lubCPR` c2)
-- This needs to commute with defaultDmd, i.e.
-- defaultDmd (r1 `lubDmdResult` r2) = defaultDmd r1 `lubDmd` defaultDmd r2
-- (See Note [Default demand on free variables] for why)

bothDmdResult :: DmdResult -> Termination () -> DmdResult
-- See Note [Asymmetry of 'both' for DmdType and DmdResult]
bothDmdResult _              Diverges   = Diverges
bothDmdResult r              _          = r
-- This needs to commute with defaultDmd, i.e.
-- defaultDmd (r1 `bothDmdResult` r2) = defaultDmd r1 `bothDmd` defaultDmd r2
-- (See Note [Default demand on free variables] for why)

instance Outputable DmdResult where
  ppr Diverges      = char 'b'
  ppr (Dunno c)     = ppr c

instance Outputable CPRResult where
  ppr NoCPR        = empty
  ppr (RetSum n)   = char 'm' <> int n
  ppr RetProd      = char 'm'

seqDmdResult :: DmdResult -> ()
seqDmdResult Diverges = ()
seqDmdResult (Dunno c)     = seqCPRResult c

seqCPRResult :: CPRResult -> ()
seqCPRResult NoCPR        = ()
seqCPRResult (RetSum n)   = n `seq` ()
seqCPRResult RetProd      = ()


------------------------------------------------------------------------
-- Combined demand result                                             --
------------------------------------------------------------------------

-- [cprRes] lets us switch off CPR analysis
-- by making sure that everything uses TopRes
topRes, botRes :: DmdResult
topRes = Dunno NoCPR
botRes = Diverges

cprSumRes :: ConTag -> DmdResult
cprSumRes tag | opt_CprOff = topRes
              | otherwise  = Dunno $ RetSum tag

cprProdRes :: [DmdType] -> DmdResult
cprProdRes _arg_tys
  | opt_CprOff = topRes
  | otherwise  = Dunno $ RetProd

vanillaCprProdRes :: Arity -> DmdResult
vanillaCprProdRes _arity
  | opt_CprOff = topRes
  | otherwise  = Dunno $ RetProd

isTopRes :: DmdResult -> Bool
isTopRes (Dunno NoCPR) = True
isTopRes _             = False

isBotRes :: DmdResult -> Bool
isBotRes Diverges = True
isBotRes _        = False

trimCPRInfo :: Bool -> Bool -> DmdResult -> DmdResult
trimCPRInfo trim_all trim_sums res
  = trimR res
  where
    trimR (Dunno c)     = Dunno (trimC c)
    trimR Diverges      = Diverges

    trimC (RetSum n)   | trim_all || trim_sums = NoCPR
                       | otherwise             = RetSum n
    trimC RetProd      | trim_all  = NoCPR
                       | otherwise = RetProd
    trimC NoCPR = NoCPR

returnsCPR :: DmdResult -> Bool
returnsCPR dr = isJust (returnsCPR_maybe dr)

returnsCPR_maybe :: DmdResult -> Maybe ConTag
returnsCPR_maybe (Dunno c)     = retCPR_maybe c
returnsCPR_maybe Diverges      = Nothing

retCPR_maybe :: CPRResult -> Maybe ConTag
retCPR_maybe (RetSum t)  = Just t
retCPR_maybe RetProd     = Just fIRST_TAG
retCPR_maybe NoCPR       = Nothing

-- See Notes [Default demand on free variables]
-- and [defaultDmd vs. resTypeArgDmd]
defaultDmd :: Termination r -> JointDmd
defaultDmd Diverges = botDmd
defaultDmd _        = absDmd

resTypeArgDmd :: DmdResult -> JointDmd
-- TopRes and BotRes are polymorphic, so that
--      BotRes === Bot -> BotRes === ...
--      TopRes === Top -> TopRes === ...
-- This function makes that concrete
-- Also see Note [defaultDmd vs. resTypeArgDmd]
resTypeArgDmd r | isBotRes r = botDmd
resTypeArgDmd _              = topDmd
\end{code}

Note [defaultDmd and resTypeArgDmd]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

These functions are similar: They express the demand on something not
explicitly mentioned in the environment resp. the argument list. Yet they are
different:
 * Variables not mentioned in the free variables environment are definitely
   unused, so we can use absDmd there.
 * Further arguments *can* be used, of course. Hence topDmd is used.

Note [Worthy functions for Worker-Wrapper split]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
For non-bottoming functions a worker-wrapper transformation takes into
account several possibilities to decide if the function is worthy for
splitting:

1. The result is of product type and the function is strict in some
(or even all) of its arguments. The check that the argument is used is
more of sanity nature, since strictness implies usage. Example:

f :: (Int, Int) -> Int
f p = (case p of (a,b) -> a) + 1

should be splitted to

f :: (Int, Int) -> Int
f p = case p of (a,b) -> $wf a

$wf :: Int -> Int
$wf a = a + 1

2. Sometimes it also makes sense to perform a WW split if the
strictness analysis cannot say for sure if the function is strict in
components of its argument. Then we reason according to the inferred
usage information: if the function uses its product argument's
components, the WW split can be beneficial. Example:

g :: Bool -> (Int, Int) -> Int
g c p = case p of (a,b) ->
          if c then a else b

The function g is strict in is argument p and lazy in its
components. However, both components are used in the RHS. The idea is
since some of the components (both in this case) are used in the
right-hand side, the product must presumable be taken apart.

Therefore, the WW transform splits the function g to

g :: Bool -> (Int, Int) -> Int
g c p = case p of (a,b) -> $wg c a b

$wg :: Bool -> Int -> Int -> Int
$wg c a b = if c then a else b

3. If an argument is absent, it would be silly to pass it to a
function, hence the worker with reduced arity is generated.


Note [Worker-wrapper for bottoming functions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We used not to split if the result is bottom.
[Justification:  there's no efficiency to be gained.]

But it's sometimes bad not to make a wrapper.  Consider
        fw = \x# -> let x = I# x# in case e of
                                        p1 -> error_fn x
                                        p2 -> error_fn x
                                        p3 -> the real stuff
The re-boxing code won't go away unless error_fn gets a wrapper too.
[We don't do reboxing now, but in general it's better to pass an
unboxed thing to f, and have it reboxed in the error cases....]

However we *don't* want to do this when the argument is not actually
taken apart in the function at all.  Otherwise we risk decomposing a
masssive tuple which is barely used.  Example:

        f :: ((Int,Int) -> String) -> (Int,Int) -> a
        f g pr = error (g pr)

        main = print (f fst (1, error "no"))

Here, f does not take 'pr' apart, and it's stupid to do so.
Imagine that it had millions of fields. This actually happened
in GHC itself where the tuple was DynFlags


%************************************************************************
%*                                                                      *
\subsection{Demand environments and types}
%*                                                                      *
%************************************************************************

\begin{code}
type Demand = JointDmd

type DmdEnv = VarEnv Demand   -- See Note [Default demand on free variables]

data DmdType = DmdType 
                  DmdEnv        -- Demand on explicitly-mentioned 
                                --      free variables
                  [Demand]      -- Demand on arguments
                  DmdResult     -- Nature of result
\end{code}

Note [Nature of result demand]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We assume the result in a demand type to be either a top or bottom
in order to represent the information about demand on the function
result, imposed by its definition. There are not so many things we 
can say, though. 

For instance, one can consider a function

        h = \v -> error "urk"

Taking the definition of strictness, we can easily see that 

        h undefined = undefined

that is, h is strict in v. However, v is not used somehow in the body
of h How can we know that h is strict in v? In fact, we know it by
considering a result demand of error and bottom and unleashing it on
all the variables in scope at a call site (in this case, this is only
v). We can also consider a case

       h = \v -> f x

where we know that the result of f is not hyper-strict (i.e, it is
lazy, or top). So, we put the same demand on v, which allow us to
infer a lazy demand that h puts on v.

Note [Asymmetry of 'both' for DmdType and DmdResult]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
'both' for DmdTypes is *assymetrical*, because there is only one
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


\begin{code}
-- Equality needed for fixpoints in DmdAnal
instance Eq DmdType where
  (==) (DmdType fv1 ds1 res1)
       (DmdType fv2 ds2 res2) =  ufmToList fv1 == ufmToList fv2
                              && ds1 == ds2 && res1 == res2

lubDmdType :: DmdType -> DmdType -> DmdType
lubDmdType (DmdType fv1 ds1 r1) (DmdType fv2 ds2 r2)
  = DmdType lub_fv (lub_ds ds1 ds2) (r1 `lubDmdResult` r2)
  where
    lub_fv  = plusVarEnv_CD lubDmd fv1 (defaultDmd r1) fv2 (defaultDmd r2)

    -- Extend the shorter argument list to match the longer, using resTypeArgDmd
    lub_ds (d1:ds1) (d2:ds2) = lubDmd d1 d2                   : lub_ds ds1 ds2
    lub_ds (d1:ds1) []       = (d1 `lubDmd` resTypeArgDmd r2) : lub_ds ds1 []
    lub_ds []       (d2:ds2) = (resTypeArgDmd r1 `lubDmd` d2) : lub_ds [] ds2
    lub_ds []       []       = []

type BothDmdArg = (DmdEnv, Termination ())

mkBothDmdArg :: DmdEnv -> BothDmdArg
mkBothDmdArg env = (env, Dunno ())

toBothDmdArg :: DmdType -> BothDmdArg
toBothDmdArg (DmdType fv _ r) = (fv, go r)
  where
  go (Dunno {})     = Dunno ()
  go Diverges       = Diverges

bothDmdType :: DmdType -> BothDmdArg -> DmdType
bothDmdType (DmdType fv1 ds1 r1) (fv2, t2)
    -- See Note [Asymmetry of 'both' for DmdType and DmdResult]
    -- 'both' takes the argument/result info from its *first* arg,
    -- using its second arg just for its free-var info.
  = DmdType both_fv ds1 (r1 `bothDmdResult` t2)
  where both_fv = plusVarEnv_CD bothDmd fv1 (defaultDmd r1) fv2 (defaultDmd t2)

instance Outputable DmdType where
  ppr (DmdType fv ds res) 
    = hsep [text "DmdType",
            hcat (map ppr ds) <> ppr res,
            if null fv_elts then empty
            else braces (fsep (map pp_elt fv_elts))]
    where
      pp_elt (uniq, dmd) = ppr uniq <> text "->" <> ppr dmd
      fv_elts = ufmToList fv

emptyDmdEnv :: VarEnv Demand
emptyDmdEnv = emptyVarEnv

-- nopDmdType is the demand of doing nothing
-- (lazy, absent, no CPR information, no termination information).
-- Note that it is ''not'' the top of the lattice (which would be "may use everything"),
-- so it is (no longer) called topDmd
nopDmdType, botDmdType :: DmdType
nopDmdType = DmdType emptyDmdEnv [] topRes
botDmdType = DmdType emptyDmdEnv [] botRes

cprProdDmdType :: Arity -> DmdType
cprProdDmdType _arity
  = DmdType emptyDmdEnv [] (Dunno RetProd)

isNopDmdType :: DmdType -> Bool
isNopDmdType (DmdType env [] res)
  | isTopRes res && isEmptyVarEnv env = True
isNopDmdType _                        = False

mkDmdType :: DmdEnv -> [Demand] -> DmdResult -> DmdType
mkDmdType fv ds res = DmdType fv ds res

dmdTypeDepth :: DmdType -> Arity
dmdTypeDepth (DmdType _ ds _) = length ds

seqDmdType :: DmdType -> ()
seqDmdType (DmdType _env ds res) = 
  {- ??? env `seq` -} seqDemandList ds `seq` seqDmdResult res `seq` ()

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
-- * We can keep demand information (i.e. lub with an absent deman)
-- * We have to kill definite divergence
-- * We can keep CPR information.
-- See Note [IO hack in the demand analyser]
deferAfterIO :: DmdType -> DmdType
deferAfterIO d@(DmdType _ _ res) =
    case d `lubDmdType` nopDmdType of
        DmdType fv ds _ -> DmdType fv ds (defer_res res)
  where
  defer_res Diverges      = topRes
  defer_res r             = r

strictenDmd :: JointDmd -> CleanDemand
strictenDmd (JD {strd = s, absd = u})
  = CD { sd = poke_s s, ud = poke_u u }
  where
    poke_s Lazy      = HeadStr
    poke_s (Str s)   = s
    poke_u Abs       = UHead
    poke_u (Use _ u) = u
\end{code}

Deferring and peeeling

\begin{code}
type DeferAndUse   -- Describes how to degrade a result type
   =( Bool        -- Lazify (defer) the type
    , Count)      -- Many => manify the type

type DeferAndUseM = Maybe DeferAndUse
  -- Nothing <=> absent-ify the result type; it will never be used

toCleanDmd :: Demand -> (CleanDemand, DeferAndUseM)
-- See Note [Analyzing with lazy demand and lambdas]
toCleanDmd (JD { strd = s, absd = u })
  = case (s,u) of
      (Str s', Use c u') -> (CD { sd = s',      ud = u' },   Just (False, c))
      (Lazy,   Use c u') -> (CD { sd = HeadStr, ud = u' },   Just (True,  c))
      (_,      Abs)      -> (CD { sd = HeadStr, ud = Used }, Nothing)

-- This is used in dmdAnalStar when post-processing
-- a function's argument demand. So we only care about what
-- does to free variables, and whether it terminates.
postProcessDmdTypeM :: DeferAndUseM -> DmdType -> BothDmdArg
postProcessDmdTypeM Nothing   _  = (emptyDmdEnv, Dunno ())
  -- Incoming demand was Absent, so just discard all usage information
  -- We only processed the thing at all to analyse the body
  -- See Note [Always analyse in virgin pass]
postProcessDmdTypeM (Just du) (DmdType fv _ res_ty)
    = (postProcessDmdEnv du fv, postProcessDmdResult du res_ty)

postProcessDmdResult :: DeferAndUse -> DmdResult -> Termination ()
postProcessDmdResult (True,_)  _          = Dunno ()
postProcessDmdResult (False,_) (Dunno {}) = Dunno ()
postProcessDmdResult (False,_) Diverges   = Diverges

postProcessDmdEnv :: DeferAndUse -> DmdEnv -> DmdEnv
postProcessDmdEnv (True,  Many) env = deferReuseEnv env
postProcessDmdEnv (False, Many) env = reuseEnv env
postProcessDmdEnv (True,  One)  env = deferEnv env
postProcessDmdEnv (False, One)  env = env


postProcessUnsat :: DeferAndUse -> DmdType -> DmdType
postProcessUnsat (True,  Many) ty = deferReuse ty
postProcessUnsat (False, Many) ty = reuseType ty
postProcessUnsat (True,  One)  ty = deferType ty
postProcessUnsat (False, One)  ty = ty

deferType, reuseType, deferReuse :: DmdType -> DmdType
deferType  (DmdType fv ds _)      = DmdType (deferEnv fv)      (map deferDmd ds)      topRes
reuseType  (DmdType fv ds res_ty) = DmdType (reuseEnv fv)      (map reuseDmd ds)      res_ty
deferReuse (DmdType fv ds _)      = DmdType (deferReuseEnv fv) (map deferReuseDmd ds) topRes

deferEnv, reuseEnv, deferReuseEnv :: DmdEnv -> DmdEnv
deferEnv      fv = mapVarEnv deferDmd fv
reuseEnv      fv = mapVarEnv reuseDmd fv
deferReuseEnv fv = mapVarEnv deferReuseDmd fv

deferDmd, reuseDmd, deferReuseDmd :: JointDmd -> JointDmd
deferDmd      (JD {strd=_, absd=a}) = mkJointDmd Lazy a
reuseDmd      (JD {strd=d, absd=a}) = mkJointDmd d    (markReusedDmd a)
deferReuseDmd (JD {strd=_, absd=a}) = mkJointDmd Lazy (markReusedDmd a)

-- Peels one call level from the demand, and also returns
-- whether it was unsaturated (separately for strictness and usage)
peelCallDmd :: CleanDemand -> (CleanDemand, DeferAndUse)
-- Exploiting the fact that
-- on the strictness side      C(B) = B
-- and on the usage side       C(U) = U
peelCallDmd (CD {sd = s, ud = u})
  = case (s, u) of
      (SCall s', UCall c u') -> (CD { sd = s',       ud = u' },   (False, c))
      (SCall s', _)          -> (CD { sd = s',       ud = Used }, (False, Many))
      (HyperStr, UCall c u') -> (CD { sd = HyperStr, ud = u' },   (False, c))
      (HyperStr, _)          -> (CD { sd = HyperStr, ud = Used }, (False, Many))
      (_,        UCall c u') -> (CD { sd = HeadStr,  ud = u' },   (True,  c))
      (_,        _)          -> (CD { sd = HeadStr,  ud = Used }, (True,  Many))
       -- The _ cases for usage includes UHead which seems a bit wrong
       -- because the body isn't used at all!
       -- c.f. the Abs case in toCleanDmd

-- Peels that multiple nestings of calls clean demand and also returns
-- whether it was unsaturated (separately for strictness and usage
-- see Note [Demands from unsaturated function calls]
peelManyCalls :: Int -> CleanDemand -> DeferAndUse
peelManyCalls n (CD { sd = str, ud = abs })
  = (go_str n str, go_abs n abs)
  where
    go_str :: Int -> StrDmd -> Bool  -- True <=> unsaturated, defer
    go_str 0 _          = False
    go_str _ HyperStr   = False -- == go_str (n-1) HyperStr, as HyperStr = Call(HyperStr)
    go_str n (SCall d') = go_str (n-1) d'
    go_str _ _          = True

    go_abs :: Int -> UseDmd -> Count -- Many <=> unsaturated, or at least
    go_abs 0 _              = One    --          one UCall Many in the demand
    go_abs n (UCall One d') = go_abs (n-1) d'
    go_abs _ _              = Many
\end{code}

Note [Demands from unsaturated function calls]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Consider a demand transformer d1 -> d2 -> r for f.
If a sufficiently detailed demand is fed into this transformer,
e.g <C(C(S)), C1(C1(S))> arising from "f x1 x2" in a strict, use-once context,
then d1 and d2 is precisely the demand unleashed onto x1 and x2 (similar for
the free variable environment) and furthermore the result information r is the
one we want to use.

But the demand fed into f might be less than <C(C(S)), C1(C1(S))>. There are a few cases:
 * Not enough demand on the strictness side:
   - In that case, we need to zap all strictness in the demand on arguments and
     free variables.
   - Furthermore, we need to remove CPR information (after all, "f x1" surely
     does not return a constructor).
   - And finally, if r said that f would (possible or definitely) diverge when
     called with two arguments, then "f x1" may diverge. So we use topRes here.
     (We could return "Converges NoCPR" if f would converge for sure, but that
     information would currently not be useful in any way.)
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

\begin{code}
peelFV :: DmdType -> Var -> (DmdType, Demand)
peelFV (DmdType fv ds res) id = -- pprTrace "rfv" (ppr id <+> ppr dmd $$ ppr fv)
                               (DmdType fv' ds res, dmd)
  where
  fv' = fv `delVarEnv` id
  -- See note [Default demand on free variables]
  dmd  = lookupVarEnv fv id `orElse` defaultDmd res

addDemand :: Demand -> DmdType -> DmdType
addDemand dmd (DmdType fv ds res) = DmdType fv (dmd:ds) res
\end{code}

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
E, but just retuned botType.  

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

%************************************************************************
%*                                                                      *
                     Demand signatures
%*                                                                      *
%************************************************************************

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

If an Id is applied to less arguments than its arity, it means that
the demand on the function at a call site is weaker than the vanilla
call demand, used for signature inference. Therefore we place a top
demand on all arguments. Otherwise, the demand is specified by Id's
signature.

For example, the demand transformer described by the DmdType
                DmdType {x -> <S(LL),U(UU)>} [V,A] Top
says that when the function is applied to two arguments, it
unleashes demand <S(LL),U(UU)> on the free var x, V on the first arg,
and A on the second.  

If this same function is applied to one arg, all we can say is that it
uses x with <L,U>, and its arg with demand <L,U>.

\begin{code}
newtype StrictSig = StrictSig DmdType
                  deriving( Eq )

instance Outputable StrictSig where
   ppr (StrictSig ty) = ppr ty

mkStrictSig :: DmdType -> StrictSig
mkStrictSig dmd_ty = StrictSig dmd_ty

mkClosedStrictSig :: [Demand] -> DmdResult -> StrictSig
mkClosedStrictSig ds res = mkStrictSig (DmdType emptyDmdEnv ds res)

splitStrictSig :: StrictSig -> ([Demand], DmdResult)
splitStrictSig (StrictSig (DmdType _ dmds res)) = (dmds, res)

increaseStrictSigArity :: Int -> StrictSig -> StrictSig
-- Add extra arguments to a strictness signature
increaseStrictSigArity arity_increase (StrictSig (DmdType env dmds res))
  = StrictSig (DmdType env (replicate arity_increase topDmd ++ dmds) res)

isNopSig :: StrictSig -> Bool
isNopSig (StrictSig ty) = isNopDmdType ty

isBottomingSig :: StrictSig -> Bool
isBottomingSig (StrictSig (DmdType _ _ res)) = isBotRes res

nopSig, botSig :: StrictSig
nopSig = StrictSig nopDmdType
botSig = StrictSig botDmdType

cprProdSig :: Arity -> StrictSig
cprProdSig arity = StrictSig (cprProdDmdType arity)

argsOneShots :: StrictSig -> Arity -> [[OneShotInfo]]
argsOneShots (StrictSig (DmdType _ arg_ds _)) n_val_args
  = go arg_ds
  where
    good_one_shot
      | arg_ds `lengthExceeds` n_val_args = ProbOneShot
      | otherwise                         = OneShotLam

    go []               = []
    go (arg_d : arg_ds) = argOneShots good_one_shot arg_d `cons` go arg_ds

    cons [] [] = []
    cons a  as = a:as

argOneShots :: OneShotInfo -> JointDmd -> [OneShotInfo]
argOneShots one_shot_info (JD { absd = usg })
  = case usg of
      Use _ arg_usg -> go arg_usg
      _             -> []
  where
    go (UCall One  u) = one_shot_info : go u
    go (UCall Many u) = NoOneShotInfo : go u
    go _              = []

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
                             (CD { sd = str, ud = abs })
  | Just str_dmds <- go_str arity str
  , Just abs_dmds <- go_abs arity abs
  = DmdType emptyDmdEnv (mkJointDmds str_dmds abs_dmds) con_res
                -- Must remember whether it's a product, hence con_res, not TopRes

  | otherwise   -- Not saturated
  = nopDmdType
  where
    go_str 0 dmd        = Just (splitStrProdDmd arity dmd)
    go_str n (SCall s') = go_str (n-1) s'
    go_str n HyperStr   = go_str (n-1) HyperStr
    go_str _ _          = Nothing

    go_abs 0 dmd            = Just (splitUseProdDmd arity dmd)
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
\end{code}

Note [Demand transformer for a dictionary selector]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If we evaluate (op dict-expr) under demand 'd', then we can push the demand 'd'
into the appropriate field of the dictionary. What *is* the appropriate field?
We just look at the strictness signature of the class op, which will be
something like: U(AAASAAAAA).  Then replace the 'S' by the demand 'd'.

For single-method classes, which are represented by newtypes the signature 
of 'op' won't look like U(...), so the splitProdDmd_maybe will fail.
That's fine: if we are doing strictness analysis we are also doing inling,
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

Note [Non-full application] 
~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
If a function having bottom as its demand result is applied to a less
number of arguments than its syntactic arity, we cannot say for sure
that it is going to diverge. This is the reason why we use the
function appIsBottom, which, given a strictness signature and a number
of arguments, says conservatively if the function is going to diverge
or not.

\begin{code}
-- appIsBottom returns true if an application to n args would diverge
appIsBottom :: StrictSig -> Int -> Bool
appIsBottom (StrictSig (DmdType _ ds res)) n
            | isBotRes res                      = not $ lengthExceeds ds n 
appIsBottom _                                 _ = False

seqStrictSig :: StrictSig -> ()
seqStrictSig (StrictSig ty) = seqDmdType ty

-- Used for printing top-level strictness pragmas in interface files
pprIfaceStrictSig :: StrictSig -> SDoc
pprIfaceStrictSig (StrictSig (DmdType _ dmds res))
  = hcat (map ppr dmds) <> ppr res
\end{code}

Zap absence or one-shot information, under control of flags

\begin{code}
zapDemand :: DynFlags -> Demand -> Demand
zapDemand dflags dmd 
  | Just kfs <- killFlags dflags = zap_dmd kfs dmd
  | otherwise                    = dmd

zapStrictSig :: DynFlags -> StrictSig -> StrictSig
zapStrictSig dflags sig@(StrictSig (DmdType env ds r)) 
  | Just kfs <- killFlags dflags = StrictSig (DmdType env (map (zap_dmd kfs) ds) r)
  | otherwise                    = sig

type KillFlags = (Bool, Bool)

killFlags :: DynFlags -> Maybe KillFlags
killFlags dflags 
  | not kill_abs && not kill_one_shot = Nothing
  | otherwise                         = Just (kill_abs, kill_one_shot)
  where
    kill_abs      = gopt Opt_KillAbsence dflags
    kill_one_shot = gopt Opt_KillOneShot dflags
      
zap_dmd :: KillFlags -> Demand -> Demand
zap_dmd kfs (JD {strd = s, absd = u}) = JD {strd = s, absd = zap_musg kfs u}

zap_musg :: KillFlags -> MaybeUsed -> MaybeUsed
zap_musg (kill_abs, _) Abs 
  | kill_abs  = useTop
  | otherwise = Abs
zap_musg kfs (Use c u) = Use (zap_count kfs c) (zap_usg kfs u)

zap_count :: KillFlags -> Count -> Count
zap_count (_, kill_one_shot) c
  | kill_one_shot = Many
  | otherwise     = c

zap_usg :: KillFlags -> UseDmd -> UseDmd
zap_usg kfs (UCall c u) = UCall (zap_count kfs c) (zap_usg kfs u)
zap_usg kfs (UProd us)  = UProd (map (zap_musg kfs) us)
zap_usg _   u           = u
\end{code}

\begin{code}
-- If the argument is a used non-newtype dictionary, give it strict
-- demand. Also split the product type & demand and recur in order to
-- similarly strictify the argument's contained used non-newtype
-- superclass dictionaries. We use the demand as our recursive measure
-- to guarantee termination.
strictifyDictDmd :: Type -> Demand -> Demand
strictifyDictDmd ty dmd = case absd dmd of
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
               CD {sd = s,ud = a} -> JD (Str s) (Use n a)
             -- TODO could optimize with an aborting variant of zipWith since
             -- the superclass dicts are always a prefix
  _ -> dmd -- unused or not a dictionary
\end{code}

Note [HyperStr and Use demands]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The information "HyperStr" needs to be in the strictness signature, and not in
the demand signature, because we still want to know about the demand on things. Consider

    f (x,y) True  = error (show x)
    f (x,y) False = x+1

The signature of f should be <S(SL),1*U(1*U(U),A)><S,1*U>m. If we were not
distinguishing the uses on x and y in the True case, we could either not figure
out how deeply we can unpack x, or that we do not have to pass y.


%************************************************************************
%*                                                                      *
                     Serialisation
%*                                                                      *
%************************************************************************


\begin{code}
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

instance Binary MaybeStr where
    put_ bh Lazy         = do 
            putByte bh 0
    put_ bh (Str s)    = do 
            putByte bh 1
            put_ bh s

    get  bh = do
            h <- getByte bh
            case h of 
              0 -> return Lazy
              _ -> do s  <- get bh
                      return $ Str s

instance Binary Count where
    put_ bh One  = do putByte bh 0
    put_ bh Many = do putByte bh 1
    
    get  bh = do h <- getByte bh
                 case h of
                   0 -> return One
                   _ -> return Many   

instance Binary MaybeUsed where
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

instance Binary JointDmd where
    put_ bh (JD {strd = x, absd = y}) = do put_ bh x; put_ bh y
    get  bh = do 
              x <- get bh
              y <- get bh
              return $ mkJointDmd x y

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
  put_ bh Diverges      = putByte bh 2

  get bh = do { h <- getByte bh
              ; case h of
                  0 -> do { c <- get bh; return (Dunno c) }
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
\end{code}
