{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1993-1998

\section[IdInfo]{@IdInfos@: Non-essential information about @Ids@}

(And a pretty good illustration of quite a few things wrong with
Haskell. [WDP 94/11])
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}

{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}

module GHC.Types.Id.Info (
        -- * The IdDetails type
        IdDetails(..), pprIdDetails, coVarDetails, isCoVarDetails,
        JoinArity, isJoinIdDetails_maybe,
        RecSelParent(..),

        -- * The IdInfo type
        IdInfo,         -- Abstract
        vanillaIdInfo, noCafIdInfo,

        -- ** The OneShotInfo type
        OneShotInfo(..),
        oneShotInfo, noOneShotInfo, hasNoOneShotInfo,
        setOneShotInfo,

        -- ** Zapping various forms of Info
        zapLamInfo, zapFragileInfo,
        zapDemandInfo, zapUsageInfo, zapUsageEnvInfo, zapUsedOnceInfo,
        zapTailCallInfo, zapCallArityInfo, zapUnfolding,

        -- ** The ArityInfo type
        ArityInfo,
        unknownArity,
        arityInfo, setArityInfo, ppArityInfo,

        callArityInfo, setCallArityInfo,

        -- ** Demand and strictness Info
        strictnessInfo, setStrictnessInfo,
        cprInfo, setCprInfo,
        demandInfo, setDemandInfo, pprStrictness,

        -- ** Unfolding Info
        unfoldingInfo, setUnfoldingInfo,

        -- ** The InlinePragInfo type
        InlinePragInfo,
        inlinePragInfo, setInlinePragInfo,

        -- ** The OccInfo type
        OccInfo(..),
        isDeadOcc, isStrongLoopBreaker, isWeakLoopBreaker,
        occInfo, setOccInfo,

        InsideLam(..), OneBranch(..),

        TailCallInfo(..),
        tailCallInfo, isAlwaysTailCalled,

        -- ** The RuleInfo type
        RuleInfo(..),
        emptyRuleInfo,
        isEmptyRuleInfo, ruleInfoFreeVars,
        ruleInfoRules, setRuleInfoHead,
        ruleInfo, setRuleInfo,

        -- ** The CAFInfo type
        CafInfo(..),
        ppCafInfo, mayHaveCafRefs,
        cafInfo, setCafInfo,

        -- ** Tick-box Info
        TickBoxOp(..), TickBoxId,

        -- ** Levity info
        LevityInfo, levityInfo, setNeverLevPoly, setLevityInfoWithType,
        isNeverLevPolyIdInfo
    ) where

#include "HsVersions.h"

import GhcPrelude

import GHC.Core hiding( hasCoreUnfolding )
import GHC.Core( hasCoreUnfolding )

import GHC.Core.Class
import {-# SOURCE #-} PrimOp (PrimOp)
import GHC.Types.Name
import GHC.Types.Var.Set
import GHC.Types.Basic
import GHC.Core.DataCon
import GHC.Core.TyCon
import GHC.Core.PatSyn
import GHC.Core.Type
import GHC.Types.ForeignCall
import Outputable
import GHC.Types.Module
import GHC.Types.Demand
import GHC.Types.Cpr
import Util

-- infixl so you can say (id `set` a `set` b)
infixl  1 `setRuleInfo`,
          `setArityInfo`,
          `setInlinePragInfo`,
          `setUnfoldingInfo`,
          `setOneShotInfo`,
          `setOccInfo`,
          `setCafInfo`,
          `setStrictnessInfo`,
          `setCprInfo`,
          `setDemandInfo`,
          `setNeverLevPoly`,
          `setLevityInfoWithType`

{-
************************************************************************
*                                                                      *
                     IdDetails
*                                                                      *
************************************************************************
-}

-- | Identifier Details
--
-- The 'IdDetails' of an 'Id' give stable, and necessary,
-- information about the Id.
data IdDetails
  = VanillaId

  -- | The 'Id' for a record selector
  | RecSelId
    { sel_tycon   :: RecSelParent
    , sel_naughty :: Bool       -- True <=> a "naughty" selector which can't actually exist, for example @x@ in:
                                --    data T = forall a. MkT { x :: a }
    }                           -- See Note [Naughty record selectors] in TcTyClsDecls

  | DataConWorkId DataCon       -- ^ The 'Id' is for a data constructor /worker/
  | DataConWrapId DataCon       -- ^ The 'Id' is for a data constructor /wrapper/

                                -- [the only reasons we need to know is so that
                                --  a) to support isImplicitId
                                --  b) when desugaring a RecordCon we can get
                                --     from the Id back to the data con]
  | ClassOpId Class             -- ^ The 'Id' is a superclass selector,
                                -- or class operation of a class

  | PrimOpId PrimOp             -- ^ The 'Id' is for a primitive operator
  | FCallId ForeignCall         -- ^ The 'Id' is for a foreign call.
                                -- Type will be simple: no type families, newtypes, etc

  | TickBoxOpId TickBoxOp       -- ^ The 'Id' is for a HPC tick box (both traditional and binary)

  | DFunId Bool                 -- ^ A dictionary function.
       -- Bool = True <=> the class has only one method, so may be
       --                  implemented with a newtype, so it might be bad
       --                  to be strict on this dictionary

  | CoVarId    -- ^ A coercion variable
               -- This only covers /un-lifted/ coercions, of type
               -- (t1 ~# t2) or (t1 ~R# t2), not their lifted variants
  | JoinId JoinArity           -- ^ An 'Id' for a join point taking n arguments
       -- Note [Join points] in GHC.Core

-- | Recursive Selector Parent
data RecSelParent = RecSelData TyCon | RecSelPatSyn PatSyn deriving Eq
  -- Either `TyCon` or `PatSyn` depending
  -- on the origin of the record selector.
  -- For a data type family, this is the
  -- /instance/ 'TyCon' not the family 'TyCon'

instance Outputable RecSelParent where
  ppr p = case p of
            RecSelData ty_con -> ppr ty_con
            RecSelPatSyn ps   -> ppr ps

-- | Just a synonym for 'CoVarId'. Written separately so it can be
-- exported in the hs-boot file.
coVarDetails :: IdDetails
coVarDetails = CoVarId

-- | Check if an 'IdDetails' says 'CoVarId'.
isCoVarDetails :: IdDetails -> Bool
isCoVarDetails CoVarId = True
isCoVarDetails _       = False

isJoinIdDetails_maybe :: IdDetails -> Maybe JoinArity
isJoinIdDetails_maybe (JoinId join_arity) = Just join_arity
isJoinIdDetails_maybe _                   = Nothing

instance Outputable IdDetails where
    ppr = pprIdDetails

pprIdDetails :: IdDetails -> SDoc
pprIdDetails VanillaId = empty
pprIdDetails other     = brackets (pp other)
 where
   pp VanillaId               = panic "pprIdDetails"
   pp (DataConWorkId _)       = text "DataCon"
   pp (DataConWrapId _)       = text "DataConWrapper"
   pp (ClassOpId {})          = text "ClassOp"
   pp (PrimOpId _)            = text "PrimOp"
   pp (FCallId _)             = text "ForeignCall"
   pp (TickBoxOpId _)         = text "TickBoxOp"
   pp (DFunId nt)             = text "DFunId" <> ppWhen nt (text "(nt)")
   pp (RecSelId { sel_naughty = is_naughty })
                              = brackets $ text "RecSel" <>
                                           ppWhen is_naughty (text "(naughty)")
   pp CoVarId                 = text "CoVarId"
   pp (JoinId arity)          = text "JoinId" <> parens (int arity)

{-
************************************************************************
*                                                                      *
\subsection{The main IdInfo type}
*                                                                      *
************************************************************************
-}

-- | Identifier Information
--
-- An 'IdInfo' gives /optional/ information about an 'Id'.  If
-- present it never lies, but it may not be present, in which case there
-- is always a conservative assumption which can be made.
--
-- Two 'Id's may have different info even though they have the same
-- 'Unique' (and are hence the same 'Id'); for example, one might lack
-- the properties attached to the other.
--
-- Most of the 'IdInfo' gives information about the value, or definition, of
-- the 'Id', independent of its usage. Exceptions to this
-- are 'demandInfo', 'occInfo', 'oneShotInfo' and 'callArityInfo'.
--
-- Performance note: when we update 'IdInfo', we have to reallocate this
-- entire record, so it is a good idea not to let this data structure get
-- too big.
data IdInfo
  = IdInfo {
        arityInfo       :: !ArityInfo,
        -- ^ 'Id' arity, as computed by 'GHC.Core.Arity'. Specifies how many
        -- arguments this 'Id' has to be applied to before it doesn any
        -- meaningful work.
        ruleInfo        :: RuleInfo,
        -- ^ Specialisations of the 'Id's function which exist.
        -- See Note [Specialisations and RULES in IdInfo]
        unfoldingInfo   :: Unfolding,
        -- ^ The 'Id's unfolding
        cafInfo         :: CafInfo,
        -- ^ 'Id' CAF info
        oneShotInfo     :: OneShotInfo,
        -- ^ Info about a lambda-bound variable, if the 'Id' is one
        inlinePragInfo  :: InlinePragma,
        -- ^ Any inline pragma attached to the 'Id'
        occInfo         :: OccInfo,
        -- ^ How the 'Id' occurs in the program
        strictnessInfo  :: StrictSig,
        -- ^ A strictness signature. Digests how a function uses its arguments
        -- if applied to at least 'arityInfo' arguments.
        cprInfo         :: CprSig,
        -- ^ Information on whether the function will ultimately return a
        -- freshly allocated constructor.
        demandInfo      :: Demand,
        -- ^ ID demand information
        callArityInfo   :: !ArityInfo,
        -- ^ How this is called. This is the number of arguments to which a
        -- binding can be eta-expanded without losing any sharing.
        -- n <=> all calls have at least n arguments
        levityInfo      :: LevityInfo
        -- ^ when applied, will this Id ever have a levity-polymorphic type?
    }

-- Setters

setRuleInfo :: IdInfo -> RuleInfo -> IdInfo
setRuleInfo       info sp = sp `seq` info { ruleInfo = sp }
setInlinePragInfo :: IdInfo -> InlinePragma -> IdInfo
setInlinePragInfo info pr = pr `seq` info { inlinePragInfo = pr }
setOccInfo :: IdInfo -> OccInfo -> IdInfo
setOccInfo        info oc = oc `seq` info { occInfo = oc }
        -- Try to avoid space leaks by seq'ing

setUnfoldingInfo :: IdInfo -> Unfolding -> IdInfo
setUnfoldingInfo info uf
  = -- We don't seq the unfolding, as we generate intermediate
    -- unfoldings which are just thrown away, so evaluating them is a
    -- waste of time.
    -- seqUnfolding uf `seq`
    info { unfoldingInfo = uf }

setArityInfo :: IdInfo -> ArityInfo -> IdInfo
setArityInfo      info ar  = info { arityInfo = ar  }
setCallArityInfo :: IdInfo -> ArityInfo -> IdInfo
setCallArityInfo info ar  = info { callArityInfo = ar  }
setCafInfo :: IdInfo -> CafInfo -> IdInfo
setCafInfo        info caf = info { cafInfo = caf }

setOneShotInfo :: IdInfo -> OneShotInfo -> IdInfo
setOneShotInfo      info lb = {-lb `seq`-} info { oneShotInfo = lb }

setDemandInfo :: IdInfo -> Demand -> IdInfo
setDemandInfo info dd = dd `seq` info { demandInfo = dd }

setStrictnessInfo :: IdInfo -> StrictSig -> IdInfo
setStrictnessInfo info dd = dd `seq` info { strictnessInfo = dd }

setCprInfo :: IdInfo -> CprSig -> IdInfo
setCprInfo info cpr = cpr `seq` info { cprInfo = cpr }

-- | Basic 'IdInfo' that carries no useful information whatsoever
vanillaIdInfo :: IdInfo
vanillaIdInfo
  = IdInfo {
            cafInfo             = vanillaCafInfo,
            arityInfo           = unknownArity,
            ruleInfo            = emptyRuleInfo,
            unfoldingInfo       = noUnfolding,
            oneShotInfo         = NoOneShotInfo,
            inlinePragInfo      = defaultInlinePragma,
            occInfo             = noOccInfo,
            demandInfo          = topDmd,
            strictnessInfo      = emptySig topDiv,
            cprInfo             = topCprSig,
            callArityInfo       = unknownArity,
            levityInfo          = NoLevityInfo
           }

-- | More informative 'IdInfo' we can use when we know the 'Id' has no CAF references
noCafIdInfo :: IdInfo
noCafIdInfo  = vanillaIdInfo `setCafInfo`    NoCafRefs
        -- Used for built-in type Ids in GHC.Types.Id.Make.

{-
************************************************************************
*                                                                      *
\subsection[arity-IdInfo]{Arity info about an @Id@}
*                                                                      *
************************************************************************

For locally-defined Ids, the code generator maintains its own notion
of their arities; so it should not be asking...  (but other things
besides the code-generator need arity info!)
-}

-- | Arity Information
--
-- An 'ArityInfo' of @n@ tells us that partial application of this
-- 'Id' to up to @n-1@ value arguments does essentially no work.
--
-- That is not necessarily the same as saying that it has @n@ leading
-- lambdas, because coerces may get in the way.
--
-- The arity might increase later in the compilation process, if
-- an extra lambda floats up to the binding site.
type ArityInfo = Arity

-- | It is always safe to assume that an 'Id' has an arity of 0
unknownArity :: Arity
unknownArity = 0

ppArityInfo :: Int -> SDoc
ppArityInfo 0 = empty
ppArityInfo n = hsep [text "Arity", int n]

{-
************************************************************************
*                                                                      *
\subsection{Inline-pragma information}
*                                                                      *
************************************************************************
-}

-- | Inline Pragma Information
--
-- Tells when the inlining is active.
-- When it is active the thing may be inlined, depending on how
-- big it is.
--
-- If there was an @INLINE@ pragma, then as a separate matter, the
-- RHS will have been made to look small with a Core inline 'Note'
--
-- The default 'InlinePragInfo' is 'AlwaysActive', so the info serves
-- entirely as a way to inhibit inlining until we want it
type InlinePragInfo = InlinePragma

{-
************************************************************************
*                                                                      *
               Strictness
*                                                                      *
************************************************************************
-}

pprStrictness :: StrictSig -> SDoc
pprStrictness sig = ppr sig

{-
************************************************************************
*                                                                      *
        RuleInfo
*                                                                      *
************************************************************************

Note [Specialisations and RULES in IdInfo]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Generally speaking, a GlobalId has an *empty* RuleInfo.  All their
RULES are contained in the globally-built rule-base.  In principle,
one could attach the to M.f the RULES for M.f that are defined in M.
But we don't do that for instance declarations and so we just treat
them all uniformly.

The EXCEPTION is PrimOpIds, which do have rules in their IdInfo. That is
just for convenience really.

However, LocalIds may have non-empty RuleInfo.  We treat them
differently because:
  a) they might be nested, in which case a global table won't work
  b) the RULE might mention free variables, which we use to keep things alive

In GHC.Iface.Tidy, when the LocalId becomes a GlobalId, its RULES are stripped off
and put in the global list.
-}

-- | Rule Information
--
-- Records the specializations of this 'Id' that we know about
-- in the form of rewrite 'CoreRule's that target them
data RuleInfo
  = RuleInfo
        [CoreRule]
        DVarSet         -- Locally-defined free vars of *both* LHS and RHS
                        -- of rules.  I don't think it needs to include the
                        -- ru_fn though.
                        -- Note [Rule dependency info] in OccurAnal

-- | Assume that no specializations exist: always safe
emptyRuleInfo :: RuleInfo
emptyRuleInfo = RuleInfo [] emptyDVarSet

isEmptyRuleInfo :: RuleInfo -> Bool
isEmptyRuleInfo (RuleInfo rs _) = null rs

-- | Retrieve the locally-defined free variables of both the left and
-- right hand sides of the specialization rules
ruleInfoFreeVars :: RuleInfo -> DVarSet
ruleInfoFreeVars (RuleInfo _ fvs) = fvs

ruleInfoRules :: RuleInfo -> [CoreRule]
ruleInfoRules (RuleInfo rules _) = rules

-- | Change the name of the function the rule is keyed on on all of the 'CoreRule's
setRuleInfoHead :: Name -> RuleInfo -> RuleInfo
setRuleInfoHead fn (RuleInfo rules fvs)
  = RuleInfo (map (setRuleIdName fn) rules) fvs

{-
************************************************************************
*                                                                      *
\subsection[CG-IdInfo]{Code generator-related information}
*                                                                      *
************************************************************************
-}

-- CafInfo is used to build Static Reference Tables (see simplStg/SRT.hs).

-- | Constant applicative form Information
--
-- Records whether an 'Id' makes Constant Applicative Form references
data CafInfo
        = MayHaveCafRefs                -- ^ Indicates that the 'Id' is for either:
                                        --
                                        -- 1. A function or static constructor
                                        --    that refers to one or more CAFs, or
                                        --
                                        -- 2. A real live CAF

        | NoCafRefs                     -- ^ A function or static constructor
                                        -- that refers to no CAFs.
        deriving (Eq, Ord)

-- | Assumes that the 'Id' has CAF references: definitely safe
vanillaCafInfo :: CafInfo
vanillaCafInfo = MayHaveCafRefs

mayHaveCafRefs :: CafInfo -> Bool
mayHaveCafRefs  MayHaveCafRefs = True
mayHaveCafRefs _               = False

instance Outputable CafInfo where
   ppr = ppCafInfo

ppCafInfo :: CafInfo -> SDoc
ppCafInfo NoCafRefs = text "NoCafRefs"
ppCafInfo MayHaveCafRefs = empty

{-
************************************************************************
*                                                                      *
\subsection{Bulk operations on IdInfo}
*                                                                      *
************************************************************************
-}

-- | This is used to remove information on lambda binders that we have
-- setup as part of a lambda group, assuming they will be applied all at once,
-- but turn out to be part of an unsaturated lambda as in e.g:
--
-- > (\x1. \x2. e) arg1
zapLamInfo :: IdInfo -> Maybe IdInfo
zapLamInfo info@(IdInfo {occInfo = occ, demandInfo = demand})
  | is_safe_occ occ && is_safe_dmd demand
  = Nothing
  | otherwise
  = Just (info {occInfo = safe_occ, demandInfo = topDmd})
  where
        -- The "unsafe" occ info is the ones that say I'm not in a lambda
        -- because that might not be true for an unsaturated lambda
    is_safe_occ occ | isAlwaysTailCalled occ           = False
    is_safe_occ (OneOcc { occ_in_lam = NotInsideLam }) = False
    is_safe_occ _other                                 = True

    safe_occ = case occ of
                 OneOcc{} -> occ { occ_in_lam = IsInsideLam
                                 , occ_tail   = NoTailCallInfo }
                 IAmALoopBreaker{}
                          -> occ { occ_tail   = NoTailCallInfo }
                 _other   -> occ

    is_safe_dmd dmd = not (isStrictDmd dmd)

-- | Remove all demand info on the 'IdInfo'
zapDemandInfo :: IdInfo -> Maybe IdInfo
zapDemandInfo info = Just (info {demandInfo = topDmd})

-- | Remove usage (but not strictness) info on the 'IdInfo'
zapUsageInfo :: IdInfo -> Maybe IdInfo
zapUsageInfo info = Just (info {demandInfo = zapUsageDemand (demandInfo info)})

-- | Remove usage environment info from the strictness signature on the 'IdInfo'
zapUsageEnvInfo :: IdInfo -> Maybe IdInfo
zapUsageEnvInfo info
    | hasDemandEnvSig (strictnessInfo info)
    = Just (info {strictnessInfo = zapUsageEnvSig (strictnessInfo info)})
    | otherwise
    = Nothing

zapUsedOnceInfo :: IdInfo -> Maybe IdInfo
zapUsedOnceInfo info
    = Just $ info { strictnessInfo = zapUsedOnceSig    (strictnessInfo info)
                  , demandInfo     = zapUsedOnceDemand (demandInfo     info) }

zapFragileInfo :: IdInfo -> Maybe IdInfo
-- ^ Zap info that depends on free variables
zapFragileInfo info@(IdInfo { occInfo = occ, unfoldingInfo = unf })
  = new_unf `seq`  -- The unfolding field is not (currently) strict, so we
                   -- force it here to avoid a (zapFragileUnfolding unf) thunk
                   -- which might leak space
    Just (info `setRuleInfo` emptyRuleInfo
               `setUnfoldingInfo` new_unf
               `setOccInfo`       zapFragileOcc occ)
  where
    new_unf = zapFragileUnfolding unf

zapFragileUnfolding :: Unfolding -> Unfolding
zapFragileUnfolding unf
 | hasCoreUnfolding unf = noUnfolding
 | otherwise            = unf

zapUnfolding :: Unfolding -> Unfolding
-- Squash all unfolding info, preserving only evaluated-ness
zapUnfolding unf | isEvaldUnfolding unf = evaldUnfolding
                 | otherwise            = noUnfolding

zapTailCallInfo :: IdInfo -> Maybe IdInfo
zapTailCallInfo info
  = case occInfo info of
      occ | isAlwaysTailCalled occ -> Just (info `setOccInfo` safe_occ)
          | otherwise              -> Nothing
        where
          safe_occ = occ { occ_tail = NoTailCallInfo }

zapCallArityInfo :: IdInfo -> IdInfo
zapCallArityInfo info = setCallArityInfo info 0

{-
************************************************************************
*                                                                      *
\subsection{TickBoxOp}
*                                                                      *
************************************************************************
-}

type TickBoxId = Int

-- | Tick box for Hpc-style coverage
data TickBoxOp
   = TickBox Module {-# UNPACK #-} !TickBoxId

instance Outputable TickBoxOp where
    ppr (TickBox mod n)         = text "tick" <+> ppr (mod,n)

{-
************************************************************************
*                                                                      *
   Levity
*                                                                      *
************************************************************************

Note [Levity info]
~~~~~~~~~~~~~~~~~~

Ids store whether or not they can be levity-polymorphic at any amount
of saturation. This is helpful in optimizing the levity-polymorphism check
done in the desugarer, where we can usually learn that something is not
levity-polymorphic without actually figuring out its type. See
isExprLevPoly in GHC.Core.Utils for where this info is used. Storing
this is required to prevent perf/compiler/T5631 from blowing up.

-}

-- See Note [Levity info]
data LevityInfo = NoLevityInfo  -- always safe
                | NeverLevityPolymorphic
  deriving Eq

instance Outputable LevityInfo where
  ppr NoLevityInfo           = text "NoLevityInfo"
  ppr NeverLevityPolymorphic = text "NeverLevityPolymorphic"

-- | Marks an IdInfo describing an Id that is never levity polymorphic (even when
-- applied). The Type is only there for checking that it's really never levity
-- polymorphic
setNeverLevPoly :: HasDebugCallStack => IdInfo -> Type -> IdInfo
setNeverLevPoly info ty
  = ASSERT2( not (resultIsLevPoly ty), ppr ty )
    info { levityInfo = NeverLevityPolymorphic }

setLevityInfoWithType :: IdInfo -> Type -> IdInfo
setLevityInfoWithType info ty
  | not (resultIsLevPoly ty)
  = info { levityInfo = NeverLevityPolymorphic }
  | otherwise
  = info

isNeverLevPolyIdInfo :: IdInfo -> Bool
isNeverLevPolyIdInfo info
  | NeverLevityPolymorphic <- levityInfo info = True
  | otherwise                                 = False
