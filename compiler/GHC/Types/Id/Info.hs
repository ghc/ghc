{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1993-1998

\section[IdInfo]{@IdInfos@: Non-essential information about @Ids@}

(And a pretty good illustration of quite a few things wrong with
Haskell. [WDP 94/11])
-}

{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}

module GHC.Types.Id.Info (
        -- * The IdDetails type
        IdDetails(..), pprIdDetails, coVarDetails, isCoVarDetails,
        JoinArity, isJoinIdDetails_maybe,
        RecSelParent(..), recSelParentName, recSelFirstConName,
        recSelParentCons, idDetailsConcreteTvs,

        -- * The IdInfo type
        IdInfo,         -- Abstract
        vanillaIdInfo, noCafIdInfo,

        -- ** The OneShotInfo type
        OneShotInfo(..),
        oneShotInfo, noOneShotInfo, hasNoOneShotInfo,
        setOneShotInfo,

        -- ** Zapping various forms of Info
        zapLamInfo, zapFragileInfo,
        lazifyDemandInfo, floatifyDemandInfo,
        zapUsageInfo, zapUsageEnvInfo, zapUsedOnceInfo,
        zapTailCallInfo, zapCallArityInfo, trimUnfolding,

        -- ** The ArityInfo type
        ArityInfo,
        unknownArity,
        arityInfo, setArityInfo, ppArityInfo,

        callArityInfo, setCallArityInfo,

        -- ** Demand and strictness Info
        dmdSigInfo, setDmdSigInfo,
        cprSigInfo, setCprSigInfo,
        demandInfo, setDemandInfo, pprStrictness,

        -- ** Unfolding Info
        realUnfoldingInfo, unfoldingInfo, setUnfoldingInfo, hasInlineUnfolding,

        -- ** The InlinePragInfo type
        InlinePragInfo,
        inlinePragInfo, setInlinePragInfo,

        -- ** The OccInfo type
        OccInfo(..),
        isDeadOcc, isStrongLoopBreaker, isWeakLoopBreaker,
        occInfo, setOccInfo,

        InsideLam(..), BranchCount,

        TailCallInfo(..),
        tailCallInfo, isAlwaysTailCalled,

        -- ** The RuleInfo type
        RuleInfo(..),
        emptyRuleInfo,
        isEmptyRuleInfo, ruleInfoFreeVars,
        ruleInfoRules, setRuleInfoHead,
        ruleInfo, setRuleInfo, tagSigInfo,

        -- ** The CAFInfo type
        CafInfo(..),
        ppCafInfo, mayHaveCafRefs,
        cafInfo, setCafInfo,

        -- ** The LambdaFormInfo type
        LambdaFormInfo,
        lfInfo, setLFInfo, setTagSig,

        tagSig,

        -- ** Tick-box Info
        TickBoxOp(..), TickBoxId,
    ) where

import GHC.Prelude

import GHC.Core
import GHC.Core.Class
import {-# SOURCE #-} GHC.Builtin.PrimOps (PrimOp)
import GHC.Types.Name
import GHC.Types.Var.Set
import GHC.Types.Basic
import GHC.Core.DataCon
import GHC.Core.TyCon
import GHC.Core.Type (mkTyConApp)
import GHC.Core.PatSyn
import GHC.Core.ConLike
import GHC.Types.ForeignCall
import GHC.Unit.Module
import GHC.Types.Demand
import GHC.Types.Cpr
import {-# SOURCE #-} GHC.Tc.Utils.TcType ( ConcreteTyVars, noConcreteTyVars )

import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Stg.InferTags.TagSig
import GHC.StgToCmm.Types (LambdaFormInfo)

import Data.Data ( Data )
import Data.Word

-- infixl so you can say (id `set` a `set` b)
infixl  1 `setRuleInfo`,
          `setArityInfo`,
          `setInlinePragInfo`,
          `setUnfoldingInfo`,
          `setOneShotInfo`,
          `setOccInfo`,
          `setCafInfo`,
          `setDmdSigInfo`,
          `setCprSigInfo`,
          `setDemandInfo`,
          `setLFInfo`
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
    { sel_tycon      :: RecSelParent
    , sel_fieldLabel :: FieldLabel
    , sel_naughty    :: Bool    -- True <=> a "naughty" selector which can't actually exist, for example @x@ in:
                                --    data T = forall a. MkT { x :: a }
                                -- See Note [Naughty record selectors] in GHC.Tc.TyCl
    , sel_cons       :: ([ConLike], [ConLike])
                                -- If record selector is not defined for all constructors
                                -- of a parent type, this is the pair of lists of constructors that
                                -- it is and is not defined for. Otherwise, it's Nothing.
                                -- Cached here based on the RecSelParent.
    }                           -- See Note [Detecting incomplete record selectors] in GHC.HsToCore.Pmc

  | DataConWorkId DataCon       -- ^ The 'Id' is for a data constructor /worker/
  | DataConWrapId DataCon       -- ^ The 'Id' is for a data constructor /wrapper/

                                -- [the only reasons we need to know is so that
                                --  a) to support isImplicitId
                                --  b) when desugaring a RecordCon we can get
                                --     from the Id back to the data con]

  | ClassOpId                   -- ^ The 'Id' is a superclass selector or class operation
      Class                     --    for this class
      Bool                      --   True <=> given a non-bottom dictionary, the class op will
                                --            definitely return a non-bottom result
                                --   and Note [exprOkForSpeculation and type classes]
                                --       in GHC.Core.Utils

  -- | A representation-polymorphic pseudo-op.
  | RepPolyId
      { id_concrete_tvs :: ConcreteTyVars }
        -- ^ Which type variables of this representation-polymorphic 'Id
        -- should be instantiated to concrete type variables?
        --
        -- See Note [Representation-polymorphism checking built-ins]
        -- in GHC.Tc.Utils.Concrete.

  -- | The 'Id' is for a primitive operator.
  | PrimOpId
     { id_primop :: PrimOp
     , id_concrete_tvs :: ConcreteTyVars }
        -- ^ Which type variables of this primop should be instantiated
        -- to concrete type variables?
        --
        -- Only ever non-empty when the PrimOp has representation-polymorphic
        -- type variables.
        --
        -- See Note [Representation-polymorphism checking built-ins]
        -- in GHC.Tc.Utils.Concrete.

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
  | JoinId JoinArity (Maybe [CbvMark])
        -- ^ An 'Id' for a join point taking n arguments
        -- Note [Join points] in "GHC.Core"
        -- Can also work as a WorkerLikeId if given `CbvMark`s.
        -- See Note [CBV Function Ids]
        -- The [CbvMark] is always empty (and ignored) until after Tidy.
  | WorkerLikeId [CbvMark]
        -- ^ An 'Id' for a worker like function, which might expect some arguments to be
        -- passed both evaluated and tagged.
        -- Worker like functions are create by W/W and SpecConstr and we can expect that they
        -- aren't used unapplied.
        -- See Note [CBV Function Ids]
        -- See Note [Tag Inference]
        -- The [CbvMark] is always empty (and ignored) until after Tidy for ids from the current
        -- module.

idDetailsConcreteTvs :: IdDetails -> ConcreteTyVars
idDetailsConcreteTvs = \ case
    PrimOpId _ conc_tvs -> conc_tvs
    RepPolyId  conc_tvs -> conc_tvs
    DataConWorkId dc    -> dataConConcreteTyVars dc
    DataConWrapId dc    -> dataConConcreteTyVars dc
    _                   -> noConcreteTyVars


{- Note [CBV Function Ids]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
A WorkerLikeId essentially allows us to constrain the calling convention
for the given Id. Each such Id carries with it a list of CbvMarks
with each element representing a value argument. Arguments who have
a matching `MarkedCbv` entry in the list need to be passed evaluated+*properly tagged*.

CallByValueFunIds give us additional expressiveness which we use to improve
runtime. This is all part of the TagInference work. See also Note [Tag Inference].

They allows us to express the fact that an argument is not only evaluated to WHNF once we
entered it's RHS but also that an lifted argument is already *properly tagged* once we jump
into the RHS.
This means when e.g. branching on such an argument the RHS doesn't needed to perform
an eval check to ensure the argument isn't an indirection. All seqs on such an argument in
the functions body become no-ops as well.

The invariants around the arguments of call by value function like Ids are then:

* In any call `(f e1 .. en)`, if `f`'s i'th argument is marked `MarkedCbv`,
  then the caller must ensure that the i'th argument
  * points directly to the value (and hence is certainly evaluated before the call)
  * is a properly tagged pointer to that value

* The following functions (and only these functions) have `CbvMarks`:
  * Any `WorkerLikeId`
  * Some `JoinId` bindings.

This works analogous to the Strict Field Invariant. See also Note [Strict Field Invariant].

To make this work what we do is:
* During W/W and SpecConstr any worker/specialized binding we introduce
  is marked as a worker binding by `asWorkerLikeId`.
* W/W and SpecConstr further set OtherCon[] unfoldings on arguments which
  represent contents of a strict fields.
* During Tidy we look at all bindings.
  For any callByValueLike Id and join point we mark arguments as cbv if they
  Are strict. We don't do so for regular bindings.
  See Note [Use CBV semantics only for join points and workers] for why.
  We might have made some ids rhs *more* strict in order to make their arguments
  be passed CBV. See Note [Call-by-value for worker args] for why.
* During CorePrep calls to CallByValueFunIds are eta expanded.
* During Stg CodeGen:
  * When we see a call to a callByValueLike Id:
    * We check if all arguments marked to be passed unlifted are already tagged.
    * If they aren't we will wrap the call in case expressions which will evaluate+tag
      these arguments before jumping to the function.
* During Cmm codeGen:
  * When generating code for the RHS of a StrictWorker binding
    we omit tag checks when using arguments marked as tagged.

We only use this for workers and specialized versions of SpecConstr
But we also check other functions during tidy and potentially turn some of them into
call by value functions and mark some of their arguments as call-by-value by looking at
argument unfoldings.

NB: I choose to put the information into a new Id constructor since these are loaded
at all optimization levels. This makes it trivial to ensure the additional
calling convention demands are available at all call sites. Putting it into
IdInfo would require us at the very least to always decode the IdInfo
just to decide if we need to throw it away or not after.

Note [Use CBV semantics only for join points and workers]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A function with cbv-semantics requires arguments to be visible
and if no arguments are visible requires us to eta-expand it's
call site. That is for a binding with three cbv arguments like
`w[WorkerLikeId[!,!,!]]` we would need to eta expand undersaturated
occurrences like `map w xs` into `map (\x1 x2 x3 -> w x1 x2 x3) xs.

In experiments it turned out that the code size increase of doing so
can outweigh the performance benefits of doing so.
So we only do this for join points, workers and
specialized functions (from SpecConstr).
Join points are naturally always called saturated so
this problem can't occur for them.
For workers and specialized functions there are also always at least
some applied arguments as we won't inline the wrapper/apply their rule
if there are unapplied occurrences like `map f xs`.
-}

-- | Parent of a record selector function.
--
-- Either the parent 'TyCon' or 'PatSyn' depending
-- on the origin of the record selector.
--
-- For a data family, this is the /instance/ 'TyCon',
-- **not** the family 'TyCon'.
data RecSelParent
  -- | Parent of a data constructor record field.
  --
  -- For a data family, this is the /instance/ 'TyCon'.
  = RecSelData TyCon
  -- | Parent of a pattern synonym record field:
  -- the 'PatSyn' itself.
  | RecSelPatSyn PatSyn
  deriving (Eq, Data)

recSelParentName :: RecSelParent -> Name
recSelParentName (RecSelData   tc) = tyConName tc
recSelParentName (RecSelPatSyn ps) = patSynName ps

recSelFirstConName :: RecSelParent -> Name
recSelFirstConName (RecSelData   tc) = dataConName $ head $ tyConDataCons tc
recSelFirstConName (RecSelPatSyn ps) = patSynName ps

recSelParentCons :: RecSelParent -> [ConLike]
recSelParentCons (RecSelData tc)
  | isAlgTyCon tc
      = map RealDataCon $ visibleDataCons
      $ algTyConRhs tc
  | otherwise
      = []
recSelParentCons (RecSelPatSyn ps) = [PatSynCon ps]

instance Outputable RecSelParent where
  ppr p = case p of
    RecSelData tc
      | Just (parent_tc, tys) <- tyConFamInst_maybe tc
      -> ppr (mkTyConApp parent_tc tys)
      | otherwise
      -> ppr tc
    RecSelPatSyn ps
      -> ppr ps

-- | Just a synonym for 'CoVarId'. Written separately so it can be
-- exported in the hs-boot file.
coVarDetails :: IdDetails
coVarDetails = CoVarId

-- | Check if an 'IdDetails' says 'CoVarId'.
isCoVarDetails :: IdDetails -> Bool
isCoVarDetails CoVarId = True
isCoVarDetails _       = False

isJoinIdDetails_maybe :: IdDetails -> Maybe (JoinArity, (Maybe [CbvMark]))
isJoinIdDetails_maybe (JoinId join_arity marks) = Just (join_arity, marks)
isJoinIdDetails_maybe _                   = Nothing

instance Outputable IdDetails where
    ppr = pprIdDetails

pprIdDetails :: IdDetails -> SDoc
pprIdDetails VanillaId = empty
pprIdDetails other     = brackets (pp other)
 where
   pp VanillaId               = panic "pprIdDetails"
   pp (WorkerLikeId dmds)     = text "StrictWorker" <> parens (ppr dmds)
   pp (DataConWorkId _)       = text "DataCon"
   pp (DataConWrapId _)       = text "DataConWrapper"
   pp (ClassOpId {})          = text "ClassOp"
   pp (RepPolyId {})          = text "RepPolyId"
   pp (PrimOpId {})           = text "PrimOp"
   pp (FCallId _)             = text "ForeignCall"
   pp (TickBoxOpId _)         = text "TickBoxOp"
   pp (DFunId nt)             = text "DFunId" <> ppWhen nt (text "(nt)")
   pp (RecSelId { sel_naughty = is_naughty })
                              = brackets $ text "RecSel" <>
                                           ppWhen is_naughty (text "(naughty)")
   pp CoVarId                 = text "CoVarId"
   pp (JoinId arity marks)    = text "JoinId" <> parens (int arity) <> parens (ppr marks)

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
        ruleInfo        :: RuleInfo,
        -- ^ Specialisations of the 'Id's function which exist.
        -- See Note [Specialisations and RULES in IdInfo]
        realUnfoldingInfo   :: Unfolding,
        -- ^ The 'Id's unfolding
        inlinePragInfo  :: InlinePragma,
        -- ^ Any inline pragma attached to the 'Id'
        occInfo         :: OccInfo,
        -- ^ How the 'Id' occurs in the program
        dmdSigInfo      :: DmdSig,
        -- ^ A strictness signature. Describes how a function uses its arguments
        --   See Note [idArity varies independently of dmdTypeDepth]
        --       in GHC.Core.Opt.DmdAnal
        cprSigInfo      :: CprSig,
        -- ^ Information on whether the function will ultimately return a
        -- freshly allocated constructor.
        demandInfo      :: Demand,
        -- ^ ID demand information
        bitfield        :: {-# UNPACK #-} !BitField,
        -- ^ Bitfield packs CafInfo, OneShotInfo, arity info, and
        -- call arity info in one 64-bit word. Packing these fields reduces size
        -- of `IdInfo` from 12 words to 7 words and reduces residency by almost
        -- 4% in some programs. See #17497 and associated MR.
        --
        -- See documentation of the getters for what these packed fields mean.
        lfInfo          :: !(Maybe LambdaFormInfo),
        -- ^ If lfInfo = Just info, then the `info` is guaranteed /correct/.
        --   If lfInfo = Nothing, then we do not have a `LambdaFormInfo` for this Id,
        --                so (for imported Ids) we make a conservative version.
        --                See Note [The LFInfo of Imported Ids] in GHC.StgToCmm.Closure
        -- For locally-defined Ids other than DataCons, the `lfInfo` field is always Nothing.
        -- See also Note [LFInfo of DataCon workers and wrappers]

        -- See documentation of the getters for what these packed fields mean.
        tagSig          :: !(Maybe TagSig)
    }

-- | Encodes arities, OneShotInfo, CafInfo.
-- From least-significant to most-significant bits:
--
-- - Bit   0   (1):  OneShotInfo
-- - Bit   1   (1):  CafInfo
-- - Bit   2   (1):  unused
-- - Bits  3-32(30): Call Arity info
-- - Bits 33-62(30): Arity info
--
newtype BitField = BitField Word64

emptyBitField :: BitField
emptyBitField = BitField 0

bitfieldGetOneShotInfo :: BitField -> OneShotInfo
bitfieldGetOneShotInfo (BitField bits) =
    if testBit bits 0 then OneShotLam else NoOneShotInfo

bitfieldGetCafInfo :: BitField -> CafInfo
bitfieldGetCafInfo (BitField bits) =
    if testBit bits 1 then NoCafRefs else MayHaveCafRefs

bitfieldGetCallArityInfo :: BitField -> ArityInfo
bitfieldGetCallArityInfo (BitField bits) =
    fromIntegral (bits `shiftR` 3) .&. ((1 `shiftL` 30) - 1)

bitfieldGetArityInfo :: BitField -> ArityInfo
bitfieldGetArityInfo (BitField bits) =
    fromIntegral (bits `shiftR` 33)

bitfieldSetOneShotInfo :: OneShotInfo -> BitField -> BitField
bitfieldSetOneShotInfo info (BitField bits) =
    case info of
      NoOneShotInfo -> BitField (clearBit bits 0)
      OneShotLam -> BitField (setBit bits 0)

bitfieldSetCafInfo :: CafInfo -> BitField -> BitField
bitfieldSetCafInfo info (BitField bits) =
    case info of
      MayHaveCafRefs -> BitField (clearBit bits 1)
      NoCafRefs -> BitField (setBit bits 1)

bitfieldSetCallArityInfo :: ArityInfo -> BitField -> BitField
bitfieldSetCallArityInfo info bf@(BitField bits) =
    assert (info < 2^(30 :: Int) - 1) $
    bitfieldSetArityInfo (bitfieldGetArityInfo bf) $
    BitField ((fromIntegral info `shiftL` 3) .|. (bits .&. 0b111))

bitfieldSetArityInfo :: ArityInfo -> BitField -> BitField
bitfieldSetArityInfo info (BitField bits) =
    assert (info < 2^(30 :: Int) - 1) $
    BitField ((fromIntegral info `shiftL` 33) .|. (bits .&. ((1 `shiftL` 33) - 1)))

-- Getters

-- | Info about a lambda-bound variable, if the 'Id' is one
oneShotInfo :: IdInfo -> OneShotInfo
oneShotInfo = bitfieldGetOneShotInfo . bitfield

-- | 'Id' arity, as computed by "GHC.Core.Opt.Arity". Specifies how many arguments
-- this 'Id' has to be applied to before it does any meaningful work.
arityInfo :: IdInfo -> ArityInfo
arityInfo = bitfieldGetArityInfo . bitfield

-- | 'Id' CAF info
cafInfo :: IdInfo -> CafInfo
cafInfo = bitfieldGetCafInfo . bitfield

-- | How this is called. This is the number of arguments to which a binding can
-- be eta-expanded without losing any sharing. n <=> all calls have at least n
-- arguments
callArityInfo :: IdInfo -> ArityInfo
callArityInfo = bitfieldGetCallArityInfo . bitfield

tagSigInfo :: IdInfo -> Maybe TagSig
tagSigInfo = tagSig

-- Setters

setRuleInfo :: IdInfo -> RuleInfo -> IdInfo
setRuleInfo       info sp = sp `seq` info { ruleInfo = sp }
setInlinePragInfo :: IdInfo -> InlinePragma -> IdInfo
setInlinePragInfo info pr = pr `seq` info { inlinePragInfo = pr }
setOccInfo :: IdInfo -> OccInfo -> IdInfo
setOccInfo        info oc = oc `seq` info { occInfo = oc }
        -- Try to avoid space leaks by seq'ing

-- | Essentially returns the 'realUnfoldingInfo' field, but does not expose the
-- unfolding of a strong loop breaker.
--
-- This is the right thing to call if you plan to decide whether an unfolding
-- will inline.
unfoldingInfo :: IdInfo -> Unfolding
unfoldingInfo info
  | isStrongLoopBreaker (occInfo info) = trimUnfolding $ realUnfoldingInfo info
  | otherwise                          =                 realUnfoldingInfo info

setUnfoldingInfo :: IdInfo -> Unfolding -> IdInfo
setUnfoldingInfo info uf
  = -- We don't seq the unfolding, as we generate intermediate
    -- unfoldings which are just thrown away, so evaluating them is a
    -- waste of time.
    -- seqUnfolding uf `seq`
    info { realUnfoldingInfo = uf }

hasInlineUnfolding :: IdInfo -> Bool
-- ^ True of a /non-loop-breaker/ Id that has a /stable/ unfolding that is
--   (a) always inlined; that is, with an `UnfWhen` guidance, or
--   (b) a DFunUnfolding which never needs to be inlined
hasInlineUnfolding info = isInlineUnfolding (unfoldingInfo info)

setArityInfo :: IdInfo -> ArityInfo -> IdInfo
setArityInfo info ar =
    info { bitfield = bitfieldSetArityInfo ar (bitfield info) }

setCallArityInfo :: IdInfo -> ArityInfo -> IdInfo
setCallArityInfo info ar =
    info { bitfield = bitfieldSetCallArityInfo ar (bitfield info) }

setCafInfo :: IdInfo -> CafInfo -> IdInfo
setCafInfo info caf =
    info { bitfield = bitfieldSetCafInfo caf (bitfield info) }

setLFInfo :: IdInfo -> LambdaFormInfo -> IdInfo
setLFInfo info lf = info { lfInfo = Just lf }

setTagSig :: IdInfo -> TagSig -> IdInfo
setTagSig info sig = info { tagSig = Just sig }

setOneShotInfo :: IdInfo -> OneShotInfo -> IdInfo
setOneShotInfo info lb =
    info { bitfield = bitfieldSetOneShotInfo lb (bitfield info) }

setDemandInfo :: IdInfo -> Demand -> IdInfo
setDemandInfo info dd = dd `seq` info { demandInfo = dd }

setDmdSigInfo :: IdInfo -> DmdSig -> IdInfo
setDmdSigInfo info dd = dd `seq` info { dmdSigInfo = dd }

setCprSigInfo :: IdInfo -> CprSig -> IdInfo
setCprSigInfo info cpr = cpr `seq` info { cprSigInfo = cpr }

-- | Basic 'IdInfo' that carries no useful information whatsoever
vanillaIdInfo :: IdInfo
vanillaIdInfo
  = IdInfo {
            ruleInfo       = emptyRuleInfo,
            realUnfoldingInfo  = noUnfolding,
            inlinePragInfo = defaultInlinePragma,
            occInfo        = noOccInfo,
            demandInfo     = topDmd,
            dmdSigInfo     = nopSig,
            cprSigInfo     = topCprSig,
            bitfield       = bitfieldSetCafInfo vanillaCafInfo $
                             bitfieldSetArityInfo unknownArity $
                             bitfieldSetCallArityInfo unknownArity $
                             bitfieldSetOneShotInfo NoOneShotInfo $
                             emptyBitField,
            lfInfo         = Nothing,
            tagSig         = Nothing
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

Note [Arity and function types]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The arity of an 'Id' must never exceed the number of arguments that
can be read off from the 'Id's type, possibly after expanding newtypes.

Examples:

  f1 :: forall a. a -> a

    idArity f1 <= 1: only one value argument, of type 'a'

  f2 :: forall a. Show a => Int -> a

    idArity f2 <= 2: two value arguments, of types 'Show a' and 'Int'.


  newtype Id a = MkId a
  f3 :: forall b. Id (Int -> b)

    idArity f3 <= 1: there is one value argument, of type 'Int', hidden under the newtype.

  newtype RecFun = MkRecFun (Int -> RecFun)
  f4 :: RecFun

    no constraint on the arity of f4: we can unwrap as many layers of the newtype as we want,
    to get arbitrarily many arguments of type 'Int'.
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
--
-- /Invariant:/ the 'Arity' of an 'Id' must never exceed the number of
-- value arguments that appear in the type of the 'Id'.
-- See Note [Arity and function types].
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

pprStrictness :: DmdSig -> SDoc
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
                        -- Note [Rule dependency info] in "GHC.Core.Opt.OccurAnal"

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

-- | Change the name of the function the rule is keyed on all of the 'CoreRule's
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

    is_safe_dmd dmd = not (isStrUsedDmd dmd)

-- | Lazify (remove the top-level demand, only) the demand in `IdInfo`
-- Keep nested demands; see Note [Floatifying demand info when floating]
-- in GHC.Core.Opt.SetLevels
lazifyDemandInfo :: IdInfo -> Maybe IdInfo
lazifyDemandInfo info@(IdInfo { demandInfo = dmd })
  = Just (info {demandInfo = lazifyDmd dmd })

-- | Floatify the demand in `IdInfo`
-- But keep /nested/ demands; see Note [Floatifying demand info when floating]
-- in GHC.Core.Opt.SetLevels
floatifyDemandInfo :: IdInfo -> Maybe IdInfo
floatifyDemandInfo info@(IdInfo { demandInfo = dmd })
  = Just (info {demandInfo = floatifyDmd dmd })

-- | Remove usage (but not strictness) info on the `IdInfo`
zapUsageInfo :: IdInfo -> Maybe IdInfo
zapUsageInfo info = Just (info {demandInfo = zapUsageDemand (demandInfo info)})

-- | Remove usage environment info from the strictness signature on the 'IdInfo'
zapUsageEnvInfo :: IdInfo -> Maybe IdInfo
zapUsageEnvInfo info
    | hasDemandEnvSig (dmdSigInfo info)
    = Just (info {dmdSigInfo = zapDmdEnvSig (dmdSigInfo info)})
    | otherwise
    = Nothing

zapUsedOnceInfo :: IdInfo -> Maybe IdInfo
zapUsedOnceInfo info
    = Just $ info { dmdSigInfo = zapUsedOnceSig    (dmdSigInfo info)
                  , demandInfo     = zapUsedOnceDemand (demandInfo     info) }

zapFragileInfo :: IdInfo -> Maybe IdInfo
-- ^ Zap info that depends on free variables
zapFragileInfo info@(IdInfo { occInfo = occ, realUnfoldingInfo = unf })
  = new_unf `seq`  -- The unfolding field is not (currently) strict, so we
                   -- force it here to avoid a (zapFragileUnfolding unf) thunk
                   -- which might leak space
    Just (info `setRuleInfo` emptyRuleInfo
               `setUnfoldingInfo` new_unf
               `setOccInfo`       zapFragileOcc occ)
  where
    new_unf = zapFragileUnfolding unf

zapFragileUnfolding :: Unfolding -> Unfolding
-- ^ Zaps any core unfolding, but /preserves/ evaluated-ness,
-- i.e. an unfolding of OtherCon
zapFragileUnfolding unf
 -- N.B. isEvaldUnfolding catches *both* OtherCon [] *and* core unfoldings
 -- representing values.
 | isEvaldUnfolding unf = evaldUnfolding
 | otherwise            = noUnfolding

trimUnfolding :: Unfolding -> Unfolding
-- Squash all unfolding info, preserving only evaluated-ness
trimUnfolding unf | isEvaldUnfolding unf = evaldUnfolding
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
