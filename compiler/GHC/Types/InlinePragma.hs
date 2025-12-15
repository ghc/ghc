
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}

{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1997-1998
-}

{-# OPTIONS_GHC -Wno-orphans #-}
{-
Suppression of warnings are required for instances:
  - Binary Activation
  - Binary CompilerPhase
  - Binary InlinePragma
  - Binary InlineSaturation
  - Binary XActivation
  - Binary XInlinePragmaGhc
  - Outputable CompilerPhase
-}

module GHC.Types.InlinePragma
  ( -- * Inline Pragma Encoding
    -- ** InlinePragma
    -- *** Data-type
    InlinePragma(..)
  , InlinePragmaInfo
    -- *** Constants
  , defaultInlinePragma
  , alwaysConLikePragma
  , alwaysInlinePragma
  , alwaysInlineConLikePragma
  , dfunInlinePragma
  , neverInlinePragma
    -- *** Field accessors
  , inlinePragmaActivation
  , inlinePragmaSaturation
  , inlinePragmaName
  , inlinePragmaRuleMatchInfo
  , inlinePragmaSource
  , inlinePragmaSpec
    -- *** Queries
  , isAnyInlinePragma
  , isDefaultInlinePragma
  , isInlinablePragma
  , isInlinePragma
  , isNoInlinePragma
  , isOpaquePragma
    -- *** Mutators
  , setInlinePragmaSource
  , setInlinePragmaSaturation
  , setInlinePragmaActivation
  , setInlinePragmaSpec
  , setInlinePragmaRuleMatchInfo
    -- *** GHC pass conversions
  , tcInlinePragma
    -- *** Pretty-printing
  , pprInline
  , pprInlineDebug

    -- ** Extensible record type for GhcRn & GhcTc
  , XInlinePragmaGhc(..)
  , InlineSaturation(..)

    -- ** InlineSpec
    -- *** Data-type
  , InlineSpec(..)
    -- *** Queries
  , noUserInlineSpec

    -- ** RuleMatchInfo
    -- *** Data-type
  , RuleMatchInfo(..)
    -- *** Queries
  , isConLike
  , isFunLike

    -- * Phase Activation
    -- ** Activation
    -- *** Data-type
  , ActivationGhc
  , ActivationX(AlwaysActive, NeverActive, ActiveAfter, ActiveBefore)
  , pattern ActiveFinal
  , PhaseNum
    -- *** Construction
  , activeAfter
    -- *** Constants
  , activateAfterInitial
  , activateDuringFinal
    -- *** Queries
  , activeInFinalPhase
  , activeInInitialPhase
  , activeInPhase
  , competesWith
  , isAlwaysActive
  , isNeverActive

    -- ** CompilerPhase
    -- *** Data-type
  , CompilerPhase(..)
    -- *** Constructors
  , beginPhase
  , endPhase
    -- *** Queries
  , isActiveInPhase
  , laterPhase
  , laterThanPhase
  , nextPhase
  ) where

import GHC.Prelude

import GHC.Data.FastString
import GHC.Hs.Extension
import GHC.Types.Arity (Arity)
import GHC.Types.SourceText (SourceText(..))
import GHC.Utils.Binary
import GHC.Utils.Outputable

import Control.DeepSeq (NFData(..))
import Data.Data (Data)

import Language.Haskell.Syntax.Binds.InlinePragma
import Language.Haskell.Syntax.Extension

-- infixl so you can say (prag `set` a `set` b)
infixl 1 `setInlinePragmaActivation`,
         `setInlinePragmaSaturation`,
         `setInlinePragmaRuleMatchInfo`,
         `setInlinePragmaSource`,
         `setInlinePragmaSpec`

-- | The arity /at which to/ inline a function.
-- This may differ from the function's syntactic arity.
data InlineSaturation
    = AppliedToAtLeast !Arity
      -- ^ Inline only when applied to @n@ explicit
      -- (non-type, non-dictionary) arguments.
      --
      -- That is, 'AppliedToAtLeast' describes the number of
      --  *source-code* arguments the thing must be applied to.
    | AnySaturation
      -- ^ There does not exist an explicit number of arguments
      -- that the inlining process should be applied to.
    deriving (Eq, Data)

instance NFData InlineSaturation where

  rnf (AppliedToAtLeast !w) = rnf w `seq` ()
  rnf !AnySaturation = ()


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
type InlinePragmaInfo = InlinePragma GhcTc

type ActivationGhc = ActivationX XXActivationGhc

data XInlinePragmaGhc = XInlinePragmaGhc
  { xinl_src :: SourceText
      -- ^ See Note [Pragma source text]
  , xinl_sat :: InlineSaturation
      -- ^ Inline only when applied to @n@ explicit
      -- (non-type, non-dictionary) arguments.
      --
      -- That is, 'xinl_sat' describes the number of *source-code*
      -- arguments the thing must be applied to.  We add on the
      -- number of implicit, dictionary arguments when making
      -- the Unfolding, and don't look at inl_sat further
  }
  deriving (Eq, Data)

instance NFData XInlinePragmaGhc where
  rnf (XInlinePragmaGhc s a) = rnf s `seq` rnf a `seq` ()

data XXActivationGhc = XActiveFinal
  deriving (Eq, Data)

instance NFData XXActivationGhc where
  rnf !x = x `seq` ()


{-# COMPLETE AlwaysActive, ActiveBefore, ActiveAfter, NeverActive, ActiveFinal #-}
pattern ActiveFinal :: Activation (GhcPass p)
pattern ActiveFinal = XActivation XActiveFinal

type instance XInlinePragma GhcPs = SourceText
type instance XInlinePragma GhcRn = XInlinePragmaGhc
type instance XInlinePragma GhcTc = XInlinePragmaGhc
type instance XXInlinePragma (GhcPass _) = DataConCantHappen
type instance XXActivation   (GhcPass _) = XXActivationGhc

-- | The default 'InlinePragma' definition for GHC.
-- The type and value of 'inl_ext' provided will differ
-- between the passes of GHC. Consequently, it may be
-- necessary to apply type annotation at the call site
-- to help the type checker disambiguate the correct
-- type of 'inl_ext'.
defaultInlinePragma :: forall p. IsPass p => InlinePragma (GhcPass p)
defaultInlinePragma =
  let srcTxt = SourceText $ fsLit "{-# INLINE"
      inlExt =  case ghcPass @p of
        GhcPs -> srcTxt
        GhcRn -> XInlinePragmaGhc srcTxt AnySaturation
        GhcTc -> XInlinePragmaGhc srcTxt AnySaturation
  in  InlinePragma
        { inl_ext = inlExt
        , inl_act = AlwaysActive
        , inl_rule = FunLike
        , inl_inline = NoUserInlinePrag }

-- | The default 'InlinePragma' definition for the "parser pass" of GHC.
alwaysInlinePragma, neverInlinePragma, alwaysConLikePragma, alwaysInlineConLikePragma, dfunInlinePragma
  :: forall p. IsPass p => InlinePragma (GhcPass p)


alwaysInlinePragma        = (defaultInlinePragma @p) { inl_inline = Inline }
neverInlinePragma         = (defaultInlinePragma @p) { inl_act    = NeverActive }
alwaysConLikePragma       = (defaultInlinePragma @p) { inl_rule   = ConLike }
alwaysInlineConLikePragma = (alwaysInlinePragma  @p) { inl_rule   = ConLike }

-- A DFun has an always-active inline activation so that
-- exprIsConApp_maybe can "see" its unfolding
-- (However, its actual Unfolding is a DFunUnfolding, which is
--  never inlined other than via exprIsConApp_maybe.)
dfunInlinePragma = (defaultInlinePragma @p) { inl_act  = AlwaysActive
                                            , inl_rule = ConLike }

isDefaultInlinePragma :: InlinePragma (GhcPass p) -> Bool
isDefaultInlinePragma (InlinePragma { inl_act = activation
                                    , inl_rule = match_info
                                    , inl_inline = inline })
  = noUserInlineSpec inline && isAlwaysActive activation && isFunLike match_info

inlinePragmaSource :: forall p. IsPass p => InlinePragma (GhcPass p) -> SourceText
inlinePragmaSource (InlinePragma { inl_ext = src }) = srcTxt
  where
    srcTxt = case ghcPass @p of
      GhcPs -> src
      GhcRn -> xinl_src src
      GhcTc -> xinl_src src

inlinePragmaSaturation :: forall p. (XInlinePragma (GhcPass p) ~ XInlinePragmaGhc)
  => InlinePragma (GhcPass p) -> InlineSaturation
inlinePragmaSaturation = xinl_sat . inl_ext

inlinePragmaSpec :: InlinePragma (GhcPass p) -> InlineSpec
inlinePragmaSpec = inl_inline

inlinePragmaActivation :: InlinePragma (GhcPass p) -> ActivationGhc
inlinePragmaActivation (InlinePragma { inl_act = activation }) = activation

inlinePragmaRuleMatchInfo :: InlinePragma (GhcPass p) -> RuleMatchInfo
inlinePragmaRuleMatchInfo (InlinePragma { inl_rule = info }) = info

setInlinePragmaSource :: forall p. IsPass p
  => InlinePragma (GhcPass p) -> SourceText -> InlinePragma (GhcPass p)
setInlinePragmaSource prag srcTxt = prag { inl_ext = newExt }
  where
    oldExt = inl_ext prag
    newExt = case ghcPass @p of
      GhcPs -> srcTxt
      GhcRn -> oldExt { xinl_src = srcTxt }
      GhcTc -> oldExt { xinl_src = srcTxt }

setInlinePragmaActivation ::
  InlinePragma (GhcPass p) -> ActivationGhc -> InlinePragma (GhcPass p)
setInlinePragmaActivation prag activation = prag { inl_act = activation }

setInlinePragmaRuleMatchInfo ::
  InlinePragma (GhcPass p) -> RuleMatchInfo -> InlinePragma (GhcPass p)
setInlinePragmaRuleMatchInfo prag info = prag { inl_rule = info }

setInlinePragmaSaturation :: forall p q. (IsPass p, XInlinePragma (GhcPass q) ~ XInlinePragmaGhc)
  => InlinePragma (GhcPass p) -> InlineSaturation -> InlinePragma (GhcPass q)
setInlinePragmaSaturation prag sat =
    prag { inl_ext = XInlinePragmaGhc (inlinePragmaSource prag) sat
         , inl_act = inl_act prag
         }

setInlinePragmaSpec ::
  InlinePragma (GhcPass p) -> InlineSpec -> InlinePragma (GhcPass p)
setInlinePragmaSpec prag spec = prag { inl_inline = spec }

tcInlinePragma :: InlinePragma GhcRn -> InlinePragma GhcTc
tcInlinePragma prag@(InlinePragma { inl_ext = src }) =
  prag { inl_ext = src
       , inl_act = inl_act prag
       }

{-
************************************************************************
*                                                                      *
\subsection{Activation}
*                                                                      *
************************************************************************

When a rule or inlining is active

Note [Compiler phases]
~~~~~~~~~~~~~~~~~~~~~~
The CompilerPhase says which phase the simplifier is running in:

* InitialPhase: before all user-visible phases

* Phase 2,1,0: user-visible phases; the phase number
  controls rule ordering an inlining.

* FinalPhase: used for all subsequent simplifier
  runs. By delaying inlining of wrappers to FinalPhase we can
  ensure that RULE have a good chance to fire. See
  Note [Wrapper activation] in GHC.Core.Opt.WorkWrap

  NB: FinalPhase is run repeatedly, not just once.

  NB: users don't have access to InitialPhase or FinalPhase.
  They write {-# INLINE[n] f #-}, meaning (Phase n)

The phase sequencing is done by GHC.Opt.Simplify.Driver
-}

-- | Compilation phase number, including the user-specifiable 'PhaseNum'
-- and the GHC internal "initial" and "final" phases.
data CompilerPhase
  = InitialPhase    -- ^ The first phase; number = infinity!
  | Phase PhaseNum  -- ^ User-specifiable phases
  | FinalPhase      -- ^ The last phase; number = -infinity!
  deriving (Eq, Data)

instance NFData CompilerPhase where
  rnf = \case
    InitialPhase -> ()
    FinalPhase -> ()
    Phase i -> rnf i

-- ^ @activeAfter p@ makes an 'Activation' that is active in phase @p@ and after
--
-- Invariant: @beginPhase (activeAfter p) = p@
activeAfter :: CompilerPhase -> ActivationGhc
activeAfter InitialPhase = AlwaysActive
activeAfter (Phase n)    = ActiveAfter n
activeAfter FinalPhase   = ActiveFinal

activeInPhase :: PhaseNum -> ActivationGhc -> Bool
activeInPhase _ AlwaysActive     = True
activeInPhase _ NeverActive      = False
activeInPhase _ ActiveFinal      = False
activeInPhase p (ActiveAfter  n) = p <= n
activeInPhase p (ActiveBefore n) = p >  n

activeInFinalPhase :: ActivationGhc -> Bool
activeInFinalPhase AlwaysActive     = True
activeInFinalPhase ActiveFinal      = True
activeInFinalPhase (ActiveAfter {}) = True
activeInFinalPhase _                = False

isNeverActive, isAlwaysActive :: ActivationGhc -> Bool
isNeverActive NeverActive = True
isNeverActive _           = False

isAlwaysActive AlwaysActive = True
isAlwaysActive _            = False

activateAfterInitial :: ActivationGhc
-- ^ Active in the first phase after the initial phase
activateAfterInitial = activeAfter (nextPhase InitialPhase)

activateDuringFinal :: ActivationGhc
-- ^ Active in the final simplification phase (which is repeated)
activateDuringFinal = ActiveFinal

activeInInitialPhase :: ActivationGhc -> Bool
activeInInitialPhase act = beginPhase act == InitialPhase

beginPhase :: ActivationGhc -> CompilerPhase
-- ^ First phase in which the 'Activation' is active,
-- or 'FinalPhase' if it is never active
beginPhase AlwaysActive      = InitialPhase
beginPhase (ActiveBefore {}) = InitialPhase
beginPhase (ActiveAfter n)   = Phase n
beginPhase ActiveFinal       = FinalPhase
beginPhase NeverActive       = FinalPhase

endPhase :: ActivationGhc -> CompilerPhase
-- ^ Last phase in which the 'Activation' is active,
-- or 'InitialPhase' if it is never active
endPhase AlwaysActive       = FinalPhase
endPhase (ActiveBefore n)   =
  if nextPhase InitialPhase == Phase n
  then InitialPhase
  else Phase $ n + 1
endPhase (ActiveAfter {})   = FinalPhase
endPhase ActiveFinal        = FinalPhase
endPhase NeverActive        = InitialPhase

nextPhase :: CompilerPhase -> CompilerPhase
-- ^ Tells you the next phase after this one
--
-- Currently we have just phases @[2,1,0,FinalPhase,FinalPhase,...]@,
-- where FinalPhase means GHC's internal simplification steps
-- after all rules have run
nextPhase InitialPhase = Phase 2
nextPhase (Phase 0)    = FinalPhase
nextPhase (Phase n)    = Phase (n-1)
nextPhase FinalPhase   = FinalPhase

laterPhase :: CompilerPhase -> CompilerPhase -> CompilerPhase
-- ^ Returns the later of two phases
laterPhase (Phase n1)   (Phase n2)   = Phase (n1 `min` n2)
laterPhase InitialPhase p2           = p2
laterPhase FinalPhase   _            = FinalPhase
laterPhase p1           InitialPhase = p1
laterPhase _            FinalPhase   = FinalPhase

-- | @p1 `laterThanOrEqualPhase` p2@ computes whether @p1@ happens (strictly)
-- after @p2@.
laterThanPhase :: CompilerPhase -> CompilerPhase -> Bool
p1 `laterThanPhase` p2 = toNum p1 < toNum p2
  where
    toNum :: CompilerPhase -> Int
    toNum InitialPhase = maxBound
    toNum (Phase i)    = i
    toNum FinalPhase   = minBound

isActiveInPhase :: CompilerPhase -> ActivationGhc -> Bool
isActiveInPhase InitialPhase act = activeInInitialPhase act
isActiveInPhase (Phase p)    act = activeInPhase p act
isActiveInPhase FinalPhase   act = activeInFinalPhase act

-- | @act1 `competesWith` act2@ returns whether @act1@ is active in the phase
-- when @act2@ __becomes__ active.
--
-- This answers the question: might @act1@ fire first?
--
-- NB: this is not the same as computing whether @act1@ and @act2@ are
-- ever active at the same time.
--
-- See Note [Competing activations]
competesWith :: ActivationGhc -> ActivationGhc-> Bool
competesWith NeverActive  _           = False
competesWith _            NeverActive = False -- See Wrinkle [Never active rules]
competesWith act1         act2        = isActiveInPhase (beginPhase act2) act1

{- Note [Competing activations]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Sometimes a RULE and an inlining may compete, or two RULES.
See Note [Rules and inlining/other rules] in GHC.HsToCore.

We say that act1 "competes with" act2 iff
   act1 is active in the phase when act2 *becomes* active
NB: remember that phases count *down*: 2, 1, 0!

It's too conservative to ensure that the two are never simultaneously
active.  For example, a rule might be always active, and an inlining
might switch on in phase 2.  We could switch off the rule, but it does
no harm.

  Wrinkle [Never active rules]

    Rules can be declared as "never active" by users, using the syntax:

      {-# RULE "blah" [~] ... #-}

        (This feature exists solely for compiler plugins, by making it possible
        to define a RULE that is never run by GHC, but is nevertheless parsed,
        typechecked etc, so that it is available to the plugin.)

    We should not warn about competing rules, so make sure that 'competesWith'
    always returns 'False' when its second argument is 'NeverActive'.
-}

{- TODO: These orphan instance should be moved to the GHC.Utils.{Binary,Outputable}
modules once TTG has progressed and the Language.Haskell.Syntax.Types module
no longer depends on importing GHC.Hs.Doc.
-}
instance Binary XInlinePragmaGhc where
    put_ bh (XInlinePragmaGhc s a) = do
            put_ bh s
            put_ bh a

    get bh = do
           s <- get bh
           a <- get bh
           return (XInlinePragmaGhc s a)

instance forall p. IsPass p => Binary (InlinePragma (GhcPass p)) where
    put_ bh (InlinePragma s a b c) = do
            put_ bh a
            put_ bh b
            put_ bh c
            case ghcPass @p of
              GhcPs -> put_ bh s
              GhcRn -> put_ bh s
              GhcTc -> put_ bh s

    get bh = do
           a <- get bh
           b <- get bh
           c <- get bh
           s <- case ghcPass @p of
                  GhcPs -> get bh
                  GhcRn -> get bh
                  GhcTc -> get bh
           return (InlinePragma s a b c)

instance Binary InlineSaturation where
    put_ bh AnySaturation = putByte bh 0
    put_ bh (AppliedToAtLeast w) = putByte bh 1 *> put_ bh w

    get bh = do
      h <- getByte bh
      if h == 0 then pure AnySaturation
                else AppliedToAtLeast <$> get bh

instance Binary ActivationGhc where
    put_ bh = \case
      NeverActive     -> putByte bh 0
      AlwaysActive    -> putByte bh 1
      ActiveBefore aa -> putByte bh 2 *> put_ bh aa
      ActiveAfter  ab -> putByte bh 3 *> put_ bh ab
      XActivation  _  -> putByte bh 4

    get bh = do
      h <- getByte bh
      case h of
        0 -> pure NeverActive
        1 -> pure AlwaysActive
        2 -> ActiveBefore <$> get bh
        3 -> ActiveAfter  <$> get bh
        _ -> pure ActiveFinal

instance Binary CompilerPhase where
  put_ bh InitialPhase = putByte bh 0
  put_ bh (Phase i)    = do { putByte bh 1; put_ bh i }
  put_ bh FinalPhase   = putByte bh 2

  get bh = do
    h <- getByte bh
    case h of
      0 -> return InitialPhase
      1 -> do { p <- get bh; return (Phase p) }
      _ -> return FinalPhase

instance Outputable CompilerPhase where
   ppr (Phase n)    = int n
   ppr InitialPhase = text "initial"
   ppr FinalPhase   = text "final"

-- | Outputs string for pragma name for any of INLINE/INLINABLE/NOINLINE. This
-- differs from the Outputable instance for the InlineSpec type where the pragma
-- name string as well as the accompanying SourceText (if any) is printed.
inlinePragmaName :: InlineSpec -> SDoc
inlinePragmaName Inline           = text "INLINE"
inlinePragmaName Inlinable        = text "INLINABLE"
inlinePragmaName NoInline         = text "NOINLINE"
inlinePragmaName Opaque           = text "OPAQUE"
inlinePragmaName NoUserInlinePrag = empty

-- | Pretty-print without displaying the user-specified 'InlineSpec'.
pprInline :: forall p. IsPass p => InlinePragma (GhcPass p) -> SDoc
pprInline = pprInline' True

-- | Pretty-print including the user-specified 'InlineSpec'.
pprInlineDebug :: forall p. IsPass p => InlinePragma (GhcPass p) -> SDoc
pprInlineDebug = pprInline' False

pprInline' :: forall p. IsPass p
           => Bool           -- True <=> do not display the inl_inline field
           -> InlinePragma (GhcPass p)
           -> SDoc
pprInline' emptyInline (InlinePragma
                        { inl_ext = ext,
                          inl_inline = inline,
                          inl_act = activation,
                          inl_rule = info })
    = pp_inl inline <> pp_act inline activation <+> pp_sat <+> pp_info
    where
      pp_inl x = if emptyInline then empty else inlinePragmaName x

      pp_act Inline   {}  AlwaysActive = empty
      pp_act NoInline {}  NeverActive  = empty
      pp_act Opaque   {}  NeverActive  = empty
      pp_act _            act          = ppr act

      pp_sat = case ghcPass @p of
        GhcPs -> empty -- No saturation information
        GhcRn -> getSat (xinl_sat ext)
        GhcTc -> getSat (xinl_sat ext)

      pp_info | isFunLike info = empty
              | otherwise      = ppr info

      getSat :: InlineSaturation -> SDoc
      getSat = \case
        AnySaturation -> empty
        AppliedToAtLeast ar -> parens (text "sat-args=" <> int ar)

instance forall p. IsPass p => Outputable (InlinePragma (GhcPass p)) where
  ppr = pprInline
