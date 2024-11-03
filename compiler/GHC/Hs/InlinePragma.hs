{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module GHC.Hs.InlinePragma(
        module Language.Haskell.Syntax.InlinePragma,
        InlinePragmaExt(..),
        inl_src, inl_sat,
        CompilerPhase(..), beginPhase, nextPhase, laterPhase,

        Activation(..), isActive, competesWith,
        isNeverActive, isAlwaysActive, activeInFinalPhase,
        activateAfterInitial, activateDuringFinal, activeAfter,

        RuleMatchInfo(..),
        InlineSpec(..), noUserInlineSpec,
        InlinePragma(..), defaultInlinePragma, alwaysInlinePragma,
        neverInlinePragma, dfunInlinePragma,
        isDefaultInlinePragma,
        isInlinePragma, isInlinablePragma, isNoInlinePragma, isOpaquePragma,
        isAnyInlinePragma, alwaysInlineConLikePragma,
        inlinePragmaSource,
        inlinePragmaName, inlineSpecSource,
        setInlinePragmaActivation, setInlinePragmaRuleMatchInfo, setInlinePragmaArity,
        pprInline, pprInlineDebug,
        convertInlinePragma, convertInlineSpec, convertActivation,
) where

import GHC.Prelude
import GHC.Types.SourceText(SourceText (..), pprWithSourceText)
import GHC.Hs.Extension(GhcPass)
import GHC.Types.Basic(Arity)

import GHC.Utils.Outputable (Outputable, SDoc
  ,ppr,  (<>), (<+>), empty, parens, brackets, text, int, char)

import Language.Haskell.Syntax.InlinePragma
import Language.Haskell.Syntax.Extension
import GHC.Data.FastString (fsLit)
import GHC.Utils.Binary (Binary, put_, get, putByte, getByte)

{-
************************************************************************
*                                                                      *
\subsection{Type Family instances and deriving instances}
*                                                                      *
************************************************************************
-}

--InlinePragma
type instance XInlinePragma   (GhcPass p) = InlinePragmaExt
type instance XXCInlinePragma (GhcPass p) = DataConCantHappen

deriving instance Eq (InlinePragma (GhcPass p))

--InlineSpec
type instance XInline    (GhcPass p) = SourceText
type instance XInlinable (GhcPass p) = SourceText
type instance XNoInline  (GhcPass p) = SourceText
type instance XOpaque    (GhcPass p) = SourceText
type instance XNoUserInlinePrag (GhcPass p) = NoExtField
type instance XXInlineSpec      (GhcPass p) = DataConCantHappen

deriving instance Eq (InlineSpec (GhcPass p))

instance Show (InlineSpec (GhcPass p)) where
  show (Inline s)    = "Inline "    ++ show s
  show (Inlinable s) = "Inlinable " ++ show s
  show (NoInline s)  = "NoInline "  ++ show s
  show (Opaque   s)  = "Opaque "    ++ show s
  show (NoUserInlinePrag _) = "NoUserInlinePrag"
  show (XInlineSpec m)      = dataConCantHappen m
  -- Show needed for GHC.Parser.Lexer

type instance XAlwaysActive (GhcPass _) = NoExtField
type instance XActiveBefore (GhcPass _) = SourceText
type instance XActiveAfter  (GhcPass _) = SourceText
type instance XFinalActive  (GhcPass _) = NoExtField
type instance XNeverActive  (GhcPass _) = NoExtField
type instance XXActivation  (GhcPass _) = DataConCantHappen

deriving instance Eq (Activation (GhcPass p))
    -- Eq used in comparing rules in GHC.Hs.Decls


{-
************************************************************************
*                                                                      *
\subsection{InlinePragma}
*                                                                      *
************************************************************************

Note [InlinePragma]
~~~~~~~~~~~~~~~~~~~~~~
This data type mirrors what you can write in an INLINE or NOINLINE pragma in
the source program.

If you write nothing at all, you get defaultInlinePragma:
   inl_inline = NoUserInlinePrag
   inl_act    = AlwaysActive
   inl_rule   = FunLike

It's not possible to get that combination by *writing* something, so
if an Id has defaultInlinePragma it means the user didn't specify anything.

If inl_inline = Inline or Inlineable, then the Id should have a stable unfolding.

If you want to know where InlinePragmas take effect: Look in GHC.HsToCore.Binds.makeCorePair

Note [inl_inline and inl_act]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
* inl_inline says what the user wrote: did they say INLINE, NOINLINE,
  INLINABLE, OPAQUE, or nothing at all

* inl_act says in what phases the unfolding is active or inactive
  E.g  If you write INLINE[1]    then inl_act will be set to ActiveAfter 1
       If you write NOINLINE[1]  then inl_act will be set to ActiveBefore 1
       If you write NOINLINE[~1] then inl_act will be set to ActiveAfter 1
  So note that inl_act does not say what pragma you wrote: it just
  expresses its consequences

* inl_act just says when the unfolding is active; it doesn't say what
  to inline.  If you say INLINE f, then f's inl_act will be AlwaysActive,
  but in addition f will get a "stable unfolding" with UnfoldingGuidance
  that tells the inliner to be pretty eager about it.

Note [CONLIKE pragma]
~~~~~~~~~~~~~~~~~~~~~
The ConLike constructor of a RuleMatchInfo is aimed at the following.
Consider first
    {-# RULE "r/cons" forall a as. r (a:as) = f (a+1) #-}
    g b bs = let x = b:bs in ..x...x...(r x)...
Now, the rule applies to the (r x) term, because GHC "looks through"
the definition of 'x' to see that it is (b:bs).

Now consider
    {-# RULE "r/f" forall v. r (f v) = f (v+1) #-}
    g v = let x = f v in ..x...x...(r x)...
Normally the (r x) would *not* match the rule, because GHC would be
scared about duplicating the redex (f v), so it does not "look
through" the bindings.

However the CONLIKE modifier says to treat 'f' like a constructor in
this situation, and "look through" the unfolding for x.  So (r x)
fires, yielding (f (v+1)).

This is all controlled with a user-visible pragma:
     {-# NOINLINE CONLIKE [1] f #-}

The main effects of CONLIKE are:

    - The occurrence analyser (OccAnal) and simplifier (Simplify) treat
      CONLIKE thing like constructors, by ANF-ing them

    - New function GHC.Core.Utils.exprIsExpandable is like exprIsCheap, but
      additionally spots applications of CONLIKE functions

    - A CoreUnfolding has a field that caches exprIsExpandable

    - The rule matcher consults this field.  See
      Note [Expanding variables] in GHC.Core.Rules.

Note [OPAQUE pragma]
~~~~~~~~~~~~~~~~~~~~
Suppose a function `f` is marked {-# OPAQUE f #-}.  Then every call of `f`
should remain a call of `f` throughout optimisation; it should not be turned
into a call of a name-mangled variant of `f` (e.g by worker/wrapper).

The motivation for the OPAQUE pragma is discussed in GHC proposal 0415:
https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0415-opaque-pragma.rst
Basically it boils down to the desire of GHC API users and GHC RULE writers for
calls to certain binders to be left completely untouched by GHCs optimisations.

What this entails at the time of writing, is that for every binder annotated
with the OPAQUE pragma we:

* Do not do worker/wrapper via cast W/W:
  See the guard in GHC.Core.Opt.Simplify.tryCastWorkerWrapper

* Do not any worker/wrapper after demand/CPR analysis. To that end add a guard
  in GHC.Core.Opt.WorkWrap.tryWW to disable worker/wrapper

* It is important that the demand signature and CPR signature do not lie, else
  clients of the function will believe that it has the CPR property etc. But it
  won't, because we've disabled worker/wrapper. To avoid the signatures lying:
  * Strip boxity information from the demand signature
    in GHC.Core.Opt.DmdAnal.finaliseArgBoxities
    See Note [The OPAQUE pragma and avoiding the reboxing of arguments]
  * Strip CPR information from the CPR signature
    in GHC.Core.Opt.CprAnal.cprAnalBind
    See Note [The OPAQUE pragma and avoiding the reboxing of results]

* Do create specialised versions of the function in
  * Specialise: see GHC.Core.Opt.Specialise.specCalls
  * SpecConstr: see GHC.Core.Opt.SpecConstr.specialise
  Both are accomplished easily: these passes already skip NOINLINE
  functions with NeverActive activation, and an OPAQUE function is
  also NeverActive.

At the moment of writing, the major difference between the NOINLINE pragma and
the OPAQUE pragma is that binders annoted with the NOINLINE pragma _are_ W/W
transformed (see also Note [Worker/wrapper for NOINLINE functions]) where
binders annoted with the OPAQUE pragma are _not_ W/W transformed.

Future "name-mangling" optimisations should respect the OPAQUE pragma and
update the list of moving parts referenced in this note.

-}

data InlinePragmaExt = InlExt
  { inl_txt    :: SourceText     -- See Note [Pragma source text]
  , inl_arr    :: Maybe Arity    -- Just n <=> Inline only when applied to n
                                     --            explicit (non-type, non-dictionary) args
                                     --   That is, inl_sat describes the number of *source-code*
                                     --   arguments the thing must be applied to.  We add on the
                                     --   number of implicit, dictionary arguments when making
                                     --   the Unfolding, and don't look at inl_sat further
  }
  deriving Eq

inl_src  :: InlinePragma (GhcPass p) -> SourceText
inl_src (InlinePragma s _ _ _)      = inl_txt s

inl_sat  :: InlinePragma (GhcPass p) -> Maybe Arity
inl_sat (InlinePragma s _ _ _)      = inl_arr s

convertInlinePragma :: InlinePragma (GhcPass p) -> InlinePragma (GhcPass p')
convertInlinePragma (InlinePragma s a b c)    = InlinePragma s (convertInlineSpec a) (convertActivation b) c

convertInlineSpec :: InlineSpec (GhcPass p) -> InlineSpec (GhcPass p')
convertInlineSpec (Inline    ext)        = Inline    ext
convertInlineSpec (Inlinable ext)        = Inlinable ext
convertInlineSpec (NoInline  ext)        = NoInline  ext
convertInlineSpec (Opaque    ext)        = Opaque    ext
convertInlineSpec (NoUserInlinePrag ext) = NoUserInlinePrag ext
convertInlineSpec (XInlineSpec ext)      = XInlineSpec ext

noUserInlineSpec :: InlineSpec (GhcPass p) -> Bool
noUserInlineSpec (NoUserInlinePrag _) = True
noUserInlineSpec _                = False

defaultInlinePragma :: InlinePragma (GhcPass p)
defaultInlinePragma = InlinePragma { inl_ext = InlExt (SourceText $ fsLit "{-# INLINE") Nothing
                                   , inl_act = AlwaysActive noExtField
                                   , inl_rule = FunLike
                                   , inl_inline = NoUserInlinePrag noExtField
                                   }

set_pragma_inline :: InlinePragma (GhcPass p) -> InlineSpec (GhcPass p) -> InlinePragma (GhcPass p)
set_pragma_inline inl@(InlinePragma{}) spec = inl{ inl_inline = spec}

alwaysInlinePragma :: InlinePragma (GhcPass p)
alwaysInlinePragma = set_pragma_inline defaultInlinePragma $ Inline (inlinePragmaSource defaultInlinePragma)

neverInlinePragma :: InlinePragma (GhcPass p)
neverInlinePragma  = setInlinePragmaActivation defaultInlinePragma $ NeverActive noExtField

alwaysInlineConLikePragma :: InlinePragma (GhcPass p)
alwaysInlineConLikePragma = setInlinePragmaRuleMatchInfo alwaysInlinePragma ConLike

inlinePragmaSource :: InlinePragma (GhcPass p) -> SourceText
inlinePragmaSource prag = case inl_inline prag of
                            Inline    x        -> x
                            Inlinable y        -> y
                            NoInline  z        -> z
                            Opaque    q        -> q
                            NoUserInlinePrag _ -> NoSourceText
                            XInlineSpec imp    -> dataConCantHappen imp

inlineSpecSource :: InlineSpec (GhcPass p) -> SourceText
inlineSpecSource spec = case spec of
                            Inline    x        -> x
                            Inlinable y        -> y
                            NoInline  z        -> z
                            Opaque    q        -> q
                            NoUserInlinePrag _ -> NoSourceText
                            XInlineSpec imp    -> dataConCantHappen imp

-- A DFun has an always-active inline activation so that
-- exprIsConApp_maybe can "see" its unfolding
-- (However, its actual Unfolding is a DFunUnfolding, which is
--  never inlined other than via exprIsConApp_maybe.)
dfunInlinePragma :: InlinePragma (GhcPass p)
dfunInlinePragma = let
  always_active         = setInlinePragmaActivation defaultInlinePragma (AlwaysActive noExtField)
  always_active_conlike = setInlinePragmaRuleMatchInfo always_active ConLike
  in always_active_conlike

isDefaultInlinePragma :: InlinePragma (GhcPass p) -> Bool
isDefaultInlinePragma (InlinePragma { inl_act = activation
                                    , inl_rule = match_info
                                    , inl_inline = inline })
  = noUserInlineSpec inline && isAlwaysActive activation && isFunLike match_info

isInlinePragma :: InlinePragma (GhcPass p) -> Bool
isInlinePragma prag@(InlinePragma{}) = case inl_inline prag of
                        Inline _  -> True
                        _         -> False

isInlinablePragma :: InlinePragma (GhcPass p) -> Bool
isInlinablePragma prag@(InlinePragma{}) =
  case inl_inline prag of
    Inlinable _  -> True
    _            -> False

isNoInlinePragma :: InlinePragma (GhcPass p) -> Bool
isNoInlinePragma prag@(InlinePragma{}) =
  case inl_inline prag of
    NoInline _   -> True
    _            -> False

isAnyInlinePragma :: InlinePragma (GhcPass p) -> Bool
-- INLINE or INLINABLE
isAnyInlinePragma prag@(InlinePragma{}) =
  case inl_inline prag of
    Inline    _   -> True
    Inlinable _   -> True
    _             -> False

isOpaquePragma :: InlinePragma (GhcPass p) -> Bool
isOpaquePragma prag@(InlinePragma{}) =
  case inl_inline prag of
    Opaque _ -> True
    _        -> False


setInlinePragmaActivation :: InlinePragma (GhcPass p) -> Activation (GhcPass p) -> InlinePragma (GhcPass p)
setInlinePragmaActivation prag@(InlinePragma{}) activation = prag { inl_act = activation }

setInlinePragmaRuleMatchInfo :: InlinePragma (GhcPass p) -> RuleMatchInfo -> InlinePragma (GhcPass p)
setInlinePragmaRuleMatchInfo prag@(InlinePragma{}) rule = prag { inl_rule = rule }

setInlinePragmaArity :: InlinePragma (GhcPass p) -> Maybe Arity -> InlinePragma (GhcPass p)
setInlinePragmaArity inl@(InlinePragma{ inl_ext = ext}) sat = inl{ inl_ext = ext{inl_arr = sat} }

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

convertActivation :: Activation (GhcPass p) -> Activation (GhcPass p')
convertActivation (AlwaysActive ext    ) = AlwaysActive ext
convertActivation (ActiveBefore ext num) = ActiveBefore ext num
convertActivation (ActiveAfter  ext num) = ActiveAfter  ext num
convertActivation (FinalActive  ext    ) = FinalActive  ext
convertActivation (NeverActive  ext    ) = NeverActive  ext
convertActivation (XActivation  ext    ) = XActivation  ext

data CompilerPhase
  = InitialPhase    -- The first phase -- number = infinity!
  | Phase PhaseNum  -- User-specificable phases
  | FinalPhase      -- The last phase  -- number = -infinity!
  deriving Eq

-- See Note [Pragma source text]

beginPhase :: Activation (GhcPass p) -> CompilerPhase
-- First phase in which the Activation is active
-- or FinalPhase if it is never active
beginPhase (AlwaysActive  _) = InitialPhase
beginPhase (ActiveBefore {}) = InitialPhase
beginPhase (ActiveAfter _ n) = Phase n
beginPhase (FinalActive _)   = FinalPhase
beginPhase (NeverActive _)   = FinalPhase
beginPhase (XActivation imp) = dataConCantHappen imp

activeAfter :: CompilerPhase -> Activation (GhcPass p)
-- (activeAfter p) makes an Activation that is active in phase p and after
-- Invariant: beginPhase (activeAfter p) = p
activeAfter InitialPhase = AlwaysActive noExtField
activeAfter (Phase n)    = ActiveAfter NoSourceText n
activeAfter FinalPhase   = FinalActive  noExtField

nextPhase :: CompilerPhase -> CompilerPhase
-- Tells you the next phase after this one
-- Currently we have just phases [2,1,0,FinalPhase,FinalPhase,...]
-- Where FinalPhase means GHC's internal simplification steps
-- after all rules have run
nextPhase InitialPhase = Phase 2
nextPhase (Phase 0)    = FinalPhase
nextPhase (Phase n)    = Phase (n-1)
nextPhase FinalPhase   = FinalPhase

laterPhase :: CompilerPhase -> CompilerPhase -> CompilerPhase
-- Returns the later of two phases
laterPhase (Phase n1)   (Phase n2)   = Phase (n1 `min` n2)
laterPhase InitialPhase p2           = p2
laterPhase FinalPhase   _            = FinalPhase
laterPhase p1           InitialPhase = p1
laterPhase _            FinalPhase   = FinalPhase

activateAfterInitial :: Activation (GhcPass p)
-- Active in the first phase after the initial phase
activateAfterInitial = activeAfter (nextPhase InitialPhase)

activateDuringFinal :: Activation (GhcPass p)
-- Active in the final simplification phase (which is repeated)
activateDuringFinal = FinalActive noExtField

isActive :: CompilerPhase -> Activation (GhcPass p) -> Bool
isActive InitialPhase act = activeInInitialPhase act
isActive (Phase p)    act = activeInPhase p act
isActive FinalPhase   act = activeInFinalPhase act

activeInInitialPhase :: Activation (GhcPass p) -> Bool
activeInInitialPhase (AlwaysActive _)  = True
activeInInitialPhase (ActiveBefore {}) = True
activeInInitialPhase _                 = False

activeInPhase :: PhaseNum -> Activation (GhcPass p) -> Bool
activeInPhase _ (AlwaysActive _)   = True
activeInPhase _ (NeverActive  _)   = False
activeInPhase _ (FinalActive  _)   = False
activeInPhase p (ActiveAfter  _ n) = p <= n
activeInPhase p (ActiveBefore _ n) = p >  n
activeInPhase _ (XActivation imp)  = dataConCantHappen imp

activeInFinalPhase :: Activation (GhcPass p) -> Bool
activeInFinalPhase (AlwaysActive _) = True
activeInFinalPhase (FinalActive  _) = True
activeInFinalPhase (ActiveAfter {}) = True
activeInFinalPhase _                = False

isNeverActive, isAlwaysActive :: Activation (GhcPass p) -> Bool
isNeverActive (NeverActive _) = True
isNeverActive _              = False

isAlwaysActive (AlwaysActive _) = True
isAlwaysActive _                = False

competesWith :: Activation (GhcPass p) -> Activation (GhcPass p) -> Bool
-- See Note [Competing activations]
competesWith (AlwaysActive _)   _                = True

competesWith (NeverActive  _)   _                = False
competesWith _                  (NeverActive _)  = False

competesWith (FinalActive  _)   (FinalActive _)  = True
competesWith (FinalActive  _)   _                = False

competesWith (ActiveBefore {})  (AlwaysActive _)  = True
competesWith (ActiveBefore {})  (FinalActive  _)  = False
competesWith (ActiveBefore {})  (ActiveBefore {}) = True
competesWith (ActiveBefore _ a) (ActiveAfter _ b) = a < b

competesWith (ActiveAfter {})  (AlwaysActive _)   = False
competesWith (ActiveAfter {})  (FinalActive  _)   = True
competesWith (ActiveAfter {})  (ActiveBefore {})  = False
competesWith (ActiveAfter _ a) (ActiveAfter _ b)  = a >= b
competesWith (XActivation imp) _                  = dataConCantHappen imp
competesWith _                 (XActivation imp)  = dataConCantHappen imp

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
-}



{-
************************************************************************
*                                                                      *
\subsection{Outputable}
*                                                                      *
************************************************************************
-}
instance Outputable (Activation (GhcPass p)) where
   ppr (AlwaysActive _)   = empty
   ppr (NeverActive  _)   = brackets (text "~")
   ppr (ActiveBefore _ n) = brackets (char '~' <> int n)
   ppr (ActiveAfter  _ n) = brackets (int n)
   ppr (FinalActive  _)   = text "[final]"
   ppr (XActivation imp)  = dataConCantHappen imp

instance Outputable CompilerPhase where
   ppr (Phase n)    = int n
   ppr InitialPhase = text "InitialPhase"
   ppr FinalPhase   = text "FinalPhase"

instance Outputable RuleMatchInfo where
   ppr ConLike = text "CONLIKE"
   ppr FunLike = text "FUNLIKE"

instance Outputable (InlineSpec (GhcPass p)) where
    ppr (Inline          src)  = text "INLINE" <+> pprWithSourceText src empty
    ppr (NoInline        src)  = text "NOINLINE" <+> pprWithSourceText src empty
    ppr (Inlinable       src)  = text "INLINABLE" <+> pprWithSourceText src empty
    ppr (Opaque          src)  = text "OPAQUE" <+> pprWithSourceText src empty
    ppr (NoUserInlinePrag _ )  = empty
    ppr (XInlineSpec     imp)  = dataConCantHappen imp

instance Outputable (InlinePragma (GhcPass p)) where
  ppr = pprInline

-- | Outputs string for pragma name for any of INLINE/INLINABLE/NOINLINE. This
-- differs from the Outputable instance for the InlineSpec type where the pragma
-- name string as well as the accompanying SourceText (if any) is printed.
inlinePragmaName :: InlineSpec (GhcPass p) -> SDoc
inlinePragmaName (Inline            _)  = text "INLINE"
inlinePragmaName (Inlinable         _)  = text "INLINABLE"
inlinePragmaName (NoInline          _)  = text "NOINLINE"
inlinePragmaName (Opaque            _)  = text "OPAQUE"
inlinePragmaName (NoUserInlinePrag  _)   = empty
inlinePragmaName (XInlineSpec impossible) = dataConCantHappen impossible

-- | Pretty-print without displaying the user-specified 'InlineSpec'.
pprInline :: InlinePragma (GhcPass p) -> SDoc
pprInline = pprInline' True

-- | Pretty-print including the user-specified 'InlineSpec'.
pprInlineDebug :: InlinePragma (GhcPass p) -> SDoc
pprInlineDebug = pprInline' False

pprInline' :: Bool           -- True <=> do not display the inl_inline field
           -> InlinePragma (GhcPass p)
           -> SDoc
pprInline' emptyInline (InlinePragma
                        { inl_ext    = InlExt _ mb_arity,
                          inl_inline = inline,
                          inl_act = activation,
                          inl_rule = info
                        })
    = pp_inl inline <> pp_act inline activation <+> pp_sat <+> pp_info
    where
      pp_inl x = if emptyInline then empty else inlinePragmaName x

      pp_act Inline   {}  (AlwaysActive _) = empty
      pp_act NoInline {}  (NeverActive  _) = empty
      pp_act Opaque   {}  (NeverActive  _) = empty
      pp_act _            act              = ppr act

      pp_sat | Just ar <- mb_arity = parens (text "sat-args=" <> int ar)
             | otherwise           = empty
      pp_info | isFunLike info = empty
              | otherwise      = ppr info

instance Binary RuleMatchInfo where
    put_ bh FunLike = putByte bh 0
    put_ bh ConLike = putByte bh 1
    get bh = do
            h <- getByte bh
            if h == 1 then return ConLike
                      else return FunLike