{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE UndecidableInstances #-} -- NFData InlinePragma

module Language.Haskell.Syntax.Binds.InlinePragma
  ( -- * Inline Pragma Encoding
    -- ** InlinePragma
    -- *** Data-type
    InlinePragma(..)
    -- *** Queries
  , isAnyInlinePragma
  , isInlinablePragma
  , isInlinePragma
  , isNoInlinePragma
  , isOpaquePragma

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
  , Activation
  , ActivationX(..)
  , PhaseNum
  ) where

import Language.Haskell.Syntax.Extension

import Control.DeepSeq (NFData(..))
import Data.Bool
import Data.Data (Data)

import Prelude -- (Eq, Int, Show, ($), seq)

{-
************************************************************************
*                                                                      *
\subsection[InlinePragma]{The InlinePragma definitions}
*                                                                      *
************************************************************************
-}

data InlinePragma pass                      -- Note [InlinePragma]
  = InlinePragma
      { inl_ext    :: !(XInlinePragma pass) -- See Note [Pragma source text]
      , inl_inline :: !InlineSpec           -- See Note [inl_inline and inl_act]
      , inl_act    :: !(Activation pass)    -- Says during which phases inlining is allowed
                                            -- See Note [inl_inline and inl_act]
      , inl_rule   :: !RuleMatchInfo        -- Should the function be treated like a constructor?
    }
  | XInlinePragma !(XXInlinePragma pass)

deriving instance ( Eq (XXActivation pass)
                  , Eq (XInlinePragma pass)
                  , Eq (XXInlinePragma pass)) => Eq (InlinePragma pass)

instance ( NFData (XInlinePragma p)
         , NFData (XXInlinePragma p)
         , NFData (XXActivation p)) => NFData (InlinePragma p) where
  rnf (InlinePragma x a b c) = rnf x `seq` rnf a `seq` rnf b `seq` rnf c
  rnf (XInlinePragma x) = rnf x

-- | Inline Specification
data InlineSpec -- What the user's INLINE pragma looked like
  = Inline      -- User wrote INLINE
  | Inlinable   -- User wrote INLINABLE
  | NoInline    -- User wrote NOINLINE
  | Opaque      -- User wrote OPAQUE
                -- Each of the above keywords is accompanied with
                -- a string of type SourceText written by the user
  | NoUserInlinePrag -- User did not write any of INLINE/INLINABLE/NOINLINE
                     -- e.g. in `defaultInlinePragma` or when created by CSE
  deriving( Eq, Data, Show )
        -- Show needed for GHC.Parser.Lexer

instance NFData InlineSpec where
  rnf = \case
    Inline -> ()
    Inlinable -> ()
    NoInline -> ()
    Opaque -> ()
    NoUserInlinePrag -> ()

-- | Rule Match Information
data RuleMatchInfo
  = ConLike -- See Note [CONLIKE pragma]
  | FunLike
  deriving( Eq, Data, Show )
        -- Show needed for GHC.Parser.Lexer

instance NFData RuleMatchInfo where
  rnf = \case
    ConLike -> ()
    FunLike -> ()

{- Note [InlinePragma]
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

isConLike :: RuleMatchInfo -> Bool
isConLike ConLike = True
isConLike _       = False

isFunLike :: RuleMatchInfo -> Bool
isFunLike FunLike = True
isFunLike _       = False

noUserInlineSpec :: InlineSpec -> Bool
noUserInlineSpec NoUserInlinePrag = True
noUserInlineSpec _                = False

isInlinePragma :: InlinePragma p -> Bool
isInlinePragma = queryInlineSpec $ \case
  Inline -> True
  _      -> False

isInlinablePragma :: InlinePragma p -> Bool
isInlinablePragma = queryInlineSpec $ \case
  Inlinable -> True
  _         -> False

isNoInlinePragma :: InlinePragma p -> Bool
isNoInlinePragma = queryInlineSpec $ \case
  NoInline -> True
  _        -> False

isAnyInlinePragma :: InlinePragma p -> Bool
-- INLINE or INLINABLE
isAnyInlinePragma = queryInlineSpec $ \case
  Inline    -> True
  Inlinable -> True
  _         -> False

isOpaquePragma :: InlinePragma p -> Bool
isOpaquePragma = queryInlineSpec $ \case
  Opaque -> True
  _      -> False

queryInlineSpec :: (InlineSpec -> Bool) -> InlinePragma p -> Bool
queryInlineSpec _ (XInlinePragma _) = False
queryInlineSpec f (InlinePragma { inl_inline = line }) = f line

-- | Compilation phase number, as can be written by users in INLINE pragmas,
-- SPECIALISE pragmas, and RULES.
--
--   - phases decrease towards zero
--   - zero is the last phase
--
-- Does not include GHC internal "initial" and "final" phases; see 'CompilerPhase'.
type PhaseNum = Int


-- The TTG data type; use this for TTG fields
type Activation pass = ActivationX (XXActivation pass)

-- | An activation is a range of phases throughout which something is active
-- (like an INLINE pragma, SPECIALISE pragma, or RULE).
data ActivationX e
  = AlwaysActive
  -- | Active only *strictly before* this phase
  | ActiveBefore PhaseNum
  -- | Active in this phase and later phases
  | ActiveAfter  PhaseNum
  -- | Active in the final phase only
  | NeverActive
  | XActivation !e

deriving instance Eq e => Eq (ActivationX e)
  -- Eq used in comparing rules in GHC.Hs.Decls

instance NFData e => NFData (ActivationX e) where
  rnf = \case
    AlwaysActive -> ()
    NeverActive -> ()
    ActiveBefore aa -> rnf aa
    ActiveAfter ab -> rnf ab
    XActivation x -> rnf x `seq` ()
