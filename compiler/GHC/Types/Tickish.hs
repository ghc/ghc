{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}

module GHC.Types.Tickish (
  GenTickish(..),
  CoreTickish, StgTickish, CmmTickish,
  XTickishId,
  tickishCounts,
  tickishHasNoScope,
  tickishHasSoftScope,
  tickishFloatable,
  tickishCanSplit,
  mkNoCount,
  mkNoScope,
  tickishIsCode,
  isProfTick,
  TickishPlacement(..),
  tickishPlace,
  tickishContains,
  combineTickish_maybe,

  -- * Breakpoint tick identifiers
  BreakpointId(..), BreakTickIndex
) where

import GHC.Prelude
import GHC.Data.FastString
import Control.DeepSeq

import GHC.Core.Type

import GHC.Unit.Module

import GHC.Types.CostCentre
import GHC.Types.SrcLoc ( RealSrcSpan, containsSpan )
import GHC.Types.Var

import GHC.Utils.Panic

import Language.Haskell.Syntax.Extension ( NoExtField )

import Data.Data
import GHC.Utils.Binary
import GHC.Utils.Outputable (Outputable (ppr), text, (<+>))

{- *********************************************************************
*                                                                      *
              Ticks
*                                                                      *
************************************************************************
-}

-- | Allows attaching extra information to points in expressions

{- | Used as a data type index for the GenTickish annotations.
     See Note [Tickish passes]
 -}
data TickishPass
  = TickishPassCore
  | TickishPassStg
  | TickishPassCmm

{-
   Note [Tickish passes]
   ~~~~~~~~~~~~~~~~~~~~~
   Tickish annotations store different information depending on
   where they are used. Here's a summary of the differences
   between the passes.

   - CoreTickish: Haskell and Core
         The tickish annotations store the free variables of
         breakpoints.

   - StgTickish: Stg
         The GHCi bytecode generator (GHC.StgToByteCode) needs
         to know the type of each breakpoint in addition to its
         free variables. Since we cannot compute the type from
         an STG expression, the tickish annotations store the
         type of breakpoints in addition to the free variables.

   - CmmTickish: Cmm
         Breakpoints are unsupported and no free variables or
         type are stored.
 -}

type family XBreakpoint (pass :: TickishPass)
type instance XBreakpoint 'TickishPassCore = NoExtField
-- | Keep track of the type of breakpoints in STG, for GHCi
type instance XBreakpoint 'TickishPassStg  = Type
type instance XBreakpoint 'TickishPassCmm  = NoExtField

type family XTickishId (pass :: TickishPass)
type instance XTickishId 'TickishPassCore = Id
type instance XTickishId 'TickishPassStg = Id
type instance XTickishId 'TickishPassCmm = NoExtField

type CoreTickish = GenTickish 'TickishPassCore
type StgTickish = GenTickish 'TickishPassStg
-- | Tickish in Cmm context (annotations only)
type CmmTickish = GenTickish 'TickishPassCmm

-- If you edit this type, you may need to update the GHC formalism
-- See Note [GHC Formalism] in GHC.Core.Lint
data GenTickish pass =
    -- | An @{-# SCC #-}@ profiling annotation, either automatically
    -- added by the desugarer as a result of -auto-all, or added by
    -- the user.
    ProfNote {
      profNoteCC    :: CostCentre, -- ^ the cost centre

      profNoteCount :: !Bool,      -- ^ bump the entry count?
      profNoteScope :: !Bool       -- ^ scopes over the enclosed expression
                                   -- (i.e. not just a tick)
      -- Invariant: the False/False case never happens
    }

  -- | A "tick" used by HPC to track the execution of each
  -- subexpression in the original source code.
  | HpcTick {
      tickModule :: Module,
      tickId     :: !Int
    }

  -- | A breakpoint for the GHCi debugger.  This behaves like an HPC
  -- tick, but has a list of free variables which will be available
  -- for inspection in GHCi when the program stops at the breakpoint.
  --
  -- NB. we must take account of these Ids when (a) counting free variables,
  -- and (b) substituting (don't substitute for them)
  | Breakpoint
    { breakpointExt    :: XBreakpoint pass
    , breakpointId     :: !BreakpointId
    , breakpointFVs    :: [XTickishId pass]
                                -- ^ the order of this list is important:
                                -- it matches the order of the lists in the
                                -- appropriate entry in 'GHC.ByteCode.Types.ModBreaks'.
                                --
                                -- Careful about substitution!  See
                                -- Note [substTickish] in "GHC.Core.Subst".
    }

  -- | A source note.
  --
  -- Source notes are pure annotations: Their presence should neither
  -- influence compilation nor execution. The semantics are given by
  -- causality: The presence of a source note means that a local
  -- change in the referenced source code span will possibly provoke
  -- the generated code to change. On the flip-side, the functionality
  -- of annotated code *must* be invariant against changes to all
  -- source code *except* the spans referenced in the source notes
  -- (see "Causality of optimized Haskell" paper for details).
  --
  -- Therefore extending the scope of any given source note is always
  -- valid. Note that it is still undesirable though, as this reduces
  -- their usefulness for debugging and profiling. Therefore we will
  -- generally try only to make use of this property where it is
  -- necessary to enable optimizations.
  | SourceNote
    { sourceSpan :: RealSrcSpan -- ^ Source covered
    , sourceName :: LexicalFastString  -- ^ Name for source location
                                       --   (uses same names as CCs)
    }

deriving instance Eq (GenTickish 'TickishPassCore)
deriving instance Ord (GenTickish 'TickishPassCore)
deriving instance Data (GenTickish 'TickishPassCore)

deriving instance Data (GenTickish 'TickishPassStg)

deriving instance Eq (GenTickish 'TickishPassCmm)
deriving instance Ord (GenTickish 'TickishPassCmm)
deriving instance Data (GenTickish 'TickishPassCmm)

--------------------------------------------------------------------------------
-- Tick breakpoint index
--------------------------------------------------------------------------------

-- | Breakpoint tick index
-- newtype BreakTickIndex = BreakTickIndex Int
--   deriving (Eq, Ord, Data, Ix, NFData, Outputable)
type BreakTickIndex = Int

-- | Breakpoint identifier.
--
-- Indexes into the structures in the @'ModBreaks'@ created during desugaring
-- (after inserting the breakpoint ticks in the expressions).
-- See Note [Breakpoint identifiers]
data BreakpointId = BreakpointId
  { bi_tick_mod   :: !Module         -- ^ Breakpoint tick module
  , bi_tick_index :: !BreakTickIndex -- ^ Breakpoint tick index
  }
  deriving (Eq, Ord, Data)

instance Outputable BreakpointId where
  ppr BreakpointId{bi_tick_mod, bi_tick_index} =
    text "BreakpointId" <+> ppr bi_tick_mod <+> ppr bi_tick_index

instance NFData BreakpointId where
  rnf BreakpointId{bi_tick_mod, bi_tick_index} =
    rnf bi_tick_mod `seq` rnf bi_tick_index

instance Binary BreakpointId where
  get bh = BreakpointId <$> get bh <*> get bh

  put_ bh BreakpointId {..} = put_ bh bi_tick_mod *> put_ bh bi_tick_index

--------------------------------------------------------------------------------

-- | A "counting tick" (for which 'tickishCounts' is True) is one that
-- counts evaluations in some way.  We cannot discard a counting tick,
-- and the compiler should preserve the number of counting ticks (as
-- far as possible).
--
-- See Note [Counting ticks]
tickishCounts :: GenTickish pass -> Bool
tickishCounts = \case
  ProfNote { profNoteCount = counts } -> counts
  HpcTick {}                          -> True
  Breakpoint {}                       -> True
  SourceNote {}                       -> False

-- | Is this a non-scoping tick, for which we don't care about precisely
-- the extent of code that the tick encompasses?
--
-- See Note [Scoped ticks]
tickishHasNoScope :: GenTickish pass -> Bool
tickishHasNoScope = \case
  ProfNote { profNoteScope = scopes } -> not scopes
  HpcTick {}                          -> True
  Breakpoint {}                       -> False
  SourceNote {}                       -> False

-- | A "tick with soft scoping" (for which 'tickishHasSoftScope' is True) is
-- one that either does not scope at all (for which 'tickishHasNoScope' is True),
-- or that has a "soft" scope: we allow new code to be floated into to the scope,
-- as long as all code that was covered remains covered.
--
-- See Note [Scoped ticks]
tickishHasSoftScope :: GenTickish pass -> Bool
tickishHasSoftScope = \case
  ProfNote { profNoteScope = scopes } -> not scopes
  HpcTick {}                          -> True
  Breakpoint {}                       -> False
  SourceNote {}                       -> True

{- Note [Scoping ticks and counting ticks]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Ticks have two independent attributes:

  * Whether the tick /counts/.
    Counting ticks are used when we want a counter to be bumped, e.g. counting
    how many times a function is called.

    See Note [Counting ticks]

  * What kind of /scope/ the tick has:
     * Cost-centre scope: you cannot move a redex into the scope of the tick,
                          nor can you float a redex out.
     * Soft scope: you can move a redex /into/ the scope of a tick,
                   but you cannot float a redex /out/
     * No scope: there are no restrictions on floating in or out.

     See Note [Scoped ticks]

Note that profiling notes which both count and scope can be split into two
separate ticks, one that counts and doesn't scope and one that scopes and doesn't
count; see 'tickishCanSplit', 'mkNoCount' and 'mkNoScope'.

Note [Counting ticks]
~~~~~~~~~~~~~~~~~~~~~
The following ticks count:
  - ProfNote ticks with profNoteCounts = True
  - HPC ticks
  - Breakpoints

Going past a counting tick implies bumping a counter.
Generally, the simplifier attempts to preserve counts when transforming
programs and moving ticks, for example by transforming:

  case <tick> e of
    alt1 -> rhs1
    alt2 -> rhs2

to

  case e of
    alt1 -> <tick> rhs1
    alt2 -> <tick> rhs2

which preserves the total count (as exactly one branch of the case
will be taken).

However, we still allow the simplifier to increase or decrease
sharing, so in practice the actual number of ticks may vary, except
that we never change the value from zero to non-zero or vice-versa.

Note [Scoped ticks]
~~~~~~~~~~~~~~~~~~~
The following ticks are scoped:
  - ProfNote ticks with profNoteScope = True
  - Breakpoints
  - Source notes

A scoped tick is one that scopes over a portion of code. For example,
an SCC anotation sets the cost centre for the code within; any allocations
within that piece of code should get attributed to that cost centre.

When the simplifier deals with a scoping tick, it ensures that all code that
was covered remains covered. For example

  let x = tick<...> (let y = foo in bar) in baz
    ===>
  let x = tick<...> bar; y = tick<...> foo in baz

is a valid transformation as far as "bar" and "foo" are concerned, because
both still are scoped over by the tick. One might object to the "let" not
being covered by the tick any more. However, we are generally lax with this;
constant costs don't matter too much, and given that the "let" was effectively
merged we can view it as having lost its identity anyway.

Perhaps surprisingly, breakpoints are considered to be scoped, because we
don't want the simplifier to move them around, changing their result type (see #1531).

We specifically forbid floating code outside of a scoping tick, as cost
associated with the floated-out code would no longer be attributed to the
appropriate scope.

Whether we are allowed to float in additional cost depends on the tick:

  Cost-centre scope ticks
    - ProfNote with profNoteScope = True
    - Breakpoints

    A tick with cost-centre scope is one for which we can neither move
    redexes into or move redexes outside of the tick. For example, we don't
    want profiling costs to move to other cost-centre stacks.
    Morever, we also object to changing the order in which such ticks
    are applied.

    A rule of thumb is that we don't want any code to gain new
    lexically-enclosing ticks. For example, we should not transform:

      f (scctick<foo> a)  ==>  scctick<foo> (f a)

    as this would attribute the cost of evaluating the application 'f a'
    to the cost centre 'foo'.

    However, there are notable exceptions, for example:

      let f = \y -> foo in tick<...> ... (f x) ...
        ==>
      tick<...> ... foo[x/y] ...

    Inlining lambdas like this is always legal, because inlining a function
    does not change the cost-centre stack when the function is called.

  Soft scope ticks
    - Source notes

    A tick with soft scope is one for which we can move redexes inside the
    tick, but cannot float redexes outside the tick. This is a slightly more
    lenient notion of scoping than cost-centres, and is used only for source
    note ticks (they are used to provide DWARF debug symbols, and for those
    it matters less if code from outside gets moved under the tick).

    Examples:

      - FloatIn (GHC.Core.Opt.FloatIn.fiExpr)

          let x = rhs in <tick> body
            ==>
          <tick> (let x = rhs in body)

      - Moving a tick outside of a case or of an application
        (GHC.Core.Opt.Simplify.Iteration.simplTick)

          case <tick> e of alts  ==>  <tick> case e of alts

          (<tick> e1) e2         ==>  <tick> (e1 e2)

    While these transformations are legal, we want to make a best effort to
    only make use of them where it exposes transformation opportunities.

Note [Tickish placement]
~~~~~~~~~~~~~~~~~~~~~~~~
The placement behaviour of ticks (i.e. which terms we want the tick to be placed
around in the AST) is governed by 'TickishPlacement'. We generally try to push
ticks inwards until they end up placed around the kind of term expected by their
placement rules.

From most restrictive to least restrictive placement rules:

  - PlaceRuntime: counting ticks.

    Ticks with 'PlaceRuntime' placement want to be placed on run-time expressions.
    They can be moved through pure compile-time constructs such as other ticks,
    casts or type lambdas.

    This is the most restrictive placement rule for ticks, as all tickishs have
    in common that they want to track runtime processes.

    Any tick that counts (see Note [Counting ticks]) has 'PlaceRuntime' placement.

  - PlaceNonLam: source notes.

    Like PlaceRuntime, but we can also float the tick through value lambdas.
    This makes sense where there is little difference between annotating the
    lambda and annotating the lambda's code.

  - PlaceCostCentre: non-counting profiling ticks.

    In addition to floating through lambdas, cost-centre style tickishs can also
    be moved from constructors and non-function variables. For example:

       let x = scc<...> C (scc<...> y) (scc<...> 3) in ...

    Neither the constructor application, the variable or the literal are likely
    to have any cost worth mentioning. And even if 'y' names a thunk, the call
    would not care about the evaluation context. Therefore, removing all
    annotations in the above example is safe.
-}

-- | Returns @True@ for ticks that can be floated upwards easily even
-- where it might change execution counts, such as:
--
--   Just (tick<...> foo)
--     ==>
--   tick<...> (Just foo)
--
-- This is a combination of @tickishHasSoftScope@ and @tickishCounts@.
-- Note that in principle splittable ticks can become floatable using @mkNoTick@,
-- even though there's currently no tickish for which that is the case.
tickishFloatable :: GenTickish pass -> Bool
tickishFloatable t = tickishHasSoftScope t && not (tickishCounts t)

-- | Returns @True@ for a tick that is both counting /and/ scoping and
-- can be split into its (tick, scope) parts using 'mkNoScope' and
-- 'mkNoTick' respectively.
tickishCanSplit :: GenTickish pass -> Bool
tickishCanSplit ProfNote{profNoteScope = True, profNoteCount = True}
                   = True
tickishCanSplit _  = False

mkNoCount :: GenTickish pass -> GenTickish pass
mkNoCount n | not (tickishCounts n)   = n
            | not (tickishCanSplit n) = panic "mkNoCount: Cannot split!"
mkNoCount n@ProfNote{}                = let n' = n {profNoteCount = False}
                                        in assert (profNoteCount n) n'
mkNoCount _                           = panic "mkNoCount: Undefined split!"

mkNoScope :: GenTickish pass -> GenTickish pass
mkNoScope n | tickishHasNoScope n         = n
            | not (tickishCanSplit n)     = panic "mkNoScope: Cannot split!"
mkNoScope n@ProfNote{}                    = let n' = n {profNoteScope = False}
                                            in assert (profNoteCount n) n'
mkNoScope _                               = panic "mkNoScope: Undefined split!"

-- | Return @True@ if this source annotation compiles to some backend
-- code. Without this flag, the tickish is seen as a simple annotation
-- that does not have any associated evaluation code.
--
-- What this means that we are allowed to disregard the tick if doing
-- so means that we can skip generating any code in the first place. A
-- typical example is top-level bindings:
--
--   foo = tick<...> \y -> ...
--     ==>
--   foo = \y -> tick<...> ...
--
-- Here there is just no operational difference between the first and
-- the second version. Therefore code generation should simply
-- translate the code as if it found the latter.
tickishIsCode :: GenTickish pass -> Bool
tickishIsCode SourceNote{} = False
tickishIsCode ProfNote{}   = True
tickishIsCode Breakpoint{} = True
tickishIsCode HpcTick{}    = True

isProfTick :: GenTickish pass -> Bool
isProfTick ProfNote{} = True
isProfTick _          = False

-- | Governs the kind of expression that the tick gets placed on when
-- annotating for example using @mkTick@. If we find that we want to
-- put a tickish on an expression ruled out here, we try to float it
-- inwards until we find a suitable expression.
--
-- See Note [Tickish placement].
data TickishPlacement =

    -- | Place ticks exactly on run-time expressions, moving them through pure
    -- compile-time constructs such as other ticks, casts or type lambdas.
    PlaceRuntime

    -- | As @PlaceRuntime@, but also allow to float the tick through all lambdas.
  | PlaceNonLam

    -- | As 'PlaceNonLam', but also float through constructors, non-function
    -- variables and literals.
  | PlaceCostCentre

  deriving (Eq,Show)

instance Outputable TickishPlacement where
  ppr = text . show

-- | Placement behaviour we want for the ticks.
--
-- See Note [Tickish placement].
tickishPlace :: GenTickish pass -> TickishPlacement
tickishPlace n@ProfNote{}
  | profNoteCount n        = PlaceRuntime
  | otherwise              = PlaceCostCentre
tickishPlace HpcTick{}     = PlaceRuntime
tickishPlace Breakpoint{}  = PlaceRuntime
tickishPlace SourceNote{}  = PlaceNonLam

-- | Merge two ticks into one, if that is possible.
--
-- Examples:
--
--  - combine two source note ticks if one contains the other,
--  - combine a non-counting profiling tick with a non-scoping profiling tick
--    for the same cost centre
--  - combine two equal breakpoint ticks or HPC ticks
combineTickish_maybe :: Eq (GenTickish pass)
                   => GenTickish pass -> GenTickish pass -> Maybe (GenTickish pass)
combineTickish_maybe
  (ProfNote { profNoteCC = cc1, profNoteCount = cnt1, profNoteScope = scope1 })
  (ProfNote { profNoteCC = cc2, profNoteCount = cnt2, profNoteScope = scope2 })
    | cc1 == cc2
    , not cnt1 || not cnt2
    = Just $ ProfNote { profNoteCC    = cc1
                      , profNoteCount = cnt1 || cnt2
                      , profNoteScope = scope1 || scope2
                      }
combineTickish_maybe t1@(SourceNote sp1 n1) t2@(SourceNote sp2 n2)
  | n1 == n2
  , sp1 `containsSpan` sp2
  = Just t1
  | n1 == n2
  , sp2 `containsSpan` sp1
  = Just t2
  -- NB: it would be possible to use 'combineRealSrcSpans' instead,
  -- but that has the risk of combining many source note ticks into a single
  -- tick with a huge source span.
combineTickish_maybe t1@(HpcTick {}) t2@(HpcTick {})
  | t1 == t2
  = Just t1
combineTickish_maybe t1@(Breakpoint {}) t2@(Breakpoint {})
  | t1 == t2
  = Just t1
combineTickish_maybe _ _ = Nothing

-- | Returns whether one tick "contains" the other one, therefore
-- making the second tick redundant.
tickishContains :: Eq (GenTickish pass)
                => GenTickish pass -> GenTickish pass -> Bool
tickishContains (SourceNote sp1 n1) (SourceNote sp2 n2)
  = containsSpan sp1 sp2 && n1 == n2
    -- compare the String last
tickishContains t1 t2
  = t1 == t2
