{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module GHC.Types.Tickish (
  GenTickish(..),
  CoreTickish, StgTickish, CmmTickish,
  XTickishId,
  tickishCounts,
  TickishScoping(..),
  tickishScoped,
  tickishScopesLike,
  tickishFloatable,
  tickishCanSplit,
  mkNoCount,
  mkNoScope,
  tickishIsCode,
  TickishPlacement(..),
  tickishPlace,
  tickishContains
) where

import GHC.Prelude

import GHC.Core.Type

import GHC.Unit.Module

import GHC.Types.CostCentre
import GHC.Types.SrcLoc ( RealSrcSpan, containsSpan )
import GHC.Types.Var

import GHC.Utils.Panic

import Language.Haskell.Syntax.Extension ( NoExtField )

import Data.Data

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
    , breakpointId     :: !Int
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
    , sourceName :: String      -- ^ Name for source location
                                --   (uses same names as CCs)
    }

deriving instance Eq (GenTickish 'TickishPassCore)
deriving instance Ord (GenTickish 'TickishPassCore)
deriving instance Data (GenTickish 'TickishPassCore)

deriving instance Data (GenTickish 'TickishPassStg)

deriving instance Eq (GenTickish 'TickishPassCmm)
deriving instance Ord (GenTickish 'TickishPassCmm)
deriving instance Data (GenTickish 'TickishPassCmm)


-- | A "counting tick" (where tickishCounts is True) is one that
-- counts evaluations in some way.  We cannot discard a counting tick,
-- and the compiler should preserve the number of counting ticks as
-- far as possible.
--
-- However, we still allow the simplifier to increase or decrease
-- sharing, so in practice the actual number of ticks may vary, except
-- that we never change the value from zero to non-zero or vice versa.
tickishCounts :: GenTickish pass -> Bool
tickishCounts n@ProfNote{} = profNoteCount n
tickishCounts HpcTick{}    = True
tickishCounts Breakpoint{} = True
tickishCounts _            = False


-- | Specifies the scoping behaviour of ticks. This governs the
-- behaviour of ticks that care about the covered code and the cost
-- associated with it. Important for ticks relating to profiling.
data TickishScoping =
    -- | No scoping: The tick does not care about what code it
    -- covers. Transformations can freely move code inside as well as
    -- outside without any additional annotation obligations
    NoScope

    -- | Soft scoping: We want all code that is covered to stay
    -- covered.  Note that this scope type does not forbid
    -- transformations from happening, as long as all results of
    -- the transformations are still covered by this tick or a copy of
    -- it. For example
    --
    --   let x = tick<...> (let y = foo in bar) in baz
    --     ===>
    --   let x = tick<...> bar; y = tick<...> foo in baz
    --
    -- Is a valid transformation as far as "bar" and "foo" is
    -- concerned, because both still are scoped over by the tick.
    --
    -- Note though that one might object to the "let" not being
    -- covered by the tick any more. However, we are generally lax
    -- with this - constant costs don't matter too much, and given
    -- that the "let" was effectively merged we can view it as having
    -- lost its identity anyway.
    --
    -- Also note that this scoping behaviour allows floating a tick
    -- "upwards" in pretty much any situation. For example:
    --
    --   case foo of x -> tick<...> bar
    --     ==>
    --   tick<...> case foo of x -> bar
    --
    -- While this is always legal, we want to make a best effort to
    -- only make us of this where it exposes transformation
    -- opportunities.
  | SoftScope

    -- | Cost centre scoping: We don't want any costs to move to other
    -- cost-centre stacks. This means we not only want no code or cost
    -- to get moved out of their cost centres, but we also object to
    -- code getting associated with new cost-centre ticks - or
    -- changing the order in which they get applied.
    --
    -- A rule of thumb is that we don't want any code to gain new
    -- annotations. However, there are notable exceptions, for
    -- example:
    --
    --   let f = \y -> foo in tick<...> ... (f x) ...
    --     ==>
    --   tick<...> ... foo[x/y] ...
    --
    -- In-lining lambdas like this is always legal, because inlining a
    -- function does not change the cost-centre stack when the
    -- function is called.
  | CostCentreScope

  deriving (Eq)

-- | Returns the intended scoping rule for a Tickish
tickishScoped :: GenTickish pass -> TickishScoping
tickishScoped n@ProfNote{}
  | profNoteScope n        = CostCentreScope
  | otherwise              = NoScope
tickishScoped HpcTick{}    = NoScope
tickishScoped Breakpoint{} = CostCentreScope
   -- Breakpoints are scoped: eventually we're going to do call
   -- stacks, but also this helps prevent the simplifier from moving
   -- breakpoints around and changing their result type (see #1531).
tickishScoped SourceNote{} = SoftScope

-- | Returns whether the tick scoping rule is at least as permissive
-- as the given scoping rule.
tickishScopesLike :: GenTickish pass -> TickishScoping -> Bool
tickishScopesLike t scope = tickishScoped t `like` scope
  where NoScope         `like` _               = True
        _               `like` NoScope         = False
        SoftScope       `like` _               = True
        _               `like` SoftScope       = False
        CostCentreScope `like` _               = True

-- | Returns @True@ for ticks that can be floated upwards easily even
-- where it might change execution counts, such as:
--
--   Just (tick<...> foo)
--     ==>
--   tick<...> (Just foo)
--
-- This is a combination of @tickishSoftScope@ and
-- @tickishCounts@. Note that in principle splittable ticks can become
-- floatable using @mkNoTick@ -- even though there's currently no
-- tickish for which that is the case.
tickishFloatable :: GenTickish pass -> Bool
tickishFloatable t = t `tickishScopesLike` SoftScope && not (tickishCounts t)

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
mkNoCount n@ProfNote{}                = n {profNoteCount = False}
mkNoCount _                           = panic "mkNoCount: Undefined split!"

mkNoScope :: GenTickish pass -> GenTickish pass
mkNoScope n | tickishScoped n == NoScope  = n
            | not (tickishCanSplit n)     = panic "mkNoScope: Cannot split!"
mkNoScope n@ProfNote{}                    = n {profNoteScope = False}
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
tickishIsCode _tickish     = True  -- all the rest for now


-- | Governs the kind of expression that the tick gets placed on when
-- annotating for example using @mkTick@. If we find that we want to
-- put a tickish on an expression ruled out here, we try to float it
-- inwards until we find a suitable expression.
data TickishPlacement =

    -- | Place ticks exactly on run-time expressions. We can still
    -- move the tick through pure compile-time constructs such as
    -- other ticks, casts or type lambdas. This is the most
    -- restrictive placement rule for ticks, as all tickishs have in
    -- common that they want to track runtime processes. The only
    -- legal placement rule for counting ticks.
    PlaceRuntime

    -- | As @PlaceRuntime@, but we float the tick through all
    -- lambdas. This makes sense where there is little difference
    -- between annotating the lambda and annotating the lambda's code.
  | PlaceNonLam

    -- | In addition to floating through lambdas, cost-centre style
    -- tickishs can also be moved from constructors, non-function
    -- variables and literals. For example:
    --
    --   let x = scc<...> C (scc<...> y) (scc<...> 3) in ...
    --
    -- Neither the constructor application, the variable or the
    -- literal are likely to have any cost worth mentioning. And even
    -- if y names a thunk, the call would not care about the
    -- evaluation context. Therefore removing all annotations in the
    -- above example is safe.
  | PlaceCostCentre

  deriving (Eq)

-- | Placement behaviour we want for the ticks
tickishPlace :: GenTickish pass -> TickishPlacement
tickishPlace n@ProfNote{}
  | profNoteCount n        = PlaceRuntime
  | otherwise              = PlaceCostCentre
tickishPlace HpcTick{}     = PlaceRuntime
tickishPlace Breakpoint{}  = PlaceRuntime
tickishPlace SourceNote{}  = PlaceNonLam

-- | Returns whether one tick "contains" the other one, therefore
-- making the second tick redundant.
tickishContains :: Eq (GenTickish pass)
                => GenTickish pass -> GenTickish pass -> Bool
tickishContains (SourceNote sp1 n1) (SourceNote sp2 n2)
  = containsSpan sp1 sp2 && n1 == n2
    -- compare the String last
tickishContains t1 t2
  = t1 == t2
