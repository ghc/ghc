{-
(c) The AQUA Project, Glasgow University, 1993-1998

-}

{-# LANGUAGE DerivingVia #-}

{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}

module GHC.Core.Opt.Utils (
    -- * Configuration of the core-to-core passes
    SimplMode(..),
    FloatOutSwitches(..),
    FloatEnable(..),
    floatEnable,

    -- ** Dealing with annotations
    getAnnotationsFromHscEnv, getFirstAnnotationsFromHscEnv,
  ) where

import GHC.Prelude

import GHC.Driver.Session
import GHC.Driver.Env

import GHC.Core.Unfold

import GHC.Types.Basic  ( CompilerPhase(..) )
import GHC.Types.Annotations
import GHC.Types.Name.Env
import GHC.Types.Error

import GHC.Utils.Outputable as Outputable
import GHC.Utils.Logger

import GHC.Unit.Module
import GHC.Unit.Module.ModGuts

import Data.Bifunctor ( bimap )
import Data.List (intersperse)
import Data.Dynamic
import Data.Word

data FloatEnable  -- Controls local let-floating
  = FloatDisabled      -- Do no local let-floating
  | FloatNestedOnly    -- Local let-floating for nested (NotTopLevel) bindings only
  | FloatEnabled       -- Do local let-floating on all bindings

floatEnable :: DynFlags -> FloatEnable
floatEnable dflags =
  case (gopt Opt_LocalFloatOut dflags, gopt Opt_LocalFloatOutTopLevel dflags) of
    (True, True) -> FloatEnabled
    (True, False)-> FloatNestedOnly
    (False, _)   -> FloatDisabled

{-
Note [Local floating]
~~~~~~~~~~~~~~~~~~~~~
The Simplifier can perform local let-floating: it floats let-bindings
out of the RHS of let-bindings.  See
  Let-floating: moving bindings to give faster programs (ICFP'96)
  https://www.microsoft.com/en-us/research/publication/let-floating-moving-bindings-to-give-faster-programs/

Here's an example
   x = let y = v+1 in (y,true)

The RHS of x is a thunk.  Much better to float that y-binding out to give
   y = v+1
   x = (y,true)

Not only have we avoided building a thunk, but any (case x of (p,q) -> ...) in
the scope of the x-binding can now be simplified.

This local let-floating is done in GHC.Core.Opt.Simplify.prepareBinding,
controlled by the predicate GHC.Core.Opt.Simplify.Env.doFloatFromRhs.

The `FloatEnable` data type controls where local let-floating takes place;
it allows you to specify that it should be done only for nested bindings;
or for top-level bindings as well; or not at all.

Note that all of this is quite separate from the global FloatOut pass;
see GHC.Core.Opt.FloatOut.

-}

data SimplMode             -- See comments in GHC.Core.Opt.Simplify.Monad
  = SimplMode
        { sm_names        :: [String]       -- ^ Name(s) of the phase
        , sm_phase        :: CompilerPhase
        , sm_uf_opts      :: !UnfoldingOpts -- ^ Unfolding options
        , sm_rules        :: !Bool          -- ^ Whether RULES are enabled
        , sm_inline       :: !Bool          -- ^ Whether inlining is enabled
        , sm_case_case    :: !Bool          -- ^ Whether case-of-case is enabled
        , sm_eta_expand   :: !Bool          -- ^ Whether eta-expansion is enabled
        , sm_cast_swizzle :: !Bool          -- ^ Do we swizzle casts past lambdas?
        , sm_pre_inline   :: !Bool          -- ^ Whether pre-inlining is enabled
        , sm_float_enable :: !FloatEnable   -- ^ Whether to enable floating out
        , sm_logger       :: !Logger
        , sm_dflags       :: DynFlags
            -- Just for convenient non-monadic access; we don't override these.
            --
            -- Used for:
            --    - target platform (for `exprIsDupable` and `mkDupableAlt`)
            --    - Opt_DictsCheap and Opt_PedanticBottoms general flags
            --    - rules options (initRuleOpts)
            --    - inlineCheck
        }

instance Outputable SimplMode where
    ppr (SimplMode { sm_phase = p, sm_names = ss
                   , sm_rules = r, sm_inline = i
                   , sm_cast_swizzle = cs
                   , sm_eta_expand = eta, sm_case_case = cc })
       = text "SimplMode" <+> braces (
         sep [ text "Phase =" <+> ppr p <+>
               brackets (text (concat $ intersperse "," ss)) <> comma
             , pp_flag i   (text "inline") <> comma
             , pp_flag r   (text "rules") <> comma
             , pp_flag eta (text "eta-expand") <> comma
             , pp_flag cs (text "cast-swizzle") <> comma
             , pp_flag cc  (text "case-of-case") ])
         where
           pp_flag f s = ppUnless f (text "no") <+> s

data FloatOutSwitches = FloatOutSwitches {
  floatOutLambdas   :: Maybe Int,  -- ^ Just n <=> float lambdas to top level, if
                                   -- doing so will abstract over n or fewer
                                   -- value variables
                                   -- Nothing <=> float all lambdas to top level,
                                   --             regardless of how many free variables
                                   -- Just 0 is the vanilla case: float a lambda
                                   --    iff it has no free vars

  floatOutConstants :: Bool,       -- ^ True <=> float constants to top level,
                                   --            even if they do not escape a lambda
  floatOutOverSatApps :: Bool,
                             -- ^ True <=> float out over-saturated applications
                             --            based on arity information.
                             -- See Note [Floating over-saturated applications]
                             -- in GHC.Core.Opt.SetLevels
  floatToTopLevelOnly :: Bool      -- ^ Allow floating to the top level only.
  }
instance Outputable FloatOutSwitches where
    ppr = pprFloatOutSwitches

pprFloatOutSwitches :: FloatOutSwitches -> SDoc
pprFloatOutSwitches sw
  = text "FOS" <+> (braces $
     sep $ punctuate comma $
     [ text "Lam ="    <+> ppr (floatOutLambdas sw)
     , text "Consts =" <+> ppr (floatOutConstants sw)
     , text "OverSatApps ="   <+> ppr (floatOutOverSatApps sw) ])

{-
************************************************************************
*                                                                      *
             Dealing with annotations
*                                                                      *
************************************************************************
-}

-- | Get all annotations of a given type. This happens lazily, that is
-- no deserialization will take place until the [a] is actually demanded and
-- the [a] can also be empty (the UniqFM is not filtered).
--
-- This should be done once at the start of a Core-to-Core pass that uses
-- annotations.
--
-- See Note [Annotations]
getAnnotationsFromHscEnv :: Typeable a => HscEnv -> ([Word8] -> a) -> ModGuts -> IO (ModuleEnv [a], NameEnv [a])
getAnnotationsFromHscEnv hsc_env deserialize guts = do
   ann_env <- prepareAnnotations hsc_env (Just guts)
   return (deserializeAnns deserialize ann_env)

-- | Get at most one annotation of a given type per annotatable item.
getFirstAnnotationsFromHscEnv :: Typeable a => HscEnv -> ([Word8] -> a) -> ModGuts -> IO (ModuleEnv a, NameEnv a)
getFirstAnnotationsFromHscEnv hsc_env deserialize guts
  = bimap mod name <$> getAnnotationsFromHscEnv hsc_env deserialize guts
  where
    mod = mapModuleEnv head . filterModuleEnv (const $ not . null)
    name = mapNameEnv head . filterNameEnv (not . null)

{-
Note [Annotations]
~~~~~~~~~~~~~~~~~~
A Core-to-Core pass that wants to make use of annotations calls
getAnnotationsFromHscEnv or getFirstAnnotationsFromHscEnv at the beginning to
obtain a UniqFM with annotations of a specific type. This produces all
annotations from interface files read so far. However, annotations from
interface files read during the pass will not be visible until
getAnnotationsFromHscEnv is called again. This is similar to how rules work and
probably isn't too bad.

The current implementation could be optimised a bit: when looking up
annotations for a thing from the HomePackageTable, we could search directly in
the module where the thing is defined rather than building one UniqFM which
contains all annotations we know of. This would work because annotations can
only be given to things defined in the same module. However, since we would
only want to deserialise every annotation once, we would have to build a cache
for every module in the HTP. In the end, it's probably not worth it as long as
we aren't using annotations heavily.
-}
