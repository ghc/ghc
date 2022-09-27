module GHC.Driver.Core.Opt ( optimizeCoreHsc, optimizeCoreIO ) where

import GHC.Prelude

import GHC.Driver.Plugins
import GHC.Driver.Session
import GHC.Driver.Env
import GHC.Driver.Core.Opt.Annotations ( getFirstAnnotationsFromHscEnv )
import GHC.Driver.Config.Core.Lint ( initLintAnnotationsConfig )
import GHC.Driver.Config.Core.EndPass ( initEndPassConfig )
import GHC.Driver.Config.Core.Opt ( getCoreToDo )

import GHC.Serialized   ( deserializeWithData )

import GHC.Runtime.Loader      ( initializePlugins )

import GHC.Plugins.Monad ( CoreM, runCoreM )

import GHC.Core.Lint ( DebugSetting(..) )
import GHC.Core.Lint.Interactive ( interactiveInScope )
import GHC.Core.Rules ( mkRuleBase )
import GHC.Core.Opt ( CoreOptEnv (..), runCorePasses )
import GHC.Core.Opt.Simplify.Monad ( simplMask )
import GHC.Core.Opt.Stats ( SimplCountM, addCounts, runSimplCountM, pprSimplCount )

import GHC.Unit
import GHC.Unit.Module.ModGuts
import GHC.Unit.Module.Deps

import GHC.Types.Name.Ppr

import GHC.Utils.Logger as Logger

import Control.Monad.IO.Class

--------------------------------------------------------------
-- Core optimization entrypoints
--------------------------------------------------------------

-- | Run Core optimizer. The list of String is a list of (Core) plugin
-- module names added via TH (cf 'addCorePlugin').
optimizeCoreIO :: HscEnv -> [String] -> ModGuts -> IO ModGuts
optimizeCoreIO hsc_env plugins guts = do
    hsc_env_with_plugins <- if null plugins -- fast path
        then return hsc_env
        else initializePlugins
             $ hscUpdateFlags (\dflags -> foldr addPluginModuleName dflags plugins)
             hsc_env
    {-# SCC "Core2Core" #-}
      core2core hsc_env_with_plugins guts

-- | Run Core optimizer. The list of String is a list of (Core) plugin
-- module names added via TH (cf 'addCorePlugin').
optimizeCoreHsc :: [String] -> ModGuts -> Hsc ModGuts
optimizeCoreHsc plugins guts = do
    hsc_env <- getHscEnv
    liftIO $ optimizeCoreIO hsc_env plugins guts

core2core :: HscEnv -> ModGuts -> IO ModGuts
core2core hsc_env guts@(ModGuts { mg_module  = mod
                                , mg_loc     = loc
                                , mg_deps    = deps
                                , mg_rdr_env = rdr_env })
  = do { let builtin_passes = getCoreToDo dflags extra_vars

       ; (guts2, stats) <- runSimplCountM dump_simpl_stats $ do
           all_passes <- liftCoreMToSimplCountM hsc_env InheritDebugLevel guts $
             withPlugins (hsc_plugins hsc_env) installCoreToDos builtin_passes
           runCorePasses env all_passes guts

       ; Logger.putDumpFileMaybe logger Opt_D_dump_simpl_stats
             "Grand total simplifier statistics"
             FormatText
             (pprSimplCount stats)

       ; return guts2 }
  where
    dflags           = hsc_dflags hsc_env
    logger           = hsc_logger hsc_env
    dump_simpl_stats = logHasDumpFlag logger Opt_D_dump_simpl_stats
    print_unqual     = mkPrintUnqualified (hsc_unit_env hsc_env) rdr_env
    extra_vars       = interactiveInScope (hsc_IC hsc_env)
    home_pkg_rules   = hptRules hsc_env (moduleUnitId mod) (GWIB { gwib_mod = moduleName mod
                                                                 , gwib_isBoot = NotBoot })
    hpt_rule_base  = mkRuleBase home_pkg_rules

    env = CoreOptEnv
      { co_logger        = logger
      , co_debugSetting  = InheritDebugLevel
      , co_unitEnv       = hsc_unit_env hsc_env
      , co_liftCoreM     = liftCoreMToSimplCountM hsc_env
      , co_hptRuleBase   = hpt_rule_base
      , co_printUnqual   = print_unqual
      , co_visOrphans    = mkModuleSet (mod : dep_orphs deps)
      , co_getEps        = hscEPS hsc_env
      , co_extraVars     = interactiveInScope $ hsc_IC hsc_env
      , co_specConstrAnn = fmap snd . getFirstAnnotationsFromHscEnv hsc_env deserializeWithData
      , co_endPassCfg    = initEndPassConfig dflags extra_vars print_unqual
      , co_lintAnnotationsCfg = initLintAnnotationsConfig dflags loc print_unqual
      }

liftCoreMToSimplCountM :: HscEnv -> DebugSetting -> ModGuts -> CoreM a -> SimplCountM a
liftCoreMToSimplCountM hsc_env debug_settings guts m = do
  (a, sc) <- liftIO $ runCoreM hsc_env' hpt_rule_base simplMask mod orph_mods print_unqual loc m
  addCounts sc
  return a
  where
    mod = mg_module guts
    -- mod: get the module out of the ModGuts so we can retrieve it from the monad.
    -- This is very convienent for the users of the monad (e.g. plugins do not have to
    -- consume the ModGuts to find the module) but somewhat ugly because mg_module may
    -- _theoretically_ be changed during the Core pipeline (it's part of ModGuts), which
    -- would mean our cached value would go out of date.
    loc = mg_loc guts
    orph_mods = mkModuleSet (mod : dep_orphs (mg_deps guts))
    gwib = GWIB { gwib_mod = moduleName mod, gwib_isBoot = NotBoot }
    hpt_rule_base = mkRuleBase (hptRules hsc_env (moduleUnitId mod) gwib)
    print_unqual = mkPrintUnqualified (hsc_unit_env hsc_env) (mg_rdr_env guts)

    hsc_env' = case debug_settings of
      InheritDebugLevel -> hsc_env
      NoDebugging -> let
        dflags' = (hsc_dflags hsc_env) { debugLevel = 0 }
        in hsc_env { hsc_dflags = dflags' }

{-
Note [The architecture of the Core optimizer]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Conceptually the Core optimizer consists of two stages:

 1. **The planning stage**: where we take the user-specified input (currently in
    `DynFlags`) and produce a domain-specific configuration for just this
    pipeline stage (a `[CoreToDo]` value). This configuration is the plan.
 2. **The execution stage**: where we we take a Core program (a `ModGuts`) and
    the configuration (a `[CoreToDo]`) and optimize that program according to
    the `[CoreToDo]` plan, producing a new `ModGuts`.

This division is mirrored in the interface of the different optimizations. For
each of those optimzations we have

 1. a configuration record bundeling the options for a particular optimization
    pass.
 2. an initialization function used to obtain such a configuration from
    `DynFlags`.
 3. the actual optimization pass itself, with an entrypoint that takes the
    configuration of the pass along with the execution context as arguments.
    This entrypoint is called in the execution stage.

The plan that is the result of the first stage is constructed by the
`getCoreToDo` function found in the `GHC.Driver.Config.Core.Opt` module. This
function determines the sequence of optimization passes run on the module in
question and derives the configuration for each pass from the session's state
(`DynFlags`) using the aforementioned initialization functions that live in
`GHC.Driver.Config.Core.Opt` as well. The `CoreToDo` type that is finally used
to wrap this configuration value is a sum type enumerating all the optimizations
available in GHC.
As an example suppose we have a `DynFlags` value "dflags" with
  gopt Opt_FullLaziness dflags == True
  floatLamArgs dflags == Just 42

If we pass that dflags value to the `getCoreToDo` planning function we receive a
list like
  [ ...
  , CoreDoFloatOutwards FloatOutSwitches
    { floatOutLambdas   = Just 0
    , floatOutConstants = True
    , floatOutOverSatApps = False
    , floatToTopLevelOnly = False
    }
  , ...
  , CoreDoFloatOutwards FloatOutSwitches
    { floatOutLambdas     = Just 42
    , floatOutConstants   = True
    , floatOutOverSatApps = True
    , floatToTopLevelOnly = False
    }
  , ...
  ]

This plan contains two "instructions" to perform two FloatOut optimization
passes with the given configuration. Note that these configurations draw their
values from dflags and the planning function `getCoreToDo` alone and are
independent of the modules they might be applied to.

The entrypoint of the second stage is the `runCorePasses` function found in
GHC.Core.Opt. It is essentially an interpreter for the `CoreToDo`s constructed
in the planning stage. It calls the entrypoints of the passes with their
respective configurations as arguments as well as some execution context like
the unit environment, the rules and the type family instance in scope, and most
notably the module we wish to compile (`ModGuts`).
Feeding the list with the two FloatOut configurations above to that function
would result in two calls of
  ...
  floatOutwards logger
    (FloatOutSwitches { floatOutLambdas = Just 0, ... }) us binds
  ...
  floatOutwards logger
    (FloatOutSwitches { floatOutLambdas = Just 42, ... }) us binds'
  ...
where the Logger "logger" and the unique supply "us" are supplied by the
execution context of the interpreter and binds and binds' are the Core bindings
of the `ModGuts` being optimized.

The Core Optimizer itself is invoked by calling one of the `optimizeCore*`
functions found in GHC.Driver.Core.Opt. These functions is part of the
Application Layer and utilize both the `getCoreToDo` function -- which is part
of the Application Layer as well -- and the `runCorePasses` function which is
part of the Domain Layer. Here, the terms "Application Layer" and "Domain Layer"
are terms borrowed from Domain Driven Design and its application in the context
of GHC is layed out in the "Modularizing GHC" paper
(https://hsyl20.fr/home/posts/2022-05-03-modularizing-ghc-paper.html).
In other words, while the `optimizeCore*` and `getCoreToDo` know about `HscEnv`
and `DynFlags` and are therefore bound to a concrete driver, `runCorePasses` is
more independent as it is a component of its own. As such it could be used by a
different driver that produces a plan in a completely different way.

A similar split in functionality is done for the Core Linting: After each pass
we may check the sanity of the resulting Core running a so-called EndPass check.
The entrypoint for this check is the `endPass` function found in
GHC.Core.EndPass. It comes as well with a configuration record and a
corresponding initialization function for it in GHC.Driver.Core.EndPass. The
definition of what actually is a correct Core program is defined by the linting
functions in GHC.Core.Lint. These are used by the EndPass to check the program.

-}
