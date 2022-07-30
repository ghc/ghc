module GHC.Driver.Core.Opt ( hscSimplify, hscSimplify' ) where

import GHC.Prelude

import GHC.Driver.Plugins
import GHC.Driver.Session
import GHC.Driver.Env
import GHC.Driver.Core.Rules ( readRuleEnv )
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
-- Simplifiers
--------------------------------------------------------------

-- | Run Core2Core simplifier. The list of String is a list of (Core) plugin
-- module names added via TH (cf 'addCorePlugin').
hscSimplify :: HscEnv -> [String] -> ModGuts -> IO ModGuts
hscSimplify hsc_env plugins modguts =
    runHsc hsc_env $ hscSimplify' plugins modguts

-- | Run Core2Core simplifier. The list of String is a list of (Core) plugin
-- module names added via TH (cf 'addCorePlugin').
hscSimplify' :: [String] -> ModGuts -> Hsc ModGuts
hscSimplify' plugins ds_result = do
    hsc_env <- getHscEnv
    hsc_env_with_plugins <- if null plugins -- fast path
        then return hsc_env
        else liftIO $ initializePlugins
                    $ hscUpdateFlags (\dflags -> foldr addPluginModuleName dflags plugins)
                      hsc_env
    {-# SCC "Core2Core" #-}
      liftIO $ core2core hsc_env_with_plugins ds_result

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

    -- mod: get the module out of the current HscEnv so we can retrieve it from the monad.
    -- This is very convienent for the users of the monad (e.g. plugins do not have to
    -- consume the ModGuts to find the module) but somewhat ugly because mg_module may
    -- _theoretically_ be changed during the Core pipeline (it's part of ModGuts), which
    -- would mean our cached value would go out of date.
    env = CoreOptEnv
      { co_logger        = logger
      , co_debugSetting  = InheritDebugLevel
      , co_unitEnv       = hsc_unit_env hsc_env
      , co_liftCoreM     = liftCoreMToSimplCountM hsc_env
      , co_getAllRules   = readRuleEnv hsc_env
      , co_hptRuleBase   = hpt_rule_base
      , co_printUnqual   = print_unqual
      , co_visOrphans    = mkModuleSet (mod : dep_orphs deps)
      , co_hasPprDebug   = hasPprDebug dflags
      , co_getEps        = hscEPS hsc_env
      , co_extraVars     = interactiveInScope $ hsc_IC hsc_env
      , co_specConstrAnn = fmap snd . getFirstAnnotationsFromHscEnv hsc_env deserializeWithData
      , co_endPassCfg    = \pass -> initEndPassConfig dflags extra_vars print_unqual pass
      , co_lintAnnotationsCfg = \pass -> initLintAnnotationsConfig dflags loc print_unqual pass
      }

liftCoreMToSimplCountM :: HscEnv -> DebugSetting -> ModGuts -> CoreM a -> SimplCountM a
liftCoreMToSimplCountM hsc_env debug_settings guts m = do
  (a, sc) <- liftIO $ runCoreM hsc_env' hpt_rule_base simplMask mod orph_mods print_unqual loc m
  addCounts sc
  return a
  where
    mod = mg_module guts
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
