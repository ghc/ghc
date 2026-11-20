{-# LANGUAGE MultiWayIf #-}

module GHC.Driver.Config.Tidy
  ( initTidyOpts
  , checkImportedPackageRule
  , initStaticPtrOpts
  )
where

import GHC.Prelude

import GHC.Iface.Tidy
import GHC.Iface.Tidy.StaticPtrTable

import GHC.Driver.DynFlags
import GHC.Driver.Env
import GHC.Driver.Backend

import GHC.Core (CoreProgram, CoreRule(..), bindersOfBinds)
import GHC.Core.Dependent (supportsImportedErasedRule)
import GHC.Core.Lint (lintRule)
import GHC.Driver.Config.Core.Lint (initLintConfig)
import GHC.Core.Make (getMkStringIds)
import GHC.Builtin.KnownKeys
import GHC.Builtin.Modules (usesEssentialsModule)
import GHC.Data.Maybe (MaybeErr(..), isNothing)
import GHC.Iface.Load (KnownEntitySource(..), loadKnownKeyOccMaps)
import GHC.Tc.Utils.Env (lookupKnownKeyGlobal)
import GHC.Tc.Utils.Monad (initIfaceLoad)
import GHC.Types.Error (pprDiagnostic)
import GHC.Types.Name (nameModule_maybe)
import GHC.Types.Name.Env (emptyNameEnv)
import GHC.Types.TyThing
import GHC.Types.TypeEnv (typeEnvFromEntities)
import GHC.Unit.Module (Module, moduleName)
import GHC.Unit.Module.ModGuts
import GHC.Utils.Panic (throwGhcExceptionIO, GhcException(..))
import GHC.Platform.Ways

import qualified GHC.LanguageExtensions as LangExt

initTidyOpts :: Bool -> HscEnv -> ModGuts -> IO TidyOpts
initTidyOpts native hsc_env guts = do
  let dflags = hsc_dflags hsc_env
  static_ptr_opts <- if not (xopt LangExt.StaticPointers dflags)
    then pure Nothing
    else Just <$> initStaticPtrOpts hsc_env guts
  pure $ TidyOpts
    { opt_name_cache        = hsc_NC hsc_env
    , opt_collect_ccs       = ways dflags `hasWay` WayProf
    , opt_unfolding_opts    = unfoldingOpts dflags
    , opt_expose_unfoldings = if | gopt Opt_OmitInterfacePragmas dflags -> ExposeNone
                                 | gopt Opt_ExposeAllUnfoldings dflags  -> ExposeAll
                                 | gopt Opt_ExposeOverloadedUnfoldings dflags  -> ExposeOverloaded
                                 | otherwise                            -> ExposeSome
    , opt_expose_rules      = not (gopt Opt_OmitInterfacePragmas dflags)
    , opt_trim_ids          = gopt Opt_OmitInterfacePragmas dflags
    , opt_static_ptr_opts   = static_ptr_opts
    , opt_has_retained_core = native
    , opt_check_imported_rule = checkImportedPackageRule hsc_env (mg_module guts) (mg_binds guts)
    , opt_keep_auto_rules   = gopt Opt_KeepAutoRules dflags
    }

-- | Resolve an imported rule head in the loaded type environment and lint both sides.
-- See Note [Imported heads of erased package rules] in GHC.Core.Dependent.
checkImportedPackageRule :: HscEnv -> Module -> CoreProgram -> CoreRule -> IO Bool
checkImportedPackageRule hsc_env owner binds rule@Rule{ru_fn = name, ru_origin = origin}
  | origin == owner, Just head_mod <- nameModule_maybe name, head_mod /= owner = do
      thing <- lookupType hsc_env name
      pure $ case thing of
        Just (AnId fun) -> supportsImportedErasedRule fun rule &&
          isNothing (lintRule (initLintConfig (hsc_dflags hsc_env) (bindersOfBinds binds)) fun rule)
        _ -> False
checkImportedPackageRule _ _ _ _ = pure False

initStaticPtrOpts :: HscEnv -> ModGuts -> IO StaticPtrOpts
initStaticPtrOpts hsc_env guts = do
  let dflags = hsc_dflags hsc_env
      this_mod = mg_module guts

  kk_source <-
    if usesEssentialsModule (gopt Opt_RebindableKnownNames dflags)
                            (moduleName this_mod)
    then do
      mb_maps <- initIfaceLoad hsc_env loadKnownKeyOccMaps
      case mb_maps of
        Succeeded kk_maps -> pure (KES_FromModule kk_maps)
        Failed err        -> throwGhcExceptionIO $
          PprProgramError "Could not load GHC.Essentials for the static-pointer table"
            (pprDiagnostic err)
    else
      pure $ KES_InScope { ke_mod          = this_mod
                         , ke_rdr_env      = mg_rdr_env guts
                         , ke_gbl_type_env = typeEnvFromEntities (bindersOfBinds (mg_binds guts))
                                               (mg_tcs guts) (mg_patsyns guts) (mg_fam_insts guts)
                         , ke_lcl_type_env = emptyNameEnv }

  let lookupKnownKey = lookupKnownKeyGlobal hsc_env kk_source

  mk_string <- getMkStringIds (fmap tyThingId . lookupKnownKey)
  static_ptr_info_datacon <- tyThingDataCon <$> lookupKnownKey staticPtrInfoDataConKey
  static_ptr_datacon      <- tyThingDataCon <$> lookupKnownKey staticPtrDataConKey

  pure $ StaticPtrOpts
    { opt_platform = targetPlatform dflags

      -- If we are compiling for the interpreter we will insert any necessary
      -- SPT entries dynamically, otherwise we add a C stub to do so
    , opt_gen_cstub = backendWritesFiles (backend dflags)
    , opt_mk_string = mk_string
    , opt_static_ptr_info_datacon = static_ptr_info_datacon
    , opt_static_ptr_datacon      = static_ptr_datacon
    }
