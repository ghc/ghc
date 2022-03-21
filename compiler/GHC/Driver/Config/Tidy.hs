{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE LambdaCase #-}

module GHC.Driver.Config.Tidy
  ( initTidyOpts
  , initStaticPtrOpts
  )
where

import GHC.Prelude

import GHC.Iface.Tidy
import GHC.Iface.Tidy.StaticPtrTable

import GHC.Driver.Session
import GHC.Driver.Env
import GHC.Driver.Backend

import GHC.Core.Make (getMkStringIds)
import GHC.Data.Maybe
import GHC.Utils.Panic
import GHC.Utils.Outputable
import GHC.Builtin.Names
import GHC.Tc.Utils.Env (lookupGlobal_maybe)
import GHC.Types.TyThing
import GHC.Platform.Ways

import qualified GHC.LanguageExtensions as LangExt

initTidyOpts :: HscEnv -> IO TidyOpts
initTidyOpts hsc_env = do
  let dflags = hsc_dflags hsc_env
  static_ptr_opts <- if not (xopt LangExt.StaticPointers dflags)
    then pure Nothing
    else Just <$> initStaticPtrOpts hsc_env
  pure $ TidyOpts
    { opt_name_cache        = hsc_NC hsc_env
    , opt_collect_ccs       = ways dflags `hasWay` WayProf
    , opt_unfolding_opts    = unfoldingOpts dflags
    , opt_expose_unfoldings = if | gopt Opt_OmitInterfacePragmas dflags -> ExposeNone
                                 | gopt Opt_ExposeAllUnfoldings dflags  -> ExposeAll
                                 | otherwise                            -> ExposeSome
    , opt_expose_rules      = not (gopt Opt_OmitInterfacePragmas dflags)
    , opt_trim_ids          = gopt Opt_OmitInterfacePragmas dflags
    , opt_static_ptr_opts   = static_ptr_opts
    }

initStaticPtrOpts :: HscEnv -> IO StaticPtrOpts
initStaticPtrOpts hsc_env = do
  let dflags = hsc_dflags hsc_env

  let lookupM n = lookupGlobal_maybe hsc_env n >>= \case
        Succeeded r -> pure r
        Failed err  -> pprPanic "initStaticPtrOpts: couldn't find" (ppr (err,n))

  mk_string <- getMkStringIds (fmap tyThingId . lookupM)
  static_ptr_info_datacon <- tyThingDataCon <$> lookupM staticPtrInfoDataConName
  static_ptr_datacon      <- tyThingDataCon <$> lookupM staticPtrDataConName

  pure $ StaticPtrOpts
    { opt_platform = targetPlatform dflags

      -- If we are compiling for the interpreter we will insert any necessary
      -- SPT entries dynamically, otherwise we add a C stub to do so
    , opt_gen_cstub = case backend dflags of
                        Interpreter -> False
                        _           -> True

    , opt_mk_string = mk_string
    , opt_static_ptr_info_datacon = static_ptr_info_datacon
    , opt_static_ptr_datacon      = static_ptr_datacon
    }

