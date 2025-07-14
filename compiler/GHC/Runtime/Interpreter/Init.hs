{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiWayIf #-}

module GHC.Runtime.Interpreter.Init
  ( initInterpreter
  , InterpOpts (..)
  )
where


import GHC.Prelude
import GHC.Platform
import GHC.Platform.Ways
import GHC.Settings
import GHC.Unit.Finder
import GHC.Unit.Env
import GHC.Utils.TmpFs
import GHC.SysTools.Tasks

import GHC.Linker.Executable
import qualified GHC.Linker.Loader as Loader
import GHC.Runtime.Interpreter
import GHC.Runtime.Interpreter.C
import GHC.StgToJS.Types (StgToJSConfig)

import GHC.Utils.Monad
import GHC.Utils.Outputable
import GHC.Utils.Logger
import GHC.Utils.Error
import Control.Concurrent
import System.Process

data InterpOpts = InterpOpts
  { interpExternal :: !Bool
  , interpProg :: String
  , interpOpts :: [String]
  , interpWays :: Ways
  , interpNameVer :: GhcNameVersion
  , interpLdConfig :: LdConfig
  , interpCcConfig :: CcConfig
  , interpJsInterp :: FilePath
  , interpTmpDir :: TempDir
  , interpFinderOpts :: FinderOpts
  , interpJsCodegenCfg :: StgToJSConfig
  , interpVerbosity :: Int
  , interpCreateProcess :: Maybe (CreateProcess -> IO ProcessHandle) -- create iserv process hook
  , interpWasmDyld :: FilePath
  , interpBrowser :: Bool
  , interpBrowserHost :: String
  , interpBrowserPort :: Int
  , interpBrowserRedirectWasiConsole :: Bool
  , interpBrowserPuppeteerLaunchOpts :: Maybe String
  , interpBrowserPlaywrightBrowserType :: Maybe String
  , interpBrowserPlaywrightLaunchOpts :: Maybe String
  , interpExecutableLinkOpts :: ExecutableLinkOpts
  }

-- | Initialize code interpreter
initInterpreter
  :: TmpFs
  -> Logger
  -> Platform
  -> FinderCache
  -> UnitEnv
  -> InterpOpts
  -> IO (Maybe Interp)
initInterpreter tmpfs logger platform finder_cache unit_env opts = do

  lookup_cache  <- liftIO $ mkInterpSymbolCache

  -- see Note [Target code interpreter]
  if
#if !defined(wasm32_HOST_ARCH)
    -- Wasm dynamic linker
    | ArchWasm32 <- platformArch platform
    -> do
        s <- liftIO $ newMVar InterpPending
        loader <- liftIO Loader.uninitializedLoader
        libdir <- liftIO $ last <$> Loader.getGccSearchDirectory logger (interpLdConfig opts) "libraries"
        let profiled = interpWays opts `hasWay` WayProf
            way_tag = if profiled then "_p" else ""
        let cfg =
              WasmInterpConfig
                { wasmInterpDyLD = interpWasmDyld opts
                , wasmInterpLibDir = libdir
                , wasmInterpOpts = interpOpts opts
                , wasmInterpBrowser = interpBrowser opts
                , wasmInterpBrowserHost = interpBrowserHost opts
                , wasmInterpBrowserPort = interpBrowserPort opts
                , wasmInterpBrowserRedirectWasiConsole = interpBrowserRedirectWasiConsole opts
                , wasmInterpBrowserPuppeteerLaunchOpts = interpBrowserPuppeteerLaunchOpts opts
                , wasmInterpBrowserPlaywrightBrowserType = interpBrowserPlaywrightBrowserType opts
                , wasmInterpBrowserPlaywrightLaunchOpts = interpBrowserPlaywrightLaunchOpts opts
                , wasmInterpTargetPlatform = platform
                , wasmInterpProfiled = profiled
                , wasmInterpHsSoSuffix = way_tag ++ dynLibSuffix (interpNameVer opts)
                , wasmInterpUnitState = ue_homeUnitState unit_env
                }
        pure $ Just $ Interp (ExternalInterp $ ExtWasm $ ExtInterpState cfg s) loader lookup_cache
#endif

    -- JavaScript interpreter
    | ArchJavaScript <- platformArch platform
    -> do
         s <- liftIO $ newMVar InterpPending
         loader <- liftIO Loader.uninitializedLoader
         let cfg = JSInterpConfig
              { jsInterpNodeConfig  = defaultNodeJsSettings
              , jsInterpScript      = interpJsInterp opts
              , jsInterpTmpFs       = tmpfs
              , jsInterpTmpDir      = interpTmpDir opts
              , jsInterpLogger      = logger
              , jsInterpCodegenCfg  = interpJsCodegenCfg opts
              , jsInterpUnitEnv     = unit_env
              , jsInterpFinderOpts  = interpFinderOpts opts
              , jsInterpFinderCache = finder_cache
              }
         return (Just (Interp (ExternalInterp (ExtJS (ExtInterpState cfg s))) loader lookup_cache))

    -- external interpreter
    | interpExternal opts
    -> do
        let
          profiled = interpWays opts `hasWay` WayProf
          dynamic  = interpWays opts `hasWay` WayDyn
        prog <- case interpProg opts of
          -- build iserv program if none specified
          "" -> generateIservC logger tmpfs (interpExecutableLinkOpts opts) unit_env
          _ -> pure (interpProg opts ++ flavour)
            where
              flavour
                | profiled && dynamic = "-prof-dyn"
                | profiled  = "-prof"
                | dynamic   = "-dyn"
                | otherwise = ""
        let msg = text "Starting " <> text prog
        tr <- if interpVerbosity opts >= 3
               then return (logInfo logger $ withPprStyle defaultDumpStyle msg)
               else return (pure ())
        let
         conf = IServConfig
           { iservConfProgram  = prog
           , iservConfOpts     = interpOpts opts
           , iservConfProfiled = profiled
           , iservConfDynamic  = dynamic
           , iservConfHook     = interpCreateProcess opts
           , iservConfTrace    = tr
           }
        s <- liftIO $ newMVar InterpPending
        loader <- liftIO Loader.uninitializedLoader
        return (Just (Interp (ExternalInterp (ExtIServ (ExtInterpState conf s))) loader lookup_cache))

    -- Internal interpreter
    | otherwise
    ->
#if defined(HAVE_INTERNAL_INTERPRETER)
     do
      loader <- liftIO Loader.uninitializedLoader
      return (Just (Interp InternalInterp loader lookup_cache))
#else
      return Nothing
#endif
