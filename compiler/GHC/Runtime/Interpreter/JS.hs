{-# LANGUAGE GADTs #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}

-- | JavaScript interpreter
--
-- See Note [The JS interpreter]
--
module GHC.Runtime.Interpreter.JS
  ( spawnJSInterp
  , jsLinkRts
  , jsLinkInterp
  , jsLinkObject
  , jsLinkObjects
  , jsLoadFile
  , jsRunServer
  -- * Reexported for convenience
  , mkExportedModFuns
  )
where

import GHC.Prelude
import GHC.Runtime.Interpreter.Types
import GHC.Runtime.Interpreter.Process
import GHC.Runtime.Utils
import GHCi.Message

import GHC.StgToJS.Linker.Types
import GHC.StgToJS.Linker.Linker
import GHC.StgToJS.Types
import GHC.StgToJS.Object

import GHC.Unit.Env
import GHC.Unit.Types
import GHC.Unit.State

import GHC.Utils.Logger
import GHC.Utils.TmpFs
import GHC.Utils.Panic
import GHC.Utils.Error (logInfo)
import GHC.Utils.Outputable (text)
import GHC.Data.FastString

import Control.Concurrent
import Control.Monad

import System.Process
import System.IO
import System.FilePath

import Data.IORef
import qualified Data.Set    as Set
import qualified Data.ByteString as B

import Foreign.C.String


-- Note [The JS interpreter]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- The JS interpreter works as follows:
--
-- ghc-interp.js is a simple JS script used to bootstrap the external
-- interpreter server (iserv) that is written in Haskell. This script waits for
-- commands on stdin:
--
--      LOAD foo.js
--
--        load a JS file in the current JS environment
--
--      RUN_SERVER ghci_unit_id
--
--        execute h$main(h$ghci_unit_idZCGHCiziServerzidefaultServer),
--        the entry point of the interpreter server
--
-- On the GHC side, when we need the interpreter we do the following:
--
-- 1. spawn nodejs with $topdir/ghc-interp.js script
-- 2. link the JS rts and send a LOAD command to load it
-- 3. link iserv (i.e. use GHCi.Server.defaultServer as root) and LOAD it
-- 4. send a RUN_SERVER command to execute the JS iserv
--
-- From this moment on, everything happens as with the native iserv, using a
-- pipe for communication, with the following differences:
--
--  - the JS iserv only supports the LoadObj linking command which has been
--  repurposed to load a JS source file. The JS iserv doesn't deal with
--  libraries (.a) and with object files (.o). The linker state is maintained on
--  the GHC side and GHC only sends the appropriate chunks of JS code to link.
--
--  - the JS iserv doesn't support ByteCode (i.e. it doesn't support CreateBCOs
--  messages). JS iserv clients should use the usual JS compilation pipeline and
--  send JS code instead. See GHC.Driver.Main.hscCompileCoreExpr for an example.
--
-- GHC keeps track of JS blocks (JS unit of linking corresponding to top-level
-- binding groups) that have already been linked by the JS interpreter. It only
-- links new ones when necessary.
--
-- Note that the JS interpreter isn't subject to staging issues: we can use it
-- in a Stage1 GHC.
--

---------------------------------------------------------
-- Running node
---------------------------------------------------------

-- | Start NodeJS interactively with "ghc-interp.js" script loaded in
startTHRunnerProcess :: FilePath -> NodeJsSettings -> IO (Handle,InterpProcess)
startTHRunnerProcess interp_js settings = do
  interp_in <- newIORef undefined

  let createProc cp = do
          let cp' = cp
                      { std_in  = CreatePipe
                      , std_out = Inherit
                      , std_err = Inherit
                      }
          (mb_in, _mb_out, _mb_err, hdl) <- createProcess cp'
          -- we can't directly return stdin for the process given the current
          -- implementation of runWithPipes. So we just use an IORef for this...
          case mb_in of
            Nothing -> panic "startTHRunnerProcess: expected stdin for interpreter"
            Just i  -> writeIORef interp_in i
          return hdl

  (hdl, rh, wh) <- runWithPipes createProc (nodeProgram settings)
                                           [interp_js]
                                           (nodeExtraArgs settings)
  std_in <- readIORef interp_in

  lo_ref <- newIORef Nothing
  let pipe = Pipe { pipeRead = rh, pipeWrite = wh, pipeLeftovers = lo_ref }
  let proc = InterpProcess
              { interpHandle = hdl
              , interpPipe   = pipe
              }
  pure (std_in, proc)

-- | Spawn a JS interpreter
--
-- Run NodeJS with "ghc-interp.js" loaded in. Then load GHCi.Server and its deps
-- (including the rts) and run GHCi.Server.defaultServer.
spawnJSInterp :: JSInterpConfig -> IO (ExtInterpInstance JSInterpExtra)
spawnJSInterp cfg = do
  let logger= jsInterpLogger cfg
  when (logVerbAtLeast logger 2) $
    logInfo logger (text "Spawning JS interpreter")

  let tmpfs        = jsInterpTmpFs cfg
      tmp_dir      = jsInterpTmpDir cfg
      logger       = jsInterpLogger cfg
      codegen_cfg  = jsInterpCodegenCfg cfg
      unit_env     = jsInterpUnitEnv cfg
      finder_opts  = jsInterpFinderOpts cfg
      finder_cache = jsInterpFinderCache cfg

  (std_in, proc) <- startTHRunnerProcess (jsInterpScript cfg) (jsInterpNodeConfig cfg)

  js_state <- newMVar (JSState
                { jsLinkState     = emptyLinkPlan
                , jsServerStarted = False
                })

  -- get the unit-id of the ghci package. We need this to load the
  -- interpreter code.
  ghci_unit_id <- case lookupPackageName (ue_units unit_env) (PackageName (fsLit "ghci")) of
    Nothing -> cmdLineErrorIO "JS interpreter: couldn't find \"ghci\" package"
    Just i  -> pure i

  let extra = JSInterpExtra
        { instStdIn        = std_in
        , instJSState      = js_state
        , instFinderCache  = finder_cache
        , instFinderOpts   = finder_opts
        , instGhciUnitId   = ghci_unit_id
        }

  pending_frees <- newMVar []
  let inst = ExtInterpInstance
        { instProcess           = proc
        , instPendingFrees      = pending_frees
        , instExtra             = extra
        }

  -- TODO: to support incremental linking of wasm modules (e.g. produced from C
  -- sources), we should:
  --
  -- 1. link the emcc rts without trimming dead code as we don't know what might
  -- be needed later by the Wasm modules we will dynamically load (cf
  -- -sMAIN_MODULE).
  -- 2. make the RUN_SERVER command wait for the emcc rts to be loaded.
  -- 3. link wasm modules with -sSIDE_MODULE
  -- 4. add a new command to load side modules with Emscripten's dlopen
  --
  -- cf https://emscripten.org/docs/compiling/Dynamic-Linking.html

  -- link rts and its deps
  jsLinkRts logger tmpfs tmp_dir codegen_cfg unit_env inst

  -- link interpreter and its deps
  jsLinkInterp logger tmpfs tmp_dir codegen_cfg unit_env inst

  -- run interpreter main loop
  jsRunServer inst

  pure inst



---------------------------------------------------------
-- Interpreter commands
---------------------------------------------------------

-- | Link JS RTS
jsLinkRts :: Logger -> TmpFs -> TempDir -> StgToJSConfig -> UnitEnv -> ExtInterpInstance JSInterpExtra -> IO ()
jsLinkRts logger tmpfs tmp_dir cfg unit_env inst = do
  let link_cfg = JSLinkConfig
        { lcNoStats         = True  -- we don't need the stats
        , lcNoRts           = False -- we need the RTS
        , lcCombineAll      = False -- we don't need the combined all.js, we'll link each part independently below
        , lcForeignRefs     = False -- we don't need foreign references
        , lcNoJSExecutables = True  -- we don't need executables
        , lcNoHsMain        = True  -- nor HsMain
        , lcForceEmccRts    = False -- nor the emcc rts
        , lcLinkCsources    = False -- we know that there are no C sources to load for the RTS
        }

  -- link the RTS and its dependencies (things it uses from `base`, etc.)
  let link_spec = LinkSpec
        { lks_unit_ids        = [rtsUnitId, ghcInternalUnitId, primUnitId]
        , lks_obj_root_filter = const False
        , lks_extra_roots     = mempty
        , lks_objs_hs         = mempty
        , lks_objs_js         = mempty
        , lks_objs_cc         = mempty
        }

  let finder_opts  = instFinderOpts (instExtra inst)
      finder_cache = instFinderCache (instExtra inst)

  ar_cache <- newArchiveCache
  link_plan <- computeLinkDependencies cfg unit_env link_spec finder_opts finder_cache ar_cache
  jsLinkPlan logger tmpfs tmp_dir ar_cache link_cfg cfg inst link_plan

-- | Link JS interpreter
jsLinkInterp :: Logger -> TmpFs -> TempDir -> StgToJSConfig -> UnitEnv -> ExtInterpInstance JSInterpExtra -> IO ()
jsLinkInterp logger tmpfs tmp_dir cfg unit_env inst = do

  let link_cfg = JSLinkConfig
        { lcNoStats         = True  -- we don't need the stats
        , lcNoRts           = True  -- we don't need the RTS
        , lcCombineAll      = False -- we don't need the combined all.js, we'll link each part independently below
        , lcForeignRefs     = False -- we don't need foreign references
        , lcNoJSExecutables = True  -- we don't need executables
        , lcNoHsMain        = True  -- nor HsMain
        , lcForceEmccRts    = False -- nor the emcc rts
        , lcLinkCsources    = True  -- enable C sources, if any
        }

  let is_root _ = True -- FIXME: we shouldn't consider every function as a root

  let ghci_unit_id = instGhciUnitId (instExtra inst)

  -- compute unit dependencies of ghc_unit_id
  let unit_map = unitInfoMap (ue_units unit_env)
  dep_units <- mayThrowUnitErr $ closeUnitDeps unit_map [(ghci_unit_id,Nothing)]
  let units = dep_units ++ [ghci_unit_id]

  -- indicate that our root function is GHCi.Server.defaultServer
  let root_deps = Set.fromList $ mkExportedFuns ghci_unit_id (fsLit "GHCi.Server") [fsLit "defaultServer"]

  -- link the interpreter and its dependencies
  let link_spec = LinkSpec
        { lks_unit_ids        = units
        , lks_obj_root_filter = is_root
        , lks_extra_roots     = root_deps
        , lks_objs_hs         = mempty
        , lks_objs_js         = mempty
        , lks_objs_cc         = mempty
        }

  let finder_cache = instFinderCache (instExtra inst)
      finder_opts  = instFinderOpts (instExtra inst)

  ar_cache <- newArchiveCache
  link_plan <- computeLinkDependencies cfg unit_env link_spec finder_opts finder_cache ar_cache
  jsLinkPlan logger tmpfs tmp_dir ar_cache link_cfg cfg inst link_plan


-- | Link object files
jsLinkObjects :: Logger -> TmpFs -> TempDir -> StgToJSConfig -> UnitEnv -> ExtInterpInstance JSInterpExtra -> [FilePath] -> (ExportedFun -> Bool) -> IO ()
jsLinkObjects logger tmpfs tmp_dir cfg unit_env inst objs is_root = do
  let link_cfg = JSLinkConfig
        { lcNoStats         = True  -- we don't need the stats
        , lcNoRts           = True  -- we don't need the RTS (already linked)
        , lcCombineAll      = False -- we don't need the combined all.js, we'll link each part independently below
        , lcForeignRefs     = False -- we don't need foreign references
        , lcNoJSExecutables = True  -- we don't need executables
        , lcNoHsMain        = True  -- nor HsMain
        , lcForceEmccRts    = False -- nor the emcc rts
        , lcLinkCsources    = True  -- enable C sources, if any
        }

  let units = preloadUnits (ue_units unit_env)

  -- compute dependencies
  let link_spec = LinkSpec
        { lks_unit_ids        = units
        , lks_obj_root_filter = is_root
        , lks_extra_roots     = mempty
        , lks_objs_hs         = objs
        , lks_objs_js         = mempty
        , lks_objs_cc         = mempty
        }

  let finder_opts  = instFinderOpts (instExtra inst)
      finder_cache = instFinderCache (instExtra inst)

  ar_cache <- newArchiveCache
  link_plan <- computeLinkDependencies cfg unit_env link_spec finder_opts finder_cache ar_cache
  jsLinkPlan logger tmpfs tmp_dir ar_cache link_cfg cfg inst link_plan



-- | Link an object file using the given functions as roots
jsLinkObject :: Logger -> TmpFs -> TempDir -> StgToJSConfig -> UnitEnv -> ExtInterpInstance JSInterpExtra -> FilePath -> [ExportedFun] -> IO ()
jsLinkObject logger tmpfs tmp_dir cfg unit_env inst obj roots = do
  let is_root f = Set.member f (Set.fromList roots)
  let objs      = [obj]
  jsLinkObjects logger tmpfs tmp_dir cfg unit_env inst objs is_root


-- | Link the given link plan
--
-- Perform incremental linking by removing what is already linked from the plan
jsLinkPlan :: Logger -> TmpFs -> TempDir -> ArchiveCache -> JSLinkConfig -> StgToJSConfig -> ExtInterpInstance JSInterpExtra -> LinkPlan -> IO ()
jsLinkPlan logger tmpfs tmp_dir ar_cache link_cfg cfg inst link_plan = do
  ----------------------------------------------------------------
  -- Get already linked stuff and compute incremental plan
  ----------------------------------------------------------------

  old_plan <- jsLinkState <$> readMVar (instJSState (instExtra inst))

  -- compute new plan discarding what's already linked
  let (diff_plan, total_plan) = incrementLinkPlan old_plan link_plan

  ----------------------------------------------------------------
  -- Generate JS code for the incremental plan
  ----------------------------------------------------------------

  tmp_out <- newTempSubDir logger tmpfs tmp_dir
  void $ jsLink link_cfg cfg logger tmpfs ar_cache tmp_out diff_plan

  -- Code has been linked into the following files:
  --  - generated rts from tmp_out/rts.js (depends on link options)
  --  - raw js files from tmp_out/lib.js
  --  - Haskell generated JS from tmp_out/out.js

  -- We need to combine at least rts.js and lib.js for the RTS because they
  -- depend on each other. We might as well combine them all, so that's what we
  -- do.
  let filenames
        | lcNoRts link_cfg = ["lib.js", "out.js"]
        | otherwise        = ["rts.js", "lib.js", "out.js"]
  let files = map (tmp_out </>) filenames
  let all_js = tmp_out </> "all.js"
  let all_files = all_js : files
  withBinaryFile all_js WriteMode $ \h -> do
    let cpy i = B.readFile i >>= B.hPut h
    mapM_ cpy files

  -- add files to clean
  addFilesToClean tmpfs TFL_CurrentModule all_files

  ----------------------------------------------------------------
  -- Link JS code
  ----------------------------------------------------------------

  -- linking JS code depends on the phase we're in:
  -- - during in the initialization phase, we send a LoadFile message to the
  --   JS server;
  -- - once the Haskell server is started, we send a LoadObj message to the
  --   Haskell server.
  server_started <- jsServerStarted <$> readMVar (instJSState (instExtra inst))
  if server_started
    then sendMessageNoResponse inst $ LoadObj all_js
    else jsLoadFile            inst all_js

  ----------------------------------------------------------------
  -- update linker state
  ----------------------------------------------------------------
  modifyMVar_ (instJSState (instExtra inst)) $ \state -> pure state { jsLinkState = total_plan }


-- | Send a command to the JS interpreter
jsSendCommand :: ExtInterpInstance JSInterpExtra -> String -> IO ()
jsSendCommand inst cmd = send_cmd cmd
  where
    extra      = instExtra inst
    handle     = instStdIn extra
    send_cmd s = do
      withCStringLen s \(p,n) -> hPutBuf handle p n
      hFlush handle

-- | Load a JS file in the interpreter
jsLoadFile :: ExtInterpInstance JSInterpExtra -> FilePath -> IO ()
jsLoadFile inst path = jsSendCommand inst ("LOAD " ++ path ++ "\n")

-- | Run JS server
jsRunServer :: ExtInterpInstance JSInterpExtra -> IO ()
jsRunServer inst = do
  let ghci_unit_id = instGhciUnitId (instExtra inst)
  let zghci_unit_id = zString (zEncodeFS (unitIdFS ghci_unit_id))

  -- Run `GHCi.Server.defaultServer`
  jsSendCommand inst ("RUN_SERVER " ++ zghci_unit_id ++ "\n")

  -- indicate that the Haskell server is now started
  modifyMVar_ (instJSState (instExtra inst)) $ \state -> pure state { jsServerStarted = True }
