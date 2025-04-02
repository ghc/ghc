{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NondecreasingIndentation #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS -fno-warn-incomplete-patterns -optc-DNON_POSIX_SOURCE #-}

-----------------------------------------------------------------------------
--
-- GHC Driver program
--
-- (c) The University of Glasgow 2005
--
-----------------------------------------------------------------------------

module Main (main) where

-- The official GHC API
import qualified GHC
import GHC              (parseTargetFiles,  Ghc, GhcMonad(..),
                          LoadHowMuch(..) )

import GHC.Driver.Backend
import GHC.Driver.CmdLine
import GHC.Driver.Env
import GHC.Driver.Errors
import GHC.Driver.Errors.Types
import GHC.Driver.Phases
import GHC.Driver.Session
import GHC.Driver.Ppr
import GHC.Driver.Pipeline  ( oneShot )
import GHC.Driver.MakeFile  ( doMkDependHS )
import GHC.Driver.Backpack  ( doBackpack )
import GHC.Driver.Plugins
import GHC.Driver.Config.Logger (initLogFlags)
import GHC.Driver.Config.Diagnostic

import GHC.Platform
import GHC.Platform.Host

#if defined(HAVE_INTERNAL_INTERPRETER)
import GHCi.UI              ( interactiveUI, ghciWelcomeMsg, defaultGhciSettings )
#endif

import GHC.Runtime.Loader   ( loadFrontendPlugin, initializeSessionPlugins )

import GHC.Unit.Module ( ModuleName, mkModuleName )
import GHC.Unit.Module.ModIface
import GHC.Unit.State  ( pprUnits, pprUnitsSimple )
import GHC.Unit.Finder ( findImportedModule, FindResult(..) )
import GHC.Unit.Types  ( IsBootInterface(..) )

import GHC.Types.Basic     ( failed )
import GHC.Types.SrcLoc
import GHC.Types.SourceError
import GHC.Types.Unique.Supply
import GHC.Types.PkgQual

import GHC.Utils.Error
import GHC.Utils.Panic
import GHC.Utils.Outputable as Outputable
import GHC.Utils.Monad       ( liftIO )
import GHC.Utils.Binary        ( openBinMem, put_ )
import GHC.Utils.Logger

import GHC.Settings.Config
import GHC.Settings.Constants
import GHC.Settings.IO

import GHC.HandleEncoding
import GHC.Data.FastString
import GHC.Data.Maybe
import GHC.SysTools.BaseDir

import GHC.Iface.Load
import GHC.Iface.Recomp.Binary ( fingerprintBinMem )

import GHC.Tc.Utils.Monad      ( initIfaceCheck )
import GHC.Iface.Errors.Ppr

import GHC.Driver.Session.Mode
import GHC.Driver.Session.Lint
import GHC.Driver.Session.Units

-- Standard Haskell libraries
import System.IO
import System.Environment
import System.Exit
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except (throwE, runExceptT)
import Data.List ( isPrefixOf, partition, intercalate )
import Prelude
import qualified Data.List.NonEmpty as NE

-----------------------------------------------------------------------------
-- ToDo:

-- time commands when run with -v
-- user ways
-- Win32 support: proper signal handling
-- reading the package configuration file is too slow
-- -K<size>

-----------------------------------------------------------------------------
-- GHC's command-line interface

main :: IO ()
main = do
   hSetBuffering stdout LineBuffering
   hSetBuffering stderr LineBuffering

   configureHandleEncoding
   GHC.defaultErrorHandler defaultFatalMessager defaultFlushOut $ do
    -- 1. extract the -B flag from the args
    argv0 <- getArgs

    let (minusB_args, argv1) = partition ("-B" `isPrefixOf`) argv0
        mbMinusB | null minusB_args = Nothing
                 | otherwise = Just (drop 2 (last minusB_args))

    let argv2 = map (mkGeneralLocated "on the commandline") argv1

    -- 2. Parse the "mode" flags (--make, --interactive etc.)
    (mode, units, argv3, flagWarnings) <- parseModeFlags argv2

    -- If all we want to do is something like showing the version number
    -- then do it now, before we start a GHC session etc. This makes
    -- getting basic information much more resilient.

    -- In particular, if we wait until later before giving the version
    -- number then bootstrapping gets confused, as it tries to find out
    -- what version of GHC it's using before package.conf exists, so
    -- starting the session fails.
    case mode of
        Left preStartupMode ->
            do case preStartupMode of
                   ShowSupportedExtensions   -> showSupportedExtensions mbMinusB
                   ShowVersion               -> showVersion
                   ShowNumVersion            -> putStrLn cProjectVersion
                   ShowOptions isInteractive -> showOptions isInteractive
        Right postStartupMode ->
            -- start our GHC session
            GHC.runGhc mbMinusB $ do

            dflags <- GHC.getSessionDynFlags

            case postStartupMode of
                Left preLoadMode ->
                    liftIO $ do
                        case preLoadMode of
                            ShowInfo               -> showInfo dflags
                            ShowGhcUsage           -> showGhcUsage  dflags
                            ShowGhciUsage          -> showGhciUsage dflags
                            PrintWithDynFlags f    -> putStrLn (f dflags)
                Right postLoadMode ->
                    main' postLoadMode units dflags argv3 flagWarnings

main' :: PostLoadMode -> [String] -> DynFlags -> [Located String] -> [Warn]
      -> Ghc ()
main' postLoadMode units dflags0 args flagWarnings = do
  let args' = case postLoadMode of
                DoRun -> takeWhile (\arg -> unLoc arg /= "--") args
                _     -> args

  -- set the default GhcMode, backend and GhcLink.  The backend
  -- can be further adjusted on a module by module basis, using only
  -- the -fllvm and -fasm flags.  If the default backend is not
  -- LLVM or NCG, -fllvm and -fasm have no effect.
  let dflt_backend = backend dflags0
      (mode, bcknd, link)
         = case postLoadMode of
               DoInteractive   -> (CompManager, interpreterBackend,  LinkInMemory)
               DoEval _        -> (CompManager, interpreterBackend,  LinkInMemory)
               DoRun           -> (CompManager, interpreterBackend,  LinkInMemory)
               DoMake          -> (CompManager, dflt_backend, LinkBinary)
               DoBackpack      -> (CompManager, dflt_backend, LinkBinary)
               DoMkDependHS    -> (MkDepend,    dflt_backend, LinkBinary)
               DoAbiHash       -> (OneShot,     dflt_backend, LinkBinary)
               _               -> (OneShot,     dflt_backend, LinkBinary)

  let dflags1 = dflags0{ ghcMode   = mode,
                         backend   = bcknd,
                         ghcLink   = link,
                         verbosity = case postLoadMode of
                                         DoEval _ -> 0
                                         DoRun    -> 0
                                         _other   -> 1
                        }

      -- turn on -fimplicit-import-qualified for GHCi now, so that it
      -- can be overridden from the command-line
      -- XXX: this should really be in the interactive DynFlags, but
      -- we don't set that until later in interactiveUI
      -- We also set -fignore-optim-changes and -fignore-hpc-changes,
      -- which are program-level options. Again, this doesn't really
      -- feel like the right place to handle this, but we don't have
      -- a great story for the moment.
      dflags2  | DoInteractive <- postLoadMode = def_ghci_flags
               | DoEval _      <- postLoadMode = def_ghci_flags
               | DoRun         <- postLoadMode = def_ghci_flags
               | otherwise                     = dflags1
        where def_ghci_flags = dflags1 `gopt_set` Opt_ImplicitImportQualified
                                       `gopt_set` Opt_IgnoreOptimChanges
                                       `gopt_set` Opt_IgnoreHpcChanges
                                       -- Setting this by default has the nice effect that
                                       -- -fno-code and --interactive falls back to interpreter rather than
                                       -- object code but has little other effect unless you are also using
                                       -- fat interface files.
                                       `gopt_set` Opt_UseBytecodeRatherThanObjects
                                       -- By default enable the debugger by inserting breakpoints
                                       `gopt_set` Opt_InsertBreakpoints

  logger1 <- getLogger
  let logger2 = setLogFlags logger1 (initLogFlags dflags2)

        -- The rest of the arguments are "dynamic"
        -- Leftover ones are presumably files
  (dflags4, fileish_args, dynamicFlagWarnings) <-
      GHC.parseDynamicFlags logger2 dflags2 args'

  let logger4 = setLogFlags logger2 (initLogFlags dflags4)

  GHC.prettyPrintGhcErrors logger4 $ do

  let diag_opts = initDiagOpts dflags4
  let flagWarnings' = GhcDriverMessage <$> mconcat [warnsToMessages diag_opts flagWarnings, dynamicFlagWarnings]

  handleSourceError (\e -> do
       GHC.printException e
       liftIO $ exitWith (ExitFailure 1)) $ do
         liftIO $ printOrThrowDiagnostics logger4 (initPrintConfig dflags4) diag_opts flagWarnings'

  liftIO $ showBanner postLoadMode dflags4

  let (dflags5, srcs, objs) = parseTargetFiles dflags4 (map unLoc fileish_args)

  -- we've finished manipulating the DynFlags, update the session
  _ <- GHC.setSessionDynFlags dflags5
  dflags6 <- GHC.getSessionDynFlags

  -- Must do this before loading plugins
  liftIO $ initUniqSupply (initialUnique dflags6) (uniqueIncrement dflags6)

  -- Initialise plugins here because the plugin author might already expect this
  -- subsequent call to `getLogger` to be affected by a plugin.
  initializeSessionPlugins
  hsc_env <- getSession
  logger <- getLogger


        ---------------- Display configuration -----------
  case verbosity dflags6 of
    v | v == 4 -> liftIO $ dumpUnitsSimple hsc_env
      | v >= 5 -> liftIO $ dumpUnits       hsc_env
      | otherwise -> return ()

        ---------------- Final sanity checking -----------
  liftIO $ checkOptions postLoadMode dflags6 srcs objs units

  ---------------- Do the business -----------
  handleSourceError (\e -> do
       GHC.printException e
       liftIO $ exitWith (ExitFailure 1)) $ do
    case postLoadMode of
       ShowInterface f        -> liftIO $ showIface logger
                                                    (hsc_dflags hsc_env)
                                                    (hsc_units  hsc_env)
                                                    (hsc_NC     hsc_env)
                                                    f
       DoMake                 -> doMake units srcs
       DoMkDependHS           -> doMkDependHS (map fst srcs)
       StopBefore p           -> liftIO (oneShot hsc_env p srcs)
       DoInteractive          -> ghciUI units srcs Nothing
       DoEval exprs           -> ghciUI units srcs $ Just $ reverse exprs
       DoRun                  -> doRun units srcs args
       DoAbiHash              -> abiHash (map fst srcs)
       ShowPackages           -> liftIO $ showUnits hsc_env
       DoFrontend f           -> doFrontend f srcs
       DoBackpack             -> doBackpack (map fst srcs)

  liftIO $ dumpFinalStats logger

doRun :: [String] -> [(FilePath, Maybe Phase)] -> [Located String] -> Ghc ()
doRun units srcs args = do
    dflags <- getDynFlags
    let mainFun = fromMaybe "main" (mainFunIs dflags)
    ghciUI units srcs (Just ["System.Environment.withArgs " ++ show args' ++ " (Control.Monad.void " ++ mainFun ++ ")"])
  where
    args' = drop 1 $ dropWhile (/= "--") $ map unLoc args

ghciUI :: [String] -> [(FilePath, Maybe Phase)] -> Maybe [String] -> Ghc ()
#if !defined(HAVE_INTERNAL_INTERPRETER)
ghciUI _ _ _ =
  throwGhcException (CmdLineError "not built for interactive use")
#else
ghciUI units srcs maybe_expr = do
  hs_srcs <- case NE.nonEmpty units of
    Just ne_units -> do
      initMulti ne_units (checkOptions DoMake)
    Nothing -> do
      case srcs of
        [] -> return []
        _  -> do
          s <- initMake srcs
          dflags <- getDynFlags
          return $ map (uncurry (,Just $ homeUnitId_ dflags,)) s
  interactiveUI defaultGhciSettings hs_srcs maybe_expr
#endif

-- ----------------------------------------------------------------------------
-- Run --make mode

doMake :: [String] -> [(String, Maybe Phase)] -> Ghc ()
doMake units targets = do
  hs_srcs <- case NE.nonEmpty units of
    Just ne_units -> do
      initMulti ne_units (checkOptions DoMake)
    Nothing -> do
      s <- initMake targets
      return $ map (uncurry (,Nothing,)) s
  case hs_srcs of
    [] -> return ()
    _  -> do
      targets' <- mapM (\(src, uid, phase) -> GHC.guessTarget src uid phase) hs_srcs
      GHC.setTargets targets'
      ok_flag <- GHC.load LoadAllTargets
      when (failed ok_flag) (liftIO $ exitWith (ExitFailure 1))

-- ---------------------------------------------------------------------------
-- Various banners and verbosity output.

showBanner :: PostLoadMode -> DynFlags -> IO ()
showBanner _postLoadMode dflags = do
   let verb = verbosity dflags

#if defined(HAVE_INTERNAL_INTERPRETER)
   -- Show the GHCi banner
   when (isInteractiveMode _postLoadMode && verb >= 1) $ putStrLn ghciWelcomeMsg
#endif

   -- Display details of the configuration in verbose mode
   when (verb >= 2) $
    do hPutStr stderr "Glasgow Haskell Compiler, Version "
       hPutStr stderr cProjectVersion
       hPutStr stderr ", stage "
       hPutStr stderr cStage
       hPutStr stderr " booted by GHC version "
       hPutStrLn stderr cBooterVersion

-- We print out a Read-friendly string, but a prettier one than the
-- Show instance gives us
showInfo :: DynFlags -> IO ()
showInfo dflags = do
        let sq x = " [" ++ x ++ "\n ]"
        putStrLn $ sq $ intercalate "\n ," $ map show $ compilerInfo dflags

-- TODO use GHC.Utils.Error once that is disentangled from all the other GhcMonad stuff?
showSupportedExtensions :: Maybe String -> IO ()
showSupportedExtensions m_top_dir = do
  res <- runExceptT $ do
    top_dir <- lift (tryFindTopDir m_top_dir) >>= \case
      Nothing -> throwE $ SettingsError_MissingData "Could not find the top directory, missing -B flag"
      Just dir -> pure dir
    initSettings top_dir
  arch_os <- case res of
    Right s -> pure $ platformArchOS $ sTargetPlatform s
    Left (SettingsError_MissingData msg) -> do
      hPutStrLn stderr $ "WARNING: " ++ show msg
      hPutStrLn stderr $ "cannot know target platform so guessing target == host (native compiler)."
      pure hostPlatformArchOS
    Left (SettingsError_BadData msg) -> do
      hPutStrLn stderr msg
      exitWith $ ExitFailure 1
  mapM_ putStrLn $ supportedLanguagesAndExtensions arch_os

showVersion :: IO ()
showVersion = putStrLn (cProjectName ++ ", version " ++ cProjectVersion)

showOptions :: Bool -> IO ()
showOptions isInteractive = putStr (unlines availableOptions)
    where
      availableOptions = concat [
        flagsForCompletion isInteractive,
        map ('-':) (getFlagNames mode_flags)
        ]
      getFlagNames opts         = map flagName opts

showGhcUsage :: DynFlags -> IO ()
showGhcUsage = showUsage False

showGhciUsage :: DynFlags -> IO ()
showGhciUsage = showUsage True

showUsage :: Bool -> DynFlags -> IO ()
showUsage ghci dflags = do
  let usage_path = if ghci then ghciUsagePath dflags
                           else ghcUsagePath dflags
  usage <- readFile usage_path
  progName <- getProgName
  dump progName usage
  where
    dump progName xs = case xs of
      ""        -> return ()
      '$':'$':s -> putStr progName >> dump progName s
      c:s       -> putChar c >> dump progName s

dumpFinalStats :: Logger -> IO ()
dumpFinalStats logger = do
  when (logHasDumpFlag logger Opt_D_faststring_stats) $ dumpFastStringStats logger

  when (logHasDumpFlag logger Opt_D_dump_faststrings) $ do
    fss <- getFastStringTable
    let ppr_table         = fmap ppr_segment (fss `zip` [0..])
        ppr_segment (s,n) = hang (text "Segment" <+> int n) 2 (vcat (fmap ppr_bucket (s `zip` [0..])))
        ppr_bucket  (b,n) = hang (text "Bucket" <+> int n) 2 (vcat (fmap ftext b))
    putDumpFileMaybe logger Opt_D_dump_faststrings "FastStrings" FormatText (vcat ppr_table)

dumpFastStringStats :: Logger -> IO ()
dumpFastStringStats logger = do
  segments <- getFastStringTable
  hasZ <- getFastStringZEncCounter
  let buckets = concat segments
      bucketsPerSegment = map length segments
      entriesPerBucket = map length buckets
      entries = sum entriesPerBucket
      msg = text "FastString stats:" $$ nest 4 (vcat
        [ text "segments:         " <+> int (length segments)
        , text "buckets:          " <+> int (sum bucketsPerSegment)
        , text "entries:          " <+> int entries
        , text "largest segment:  " <+> int (maximum bucketsPerSegment)
        , text "smallest segment: " <+> int (minimum bucketsPerSegment)
        , text "longest bucket:   " <+> int (maximum entriesPerBucket)
        , text "has z-encoding:   " <+> (hasZ `pcntOf` entries)
        ])
        -- we usually get more "has z-encoding" than "z-encoded", because
        -- when we z-encode a string it might hash to the exact same string,
        -- which is not counted as "z-encoded".  Only strings whose
        -- Z-encoding is different from the original string are counted in
        -- the "z-encoded" total.
  putMsg logger msg
  where
   x `pcntOf` y = int ((x * 100) `quot` y) Outputable.<> char '%'

showUnits, dumpUnits, dumpUnitsSimple :: HscEnv -> IO ()
showUnits       hsc_env = putStrLn (showSDoc (hsc_dflags hsc_env) (pprUnits (hsc_units hsc_env)))
dumpUnits       hsc_env = putMsg (hsc_logger hsc_env) (pprUnits (hsc_units hsc_env))
dumpUnitsSimple hsc_env = putMsg (hsc_logger hsc_env) (pprUnitsSimple (hsc_units hsc_env))

-- -----------------------------------------------------------------------------
-- Frontend plugin support

doFrontend :: ModuleName -> [(String, Maybe Phase)] -> Ghc ()
doFrontend modname srcs = do
    hsc_env <- getSession
    (frontend_plugin, _pkgs, _deps) <- liftIO $ loadFrontendPlugin hsc_env modname -- TODO do these need to recorded?
    frontend frontend_plugin
      (reverse $ frontendPluginOpts (hsc_dflags hsc_env)) srcs

-- -----------------------------------------------------------------------------
-- ABI hash support

{-
        ghc --abi-hash Data.Foo System.Bar

Generates a combined hash of the ABI for modules Data.Foo and
System.Bar.  The modules must already be compiled, and appropriate -i
options may be necessary in order to find the .hi files.

This is used by Cabal for generating the ComponentId for a
package.  The ComponentId must change when the visible ABI of
the package changes, so during registration Cabal calls ghc --abi-hash
to get a hash of the package's ABI.
-}

-- | Print ABI hash of input modules.
--
-- The resulting hash is the MD5 of the GHC version used (#5328,
-- see 'hiVersion') and of the existing ABI hash from each module (see
-- 'mi_mod_hash').
abiHash :: [String] -- ^ List of module names
        -> Ghc ()
abiHash strs = do
  hsc_env <- getSession
  let dflags    = hsc_dflags hsc_env

  liftIO $ do

  let find_it str = do
         let modname = mkModuleName str
         r <- findImportedModule hsc_env modname NoPkgQual
         case r of
           Found _ m -> return m
           _error    ->
            let opts   = initIfaceMessageOpts dflags
                err_txt = missingInterfaceErrorDiagnostic opts
                        $ cannotFindModule hsc_env modname r
            in throwGhcException . CmdLineError $ showSDoc dflags err_txt

  mods <- mapM find_it strs

  -- MP: loadUserInterface is inefficient here since we will never find a cached
  -- interface. computeInterface is probably better.
  let get_iface modl = loadUserInterface NotBoot (text "abiHash") modl
  ifaces <- initIfaceCheck (text "abiHash") hsc_env $ mapM get_iface mods

  bh <- openBinMem (3*1024) -- just less than a block
  put_ bh hiVersion
    -- package hashes change when the compiler version changes (for now)
    -- see #5328
  mapM_ (put_ bh . mi_mod_hash) ifaces
  f <- fingerprintBinMem bh

  putStrLn (showPpr dflags f)

