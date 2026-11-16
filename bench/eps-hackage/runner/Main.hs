{-# LANGUAGE CPP #-}
-- | eps-hackage-run: compile a package with the GHC API (like @ghc --make@)
-- and measure time and memory, in-process.
--
-- > eps-hackage-run --libdir DIR --label L --out FILE [--store STORE] [--hashes]
-- >                 -- GHC-ARGS...
--
-- GHC-ARGS are the flags, response files (@\@file@) and targets of the
-- compilation. With the cabal flag @abstract-eps@ (only for a GHC with the
-- abstract EPS API), @--store memory|hi|sqlite@ installs an EPS store from
-- bench/eps-store; without it, GHC's default store is used.
--
-- Writes a 'Result' (see "EpsHackage.Result") to FILE.
module Main (main) where

import EpsHackage.Result

import GHC
import GHC.Clock ( getMonotonicTime )
import GHC.Fingerprint ( getFileHash )
import GHC.ResponseFile ( expandResponse )
import GHC.Stats
import GHC.Utils.Monad ( MonadIO (..) )

#if defined(ABSTRACT_EPS)
import EpsStore.DeclStore ( ds_close )
import EpsStore.HiFile ( newHiFileStore )
import EpsStore.LowMem
import EpsStore.Sqlite ( newSqliteStore )
import GHC.Driver.Env ( hsc_NC )
import GHC.Unit.External ( newInMemoryEps )
import System.Directory ( getTemporaryDirectory, removeFile )
import System.Posix.Process ( getProcessID )
#endif

import Control.Monad
import Data.List ( isPrefixOf, isSuffixOf, sort )
import System.Directory ( doesDirectoryExist, listDirectory )
import System.Environment ( getArgs )
import System.Exit
import System.FilePath ( (</>) )
import System.IO
import System.Mem ( performMajorGC )

data Opts = Opts
  { oLibdir :: FilePath
  , oLabel  :: String
  , oOut    :: FilePath
  , oStore  :: Maybe String
  , oHashes :: Bool
  , oGhc    :: [String]
  }

usage :: String
usage = "usage: eps-hackage-run --libdir DIR --label L --out FILE [--store STORE] [--hashes] -- GHC-ARGS..."

parseOpts :: [String] -> Opts -> Either String Opts
parseOpts args o = case args of
  "--libdir" : v : r -> parseOpts r o { oLibdir = v }
  "--label"  : v : r -> parseOpts r o { oLabel = v }
  "--out"    : v : r -> parseOpts r o { oOut = v }
  "--store"  : v : r -> parseOpts r o { oStore = Just v }
  "--hashes"     : r -> parseOpts r o { oHashes = True }
  "--"           : r -> Right o { oGhc = r }
  a : _              -> Left ("unknown argument " ++ a ++ "\n" ++ usage)
  []                 -> Right o

main :: IO ()
main = do
  o <- either die pure . flip parseOpts (Opts "" "" "" Nothing False []) =<< getArgs
  when (null (oLibdir o) || null (oOut o)) $ die usage
  enabled <- getRTSStatsEnabled
  unless enabled $ die "eps-hackage-run: needs +RTS -T"
  ghcArgs <- expandResponse (oGhc o)

  (ok, wall, live, storeStats, outputDir) <- runGhc (Just (oLibdir o)) $ do
    dflags0 <- getSessionDynFlags
    logger  <- getLogger
    (dflags1, targets, _warns) <- parseDynamicFlags logger dflags0 (map noLoc ghcArgs)
    _ <- setSessionDynFlags dflags1
    (getStoreStats, close) <- installStore (oStore o)
    ts <- mapM (\t -> guessTarget (unLoc t) Nothing Nothing) targets
    setTargets ts
    start <- liftIO getMonotonicTime
    res <- load LoadAllTargets
    end <- liftIO getMonotonicTime
    -- The session, and with it the EPS, is still alive here.
    liftIO performMajorGC
    live <- liftIO (gcdetails_live_bytes . gc <$> getRTSStats)
    st <- liftIO getStoreStats
    liftIO close
    pure (succeeded res, end - start, live, st, objectDir dflags1)

  st <- getRTSStats
  rss <- maxRss
  hashes <- if oHashes o then maybe (pure []) hashOutputs outputDir else pure []
  let r = Result
        { resLabel = oLabel o
        , resMode = maybe "ghc-api" ("store:" ++) (oStore o)
        , resSucceeded = ok
        , resWallSeconds = wall
        , resMutatorSeconds = fromIntegral (mutator_cpu_ns st) / 1e9
        , resGcSeconds = fromIntegral (gc_cpu_ns st) / 1e9
        , resAllocatedBytes = fromIntegral (allocated_bytes st)
        , resMaxLiveBytes = fromIntegral (max_live_bytes st)
        , resMaxMemInUseBytes = fromIntegral (max_mem_in_use_bytes st)
        , resLiveAtEndBytes = fromIntegral live
        , resMaxRssBytes = rss
        , resMajorGcs = fromIntegral (major_gcs st)
        , resStoreStats = storeStats
        , resOutputs = hashes
        }
  writeFile (oOut o) (show r)
  hPutStrLn stderr ("eps-hackage-run: wrote " ++ oOut o)
  unless ok exitFailure

-- | Install the requested EPS store. Returns an action reading its statistics
-- and one closing it.
installStore :: Maybe String -> Ghc (IO (Maybe StoreStats), IO ())
#if defined(ABSTRACT_EPS)
installStore Nothing = pure (pure Nothing, pure ())
installStore (Just s) = do
  hsc_env <- getSession
  (eps, stats, close) <- liftIO $ case s of
    "memory" -> do
      eps <- newInMemoryEps
      pure (eps, pure Nothing, pure ())
    "hi" -> do
      store <- newHiFileStore hsc_env
      lowMem store (ds_close store)
    "sqlite" -> do
      tmp <- getTemporaryDirectory
      pid <- getProcessID
      let path = tmp </> ("eps-hackage-" ++ show pid ++ ".sqlite")
      store <- newSqliteStore (hsc_NC hsc_env) path
      lowMem store (ds_close store >> removeFile path)
    _ -> die ("unknown EPS store " ++ s ++ " (memory, hi, sqlite)")
  setExternalPackageStore eps
  pure (stats, close)
  where
    lowMem store close = do
      (eps, get_stats) <- newLowMemEps store
      let stats = do
            l <- get_stats
            pure $ Just StoreStats
              { ssModules = lms_modules l, ssDecls = lms_decls l
              , ssLookups = lms_lookups l, ssCacheHits = lms_cache_hits l
              , ssRehydrations = lms_rehydrations l }
      pure (eps, stats, close)
#else
installStore Nothing  = pure (pure Nothing, pure ())
installStore (Just _) =
  liftIO (die "--store needs eps-hackage-runner built with the abstract-eps flag")
#endif

-- | Peak resident set size of this process (VmHWM), in bytes.
maxRss :: IO Integer
maxRss = do
  status <- readFile "/proc/self/status"
  case [ l | l <- lines status, "VmHWM:" `isPrefixOf` l ] of
    (l : _) | [_, kb, "kB"] <- words l -> pure (read kb * 1024)
    _ -> pure 0

-- | Hashes of the interface and object files in the output directory.
hashOutputs :: FilePath -> IO [(FilePath, String)]
hashOutputs dir = do
  files <- sort <$> walk dir
  forM [ f | f <- files, any (`isSuffixOf` f) [".hi", ".o", ".dyn_hi", ".dyn_o"] ] $ \f ->
    (,) (drop (length dir + 1) f) . show <$> getFileHash f
  where
    walk d = do
      es <- listDirectory d
      fmap concat $ forM es $ \e -> do
        let p = d </> e
        isDir <- doesDirectoryExist p
        if isDir then walk p else pure [p]
