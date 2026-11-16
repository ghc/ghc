-- | @ghc --make@ with an exchangeable External Package State store.
--
-- > ghc-eps-store -B<libdir> --eps-store=STORE [--eps-stats] [ghc flags and targets]
--
-- STORE is one of
--
--   * @memory@: GHC's own in-memory store (the baseline)
--   * @sqlite[:PATH]@: low-memory store, declarations in an SQLite database
--     (default: a fresh file in the temporary directory, deleted at the end)
--   * @hi@: low-memory store, declarations re-read from the interface files
--
-- Response files (@\@file@) are expanded. Only @--make@ mode is supported.
module Main (main) where

import EpsStore.DeclStore
import EpsStore.HiFile
import EpsStore.LowMem
import EpsStore.Sqlite

import GHC
import GHC.Driver.Env ( hsc_NC )
import GHC.Unit.External ( newInMemoryEps )

import Control.Monad
import Control.Monad.IO.Class
import Data.List ( isPrefixOf, partition, stripPrefix )
import GHC.ResponseFile ( expandResponse )
import System.Directory ( getTemporaryDirectory, removeFile )
import System.Environment ( getArgs )
import System.Exit
import System.FilePath ( (</>) )
import System.IO
import System.Posix.Process ( getProcessID )
import System.Mem ( performMajorGC )
import GHC.Stats

data StoreSpec = Memory | Sqlite (Maybe FilePath) | HiFiles

parseStore :: String -> Either String StoreSpec
parseStore "memory" = Right Memory
parseStore "hi"     = Right HiFiles
parseStore "sqlite" = Right (Sqlite Nothing)
parseStore s | Just p <- stripPrefix "sqlite:" s = Right (Sqlite (Just p))
parseStore s = Left ("unknown EPS store: " ++ s)

usage :: String
usage = "usage: ghc-eps-store -B<libdir> --eps-store=memory|sqlite[:PATH]|hi \
        \[--eps-stats] [ghc flags] targets..."

main :: IO ()
main = do
  args0 <- expandResponse =<< getArgs
  let (ours, rest0) = partition ("--eps-" `isPrefixOf`) args0
      (libdirs, rest) = partition ("-B" `isPrefixOf`) (filter (/= "--make") rest0)
      want_stats = "--eps-stats" `elem` ours
  spec <- case [ s | o <- ours, Just s <- [stripPrefix "--eps-store=" o] ] of
    [s] -> either die pure (parseStore s)
    _   -> die usage
  libdir <- case libdirs of
    [b] -> pure (drop 2 b)
    _   -> die usage

  ok <- runGhc (Just libdir) $ do
    dflags0 <- getSessionDynFlags
    logger  <- getLogger
    (dflags1, targets, _warns) <- parseDynamicFlags logger dflags0 (map noLoc rest)
    _ <- setSessionDynFlags dflags1

    -- Install the store before anything is loaded.
    hsc_env <- getSession
    (cleanup, report) <- liftIO $ case spec of
      Memory -> do
        eps <- newInMemoryEps
        pure ((eps, pure ()), pure ())
      Sqlite mb_path -> do
        path <- maybe default_db pure mb_path
        store <- newSqliteStore (hsc_NC hsc_env) path
        low_mem store $ do
          ds_close store
          when (null mb_path) $ removeFile path
      HiFiles -> do
        store <- newHiFileStore hsc_env
        low_mem store (ds_close store)
    let (eps, close) = cleanup
    setExternalPackageStore eps

    ts <- mapM (\t -> guessTarget (unLoc t) Nothing Nothing) targets
    setTargets ts
    res <- load LoadAllTargets
    liftIO $ do
      when want_stats $ do
        report
        -- The session (and with it the EPS) is still alive here.
        enabled <- getRTSStatsEnabled
        when enabled $ do
          performMajorGC
          st <- getRTSStats
          hPutStrLn stderr $ unlines
            [ "eps-store rts:"
            , "  live bytes at end:   " ++ show (gcdetails_live_bytes (gc st))
            , "  max live bytes:      " ++ show (max_live_bytes st)
            , "  allocated bytes:     " ++ show (allocated_bytes st)
            , "  mutator cpu ns:      " ++ show (mutator_cpu_ns st)
            , "  gc cpu ns:           " ++ show (gc_cpu_ns st)
            ]
      close
    pure (succeeded res)
  unless ok exitFailure
  where
    default_db = do
      tmp <- getTemporaryDirectory
      pid <- getProcessID
      pure (tmp </> ("eps-store-" ++ show pid ++ ".sqlite"))

    low_mem store close = do
      (eps, get_stats) <- newLowMemEps store
      let report = do
            s <- get_stats
            hPutStrLn stderr $ unlines
              [ "eps-store " ++ ds_name store ++ ":"
              , "  modules:       " ++ show (lms_modules s)
              , "  declarations:  " ++ show (lms_decls s)
              , "  lookups:       " ++ show (lms_lookups s)
              , "  cache hits:    " ++ show (lms_cache_hits s)
              , "  rehydrations:  " ++ show (lms_rehydrations s)
              ]
      pure ((eps, close), report)
