{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import GHC hiding (SuccessFlag(..))
import GHC.Utils.Outputable
import GHC.Data.FastString
import GHC.Tc.Utils.Monad
import GHC.Iface.Load
import GHC.Iface.Recomp
import GHC.Driver.Env.Types
import GHC.Driver.Ppr
import GHC.Unit.Types

import Control.Monad
import System.Environment
import System.FilePath
import System.Directory
import GHC.Data.Maybe
import GHC.Unit.Env
import GHC.Driver.Make
import GHC.Driver.Env

-- This test checks propeties of reading interface files from disk

parseMode :: String -> (MaybeValidated ModIface -> Bool)
parseMode "full" = checkFullMode
parseMode "src" = checkSrcMode
parseMode mode = error $ "unknown mode:" ++ mode

-- Checks performance of a full complete recompilation check
checkFullMode :: MaybeValidated ModIface -> Bool
checkFullMode (UpToDateItem {}) = True
checkFullMode _ = False

-- Checks performance of a recompilation check that only checks if the source file has changed
checkSrcMode :: MaybeValidated ModIface -> Bool
checkSrcMode (OutOfDateItem (RecompBecause SourceFileChanged) _) = True
checkSrcMode _ = False

main :: IO ()
main = do
  args <- getArgs
  (libdir, iterations, check) <- case args of
    (dir: n: mode: _) -> return (dir, read n, parseMode mode)
    _ -> do
      error "Error: Expected libdir, iterations, mode"


  runGhc (Just libdir) $ do
    -- Set GHC session flags
    -- Parse command line flags

    logger <- getLogger
    dflags0 <- getSessionDynFlags
    let parseDynFlags dflags as = do
          (dflags', _, _) <- parseDynamicFlags logger dflags (map noLoc as)
          return dflags'

    -- Set up basic flags for our test
    dflags1 <- parseDynFlags dflags0 ["-haddock", "-fwrite-if-self-recomp-flags", "-fno-code", "-fwrite-interface", "-fwrite-if-simplified-core","-v0", "-O"]
    _ <- setSessionDynFlags dflags1


    -- Get the current directory to find our test module
    pwd <- liftIO getCurrentDirectory
    let moduleFile = pwd </> "IfaceRecompTest.hs"

    hsc_env <- getSession
    let hiFile = pwd </> "IfaceRecompTest.hi"
    Right mod_sum <- liftIO $ summariseFile hsc_env (ue_unitHomeUnit (UnitId (mkFastString "main")) (hsc_unit_env hsc_env)) mempty moduleFile Nothing Nothing


    -- liftIO $ putStrLn $ "Loading interface file: " ++ hiFile

    dflags <- getSessionDynFlags
    let test_mod = mkModule (fsToUnit (mkFastString "main")) (mkModuleName "IfaceRecompTest")

    -- Load the interface
    let hooks = hsc_hooks hsc_env
    let name_cache = hsc_NC hsc_env
    mb_iface <- liftIO $ readIface hooks logger dflags name_cache test_mod hiFile
    case mb_iface of
      Failed {} -> liftIO $ do
        error "Failed to load interface"
      Succeeded iface -> do
        -- liftIO $ putStrLn "Successfully loaded interface. Now checking it 10000 times."

        -- Call checkOldIface 10000 times
        liftIO $ replicateM_ iterations $ do
          res <- checkOldIface (hscSetFlags (ms_hspp_opts mod_sum) hsc_env) mod_sum (Just iface)
          if check res
            then return ()
            else error $ "Interface check failed: " ++ showSDoc dflags (ppr (fmap (const ()) res))

        liftIO $ putStrLn "Completed interface checks."
