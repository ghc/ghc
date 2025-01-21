module Main where

import GHC
import GHC.Driver.Monad
import GHC.Driver.Session
import System.Environment
import GHC.Driver.Env.Types
import GHC.Profiling
import System.Mem
import Data.List (isPrefixOf)
import Control.Monad
import System.Exit
import GHC.Platform

main :: IO ()
main = do
    [libdir] <- getArgs
    runGhc (Just libdir) $ do
      initGhcM ["JSpaceTest.hs", "-O", "-j", "-v0"]


initGhcM :: [String] -> Ghc ()
initGhcM xs = do
    session <- getSession
    df1 <- getSessionDynFlags
    let cmdOpts = ["-fforce-recomp", "-dno-debug-output"] ++ xs
    (df2, leftovers, _) <- parseDynamicFlags (hsc_logger session) df1 (map noLoc cmdOpts)
    setSessionDynFlags df2
    ghcUnitId <- case lookup "Project Unit Id" (compilerInfo df2) of
                    Nothing -> fail "failed to find ghc's unit-id in the compiler info"
                    Just ghcUnitId -> pure ghcUnitId
    ts <- mapM (\s -> guessTarget s Nothing Nothing) $ map unLoc leftovers
    setTargets ts
    _ <- load LoadAllTargets
    let plat :: Platform
        plat = targetPlatform df2
        word_size = case platformWordSize plat of
                      PW8 -> 8
                      PW4 -> 4
    liftIO $ do
      requestHeapCensus
      performGC
      [ys] <- filter (isPrefixOf (ghcUnitId <> ":GHC.Unit.Module.ModDetails.ModDetails")) . lines <$> readFile "jspace.hp"
      let (n :: Int) = read (last (words ys))
      -- The output should be 50 * 8 * word_size (i.e. 3600, or 1600 on 32-bit architectures):
      -- the test contains DEPTH + WIDTH + 2 = 50 modules J, H_0, .., H_DEPTH, W_1, .., W_WIDTH,
      -- and each ModDetails contains 1 (info table) + 8 word-sized fields.
      -- If this number changes DO NOT ACCEPT THE TEST, you have introduced a space leak.
      --
      -- There is some unexplained behaviour where the result is infrequently 3264.. but
      -- this resisted investigation using ghc-debug so the test actually checks whether there
      -- are less than 51 live ModDetails which is still a big improvement over before.
      when (n > (51 * word_size * 9)) $ do
        putStrLn "Space leak detected by jspace test:"
        putStrLn $ (show (n `div` (word_size * 9))) ++ " live ModDetails when <= 51 are expected"
        readFile "jspace.hp" >>= putStrLn
        exitFailure
    return ()


