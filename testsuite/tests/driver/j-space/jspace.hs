module Main where

import GHC
import GHC.Driver.Monad
import System.Environment
import GHC.Driver.Env.Types
import GHC.Profiling
import System.Mem

main :: IO ()
main = do
    [libdir] <- getArgs
    runGhc (Just libdir) $ do
      initGhcM ["JSpace.hs", "-O", "-j", "-v0"]


initGhcM :: [String] -> Ghc ()
initGhcM xs = do
    session <- getSession
    df1 <- getSessionDynFlags
    let cmdOpts = ["-fforce-recomp"] ++ xs
    (df2, leftovers, _) <- parseDynamicFlags (hsc_logger session) df1 (map noLoc cmdOpts)
    setSessionDynFlags df2
    ts <- mapM (\s -> guessTarget s Nothing Nothing) $ map unLoc leftovers
    setTargets ts
    _ <- load LoadAllTargets
    liftIO $ do
      requestHeapCensus
      performGC



