import GHC
import GHC.Run
import System.Environment

main = do
  (libdir:_) <- getArgs
  runGhcTWithAbiHashes (Just libdir) $ do
    df <- getSessionDynFlags
    setSessionDynFlags $ df { ghcLink = LinkInMemory }
    load LoadAllTargets
