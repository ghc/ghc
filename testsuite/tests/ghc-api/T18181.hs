import GHC
import System.Environment

main = do
  (libdir:_) <- getArgs
  GHC.runGhcT (Just libdir) $ do
    df <- getSessionDynFlags
    setSessionDynFlags $ df { ghcLink = LinkInMemory }
    load LoadAllTargets
