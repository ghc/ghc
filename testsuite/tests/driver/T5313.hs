import qualified GHC
import qualified GHC.Run as GHC
import System.Environment

main = do
  (libdir:_) <- getArgs
  GHC.runGhcTWithAbiHashes (Just libdir) $ do
      -- begin initialize
      df0 <- GHC.getSessionDynFlags
      let df1 = df0{GHC.ghcMode    = GHC.CompManager,
                    GHC.backend    = GHC.Interpreter,
                    GHC.ghcLink    = GHC.LinkInMemory,
                    GHC.verbosity  = 0}
      _ <- GHC.setSessionDynFlags df1
      -- begin reset
      GHC.setContext []
      GHC.setTargets []
      _ <- GHC.load GHC.LoadAllTargets
      return ()
