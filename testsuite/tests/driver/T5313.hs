import qualified GHC
import System.Environment

main = do
  (libdir:_) <- getArgs
  GHC.runGhcT (Just libdir) $ do
      -- begin initialize
      df0 <- GHC.getSessionDynFlags
      let df1 = df0{GHC.ghcMode    = GHC.CompManager,
                    GHC.hscTarget  = GHC.HscInterpreted,
                    GHC.ghcLink    = GHC.LinkInMemory,
                    GHC.verbosity  = 0}
      _ <- GHC.setSessionDynFlags df1
      -- begin reset
      GHC.setContext []
      GHC.setTargets []
      _ <- GHC.load GHC.LoadAllTargets
      return ()
