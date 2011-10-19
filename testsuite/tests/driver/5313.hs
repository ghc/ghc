import qualified GHC
import System.Environment

main = do
  (libdir0:_) <- getArgs
  let libdir = filter (/= '\'') libdir0 -- sigh, remove superfluous quotes.
                                        -- necessary to get the ghci way working
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
