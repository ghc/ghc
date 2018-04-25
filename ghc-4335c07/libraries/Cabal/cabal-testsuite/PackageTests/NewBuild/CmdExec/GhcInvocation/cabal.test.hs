import Test.Cabal.Prelude
import Control.Monad.Trans (liftIO)
import System.Directory-- (getDirectoryContents, removeFile)
main = cabalTest $ do
    cabal "new-build" ["inplace-dep"]
    env <- getTestEnv
    liftIO $ removeEnvFiles $ testSourceDir env -- we don't want existing env files to interfere
    cabal "new-exec" ["ghc", "Main.hs"]
    -- TODO external (store) deps, once new-install is working

removeEnvFiles :: FilePath -> IO ()
removeEnvFiles dir = (mapM_ (removeFile . (dir </>))
                   . filter
                       ((".ghc.environment" ==)
                       . take 16))
                   =<< getDirectoryContents dir

