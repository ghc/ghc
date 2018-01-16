
import Test.Cabal.Prelude

import Control.Monad.IO.Class

import Test.Cabal.CheckArMetadata

-- Test that setup determinstically generates object archives
main = setupAndCabalTest $ do
    setup_build ["--disable-response-files"]
    dist_dir <- fmap testDistDir getTestEnv
    lbi <- getLocalBuildInfoM
    liftIO $ checkMetadata lbi (dist_dir </> "build")
