
import Test.Cabal.Prelude

import Control.Monad.IO.Class

import Test.Cabal.CheckArMetadata

-- Test that setup determinstically generates object archives
main = setupAndCabalTest $ do
    setup_build []
    dist_dir <- fmap testDistDir getTestEnv
    lbi <- getLocalBuildInfoM
    liftIO $ checkMetadata lbi (dist_dir </> "build")
