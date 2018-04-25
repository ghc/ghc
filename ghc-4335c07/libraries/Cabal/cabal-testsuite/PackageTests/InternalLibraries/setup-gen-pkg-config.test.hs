import Test.Cabal.Prelude
import Control.Monad
import Control.Monad.IO.Class
import System.Directory
import Data.List
-- Test to see if --gen-pkg-config works.
main = setupAndCabalTest $ do
    withPackageDb $ do
        withDirectory "p" $ do
            setup_build []
            setup "copy" []
            let dir = "pkg-config.bak"
            setup "register" ["--gen-pkg-config=" ++ dir]
            -- Infelicity! Does not respect CWD.
            env <- getTestEnv
            let cwd = testCurrentDir env
                notHidden = not . isHidden
                isHidden name = "." `isPrefixOf` name
            confs <- fmap (sort . filter notHidden)
                   . liftIO $ getDirectoryContents (cwd </> dir)
            forM_ confs $ \conf -> ghcPkg "register" [cwd </> dir </> conf]

        -- Make sure we can see p
        withDirectory "r" $ setup_install []
