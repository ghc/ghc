import Test.Cabal.Prelude
import Data.List
import qualified Data.Char as Char
main = setupAndCabalTest $ do
    skipUnless =<< ghcVersionIs (>= mkVersion [8,1])
    withPackageDb $ do
      containers_id <- getIPID "containers"
      withDirectory "sigs" $ setup_install_with_docs ["--ipid", "sigs-0.1.0.0"]
      withDirectory "indef" $ setup_install_with_docs ["--ipid", "indef-0.1.0.0"]
      withDirectory "sigs" $ do
        -- NB: this REUSES the dist directory that we typechecked
        -- indefinitely, but it's OK; the recompile checker should get it.
        setup_install_with_docs ["--ipid", "sigs-0.1.0.0",
                       "--instantiate-with", "Data.Map=" ++ containers_id ++ ":Data.Map"]
      withDirectory "indef" $ do
        -- Ditto.
        setup_install_with_docs ["--ipid", "indef-0.1.0.0",
                       "--instantiate-with", "Data.Map=" ++ containers_id ++ ":Data.Map"]
      withDirectory "exe" $ do
        setup_install []
        runExe' "exe" [] >>= assertOutputContains "fromList [(0,2),(2,4)]"

