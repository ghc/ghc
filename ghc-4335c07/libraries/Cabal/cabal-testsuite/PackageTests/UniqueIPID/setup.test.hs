import Test.Cabal.Prelude
import Data.List
-- Test that setup computes different IPIDs when dependencies change
main = setupAndCabalTest $ do
    withPackageDb $ do
        withDirectory "P1" $ setup "configure" ["--disable-deterministic"]
        withDirectory "P2" $ setup "configure" ["--disable-deterministic"]
        withDirectory "P1" $ setup "build" []
        withDirectory "P1" $ setup "build" [] -- rebuild should work
        recordMode DoNotRecord $ do
            r1 <- withDirectory "P1" $ setup' "register" ["--print-ipid", "--inplace"]
            withDirectory "P2" $ setup "build" []
            r2 <- withDirectory "P2" $ setup' "register" ["--print-ipid", "--inplace"]
            let exIPID s = takeWhile (/= '\n') $
                     head . filter (isPrefixOf $ "UniqueIPID-0.1-") $ (tails s)
            assertNotEqual "ipid match" (exIPID $ resultOutput r1) (exIPID $ resultOutput r2)
