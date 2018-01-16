import Test.Cabal.Prelude

import Distribution.Version
import Distribution.Simple.LocalBuildInfo
import Distribution.Package
import Distribution.PackageDescription
import Distribution.Types.UnqualComponentName
import Control.Monad.IO.Class
import Distribution.Simple.Configure

main = setupAndCabalTest $ do
    assertOutputDoesNotContain "unknown section type"
        =<< setup' "configure" ["--enable-tests"]
    lbi <- getLocalBuildInfoM
    let gotTestSuite = head $ testSuites (localPkgDescr lbi)
    assertEqual "testName"  (mkUnqualComponentName "dummy")
                            (testName gotTestSuite)
    assertEqual "testInterface" (TestSuiteExeV10 (mkVersion [1,0]) "dummy.hs")
                                (testInterface gotTestSuite)
    -- NB: Not testing targetBuildDepends (testBuildInfo gotTestSuite)
    -- as dependency varies with cabal-install
    assertEqual "testBuildInfo/hsSourceDirs" ["."] (hsSourceDirs (testBuildInfo gotTestSuite))
    return ()
