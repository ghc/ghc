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
        =<< setup' "configure" ["--enable-benchmarks"]
    lbi <- getLocalBuildInfoM
    let gotBenchmark = head $ benchmarks (localPkgDescr lbi)
    assertEqual "benchmarkName"
                (mkUnqualComponentName "dummy")
                (benchmarkName gotBenchmark)
    assertEqual "benchmarkInterface"
                (BenchmarkExeV10 (mkVersion [1,0]) "dummy.hs")
                (benchmarkInterface gotBenchmark)
    -- NB: Not testing targetBuildDepends (benchmarkBuildInfo gotBenchmark),
    -- as the dependency varies with cabal-install
    assertEqual "benchmarkBuildInfo/hsSourceDirs"
                ["."]
                (hsSourceDirs (benchmarkBuildInfo gotBenchmark))
    return ()
