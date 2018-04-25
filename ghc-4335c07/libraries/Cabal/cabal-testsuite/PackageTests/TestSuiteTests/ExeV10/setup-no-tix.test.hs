import Test.Cabal.Prelude
import Distribution.Simple.Hpc

-- When -fhpc is manually provided, but --enable-coverage is not,
-- the desired behavior is that we pass on -fhpc to GHC, but do NOT
-- attempt to do anything with the tix file (i.e., do not change
-- where it gets output, do not attempt to run hpc on it.)
--
-- This was requested in #1945, by a user who wanted to handle the
-- coverage manually.  Unfortunately, this behavior is (not yet)
-- documented in the manual. (In fact, coverage is not documented
-- at all.)
--
main = setupAndCabalTest $ do
    -- Source copy is necessary as GHC defaults to dumping tix
    -- file in the CWD, and we do NOT clean it up after the fact.
    withSourceCopy $ do
        dist_dir <- fmap testDistDir getTestEnv
        setup_build
          [ "--enable-tests"
          , "--ghc-option=-fhpc"
          , "--ghc-option=-hpcdir"
          , "--ghc-option=" ++ dist_dir ++ "/hpc/vanilla" ]
        setup "test" ["test-Short", "--show-details=direct"]
        lbi <- getLocalBuildInfoM
        let way = guessWay lbi
        shouldNotExist $ tixFilePath dist_dir way "test-Short"
