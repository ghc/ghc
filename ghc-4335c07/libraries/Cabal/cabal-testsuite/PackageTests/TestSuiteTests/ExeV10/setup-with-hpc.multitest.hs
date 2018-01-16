import qualified Control.Exception as E (IOException, catch)
import Control.Monad
import Control.Monad.IO.Class
import Data.Maybe (catMaybes)
import System.FilePath
import Data.List

import Distribution.Compiler (CompilerFlavor(..), CompilerId(..))
import Distribution.Simple.Compiler (compilerId)
import Distribution.Types.LocalBuildInfo (localPackage)
import Distribution.Simple.LocalBuildInfo (compiler, localCompatPackageKey)
import Distribution.Simple.Hpc
import Distribution.Simple.Program.Builtin (hpcProgram)
import Distribution.Simple.Program.Db
    ( emptyProgramDb, configureProgram, requireProgramVersion )
import Distribution.Text (display)
import qualified Distribution.Verbosity as Verbosity
import Distribution.Version (mkVersion, orLaterVersion)

import Test.Cabal.Prelude

main =
  forM_ (choose4 [True, False]) $ \(libProf, exeProf, exeDyn, shared) ->
   -- NB: inside so we cleanup each time.  This seems to
   -- be important on Mac OS X, where leftover build products
   -- can cause errors like this:
   --
   -- Test suite test-Short: RUNNING...
   -- setup-with-hpc.dist/dist/build/test-Short/test-Short
   -- dyld: Library not loaded:
   -- @rpath/libHSmy-0.1-B1rF0UIcOou1OUvUhHrTHK-ghc7.8.4.dylib
   --   Referenced from:
   --   /Users/travis/build/haskell/cabal/cabal-testsuite/PackageTests/TestSuiteTests/ExeV10/setup-with-hpc.dist/dist/build/test-Short/test-Short
   --     Reason: image not found
   --
   -- This should get fixed eventually, but not today as I don't
   -- have an actual Mac OS X box to debug on.
   setupAndCabalTest . recordMode DoNotRecord $ do
    let name | null suffixes = "Vanilla"
             | otherwise = intercalate "-" suffixes
          where
            suffixes = catMaybes
                      [ if libProf then Just "LibProf" else Nothing
                      , if exeProf then Just "ExeProf" else Nothing
                      , if exeDyn then Just "ExeDyn" else Nothing
                      , if shared then Just "Shared" else Nothing
                      ]
        opts = catMaybes
            [ enable libProf "library-profiling"
            , enable exeProf "profiling"
            , enable exeDyn "executable-dynamic"
            , enable shared "shared"
            ]
          where
            enable cond flag
              | cond = Just $ "--enable-" ++ flag
              | otherwise = Nothing
    -- Ensure that both .tix file and markup are generated if coverage
    -- is enabled.
    shared_libs <- hasSharedLibraries
    prof_libs <- hasProfiledLibraries
    unless ((exeDyn || shared) && not shared_libs) $ do
      unless ((libProf || exeProf) && not prof_libs) $ do
        isCorrectVersion <- liftIO $ correctHpcVersion
        when isCorrectVersion $ do
            dist_dir <- fmap testDistDir getTestEnv
            setup_build ("--enable-tests" : "--enable-coverage" : opts)
            setup "test" ["test-Short", "--show-details=direct"]
            lbi <- getLocalBuildInfoM
            let way = guessWay lbi
                CompilerId comp version = compilerId (compiler lbi)
                subdir
                  | comp == GHC && version >= mkVersion [7,10] =
                      localCompatPackageKey lbi
                  | otherwise = display (localPackage lbi)
            mapM_ shouldExist
                [ mixDir dist_dir way "my-0.1" </> subdir </> "Foo.mix"
                , mixDir dist_dir way "test-Short" </> "Main.mix"
                , tixFilePath dist_dir way "test-Short"
                , htmlDir dist_dir way "test-Short" </> "hpc_index.html"
                ]
  where
    choose4 :: [a] -> [(a, a, a, a)]
    choose4 xs = liftM4 (,,,) xs xs xs xs

-- | Checks for a suitable HPC version for testing.
correctHpcVersion :: IO Bool
correctHpcVersion = do
    let programDb' = emptyProgramDb
    let verbosity = Verbosity.normal
    let verRange  = orLaterVersion (mkVersion [0,7])
    programDb <- configureProgram verbosity hpcProgram programDb'
    (requireProgramVersion verbosity hpcProgram verRange programDb
     >> return True) `catchIO` (\_ -> return False)
  where
    -- Distribution.Compat.Exception is hidden.
    catchIO :: IO a -> (E.IOException -> IO a) -> IO a
    catchIO = E.catch
