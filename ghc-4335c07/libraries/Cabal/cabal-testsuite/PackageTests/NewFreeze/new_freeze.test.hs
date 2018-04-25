import Test.Cabal.Prelude
import Control.Monad.IO.Class
import System.Directory

-- Test that 'cabal new-freeze' works with multiple versions of a build tool
-- dependency.
--
-- The repository contains versions 1.0, 2.0, and 3.0 of the build tool. There
-- is one local package, which requires >= 2, and a library dependency of the
-- local package, which requires < 2, so cabal should pick versions 1.0 and 3.0
-- of the build tool when there are no constraints.
main = cabalTest $ withSourceCopy $ do
  withRepo "repo" $ do
    cabal' "new-build" ["--dry-run"] >>= assertUsesLatestBuildTool

    -- Force the project to use versions 1.0 and 2.0 of the build tool.
    cabal "new-freeze" ["--constraint=any.my-build-tool-dep < 3"]

    cwd <- fmap testCurrentDir getTestEnv
    let freezeFile = cwd </> "cabal.project.freeze"

    -- The freeze file should specify a version range that includes both
    -- versions of the build tool from the install plan. (This constraint will
    -- be replaced with two exe qualified constraints once #3502 is fully
    -- implemented).
    assertFileDoesContain freezeFile "any.my-build-tool-dep ==1.0 || ==2.0"

    -- The library dependency should have a constraint on an exact version.
    assertFileDoesContain freezeFile "any.my-library-dep ==1.0"

    -- The local package should be unconstrained.
    assertFileDoesNotContain freezeFile "my-local-package"

    -- cabal should be able to find an install plan that fits the constraints
    -- from the freeze file.
    cabal' "new-build" ["--dry-run"] >>= assertDoesNotUseLatestBuildTool

    -- cabal should choose the latest version again after the freeze file is
    -- removed.
    liftIO $ removeFile freezeFile
    cabal' "new-build" ["--dry-run"] >>= assertUsesLatestBuildTool
  where
    assertUsesLatestBuildTool out = do
      assertOutputContains "my-build-tool-dep-3.0 (exe:my-build-tool)" out
      assertOutputDoesNotContain "my-build-tool-dep-2.0" out

    assertDoesNotUseLatestBuildTool out = do
      assertOutputContains "my-build-tool-dep-2.0 (exe:my-build-tool)" out
      assertOutputDoesNotContain "my-build-tool-dep-3.0" out
