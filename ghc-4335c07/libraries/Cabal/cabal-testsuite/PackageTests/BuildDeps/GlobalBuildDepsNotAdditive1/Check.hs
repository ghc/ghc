module PackageTests.BuildDeps.GlobalBuildDepsNotAdditive1.Check where

import Test.Tasty.HUnit
import PackageTests.PackageTester
import System.FilePath
import Data.List
import Control.Exception
import Prelude hiding (catch)


suite :: FilePath -> Assertion
suite ghcPath = do
    let spec = PackageSpec ("PackageTests" </> "BuildDeps" </> "GlobalBuildDepsNotAdditive1") []
    result <- cabal_build spec ghcPath
    do
        assertEqual "cabal build should fail - see test-log.txt" False (successful result)
        let sb = "Could not find module `Prelude'"
        assertBool ("cabal output should be "++show sb) $
            sb `isInfixOf` outputText result
      `catch` \exc -> do
        putStrLn $ "Cabal result was "++show result
        throwIO (exc :: SomeException)
