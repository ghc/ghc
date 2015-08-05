module Rules.Compile (compilePackage) where

import Way
import Base
import Util
import Builder
import Expression
import qualified Target
import Oracles.DependencyList
import Settings.TargetDirectory
import Rules.Actions
import Rules.Resources
import Data.Maybe

matchBuildResult :: FilePath -> String -> FilePath -> Bool
matchBuildResult buildPath extension file =
    (buildPath <//> "*" ++ extension) ?== file && (isJust . detectWay $ file)

compilePackage :: Resources -> StagePackageTarget -> Rules ()
compilePackage _ target = do
    let stage     = Target.stage target
        pkg       = Target.package target
        path      = targetPath stage pkg
        buildPath = path -/- "build"
        cDepsFile = buildPath -/- "c.deps"
        hDepsFile = buildPath -/- "haskell.deps"

    matchBuildResult buildPath "hi" ?> \hi -> do
        let way = fromJust . detectWay $ hi -- fromJust is safe
        need [hi -<.> osuf way]

    matchBuildResult buildPath "o" ?> \obj -> do
        let way        = fromJust . detectWay $ obj -- fromJust is safe
            vanillaObj = takeFileName obj -<.> "o"
        cDeps <- dependencyList cDepsFile vanillaObj
        hDeps <- dependencyList hDepsFile obj
        let hSrcDeps = filter ("//*hs" ?==) hDeps

        when (null cDeps && null hDeps) $
            putError $ "Cannot determine sources for '" ++ obj ++ "'."

        if null cDeps
        then build $ fullTargetWithWay target hSrcDeps (Ghc stage) way [obj]
        else build $ fullTarget        target cDeps    (Gcc stage)     [obj]
