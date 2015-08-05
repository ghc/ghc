module Rules.Compile (compilePackage) where

import Way
import Base
import Util
import Builder
import Expression
import qualified Target
import Oracles.DependencyList
import Settings.Ways
import Settings.TargetDirectory
import Rules.Actions
import Rules.Resources

compilePackage :: Resources -> StagePackageTarget -> Rules ()
compilePackage _ target = do
    let stage     = Target.stage target
        pkg       = Target.package target
        path      = targetPath stage pkg
        buildPath = path -/- "build"
        cDepsFile = buildPath -/- "c.deps"
        hDepsFile = buildPath -/- "haskell.deps"

    forM_ knownWays $ \way -> do
        (buildPath <//> "*." ++ hisuf way) %> \hi -> do
            let obj = hi -<.> osuf way
            need [obj]

        (buildPath <//> "*." ++ osuf way) %> \obj -> do
            let vanillaObjName = takeFileName obj -<.> "o"
            cDeps <- dependencyList cDepsFile vanillaObjName
            hDeps <- dependencyList hDepsFile obj
            let hSrcDeps = filter ("//*hs" ?==) hDeps

            when (null cDeps && null hDeps) $
                putError_ $ "Cannot determine sources for '" ++ obj ++ "'."

            if null cDeps
            then build $ fullTargetWithWay target hSrcDeps (Ghc stage) way [obj]
            else build $ fullTarget        target cDeps    (Gcc stage)     [obj]
