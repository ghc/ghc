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

compilePackage :: Resources -> StagePackageTarget -> Rules ()
compilePackage _ target = do
    let stage     = Target.stage target
        pkg       = Target.package target
        path      = targetPath stage pkg
        buildPath = path -/- "build"
        cDepsFile = buildPath -/- "c.deps"
        hDepsFile = buildPath -/- "haskell.deps"

    matchBuildResult buildPath "hi" ?> \hi ->
        need [ hi -<.> osuf (detectWay hi) ]

    matchBuildResult buildPath "hi-boot" ?> \hiboot ->
        need [ hiboot -<.> obootsuf (detectWay hiboot) ]

    matchBuildResult buildPath "o" ?> \obj -> do
        cDeps <- dependencyList cDepsFile (takeFileName obj -<.> "o")
        if not (null cDeps)
        then do -- obj is produced from a C source file
            need cDeps
            build $ fullTarget target cDeps (Gcc stage) [obj]
        else do -- obj is produced from a Haskell source file
            hDeps <- dependencyList hDepsFile obj
            when (null hDeps) . putError $
                "No dependencies found for '" ++ obj ++ "'."
            let way  = detectWay obj
                hSrc = head hDeps
            unless ("//*hs" ?== hSrc) . putError $
                "No Haskell source file found for '" ++ obj ++ "'."
            need hDeps
            build $ fullTargetWithWay target [hSrc] (Ghc stage) way [obj]

    matchBuildResult buildPath "o-boot" ?> \obj -> do
        hDeps <- dependencyList hDepsFile obj
        when (null hDeps) . putError $
            "No dependencies found for '" ++ obj ++ "'."
        let way  = detectWay obj
            hSrc = head hDeps
        unless ("//*.hs-boot" ?== hSrc) . putError $
            "No Haskell source file found for '" ++ obj ++ "'."
        need hDeps
        build $ fullTargetWithWay target [hSrc] (Ghc stage) way [obj]
