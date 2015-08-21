module Rules.Compile (compilePackage) where

import Way
import Base
import Util
import Builder
import Target (PartialTarget (..), fullTarget, fullTargetWithWay)
import Oracles.Dependencies
import Settings.TargetDirectory
import Rules.Actions
import Rules.Resources

compilePackage :: Resources -> PartialTarget -> Rules ()
compilePackage _ target @ (PartialTarget stage package) = do
    let path      = targetPath stage package
        buildPath = path -/- "build"

    matchBuildResult buildPath "hi" ?> \hi ->
        need [ hi -<.> osuf (detectWay hi) ]

    matchBuildResult buildPath "hi-boot" ?> \hiboot ->
        need [ hiboot -<.> obootsuf (detectWay hiboot) ]

    matchBuildResult buildPath "o" ?> \obj -> do
        (src, deps) <- dependencies buildPath obj
        need deps
        if ("//*.c" ?== src)
        then build $ fullTarget target (Gcc stage) [src] [obj]
        else do
            let way = detectWay obj
            build $ fullTargetWithWay target (Ghc stage) way [src] [obj]

    matchBuildResult buildPath "o-boot" ?> \obj -> do
        (src, deps) <- dependencies buildPath obj
        need deps
        let way = detectWay obj
        build $ fullTargetWithWay target (Ghc stage) way [src] [obj]
