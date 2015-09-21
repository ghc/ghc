module Rules.Compile (compilePackage) where

import Expression
import Oracles
import Rules.Actions
import Rules.Resources
import Settings

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
        need $ src : deps
        if ("//*.c" ?== src)
        then build $ fullTarget target (Gcc stage) [src] [obj]
        else do
            let way = detectWay obj
            build $ fullTargetWithWay target (Ghc stage) way [src] [obj]

    matchBuildResult buildPath "o-boot" ?> \obj -> do
        (src, deps) <- dependencies buildPath obj
        need $ src : deps
        let way = detectWay obj
        build $ fullTargetWithWay target (Ghc stage) way [src] [obj]
