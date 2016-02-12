{-# LANGUAGE RecordWildCards #-}
module Rules.Compile (compilePackage) where

import Base
import Context
import Expression
import Oracles.Dependencies
import Rules.Actions
import Rules.Resources
import Settings
import Target hiding (context)

-- TODO: Use way from Context, #207
compilePackage :: Resources -> Context -> Rules ()
compilePackage rs context @ (Context {..}) = do
    let buildPath = targetPath stage package -/- "build"

    matchBuildResult buildPath "hi" ?> \hi ->
        if compileInterfaceFilesSeparately && not ("//HpcParser.*" ?== hi)
        then do
            let w = detectWay hi
            (src, deps) <- dependencies buildPath $ hi -<.> osuf w
            need $ src : deps
            buildWithResources [(resPackageDb rs, 1)] $
                Target (context { way = w }) (Ghc stage) [src] [hi]
        else need [ hi -<.> osuf (detectWay hi) ]

    matchBuildResult buildPath "hi-boot" ?> \hiboot ->
        if compileInterfaceFilesSeparately
        then do
            let w = detectWay hiboot
            (src, deps) <- dependencies buildPath $ hiboot -<.> obootsuf w
            need $ src : deps
            buildWithResources [(resPackageDb rs, 1)] $
                Target (context { way = w }) (Ghc stage) [src] [hiboot]
        else need [ hiboot -<.> obootsuf (detectWay hiboot) ]

    -- TODO: add dependencies for #include of .h and .hs-incl files (gcc -MM?)
    matchBuildResult buildPath "o" ?> \obj -> do
        (src, deps) <- dependencies buildPath obj
        if ("//*.c" ?== src)
        then do
            need $ src : deps
            build $ Target context (Gcc stage) [src] [obj]
        else do
            let w = detectWay obj
            if compileInterfaceFilesSeparately && "//*.hs" ?== src && not ("//HpcParser.*" ?== src)
            then need $ (obj -<.> hisuf (detectWay obj)) : src : deps
            else need $ src : deps
            buildWithResources [(resPackageDb rs, 1)] $
                Target (context { way = w }) (Ghc stage) [src] [obj]

    -- TODO: get rid of these special cases
    matchBuildResult buildPath "o-boot" ?> \obj -> do
        (src, deps) <- dependencies buildPath obj
        let w = detectWay obj
        if compileInterfaceFilesSeparately
        then need $ (obj -<.> hibootsuf (detectWay obj)) : src : deps
        else need $ src : deps
        buildWithResources [(resPackageDb rs, 1)] $
            Target (context { way = w }) (Ghc stage) [src] [obj]
