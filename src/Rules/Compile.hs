{-# LANGUAGE RecordWildCards #-}
module Rules.Compile (compilePackage) where

import Base
import Context
import Expression
import Oracles.Dependencies
import Rules.Actions
import Settings
import Target

compilePackage :: [(Resource, Int)] -> Context -> Rules ()
compilePackage rs context @ (Context {..}) = do
    let buildPath = targetPath stage package -/- "build"

    buildPath <//> "*" <.> hisuf way %> \hi ->
        if compileInterfaceFilesSeparately
        then do
            (src, deps) <- dependencies buildPath $ hi -<.> osuf way
            need $ src : deps
            buildWithResources rs $ Target context (Ghc stage) [src] [hi]
        else need [ hi -<.> osuf way ]

    buildPath <//> "*" <.> hibootsuf way %> \hiboot ->
        if compileInterfaceFilesSeparately
        then do
            (src, deps) <- dependencies buildPath $ hiboot -<.> obootsuf way
            need $ src : deps
            buildWithResources rs $ Target context (Ghc stage) [src] [hiboot]
        else need [ hiboot -<.> obootsuf way ]

    -- TODO: add dependencies for #include of .h and .hs-incl files (gcc -MM?)
    buildPath <//> "*" <.> osuf way %> \obj -> do
        (src, deps) <- dependencies buildPath obj
        if ("//*.c" ?== src)
        then do
            need $ src : deps
            build $ Target context (Gcc stage) [src] [obj]
        else do
            if compileInterfaceFilesSeparately && "//*.hs" ?== src
            then need $ (obj -<.> hisuf way) : src : deps
            else need $ src : deps
            buildWithResources rs $ Target context (Ghc stage) [src] [obj]

    -- TODO: get rid of these special cases
    buildPath <//> "*" <.> obootsuf way %> \obj -> do
        (src, deps) <- dependencies buildPath obj
        if compileInterfaceFilesSeparately
        then need $ (obj -<.> hibootsuf way) : src : deps
        else need $ src : deps
        buildWithResources rs $ Target context (Ghc stage) [src] [obj]
