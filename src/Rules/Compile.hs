module Rules.Compile (compilePackage) where

import Base
import Context
import Expression
import Oracles.Dependencies
import Rules.Actions
import Settings
import Target

compilePackage :: [(Resource, Int)] -> Context -> Rules ()
compilePackage rs context@Context {..} = do
    let path = buildPath context

    path <//> "*" <.> hisuf way %> \hi ->
        if compileInterfaceFilesSeparately
        then do
            (src, deps) <- dependencies path $ hi -<.> osuf way
            need $ src : deps
            buildWithResources rs $ Target context (Ghc stage) [src] [hi]
        else need [ hi -<.> osuf way ]

    path <//> "*" <.> hibootsuf way %> \hiboot ->
        if compileInterfaceFilesSeparately
        then do
            (src, deps) <- dependencies path $ hiboot -<.> obootsuf way
            need $ src : deps
            buildWithResources rs $ Target context (Ghc stage) [src] [hiboot]
        else need [ hiboot -<.> obootsuf way ]

    -- TODO: add dependencies for #include of .h and .hs-incl files (gcc -MM?)
    path <//> "*" <.> osuf way %> \obj -> do
        (src, deps) <- dependencies path obj
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
    path <//> "*" <.> obootsuf way %> \obj -> do
        (src, deps) <- dependencies path obj
        if compileInterfaceFilesSeparately
        then need $ (obj -<.> hibootsuf way) : src : deps
        else need $ src : deps
        buildWithResources rs $ Target context (Ghc stage) [src] [obj]
