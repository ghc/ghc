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

    path <//> "*" <.> hisuf way %> \hi -> need [ hi -<.> osuf way ]

    path <//> "*" <.> hibootsuf way %> \hiboot -> need [ hiboot -<.> obootsuf way ]

    -- TODO: add dependencies for #include of .h and .hs-incl files (gcc -MM?)
    path <//> "*" <.> osuf way %> \obj -> do
        (src, deps) <- dependencies path obj
        if ("//*.c" ?== src)
        then do
            need $ src : deps
            build $ Target context (Cc Compile stage) [src] [obj]
        else do
            need $ src : deps
            buildWithResources rs $ Target context (Ghc Compile stage) [src] [obj]

    -- TODO: get rid of these special cases
    path <//> "*" <.> obootsuf way %> \obj -> do
        (src, deps) <- dependencies path obj
        need $ src : deps
        buildWithResources rs $ Target context (Ghc Compile stage) [src] [obj]
