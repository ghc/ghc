module Rules.Compile (compilePackage) where

import Base
import Context
import Expression
import Oracles.Dependencies
import Rules.Actions
import Settings.Paths
import Target

compilePackage :: [(Resource, Int)] -> Context -> Rules ()
compilePackage rs context@Context {..} = do
    let path = buildPath context

    path <//> "*" <.> hisuf way %> \hi -> need [ hi -<.> osuf way ]

    path <//> "*" <.> hibootsuf way %> \hiboot -> need [ hiboot -<.> obootsuf way ]

    -- TODO: Add dependencies for #include of .h and .hs-incl files (gcc -MM?).
    path <//> "*" <.> osuf way %> \obj -> do
        (src, deps) <- fileDependencies context obj
        if ("//*.c" ?== src)
        then do
            need $ src : deps
            build $ Target context (Cc Compile stage) [src] [obj]
        else do
            need $ src : deps
            needCompileDependencies context
            buildWithResources rs $ Target context (Ghc Compile stage) [src] [obj]

    -- TODO: Get rid of these special cases.
    path <//> "*" <.> obootsuf way %> \obj -> do
        (src, deps) <- fileDependencies context obj
        need $ src : deps
        needCompileDependencies context
        buildWithResources rs $ Target context (Ghc Compile stage) [src] [obj]

needCompileDependencies :: Context -> Action ()
needCompileDependencies context@Context {..} = do
    when (isLibrary package) $ need =<< return <$> pkgConfFile context
    needContext =<< contextDependencies context
