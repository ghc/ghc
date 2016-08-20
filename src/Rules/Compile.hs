module Rules.Compile (compilePackage) where

import Base
import Context
import Expression
import Oracles.Dependencies
import Rules.Actions
import Rules.Generate
import Settings.Paths
import Target

import Development.Shake.Util

import qualified Data.Set as Set

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
            -- TODO: Improve parallelism by collecting all dependencies and
            -- need'ing them all at once
            mapM_  (needGenerated context) . filter ("//*.c" ?==) $ src : deps
            build $ Target context (Cc CompileC stage) [src] [obj]
        else do
            need $ src : deps
            needCompileDependencies context
            buildWithResources rs $ Target context (Ghc CompileHs stage) [src] [obj]

    -- TODO: Get rid of these special cases.
    path <//> "*" <.> obootsuf way %> \obj -> do
        (src, deps) <- fileDependencies context obj
        need $ src : deps
        needCompileDependencies context
        buildWithResources rs $ Target context (Ghc CompileHs stage) [src] [obj]

needCompileDependencies :: Context -> Action ()
needCompileDependencies context@Context {..} = do
    when (isLibrary package) $ need =<< return <$> pkgConfFile context
    needContext =<< contextDependencies context

needGenerated :: Context -> FilePath -> Action ()
needGenerated context origFile = go Set.empty
  where
    go :: Set.Set String -> Action ()
    go done = withTempFile $ \outFile -> do
        let builder = Cc FindMissingInclude $ stage context
            target = Target context builder [origFile] [outFile]
        build target
        deps <- parseFile outFile

        -- Get the full path if the include refers to a generated file and call
        -- `need` on it.
        needed <- liftM catMaybes $
            interpretInContext context (mapM getPathIfGenerated deps)
        need needed

        let newdone = Set.fromList needed `Set.union` done
        -- If we added a new file to the set of needed files, let's try one more
        -- time, since the new file might include a genreated header of itself
        -- (which we'll `need`).
        when (Set.size newdone > Set.size done) (go newdone)

    parseFile :: FilePath -> Action [String]
    parseFile file = do
        input <- liftIO $ readFile file
        case parseMakefile input of
            [(_file, deps)] -> return deps
            _               -> return []

