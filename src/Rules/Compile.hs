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
    let path            = buildPath context
        nonHs extension = path </> extension <//> "*" <.> osuf way
        compile compiler obj2src obj = do
            let depFile = obj -<.> "d"
                src     = obj2src context obj
            need [src]
            needGenerated context src
            build $ Target context (Cc FindCDependencies stage) [src] [depFile]
            needMakefileDependencies depFile -- TODO: Is this actually needed?
            build $ Target context (compiler stage) [src] [obj]
        compileHs = \[obj, _] -> do
            (src, deps) <- fileDependencies context obj
            need $ src : deps
            when (isLibrary package) $ need =<< return <$> pkgConfFile context
            needContext =<< contextDependencies context
            buildWithResources rs $ Target context (Ghc CompileHs stage) [src] [obj]

    priority 2.0 $ do
        nonHs "c"   %> compile (Cc  CompileC ) (obj2src "c"   isGeneratedCFile  )
        nonHs "cmm" %> compile (Ghc CompileHs) (obj2src "cmm" isGeneratedCmmFile)
        nonHs "s"   %> compile (Ghc CompileHs) (obj2src "S"   $ const False     )

    -- TODO: Add dependencies for #include of .h and .hs-incl files (gcc -MM?).
    [ path <//> "*" <.> suf way | suf <- [    osuf,     hisuf] ] &%> compileHs
    [ path <//> "*" <.> suf way | suf <- [obootsuf, hibootsuf] ] &%> compileHs

-- TODO: Simplify.
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

obj2src :: String -> (FilePath -> Bool) -> Context -> FilePath -> FilePath
obj2src extension isGenerated context@Context {..} obj
    | isGenerated src = src
    | otherwise       = pkgPath package ++ suffix
  where
    src    = obj -<.> extension
    suffix = fromMaybe ("Cannot determine source for " ++ obj)
           $ stripPrefix (buildPath context -/- extension) src
