module Rules.Compile (compilePackage) where

import Hadrian.Oracles.TextFile

import Base
import Context
import Expression
import Rules.Generate
import Target
import Utilities

compilePackage :: [(Resource, Int)] -> Context -> Rules ()
compilePackage rs context@Context {..} = do
    let dir             = "//" ++ contextDir context
        nonHs extension = dir -/- extension <//> "*" <.> osuf way
        compile compiler obj2src obj = do
            src <- obj2src context obj
            need [src]
            needDependencies context src $ obj <.> "d"
            buildWithResources rs $ target context (compiler stage) [src] [obj]
        compileHs = \[obj, _hi] -> do
            path <- buildPath context
            (src, deps) <- lookupDependencies (path -/- ".dependencies") obj
            need $ src : deps
            buildWithResources rs $ target context (Ghc CompileHs stage) [src] [obj]

    priority 2.0 $ do
        nonHs "c"   %> compile (Ghc CompileCWithGhc) (obj2src "c"   isGeneratedCFile  )
        nonHs "cmm" %> compile (Ghc CompileHs)       (obj2src "cmm" isGeneratedCmmFile)
        nonHs "s"   %> compile (Ghc CompileHs)       (obj2src "S"   $ const False     )

    -- TODO: Add dependencies for #include of .h and .hs-incl files (gcc -MM?).
    [ dir <//> "*" <.> suf way | suf <- [    osuf,     hisuf] ] &%> compileHs
    [ dir <//> "*" <.> suf way | suf <- [obootsuf, hibootsuf] ] &%> compileHs

-- | Discover dependencies of a given source file by iteratively calling @gcc@
-- in the @-MM -MG@ mode and building generated dependencies if they are missing
-- until reaching a fixed point.
needDependencies :: Context -> FilePath -> FilePath -> Action ()
needDependencies context@Context {..} src depFile = discover
  where
    discover = do
        build $ target context (Cc FindCDependencies stage) [src] [depFile]
        deps <- parseFile depFile
        -- Generated dependencies, if not yet built, will not be found and hence
        -- will be referred to simply by their file names.
        let notFound = filter (\file -> file == takeFileName file) deps
        -- We find the full paths to generated dependencies, so we can request
        -- to build them by calling 'need'.
        todo <- catMaybes <$> mapM (fullPathIfGenerated context) notFound

        if null todo
        then need deps -- The list of dependencies is final, need all
        else do
            need todo  -- Build newly discovered generated dependencies
            discover   -- Continue the discovery process

    parseFile :: FilePath -> Action [String]
    parseFile file = do
        input <- liftIO $ readFile file
        case parseMakefile input of
            [(_file, deps)] -> return deps
            _               -> return []

-- | Find a given 'FilePath' in the list of generated files in the given
-- 'Context' and return its full path.
fullPathIfGenerated :: Context -> FilePath -> Action (Maybe FilePath)
fullPathIfGenerated context file = interpretInContext context $ do
    generated <- generatedDependencies
    return $ find ((== file) . takeFileName) generated

obj2src :: String -> (FilePath -> Bool) -> Context -> FilePath -> Action FilePath
obj2src extension isGenerated context@Context {..} obj
    | isGenerated src = return src
    | otherwise       = (pkgPath package ++) <$> suffix
  where
    src    = obj -<.> extension
    suffix = do
        path <- buildPath context
        return $ fromMaybe ("Cannot determine source for " ++ obj)
               $ stripPrefix (path -/- extension) src
