module Utilities (
    build, buildWithResources, buildWithCmdOptions,
    askWithResources,
    runBuilder, runBuilderWith,
    needLibrary, contextDependencies, stage1Dependencies, libraryTargets,
    topsortPackages, cabalDependencies
    ) where

import qualified Hadrian.Builder as H
import Hadrian.Haskell.Cabal
import Hadrian.Haskell.Cabal.PackageData as PD
import Hadrian.Utilities

import Context
import Expression hiding (stage)
import Settings
import Target

build :: Target -> Action ()
build target = H.build target getArgs

buildWithResources :: [(Resource, Int)] -> Target -> Action ()
buildWithResources rs target = H.buildWithResources rs target getArgs

buildWithCmdOptions :: [CmdOption] -> Target -> Action ()
buildWithCmdOptions opts target = H.buildWithCmdOptions opts target getArgs

askWithResources :: [(Resource, Int)] -> Target -> Action String
askWithResources rs target = H.askWithResources rs target getArgs

-- TODO: Cache the computation.
-- | Given a 'Context' this 'Action' looks up the package dependencies and wraps
-- the results in appropriate contexts. The only subtlety here is that we never
-- depend on packages built in 'Stage2' or later, therefore the stage of the
-- resulting dependencies is bounded from above at 'Stage1'. To compute package
-- dependencies we transitively scan @.cabal@ files using 'pkgDependencies'
-- defined in "Hadrian.Haskell.Cabal".
contextDependencies :: Context -> Action [Context]
contextDependencies ctx@Context {..} = do
    depPkgs <- go [package]
    return [ Context depStage pkg way | pkg <- depPkgs, pkg /= package ]
  where
    depStage = min stage Stage1
    go pkgs  = do
        deps <- concatMapM step pkgs
        let newPkgs = nubOrd $ sort (deps ++ pkgs)
        if pkgs == newPkgs then return pkgs else go newPkgs
    step pkg = pkgDependencies (ctx { Context.package = pkg }) >>= \case
        Nothing -> return [] -- Non-Cabal packages have no dependencies.
        Just deps -> do
            active <- sort <$> stagePackages depStage
            return $ intersectOrd (compare . pkgName) active deps

cabalDependencies :: Context -> Action [String]
cabalDependencies ctx = interpretInContext ctx $ getPackageData PD.depIpIds

-- | Lookup dependencies of a 'Package' in the vanilla Stage1 context.
stage1Dependencies :: Package -> Action [Package]
stage1Dependencies =
    fmap (map Context.package) . contextDependencies . vanillaContext Stage1

-- | Given a library 'Package' this action computes all of its targets. See
-- 'packageTargets' for the explanation of the @includeGhciLib@ parameter.
libraryTargets :: Bool -> Context -> Action [FilePath]
libraryTargets includeGhciLib context = do
    libFile  <- pkgLibraryFile     context
    ghciLib  <- pkgGhciLibraryFile context
    ghci     <- if includeGhciLib
                then interpretInContext context $ getPackageData PD.buildGhciLib
                else return False
    return $ [ libFile ] ++ [ ghciLib | ghci ]

-- | Coarse-grain 'need': make sure all given libraries are fully built.
needLibrary :: [Context] -> Action ()
needLibrary cs = need =<< concatMapM (libraryTargets True) cs

-- HACK (izgzhen), see https://github.com/snowleopard/hadrian/issues/344.
-- | Topological sort of packages according to their dependencies.
topsortPackages :: [Package] -> Action [Package]
topsortPackages pkgs = do
    elems <- mapM (\p -> (p,) <$> stage1Dependencies p) pkgs
    return $ map fst $ topSort elems
  where
    annotateInDeg es e =
     (foldr (\e' s -> if fst e' `elem` snd e then s + 1 else s) (0 :: Int) es, e)
    topSort [] = []
    topSort es =
      let annotated = map (annotateInDeg es) es
          inDegZero = map snd $ filter ((== 0). fst) annotated
      in  inDegZero ++ topSort (es \\ inDegZero)
