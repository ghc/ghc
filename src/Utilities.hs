module Utilities (
    build, buildWithResources, buildWithCmdOptions, runBuilder, runBuilderWith,
    needLibrary, contextDependencies, stage1Dependencies, libraryTargets,
    topsortPackages
    ) where

import qualified Hadrian.Builder as H
import Hadrian.Haskell.Cabal
import Hadrian.Utilities

import Context
import Expression hiding (stage)
import Oracles.PackageData
import Settings
import Target
import UserSettings

build :: Target -> Action ()
build target = H.build target getArgs

buildWithResources :: [(Resource, Int)] -> Target -> Action ()
buildWithResources rs target = H.buildWithResources rs target getArgs

buildWithCmdOptions :: [CmdOption] -> Target -> Action ()
buildWithCmdOptions opts target = H.buildWithCmdOptions opts target getArgs

-- | Given a 'Context' this 'Action' look up the package dependencies and wrap
-- the results in appropriate contexts. The only subtlety here is that we never
-- depend on packages built in 'Stage2' or later, therefore the stage of the
-- resulting dependencies is bounded from above at 'Stage1'. To compute package
-- dependencies we scan package @.cabal@ files, see 'pkgDependencies' defined
-- in "Hadrian.Haskell.Cabal".
contextDependencies :: Context -> Action [Context]
contextDependencies Context {..} = case pkgCabalFile package of
    Nothing        -> return [] -- Non-Cabal packages have no dependencies.
    Just cabalFile -> do
        let depStage   = min stage Stage1
            depContext = \pkg -> Context depStage pkg way
        deps <- pkgDependencies cabalFile
        pkgs <- sort <$> stagePackages depStage
        return . map depContext $ intersectOrd (compare . pkgName) pkgs deps

-- | Lookup dependencies of a 'Package' in the vanilla Stage1 context.
stage1Dependencies :: Package -> Action [Package]
stage1Dependencies =
    fmap (map Context.package) . contextDependencies . vanillaContext Stage1

-- | Given a library 'Package' this action computes all of its targets. See
-- 'packageTargets' for the explanation of the @includeGhciLib@ parameter.
libraryTargets :: Bool -> Context -> Action [FilePath]
libraryTargets includeGhciLib context = do
    confFile <- pkgConfFile        context
    libFile  <- pkgLibraryFile     context
    lib0File <- pkgLibraryFile0    context
    lib0     <- buildDll0          context
    ghciLib  <- pkgGhciLibraryFile context
    ghciFlag <- if includeGhciLib
                then interpretInContext context $ getPkgData BuildGhciLib
                else return "NO"
    let ghci = ghciFlag == "YES" && (stage context == Stage1 || stage1Only)
    return $ [ confFile, libFile ] ++ [ lib0File | lib0 ] ++ [ ghciLib | ghci ]

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
