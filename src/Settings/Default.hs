module Settings.Default (
    defaultSplitObjects, defaultTargetDirectory, defaultProgramPath
    ) where

import Base
import Expression
import GHC
import Oracles.Config.Flag
import Predicates (notStage0)

-- | GHC build results will be placed into target directories with the
-- following typical structure:

-- * @build/@ contains compiled object code
-- * @doc/@ is produced by haddock
-- * @package-data.mk@ contains output of ghc-cabal applied to pkgCabal
defaultTargetDirectory :: Stage -> Package -> FilePath
defaultTargetDirectory stage _ = stageString stage

-- TODO: move to buildRootPath, see #113
-- TODO: simplify, add programInplaceLibPath
-- | The relative path to the program executable
defaultProgramPath :: Stage -> Package -> Maybe FilePath
defaultProgramPath stage pkg
    | pkg == ghc = Just . inplaceProgram $ "ghc-stage" ++ show (fromEnum stage + 1)
    | pkg == haddock || pkg == ghcTags = case stage of
        Stage2 -> Just . inplaceProgram $ pkgNameString pkg
        _      -> Nothing
    | pkg `elem` [touchy, unlit] = case stage of
        Stage0 -> Just $ "inplace/lib/bin" -/- pkgNameString pkg <.> exe
        _      -> Nothing
    | isProgram pkg = case stage of
        Stage0 -> Just . inplaceProgram $ pkgNameString pkg
        _      -> Just . installProgram $ pkgNameString pkg
    | otherwise = Nothing
  where
    inplaceProgram name = programInplacePath -/- name <.> exe
    installProgram name = pkgPath pkg -/- defaultTargetDirectory stage pkg
                                      -/- "build/tmp" -/- name <.> exe

defaultSplitObjects :: Predicate
defaultSplitObjects = do
    goodStage <- notStage0 -- We don't split bootstrap (stage 0) packages
    pkg       <- getPackage
    supported <- lift supportsSplitObjects
    let goodPackage = isLibrary pkg && pkg /= compiler && pkg /= rts
    return $ goodStage && goodPackage && supported
