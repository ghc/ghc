module Rules.SimpleTargets
  ( simplePackageTargets
  , completionRule
  ) where

import Base
import CommandLine
import Context
import Packages
import Settings

import Data.Foldable

-- | Simple aliases for library and executable targets.
--
--  - @stage<N>:lib:<name>@ will build library @name@ with
--    the stage N compiler, putting the result under
--    @<build root>/stage<N>/lib@.
--  - @stage<N>:exe:<name>@ will build executable @name@
--    with the stage N-1 compiler, putting the result under
--    @<build root>/stage<N-1>/bin.
simplePackageTargets :: Rules ()
simplePackageTargets = traverse_ simpleTarget targets

  where targets = [ (stage, target)
                  | stage <- allStages
                  , target <- knownPackages
                  ]

simpleTarget :: (Stage, Package) -> Rules ()
simpleTarget (stage, target) = do
  root <- buildRootRules

  let tgt = intercalate ":" [stagestr, typ, pkgname]
  tgt ~> do
    p <- getTargetPath stage target
    need [ p ]
    -- build the _build/ghc-stageN wrappers if this is the ghc package
    if target == Packages.ghc
      then need [ root -/- ("ghc-" <> stagestr) ]
      else pure ()

  where typ = if isLibrary target then "lib" else "exe"
        stagestr = stageString stage
        pkgname = pkgName target

getTargetPath :: Stage -> Package -> Action FilePath
getTargetPath stage pkg
  | isLibrary pkg = getLibraryPath stage pkg
  | otherwise     = getProgramPath stage pkg

getLibraryPath :: Stage -> Package -> Action FilePath
getLibraryPath stage pkg = pkgConfFile (vanillaContext stage pkg)

getProgramPath :: Stage -> Package -> Action FilePath
getProgramPath stage pkg =
  case stage of
    (Stage0 GlobalLibs) -> error "Can't build executable in stageBoot"
    (Stage0 InTreeLibs) -> programPath (vanillaContext stage0Boot pkg)
    s -> programPath (vanillaContext (predStage s) pkg)


-- | A phony @autocomplete@ rule that prints all valid setting keys
--   completions of the value specified in the @--complete-setting=...@ flag,
--   or simply all valid setting keys if no such argument is passed to Hadrian.
--
--   It is based on the 'completeSetting' function, from the "Settings" module.
completionRule :: Rules ()
completionRule =
  "autocomplete" ~> do
    partialStr <- fromMaybe "" <$> cmdCompleteSetting
    case completeSetting (splitOn "." partialStr) of
      [] -> fail $ "No valid completion found for " ++ partialStr
      cs -> forM_ cs $ \ks ->
        liftIO . putStrLn $ intercalate "." ks
