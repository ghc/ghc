{-# LANGUAGE TupleSections #-}

module Settings (
    getArgs, getLibraryWays, getRtsWays, flavour, knownPackages,
    findPackageByName, unsafeFindPackageByName, unsafeFindPackageByPath,
    isLibrary, stagePackages, getBignumBackend, getBignumCheck, completeSetting,
    flavourFileRules
    ) where

import CommandLine
import Expression
import Flavour
import Packages
import Settings.Parser
import UserSettings (userFlavours, userPackages, userDefaultFlavour)

import {-# SOURCE #-} Settings.Default
import Settings.Flavours.Benchmark
import Settings.Flavours.Development
import Settings.Flavours.GhcInGhci
import Settings.Flavours.Performance
import Settings.Flavours.Quick
import Settings.Flavours.Quickest
import Settings.Flavours.QuickCross
import Settings.Flavours.Validate


getArgs :: Args
getArgs = expr flavour >>= args

getLibraryWays :: Ways
getLibraryWays = expr flavour >>= libraryWays

getRtsWays :: Ways
getRtsWays = expr flavour >>= rtsWays

getBignumBackend :: Expr String
getBignumBackend = expr $ cmdBignum >>= \case
   Nothing -> bignumBackend <$> flavour
   Just b  -> pure b

getBignumCheck :: Expr Bool
getBignumCheck = expr $ cmdBignum >>= \case
   Nothing -> bignumCheck <$> flavour
   Just _  -> cmdBignumCheck

stagePackages :: Stage -> Action [Package]
stagePackages stage = do
    f <- flavour
    packages f stage

hadrianFlavours :: [Flavour]
hadrianFlavours =
    [ benchmarkFlavour, defaultFlavour, developmentFlavour Stage1
    , developmentFlavour Stage2, performanceFlavour
    , quickFlavour, quickValidateFlavour, quickDebugFlavour
    , quickestFlavour
    , quickCrossFlavour
    , ghcInGhciFlavour, validateFlavour, slowValidateFlavour
    ]

{-
Note [Persisting the flavour]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
For a long time Hadrian relied on UserSettings.hs for all configuration.
However, this approach to configuration is both inconvenient and interacts
poorly with multiple build roots. For this reason we introduced flavour
transformers, making it easier to modify the flavour from the command-line.
However, passing the flavour on the command-line introduces another problem:
the user must consistently pass the same --flavour flag to all Hadrian
invocations on a particular build root. Forgetting even once can result in a
full rebuild of the tree.

To eliminate the inconvenience of needing to always pass --flavour arguments we
persist the flavour name used to build a particular build root in the build
root itself (specifically, in `<build root>/flavour`). We refer to this file
when looking up the active flavour, defaulting to it if the user does not
specify a flavour name explicitly.
-}

flavourFileRules :: Rules ()
flavourFileRules = do
    root <- buildRootRules
    root -/- "flavour" %> \out -> do
        flavourName <- getFlavourName
        writeFile' out flavourName

    -- Ensure that the persistent flavour file is always updated.
    want [root -/- "flavour"]

-- | Lookup the requested flavour name, referring to the persistent flavour
-- file saved from the previous Hadrian execution if not specified.
getFlavourName :: Action String
getFlavourName = do
    root <- buildRoot
    let flavourFile = root -/- "flavour"
    mbFlavourName <- cmdFlavour
    case mbFlavourName of
      Nothing -> do exists <- doesFileExist flavourFile
                    if exists
                      then liftIO $ readFile flavourFile
                      else return userDefaultFlavour
      Just nm -> return nm

-- | This action looks up a flavour with the name given on the
--   command line with @--flavour@, defaulting to 'userDefaultFlavour'
--   when no explicit @--flavour@ is passed. It then applies any
--   potential setting update specified on the command line or in a
--   <build root>/hadrian.settings file, using @k = v@ or @k += v@ style
--   syntax. See Note [Hadrian settings] at the bottom of this file.
flavour :: Action Flavour
flavour = do
    flavourName <- getFlavourName
    kvs <- userSetting ([] :: [KeyVal])
    let flavours = hadrianFlavours ++ userFlavours
        (settingErrs, tweak) = applySettings kvs

    when (not $ null settingErrs) $ fail
      $ "failed to apply key-value settings:" ++ unlines (map (" - " ++) settingErrs)

    case parseFlavour flavours flavourTransformers flavourName of
      Left err -> fail err
      Right f -> return $ tweak f

-- TODO: switch to Set Package as the order of packages should not matter?
-- Otherwise we have to keep remembering to sort packages from time to time.
knownPackages :: [Package]
knownPackages = sort $ ghcPackages ++ userPackages

-- TODO: Speed up? Switch to Set?
-- Note: this is slow but we keep it simple as there are just ~50 packages
findPackageByName :: PackageName -> Maybe Package
findPackageByName name = find (\pkg -> pkgName pkg == name) knownPackages

unsafeFindPackageByName :: PackageName -> Package
unsafeFindPackageByName name = fromMaybe (error msg) $ findPackageByName name
  where
    msg = "unsafeFindPackageByName: No package with name " ++ name

unsafeFindPackageByPath :: FilePath -> Package
unsafeFindPackageByPath path = err $ find (\pkg -> pkgPath pkg == path) knownPackages
  where
    err = fromMaybe $ error ("findPackageByPath: No package for path " ++ path)
