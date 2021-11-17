{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies  #-}

module Settings (
    getExtraArgs, getArgs, getLibraryWays, getRtsWays, flavour, knownPackages,
    findPackageByName, unsafeFindPackageByName, unsafeFindPackageByPath,
    isLibrary, stagePackages, getBignumBackend, getBignumCheck, completeSetting,
    queryBuildTarget, queryHostTarget, queryTargetTarget,
    queryBuild, queryHost, queryTarget,
    queryArch, queryOS, queryVendor,
    flavourFileRules
    ) where

import CommandLine
import Expression
import Flavour
import Packages
import Settings.Parser
import UserSettings (userFlavours, userPackages, userDefaultFlavour)

import {-# SOURCE #-} Settings.Default
import Settings.Flavours.Development
import Settings.Flavours.GhcInGhci
import Settings.Flavours.Performance
import Settings.Flavours.Quick
import Settings.Flavours.Quickest
import Settings.Flavours.QuickCross
import Settings.Flavours.Validate
import Settings.Flavours.Release

import Hadrian.Oracles.TextFile
import GHC.Toolchain.Target
import GHC.Platform.ArchOS
import Oracles.Setting (isJsTarget)

getExtraArgs :: Args
getExtraArgs = expr flavour >>= extraArgs

getArgs :: Args
getArgs = mconcat [ defaultBuilderArgs, defaultPackageArgs, getExtraArgs ]

getLibraryWays :: Ways
getLibraryWays = expr flavour >>= libraryWays

getRtsWays :: Ways
getRtsWays = expr flavour >>= rtsWays

getBignumBackend :: Expr String
getBignumBackend = bignumBackend <$> expr flavour

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
    [ defaultFlavour, developmentFlavour Stage1
    , developmentFlavour Stage2, performanceFlavour
    , releaseFlavour
    , quickFlavour, quickValidateFlavour, quickDebugFlavour
    , quickestFlavour
    , quickCrossFlavour
    , ghcInGhciFlavour, validateFlavour, slowValidateFlavour
    ]

{-
Note [Persisting the flavour]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Passing the flavour on the command-line means the user must consistently pass
the same --flavour flag to all Hadrian invocations on a particular build root.
Forgetting even once can result in a full rebuild of the tree.

To avoid this we persist the flavour name in the build root itself
(specifically, in `<build root>/flavour`). We refer to this file when looking
up the active flavour, defaulting to it if the user does not specify a flavour
name explicitly.
-}

-- | Oracle query type for the active flavour name.
-- See Note [Persisting the flavour].
newtype FlavourName = FlavourName ()
    deriving (Binary, Eq, Hashable, NFData, Show)
type instance RuleResult FlavourName = String

flavourFileRules :: Rules ()
flavourFileRules = do
    root <- buildRootRules

    -- Cache the flavour name lookup for the whole Shake run.
    -- alwaysRerun ensures the oracle re-executes on every new invocation so
    -- that a changed --flavour flag is never ignored due to a stale cross-run
    -- cache entry (CLI args are not tracked as Shake dependencies).
    -- See Note [Persisting the flavour].
    void $ addOracleCache $ \(FlavourName _) -> do
        alwaysRerun
        let flavourFile = root -/- "flavour"
        cmdFlavour >>= \case
          Just nm -> do
            putBuild $ "Flavour: " ++ nm
            liftIO $ writeFile flavourFile nm
            return nm
          Nothing -> do
            exists <- doesFileExist flavourFile
            if exists
              then do nm <- liftIO $ readFile flavourFile
                      putBuild $ "Flavour: " ++ nm ++ " (cached from last build, override by passing --flavour=... explicitly)"
                      return nm
              else return userDefaultFlavour

-- | Lookup the requested flavour name, referring to the persistent flavour
-- file saved from the previous Hadrian execution if not specified.
-- The result is cached for the whole Shake run via the 'FlavourName' oracle.
getFlavourName :: Action String
getFlavourName = askOracle (FlavourName ())

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

    -- This used to be a command line argument, and is now a flavour transformer.
    uid_hash_cmd <- cmdUnitIdHash
    let flagsTweak = if uid_hash_cmd then enableHashUnitIds else id

    when (not $ null settingErrs) $ fail
      $ "failed to apply key-value settings:\n\t" ++ unlines (map (" - " ++) settingErrs) ++
        "\t   Entries should look something like \"stage1.containers.ghc.hs.opts += -Werror\""

    -- Handle --bignum (deprecated) and JS auto-detection
    bignumTweak <- cmdBignum >>= \case
        Just "native"  -> return useNativeBignum
        Just b         -> return $ \f -> f { bignumBackend = b }
        Nothing        -> do
          js <- isJsTarget
          return $ if js then useNativeBignum else id

    case parseFlavour flavours flavourTransformers flavourName of
      Left err -> fail err
      Right f -> return $ bignumTweak (flagsTweak (tweak f))

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

-- * Combinators for querying configuration defined in the toolchain
--
-- Be careful querying values from the HOST and BUILD targets until the targets
-- are only generated by ghc-toolchain:
-- See Note [The dummy values in the HOST target description]
queryBuild, queryHost, queryTarget :: (Target -> a) -> Expr a
queryBuild  f = expr $ queryBuildTarget f
queryHost   f = expr $ queryHostTarget f
queryTarget f = expr $ queryTargetTarget f
queryArch, queryOS, queryVendor :: Target -> String
queryArch = stringEncodeArch . archOS_arch . tgtArchOs
queryOS   = stringEncodeOS . archOS_OS . tgtArchOs
queryVendor = fromMaybe "" . tgtVendor
