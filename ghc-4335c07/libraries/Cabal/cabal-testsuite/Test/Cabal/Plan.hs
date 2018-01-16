{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | Utilities for understanding @plan.json@.
module Test.Cabal.Plan (
    Plan,
    planDistDir,
) where

import Distribution.Text
import Distribution.Types.ComponentName
import Distribution.Package
import qualified Data.Text as Text
import Data.Aeson
import Data.Aeson.Types
import Control.Monad

-- TODO: index this
data Plan = Plan { planInstallPlan :: [InstallItem] }

data InstallItem
    = APreExisting
    | AConfiguredGlobal
    | AConfiguredInplace ConfiguredInplace

-- local or inplace package
data ConfiguredInplace = ConfiguredInplace
    { configuredInplaceDistDir       :: FilePath
    , configuredInplacePackageName   :: PackageName
    , configuredInplaceComponentName :: Maybe ComponentName }

instance FromJSON Plan where
    parseJSON (Object v) = fmap Plan (v .: "install-plan")
    parseJSON invalid = typeMismatch "Plan" invalid

instance FromJSON InstallItem where
    parseJSON obj@(Object v) = do
        t <- v .: "type"
        case t :: String of
            "pre-existing" -> return APreExisting
            "configured"   -> do
                s <- v .: "style"
                case s :: String of
                  "global"  -> return AConfiguredGlobal
                  "inplace" -> AConfiguredInplace `fmap` parseJSON obj
                  "local"   -> AConfiguredInplace `fmap` parseJSON obj
                  _         -> fail $ "unrecognized value of 'style' field: " ++ s
            _              -> fail "unrecognized value of 'type' field"
    parseJSON invalid = typeMismatch "InstallItem" invalid

instance FromJSON ConfiguredInplace where
    parseJSON (Object v) = do
        dist_dir <- v .: "dist-dir"
        pkg_name <- v .: "pkg-name"
        component_name <- v .:? "component-name"
        return (ConfiguredInplace dist_dir pkg_name component_name)
    parseJSON invalid = typeMismatch "ConfiguredInplace" invalid

instance FromJSON PackageName where
    parseJSON (String t) = return (mkPackageName (Text.unpack t))
    parseJSON invalid = typeMismatch "PackageName" invalid

instance FromJSON ComponentName where
    parseJSON (String t) =
        case simpleParse s of
            Nothing -> fail ("could not parse component-name: " ++ s)
            Just r  -> return r
      where s = Text.unpack t
    parseJSON invalid = typeMismatch "ComponentName" invalid

planDistDir :: Plan -> PackageName -> ComponentName -> FilePath
planDistDir plan pkg_name cname =
    case concatMap p (planInstallPlan plan) of
        [x] -> x
        []  -> error $ "planDistDir: component " ++ display cname
                    ++ " of package " ++ display pkg_name ++ " either does not"
                    ++ " exist in the install plan or does not have a dist-dir"
        _   -> error $ "planDistDir: found multiple copies of component " ++ display cname
                    ++ " of package " ++ display pkg_name ++ " in install plan"
  where
    p APreExisting      = []
    p AConfiguredGlobal = []
    p (AConfiguredInplace conf) = do
        guard (configuredInplacePackageName conf == pkg_name)
        guard $ case configuredInplaceComponentName conf of
                    Nothing     -> True
                    Just cname' -> cname == cname'
        return (configuredInplaceDistDir conf)
