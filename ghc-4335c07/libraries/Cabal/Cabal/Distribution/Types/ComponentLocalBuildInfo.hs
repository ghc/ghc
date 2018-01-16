{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}

module Distribution.Types.ComponentLocalBuildInfo (
  ComponentLocalBuildInfo(..),
  componentIsIndefinite,
  maybeComponentInstantiatedWith,
  ) where

import Prelude ()
import Distribution.Compat.Prelude
import Distribution.ModuleName

import Distribution.Backpack
import Distribution.Compat.Graph
import Distribution.Types.ComponentId
import Distribution.Types.MungedPackageId
import Distribution.Types.UnitId
import Distribution.Types.ComponentName
import Distribution.Types.MungedPackageName

import Distribution.PackageDescription
import qualified Distribution.InstalledPackageInfo as Installed

-- | The first five fields are common across all algebraic variants.
data ComponentLocalBuildInfo
  = LibComponentLocalBuildInfo {
    -- | It would be very convenient to store the literal Library here,
    -- but if we do that, it will get serialized (via the Binary)
    -- instance twice.  So instead we just provide the ComponentName,
    -- which can be used to find the Component in the
    -- PackageDescription.  NB: eventually, this will NOT uniquely
    -- identify the ComponentLocalBuildInfo.
    componentLocalName :: ComponentName,
    -- | The computed 'ComponentId' of this component.
    componentComponentId :: ComponentId,
    -- | The computed 'UnitId' which uniquely identifies this
    -- component.  Might be hashed.
    componentUnitId :: UnitId,
    -- | Is this an indefinite component (i.e. has unfilled holes)?
    componentIsIndefinite_ :: Bool,
    -- | How the component was instantiated
    componentInstantiatedWith :: [(ModuleName, OpenModule)],
    -- | Resolved internal and external package dependencies for this component.
    -- The 'BuildInfo' specifies a set of build dependencies that must be
    -- satisfied in terms of version ranges. This field fixes those dependencies
    -- to the specific versions available on this machine for this compiler.
    componentPackageDeps :: [(UnitId, MungedPackageId)],
    -- | The set of packages that are brought into scope during
    -- compilation, including a 'ModuleRenaming' which may used
    -- to hide or rename modules.  This is what gets translated into
    -- @-package-id@ arguments.  This is a modernized version of
    -- 'componentPackageDeps', which is kept around for BC purposes.
    componentIncludes :: [(OpenUnitId, ModuleRenaming)],
    componentExeDeps :: [UnitId],
    -- | The internal dependencies which induce a graph on the
    -- 'ComponentLocalBuildInfo' of this package.  This does NOT
    -- coincide with 'componentPackageDeps' because it ALSO records
    -- 'build-tool' dependencies on executables.  Maybe one day
    -- @cabal-install@ will also handle these correctly too!
    componentInternalDeps :: [UnitId],
    -- | Compatibility "package key" that we pass to older versions of GHC.
    componentCompatPackageKey :: String,
    -- | Compatibility "package name" that we register this component as.
    componentCompatPackageName :: MungedPackageName,
    -- | A list of exposed modules (either defined in this component,
    -- or reexported from another component.)
    componentExposedModules :: [Installed.ExposedModule],
    -- | Convenience field, specifying whether or not this is the
    -- "public library" that has the same name as the package.
    componentIsPublic :: Bool
  }
  -- TODO: refactor all these duplicates
  | FLibComponentLocalBuildInfo {
    componentLocalName :: ComponentName,
    componentComponentId :: ComponentId,
    componentUnitId :: UnitId,
    componentPackageDeps :: [(UnitId, MungedPackageId)],
    componentIncludes :: [(OpenUnitId, ModuleRenaming)],
    componentExeDeps :: [UnitId],
    componentInternalDeps :: [UnitId]
  }
  | ExeComponentLocalBuildInfo {
    componentLocalName :: ComponentName,
    componentComponentId :: ComponentId,
    componentUnitId :: UnitId,
    componentPackageDeps :: [(UnitId, MungedPackageId)],
    componentIncludes :: [(OpenUnitId, ModuleRenaming)],
    componentExeDeps :: [UnitId],
    componentInternalDeps :: [UnitId]
  }
  | TestComponentLocalBuildInfo {
    componentLocalName :: ComponentName,
    componentComponentId :: ComponentId,
    componentUnitId :: UnitId,
    componentPackageDeps :: [(UnitId, MungedPackageId)],
    componentIncludes :: [(OpenUnitId, ModuleRenaming)],
    componentExeDeps :: [UnitId],
    componentInternalDeps :: [UnitId]

  }
  | BenchComponentLocalBuildInfo {
    componentLocalName :: ComponentName,
    componentComponentId :: ComponentId,
    componentUnitId :: UnitId,
    componentPackageDeps :: [(UnitId, MungedPackageId)],
    componentIncludes :: [(OpenUnitId, ModuleRenaming)],
    componentExeDeps :: [UnitId],
    componentInternalDeps :: [UnitId]
  }
  deriving (Generic, Read, Show)

instance Binary ComponentLocalBuildInfo

instance IsNode ComponentLocalBuildInfo where
    type Key ComponentLocalBuildInfo = UnitId
    nodeKey = componentUnitId
    nodeNeighbors = componentInternalDeps

componentIsIndefinite :: ComponentLocalBuildInfo -> Bool
componentIsIndefinite LibComponentLocalBuildInfo{ componentIsIndefinite_ = b } = b
componentIsIndefinite _ = False

maybeComponentInstantiatedWith :: ComponentLocalBuildInfo -> Maybe [(ModuleName, OpenModule)]
maybeComponentInstantiatedWith
    LibComponentLocalBuildInfo { componentInstantiatedWith = insts } = Just insts
maybeComponentInstantiatedWith _ = Nothing
