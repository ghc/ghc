
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module GHC.Iface.Errors.Types (

    MissingInterfaceError(..)
  , InterfaceLookingFor(..)
  , IfaceMessage(..)
  , ReadInterfaceError(..)
  , CantFindInstalled(..)
  , CantFindInstalledReason(..)
  , FindingModuleOrInterface(..)

  , BuildingCabalPackage(..)

  , IfaceMessageOpts(..)

  ) where

import GHC.Prelude

import GHC.Types.Name (Name)
import GHC.Types.TyThing (TyThing)
import GHC.Unit.Types (Module, InstalledModule, UnitId, Unit)
import GHC.Unit.State (UnitState, ModuleSuggestion, ModuleOrigin, UnusableUnit, UnitInfo)
import GHC.Exception.Type (SomeException)
import GHC.Unit.Types ( IsBootInterface )
import Language.Haskell.Syntax.Module.Name ( ModuleName )


import GHC.Generics ( Generic )
import GHC.Unit.Module.Location

data IfaceMessageOpts = IfaceMessageOpts { ifaceShowTriedFiles :: !Bool -- ^ Whether to show files we tried to look for or not when printing loader errors
                                         , ifaceBuildingCabalPackage :: !BuildingCabalPackage
                                         }

data InterfaceLookingFor
  = LookingForName   !Name
  | LookingForHiBoot !Module
  | LookingForModule !ModuleName !IsBootInterface
  | LookingForSig    !InstalledModule

data IfaceMessage
  = Can'tFindInterface
      MissingInterfaceError
      InterfaceLookingFor
  | Can'tFindNameInInterface
      Name
      [TyThing] -- possibly relevant TyThings
  | CircularImport !Module
  deriving Generic

data MissingInterfaceError
  = BadSourceImport !Module
  | HomeModError !InstalledModule !ModLocation
  | DynamicHashMismatchError !Module !ModLocation

  | CantFindErr !UnitState FindingModuleOrInterface CantFindInstalled

  | BadIfaceFile ReadInterfaceError
  | FailedToLoadDynamicInterface Module ReadInterfaceError
    deriving Generic

data ReadInterfaceError
  = ExceptionOccurred        FilePath SomeException
  | HiModuleNameMismatchWarn FilePath Module Module
  deriving Generic

data CantFindInstalledReason
  = NoUnitIdMatching UnitId [UnitInfo]
  | MissingPackageFiles UnitId [FilePath]
  | MissingPackageWayFiles String UnitId [FilePath]
  | ModuleSuggestion [ModuleSuggestion] [FilePath]
  | NotAModule
  | CouldntFindInFiles [FilePath]
  | GenericMissing
      [(Unit, Maybe UnitInfo)] [Unit]
      [UnusableUnit] [FilePath]
  | MultiplePackages [(Module, ModuleOrigin)]
  deriving Generic

data CantFindInstalled =
  CantFindInstalled ModuleName CantFindInstalledReason
  deriving Generic
data FindingModuleOrInterface = FindingModule
                              | FindingInterface

-- | Pass to a 'DriverMessage' the information whether or not the
-- '-fbuilding-cabal-package' flag is set.
data BuildingCabalPackage
  = YesBuildingCabalPackage
  | NoBuildingCabalPackage
  deriving Eq
