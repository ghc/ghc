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

import GHC.Types.Name (Name, KnownKey, KnownOcc)
import GHC.Types.Name.Reader (GlobalRdrElt)
import GHC.Types.TyThing (TyThing)

import GHC.Unit.Types (Module, InstalledModule, UnitId, Unit)
import GHC.Unit.State (UnitState, ModuleSuggestion, ModuleOrigin, UnusableUnit, UnitInfo)

import GHC.Exception.Type (SomeException)

import GHC.Unit.Types ( IsBootInterface )
import GHC.Unit.Module.Location

import Language.Haskell.Syntax.Module.Name ( ModuleName )

import GHC.Stack( CallStack )
import GHC.Generics ( Generic )

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

  | MissingKnownKey1 KnownKey
    -- We looked up a known-key, but it wasn't in the
    -- known-key map that came from importing GHC.KnownKeyNames

   | MissingKnownKey2 KnownKey
     -- We looked up a known-key, but it wasn't in
     -- the `knownKeyTable` of all known keys

   | MissingKnownKey3 KnownOcc
     -- We looked up a known-occ, but it wasn't in
     -- the exports of GHC.KnownKeyNames

   | KnownKeyScopeError KnownOcc [GlobalRdrElt] CallStack
     -- We looked up a known-key in the GlobalRdrEnv,
     -- but did not find a unique hit
     -- CallStack is so that we can get a backtrace
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
