{-
(c) The University of Glasgow, 2004-2006


Module
~~~~~~~~~~
Simply the name of a module, represented as a FastString.
These are Uniquable, hence we can build Maps with Modules as
the keys.
-}

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeSynonymInstances #-}

module GHC.Unit.Module
    ( module GHC.Unit.Types

      -- * The ModuleName type
    , module GHC.Unit.Module.Name

      -- * The ModLocation type
    , module GHC.Unit.Module.Location

      -- * ModuleEnv
    , module GHC.Unit.Module.Env

      -- * Generalization
    , getModuleInstantiation
    , getUnitInstantiations
    , uninstantiateInstantiatedUnit
    , uninstantiateInstantiatedModule

      -- * The Module type
    , mkHoleModule
    , isHoleModule
    , stableModuleCmp
    , moduleStableString
    , moduleIsDefinite
    , HasModule(..)
    , ContainsModule(..)
    , unitIdEq
    , installedModuleEq
    ) where

import GHC.Prelude

import GHC.Types.Unique.DSet
import GHC.Unit.Types
import GHC.Unit.Module.Name
import GHC.Unit.Module.Location
import GHC.Unit.Module.Env
import GHC.Utils.Misc

-- | A 'Module' is definite if it has no free holes.
moduleIsDefinite :: Module -> Bool
moduleIsDefinite = isEmptyUniqDSet . moduleFreeHoles

-- | Get a string representation of a 'Module' that's unique and stable
-- across recompilations.
-- eg. "$aeson_70dylHtv1FFGeai1IoxcQr$Data.Aeson.Types.Internal"
moduleStableString :: Module -> String
moduleStableString Module{..} =
  "$" ++ unitString moduleUnit ++ "$" ++ moduleNameString moduleName


-- | This gives a stable ordering, as opposed to the Ord instance which
-- gives an ordering based on the 'Unique's of the components, which may
-- not be stable from run to run of the compiler.
stableModuleCmp :: Module -> Module -> Ordering
stableModuleCmp (Module p1 n1) (Module p2 n2)
   = (p1 `stableUnitCmp`  p2) `thenCmp`
     (n1 `stableModuleNameCmp` n2)

class ContainsModule t where
    extractModule :: t -> Module

class HasModule m where
    getModule :: m Module


-- | Test if a 'Module' corresponds to a given 'InstalledModule',
-- modulo instantiation.
installedModuleEq :: InstalledModule -> Module -> Bool
installedModuleEq imod mod =
    fst (getModuleInstantiation mod) == imod

-- | Test if a 'Unit' corresponds to a given 'UnitId',
-- modulo instantiation.
unitIdEq :: UnitId -> Unit -> Bool
unitIdEq iuid uid = toUnitId uid == iuid

{-
************************************************************************
*                                                                      *
                        Hole substitutions
*                                                                      *
************************************************************************
-}

-- | Given a possibly on-the-fly instantiated module, split it into
-- a 'Module' that we definitely can find on-disk, as well as an
-- instantiation if we need to instantiate it on the fly.  If the
-- instantiation is @Nothing@ no on-the-fly renaming is needed.
getModuleInstantiation :: Module -> (InstalledModule, Maybe InstantiatedModule)
getModuleInstantiation m =
    let (uid, mb_iuid) = getUnitInstantiations (moduleUnit m)
    in (Module uid (moduleName m),
        fmap (\iuid -> Module iuid (moduleName m)) mb_iuid)

-- | Return the unit-id this unit is an instance of and the module instantiations (if any).
getUnitInstantiations :: Unit -> (UnitId, Maybe InstantiatedUnit)
getUnitInstantiations (VirtUnit iuid)           = (indefUnit (instUnitInstanceOf iuid), Just iuid)
getUnitInstantiations (RealUnit (Definite uid)) = (uid, Nothing)
getUnitInstantiations HoleUnit                  = error "Hole unit"

-- | Remove instantiations of the given instantiated unit
uninstantiateInstantiatedUnit :: InstantiatedUnit -> InstantiatedUnit
uninstantiateInstantiatedUnit u =
    mkInstantiatedUnit (instUnitInstanceOf u)
                       (map (\(m,_) -> (m, mkHoleModule m))
                         (instUnitInsts u))

-- | Remove instantiations of the given module instantiated unit
uninstantiateInstantiatedModule :: InstantiatedModule -> InstantiatedModule
uninstantiateInstantiatedModule (Module uid n) = Module (uninstantiateInstantiatedUnit uid) n

-- | Test if a Module is not instantiated
isHoleModule :: GenModule (GenUnit u) -> Bool
isHoleModule (Module HoleUnit _) = True
isHoleModule _                   = False

-- | Create a hole Module
mkHoleModule :: ModuleName -> GenModule (GenUnit u)
mkHoleModule = Module HoleUnit
