module GHC.Unit.External.Substitution (
  -- * Substitution and module renaming
  ShHoleSubst,
  renameHoleModule',
  renameHoleUnit',
  renameUnitInfo,
) where

import GHC.Prelude

import GHC.Unit.Module
import GHC.Unit.Info
import GHC.Types.Unique.FM
import GHC.Types.Unique.DFM
import GHC.Types.Unique.DSet

-- -----------------------------------------------------------------------------
-- Module renaming

-- | Substitution on module variables, mapping module names to module
-- identifiers.
type ShHoleSubst = ModuleNameEnv Module

-- | Rename a 'UnitInfo' according to some module instantiation.
renameUnitInfo :: UnitInfoMap -> [(ModuleName, Module)] -> UnitInfo -> UnitInfo
renameUnitInfo pkg_map insts conf =
    let hsubst = listToUFM insts
        smod  = renameHoleModule' pkg_map hsubst
        new_insts = map (\(k,v) -> (k,smod v)) (unitInstantiations conf)
    in conf {
        unitInstantiations = new_insts,
        unitExposedModules = map (\(mod_name, mb_mod) -> (mod_name, fmap smod mb_mod))
                             (unitExposedModules conf)
    }


-- | Like 'renameHoleModule', but requires only 'UnitInfoMap'
-- so it can be used by "GHC.Unit.State".
renameHoleModule' :: UnitInfoMap -> ShHoleSubst -> Module -> Module
renameHoleModule' pkg_map env m
  | not (isHoleModule m) =
        let uid = renameHoleUnit' pkg_map env (moduleUnit m)
        in mkModule uid (moduleName m)
  | Just m' <- lookupUFM env (moduleName m) = m'
  -- NB m = <Blah>, that's what's in scope.
  | otherwise = m

-- | Like 'renameHoleUnit', but requires only 'UnitInfoMap'
-- so it can be used by "GHC.Unit.State".
renameHoleUnit' :: UnitInfoMap -> ShHoleSubst -> Unit -> Unit
renameHoleUnit' pkg_map env uid =
    case uid of
      (VirtUnit
        InstantiatedUnit{ instUnitInstanceOf = cid
                        , instUnitInsts      = insts
                        , instUnitHoles      = fh })
          -> if isNullUFM (intersectUFM_C const (udfmToUfm (getUniqDSet fh)) env)
                then uid
                else mkVirtUnit cid
                          (map (\(k,v) -> (k, renameHoleModule' pkg_map env v)) insts)
      _ -> uid
