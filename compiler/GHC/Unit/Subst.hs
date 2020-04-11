-- | Module hole substitutions
module GHC.Unit.Subst
   ( ShHoleSubst
   , renameHoleUnit
   , renameHoleModule
   , renameHoleUnit'
   , renameHoleModule'
   )
where

import GHC.Prelude

import {-# SOURCE #-} GHC.Unit.State
import GHC.Unit.Types
import GHC.Unit.Module.Env
import GHC.Unit.Module
import GHC.Types.Unique.FM
import GHC.Types.Unique.DFM
import GHC.Types.Unique.DSet

-- | Substitution on module variables, mapping module names to module
-- identifiers.
type ShHoleSubst = ModuleNameEnv Module

-- | Substitutes holes in a 'Module'.  NOT suitable for being called
-- directly on a 'nameModule', see Note [Representation of module/name variable].
-- @p[A=<A>]:B@ maps to @p[A=q():A]:B@ with @A=q():A@;
-- similarly, @<A>@ maps to @q():A@.
renameHoleModule :: PackageState -> ShHoleSubst -> Module -> Module
renameHoleModule state = renameHoleModule' (unitInfoMap state)

-- | Substitutes holes in a 'Unit', suitable for renaming when
-- an include occurs; see Note [Representation of module/name variable].
--
-- @p[A=<A>]@ maps to @p[A=<B>]@ with @A=<B>@.
renameHoleUnit :: PackageState -> ShHoleSubst -> Unit -> Unit
renameHoleUnit state = renameHoleUnit' (unitInfoMap state)

-- | Like 'renameHoleModule', but requires only 'UnitInfoMap'
-- so it can be used by "Packages".
renameHoleModule' :: UnitInfoMap -> ShHoleSubst -> Module -> Module
renameHoleModule' pkg_map env m
  | not (isHoleModule m) =
        let uid = renameHoleUnit' pkg_map env (moduleUnit m)
        in mkModule uid (moduleName m)
  | Just m' <- lookupUFM env (moduleName m) = m'
  -- NB m = <Blah>, that's what's in scope.
  | otherwise = m

-- | Like 'renameHoleUnit, but requires only 'UnitInfoMap'
-- so it can be used by "Packages".
renameHoleUnit' :: UnitInfoMap -> ShHoleSubst -> Unit -> Unit
renameHoleUnit' pkg_map env uid =
    case uid of
      (VirtUnit
        InstantiatedUnit{ instUnitInstanceOf = cid
                        , instUnitInsts      = insts
                        , instUnitHoles      = fh })
          -> if isNullUFM (intersectUFM_C const (udfmToUfm (getUniqDSet fh)) env)
                then uid
                -- Functorially apply the substitution to the instantiation,
                -- then check the 'UnitInfoMap' to see if there is
                -- a compiled version of this 'InstantiatedUnit' we can improve to.
                -- See Note [VirtUnit to RealUnit improvement]
                else improveUnit pkg_map $
                        mkVirtUnit cid
                            (map (\(k,v) -> (k, renameHoleModule' pkg_map env v)) insts)
      _ -> uid

