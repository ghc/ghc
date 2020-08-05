-- | The home unit is the unit (i.e. compiled package) that contains the module
-- we are compiling/typechecking.
module GHC.Unit.Home
   ( GenHomeUnit (..)
   , HomeUnit
   , homeUnitId
   , homeUnitInstantiations
   , homeUnitInstanceOf
   , homeUnitInstanceOfMaybe
   , homeUnitAsUnit
   -- * Predicates
   , isHomeUnitIndefinite
   , isHomeUnitDefinite
   , isHomeUnitInstantiating
   , isHomeUnit
   , isHomeUnitId
   , isHomeUnitInstanceOf
   , isHomeModule
   , isHomeInstalledModule
   , notHomeModule
   , notHomeModuleMaybe
   , notHomeInstalledModule
   , notHomeInstalledModuleMaybe
   -- * Helpers
   , mkHomeModule
   , mkHomeInstalledModule
   , homeModuleInstantiation
   , homeModuleNameInstantiation
   )
where

import GHC.Prelude
import GHC.Unit.Types
import GHC.Unit.Module.Name
import Data.Maybe

-- | Information about the home unit (i.e., the until that will contain the
-- modules we are compiling)
data GenHomeUnit u
   = DefiniteHomeUnit u (Maybe (u, GenInstantiations u))
      -- ^ Definite home unit (i.e. that we can compile).
      --
      -- Nothing:        not an instantiated unit
      -- Just (i,insts): made definite by instantiating "i" with "insts"

   | IndefiniteHomeUnit u (GenInstantiations u)
      -- ^ Indefinite home unit (i.e. that we can only typecheck)
      --
      -- All the holes are instantiated with fake modules from the Hole unit.
      -- See Note [Representation of module/name variables] in "GHC.Unit"

type HomeUnit = GenHomeUnit UnitId

-- | Return home unit id
homeUnitId :: GenHomeUnit u -> u
homeUnitId (DefiniteHomeUnit u _)   = u
homeUnitId (IndefiniteHomeUnit u _) = u

-- | Return home unit instantiations
homeUnitInstantiations :: GenHomeUnit u -> GenInstantiations u
homeUnitInstantiations (DefiniteHomeUnit   _ Nothing)       = []
homeUnitInstantiations (DefiniteHomeUnit   _ (Just (_,is))) = is
homeUnitInstantiations (IndefiniteHomeUnit _ is)            = is

-- | Return the unit id of the unit that is instantiated by the home unit.
--
-- E.g. if home unit = q[A=p:B,...] we return q.
--
-- If the home unit is not an instance of another unit, we return its own unit
-- id (it is an instance of itself if you will).
homeUnitInstanceOf :: GenHomeUnit u -> u
homeUnitInstanceOf h = fromMaybe (homeUnitId h) (homeUnitInstanceOfMaybe h)

-- | Return the unit id of the unit that is instantiated by the home unit.
--
-- E.g. if home unit = q[A=p:B,...] we return (Just q).
--
-- If the home unit is not an instance of another unit, we return Nothing.
homeUnitInstanceOfMaybe :: GenHomeUnit u -> Maybe u
homeUnitInstanceOfMaybe (DefiniteHomeUnit   _ (Just (u,_))) = Just u
homeUnitInstanceOfMaybe _                                   = Nothing

-- | Return the home unit as a normal unit.
--
-- We infer from the home unit itself the kind of unit we create:
--    1. If the home unit is definite, we must be compiling so we return a real
--    unit. The definite home unit may be the result of a unit instantiation,
--    say `p = q[A=r:X]`. In this case we could have returned a virtual unit
--    `q[A=r:X]` but it's not what the clients of this function expect,
--    especially because `p` is lost when we do this. The unit id of a virtual
--    unit is made up internally so `unitId(q[A=r:X])` is not equal to `p`.
--
--    2. If the home unit is indefinite we can only create a virtual unit from
--    it. It's ok because we must be only typechecking the home unit so we won't
--    produce any code object that rely on the unit id of this virtual unit.
homeUnitAsUnit :: IsUnitId u => GenHomeUnit u -> GenUnit u
homeUnitAsUnit (DefiniteHomeUnit u _)    = RealUnit (Definite u)
homeUnitAsUnit (IndefiniteHomeUnit u is) = mkVirtUnit (Indefinite u Nothing) is

----------------------------
-- Predicates
----------------------------

-- | Test if we are type-checking an indefinite unit
--
-- (if it is not, we should never use on-the-fly renaming)
isHomeUnitIndefinite :: GenHomeUnit u -> Bool
isHomeUnitIndefinite (DefiniteHomeUnit {})   = False
isHomeUnitIndefinite (IndefiniteHomeUnit {}) = True

-- | Test if we are compiling a definite unit
--
-- (if it is, we should never use on-the-fly renaming)
isHomeUnitDefinite :: GenHomeUnit u -> Bool
isHomeUnitDefinite (DefiniteHomeUnit {})   = True
isHomeUnitDefinite (IndefiniteHomeUnit {}) = False

-- | Test if we are compiling by instantiating a definite unit
isHomeUnitInstantiating :: GenHomeUnit u -> Bool
isHomeUnitInstantiating u =
   isHomeUnitDefinite u && not (null (homeUnitInstantiations u))

-- | Test if the unit is the home unit
isHomeUnit :: (IsUnitId u) => GenHomeUnit u -> GenUnit u -> Bool
isHomeUnit hu u = u == homeUnitAsUnit hu

-- | Test if the unit-id is the home unit-id
isHomeUnitId :: Eq u => GenHomeUnit u -> u -> Bool
isHomeUnitId hu uid = uid == homeUnitId hu

-- | Test if the home unit is an instance of the given unit-id
isHomeUnitInstanceOf :: Eq u => GenHomeUnit u -> u -> Bool
isHomeUnitInstanceOf h u = homeUnitInstanceOf h == u

-- | Test if the module comes from the home unit
isHomeModule :: (IsUnitId u) => GenHomeUnit u -> GenModule (GenUnit u) -> Bool
isHomeModule hu m = isHomeUnit hu (moduleUnit m)

-- | Test if the module comes from the home unit
isHomeInstalledModule :: Eq u => GenHomeUnit u -> GenModule u -> Bool
isHomeInstalledModule hu m = isHomeUnitId hu (moduleUnit m)


-- | Test if a module doesn't come from the given home unit
notHomeInstalledModule :: Eq u => GenHomeUnit u -> GenModule u -> Bool
notHomeInstalledModule hu m = not (isHomeInstalledModule hu m)

-- | Test if a module doesn't come from the given home unit
notHomeInstalledModuleMaybe :: Eq u => Maybe (GenHomeUnit u) -> GenModule  u -> Bool
notHomeInstalledModuleMaybe mh m = fromMaybe True $ fmap (`notHomeInstalledModule` m) mh


-- | Test if a module doesn't come from the given home unit
notHomeModule :: (IsUnitId u) => GenHomeUnit u -> GenModule (GenUnit u) -> Bool
notHomeModule hu m = not (isHomeModule hu m)

-- | Test if a module doesn't come from the given home unit
notHomeModuleMaybe :: (IsUnitId u) => Maybe (GenHomeUnit u) -> GenModule (GenUnit u) -> Bool
notHomeModuleMaybe mh m = fromMaybe True $ fmap (`notHomeModule` m) mh

----------------------------
-- helpers
----------------------------

-- | Make a module in home unit
mkHomeModule :: IsUnitId u => GenHomeUnit u -> ModuleName -> GenModule (GenUnit u)
mkHomeModule hu = mkModule (homeUnitAsUnit hu)

-- | Make a module in home unit
mkHomeInstalledModule :: GenHomeUnit u -> ModuleName -> GenModule u
mkHomeInstalledModule hu = mkModule (homeUnitId hu)

-- | Return the module that is used to instantiate the given home module name.
-- If the ModuleName doesn't refer to a signature, return the actual home
-- module.
--
-- E.g., the instantiating module of @A@ in @p[A=q[]:B]@ is @q[]:B@.
homeModuleNameInstantiation :: (IsUnitId u) => GenHomeUnit u -> ModuleName -> GenModule (GenUnit u)
homeModuleNameInstantiation hu mod_name =
    case lookup mod_name (homeUnitInstantiations hu) of
        Nothing  -> mkHomeModule hu mod_name
        Just mod -> mod

-- | Return the module that is used to instantiate the given home module.
--
-- If the given module isn't a module hole, return the actual home module.
--
-- E.g., the instantiating module of @p:A@ in @p[A=q[]:B]@ is @q[]:B@.
homeModuleInstantiation :: (IsUnitId u) => GenHomeUnit u -> GenModule (GenUnit u) -> GenModule (GenUnit u)
homeModuleInstantiation hu mod
   | isHomeModule hu mod = homeModuleNameInstantiation hu (moduleName mod)
   | otherwise           = mod

