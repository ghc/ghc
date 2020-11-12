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
   , homeUnitMap
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
--
-- The unit identifier of the instantiating units is left open to allow
-- switching from UnitKey (what is provided by the user) to UnitId (internal
-- unit identifier) with `homeUnitMap`.
--
-- TODO: this isn't implemented yet. UnitKeys are still converted too early into
-- UnitIds in GHC.Unit.State.readUnitDataBase
data GenHomeUnit u
   = DefiniteHomeUnit UnitId (Maybe (u, GenInstantiations u))
      -- ^ Definite home unit (i.e. that we can compile).
      --
      -- Nothing:        not an instantiated unit
      -- Just (i,insts): made definite by instantiating "i" with "insts"

   | IndefiniteHomeUnit UnitId (GenInstantiations u)
      -- ^ Indefinite home unit (i.e. that we can only typecheck)
      --
      -- All the holes are instantiated with fake modules from the Hole unit.
      -- See Note [Representation of module/name variables] in "GHC.Unit"

type HomeUnit = GenHomeUnit UnitId

-- | Return home unit id
homeUnitId :: GenHomeUnit u -> UnitId
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
homeUnitInstanceOf :: HomeUnit -> UnitId
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
homeUnitAsUnit :: HomeUnit -> Unit
homeUnitAsUnit (DefiniteHomeUnit u _)    = RealUnit (Definite u)
homeUnitAsUnit (IndefiniteHomeUnit u is) = mkVirtUnit (Indefinite u) is

-- | Map over the unit identifier for instantiating units
homeUnitMap :: IsUnitId v => (u -> v) -> GenHomeUnit u -> GenHomeUnit v
homeUnitMap _ (DefiniteHomeUnit u Nothing)       = DefiniteHomeUnit u Nothing
homeUnitMap f (DefiniteHomeUnit u (Just (i,is))) = DefiniteHomeUnit u (Just (f i, mapInstantiations f is))
homeUnitMap f (IndefiniteHomeUnit u is)          = IndefiniteHomeUnit u (mapInstantiations f is)

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
isHomeUnit :: HomeUnit -> Unit -> Bool
isHomeUnit hu u = u == homeUnitAsUnit hu

-- | Test if the unit-id is the home unit-id
isHomeUnitId :: GenHomeUnit u -> UnitId -> Bool
isHomeUnitId hu uid = uid == homeUnitId hu

-- | Test if the home unit is an instance of the given unit-id
isHomeUnitInstanceOf :: HomeUnit -> UnitId -> Bool
isHomeUnitInstanceOf hu u = homeUnitInstanceOf hu == u

-- | Test if the module comes from the home unit
isHomeModule :: HomeUnit -> Module -> Bool
isHomeModule hu m = isHomeUnit hu (moduleUnit m)

-- | Test if the module comes from the home unit
isHomeInstalledModule :: GenHomeUnit u -> InstalledModule -> Bool
isHomeInstalledModule hu m = isHomeUnitId hu (moduleUnit m)


-- | Test if a module doesn't come from the given home unit
notHomeInstalledModule :: GenHomeUnit u -> InstalledModule -> Bool
notHomeInstalledModule hu m = not (isHomeInstalledModule hu m)

-- | Test if a module doesn't come from the given home unit
notHomeInstalledModuleMaybe :: Maybe (GenHomeUnit u) -> InstalledModule -> Bool
notHomeInstalledModuleMaybe mh m = fromMaybe True $ fmap (`notHomeInstalledModule` m) mh


-- | Test if a module doesn't come from the given home unit
notHomeModule :: HomeUnit -> Module -> Bool
notHomeModule hu m = not (isHomeModule hu m)

-- | Test if a module doesn't come from the given home unit
notHomeModuleMaybe :: Maybe HomeUnit -> Module -> Bool
notHomeModuleMaybe mh m = fromMaybe True $ fmap (`notHomeModule` m) mh

----------------------------
-- helpers
----------------------------

-- | Make a module in home unit
mkHomeModule :: HomeUnit -> ModuleName -> Module
mkHomeModule hu = mkModule (homeUnitAsUnit hu)

-- | Make a module in home unit
mkHomeInstalledModule :: GenHomeUnit u -> ModuleName -> InstalledModule
mkHomeInstalledModule hu = mkModule (homeUnitId hu)

-- | Return the module that is used to instantiate the given home module name.
-- If the ModuleName doesn't refer to a signature, return the actual home
-- module.
--
-- E.g., the instantiating module of @A@ in @p[A=q[]:B]@ is @q[]:B@.
--       the instantiating module of @A@ in @p@ is @p:A@.
homeModuleNameInstantiation :: HomeUnit -> ModuleName -> Module
homeModuleNameInstantiation hu mod_name =
    case lookup mod_name (homeUnitInstantiations hu) of
        Nothing  -> mkHomeModule hu mod_name
        Just mod -> mod

-- | Return the module that is used to instantiate the given home module.
--
-- If the given module isn't a module hole, return the actual home module.
--
-- E.g., the instantiating module of @p:A@ in @p[A=q[]:B]@ is @q[]:B@.
--       the instantiating module of @r:A@ in @p[A=q[]:B]@ is @r:A@.
--       the instantiating module of @p:A@ in @p@ is @p:A@.
--       the instantiating module of @r:A@ in @p@ is @r:A@.
homeModuleInstantiation :: HomeUnit -> Module -> Module
homeModuleInstantiation hu mod
   | isHomeModule hu mod = homeModuleNameInstantiation hu (moduleName mod)
   | otherwise           = mod

