-- | Module environment
module GHC.Unit.Module.Env
   ( -- * Module mappings
     ModuleEnv
   , elemModuleEnv, extendModuleEnv, extendModuleEnvList
   , extendModuleEnvList_C, plusModuleEnv_C
   , delModuleEnvList, delModuleEnv, plusModuleEnv, lookupModuleEnv
   , lookupWithDefaultModuleEnv, mapModuleEnv, mkModuleEnv, emptyModuleEnv
   , partitionModuleEnv
   , moduleEnvKeys, moduleEnvElts, moduleEnvToList
   , unitModuleEnv, isEmptyModuleEnv
   , extendModuleEnvWith, filterModuleEnv

     -- * ModuleName mappings
   , ModuleNameEnv, DModuleNameEnv

     -- * Sets of Modules
   , ModuleSet
   , emptyModuleSet, mkModuleSet, moduleSetElts
   , extendModuleSet, extendModuleSetList, delModuleSet
   , elemModuleSet, intersectModuleSet, minusModuleSet, unionModuleSet
   , unitModuleSet, isEmptyModuleSet
   , unionManyModuleSets

     -- * InstalledModuleEnv
   , InstalledModuleEnv
   , emptyInstalledModuleEnv
   , lookupInstalledModuleEnv
   , extendInstalledModuleEnv
   , filterInstalledModuleEnv
   , delInstalledModuleEnv
   , mergeInstalledModuleEnv
   , plusInstalledModuleEnv
   , installedModuleEnvElts
   )
where

import GHC.Prelude

import GHC.Unit.Module.Name (ModuleName)
import GHC.Types.Unique
import GHC.Types.Unique.FM
import GHC.Types.Unique.DFM
import GHC.Unit.Types
import GHC.Utils.Misc
import Data.List (sortBy, sort)
import Data.Ord

import Data.Coerce
import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified GHC.Data.FiniteMap as Map
import GHC.Utils.Outputable

-- | A map keyed off of 'Module's
newtype ModuleEnv elt = ModuleEnv (Map NDModule elt)

instance Outputable a => Outputable (ModuleEnv a) where
  ppr (ModuleEnv m) = ppr m

{-
Note [ModuleEnv performance and determinism]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
To prevent accidental reintroduction of nondeterminism the Ord instance
for Module was changed to not depend on Unique ordering and to use the
lexicographic order. This is potentially expensive, but when measured
there was no difference in performance.

To be on the safe side and not pessimize ModuleEnv uses nondeterministic
ordering on Module and normalizes by doing the lexicographic sort when
turning the env to a list.
See Note [Unique Determinism] for more information about the source of
nondeterminismand and Note [Deterministic UniqFM] for explanation of why
it matters for maps.
-}

newtype NDModule = NDModule { unNDModule :: Module }
  deriving Eq
  -- A wrapper for Module with faster nondeterministic Ord.
  -- Don't export, See [ModuleEnv performance and determinism]
  --
instance Outputable NDModule where
  ppr (NDModule a) = ppr a

instance Ord NDModule where
  compare (NDModule (Module p1 n1)) (NDModule (Module p2 n2)) =
    (getUnique p1 `nonDetCmpUnique` getUnique p2) `thenCmp`
    (getUnique n1 `nonDetCmpUnique` getUnique n2)

filterModuleEnv :: (Module -> a -> Bool) -> ModuleEnv a -> ModuleEnv a
filterModuleEnv f (ModuleEnv e) =
  ModuleEnv (Map.filterWithKey (f . unNDModule) e)

elemModuleEnv :: Module -> ModuleEnv a -> Bool
elemModuleEnv m (ModuleEnv e) = Map.member (NDModule m) e

extendModuleEnv :: ModuleEnv a -> Module -> a -> ModuleEnv a
extendModuleEnv (ModuleEnv e) m x = ModuleEnv (Map.insert (NDModule m) x e)

extendModuleEnvWith :: (a -> a -> a) -> ModuleEnv a -> Module -> a
                    -> ModuleEnv a
extendModuleEnvWith f (ModuleEnv e) m x =
  ModuleEnv (Map.insertWith f (NDModule m) x e)

extendModuleEnvList :: ModuleEnv a -> [(Module, a)] -> ModuleEnv a
extendModuleEnvList (ModuleEnv e) xs =
  ModuleEnv (Map.insertList [(NDModule k, v) | (k,v) <- xs] e)

extendModuleEnvList_C :: (a -> a -> a) -> ModuleEnv a -> [(Module, a)]
                      -> ModuleEnv a
extendModuleEnvList_C f (ModuleEnv e) xs =
  ModuleEnv (Map.insertListWith f [(NDModule k, v) | (k,v) <- xs] e)

plusModuleEnv_C :: (a -> a -> a) -> ModuleEnv a -> ModuleEnv a -> ModuleEnv a
plusModuleEnv_C f (ModuleEnv e1) (ModuleEnv e2) =
  ModuleEnv (Map.unionWith f e1 e2)

delModuleEnvList :: ModuleEnv a -> [Module] -> ModuleEnv a
delModuleEnvList (ModuleEnv e) ms =
  ModuleEnv (Map.deleteList (map NDModule ms) e)

delModuleEnv :: ModuleEnv a -> Module -> ModuleEnv a
delModuleEnv (ModuleEnv e) m = ModuleEnv (Map.delete (NDModule m) e)

plusModuleEnv :: ModuleEnv a -> ModuleEnv a -> ModuleEnv a
plusModuleEnv (ModuleEnv e1) (ModuleEnv e2) = ModuleEnv (Map.union e1 e2)

lookupModuleEnv :: ModuleEnv a -> Module -> Maybe a
lookupModuleEnv (ModuleEnv e) m = Map.lookup (NDModule m) e

lookupWithDefaultModuleEnv :: ModuleEnv a -> a -> Module -> a
lookupWithDefaultModuleEnv (ModuleEnv e) x m =
  Map.findWithDefault x (NDModule m) e

mapModuleEnv :: (a -> b) -> ModuleEnv a -> ModuleEnv b
mapModuleEnv f (ModuleEnv e) = ModuleEnv (Map.mapWithKey (\_ v -> f v) e)

partitionModuleEnv :: (a -> Bool) -> ModuleEnv a -> (ModuleEnv a, ModuleEnv a)
partitionModuleEnv f (ModuleEnv e) = (ModuleEnv a, ModuleEnv b)
  where
    (a,b) = Map.partition f e

mkModuleEnv :: [(Module, a)] -> ModuleEnv a
mkModuleEnv xs = ModuleEnv (Map.fromList [(NDModule k, v) | (k,v) <- xs])

emptyModuleEnv :: ModuleEnv a
emptyModuleEnv = ModuleEnv Map.empty

moduleEnvKeys :: ModuleEnv a -> [Module]
moduleEnvKeys (ModuleEnv e) = sort $ map unNDModule $ Map.keys e
  -- See Note [ModuleEnv performance and determinism]

moduleEnvElts :: ModuleEnv a -> [a]
moduleEnvElts e = map snd $ moduleEnvToList e
  -- See Note [ModuleEnv performance and determinism]

moduleEnvToList :: ModuleEnv a -> [(Module, a)]
moduleEnvToList (ModuleEnv e) =
  sortBy (comparing fst) [(m, v) | (NDModule m, v) <- Map.toList e]
  -- See Note [ModuleEnv performance and determinism]

unitModuleEnv :: Module -> a -> ModuleEnv a
unitModuleEnv m x = ModuleEnv (Map.singleton (NDModule m) x)

isEmptyModuleEnv :: ModuleEnv a -> Bool
isEmptyModuleEnv (ModuleEnv e) = Map.null e

-- | A set of 'Module's
type ModuleSet = Set NDModule

mkModuleSet :: [Module] -> ModuleSet
mkModuleSet = Set.fromList . coerce

extendModuleSet :: ModuleSet -> Module -> ModuleSet
extendModuleSet s m = Set.insert (NDModule m) s

extendModuleSetList :: ModuleSet -> [Module] -> ModuleSet
extendModuleSetList s ms = foldl' (coerce . flip Set.insert) s ms

emptyModuleSet :: ModuleSet
emptyModuleSet = Set.empty

isEmptyModuleSet :: ModuleSet -> Bool
isEmptyModuleSet = Set.null

moduleSetElts :: ModuleSet -> [Module]
moduleSetElts = sort . coerce . Set.toList

elemModuleSet :: Module -> ModuleSet -> Bool
elemModuleSet = Set.member . coerce

intersectModuleSet :: ModuleSet -> ModuleSet -> ModuleSet
intersectModuleSet = coerce Set.intersection

minusModuleSet :: ModuleSet -> ModuleSet -> ModuleSet
minusModuleSet = coerce Set.difference

delModuleSet :: ModuleSet -> Module -> ModuleSet
delModuleSet = coerce (flip Set.delete)

unionModuleSet :: ModuleSet -> ModuleSet -> ModuleSet
unionModuleSet = coerce Set.union

unionManyModuleSets :: [ModuleSet] -> ModuleSet
unionManyModuleSets = coerce (Set.unions :: [Set NDModule] -> Set NDModule)

unitModuleSet :: Module -> ModuleSet
unitModuleSet = coerce Set.singleton

{-
A ModuleName has a Unique, so we can build mappings of these using
UniqFM.
-}

-- | A map keyed off of 'ModuleName's (actually, their 'Unique's)
type ModuleNameEnv elt = UniqFM ModuleName elt


-- | A map keyed off of 'ModuleName's (actually, their 'Unique's)
-- Has deterministic folds and can be deterministically converted to a list
type DModuleNameEnv elt = UniqDFM ModuleName elt


--------------------------------------------------------------------
-- InstalledModuleEnv
--------------------------------------------------------------------

-- | A map keyed off of 'InstalledModule'
newtype InstalledModuleEnv elt = InstalledModuleEnv (Map InstalledModule elt)

instance Outputable elt => Outputable (InstalledModuleEnv elt) where
  ppr (InstalledModuleEnv env) = ppr env


emptyInstalledModuleEnv :: InstalledModuleEnv a
emptyInstalledModuleEnv = InstalledModuleEnv Map.empty

lookupInstalledModuleEnv :: InstalledModuleEnv a -> InstalledModule -> Maybe a
lookupInstalledModuleEnv (InstalledModuleEnv e) m = Map.lookup m e

extendInstalledModuleEnv :: InstalledModuleEnv a -> InstalledModule -> a -> InstalledModuleEnv a
extendInstalledModuleEnv (InstalledModuleEnv e) m x = InstalledModuleEnv (Map.insert m x e)

filterInstalledModuleEnv :: (InstalledModule -> a -> Bool) -> InstalledModuleEnv a -> InstalledModuleEnv a
filterInstalledModuleEnv f (InstalledModuleEnv e) =
  InstalledModuleEnv (Map.filterWithKey f e)

delInstalledModuleEnv :: InstalledModuleEnv a -> InstalledModule -> InstalledModuleEnv a
delInstalledModuleEnv (InstalledModuleEnv e) m = InstalledModuleEnv (Map.delete m e)

installedModuleEnvElts :: InstalledModuleEnv a -> [(InstalledModule, a)]
installedModuleEnvElts (InstalledModuleEnv e) = Map.assocs e

mergeInstalledModuleEnv
  :: (elta -> eltb -> Maybe eltc)
  -> (InstalledModuleEnv elta -> InstalledModuleEnv eltc)  -- map X
  -> (InstalledModuleEnv eltb -> InstalledModuleEnv eltc) -- map Y
  -> InstalledModuleEnv elta
  -> InstalledModuleEnv eltb
  -> InstalledModuleEnv eltc
mergeInstalledModuleEnv f g h (InstalledModuleEnv xm) (InstalledModuleEnv ym)
  = InstalledModuleEnv $ Map.mergeWithKey
      (\_ x y -> (x `f` y))
      (coerce g)
      (coerce h)
      xm ym

plusInstalledModuleEnv :: (elt -> elt -> elt)
  -> InstalledModuleEnv elt
  -> InstalledModuleEnv elt
  -> InstalledModuleEnv elt
plusInstalledModuleEnv f (InstalledModuleEnv xm) (InstalledModuleEnv ym) =
  InstalledModuleEnv $ Map.unionWith f xm ym

