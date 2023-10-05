module GHC.Driver.Stability (

  checkStability,

  module GHC.Types.Stability

) where

import GHC.Driver.DynFlags
import GHC.Driver.Env
import GHC.LanguageExtensions
import GHC.Types.Stability
import GHC.Unit.Module.Deps
import GHC.Unit.Module.Env
import GHC.Unit.Module.ModIface
import GHC.Unit.Env
import GHC.Unit.External
import GHC.Unit.Home.ModInfo
import GHC.Unit.Types

import GHC.Data.EnumSet as EnumSet
import GHC.IORef

import qualified Data.Set as Set
import Data.List (singleton)
import Data.Maybe

import GHC.Prelude

extensionStability :: Extension -> StabilityMode
extensionStability _ = StabilityDefault

checkStability :: HscEnv -> Dependencies -> Module -> IO (Maybe StabilityMode)
checkStability hsc_env deps m = do
  external_graph <- hscEPS hsc_env
  let
    home_graph = hsc_HUG hsc_env
    hu_deps = hptSomeThingsBelowUs (singleton . mi_stability . hm_iface)
                True hsc_env (moduleUnitId m) (GWIB (moduleName m) NotBoot)

  return $ checkStability' flagMode exts (hu_deps ++ [])
  where
    dflags = hsc_dflags hsc_env
    flagMode = stabilityMode dflags
    exts = EnumSet.toList $ extensionFlags dflags

dependencyIface :: UnitId -> ModuleNameWithIsBoot -> HomeUnitGraph -> Maybe ModIface
dependencyIface uid (GWIB modu _) hug = hm_iface <$> lookupHugByModule (Module (RealUnit $ Definite uid) modu) hug

checkStability'
  :: StabilityMode
  -> [Extension]
  -> [StabilityMode]
  -> Maybe StabilityMode
checkStability' flag exts deps =
  if thisStability <= flag
    then Just flag
    else Nothing
  where
    extsStability = maximum $ StabilityDefault : map extensionStability exts
    depsStability = maximum $ StabilityDefault : deps
    thisStability = max extsStability depsStability