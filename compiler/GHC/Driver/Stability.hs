{-# LANGUAGE LambdaCase #-}

module GHC.Driver.Stability (

  checkStability, checkStability_,

  module GHC.Types.Stability

) where

import GHC.Driver.DynFlags
import GHC.Driver.Env
import GHC.LanguageExtensions
import GHC.Tc.Module
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
import GHC.Utils.Panic

import qualified Data.Set as Set
import Data.List (singleton)
import Data.Maybe

import Control.Monad.IO.Class

import GHC.Prelude

extensionStability :: Extension -> StabilityMode
extensionStability _ = StabilityDefault

checkStability_ :: MonadIO m => HscEnv -> Dependencies -> Module -> m StabilityMode
checkStability_ hsc_env deps m = liftIO (checkStability hsc_env deps m) >>= \case
  Just  s -> return s
  Nothing -> panic $ "Failed stability check for " ++ (show $ moduleName m)

checkStability :: HscEnv -> Dependencies -> Module -> IO (Maybe StabilityMode)
checkStability hsc_env deps m = do
  ifaceMs <- mapM (getModuleInterface hsc_env) dep_mods
  let ifaces = catMaybes $ map snd ifaceMs

  -- external_graph <- hscEPS hsc_env
  -- let
  --   home_graph = hsc_HUG hsc_env
  --   hu_deps = hptSomeThingsBelowUs (singleton . mi_stability . hm_iface)
  --               True hsc_env (moduleUnitId m) (GWIB (moduleName m) IsBoot)

  return $ checkStability' flagMode exts (map mi_stability ifaces)
  where
    dep_mods = catMaybes
             . map toModule
             $ Set.toList (dep_direct_mods deps)
    toModule (_  , GWIB _ IsBoot ) = Nothing
    toModule (uid, GWIB m NotBoot) = Just $ Module (RealUnit $ Definite uid) m
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