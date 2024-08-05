{-# LANGUAGE DeriveDataTypeable #-}

module GHC.Types.DefaultEnv
   ( ClassDefaults (..)
   , DefaultEnv
   , emptyDefaultEnv
   , isEmptyDefaultEnv
   , defaultEnv
   , unitDefaultEnv
   , lookupDefaultEnv
   , filterDefaultEnv
   , defaultList
   , plusDefaultEnv
   )
where

import GHC.Prelude
import GHC.Core.TyCon (TyCon(tyConName))
import GHC.Core.TyCon.Env (TyConEnv, emptyTyConEnv, isEmptyTyConEnv, mkTyConEnvWith, unitTyConEnv,
                           filterTyConEnv, nonDetTyConEnvElts, plusTyConEnv)
import GHC.Hs.Extension (GhcRn)
import GHC.Tc.Utils.TcType (Type)
import GHC.Types.Name (Name, nameUnique, stableNameCmp)
import GHC.Types.Unique.FM (lookupUFM_Directly)
import GHC.Unit.Module.Warnings (WarningTxt)
import GHC.Unit.Types (Module)
import GHC.Utils.Outputable

import Data.Data (Data)
import Data.List (sortBy)
import Data.Function (on)

-- See Note [Named default declarations] in GHC.Tc.Gen.Default
-- | Default environment mapping class @TyCon@s to their default type lists
type DefaultEnv = TyConEnv ClassDefaults

data ClassDefaults
  = ClassDefaults { cd_class   :: !TyCon  -- ^ always a class constructor
                  , cd_types   :: [Type]
                  , cd_module :: Maybe Module
                    -- ^ @Nothing@ for built-in,
                    -- @Just@ the current module or the module whence the default was imported
                    -- see Note [Default exports] in GHC.Tc.Gen.Export
                  , cd_warn    :: Maybe (WarningTxt GhcRn)
                    -- ^ Warning emitted when the default is used
                  }
  deriving Data

instance Outputable ClassDefaults where
  ppr ClassDefaults {cd_class = cls, cd_types = tys} = text "default" <+> ppr cls <+> parens (interpp'SP tys)

emptyDefaultEnv :: DefaultEnv
emptyDefaultEnv = emptyTyConEnv

isEmptyDefaultEnv :: DefaultEnv -> Bool
isEmptyDefaultEnv = isEmptyTyConEnv

unitDefaultEnv :: ClassDefaults -> DefaultEnv
unitDefaultEnv d = unitTyConEnv (cd_class d) d

defaultEnv :: [ClassDefaults] -> DefaultEnv
defaultEnv = mkTyConEnvWith cd_class

defaultList :: DefaultEnv -> [ClassDefaults]
defaultList = sortBy (stableNameCmp `on` (tyConName . cd_class)) . nonDetTyConEnvElts
              -- sortBy recovers determinism

lookupDefaultEnv :: DefaultEnv -> Name -> Maybe ClassDefaults
lookupDefaultEnv env = lookupUFM_Directly env . nameUnique

filterDefaultEnv :: (ClassDefaults -> Bool) -> DefaultEnv -> DefaultEnv
filterDefaultEnv = filterTyConEnv

plusDefaultEnv :: DefaultEnv -> DefaultEnv -> DefaultEnv
plusDefaultEnv = plusTyConEnv
