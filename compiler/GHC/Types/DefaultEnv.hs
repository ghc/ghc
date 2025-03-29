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
   , mkDefaultEnv
   )
where

import GHC.Core.Class (Class (className))
import GHC.Prelude
import GHC.Hs.Extension (GhcRn)
import GHC.Tc.Utils.TcType (Type)
import GHC.Types.Name (Name, nameUnique, stableNameCmp)
import GHC.Types.Name.Env
import GHC.Types.Unique.FM (lookupUFM_Directly)
import GHC.Unit.Module.Warnings (WarningTxt)
import GHC.Unit.Types (Module)
import GHC.Utils.Outputable

import Data.Data (Data)
import Data.List (sortBy)
import Data.Function (on)

-- See Note [Named default declarations] in GHC.Tc.Gen.Default
-- | Default environment mapping class name @Name@ to their default type lists
type DefaultEnv = NameEnv ClassDefaults

data ClassDefaults
  = ClassDefaults { cd_class   :: Class -- ^ The class whose defaults are being defined
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
  ppr ClassDefaults {cd_class = cls, cd_types = tys} = text "default" <+> ppr cls
        <+> parens (interpp'SP tys)

emptyDefaultEnv :: DefaultEnv
emptyDefaultEnv = emptyNameEnv

isEmptyDefaultEnv :: DefaultEnv -> Bool
isEmptyDefaultEnv = isEmptyNameEnv

unitDefaultEnv :: ClassDefaults -> DefaultEnv
unitDefaultEnv d = unitNameEnv (className $ cd_class d) d

defaultEnv :: [ClassDefaults] -> DefaultEnv
defaultEnv = mkNameEnvWith (className . cd_class)

defaultList :: DefaultEnv -> [ClassDefaults]
defaultList = sortBy (stableNameCmp `on` className . cd_class) . nonDetNameEnvElts
              -- sortBy recovers determinism

lookupDefaultEnv :: DefaultEnv -> Name -> Maybe ClassDefaults
lookupDefaultEnv env = lookupUFM_Directly env . nameUnique

filterDefaultEnv :: (ClassDefaults -> Bool) -> DefaultEnv -> DefaultEnv
filterDefaultEnv = filterNameEnv

plusDefaultEnv :: DefaultEnv -> DefaultEnv -> DefaultEnv
plusDefaultEnv = plusNameEnv

-- | Create a 'DefaultEnv' from a list of (Name, ClassDefaults) pairs
-- it is useful if we don't want to poke into the 'ClassDefaults' structure
-- to get the 'Name' of the class, it can be problematic, see #25858
mkDefaultEnv :: [(Name, ClassDefaults)] -> DefaultEnv
mkDefaultEnv = mkNameEnv
