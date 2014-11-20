{-# LANGUAGE CPP #-}
-- | This module provides an interface for typechecker plugins to
-- access select functions of the 'TcM', principally those to do with
-- reading parts of the state.
module TcPluginM (
#ifdef GHCI
        -- * Basic TcPluginM functionality
        TcPluginM,
        tcPluginIO,
        tcPluginTrace,
        unsafeTcPluginTcM,

        -- * Lookup
        lookupRdrName,
        tcLookupGlobal,
        tcLookupTyCon,
        tcLookupDataCon,
        tcLookupClass,
        tcLookup,
        tcLookupId,

        -- * Getting the TcM state
        getTopEnv,
        getEnvs,
        getInstEnvs,
        getFamInstEnvs,

        -- * Type variables
        newFlexiTyVar,
        isTouchableTcPluginM,

        -- * Zonking
        zonkTcType,
        zonkCt
#endif
    ) where

#ifdef GHCI
import qualified TcRnMonad
import qualified TcEnv
import qualified TcMType
import qualified Inst
import qualified FamInst

import FamInstEnv ( FamInstEnv )
import TcRnMonad  ( TcGblEnv, TcLclEnv, Ct, TcPluginM
                  , unsafeTcPluginTcM, liftIO, traceTc )
import TcMType    ( TcTyVar, TcType )
import TcEnv      ( TcTyThing )

import Module
import Name
import RdrName
import TyCon
import DataCon
import Class
import HscTypes
import Outputable
import Type
import DynamicLoading
import Id
import InstEnv


-- | Perform some IO, typically to interact with an external tool.
tcPluginIO :: IO a -> TcPluginM a
tcPluginIO a = unsafeTcPluginTcM (liftIO a)

-- | Output useful for debugging the compiler.
tcPluginTrace :: String -> SDoc -> TcPluginM ()
tcPluginTrace a b = unsafeTcPluginTcM (traceTc a b)


lookupRdrName :: ModuleName -> RdrName -> TcPluginM (Maybe Name)
lookupRdrName mod rdr = do
  hsc_env <- getTopEnv
  tcPluginIO $ lookupRdrNameInModuleForPlugins hsc_env mod rdr

tcLookupGlobal :: Name -> TcPluginM TyThing
tcLookupGlobal = unsafeTcPluginTcM . TcEnv.tcLookupGlobal

tcLookupTyCon :: Name -> TcPluginM TyCon
tcLookupTyCon = unsafeTcPluginTcM . TcEnv.tcLookupTyCon

tcLookupDataCon :: Name -> TcPluginM DataCon
tcLookupDataCon = unsafeTcPluginTcM . TcEnv.tcLookupDataCon

tcLookupClass :: Name -> TcPluginM Class
tcLookupClass = unsafeTcPluginTcM . TcEnv.tcLookupClass

tcLookup :: Name -> TcPluginM TcTyThing
tcLookup = unsafeTcPluginTcM . TcEnv.tcLookup

tcLookupId :: Name -> TcPluginM Id
tcLookupId = unsafeTcPluginTcM . TcEnv.tcLookupId


getTopEnv :: TcPluginM HscEnv
getTopEnv = unsafeTcPluginTcM TcRnMonad.getTopEnv

getEnvs :: TcPluginM (TcGblEnv, TcLclEnv)
getEnvs = unsafeTcPluginTcM TcRnMonad.getEnvs

getInstEnvs :: TcPluginM (InstEnv, InstEnv)
getInstEnvs = unsafeTcPluginTcM Inst.tcGetInstEnvs

getFamInstEnvs :: TcPluginM (FamInstEnv, FamInstEnv)
getFamInstEnvs = unsafeTcPluginTcM FamInst.tcGetFamInstEnvs


newFlexiTyVar :: Kind -> TcPluginM TcTyVar
newFlexiTyVar = unsafeTcPluginTcM . TcMType.newFlexiTyVar

isTouchableTcPluginM :: TcTyVar -> TcPluginM Bool
isTouchableTcPluginM = unsafeTcPluginTcM . TcRnMonad.isTouchableTcM


zonkTcType :: TcType -> TcPluginM TcType
zonkTcType = unsafeTcPluginTcM . TcMType.zonkTcType

zonkCt :: Ct -> TcPluginM Ct
zonkCt = unsafeTcPluginTcM . TcMType.zonkCt
#endif
