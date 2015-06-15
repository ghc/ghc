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

        -- * Finding Modules and Names
        FindResult(..),
        findImportedModule,
        lookupOrig,

        -- * Looking up Names in the typechecking environment
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
        matchFam,

        -- * Type variables
        newUnique,
        newFlexiTyVar,
        isTouchableTcPluginM,

        -- * Zonking
        zonkTcType,
        zonkCt,

        -- * Creating constraints
        newWanted,
        newDerived,
        newGiven,

        -- * Manipulating evidence bindings
        newEvVar,
        setEvBind,
        getEvBindsTcPluginM,
        getEvBindsTcPluginM_maybe
#endif
    ) where

#ifdef GHCI
import qualified TcRnMonad
import qualified TcSMonad
import qualified TcEnv
import qualified TcMType
import qualified Inst
import qualified FamInst
import qualified IfaceEnv
import qualified Finder

import FamInstEnv ( FamInstEnv )
import TcRnMonad  ( TcGblEnv, TcLclEnv, Ct, CtLoc, TcPluginM
                  , unsafeTcPluginTcM, getEvBindsTcPluginM_maybe
                  , liftIO, traceTc )
import TcMType    ( TcTyVar, TcType )
import TcEnv      ( TcTyThing )
import TcEvidence ( TcCoercion, EvTerm, EvBind, EvBindsVar, mkGivenEvBind )
import TcRnTypes  ( CtEvidence(..) )
import Var        ( EvVar )

import Module
import Name
import TyCon
import DataCon
import Class
import HscTypes
import Outputable
import Type
import Id
import InstEnv
import FastString
import Maybes
import Unique


-- | Perform some IO, typically to interact with an external tool.
tcPluginIO :: IO a -> TcPluginM a
tcPluginIO a = unsafeTcPluginTcM (liftIO a)

-- | Output useful for debugging the compiler.
tcPluginTrace :: String -> SDoc -> TcPluginM ()
tcPluginTrace a b = unsafeTcPluginTcM (traceTc a b)


findImportedModule :: ModuleName -> Maybe FastString -> TcPluginM FindResult
findImportedModule mod_name mb_pkg = do
    hsc_env <- getTopEnv
    tcPluginIO $ Finder.findImportedModule hsc_env mod_name mb_pkg

lookupOrig :: Module -> OccName -> TcPluginM Name
lookupOrig mod = unsafeTcPluginTcM . IfaceEnv.lookupOrig mod


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

getInstEnvs :: TcPluginM InstEnvs
getInstEnvs = unsafeTcPluginTcM Inst.tcGetInstEnvs

getFamInstEnvs :: TcPluginM (FamInstEnv, FamInstEnv)
getFamInstEnvs = unsafeTcPluginTcM FamInst.tcGetFamInstEnvs

matchFam :: TyCon -> [Type] -> TcPluginM (Maybe (TcCoercion, TcType))
matchFam tycon args = unsafeTcPluginTcM $ TcSMonad.matchFamTcM tycon args


newUnique :: TcPluginM Unique
newUnique = unsafeTcPluginTcM TcRnMonad.newUnique

newFlexiTyVar :: Kind -> TcPluginM TcTyVar
newFlexiTyVar = unsafeTcPluginTcM . TcMType.newFlexiTyVar

isTouchableTcPluginM :: TcTyVar -> TcPluginM Bool
isTouchableTcPluginM = unsafeTcPluginTcM . TcRnMonad.isTouchableTcM


zonkTcType :: TcType -> TcPluginM TcType
zonkTcType = unsafeTcPluginTcM . TcMType.zonkTcType

zonkCt :: Ct -> TcPluginM Ct
zonkCt = unsafeTcPluginTcM . TcMType.zonkCt


-- | Create a new wanted constraint.
newWanted  :: CtLoc -> PredType -> TcPluginM CtEvidence
newWanted loc pty = do
    new_ev <- newEvVar pty
    return CtWanted { ctev_pred = pty, ctev_evar = new_ev, ctev_loc = loc }

-- | Create a new derived constraint.
newDerived :: CtLoc -> PredType -> TcPluginM CtEvidence
newDerived loc pty = return CtDerived { ctev_pred = pty, ctev_loc = loc }

-- | Create a new given constraint, with the supplied evidence.  This
-- must not be invoked from 'tcPluginInit' or 'tcPluginStop', or it
-- will panic.
newGiven :: CtLoc -> PredType -> EvTerm -> TcPluginM CtEvidence
newGiven loc pty evtm = do
   new_ev <- newEvVar pty
   setEvBind $ mkGivenEvBind new_ev evtm
   return CtGiven { ctev_pred = pty, ctev_evar = new_ev, ctev_loc = loc }

-- | Create a fresh evidence variable.
newEvVar :: PredType -> TcPluginM EvVar
newEvVar = unsafeTcPluginTcM . TcMType.newEvVar

-- | Bind an evidence variable.  This must not be invoked from
-- 'tcPluginInit' or 'tcPluginStop', or it will panic.
setEvBind :: EvBind -> TcPluginM ()
setEvBind ev_bind = do
    tc_evbinds <- getEvBindsTcPluginM
    unsafeTcPluginTcM $ TcMType.addTcEvBind tc_evbinds ev_bind

-- | Access the 'EvBindsVar' carried by the 'TcPluginM' during
-- constraint solving.  This must not be invoked from 'tcPluginInit'
-- or 'tcPluginStop', or it will panic.
getEvBindsTcPluginM :: TcPluginM EvBindsVar
getEvBindsTcPluginM = fmap (expectJust oops) getEvBindsTcPluginM_maybe
  where
    oops = "plugin attempted to read EvBindsVar outside the constraint solver"
#endif
