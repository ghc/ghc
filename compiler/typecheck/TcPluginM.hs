{-# LANGUAGE CPP #-}
-- | This module provides an interface for typechecker plugins to
-- access select functions of the 'TcM', principally those to do with
-- reading parts of the state.
module TcPluginM (
#if defined(GHCI)
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
        newCoercionHole,

        -- * Manipulating evidence bindings
        newEvVar,
        setEvBind,
        getEvBindsTcPluginM
#endif
    ) where

#if defined(GHCI)
import GhcPrelude

import qualified TcRnMonad as TcM
import qualified TcSMonad  as TcS
import qualified TcEnv     as TcM
import qualified TcMType   as TcM
import qualified FamInst   as TcM
import qualified IfaceEnv
import qualified Finder

import FamInstEnv ( FamInstEnv )
import TcRnMonad  ( TcGblEnv, TcLclEnv, Ct, CtLoc, TcPluginM
                  , unsafeTcPluginTcM, getEvBindsTcPluginM
                  , liftIO, traceTc )
import TcMType    ( TcTyVar, TcType )
import TcEnv      ( TcTyThing )
import TcEvidence ( TcCoercion, CoercionHole, EvTerm(..)
                  , EvExpr, EvBind, mkGivenEvBind )
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
tcLookupGlobal = unsafeTcPluginTcM . TcM.tcLookupGlobal

tcLookupTyCon :: Name -> TcPluginM TyCon
tcLookupTyCon = unsafeTcPluginTcM . TcM.tcLookupTyCon

tcLookupDataCon :: Name -> TcPluginM DataCon
tcLookupDataCon = unsafeTcPluginTcM . TcM.tcLookupDataCon

tcLookupClass :: Name -> TcPluginM Class
tcLookupClass = unsafeTcPluginTcM . TcM.tcLookupClass

tcLookup :: Name -> TcPluginM TcTyThing
tcLookup = unsafeTcPluginTcM . TcM.tcLookup

tcLookupId :: Name -> TcPluginM Id
tcLookupId = unsafeTcPluginTcM . TcM.tcLookupId


getTopEnv :: TcPluginM HscEnv
getTopEnv = unsafeTcPluginTcM TcM.getTopEnv

getEnvs :: TcPluginM (TcGblEnv, TcLclEnv)
getEnvs = unsafeTcPluginTcM TcM.getEnvs

getInstEnvs :: TcPluginM InstEnvs
getInstEnvs = unsafeTcPluginTcM TcM.tcGetInstEnvs

getFamInstEnvs :: TcPluginM (FamInstEnv, FamInstEnv)
getFamInstEnvs = unsafeTcPluginTcM TcM.tcGetFamInstEnvs

matchFam :: TyCon -> [Type]
         -> TcPluginM (Maybe (TcCoercion, TcType))
matchFam tycon args = unsafeTcPluginTcM $ TcS.matchFamTcM tycon args

newUnique :: TcPluginM Unique
newUnique = unsafeTcPluginTcM TcM.newUnique

newFlexiTyVar :: Kind -> TcPluginM TcTyVar
newFlexiTyVar = unsafeTcPluginTcM . TcM.newFlexiTyVar

isTouchableTcPluginM :: TcTyVar -> TcPluginM Bool
isTouchableTcPluginM = unsafeTcPluginTcM . TcM.isTouchableTcM

-- Confused by zonking? See Note [What is zonking?] in TcMType.
zonkTcType :: TcType -> TcPluginM TcType
zonkTcType = unsafeTcPluginTcM . TcM.zonkTcType

zonkCt :: Ct -> TcPluginM Ct
zonkCt = unsafeTcPluginTcM . TcM.zonkCt


-- | Create a new wanted constraint.
newWanted  :: CtLoc -> PredType -> TcPluginM CtEvidence
newWanted loc pty
  = unsafeTcPluginTcM (TcM.newWanted (TcM.ctLocOrigin loc) Nothing pty)

-- | Create a new derived constraint.
newDerived :: CtLoc -> PredType -> TcPluginM CtEvidence
newDerived loc pty = return CtDerived { ctev_pred = pty, ctev_loc = loc }

-- | Create a new given constraint, with the supplied evidence.  This
-- must not be invoked from 'tcPluginInit' or 'tcPluginStop', or it
-- will panic.
newGiven :: CtLoc -> PredType -> EvExpr -> TcPluginM CtEvidence
newGiven loc pty evtm = do
   new_ev <- newEvVar pty
   setEvBind $ mkGivenEvBind new_ev (EvExpr evtm)
   return CtGiven { ctev_pred = pty, ctev_evar = new_ev, ctev_loc = loc }

-- | Create a fresh evidence variable.
newEvVar :: PredType -> TcPluginM EvVar
newEvVar = unsafeTcPluginTcM . TcM.newEvVar

-- | Create a fresh coercion hole.
newCoercionHole :: PredType -> TcPluginM CoercionHole
newCoercionHole = unsafeTcPluginTcM . TcM.newCoercionHole

-- | Bind an evidence variable.  This must not be invoked from
-- 'tcPluginInit' or 'tcPluginStop', or it will panic.
setEvBind :: EvBind -> TcPluginM ()
setEvBind ev_bind = do
    tc_evbinds <- getEvBindsTcPluginM
    unsafeTcPluginTcM $ TcM.addTcEvBind tc_evbinds ev_bind
#else
-- this dummy import is needed as a consequence of NoImplicitPrelude
import GhcPrelude ()
#endif
