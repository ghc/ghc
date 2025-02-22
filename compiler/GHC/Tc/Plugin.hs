
-- | This module provides an interface for typechecker plugins to
-- access select functions of the 'TcM', principally those to do with
-- reading parts of the state.
module GHC.Tc.Plugin (
        -- * Basic TcPluginM functionality
        TcPluginM,
        tcPluginIO,
        tcPluginTrace,
        unsafeTcPluginTcM,

        -- * Finding Modules and Names
        Finder.FindResult(..),
        findImportedModule,
        lookupOrig,

        -- * Looking up Names in the typechecking environment
        lookupTHName,
        tcLookupGlobal,
        tcLookupTyCon,
        tcLookupDataCon,
        tcLookupClass,
        tcLookup,
        tcLookupId,

        -- * Getting the TcM state
        getTopEnv,
        getTargetPlatform,
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
        newGiven,
        newCoercionHole,

        -- * Manipulating evidence bindings
        newEvVar,
        setEvBind,
    ) where

import GHC.Prelude

import GHC.Platform (Platform)

import qualified GHC.Tc.Utils.Monad     as TcM
import qualified GHC.Tc.Solver.Monad    as TcS
import qualified GHC.Tc.Utils.Env       as TcM
import qualified GHC.Tc.Utils.TcMType   as TcM
import qualified GHC.Tc.Zonk.TcType       as TcM
import qualified GHC.Tc.Instance.Family as TcM
import qualified GHC.Iface.Env          as IfaceEnv
import qualified GHC.Unit.Finder        as Finder

import GHC.Core.FamInstEnv     ( FamInstEnv )
import GHC.Tc.Utils.Monad      ( TcGblEnv, TcLclEnv, TcPluginM
                               , unsafeTcPluginTcM
                               , liftIO, traceTc )
import GHC.Tc.Types.Constraint ( Ct, CtEvidence(..) )
import GHC.Tc.Types.CtLoc      ( CtLoc )

import GHC.Tc.Utils.TcMType    ( TcTyVar, TcType )
import GHC.Tc.Utils.Env        ( TcTyThing )
import GHC.Tc.Types.Evidence   ( CoercionHole, EvTerm(..)
                               , EvExpr, EvBindsVar, EvBind, mkGivenEvBind )
import GHC.Types.Var           ( EvVar )
import GHC.Plugins             ( thNameToGhcNameIO )

import GHC.Unit.Module    ( ModuleName, Module )
import GHC.Types.Name     ( OccName, Name )
import GHC.Types.TyThing  ( TyThing )
import GHC.Core.Reduction ( Reduction )
import GHC.Core.TyCon     ( TyCon )
import GHC.Core.DataCon   ( DataCon )
import GHC.Core.Class     ( Class )
import GHC.Driver.Env       ( HscEnv(..) )
import GHC.Utils.Outputable ( SDoc )
import GHC.Core.Type        ( Kind, Type, PredType )
import GHC.Types.Id         ( Id )
import GHC.Core.InstEnv     ( InstEnvs )
import GHC.Types.Unique     ( Unique )
import GHC.Types.PkgQual    ( PkgQual )

import qualified GHC.Boot.TH.Syntax as TH

-- | Perform some IO, typically to interact with an external tool.
tcPluginIO :: IO a -> TcPluginM a
tcPluginIO a = unsafeTcPluginTcM (liftIO a)

-- | Output useful for debugging the compiler.
tcPluginTrace :: String -> SDoc -> TcPluginM ()
tcPluginTrace a b = unsafeTcPluginTcM (traceTc a b)


findImportedModule :: ModuleName -> PkgQual -> TcPluginM Finder.FindResult
findImportedModule mod_name mb_pkg = do
    hsc_env <- getTopEnv
    tcPluginIO $ Finder.findImportedModule hsc_env mod_name mb_pkg

lookupOrig :: Module -> OccName -> TcPluginM Name
lookupOrig mod = unsafeTcPluginTcM . IfaceEnv.lookupOrig mod

-- | Resolve a @template-haskell@ 'TH.Name' to a GHC 'Name'.
--
-- @since 9.14.1
lookupTHName :: TH.Name -> TcPluginM (Maybe Name)
lookupTHName th = do
    nc <- hsc_NC <$> getTopEnv
    tcPluginIO $ thNameToGhcNameIO nc th

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

getTargetPlatform :: TcPluginM Platform
getTargetPlatform = unsafeTcPluginTcM TcM.getPlatform


getEnvs :: TcPluginM (TcGblEnv, TcLclEnv)
getEnvs = unsafeTcPluginTcM TcM.getEnvs

getInstEnvs :: TcPluginM InstEnvs
getInstEnvs = unsafeTcPluginTcM TcM.tcGetInstEnvs

getFamInstEnvs :: TcPluginM (FamInstEnv, FamInstEnv)
getFamInstEnvs = unsafeTcPluginTcM TcM.tcGetFamInstEnvs

matchFam :: TyCon -> [Type]
         -> TcPluginM (Maybe Reduction)
matchFam tycon args = unsafeTcPluginTcM $ TcS.matchFamTcM tycon args

newUnique :: TcPluginM Unique
newUnique = unsafeTcPluginTcM TcM.newUnique

newFlexiTyVar :: Kind -> TcPluginM TcTyVar
newFlexiTyVar = unsafeTcPluginTcM . TcM.newFlexiTyVar

isTouchableTcPluginM :: TcTyVar -> TcPluginM Bool
isTouchableTcPluginM = unsafeTcPluginTcM . TcM.isTouchableTcM

-- | Confused by zonking? See Note [What is zonking?] in "GHC.Tc.Zonk.Type".
zonkTcType :: TcType -> TcPluginM TcType
zonkTcType = unsafeTcPluginTcM . TcM.liftZonkM . TcM.zonkTcType

zonkCt :: Ct -> TcPluginM Ct
zonkCt = unsafeTcPluginTcM . TcM.liftZonkM . TcM.zonkCt

-- | Create a new Wanted constraint with the given 'CtLoc'.
newWanted :: CtLoc -> PredType -> TcPluginM CtEvidence
newWanted loc pty
  = unsafeTcPluginTcM (TcM.newWantedWithLoc loc pty)

-- | Create a new given constraint, with the supplied evidence.
--
-- This should only be invoked within 'tcPluginSolve'.
newGiven :: EvBindsVar -> CtLoc -> PredType -> EvExpr -> TcPluginM CtEvidence
newGiven tc_evbinds loc pty evtm = do
   new_ev <- newEvVar pty
   setEvBind tc_evbinds $ mkGivenEvBind new_ev (EvExpr evtm)
   return CtGiven { ctev_pred = pty, ctev_evar = new_ev, ctev_loc = loc }

-- | Create a fresh evidence variable.
--
-- This should only be invoked within 'tcPluginSolve'.
newEvVar :: PredType -> TcPluginM EvVar
newEvVar = unsafeTcPluginTcM . TcM.newEvVar

-- | Create a fresh coercion hole.
-- This should only be invoked within 'tcPluginSolve'.
newCoercionHole :: PredType -> TcPluginM CoercionHole
newCoercionHole = unsafeTcPluginTcM . TcM.newVanillaCoercionHole

-- | Bind an evidence variable.
--
-- This should only be invoked within 'tcPluginSolve'.
setEvBind :: EvBindsVar -> EvBind -> TcPluginM ()
setEvBind tc_evbinds ev_bind = do
    unsafeTcPluginTcM $ TcM.addTcEvBind tc_evbinds ev_bind
