{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Common
  ( PluginDefs(..)
  , mkPlugin
  , don'tSolve, don'tRewrite
  )
  where

-- ghc
import GHC.Core.Class
  ( Class )
import GHC.Core.DataCon
  ( promoteDataCon )
import GHC.Core.TyCon
  ( TyCon )
import GHC.Core.Type
  ( Type
  , mkTyConApp
  )
import GHC.Plugins
  ( Plugin(..)
  , defaultPlugin, purePlugin
  )
import GHC.Tc.Plugin
  ( TcPluginM
  , findImportedModule, lookupOrig
  , tcLookupClass, tcLookupDataCon, tcLookupTyCon
  )
import GHC.Tc.Types
  ( TcPlugin(..), TcPluginSolveResult(..), TcPluginRewriteResult(..)
  , TcPluginRewriter
  )
import GHC.Tc.Types.Constraint
  ( Ct )
import GHC.Tc.Types.Evidence
  ( EvBindsVar )
import GHC.Types.Name.Occurrence
  ( mkClsOcc, mkDataOcc, mkTcOcc )
import GHC.Types.Unique.FM
  ( UniqFM, emptyUFM )
import GHC.Types.PkgQual
import GHC.Unit.Finder
  ( FindResult(..) )
import GHC.Unit.Module
  ( Module
  , mkModuleName
  )

--------------------------------------------------------------------------------

-- This module defines some common operations so that each individual plugin
-- doesn't have to do the same work over again:
--
--   - lookup the names of things the plugins will use
--     (the definitions are shared between most type-checking plugin tests)
--   - create a type-checking plugin from a solver, taking care of passing
--     the relevant data to the solver stage.

data PluginDefs =
  PluginDefs
    { nullary  :: !Class
    , myClass  :: !Class
    , myTyFam  :: !TyCon
    , nat      :: !Type
    , zero     :: !TyCon
    , succ     :: !TyCon
    , add      :: !TyCon
    }

definitionsModule :: TcPluginM Module
definitionsModule = do
  findResult <- findImportedModule ( mkModuleName "Definitions" ) NoPkgQual
  case findResult of
    Found _ res     -> pure res
    FoundMultiple _ -> error $ "TcPlugin test: found multiple modules named 'Definitions'."
    _               -> error $ "TcPlugin test: could not find any module named 'Defintiions'."

lookupDefs :: TcPluginM PluginDefs
lookupDefs = do
  defs <- definitionsModule
  nullary                       <- tcLookupClass   =<< lookupOrig defs ( mkClsOcc  "Nullary"  )
  myClass                       <- tcLookupClass   =<< lookupOrig defs ( mkClsOcc  "MyClass"  )
  myTyFam                       <- tcLookupTyCon   =<< lookupOrig defs ( mkTcOcc   "MyTyFam"  )
  ( (`mkTyConApp` []) -> nat  ) <- tcLookupTyCon   =<< lookupOrig defs ( mkTcOcc   "Nat"      )
  ( promoteDataCon    -> zero ) <- tcLookupDataCon =<< lookupOrig defs ( mkDataOcc "Zero"     )
  ( promoteDataCon    -> succ ) <- tcLookupDataCon =<< lookupOrig defs ( mkDataOcc "Succ"     )
  add                           <- tcLookupTyCon   =<< lookupOrig defs ( mkTcOcc   "Add"      )
  pure ( PluginDefs { .. } )

mkPlugin :: ( [String] -> PluginDefs -> EvBindsVar -> [Ct] -> [Ct] -> TcPluginM TcPluginSolveResult )
         -> ( [String] -> PluginDefs -> UniqFM TyCon TcPluginRewriter )
         -> Plugin
mkPlugin solve rewrite =
  defaultPlugin
    { tcPlugin        = \ args -> Just $ mkTcPlugin ( solve args ) ( rewrite args )
    , pluginRecompile = purePlugin
    }

mkTcPlugin :: ( PluginDefs -> EvBindsVar -> [Ct] -> [Ct] -> TcPluginM TcPluginSolveResult )
           -> ( PluginDefs -> UniqFM TyCon TcPluginRewriter )
           -> TcPlugin
mkTcPlugin solve rewrite =
  TcPlugin
    { tcPluginInit    = lookupDefs
    , tcPluginSolve   = solve
    , tcPluginRewrite = rewrite
    , tcPluginStop    = \ _ -> pure ()
    }

don'tSolve :: [String] -> s -> EvBindsVar -> [Ct] -> [Ct] -> TcPluginM TcPluginSolveResult
don'tSolve _ _ _ _ _ = pure $ TcPluginOk [] []

don'tRewrite :: [String] -> s -> UniqFM TyCon TcPluginRewriter
don'tRewrite _ _ = emptyUFM
