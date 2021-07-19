{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Common
  ( PluginDefs(..)
  , mkPlugin
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
  ( TcPlugin(..), TcPluginResult )
import GHC.Tc.Types.Constraint
  ( Ct )
import GHC.Types.Name.Occurrence
  ( mkClsOcc, mkDataOcc, mkTcOcc )
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
  findResult <- findImportedModule ( mkModuleName "Definitions" ) Nothing
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

mkPlugin :: ( [String] -> PluginDefs -> [Ct] -> [Ct] -> [Ct] -> TcPluginM TcPluginResult )
         -> Plugin
mkPlugin solve =
  defaultPlugin
    { tcPlugin        = \ args -> Just $ mkTcPlugin ( solve args )
    , pluginRecompile = purePlugin
    }

mkTcPlugin :: ( PluginDefs -> [Ct] -> [Ct] -> [Ct] -> TcPluginM TcPluginResult )
           -> TcPlugin
mkTcPlugin solve =
  TcPlugin
    { tcPluginInit  = lookupDefs
    , tcPluginSolve = solve
    , tcPluginStop  = \ _ -> pure ()
    }
