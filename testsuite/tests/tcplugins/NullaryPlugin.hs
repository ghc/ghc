{-# LANGUAGE RecordWildCards #-}

module NullaryPlugin where

-- base
import Data.Maybe
  ( catMaybes )

-- ghc
import GHC.Core.Class
  ( Class(..) )
import GHC.Core.DataCon
  ( classDataCon )
import GHC.Core.Make
  ( mkCoreConApps )
import GHC.Plugins
  ( Plugin )
import GHC.Tc.Plugin
  ( TcPluginM )
import GHC.Tc.Types
  ( TcPluginSolveResult(..) )
import GHC.Tc.Types.Constraint
  ( Ct(..), DictCt(..) )
import GHC.Tc.Types.Evidence
  ( EvBindsVar, EvTerm(EvExpr) )

-- common
import Common
  ( PluginDefs(..)
  , mkPlugin, don'tRewrite
  )

--------------------------------------------------------------------------------

-- This plugin solves Wanted 'Nullary' constraints.
-- To do this, we just look through the Wanteds,
-- find any constraint whose className matches that of 'Nullary',
-- in which case we provide evidence (a nullary dictionary).

plugin :: Plugin
plugin = mkPlugin solver don'tRewrite

-- Solve "Nullary".
solver :: [String]
       -> PluginDefs -> EvBindsVar -> [Ct] -> [Ct]
       -> TcPluginM TcPluginSolveResult
solver _args defs _ev _gs ws = do
  solved <- catMaybes <$> traverse ( solveCt defs ) ws
  pure $ TcPluginOk solved []

solveCt :: PluginDefs -> Ct -> TcPluginM ( Maybe (EvTerm, Ct) )
solveCt ( PluginDefs {..} ) ct@( CDictCan (DictCt { di_cls } ))
  | className di_cls == className nullary
  , let
      evTerm :: EvTerm
      evTerm = EvExpr $ mkCoreConApps ( classDataCon di_cls ) []
  = pure $ Just ( evTerm, ct )
solveCt _ ct = pure Nothing
