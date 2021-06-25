{-# LANGUAGE RecordWildCards #-}

module ArgsPlugin where
-- Creating class evidence based on a plugin argument.

-- base
import Data.Maybe
  ( catMaybes )

-- ghc
import GHC.Builtin.Types
  ( integerTy )
import GHC.Core
  ( Expr(Type) )
import GHC.Core.Class
  ( Class(..) )
import GHC.Core.DataCon
  ( classDataCon )
import GHC.Core.Make
  ( mkCoreConApps, mkIntegerExpr )
import GHC.Core.Type
  ( eqType )
import GHC.Plugins
  ( Plugin )
import GHC.Tc.Plugin
  ( TcPluginM )
import GHC.Tc.Types
  ( TcPluginSolveResult(..) )
import GHC.Tc.Types.Constraint
  ( Ct(..) )
import GHC.Tc.Types.Evidence
  ( EvBindsVar, EvTerm(EvExpr) )

-- common
import Common
  ( PluginDefs(..)
  , mkPlugin, don'tRewrite
  )

--------------------------------------------------------------------------------

plugin :: Plugin
plugin = mkPlugin solver don'tRewrite

solver :: [String]
       -> PluginDefs -> EvBindsVar -> [Ct] -> [Ct] -> [Ct]
       -> TcPluginM TcPluginSolveResult
solver args defs _ev _gs _ds ws = do
  let
    argsVal :: Integer
    argsVal = case args of
      arg : _ -> read arg
      _       -> error "ArgsPlugin: expected at least one argument"
  solved <- catMaybes <$> traverse ( solveCt defs argsVal ) ws
  pure $ TcPluginOk solved []

solveCt :: PluginDefs -> Integer -> Ct -> TcPluginM ( Maybe (EvTerm, Ct) )
solveCt ( PluginDefs {..} ) i ct@( CDictCan { cc_class, cc_tyargs } )
  | className cc_class == className myClass
  , [tyArg] <- cc_tyargs
  , tyArg `eqType` integerTy
  , let
      evTerm :: EvTerm
      evTerm = EvExpr $ mkCoreConApps ( classDataCon cc_class ) [ Type integerTy, mkIntegerExpr i ]
  = pure $ Just ( evTerm, ct )
solveCt _ _ ct = pure Nothing
