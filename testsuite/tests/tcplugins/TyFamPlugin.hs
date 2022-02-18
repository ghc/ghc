{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module TyFamPlugin where
-- Solving an equality constraint involving type families.

-- base
import Data.Maybe
  ( catMaybes )

-- ghc
import GHC.Builtin.Types
  ( unitTy )
import GHC.Core
  ( Expr(Coercion) )
import GHC.Core.Coercion
  ( mkUnivCo )
import GHC.Core.Predicate
  ( EqRel(NomEq), Pred(EqPred)
  , classifyPredType
  )
import GHC.Core.TyCo.Rep
  ( Type, UnivCoProvenance(PluginProv) )
import GHC.Core.Type
  ( eqType, mkTyConApp, splitTyConApp_maybe )
import GHC.Plugins
  ( Plugin )
import GHC.Tc.Plugin
  ( TcPluginM
  , unsafeTcPluginTcM
  )
import GHC.Tc.Types
  ( TcPluginSolveResult(..) )
import GHC.Tc.Types.Constraint
  ( Ct(..), CanEqLHS(..)
  , ctPred
  )
import GHC.Tc.Types.Evidence
  ( EvBindsVar, EvTerm(EvExpr), Role(Nominal) )

-- common
import Common
  ( PluginDefs(..)
  , mkPlugin, don'tRewrite
  )

--------------------------------------------------------------------------------

-- This plugin solves Wanted constraints of the form "MyTyFam a a ~ ()".
--
-- It does so by looking through the Wanted constraints for equality constraints
-- whose left hand side is a type-family application "MyTyFam arg1 arg2",
-- such that "arg1 `eqType` arg2" returns "True",
-- and whose left hand side is the unit type "()".
--
-- In such a case, the plugin creates a universal coercion
-- with Plugin provenance to prove the equality constraint.

plugin :: Plugin
plugin = mkPlugin solver don'tRewrite

solver :: [String]
       -> PluginDefs -> EvBindsVar -> [Ct] -> [Ct]
       -> TcPluginM TcPluginSolveResult
solver _args defs _ev _gs ws = do
  solved <- catMaybes <$> traverse ( solveCt defs ) ws
  pure $ TcPluginOk solved []

-- Solve `MyTyFam a a ~ ()`.
solveCt :: PluginDefs -> Ct -> TcPluginM ( Maybe (EvTerm, Ct) )
solveCt ( PluginDefs {..} ) ct@( classifyPredType . ctPred -> EqPred NomEq lhs rhs )
  | Just ( tyFam, [arg1, arg2] ) <- splitTyConApp_maybe lhs
  , tyFam == myTyFam
  , arg1 `eqType` arg2
  , rhs `eqType` unitTy
  , let
      evTerm :: EvTerm
      evTerm = EvExpr . Coercion
             $ mkUnivCo ( PluginProv "TyFamPlugin" ) Nominal lhs rhs
  = pure $ Just ( evTerm, ct )
solveCt _ ct = pure Nothing
