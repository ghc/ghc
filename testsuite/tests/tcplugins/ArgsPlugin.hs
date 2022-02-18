{-# LANGUAGE RecordWildCards #-}

module ArgsPlugin where

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
  ( TcPluginM, getTargetPlatform )
import GHC.Tc.Types
  ( TcPluginSolveResult(..) )
import GHC.Tc.Types.Constraint
  ( Ct(..) )
import GHC.Tc.Types.Evidence
  ( EvBindsVar, EvTerm(EvExpr) )
import GHC.Platform
  ( Platform )

-- common
import Common
  ( PluginDefs(..)
  , mkPlugin, don'tRewrite
  )

--------------------------------------------------------------------------------

-- This plugin solves Wanted constraints of the form "MyClass Integer",
-- and supplies evidence that depends on the arguments to the plugin.
--
-- To find such constraints, we traverse through the Wanteds provided
-- to the plugin to find those whose name matches that of "MyClass",
-- and check that it is applied to the type "Integer".
--
-- We then construct a dictionary which is the integer that was passed
-- as an argument to the plugin.

plugin :: Plugin
plugin = mkPlugin solver don'tRewrite

-- Solve "MyClass Integer" with a class dictionary that depends on
-- a plugin argument.
solver :: [String]
       -> PluginDefs -> EvBindsVar -> [Ct] -> [Ct]
       -> TcPluginM TcPluginSolveResult
solver args defs _ev _gs ws = do
  let
    argsVal :: Integer
    argsVal = case args of
      arg : _ -> read arg
      _       -> error "ArgsPlugin: expected at least one argument"
  platform <- getTargetPlatform
  solved <- catMaybes <$> traverse ( solveCt platform defs argsVal ) ws
  pure $ TcPluginOk solved []

solveCt :: Platform -> PluginDefs -> Integer -> Ct -> TcPluginM ( Maybe (EvTerm, Ct) )
solveCt platform ( PluginDefs {..} ) i ct@( CDictCan { cc_class, cc_tyargs } )
  | className cc_class == className myClass
  , [tyArg] <- cc_tyargs
  , tyArg `eqType` integerTy
  , let
      evTerm :: EvTerm
      evTerm = EvExpr $
        mkCoreConApps ( classDataCon cc_class )
         [ Type integerTy, mkIntegerExpr platform i ]
  = pure $ Just ( evTerm, ct )
solveCt _ _ _ ct = pure Nothing
