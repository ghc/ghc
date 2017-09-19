module Vectorise.Utils.Hoisting
  ( Inline(..)
  , addInlineArity
  , inlineMe

  , hoistBinding
  , hoistExpr
  , hoistVExpr
  , hoistPolyVExpr
  , takeHoisted
  )
where

import GhcPrelude

import Vectorise.Monad
import Vectorise.Env
import Vectorise.Vect
import Vectorise.Utils.Poly

import CoreSyn
import CoreUtils
import CoreUnfold
import Type
import Id
import BasicTypes  (Arity)
import FastString
import Control.Monad

-- Inline ---------------------------------------------------------------------

-- |Records whether we should inline a particular binding.
--
data Inline
        = Inline Arity
        | DontInline

-- |Add to the arity contained within an `Inline`, if any.
--
addInlineArity :: Inline -> Int -> Inline
addInlineArity (Inline m) n = Inline (m+n)
addInlineArity DontInline _ = DontInline

-- |Says to always inline a binding.
--
inlineMe :: Inline
inlineMe = Inline 0


-- Hoisting --------------------------------------------------------------------

hoistBinding :: Var -> CoreExpr -> VM ()
hoistBinding v e = updGEnv $ \env ->
  env { global_bindings = (v,e) : global_bindings env }

hoistExpr :: FastString -> CoreExpr -> Inline -> VM Var
hoistExpr fs expr inl
  = do
      var <- mk_inline `liftM` newLocalVar fs (exprType expr)
      hoistBinding var expr
      return var
  where
    mk_inline var = case inl of
                      Inline arity -> var `setIdUnfolding`
                                      mkInlineUnfoldingWithArity arity expr
                      DontInline   -> var

hoistVExpr :: VExpr -> Inline -> VM VVar
hoistVExpr (ve, le) inl
  = do
      fs <- getBindName
      vv <- hoistExpr ('v' `consFS` fs) ve inl
      lv <- hoistExpr ('l' `consFS` fs) le (addInlineArity inl 1)
      return (vv, lv)

-- |Hoist a polymorphic vectorised expression into a new top-level binding (representing a closure
-- function).
--
-- The hoisted expression is parameterised by (1) a set of type variables and (2) a set of value
-- variables that are passed as conventional type and value arguments.  The latter is implicitly
-- extended by the set of 'PA' dictionaries required for the type variables.
--
hoistPolyVExpr :: [TyVar] -> [Var] -> Inline -> VM VExpr -> VM VExpr
hoistPolyVExpr tvs vars inline p
  = do { inline' <- addInlineArity inline . (+ length vars) <$> polyArity tvs
       ; expr <- closedV . polyAbstract tvs $ \args ->
                   mapVect (mkLams $ tvs ++ args ++ vars) <$> p
       ; fn   <- hoistVExpr expr inline'
       ; let varArgs = varsToCoreExprs vars
       ; mapVect (\e -> e `mkApps` varArgs) <$> polyVApply (vVar fn) (mkTyVarTys tvs)
       }

takeHoisted :: VM [(Var, CoreExpr)]
takeHoisted
  = do
      env <- readGEnv id
      setGEnv $ env { global_bindings = [] }
      return $ global_bindings env
