
{-# OPTIONS -fno-warn-tabs #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and
-- detab the module (please do the detabbing in a separate patch). See
--     http://hackage.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#TabsvsSpaces
-- for details

module Vectorise.Utils.Hoisting (
	Inline(..),
	addInlineArity,
	inlineMe,
	
	hoistBinding,
	hoistExpr,
	hoistVExpr,
	hoistPolyVExpr,
	takeHoisted
)
where
import Vectorise.Monad
import Vectorise.Env
import Vectorise.Vect
import Vectorise.Utils.Poly

import CoreSyn
import CoreUtils
import CoreUnfold
import Type
import Id
import BasicTypes( Arity )
import FastString
import Control.Monad


-- Inline ---------------------------------------------------------------------
-- | Records whether we should inline a particular binding.
data Inline 
        = Inline Arity
        | DontInline

-- | Add to the arity contained within an `Inline`, if any.
addInlineArity :: Inline -> Int -> Inline
addInlineArity (Inline m) n = Inline (m+n)
addInlineArity DontInline _ = DontInline

-- | Says to always inline a binding.
inlineMe :: Inline
inlineMe = Inline 0


-- Hoising --------------------------------------------------------------------
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
                                      mkInlineUnfolding (Just arity) expr
                      DontInline   -> var


hoistVExpr :: VExpr -> Inline -> VM VVar
hoistVExpr (ve, le) inl
  = do
      fs <- getBindName
      vv <- hoistExpr ('v' `consFS` fs) ve inl
      lv <- hoistExpr ('l' `consFS` fs) le (addInlineArity inl 1)
      return (vv, lv)


hoistPolyVExpr :: [TyVar] -> Inline -> VM VExpr -> VM VExpr
hoistPolyVExpr tvs inline p
  = do
      inline' <- liftM (addInlineArity inline) (polyArity tvs)
      expr <- closedV . polyAbstract tvs $ \args ->
              liftM (mapVect (mkLams $ tvs ++ args)) p
      fn   <- hoistVExpr expr inline'
      polyVApply (vVar fn) (mkTyVarTys tvs)


takeHoisted :: VM [(Var, CoreExpr)]
takeHoisted
  = do
      env <- readGEnv id
      setGEnv $ env { global_bindings = [] }
      return $ global_bindings env
