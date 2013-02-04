module Vectorise.Convert
  ( fromVect
  )
where

import Vectorise.Monad
import Vectorise.Builtins
import Vectorise.Type.Type

import CoreSyn
import TyCon
import Type
import TypeRep
import NameSet
import FastString
import Outputable

import Control.Applicative


-- |Convert a vectorised expression such that it computes the non-vectorised equivalent of its
-- value.
--
-- For functions, we eta expand the function and convert the arguments and result:

-- For example
-- @   
--    \(x :: Double) -> 
--    \(y :: Double) -> 
--    ($v_foo $: x) $: y
-- @
--
-- We use the type of the original binding to work out how many outer lambdas to add.
--
fromVect :: Type        -- ^ The type of the original binding.
         -> CoreExpr    -- ^ Expression giving the closure to use, eg @$v_foo@.
         -> VM CoreExpr
  
-- Convert the type to the core view if it isn't already.
--
fromVect ty expr 
  | Just ty' <- coreView ty 
  = fromVect ty' expr

-- For each function constructor in the original type we add an outer 
-- lambda to bind the parameter variable, and an inner application of it.
fromVect (FunTy arg_ty res_ty) expr
  = do
      arg     <- newLocalVar (fsLit "x") arg_ty
      varg    <- toVect arg_ty (Var arg)
      varg_ty <- vectType arg_ty
      vres_ty <- vectType res_ty
      apply   <- builtin applyVar
      body    <- fromVect res_ty
               $ Var apply `mkTyApps` [varg_ty, vres_ty] `mkApps` [expr, varg]
      return $ Lam arg body

-- If the type isn't a function, then we can't current convert it unless the type is scalar (i.e.,
-- is identical to the non-vectorised version).
--
fromVect ty expr
  = identityConv ty >> return expr

-- Convert an expression such that it evaluates to the vectorised equivalent of the value of the
-- original expression.
--
-- WARNING: Currently only works for the scalar types, where the vectorised value coincides with the
--          original one.
--
toVect :: Type -> CoreExpr -> VM CoreExpr
toVect ty expr = identityConv ty >> return expr

-- |Check that the type is neutral under type vectorisation — i.e., all involved type constructor
-- are not altered by vectorisation as they contain no parallel arrays.
--
identityConv :: Type -> VM ()
identityConv ty 
  | Just ty' <- coreView ty 
  = identityConv ty'
identityConv (TyConApp tycon tys)
  = do { mapM_ identityConv tys
       ; identityConvTyCon tycon
       }
identityConv (LitTy {})    = noV $ text "identityConv: not sure about literal types under vectorisation"
identityConv (TyVarTy {})  = noV $ text "identityConv: type variable changes under vectorisation"
identityConv (AppTy {})    = noV $ text "identityConv: type appl. changes under vectorisation"
identityConv (FunTy {})    = noV $ text "identityConv: function type changes under vectorisation"
identityConv (ForAllTy {}) = noV $ text "identityConv: quantified type changes under vectorisation"

-- |Check that this type constructor is not changed by vectorisation — i.e., it does not embed any
-- parallel arrays.
--
identityConvTyCon :: TyCon -> VM ()
identityConvTyCon tc
  = do 
    { isParallel <- (tyConName tc `elemNameSet`) <$> globalParallelTyCons
    ; parray     <- builtin parrayTyCon
    ; if isParallel && not (tc == parray)
      then noV idErr
      else return ()
    }
  where
    idErr = text "identityConvTyCon: type constructor contains parallel arrays" <+> ppr tc
