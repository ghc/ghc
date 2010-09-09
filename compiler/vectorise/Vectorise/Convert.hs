
module Vectorise.Convert
	(fromVect)
where
import Vectorise.Monad
import Vectorise.Builtins
import Vectorise.Type.Type

import CoreSyn
import TyCon
import Type
import TypeRep
import FastString


-- | Build an expression that calls the vectorised version of some 
--   function from a `Closure`.
--
--   For example
--   @   
--      \(x :: Double) -> 
--      \(y :: Double) -> 
--      ($v_foo $: x) $: y
--   @
--
--   We use the type of the original binding to work out how many
--   outer lambdas to add.
--
fromVect 
	:: Type 	-- ^ The type of the original binding.
	-> CoreExpr	-- ^ Expression giving the closure to use, eg @$v_foo@.
	-> VM CoreExpr
	
-- Convert the type to the core view if it isn't already.
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

-- If the type isn't a function then it's time to call on the closure.
fromVect ty expr
  = identityConv ty >> return expr


-- TODO: What is this really doing?
toVect :: Type -> CoreExpr -> VM CoreExpr
toVect ty expr = identityConv ty >> return expr


-- | Check that we have the vectorised versions of all the
--   type constructors in this type.
identityConv :: Type -> VM ()
identityConv ty 
  | Just ty' <- coreView ty 
  = identityConv ty'

identityConv (TyConApp tycon tys)
 = do mapM_ identityConv tys
      identityConvTyCon tycon

identityConv _ = noV


-- | Check that we have the vectorised version of this type constructor.
identityConvTyCon :: TyCon -> VM ()
identityConvTyCon tc
  | isBoxedTupleTyCon tc = return ()
  | isUnLiftedTyCon tc   = return ()
  | otherwise 
  = do tc' <- maybeV (lookupTyCon tc)
       if tc == tc' then return () else noV
