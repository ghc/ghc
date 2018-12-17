{-
Worker/wrapper transformation for type directed etaExpansion.

To be done as part of tidying just before translation to STG.
-}

module CoreEta (arityWorkerWrapper) where

import GhcPrelude

import BasicTypes
import CoreSyn
import Id
import TyCoRep
import UniqSupply
import Unique
import Var

{-
************************************************************************
*                                                                      *
                   Call Arity in the Types
*                                                                      *
************************************************************************
-}

-- ^ Given a top level entity, produce the WorkerWrapper transformed
-- version. This transformation may or may not produce new top level entities
-- depending on its arity.
arityWorkerWrapper :: CoreBind -> UniqSM [CoreBind]
arityWorkerWrapper (NonRec name expr)
  = fmap (uncurry NonRec) <$> (arityWorkerWrapper' name expr)
arityWorkerWrapper (Rec binds)
  = do { bindss' <- mapM (uncurry arityWorkerWrapper') binds
       ; return [Rec (concat bindss')] }

-- ^ Change a function binding into a call to its wrapper and the production of
-- a wrapper.
arityWorkerWrapper' :: CoreBndr -> CoreExpr -> UniqSM [(CoreBndr,CoreExpr)]
arityWorkerWrapper' name expr
  = let arity = idCallArity name in
      case arity >= 1 of
        True ->
          do { uniq <- getUniqueM
             ; worker  <- mkArityWrapper name expr uniq arity
             ; wrapper <- mkArityWorker  name expr uniq arity
             ; return [worker,wrapper] }
        False -> return [(name,expr)]

-- ^ Create the new type for an extensional function
mkArityType :: Type -> Arity -> Type
mkArityType (ForAllTy x ty) n = ForAllTy x (mkArityType ty n)
mkArityType (FunTy _ a b) n = FunTildeTy a (mkArityType b (n-1))
mkArityType ty _ = ty

-- ^ Given an expression and it's name, generate a new expression with a
-- tilde-lambda type. For expressions that are not functions, we do not generate
-- a worker
mkArityWorker
  :: CoreBndr -> CoreExpr -> Unique -> Arity -> UniqSM (CoreBndr,CoreExpr)
mkArityWorker name expr uniq arity = return
  ( mkWorkerId uniq name (mkArityType (idType name) arity) , expr )

-- ^ The wrapper does not change the type. It leaves anything that is not a
-- lambda unchanged
mkArityWrapper
  :: CoreBndr -> CoreExpr -> Unique -> Arity -> UniqSM (CoreBndr,CoreExpr)
mkArityWrapper name expr uniq arity
  = do { expr' <- mkArityWrapper'
                    expr
                    arity
                    (mkWorkerId uniq name (varType name)) []
       ; return (name,expr') }

mkArityWrapper'
  :: CoreExpr -> Arity -> CoreBndr -> [CoreBndr] -> UniqSM CoreExpr
mkArityWrapper' (Lam bndr expr) arity w ls
  | isTyVar bndr   = Lam bndr <$> mkArityWrapper' expr arity w (bndr : ls)
  | isTcTyVar bndr = Lam bndr <$> mkArityWrapper' expr arity w (bndr : ls)
  | arity == 1     = return $ Lam bndr (mkApps (Var w) (fmap Var ls))
  | otherwise      = Lam bndr <$> (mkArityWrapper' expr (arity-1) w (bndr : ls))
mkArityWrapper' expr _ _ _ = return expr
