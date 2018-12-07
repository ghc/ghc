{-
Worker/wrapper transformation for type directed etaExpansion.

To be done as part of tidying just before translation to STG.
-}

module CoreEta (arityWorkerWrapper) where

import GhcPrelude

import BasicTypes
import CoreSyn
import CoreArity
import FastString
import Id
import Name
import OccName
import Panic
import TyCoRep
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
arityWorkerWrapper :: CoreBind -> [CoreBind]
arityWorkerWrapper (NonRec name expr)
  = map (uncurry NonRec) (arityWorkerWrapper' name expr)
arityWorkerWrapper (Rec binds)
  = [Rec (concatMap (uncurry arityWorkerWrapper') binds)]

-- ^ Change a function binding into a call to its wrapper and the production of
-- a wrapper.
arityWorkerWrapper' :: CoreBndr -> CoreExpr -> [(CoreBndr,CoreExpr)]
arityWorkerWrapper' name expr
  = let arity = idArity name in
      case arity >= 1 of
        True -> [ mkArityWrapper name expr arity
                , mkArityWorker  name expr arity ]
        False -> [(name,expr)]

-- ^ Create the new type for an extensional function
mkArityType :: Type -> Arity -> Type
mkArityType (ForAllTy x ty) n = ForAllTy x (mkArityType ty n)
mkArityType (FunTy a b) n = FunTildeTy a (mkArityType b (n-1))
mkArityType ty _ = ty

mkWorkerName :: Id -> Id
mkWorkerName bndr
  = mkGlobalVar (idDetails bndr) name (varType bndr) (idInfo bndr)
  where name = mkSystemName
                 (stepUnique (nameUnique name) 4242) -- TODO: this looks utterly unsafe
                 (mkOccName OccName.varName
                            (occNameString (nameOccName (Var.varName bndr))
                             ++ "worker"))

-- ^ Given an expression and it's name, generate a new expression with a
-- tilde-lambda type. For expressions that are not functions, we do not generate
-- a worker
mkArityWorker :: CoreBndr -> CoreExpr -> Arity -> (CoreBndr,CoreExpr)
mkArityWorker name expr arity =
  ( setVarType (mkWorkerName name) (mkArityType (idType name) arity)
  , expr )

-- ^ The wrapper does not change the type. It leaves anything that is not a
-- lambda unchanged
mkArityWrapper
  :: CoreBndr
  -> CoreExpr
  -> Arity
  -> (CoreBndr,CoreExpr)
mkArityWrapper name expr arity
  = (name, mkArityWrapper' expr arity (mkWorkerName name) [])

mkArityWrapper' :: CoreExpr -> Arity -> CoreBndr -> [CoreBndr] -> CoreExpr
mkArityWrapper' (Lam bndr expr) arity w ls
  | isTyVar bndr = Lam bndr (mkArityWrapper' expr arity w (bndr : ls))
  | arity == 1   = Lam bndr (mkApps (Var w) (fmap Var ls))
  | otherwise    = Lam bndr (mkArityWrapper' expr (arity-1) w (bndr : ls))
mkArityWrapper' expr _ _ _ = expr
