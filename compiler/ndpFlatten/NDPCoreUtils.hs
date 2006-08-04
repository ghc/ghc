--  $Id$
--
--  Copyright (c) [2001..2002] Manuel M T Chakravarty & Gabriele Keller
--
--  Auxiliary routines for NDP-related Core transformations.
--
--- DESCRIPTION ---------------------------------------------------------------
--
--  This module exports all functions to access and alter the `Type' data 
--  structure from modules `Type' and `CoreExpr' from `CoreSyn'.  As it is part
--  of the NDP flattening component, the functions provide access to all the
--  fields that are important for the flattening and lifting transformation.
-- 
--- DOCU ----------------------------------------------------------------------
--
--  Language: Haskell 98
--
--- TODO ----------------------------------------------------------------------
--

module NDPCoreUtils (

  -- type inspection functions
  --
  tupleTyArgs,		-- :: Type -> [Type]
  funTyArgs,		-- :: Type -> (Type, Type)
  parrElemTy,		-- :: Type -> Type

  -- Core generation functions
  --
  mkTuple,		-- :: [Type] -> [CoreExpr] -> CoreExpr
  mkInt,		-- :: CoreExpr -> CoreExpr

  -- query functions
  --
  isDefault,            -- :: CoreAlt -> Bool
  isLit,		-- :: [CoreAlt] -> Bool
  isSimpleExpr,		-- :: CoreExpr -> Bool

  -- re-exported functions
  --
  mkPArrTy,		-- :: Type -> Type
  boolTy,		-- :: Type
  
  -- substitution
  -- 
  substIdEnv
) where

-- GHC
import Panic      (panic)
import Outputable (Outputable(ppr), pprPanic)
import BasicTypes (Boxity(..))
import Type       (Type, splitTyConApp_maybe, splitFunTy)
import TyCon      (isTupleTyCon)
import TysWiredIn (parrTyCon, unitDataConId, tupleCon, intDataCon, mkPArrTy,
		   boolTy) 
import CoreSyn    (CoreExpr, CoreAlt, Expr(..), AltCon(..),
		   Bind(..), mkConApp)
import PprCore	  ( {- instances -} )
import Var        (Id)
import VarEnv     (IdEnv, delVarEnv, delVarEnvList, lookupVarEnv)

-- friends: don't import any to avoid cyclic imports
-- 


-- type inspection functions
-- -------------------------

-- determines the argument types of a tuple type (EXPORTED)
--
tupleTyArgs    :: Type -> [Type]
tupleTyArgs ty  =
  case splitTyConApp_maybe ty of
    Just (tyCon, argTys) | isTupleTyCon tyCon -> argTys
    _					      -> 
      pprPanic "NDPCoreUtils.tupleTyArgs: wrong type: " (ppr ty)

-- determines the argument and result type of a function type (EXPORTED)
--
funTyArgs :: Type -> (Type, Type)
funTyArgs  = splitFunTy

-- for a type of the form `[:t:]', yield `t' (EXPORTED)
--
--  * if the type has any other form, a fatal error occurs
--
parrElemTy    :: Type -> Type
parrElemTy ty  = 
  case splitTyConApp_maybe ty of
    Just (tyCon, [argTy]) | tyCon == parrTyCon -> argTy
    _							     -> 
      pprPanic "NDPCoreUtils.parrElemTy: wrong type: " (ppr ty)


-- Core generation functions
-- -------------------------

-- make a tuple construction expression from a list of argument types and
-- argument values (EXPORTED)
--
--  * the two lists need to be of the same length
--
mkTuple                                  :: [Type] -> [CoreExpr] -> CoreExpr
mkTuple []  []                            = Var unitDataConId
mkTuple [_] [e]                           = e
mkTuple ts  es  | length ts == length es  = 
  mkConApp (tupleCon Boxed (length es)) (map Type ts ++ es)
mkTuple _   _                             =
  panic "NDPCoreUtils.mkTuple: mismatch between number of types and exprs!"

-- make a boxed integer from an unboxed one (EXPORTED)
--
mkInt   :: CoreExpr -> CoreExpr
mkInt e  = mkConApp intDataCon [e]


-- query functions
-- ---------------

-- checks whether a given case alternative is a default alternative (EXPORTED)
--
isDefault                 :: CoreAlt -> Bool
isDefault (DEFAULT, _, _)  = True
isDefault _                = False

-- check whether a list of case alternatives in belongs to a case over a
-- literal type (EXPORTED) 
--
isLit			      :: [CoreAlt] -> Bool
isLit ((DEFAULT, _, _ ):alts)  = isLit alts
isLit ((LitAlt _, _, _):_   )  = True
isLit _                        = False

-- FIXME: this function should get a more expressive name and maybe also a
--	  more detailed return type (depends on how the analysis goes)
isSimpleExpr:: CoreExpr -> Bool
isSimpleExpr _ =
  -- FIXME
  False


--  Substitution
--  -------------

substIdEnv:: IdEnv Id -> CoreExpr -> CoreExpr
substIdEnv env e@(Lit _) = e
substIdEnv env e@(Var id)  =
  case (lookupVarEnv env id) of
    Just v -> (Var v)
    _      -> e
substIdEnv env (App e arg) =
  App (substIdEnv env e) (substIdEnv env arg)
substIdEnv env (Lam b expr) =
  Lam b (substIdEnv (delVarEnv env b) expr)
substIdEnv env (Let (NonRec b expr1) expr2) =
  Let (NonRec b (substIdEnv env expr1)) 
         (substIdEnv (delVarEnv env b) expr2)
substIdEnv env (Let (Rec bnds) expr) = 
   let 
     newEnv  = delVarEnvList env (map fst bnds)
     newExpr = substIdEnv newEnv expr 
     substBnd (b,e) = (b, substIdEnv newEnv e)      
   in Let (Rec (map substBnd bnds)) newExpr
substIdEnv env (Case expr b ty alts) =
   Case (substIdEnv newEnv expr) b ty (map substAlt alts)
   where
     newEnv = delVarEnv env b
     substAlt (c, bnds, expr) =
       (c, bnds, substIdEnv (delVarEnvList env bnds) expr)
substIdEnv env (Note n expr) =
  Note n (substIdEnv env expr)
substIdEnv env (Cast e co) = Cast (substIdEnv env e) co
substIdEnv env e@(Type t) = e
