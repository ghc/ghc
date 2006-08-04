--  $Id$
--
--  Copyright (c) 2002 Manuel M T Chakravarty & Gabriele Keller
--  
--  Analysis phase for an optimised flattening transformation
--
--- DESCRIPTION ---------------------------------------------------------------
--
--  This module implements an analysis phase that identifies Core expressions
--  that need not be transformed during flattening.  The expressions when
--  executed in a parallel context are implemented as an iteration over the
--  original scalar computation, instead of vectorising the computation.  This
--  usually improves efficiency by increasing locality and also reduces code
--  size. 
--
--- DOCU ----------------------------------------------------------------------
--
--  Language: Haskell 98 with C preprocessor
--
-- Analyse the expression and annotate each simple subexpression accordingly. 
--
--  The result of the analysis is stored in a new field in IdInfo (has yet to
--  be extended)
--
--  A simple expression is any expression which is not a function, not of
--  recursive type and does not contain a value of PArray type. Polymorphic
--  variables are simple expressions even though they might be instantiated to
--  a parray value or function.
--
--- TODO ----------------------------------------------------------------------
--

module PArrAnal (
  markScalarExprs	-- :: [CoreBind] -> [CoreBind]
) where

import Panic   (panic)
import Outputable (pprPanic, ppr)
import CoreSyn (CoreBind)

import TypeRep      (Type(..))
import Var (Var(..),Id)
import Literal      (Literal)
import CoreSyn (Expr(..),CoreExpr,Bind(..))
import PprCore ( {- instances -} )
-- 

data ArrayUsage = Prim | NonPrim | Array 
                | PolyExpr (Id -> Maybe (ArrayUsage -> ArrayUsage))
                | PolyFun (ArrayUsage -> ArrayUsage)

         
arrUsage:: CoreExpr -> ArrayUsage
arrUsage (Var id)  = varArrayUsage id
arrUsage (Lit lit) = litArrayUsage lit
arrUsage (App expr1 expr2) =
  let
    arr1 = arrUsage expr1
    arr2 = arrUsage expr2
  in 
  case (arr1, arr2) of   
    (_,        Array)  -> Array
    (PolyFun f, _)     -> f arr2
    (_, _)             -> arr1

arrUsage (Lam b expr) =
  bindType (b, expr)

arrUsage (Let (NonRec b expr1) expr2) =
  arrUsage (App (Lam b expr2) expr1)

arrUsage (Let (Rec bnds) expr) =
  let 
    t1 = foldr combineArrayUsage Prim (map bindType bnds)
    t2 = arrUsage expr
  in if isArrayUsage t1 then Array else t2

arrUsage (Case expr b _ alts) = 
  let 
    t1 = arrUsage expr
    t2 = scanType (map (arrUsage . (\ (_,_,x) -> x)) alts)
  in scanType [t1, t2]

arrUsage (Note n expr) =
  arrUsage expr

arrUsage (Type t) =
  typeArrayUsage  t

-- not quite sure this is right
arrUsage (Cast expr co) =
  arrUsage expr 

bindType (b, expr) =
  let
    bT    = varArrayUsage b
    exprT = arrUsage expr
  in case (bT, exprT) of
       (Array, _) -> Array
       _          -> exprT

scanType:: [ArrayUsage] -> ArrayUsage
scanType [t]        = t
scanType (Array:ts) = Array
scanType (_:ts)     = scanType ts
  


-- the code expression represents a built-in function which generates
-- an array
isArrayGen:: CoreExpr -> Bool
isArrayGen _ = 
  panic "PArrAnal: isArrayGen: not yet implemented"

isArrayCon:: CoreExpr -> Bool
isArrayCon _ = 
  panic "PArrAnal: isArrayCon: not yet implemented"

markScalarExprs:: [CoreBind] -> [CoreBind]
markScalarExprs _ =
  panic "PArrAnal.markScalarExprs: not implemented yet"


varArrayUsage:: Id -> ArrayUsage
varArrayUsage =
  panic "PArrAnal.varArrayUsage: not yet implented"

litArrayUsage:: Literal -> ArrayUsage
litArrayUsage =
  panic "PArrAnal.litArrayUsage: not yet implented"


typeArrayUsage:: Type -> ArrayUsage
typeArrayUsage (TyVarTy tvar) = 
  PolyExpr (tIdFun tvar)
typeArrayUsage (AppTy _ _) =
   panic "PArrAnal.typeArrayUsage: AppTy case not yet implemented"
typeArrayUsage (TyConApp tc tcargs) =
  let
    tcargsAU = map typeArrayUsage tcargs
    tcCombine  = foldr combineArrayUsage Prim tcargsAU
  in auCon tcCombine
typeArrayUsage t@(PredTy _) =
  pprPanic "PArrAnal.typeArrayUsage: encountered 'PredType - shouldn't be here!"
           (ppr t)                 
 

combineArrayUsage:: ArrayUsage -> ArrayUsage -> ArrayUsage 
combineArrayUsage Array _  = Array 
combineArrayUsage _ Array  = Array 
combineArrayUsage (PolyExpr f1) (PolyExpr f2) =
  PolyExpr f'   
  where 
    f' var = 
      let
        f1lookup = f1 var
        f2lookup = f2 var
       in 
       case (f1lookup, f2lookup) of
         (Nothing, _) -> f2lookup
         (_, Nothing) -> f1lookup
         (Just f1', Just f2') -> Just ( \e -> (combineArrayUsage (f1' e) (f2' e)))
combineArrayUsage (PolyFun f) (PolyExpr g) = 
        panic ("PArrAnal.typeArrayUsage: PolyFun as argument in data" ++
               " constructor - should not (?) happen\n")
combineArrayUsage (PolyExpr g) (PolyFun f)  = 
        panic ("PArrAnal.typeArrayUsage: PolyFun as argument in data" ++
               " constructor - should not (?) happen\n")
combineArrayUsage NonPrim _ = NonPrim
combineArrayUsage _ NonPrim = NonPrim
combineArrayUsage Prim Prim = Prim


isArrayUsage:: ArrayUsage -> Bool
isArrayUsage Array = True
isArrayUsage _     = False

--  Functions to serve as arguments for PolyExpr
--  ---------------------------------------------

tIdFun:: Var -> Var -> Maybe (ArrayUsage -> ArrayUsage) 
tIdFun t tcomp =
  if t == tcomp then
     Just auId
  else
     Nothing  

-- Functions to serve as argument for PolyFun
-- -------------------------------------------

auId:: ArrayUsage -> ArrayUsage 
auId = id

auCon:: ArrayUsage -> ArrayUsage
auCon Prim = NonPrim
auCon (PolyExpr f) = PolyExpr f'
  where f' v  = case f v of
                   Nothing -> Nothing
                   Just g  -> Just  ( \e -> (auCon (g e)))
auCon (PolyFun f)  = PolyFun (auCon . f)
auCon _    = Array

-- traversal of Core expressions
-- -----------------------------

-- FIXME: implement

