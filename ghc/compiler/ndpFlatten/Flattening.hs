--  $Id$
--
--  Copyright (c) [2001..2002] Manuel M T Chakravarty & Gabriele Keller
--  
--  Vectorisation and lifting
--
--- DESCRIPTION ---------------------------------------------------------------
--
--  This module implements the vectorisation and function lifting
--  transformations of the flattening transformation.
-- 
--- DOCU ----------------------------------------------------------------------
--
--  Language: Haskell 98 with C preprocessor
--
--  Types: 
--    the transformation on types has five purposes:
--
--        1) for each type definition, derive the lifted version of this type
--             liftTypeef
--        2) change the type annotations of functions & variables acc. to rep.
--             flattenType
--        3) derive the type of a lifted function
--             liftType
--        4) sumtypes:
--             this is the most fuzzy and complicated part. For each lifted
--             sumtype we need to generate function to access and combine the
--             component arrays
--
--   NOTE: the type information of variables and data constructors is *not*
--          changed to reflect it's representation. This has to be solved 
--          somehow (???, FIXME)  using type indexed types
--
--   Vectorisation:
--    is very naive at the moment. One of the most striking inefficiencies is
--    application vect (app e1 e2) -> app (fst (vect e1) (vect e2)) if e1 is a
--    lambda abstraction. The vectorisation produces a pair consisting of the
--    original and the lifted function, but the lifted version is discarded.
--    I'm also not sure how much of this would be thrown out by the simplifier
--    eventually
--
--        *) vectorise
--
--  Conventions:
--
--- TODO ----------------------------------------------------------------------
--
--   * look closer into the definition of type definition (TypeThing or so)
--

module Flattening (
  flatten, flattenExpr, 
) where 

#include "HsVersions.h"

-- friends
import NDPCoreUtils (tupleTyArgs, funTyArgs, parrElemTy, isDefault,
		     isLit, mkPArrTy, mkTuple, isSimpleExpr, substIdEnv)
import FlattenMonad (Flatten, runFlatten, mkBind, extendContext, packContext,
		     liftVar, liftConst, intersectWithContext, mk'fst,
		     mk'lengthP, mk'replicateP, mk'mapP, mk'bpermuteDftP,
		     mk'indexOfP,mk'eq,mk'neq) 

-- GHC
import StaticFlags  (opt_Flatten)
import Panic        (panic)
import ErrUtils     (dumpIfSet_dyn)
import UniqSupply   (mkSplitUniqSupply)
import DynFlags  (DynFlag(..))
import Literal      (Literal, literalType)
import Var	    (Var(..), idType, isTyVar)
import Id	    (setIdType)
import DataCon	    (DataCon, dataConTag)
import TypeRep      (Type(..))
import HscTypes	    ( ModGuts(..), ModGuts, HscEnv(..), hscEPS )
import CoreFVs	    (exprFreeVars)
import CoreSyn	    (Expr(..), Bind(..), Alt(..), AltCon(..), Note(..),
		     CoreBndr, CoreExpr, CoreBind, mkLams, mkLets,
		     mkApps, mkIntLitInt)  
import PprCore      (pprCoreExpr)
import CoreLint	    (showPass, endPass)

import CoreUtils    (exprType, applyTypeToArg, mkPiType)
import VarEnv       (zipVarEnv)
import TysWiredIn   (mkTupleTy)
import BasicTypes   (Boxity(..))
import Outputable
import FastString


-- FIXME: fro debugging - remove this
import TRACE    (trace)

-- standard
import Monad        (liftM, foldM)

-- toplevel transformation
-- -----------------------

-- entry point to the flattening transformation for the compiler driver when
-- compiling a complete module (EXPORTED) 
--
flatten :: HscEnv
	-> ModGuts
	-> IO ModGuts
flatten hsc_env mod_impl@(ModGuts {mg_binds = binds}) 
  | not opt_Flatten = return mod_impl -- skip without -fflatten
  | otherwise       =
  do
    let dflags = hsc_dflags hsc_env

    eps <- hscEPS hsc_env
    us <- mkSplitUniqSupply 'l'		-- 'l' as in fLattening
    --
    -- announce vectorisation
    --
    showPass dflags "Flattening [first phase: vectorisation]"
    --
    -- vectorise all toplevel bindings
    --
    let binds' = runFlatten hsc_env eps us $ vectoriseTopLevelBinds binds
    --
    -- and dump the result if requested
    --
    endPass dflags "Flattening [first phase: vectorisation]" 
	    Opt_D_dump_vect binds'
    return $ mod_impl {mg_binds = binds'}

-- entry point to the flattening transformation for the compiler driver when
-- compiling a single expression in interactive mode (EXPORTED) 
--
flattenExpr :: HscEnv
	    -> CoreExpr			-- the expression to be flattened
	    -> IO CoreExpr
flattenExpr hsc_env expr
  | not opt_Flatten = return expr       -- skip without -fflatten
  | otherwise       =
  do
    let dflags = hsc_dflags hsc_env
    eps <- hscEPS hsc_env

    us <- mkSplitUniqSupply 'l'		-- 'l' as in fLattening
    --
    -- announce vectorisation
    --
    showPass dflags "Flattening [first phase: vectorisation]"
    --
    -- vectorise the expression
    --
    let expr' = fst . runFlatten hsc_env eps us $ vectorise expr
    --
    -- and dump the result if requested
    --
    dumpIfSet_dyn dflags Opt_D_dump_vect "Vectorised expression"
		  (pprCoreExpr expr')
    return expr'


-- vectorisation of bindings and expressions
-- -----------------------------------------


vectoriseTopLevelBinds:: [CoreBind] -> Flatten [CoreBind]
vectoriseTopLevelBinds binds =
  do
    vbinds <- mapM vectoriseBind binds
    return (adjustTypeBinds vbinds)

adjustTypeBinds:: [CoreBind] -> [CoreBind]
adjustTypeBinds vbinds =
    let 
       ids = concat (map extIds vbinds)
       idEnv =  zipVarEnv ids ids
     in map (substIdEnvBind idEnv) vbinds
  where 
    -- FIXME replace by 'bindersOf'
    extIds (NonRec b expr) = [b]
    extIds (Rec      bnds) = map fst bnds
    substIdEnvBind idEnv (NonRec b expr) = NonRec b (substIdEnv idEnv expr)
    substIdEnvBind idEnv (Rec bnds)      
       = Rec (map (\ (b,e) -> (b, (substIdEnv idEnv e))) bnds) 

-- vectorise a single core binder
--
vectoriseBind	              :: CoreBind -> Flatten CoreBind
vectoriseBind (NonRec b expr)  = 
  liftM (NonRec b) $ liftM fst $ vectorise expr
vectoriseBind (Rec bindings)   = 
  liftM Rec        $ mapM vectoriseOne bindings
  where
    vectoriseOne (b, expr) = 
      do
	(vexpr, ty) <- vectorise expr
	return (setIdType b ty, vexpr)


-- Searches for function definitions and creates a lifted version for 
-- each function.
-- We have only two interesting cases:
-- 1) function application  (ex1) (ex2)
--      vectorise both subexpressions. The function will end up becoming a
--      pair (orig. fun, lifted fun), choose first component (in many cases,
--      this is pretty inefficient, since the lifted version is generated
--      although it is clear that it won't be used
-- 
-- 2) lambda abstraction
--      any function has to exist in two forms: it's original form and it's 
--      lifted form. Therefore, every lambda abstraction is transformed into
--      a pair of functions: the original function and its lifted variant
-- 
--
--  FIXME: currently, I use 'exprType' all over the place - this is terribly
--  inefficient. It should be suffiecient to change 'vectorise' and 'lift' to
--  return the type of the result expression as well.
--
vectorise:: CoreExpr -> Flatten (CoreExpr, Type)
vectorise (Var id)  =  
  do 
    let varTy  = idType id
    let vecTy  = vectoriseTy varTy
    return (Var (setIdType id vecTy), vecTy)

vectorise (Lit lit) =  
  return ((Lit lit), literalType lit) 


vectorise e@(App expr t@(Type _)) = 
  do 
    (vexpr, vexprTy) <- vectorise expr
    return ((App vexpr t), applyTypeToArg vexprTy t) 

vectorise  (App (Lam b expr) arg) =
  do
    (varg, argTy)    <- vectorise arg
    (vexpr, vexprTy) <- vectorise expr
    let vb            = setIdType b argTy
    return ((App (Lam vb  vexpr) varg), 
            applyTypeToArg (mkPiType vb vexprTy) varg)

-- if vexpr expects a type as first argument
-- application stays just as it is
--
vectorise (App expr arg) =          
  do 
    (vexpr, vexprTy) <-  vectorise expr
    (varg,  vargTy)  <-  vectorise arg

    if (isPolyType vexprTy)
      then do
        let resTy =  applyTypeToArg vexprTy varg
        return (App vexpr varg, resTy)
      else do 
        let [t1, t2] = tupleTyArgs  vexprTy
        vexpr'      <-  mk'fst t1 t2 vexpr
        let resTy    = applyTypeToArg t1 varg   
        return  ((App vexpr' varg), resTy)  -- apply the first component of
                                            -- the vectorized function
  where
    isPolyType t =  
        (case t  of
           (ForAllTy _ _)  -> True
           (NoteTy _ nt)   -> isPolyType nt
           _               -> False)
    

vectorise  e@(Lam b expr)
  | isTyVar b
  =  do
        (vexpr, vexprTy) <- vectorise expr          -- don't vectorise 'b'!
        return ((Lam b vexpr), mkPiType b vexprTy)
  | otherwise =
     do          
       (vexpr, vexprTy)  <- vectorise expr
       let vb             = setIdType b (vectoriseTy (idType b))
       let ve             =  Lam  vb  vexpr 
       (lexpr, lexprTy)  <- lift e
       let veTy = mkPiType vb vexprTy  
       return $ (mkTuple [veTy, lexprTy] [ve, lexpr], 
                 mkTupleTy Boxed 2 [veTy, lexprTy])

vectorise (Let bind body) = 
  do    
    vbind            <- vectoriseBind bind
    (vbody, vbodyTy) <- vectorise body
    return ((Let vbind vbody), vbodyTy)

vectorise (Case expr b ty alts) =
  do 
    (vexpr, vexprTy) <- vectorise expr
    valts <- mapM vectorise' alts
    let res_ty = snd (head valts)
    return (Case vexpr (setIdType b vexprTy) res_ty (map fst valts), res_ty)
  where vectorise' (con, bs, expr) = 
          do 
            (vexpr, vexprTy) <- vectorise expr
            return ((con, bs, vexpr), vexprTy)  -- FIXME: change type of con
                                                --   and bs



vectorise (Note note expr) = 
 do 
   (vexpr, vexprTy) <- vectorise expr        -- FIXME: is this ok or does it
   return ((Note note vexpr), vexprTy)       --   change the validity of note?

vectorise e@(Type t) = 
  return (e, t)                              -- FIXME: panic instead of 't'???


{-
myShowTy (TyVarTy _) = "TyVar "
myShowTy (AppTy t1 t2) = 
  "AppTy (" ++ (myShowTy t1) ++ ", " ++ (myShowTy t2) ++ ")"
myShowTy (TyConApp _ t) =
  "TyConApp TC (" ++ (myShowTy t) ++ ")"
-}

vectoriseTy :: Type -> Type 
vectoriseTy t@(TyVarTy v)      =  t
vectoriseTy t@(AppTy t1 t2)    = 
  AppTy (vectoriseTy t1) (vectoriseTy t2)
vectoriseTy t@(TyConApp tc ts) = 
  TyConApp tc (map vectoriseTy ts)
vectoriseTy t@(FunTy t1 t2)    = 
  mkTupleTy Boxed 2 [(FunTy (vectoriseTy t1) (vectoriseTy t2)), 
                     (liftTy t)]
vectoriseTy  t@(ForAllTy v ty)  = 
  ForAllTy v (vectoriseTy  ty)
vectoriseTy t@(NoteTy note ty) =  -- FIXME: is the note still valid after
  NoteTy note  (vectoriseTy ty)   --   this or should we just throw it away
vectoriseTy  t =  t


-- liftTy: wrap the type in an array but be careful with function types
--    on the *top level* (is this sufficient???)

liftTy:: Type -> Type
liftTy (FunTy t1 t2)   = FunTy (liftTy t1) (liftTy t2)
liftTy (ForAllTy tv t) = ForAllTy tv (liftTy t)
liftTy (NoteTy n t)    = NoteTy n $ liftTy t
liftTy  t              = mkPArrTy t


--  lifting:
-- ----------
--  * liftType
--  * lift


-- liftBinderType: Converts a  type 'a' stored in the binder to the
-- representation of '[:a:]' will therefore call liftType
--  
--  lift type, don't change name (incl unique) nor IdInfo. IdInfo looks ok,
--  but I'm not entirely sure about some fields (e.g., strictness info)
liftBinderType:: CoreBndr ->  Flatten CoreBndr
liftBinderType bndr = return $  setIdType bndr (liftTy (idType bndr))

-- lift: lifts an expression (a -> [:a:])
-- If the expression is a simple expression, it is treated like a constant
-- expression. 
-- If the body of a lambda expression is a simple expression, it is
-- transformed into a mapP
lift:: CoreExpr -> Flatten (CoreExpr, Type)
lift cExpr@(Var id)    = 
  do
    lVar@(Var lId) <- liftVar id
    return (lVar, idType lId)

lift cExpr@(Lit lit)   = 
  do
    lLit  <- liftConst cExpr
    return (lLit, exprType lLit)   
                                   

lift (Lam b expr)
  | isSimpleExpr expr      =  liftSimpleFun b expr
  | isTyVar b = 
    do
      (lexpr, lexprTy) <- lift expr  -- don't lift b!
      return (Lam b lexpr, mkPiType b lexprTy)
  | otherwise =
    do
      lb               <- liftBinderType b
      (lexpr, lexprTy) <- extendContext [lb] (lift expr)
      return ((Lam lb lexpr) , mkPiType lb lexprTy)

lift (App expr1 expr2) = 
  do
    (lexpr1, lexpr1Ty) <- lift expr1
    (lexpr2, _)        <- lift expr2
    return ((App lexpr1 lexpr2), applyTypeToArg lexpr1Ty lexpr2)


lift (Let (NonRec b expr1) expr2) 
  |isSimpleExpr expr2 =
    do  			
      (lexpr1, _)        <- lift expr1
      (lexpr2, lexpr2Ty) <- liftSimpleFun b expr2
      let (t1, t2) = funTyArgs lexpr2Ty
      liftM (\x -> (x, liftTy t2)) $  mk'mapP t1 t2 lexpr2 lexpr1 

  | otherwise =
    do 
      (lexpr1, _)        <- lift expr1
      lb                 <- liftBinderType b
      (lexpr2, lexpr2Ty) <- extendContext [lb] (lift expr1)
      return ((Let (NonRec lb lexpr1) lexpr2), lexpr2Ty)

lift (Let (Rec binds) expr2) =
  do
    let (bndVars, exprs)  = unzip binds
    lBndVars           <- mapM liftBinderType bndVars 
    lexprs             <- extendContext bndVars (mapM lift exprs)
    (lexpr2, lexpr2Ty) <- extendContext bndVars (lift expr2)
    return ((Let (Rec (zip  lBndVars (map fst lexprs))) lexpr2), lexpr2Ty)

-- FIXME: 
-- Assumption: alternatives can either be literals or data construtors.
--             Due to type restrictions, I don't think it is possible 
--             that they are mixed.
--             The handling of literals and data constructors is completely
--             different
--
--
-- let b = expr in alts
--
-- I think I read somewhere that the default case (if present) is stored
-- in the head of the list. Assume for now this is true, have to check
--
-- (1) literals
-- (2) data constructors
--
-- FIXME: optimisation: first, filter out all simple expression and 
--   loop (mapP & filter) over all the corresponding values in a single
--   traversal:
							     
--    (1) splitAlts:: [Alt CoreBndr] -> ([Alt CoreBndr],[Alt CoreBndr])
--                                       simple alts     reg alts
--    (2) if simpleAlts = [] then (just as before)
--        if regAlts    = [] then (the whole thing is just a loop)
--        otherwise (a) compute index vector for simpleAlts (for def permute
--                      later on
--                  (b) 
-- gaw 2004 FIX? 
lift cExpr@(Case expr b _ alts)  =
  do  
    (lExpr, _) <- lift expr
    lb    <- liftBinderType  b     -- lift alt-expression
    lalts <- if isLit alts 
                then extendContext [lb] (liftCaseLit b alts)
                else extendContext [lb] (liftCaseDataCon b alts)
    letWrapper lExpr b lalts

lift (Note (Coerce t1 t2) expr) =
  do  
    (lexpr, t) <- lift expr
    let lt1 = liftTy t1
    return ((Note (Coerce lt1 (liftTy t2)) lexpr), lt1)

lift (Note note expr) =
  do 
    (lexpr, t) <- lift expr
    return ((Note note lexpr), t)

lift e@(Type t) = return (e, t)


-- auxilliary functions for lifting of case statements 
--

liftCaseDataCon:: CoreBndr -> [Alt CoreBndr] -> 
       Flatten (([CoreBind], [CoreBind], [CoreBind]))
liftCaseDataCon b [] =
  return ([], [], [])
liftCaseDataCon b alls@(alt:alts)
  | isDefault alt  =
    do
      (i,  e,  defAltBndrs) <- liftCaseDataConDefault b alt alts 
      (is, es, altBndrs)    <- liftCaseDataCon' b alts 
      return (i:is, e:es, defAltBndrs ++ altBndrs)
  | otherwise =
    liftCaseDataCon' b alls

liftCaseDataCon':: CoreBndr -> [Alt CoreBndr] ->  
    Flatten ([CoreBind], [CoreBind], [CoreBind])
liftCaseDataCon' _ [] =
  do
    return ([], [], []) 


liftCaseDataCon' b ((DataAlt dcon, bnds, expr): alts) =
  do
    (permBnd, exprBnd, packBnd)    <-  liftSingleDataCon b dcon bnds expr   
    (permBnds, exprBnds, packBnds) <-  liftCaseDataCon' b alts 
    return (permBnd:permBnds, exprBnd:exprBnds, packBnd ++ packBnds)


-- FIXME: is is really necessary to return the binding to the permutation
-- array in the data constructor case, as the representation already 
-- contains the extended flag vector
liftSingleDataCon:: CoreBndr -> DataCon -> [CoreBndr] -> CoreExpr ->
  Flatten (CoreBind, CoreBind, [CoreBind])
liftSingleDataCon b dcon bnds expr =
  do 
    let dconId           = dataConTag dcon
    indexExpr           <- mkIndexOfExprDCon (idType b)  b dconId
    (bb, bbind)         <- mkBind FSLIT("is") indexExpr
    lbnds               <- mapM liftBinderType bnds
    ((lExpr, _), bnds') <- packContext  bb (extendContext lbnds (lift expr))
    (_, vbind)          <- mkBind FSLIT("r") lExpr
    return (bbind, vbind, bnds')

-- FIXME: clean this up. the datacon and the literal case are so
--   similar that it would be easy to use the same function here
--   instead of duplicating all the code.
--
liftCaseDataConDefault:: CoreBndr -> (Alt CoreBndr) ->  [Alt CoreBndr] 
  ->  Flatten (CoreBind, CoreBind, [CoreBind])
liftCaseDataConDefault b (_, _, def) alts =
  do
    let dconIds        = map (\(DataAlt d, _, _) -> dataConTag d) alts
    indexExpr         <- mkIndexOfExprDConDft (idType b) b dconIds
    (bb, bbind)       <- mkBind FSLIT("is") indexExpr
    ((lDef, _), bnds) <- packContext  bb (lift def)     
    (_, vbind)        <- mkBind FSLIT("r") lDef
    return (bbind, vbind, bnds)

-- liftCaseLit: checks if we have a default case and handles it 
-- if necessary
liftCaseLit:: CoreBndr -> [Alt CoreBndr] -> 
       Flatten ([CoreBind], [CoreBind], [CoreBind])
liftCaseLit b [] =
    return ([], [], [])    --FIXME: a case with no cases at all???
liftCaseLit b alls@(alt:alts)
  | isDefault alt  =
    do
        (i,  e,  defAltBndrs) <- liftCaseLitDefault b alt alts 
        (is, es, altBndrs)    <- liftCaseLit' b alts 
        return (i:is, e:es, defAltBndrs ++ altBndrs)
  | otherwise = 
    do 
      liftCaseLit' b alls 

-- liftCaseLitDefault: looks at all the other alternatives which 
--    contain a literal and filters all those elements from the 
--    array which do not match any of the literals in the other
--    alternatives.
liftCaseLitDefault:: CoreBndr -> (Alt CoreBndr) ->  [Alt CoreBndr] 
  ->  Flatten (CoreBind, CoreBind, [CoreBind])
liftCaseLitDefault b (_, _, def) alts =
  do
    let lits           = map (\(LitAlt l, _, _) -> l) alts
    indexExpr         <- mkIndexOfExprDft (idType b) b lits
    (bb, bbind)       <- mkBind FSLIT("is") indexExpr
    ((lDef, _), bnds) <- packContext  bb (lift def)     
    (_, vbind)        <- mkBind FSLIT("r") lDef
    return (bbind, vbind, bnds)

-- FIXME: 
--  Assumption: in case of Lit, the list of binders of the alt is empty.
--
-- returns 
--   a list of all vars bound to the expr in the body of the alternative
--   a list of (var, expr) pairs, where var has to be bound to expr
--   by letWrapper
liftCaseLit':: CoreBndr -> [Alt CoreBndr] ->  
    Flatten ([CoreBind], [CoreBind], [CoreBind])						       
liftCaseLit' _ [] =
  do
    return ([], [], [])
liftCaseLit' b ((LitAlt lit, [], expr):alts) =
  do
    (permBnd, exprBnd, packBnd)    <-  liftSingleCaseLit b lit expr 
    (permBnds, exprBnds, packBnds) <-  liftCaseLit' b alts 
    return (permBnd:permBnds, exprBnd:exprBnds, packBnd ++ packBnds)

-- lift a single alternative of the form: case  b of lit -> expr. 
--    
--   It returns the bindings:
--   (a) let b' = indexOfP (mapP (\x -> x == lit) b)
--
--   (b) lift expr in the packed context. Returns lexpr and the
--       list of binds (bnds) that describe the packed arrays
--
--   (c) create new var v' to bind lexpr to
--
--   (d) return (b' = indexOf...., v' = lexpr, bnds)
liftSingleCaseLit:: CoreBndr -> Literal -> CoreExpr  -> 
  Flatten (CoreBind, CoreBind, [CoreBind])
liftSingleCaseLit b lit expr =
 do 
   indexExpr          <- mkIndexOfExpr (idType b) b lit -- (a)
   (bb, bbind)        <- mkBind FSLIT("is") indexExpr
   ((lExpr, t), bnds) <- packContext  bb (lift expr)     -- (b)         
   (_, vbind)         <- mkBind FSLIT("r") lExpr
   return (bbind, vbind, bnds)

-- letWrapper lExpr b ([indexbnd_i], [exprbnd_i], [pckbnd_ij])
-- 
-- let b = lExpr in
--  let index_bnd_1 in
--    let packbnd_11 in
--      ... packbnd_1m in 
--         let exprbnd_1 in        ....
--      ...
--          let nvar = replicate dummy (length <current context>)
--               nvar1 = bpermuteDftP index_bnd_1 ...
--
--   in bpermuteDftP index_bnd_n nvar_(n-1)
--
letWrapper:: CoreExpr -> CoreBndr ->([CoreBind], [CoreBind], [CoreBind]) ->
  Flatten (CoreExpr, Type)
letWrapper lExpr b (indBnds, exprBnds, pckBnds)  =
  do 
    (defBpBnds, ty) <- dftbpBinders indBnds exprBnds
    let resExpr      = getExprOfBind (head defBpBnds)
    return ((mkLets (indBnds ++ pckBnds ++ exprBnds ++ defBpBnds) resExpr), ty)

-- dftbpBinders: return the list of binders necessary to construct the overall
--   result from the subresults computed in the different branches of the case
--   statement. The binding which contains the final result is in the *head*
--   of the result list.
-- 
-- dftbpBinders [ind_i = ...] [expr_i = ...] = [dn = ..., d_n-1 = .., d1 = ...]
--
-- let def = replicate (length of context) undefined
--     d1  = bpermuteDftP dft e1 i1
--     .....
--
dftbpBinders:: [CoreBind] -> [CoreBind] -> Flatten ([CoreBind], Type)
dftbpBinders indexBnds exprBnds =
  do
    let expr = getExprOfBind (head exprBnds)
    defVecExpr     <- createDftArrayBind expr
    ((b, bnds), t) <- dftbpBinders' indexBnds exprBnds defVecExpr
    return ((b:bnds),t)
  where
    dftbpBinders' :: [CoreBind] 
		  -> [CoreBind] 
		  -> CoreBind 
		  -> Flatten ((CoreBind, [CoreBind]), Type)
    dftbpBinders' [] [] cBnd =
      return ((cBnd, []), panic "dftbpBinders: undefined type")
    dftbpBinders' (i:is) (e:es) cBind =
      do
	let iVar = getVarOfBind i
	let eVar = getVarOfBind e
	let cVar = getVarOfBind cBind
        let ty   = idType eVar
	newBnd  <- mkDftBackpermute ty iVar eVar cVar
	((fBnd, restBnds), _) <- dftbpBinders' is es newBnd
	return ((fBnd, (newBnd:restBnds)), liftTy ty)

    dftbpBinders'  _ _ _ = 
      panic "Flattening.dftbpBinders: index and expression binder lists have different length!"

getExprOfBind:: CoreBind -> CoreExpr
getExprOfBind (NonRec _ expr) = expr

getVarOfBind:: CoreBind -> Var
getVarOfBind (NonRec b _) = b



-- Optimised Transformation
-- =========================
--

-- liftSimpleFun
--   if variables x_1 to x_i occur in the context *and* free in expr
--   then 
--   (liftSimpleExpression expr) => mapP (\ (x1,..xn) -> expr) (x1,..xn)
--
liftSimpleFun:: CoreBndr -> CoreExpr -> Flatten (CoreExpr, Type)
liftSimpleFun b expr =
  do
    bndVars <- collectBoundVars expr
    let bndVars'     = b:bndVars
        bndVarsTuple = mkTuple (map idType bndVars') (map Var bndVars')
	lamExpr      = mkLams (b:bndVars) expr     -- FIXME: should be tuple
                                                   -- here 
    let (t1, t2)     = funTyArgs . exprType $ lamExpr
    mapExpr         <-  mk'mapP t1 t2 lamExpr bndVarsTuple
    let lexpr        = mkApps mapExpr [bndVarsTuple]
    return (lexpr, undefined)                      -- FIXME!!!!!


collectBoundVars:: CoreExpr -> Flatten [CoreBndr]
collectBoundVars  expr = 
  intersectWithContext (exprFreeVars expr)


-- auxilliary routines
-- -------------------

-- mkIndexOfExpr b lit ->
--   indexOf (mapP (\x -> x == lit) b) b
--
mkIndexOfExpr:: Type -> CoreBndr -> Literal -> Flatten CoreExpr
mkIndexOfExpr  idType b lit =
  do 
    eqExpr        <- mk'eq idType (Var b) (Lit lit)
    let lambdaExpr = (Lam b eqExpr)
    mk'indexOfP idType  lambdaExpr (Var b)

-- there is FlattenMonad.mk'indexOfP as well as
-- CoreSyn.mkApps and CoreSyn.mkLam, all of which should help here

-- for case-distinction over data constructors:
-- let b = expr in 
--   case b of
--      dcon args -> ....
-- dconId = dataConTag dcon 
-- the call "mkIndexOfExprDCon b dconId" computes the core expression for
-- indexOfP (\x -> x == dconId) b)
--
mkIndexOfExprDCon::Type -> CoreBndr -> Int -> Flatten CoreExpr
mkIndexOfExprDCon  idType b dId = 
  do 
    let intExpr    = mkIntLitInt dId
    eqExpr        <- mk'eq  idType (Var b) intExpr
    let lambdaExpr = (Lam b intExpr)
    mk'indexOfP idType lambdaExpr (Var b) 

  

-- there is FlattenMonad.mk'indexOfP as well as
-- CoreSyn.mkApps and CoreSyn.mkLam, all of which should help here

-- mk'IndexOfExprDConDft b dconIds : Generates the index expression for the
-- default case. "dconIds" is a list of all the data constructor idents which 
-- are covered by the other cases.
-- indexOfP (\x -> x != dconId_1 && ....) b)
--
mkIndexOfExprDConDft:: Type -> CoreBndr -> [Int] -> Flatten CoreExpr
mkIndexOfExprDConDft idType b dId  = 
  do 
    let intExprs   = map mkIntLitInt dId
    bExpr         <- foldM (mk'neq idType) (head intExprs) (tail intExprs)
    let lambdaExpr = (Lam b bExpr)
    mk'indexOfP idType (Var b) bExpr
  

-- mkIndexOfExprDef b [lit1, lit2,...] ->
--   indexOf (\x -> not (x == lit1 || x == lit2 ....) b
mkIndexOfExprDft:: Type -> CoreBndr -> [Literal] -> Flatten CoreExpr
mkIndexOfExprDft idType b lits = 
  do 
    let litExprs   = map (\l-> Lit l)  lits
    bExpr         <- foldM (mk'neq idType) (head litExprs) (tail litExprs)
    let lambdaExpr = (Lam b bExpr)
    mk'indexOfP idType bExpr (Var b) 


-- create a back-permute binder
--
-- * `mkDftBackpermute ty indexArrayVar srcArrayVar dftArrayVar' creates a
--   Core binding of the form
--
--     x = bpermuteDftP indexArrayVar srcArrayVar dftArrayVar
--
--   where `x' is a new local variable
--
mkDftBackpermute :: Type -> Var -> Var -> Var -> Flatten CoreBind
mkDftBackpermute ty idx src dft = 
  do
    rhs <- mk'bpermuteDftP ty (Var idx) (Var src) (Var dft)
    liftM snd $ mkBind FSLIT("dbp") rhs

-- create a dummy array with elements of the given type, which can be used as
-- default array for the combination of the subresults of the lifted case
-- expression
--
createDftArrayBind    :: CoreExpr -> Flatten CoreBind
createDftArrayBind e  =
  panic "Flattening.createDftArrayBind: not implemented yet"
{-
  do
    let ty = parrElemTy . exprType $ expr
    len <- mk'lengthP e
    rhs <- mk'replicateP ty len err??
    lift snd $ mkBind FSLIT("dft") rhs
FIXME: nicht so einfach; man kann kein "error"-Wert nehmen, denn der w"urde
  beim bpermuteDftP sofort evaluiert, aber es ist auch schwer m"oglich einen
  generischen Wert f"ur jeden beliebigen Typ zu erfinden.
-}




-- show functions (the pretty print functions sometimes don't 
-- show it the way I want....

-- shows just the structure
showCoreExpr (Var _ )    = "Var "
showCoreExpr (Lit _) = "Lit "
showCoreExpr (App e1 e2) = 
  "(App \n  " ++ (showCoreExpr e1) ++ "\n  " ++ (showCoreExpr e2) ++ ") "
showCoreExpr (Lam b e)   =
  "Lam b " ++ (showCoreExpr e)
showCoreExpr (Let bnds expr) =
  "Let \n" ++ (showBinds bnds) ++ "in " ++ (showCoreExpr expr)
  where showBinds (NonRec b e) = showBind (b,e)
        showBinds (Rec bnds)   = concat (map showBind bnds)
        showBind (b,e) = "  b = " ++ (showCoreExpr e)++ "\n"
-- gaw 2004 FIX?
showCoreExpr (Case ex b ty alts) =
  "Case b = " ++ (showCoreExpr ex) ++ " of \n" ++ (showAlts alts)
  where showAlts _ = ""  
showCoreExpr (Note _ ex) = "Note n " ++ (showCoreExpr ex)
showCoreExpr (Type t) = "Type"
