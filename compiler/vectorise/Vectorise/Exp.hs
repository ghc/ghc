{-# LANGUAGE TupleSections #-}

-- |Vectorisation of expressions.

module Vectorise.Exp
  (   -- * Vectorise right-hand sides of toplevel bindings
    vectTopExpr
  , vectTopExprs
  , vectScalarFun
  , vectScalarDFun
  ) 
where

#include "HsVersions.h"

import Vectorise.Type.Type
import Vectorise.Var
import Vectorise.Convert
import Vectorise.Vect
import Vectorise.Env
import Vectorise.Monad
import Vectorise.Builtins
import Vectorise.Utils

import CoreUtils
import MkCore
import CoreSyn
import CoreFVs
import Class
import DataCon
import TyCon
import TcType
import Type
import TypeRep
import Var
import VarEnv
import VarSet
import NameSet
import Id
import BasicTypes( isStrongLoopBreaker )
import Literal
import TysPrim
import Outputable
import FastString
import DynFlags
import Util
import MonadUtils

import Control.Monad
import Data.Maybe
import Data.List


-- Main entry point to vectorise expressions -----------------------------------

-- |Vectorise a polymorphic expression that forms a *non-recursive* binding.
--
-- Return 'Nothing' if the expression is scalar; otherwise, the first component of the result
-- (which is of type 'Bool') indicates whether the expression is parallel (i.e., whether it is
-- tagged as 'VIParr').
--
-- We have got the non-recursive case as a special case as it doesn't require to compute
-- vectorisation information twice.
--
vectTopExpr :: Var -> CoreExpr -> VM (Maybe (Bool, Inline, CoreExpr))
vectTopExpr var expr
  = do
    { exprVI <- encapsulateScalars <=< vectAvoidInfo emptyVarSet . freeVars $ expr
    ; if isVIEncaps exprVI
      then
        return Nothing
      else do
      { vExpr <- closedV $
                   inBind var $
                     vectAnnPolyExpr False exprVI
      ; inline <- computeInline exprVI
      ; return $ Just (isVIParr exprVI, inline, vectorised vExpr)
      }
    }

-- Compute the inlining hint for the right-hand side of a top-level binding.
--
computeInline :: CoreExprWithVectInfo -> VM Inline
computeInline ((_, VIDict), _)     = return $ DontInline
computeInline (_, AnnTick _ expr)  = computeInline expr
computeInline expr@(_, AnnLam _ _) = Inline <$> polyArity tvs
  where
    (tvs, _) = collectAnnTypeBinders expr
computeInline _expr                = return $ DontInline

-- |Vectorise a recursive group of top-level polymorphic expressions.
--
-- Return 'Nothing' if the expression group is scalar; otherwise, the first component of the result
-- (which is of type 'Bool') indicates whether the expressions are parallel (i.e., whether they are
-- tagged as 'VIParr').
--
vectTopExprs :: [(Var, CoreExpr)] -> VM (Maybe (Bool, [(Inline, CoreExpr)]))
vectTopExprs binds
  = do
    { exprVIs <- mapM (vectAvoidAndEncapsulate emptyVarSet) exprs
    ; if all isVIEncaps exprVIs
        -- if all bindings are scalar => don't vectorise this group of bindings
      then return Nothing
      else do
      {   -- non-scalar bindings need to be vectorised
      ; let areVIParr = any isVIParr exprVIs
      ; revised_exprVIs <- if not areVIParr
                             -- if no binding is parallel => 'exprVIs' is ready for vectorisation
                           then return exprVIs
                             -- if any binding is parallel => recompute the vectorisation info
                           else mapM (vectAvoidAndEncapsulate (mkVarSet vars)) exprs

      ; vExprs <- zipWithM vect vars revised_exprVIs
      ; return $ Just (areVIParr, vExprs)
      }
    }
  where
    (vars, exprs) = unzip binds
    
    vectAvoidAndEncapsulate pvs = encapsulateScalars <=< vectAvoidInfo pvs . freeVars
    
    vect var exprVI
      = do
        { vExpr  <- closedV $
                      inBind var $
                        vectAnnPolyExpr (isStrongLoopBreaker $ idOccInfo var) exprVI
        ; inline <- computeInline exprVI
        ; return (inline, vectorised vExpr)
        }

-- |Vectorise a polymorphic expression annotated with vectorisation information.
--
-- The special case of dictionary functions is currently handled separately. (Would be neater to
-- integrate them, though!)
--
vectAnnPolyExpr :: Bool -> CoreExprWithVectInfo -> VM VExpr
vectAnnPolyExpr loop_breaker (_, AnnTick tickish expr)
    -- traverse through ticks
  = vTick tickish <$> vectAnnPolyExpr loop_breaker expr
vectAnnPolyExpr loop_breaker expr
  | isVIDict expr
    -- special case the right-hand side of dictionary functions
  = (, undefined) <$> vectDictExpr (deAnnotate expr)
  | otherwise
    -- collect and vectorise type abstractions; then, descent into the body
  = polyAbstract tvs $ \args ->
      mapVect (mkLams $ tvs ++ args) <$> vectFnExpr False loop_breaker mono
  where
    (tvs, mono) = collectAnnTypeBinders expr

-- Encapsulate every purely sequential subexpression of a (potentially) parallel expression into a
-- lambda abstraction over all its free variables followed by the corresponding application to those
-- variables.  We can, then, avoid the vectorisation of the ensapsulated subexpressions.
--
-- Preconditions:
--
-- * All free variables and the result type must be /simple/ types.
-- * The expression is sufficiently complex (to warrant special treatment).  For now, that is
--   every expression that is not constant and contains at least one operation.
--  
encapsulateScalars :: CoreExprWithVectInfo -> VM CoreExprWithVectInfo
encapsulateScalars ce@(_, AnnType _ty)
  = return ce
encapsulateScalars ce@((_, VISimple), AnnVar _v)
      -- NB: diverts from the paper: encapsulate scalar variables (including functions)
  = liftSimpleAndCase ce
encapsulateScalars ce@(_, AnnVar _v)
  = return ce
encapsulateScalars ce@(_, AnnLit _)
  = return ce
encapsulateScalars ((fvs, vi), AnnTick tck expr)
  = do
    { encExpr <- encapsulateScalars expr
    ; return ((fvs, vi), AnnTick tck encExpr)
    }
encapsulateScalars ce@((fvs, vi), AnnLam bndr expr) 
  = do 
    { varsS  <- allScalarVarTypeSet fvs 
        -- NB: diverts from the paper: we need to check the scalarness of bound variables as well,
        --     as 'vectScalarFun' will handle them just the same as those introduced for the 'fvs'
        --     by encapsulation.
    ; bndrsS <- allScalarVarType bndrs
    ; case (vi, varsS && bndrsS) of
        (VISimple, True) -> liftSimpleAndCase ce
        _                -> do 
                            { encExpr <- encapsulateScalars expr
                            ; return ((fvs, vi), AnnLam bndr encExpr)
                            }
    }
  where
    (bndrs, _) = collectAnnBndrs ce
encapsulateScalars ce@((fvs, vi), AnnApp ce1 ce2)
  = do
    { varsS <- allScalarVarTypeSet fvs
    ; case (vi, varsS) of
        (VISimple, True) -> liftSimpleAndCase ce
        _                -> do 
                            { encCe1 <- encapsulateScalars ce1
                            ; encCe2 <- encapsulateScalars ce2
                            ; return ((fvs, vi), AnnApp encCe1 encCe2)
                            }
    }
encapsulateScalars ce@((fvs, vi), AnnCase scrut bndr ty alts) 
  = do 
    { varsS <- allScalarVarTypeSet fvs 
    ; case (vi, varsS) of
        (VISimple, True) -> liftSimpleAndCase ce
        _                -> do 
                            { encScrut <- encapsulateScalars scrut
                            ; encAlts  <- mapM encAlt alts
                            ; return ((fvs, vi), AnnCase encScrut bndr ty encAlts)
                            }
    }
  where
    encAlt (con, bndrs, expr) = (con, bndrs,) <$> encapsulateScalars expr
encapsulateScalars ce@((fvs, vi), AnnLet (AnnNonRec bndr expr1) expr2) 
  = do 
    { varsS <- allScalarVarTypeSet fvs 
    ; case (vi, varsS) of
        (VISimple, True) -> liftSimpleAndCase ce
        _                -> do 
                            { encExpr1 <- encapsulateScalars expr1
                            ; encExpr2 <- encapsulateScalars expr2
                            ; return ((fvs, vi), AnnLet (AnnNonRec bndr encExpr1) encExpr2)
                            }
    }
encapsulateScalars ce@((fvs, vi), AnnLet (AnnRec binds) expr) 
  = do 
    { varsS <- allScalarVarTypeSet fvs 
    ; case (vi, varsS) of 
        (VISimple, True) -> liftSimpleAndCase ce
        _                -> do 
                            { encBinds <- mapM encBind binds
                            ; encExpr  <- encapsulateScalars expr
                            ; return ((fvs, vi), AnnLet (AnnRec encBinds) encExpr)
                            }
    }                            
 where
   encBind (bndr, expr) = (bndr,) <$> encapsulateScalars expr
encapsulateScalars ((fvs, vi), AnnCast expr coercion)
  = do 
    { encExpr <- encapsulateScalars expr
    ; return ((fvs, vi), AnnCast encExpr coercion)
    }
encapsulateScalars _
  = panic "Vectorise.Exp.encapsulateScalars: unknown constructor"

-- Lambda-lift the given simple expression and apply it to the abstracted free variables.
--
-- If the expression is a case expression scrutinising anything, but a scalar type, then lift
-- each alternative individually.
--
liftSimpleAndCase :: CoreExprWithVectInfo -> VM CoreExprWithVectInfo
liftSimpleAndCase aexpr@((fvs, _vi), AnnCase expr bndr t alts)
  = do
    { vi <- vectAvoidInfoTypeOf expr
    ; if (vi == VISimple)
      then
        liftSimple aexpr  -- if the scrutinee is scalar, we need no special treatment
      else do
      { alts' <- mapM (\(ac, bndrs, aexpr) -> (ac, bndrs,) <$> liftSimpleAndCase aexpr) alts
      ; return ((fvs, vi), AnnCase expr bndr t alts')
      }
    }
liftSimpleAndCase aexpr = liftSimple aexpr

liftSimple :: CoreExprWithVectInfo -> VM CoreExprWithVectInfo
liftSimple ((fvs, vi), AnnVar v)
  | v `elemVarSet` fvs                -- special case to avoid producing: (\v -> v) v
  && not (isToplevel v)               --   NB: if 'v' not free or is toplevel, we must get the 'VIEncaps'
  = return $ ((fvs, vi), AnnVar v)
liftSimple aexpr@((fvs_orig, VISimple), expr) 
  = do 
    { let liftedExpr = mkAnnApps (mkAnnLams (reverse vars) fvs expr) vars

    ; traceVt "encapsulate:" $ ppr (deAnnotate aexpr) $$ text "==>" $$ ppr (deAnnotate liftedExpr)

    ; return $ liftedExpr
    }
  where
    vars = varSetElems fvs
    fvs  = filterVarSet (not . isToplevel) fvs_orig -- only include 'Id's that are not toplevel
    
    mkAnnLams :: [Var] -> VarSet -> AnnExpr' Var (VarSet, VectAvoidInfo) -> CoreExprWithVectInfo
    mkAnnLams []     fvs expr = ASSERT(isEmptyVarSet fvs)
                                ((emptyVarSet, VIEncaps), expr)
    mkAnnLams (v:vs) fvs expr = mkAnnLams vs (fvs `delVarSet` v) (AnnLam v ((fvs, VIEncaps), expr))
      
    mkAnnApps :: CoreExprWithVectInfo -> [Var] -> CoreExprWithVectInfo
    mkAnnApps aexpr []     = aexpr
    mkAnnApps aexpr (v:vs) = mkAnnApps (mkAnnApp aexpr v) vs

    mkAnnApp :: CoreExprWithVectInfo -> Var -> CoreExprWithVectInfo
    mkAnnApp aexpr@((fvs, _vi), _expr) v 
      = ((fvs `extendVarSet` v, VISimple), AnnApp aexpr ((unitVarSet v, VISimple), AnnVar v))
liftSimple aexpr
  = pprPanic "Vectorise.Exp.liftSimple: not simple" $ ppr (deAnnotate aexpr)

isToplevel :: Var -> Bool
isToplevel v | isId v    = case realIdUnfolding v of
                             NoUnfolding                     -> False
                             OtherCon      {}                -> True
                             DFunUnfolding {}                -> True 
                             CoreUnfolding {uf_is_top = top} -> top 
             | otherwise = False

-- |Vectorise an expression.
--
vectExpr :: CoreExprWithVectInfo -> VM VExpr

vectExpr aexpr
    -- encapsulated expression of functional type => try to vectorise as a scalar subcomputation
  | (isFunTy . annExprType $ aexpr) && isVIEncaps aexpr
  = vectFnExpr True False aexpr
    -- encapsulated constant => vectorise as a scalar constant
  | isVIEncaps aexpr
  = traceVt "vectExpr (encapsulated constant):" (ppr . deAnnotate $ aexpr) >> 
    vectConst (deAnnotate aexpr)

vectExpr (_, AnnVar v)
  = vectVar v

vectExpr (_, AnnLit lit)
  = vectConst $ Lit lit

vectExpr aexpr@(_, AnnLam _ _)
  = traceVt "vectExpr [AnnLam]:" (ppr . deAnnotate $ aexpr) >> 
    vectFnExpr True False aexpr

  -- SPECIAL CASE: Vectorise/lift 'patError @ ty err' by only vectorising/lifting the type 'ty';
  --   its only purpose is to abort the program, but we need to adjust the type to keep CoreLint
  --   happy.
-- FIXME: can't be do this with a VECTORISE pragma on 'pAT_ERROR_ID' now?
vectExpr (_, AnnApp (_, AnnApp (_, AnnVar v) (_, AnnType ty)) err)
  | v == pAT_ERROR_ID
  = do 
    { (vty, lty) <- vectAndLiftType ty
    ; return (mkCoreApps (Var v) [Type vty, err'], mkCoreApps (Var v) [Type lty, err'])
    }
  where
    err' = deAnnotate err

  -- type application (handle multiple consecutive type applications simultaneously to ensure the
  -- PA dictionaries are put at the right places)
vectExpr e@(_, AnnApp _ arg)
  | isAnnTypeArg arg
  = vectPolyApp e

  -- Lifted literal
vectExpr (_, AnnApp (_, AnnVar v) (_, AnnLit lit))
  | Just _con <- isDataConId_maybe v
  = do
    { let vexpr = App (Var v) (Lit lit)
    ; lexpr <- liftPD vexpr
    ; return (vexpr, lexpr)
    }

  -- value application (dictionary or user value)
vectExpr e@(_, AnnApp fn arg)
  | isPredTy arg_ty   -- dictionary application (whose result is not a dictionary)
  = vectPolyApp e
  | otherwise         -- user value
  = do 
    {   -- vectorise the types
    ; varg_ty <- vectType arg_ty 
    ; vres_ty <- vectType res_ty

        -- vectorise the function and argument expression
    ; vfn  <- vectExpr fn
    ; varg <- vectExpr arg

        -- the vectorised function is a closure; apply it to the vectorised argument
    ; mkClosureApp varg_ty vres_ty vfn varg
    }
  where
    (arg_ty, res_ty) = splitFunTy . exprType $ deAnnotate fn

vectExpr (_, AnnCase scrut bndr ty alts)
  | Just (tycon, ty_args) <- splitTyConApp_maybe scrut_ty
  , isAlgTyCon tycon
  = vectAlgCase tycon ty_args scrut bndr ty alts
  | otherwise 
  = do 
    { dflags <- getDynFlags
    ; cantVectorise dflags "Can't vectorise expression (no algebraic type constructor)" $ 
        ppr scrut_ty
    }
  where
    scrut_ty = exprType (deAnnotate scrut)

vectExpr (_, AnnLet (AnnNonRec bndr rhs) body)
  = do
    { traceVt "let binding (non-recursive)" empty
    ; vrhs <- localV $ 
                inBind bndr $ 
                  vectAnnPolyExpr False rhs
    ; traceVt "let body (non-recursive)" empty
    ; (vbndr, vbody) <- vectBndrIn bndr (vectExpr body)
    ; return $ vLet (vNonRec vbndr vrhs) vbody
    }

vectExpr (_, AnnLet (AnnRec bs) body)
  = do
    { (vbndrs, (vrhss, vbody)) <- vectBndrsIn bndrs $ do
                                  { traceVt "let bindings (recursive)" empty
                                  ; vrhss <- zipWithM vect_rhs bndrs rhss
                                  ; traceVt "let body (recursive)" empty
                                  ; vbody <- vectExpr body
                                  ; return (vrhss, vbody) 
                                  }
    ; return $ vLet (vRec vbndrs vrhss) vbody
    }
  where
    (bndrs, rhss) = unzip bs

    vect_rhs bndr rhs = localV $
                          inBind bndr $
                            vectAnnPolyExpr (isStrongLoopBreaker $ idOccInfo bndr) rhs

vectExpr (_, AnnTick tickish expr)
  = vTick tickish <$> vectExpr expr

vectExpr (_, AnnType ty)
  = vType <$> vectType ty

vectExpr e
  = do 
    { dflags <- getDynFlags
    ; cantVectorise dflags "Can't vectorise expression (vectExpr)" $ ppr (deAnnotate e)
    }

-- |Vectorise an expression that *may* have an outer lambda abstraction. If the expression is marked
-- as encapsulated ('VIEncaps'), vectorise it as a scalar computation (using a generalised scalar
-- zip).
--
-- We do not handle type variables at this point, as they will already have been stripped off by
-- 'vectPolyExpr'. We also only have to worry about one set of dictionary arguments as we (1) only
-- deal with Haskell 2011 and (2) class selectors are vectorised elsewhere.
--
vectFnExpr :: Bool                  -- ^If we process the RHS of a binding, whether that binding
                                    --  should be inlined
           -> Bool                  -- ^Whether the binding is a loop breaker
           -> CoreExprWithVectInfo  -- ^Expression to vectorise; must have an outer `AnnLam`
           -> VM VExpr
vectFnExpr inline loop_breaker aexpr@(_ann, AnnLam bndr body)
    -- predicate abstraction: leave as a normal abstraction, but vectorise the predicate type
  | isId bndr
    && isPredTy (idType bndr)
  = do 
    { vBndr <- vectBndr bndr
    ; vbody <- vectFnExpr inline loop_breaker body
    ; return $ mapVect (mkLams [vectorised vBndr]) vbody
    }
    -- encapsulated non-predicate abstraction: vectorise as a scalar computation
  | isId bndr && isVIEncaps aexpr
  = vectScalarFun . deAnnotate $ aexpr
    -- non-predicate abstraction: vectorise as a non-scalar computation
  | isId bndr
  = vectLam inline loop_breaker aexpr
  | otherwise 
  = do 
    { dflags <- getDynFlags
    ; cantVectorise dflags "Vectorise.Exp.vectFnExpr: Unexpected type lambda" $ 
        ppr (deAnnotate aexpr)
    }
vectFnExpr _ _ aexpr
    -- encapsulated function: vectorise as a scalar computation
  | (isFunTy . annExprType $ aexpr) && isVIEncaps aexpr
  = vectScalarFun . deAnnotate $ aexpr
  | otherwise
    -- not an abstraction: vectorise as a non-scalar vanilla expression
    -- NB: we can get here due to the recursion in the first case above and from 'vectAnnPolyExpr'
  = vectExpr aexpr

-- |Vectorise type and dictionary applications.
--
-- These are always headed by a variable (as we don't support higher-rank polymorphism), but may
-- involve two sets of type variables and dictionaries. Consider,
--
-- > class C a where
-- >   m :: D b => b -> a
--
-- The type of 'm' is 'm :: forall a. C a => forall b. D b => b -> a'.
--
vectPolyApp :: CoreExprWithVectInfo -> VM VExpr
vectPolyApp e0
  = case e4 of
      (_, AnnVar var)
        -> do {   -- get the vectorised form of the variable
              ; vVar <- lookupVar var
              ; traceVt "vectPolyApp of" (ppr var)

                  -- vectorise type and dictionary arguments
              ; vDictsOuter <- mapM vectDictExpr (map deAnnotate dictsOuter)
              ; vDictsInner <- mapM vectDictExpr (map deAnnotate dictsInner)
              ; vTysOuter   <- mapM vectType     tysOuter
              ; vTysInner   <- mapM vectType     tysInner
              
              ; let reconstructOuter v = (`mkApps` vDictsOuter) <$> polyApply v vTysOuter

              ; case vVar of
                  Local (vv, lv)
                    -> do { MASSERT( null dictsInner )    -- local vars cannot be class selectors
                          ; traceVt "  LOCAL" (text "")
                          ; (,) <$> reconstructOuter (Var vv) <*> reconstructOuter (Var lv)
                          }
                  Global vv
                    | isDictComp var                      -- dictionary computation
                    -> do {   -- in a dictionary computation, the innermost, non-empty set of
                              -- arguments are non-vectorised arguments, where no 'PA'dictionaries
                              -- are needed for the type variables
                          ; ve <- if null dictsInner
                                  then 
                                    return $ Var vv `mkTyApps` vTysOuter `mkApps` vDictsOuter
                                  else 
                                    reconstructOuter 
                                      (Var vv `mkTyApps` vTysInner `mkApps` vDictsInner)
                          ; traceVt "  GLOBAL (dict):" (ppr ve)
                          ; vectConst ve
                          }
                    | otherwise                           -- non-dictionary computation
                    -> do { MASSERT( null dictsInner )
                          ; ve <- reconstructOuter (Var vv)
                          ; traceVt "  GLOBAL (non-dict):" (ppr ve)
                          ; vectConst ve
                          }
              }
      _ -> pprSorry "Cannot vectorise programs with higher-rank types:" (ppr . deAnnotate $ e0)
  where
    -- if there is only one set of variables or dictionaries, it will be the outer set
    (e1, dictsOuter) = collectAnnDictArgs e0
    (e2, tysOuter)   = collectAnnTypeArgs e1
    (e3, dictsInner) = collectAnnDictArgs e2
    (e4, tysInner)   = collectAnnTypeArgs e3
    --
    isDictComp var = (isJust . isClassOpId_maybe $ var) || isDFunId var
    
-- |Vectorise the body of a dfun.  
--
-- Dictionary computations are special for the following reasons.  The application of dictionary
-- functions are always saturated, so there is no need to create closures.  Dictionary computations
-- don't depend on array values, so they are always scalar computations whose result we can
-- replicate (instead of executing them in parallel).
--
-- NB: To keep things simple, we are not rewriting any of the bindings introduced in a dictionary
--     computation.  Consequently, the variable case needs to deal with cases where binders are
--     in the vectoriser environments and where that is not the case.
--
vectDictExpr :: CoreExpr -> VM CoreExpr
vectDictExpr (Var var)
  = do { mb_scope <- lookupVar_maybe var
       ; case mb_scope of
           Nothing                -> return $ Var var   -- binder from within the dict. computation
           Just (Local (vVar, _)) -> return $ Var vVar  -- local vectorised variable
           Just (Global vVar)     -> return $ Var vVar  -- global vectorised variable
       }
vectDictExpr (Lit lit)
  = pprPanic "Vectorise.Exp.vectDictExpr: literal in dictionary computation" (ppr lit)
vectDictExpr (Lam bndr e)
  = Lam bndr <$> vectDictExpr e
vectDictExpr (App fn arg)
  = App <$> vectDictExpr fn <*> vectDictExpr arg
vectDictExpr (Case e bndr ty alts)
  = Case <$> vectDictExpr e <*> pure bndr <*> vectType ty <*> mapM vectDictAlt alts
  where
    vectDictAlt (con, bs, e) = (,,) <$> vectDictAltCon con <*> pure bs <*> vectDictExpr e
    --
    vectDictAltCon (DataAlt datacon) = DataAlt <$> maybeV dataConErr (lookupDataCon datacon)
      where
        dataConErr = ptext (sLit "Cannot vectorise data constructor:") <+> ppr datacon
    vectDictAltCon (LitAlt lit)      = return $ LitAlt lit
    vectDictAltCon DEFAULT           = return DEFAULT
vectDictExpr (Let bnd body)
  = Let <$> vectDictBind bnd <*> vectDictExpr body
  where
    vectDictBind (NonRec bndr e) = NonRec bndr <$> vectDictExpr e
    vectDictBind (Rec bnds)      = Rec <$> mapM (\(bndr, e) -> (bndr,) <$> vectDictExpr e) bnds
vectDictExpr e@(Cast _e _coe)
  = pprSorry "Vectorise.Exp.vectDictExpr: cast" (ppr e)
vectDictExpr (Tick tickish e)
  = Tick tickish <$> vectDictExpr e
vectDictExpr (Type ty)
  = Type <$> vectType ty
vectDictExpr (Coercion coe)
  = pprSorry "Vectorise.Exp.vectDictExpr: coercion" (ppr coe)

-- |Vectorise an expression of functional type, where all arguments and the result are of primitive
-- types (i.e., 'Int', 'Float', 'Double' etc., which have instances of the 'Scalar' type class) and
-- which does not contain any subcomputations that involve parallel arrays.  Such functionals do not
-- require the full blown vectorisation transformation; instead, they can be lifted by application
-- of a member of the zipWith family (i.e., 'map', 'zipWith', zipWith3', etc.)
--
-- Dictionary functions are also scalar functions (as dictionaries themselves are not vectorised,
-- instead they become dictionaries of vectorised methods).  We treat them differently, though see
-- "Note [Scalar dfuns]" in 'Vectorise'.
--
vectScalarFun :: CoreExpr -> VM VExpr
vectScalarFun expr 
  = do 
    { traceVt "vectScalarFun:" (ppr expr) 
    ; let (arg_tys, res_ty) = splitFunTys (exprType expr)
    ; mkScalarFun arg_tys res_ty expr
    }

-- Generate code for a scalar function by generating a scalar closure.  If the function is a
-- dictionary function, vectorise it as dictionary code.
-- 
mkScalarFun :: [Type] -> Type -> CoreExpr -> VM VExpr
mkScalarFun arg_tys res_ty expr
  | isPredTy res_ty
  = do { vExpr <- vectDictExpr expr
       ; return (vExpr, unused)
       }
  | otherwise
  = do { traceVt "mkScalarFun: " $ ppr expr $$ ptext (sLit "  ::") <+> ppr (mkFunTys arg_tys res_ty)

       ; fn_var  <- hoistExpr (fsLit "fn") expr DontInline
       ; zipf    <- zipScalars arg_tys res_ty
       ; clo     <- scalarClosure arg_tys res_ty (Var fn_var) (zipf `App` Var fn_var)
       ; clo_var <- hoistExpr (fsLit "clo") clo DontInline
       ; lclo    <- liftPD (Var clo_var)
       ; return (Var clo_var, lclo)
       }
  where
    unused = error "Vectorise.Exp.mkScalarFun: we don't lift dictionary expressions"

-- |Vectorise a dictionary function that has a 'VECTORISE SCALAR instance' pragma.
-- 
-- In other words, all methods in that dictionary are scalar functions — to be vectorised with
-- 'vectScalarFun'.  The dictionary "function" itself may be a constant, though.
--
-- NB: You may think that we could implement this function guided by the struture of the Core
--     expression of the right-hand side of the dictionary function.  We cannot proceed like this as
--     'vectScalarDFun' must also work for *imported* dfuns, where we don't necessarily have access
--     to the Core code of the unvectorised dfun.
--
-- Here an example — assume,
--
-- > class Eq a where { (==) :: a -> a -> Bool }
-- > instance (Eq a, Eq b) => Eq (a, b) where { (==) = ... }
-- > {-# VECTORISE SCALAR instance Eq (a, b) }
--
-- The unvectorised dfun for the above instance has the following signature:
--
-- > $dEqPair :: forall a b. Eq a -> Eq b -> Eq (a, b)
--
-- We generate the following (scalar) vectorised dfun (liberally using TH notation):
--
-- > $v$dEqPair :: forall a b. V:Eq a -> V:Eq b -> V:Eq (a, b)
-- > $v$dEqPair = /\a b -> \dEqa :: V:Eq a -> \dEqb :: V:Eq b ->
-- >                D:V:Eq $(vectScalarFun True recFns 
-- >                         [| (==) @(a, b) ($dEqPair @a @b $(unVect dEqa) $(unVect dEqb)) |])
--
-- NB:
-- * '(,)' vectorises to '(,)' — hence, the type constructor in the result type remains the same.
-- * We share the '$(unVect di)' sub-expressions between the different selectors, but duplicate
--   the application of the unvectorised dfun, to enable the dictionary selection rules to fire.
--
vectScalarDFun :: Var        -- ^ Original dfun
               -> VM CoreExpr
vectScalarDFun var
  = do {   -- bring the type variables into scope
       ; mapM_ defLocalTyVar tvs

           -- vectorise dictionary argument types and generate variables for them
       ; vTheta     <- mapM vectType theta
       ; vThetaBndr <- mapM (newLocalVar (fsLit "vd")) vTheta
       ; let vThetaVars = varsToCoreExprs vThetaBndr
       
           -- vectorise superclass dictionaries and methods as scalar expressions
       ; thetaVars  <- mapM (newLocalVar (fsLit "d")) theta
       ; thetaExprs <- zipWithM unVectDict theta vThetaVars
       ; let thetaDictBinds = zipWith NonRec thetaVars thetaExprs
             dict           = Var var `mkTyApps` (mkTyVarTys tvs) `mkVarApps` thetaVars
             scsOps         = map (\selId -> varToCoreExpr selId `mkTyApps` tys `mkApps` [dict])
                                  selIds
       ; vScsOps <- mapM (\e -> vectorised <$> vectScalarFun e) scsOps

           -- vectorised applications of the class-dictionary data constructor
       ; Just vDataCon <- lookupDataCon dataCon
       ; vTys          <- mapM vectType tys
       ; let vBody = thetaDictBinds `mkLets` mkCoreConApps vDataCon (map Type vTys ++ vScsOps)

       ; return $ mkLams (tvs ++ vThetaBndr) vBody
       }
  where
    ty                = varType var
    (tvs, theta, pty) = tcSplitSigmaTy  ty        -- 'theta' is the instance context
    (cls, tys)        = tcSplitDFunHead pty       -- 'pty' is the instance head
    selIds            = classAllSelIds cls
    dataCon           = classDataCon cls

-- Build a value of the dictionary before vectorisation from original, unvectorised type and an
-- expression computing the vectorised dictionary.
--
-- Given the vectorised version of a dictionary 'vd :: V:C vt1..vtn', generate code that computes
-- the unvectorised version, thus:
--
-- > D:C op1 .. opm
-- > where
-- >   opi = $(fromVect opTyi [| vSeli @vt1..vtk vd |])
--
-- where 'opTyi' is the type of the i-th superclass or op of the unvectorised dictionary.
--
unVectDict :: Type -> CoreExpr -> VM CoreExpr
unVectDict ty e 
  = do { vTys <- mapM vectType tys
       ; let meths = map (\sel -> Var sel `mkTyApps` vTys `mkApps` [e]) selIds
       ; scOps <- zipWithM fromVect methTys meths
       ; return $ mkCoreConApps dataCon (map Type tys ++ scOps)
       }
  where
    (tycon, tys, dataCon, methTys) = splitProductType "unVectDict: original type" ty
    cls                            = case tyConClass_maybe tycon of
                                       Just cls -> cls
                                       Nothing  -> panic "Vectorise.Exp.unVectDict: no class"
    selIds                         = classAllSelIds cls

-- Vectorise an 'n'-ary lambda abstraction by building a set of 'n' explicit closures.
--
-- All non-dictionary free variables go into the closure's environment, whereas the dictionary
-- variables are passed explicit (as conventional arguments) into the body during closure
-- construction.
--
vectLam :: Bool                 -- ^ Should the RHS of a binding be inlined?
        -> Bool                 -- ^ Whether the binding is a loop breaker.
        -> CoreExprWithVectInfo -- ^ Body of abstraction.
        -> VM VExpr
vectLam inline loop_breaker expr@((fvs, _vi), AnnLam _ _)
 = do { traceVt "fully vectorise a lambda expression" (ppr . deAnnotate $ expr)
 
      ; let (bndrs, body) = collectAnnValBinders expr

          -- grab the in-scope type variables
      ; tyvars <- localTyVars

          -- collect and vectorise all /local/ free variables
      ; vfvs <- readLEnv $ \env ->
                  [ (var, fromJust mb_vv) 
                  | var <- varSetElems fvs
                  , let mb_vv = lookupVarEnv (local_vars env) var
                  , isJust mb_vv         -- its local == is in local var env
                  ]
          -- separate dictionary from non-dictionary variables in the free variable set
      ; let (vvs_dict, vvs_nondict)     = partition (isPredTy . varType . fst) vfvs
            (_fvs_dict, vfvs_dict)      = unzip vvs_dict
            (fvs_nondict, vfvs_nondict) = unzip vvs_nondict

          -- compute the type of the vectorised closure
      ; arg_tys <- mapM (vectType . idType) bndrs
      ; res_ty  <- vectType (exprType $ deAnnotate body)

      ; let arity      = length fvs_nondict + length bndrs
            vfvs_dict' = map vectorised vfvs_dict
      ; buildClosures tyvars vfvs_dict' vfvs_nondict arg_tys res_ty
        . hoistPolyVExpr tyvars vfvs_dict' (maybe_inline arity)
        $ do {   -- generate the vectorised body of the lambda abstraction
             ; lc              <- builtin liftingContext
             ; (vbndrs, vbody) <- vectBndrsIn (fvs_nondict ++ bndrs) $ vectExpr body

             ; vbody' <- break_loop lc res_ty vbody
             ; return $ vLams lc vbndrs vbody'
             }
      }
  where
    maybe_inline n | inline    = Inline n
                   | otherwise = DontInline

    -- If this is the body of a binding marked as a loop breaker, add a recursion termination test
    -- to the /lifted/ version of the function body.  The termination tests checks if the lifting
    -- context is empty.  If so, it returns an empty array of the (lifted) result type instead of
    -- executing the function body.  This is the test from the last line (defining \mathcal{L}')
    -- in Figure 6 of HtM.
    break_loop lc ty (ve, le)
      | loop_breaker
      = do { empty <- emptyPD ty
           ; lty   <- mkPDataType ty
           ; return (ve, mkWildCase (Var lc) intPrimTy lty
                           [(DEFAULT, [], le),
                            (LitAlt (mkMachInt 0), [], empty)])
           }
      | otherwise = return (ve, le)
vectLam _ _ _ = panic "Vectorise.Exp.vectLam: not a lambda"

-- Vectorise an algebraic case expression.
--
-- We convert
--
--   case e :: t of v { ... }
--
-- to
--
--   V:    let v' = e in case v' of _ { ... }
--   L:    let v' = e in case v' `cast` ... of _ { ... }
--
--   When lifting, we have to do it this way because v must have the type
--   [:V(T):] but the scrutinee must be cast to the representation type. We also
--   have to handle the case where v is a wild var correctly.
--

-- FIXME: this is too lazy...is it?
vectAlgCase :: TyCon -> [Type] -> CoreExprWithVectInfo -> Var -> Type  
            -> [(AltCon, [Var], CoreExprWithVectInfo)]
            -> VM VExpr
vectAlgCase _tycon _ty_args scrut bndr ty [(DEFAULT, [], body)]
  = do
    { traceVt "scrutinee (DEFAULT only)" empty
    ; vscrut         <- vectExpr scrut
    ; (vty, lty)     <- vectAndLiftType ty
    ; traceVt "alternative body (DEFAULT only)" empty
    ; (vbndr, vbody) <- vectBndrIn bndr (vectExpr body)
    ; return $ vCaseDEFAULT vscrut vbndr vty lty vbody
    }
vectAlgCase _tycon _ty_args scrut bndr ty [(DataAlt _, [], body)]
  = do
    { traceVt "scrutinee (one shot w/o binders)" empty
    ; vscrut         <- vectExpr scrut
    ; (vty, lty)     <- vectAndLiftType ty
    ; traceVt "alternative body (one shot w/o binders)" empty
    ; (vbndr, vbody) <- vectBndrIn bndr (vectExpr body)
    ; return $ vCaseDEFAULT vscrut vbndr vty lty vbody
    }
vectAlgCase _tycon _ty_args scrut bndr ty [(DataAlt dc, bndrs, body)]
  = do
    { traceVt "scrutinee (one shot w/ binders)" empty
    ; vexpr      <- vectExpr scrut
    ; (vty, lty) <- vectAndLiftType ty
    ; traceVt "alternative body (one shot w/ binders)" empty
    ; (vbndr, (vbndrs, (vect_body, lift_body)))
        <- vect_scrut_bndr
         . vectBndrsIn bndrs
         $ vectExpr body
    ; let (vect_bndrs, lift_bndrs) = unzip vbndrs
    ; (vscrut, lscrut, pdata_dc) <- pdataUnwrapScrut (vVar vbndr)
    ; vect_dc <- maybeV dataConErr (lookupDataCon dc)

    ; let vcase = mk_wild_case vscrut vty vect_dc  vect_bndrs vect_body
          lcase = mk_wild_case lscrut lty pdata_dc lift_bndrs lift_body

    ; return $ vLet (vNonRec vbndr vexpr) (vcase, lcase)
    }
  where
    vect_scrut_bndr | isDeadBinder bndr = vectBndrNewIn bndr (fsLit "scrut")
                    | otherwise         = vectBndrIn bndr

    mk_wild_case expr ty dc bndrs body
      = mkWildCase expr (exprType expr) ty [(DataAlt dc, bndrs, body)]
      
    dataConErr = (text "vectAlgCase: data constructor not vectorised" <+> ppr dc)

vectAlgCase tycon _ty_args scrut bndr ty alts
  = do
    { traceVt "scrutinee (general case)" empty
    ; vexpr <- vectExpr scrut

    ; vect_tc     <- vectTyCon tycon
    ; (vty, lty)  <- vectAndLiftType ty

    ; let arity = length (tyConDataCons vect_tc)
    ; sel_ty <- builtin (selTy arity)
    ; sel_bndr <- newLocalVar (fsLit "sel") sel_ty
    ; let sel = Var sel_bndr

    ; traceVt "alternatives' body (general case)" empty
    ; (vbndr, valts) <- vect_scrut_bndr
                      $ mapM (proc_alt arity sel vty lty) alts'
    ; let (vect_dcs, vect_bndrss, lift_bndrss, vbodies) = unzip4 valts

    ; (vect_scrut, lift_scrut, pdata_dc) <- pdataUnwrapScrut (vVar vbndr)

    ; let (vect_bodies, lift_bodies) = unzip vbodies

    ; vdummy <- newDummyVar (exprType vect_scrut)
    ; ldummy <- newDummyVar (exprType lift_scrut)
    ; let vect_case = Case vect_scrut vdummy vty
                           (zipWith3 mk_vect_alt vect_dcs vect_bndrss vect_bodies)

    ; lc <- builtin liftingContext
    ; lbody <- combinePD vty (Var lc) sel lift_bodies
    ; let lift_case = Case lift_scrut ldummy lty
                           [(DataAlt pdata_dc, sel_bndr : concat lift_bndrss,
                             lbody)]

    ; return . vLet (vNonRec vbndr vexpr)
             $ (vect_case, lift_case)
    }
  where
    vect_scrut_bndr | isDeadBinder bndr = vectBndrNewIn bndr (fsLit "scrut")
                    | otherwise         = vectBndrIn bndr

    alts' = sortBy (\(alt1, _, _) (alt2, _, _) -> cmp alt1 alt2) alts

    cmp (DataAlt dc1) (DataAlt dc2) = dataConTag dc1 `compare` dataConTag dc2
    cmp DEFAULT       DEFAULT       = EQ
    cmp DEFAULT       _             = LT
    cmp _             DEFAULT       = GT
    cmp _             _             = panic "vectAlgCase/cmp"

    proc_alt arity sel _ lty (DataAlt dc, bndrs, body@((fvs_body, _), _))
      = do
          vect_dc <- maybeV dataConErr (lookupDataCon dc)
          let ntag = dataConTagZ vect_dc
              tag  = mkDataConTag vect_dc
              fvs  = fvs_body `delVarSetList` bndrs

          sel_tags  <- liftM (`App` sel) (builtin (selTags arity))
          lc        <- builtin liftingContext
          elems     <- builtin (selElements arity ntag)

          (vbndrs, vbody)
            <- vectBndrsIn bndrs
             . localV
             $ do
               { binds    <- mapM (pack_var (Var lc) sel_tags tag)
                           . filter isLocalId
                           $ varSetElems fvs
               ; traceVt "case alternative:" (ppr . deAnnotate $ body)
               ; (ve, le) <- vectExpr body
               ; return (ve, Case (elems `App` sel) lc lty
                             [(DEFAULT, [], (mkLets (concat binds) le))])
               }
                 -- empty    <- emptyPD vty
                 -- return (ve, Case (elems `App` sel) lc lty
                 --             [(DEFAULT, [], Let (NonRec flags_var flags_expr)
                 --                             $ mkLets (concat binds) le),
                 --               (LitAlt (mkMachInt 0), [], empty)])
          let (vect_bndrs, lift_bndrs) = unzip vbndrs
          return (vect_dc, vect_bndrs, lift_bndrs, vbody)
      where
        dataConErr = (text "vectAlgCase: data constructor not vectorised" <+> ppr dc)

    proc_alt _ _ _ _ _ = panic "vectAlgCase/proc_alt"

    mk_vect_alt vect_dc bndrs body = (DataAlt vect_dc, bndrs, body)

      -- Pack a variable for a case alternative context *if* the variable is vectorised. If it
      -- isn't, ignore it as scalar variables don't need to be packed.
    pack_var len tags t v
      = do
        { r <- lookupVar_maybe v
        ; case r of
            Just (Local (vv, lv)) ->
              do
              { lv'  <- cloneVar lv
              ; expr <- packByTagPD (idType vv) (Var lv) len tags t
              ; updLEnv (\env -> env { local_vars = extendVarEnv (local_vars env) v (vv, lv') })
              ; return [(NonRec lv' expr)]
              }
            _ -> return []
        }
   

-- Support to compute information for vectorisation avoidance ------------------

-- Annotation for Core AST nodes that describes how they should be handled during vectorisation
-- and especially if vectorisation of the corresponding computation can be avoided.
--
data VectAvoidInfo = VIParr       -- tree contains parallel computations
                   | VISimple     -- result type is scalar & no parallel subcomputation
                   | VIComplex    -- any result type, no parallel subcomputation
                   | VIEncaps     -- tree encapsulated by 'liftSimple'
                   | VIDict       -- dictionary computation (never parallel)
                   deriving (Eq, Show)

-- Core expression annotated with free variables and vectorisation-specific information.
--
type CoreExprWithVectInfo = AnnExpr Id (VarSet, VectAvoidInfo)

-- Yield the type of an annotated core expression.
--
annExprType :: AnnExpr Var ann -> Type
annExprType = exprType . deAnnotate

-- Project the vectorisation information from an annotated Core expression.
--
vectAvoidInfoOf :: CoreExprWithVectInfo -> VectAvoidInfo
vectAvoidInfoOf ((_, vi), _) = vi

-- Is this a 'VIParr' node?
--
isVIParr :: CoreExprWithVectInfo -> Bool
isVIParr = (== VIParr) . vectAvoidInfoOf

-- Is this a 'VIEncaps' node?
--
isVIEncaps :: CoreExprWithVectInfo -> Bool
isVIEncaps = (== VIEncaps) . vectAvoidInfoOf

-- Is this a 'VIDict' node?
--
isVIDict :: CoreExprWithVectInfo -> Bool
isVIDict = (== VIDict) . vectAvoidInfoOf

-- 'VIParr' if either argument is 'VIParr'; otherwise, the first argument.
--
unlessVIParr :: VectAvoidInfo -> VectAvoidInfo -> VectAvoidInfo
unlessVIParr _  VIParr = VIParr
unlessVIParr vi _      = vi

-- 'VIParr' if either arguments vectorisation information is 'VIParr'; otherwise, the vectorisation
-- information of the first argument is produced.
--
unlessVIParrExpr :: VectAvoidInfo -> CoreExprWithVectInfo -> VectAvoidInfo
infixl `unlessVIParrExpr`
unlessVIParrExpr e1 e2 = e1 `unlessVIParr` vectAvoidInfoOf e2

-- Compute Core annotations to determine for which subexpressions we can avoid vectorisation.
--
-- * The first argument is the set of free, local variables whose evaluation may entail parallelism.
--
vectAvoidInfo :: VarSet -> CoreExprWithFVs -> VM CoreExprWithVectInfo
vectAvoidInfo pvs ce@(fvs, AnnVar v)
  = do 
    { gpvs <- globalParallelVars
    ; vi <- if v `elemVarSet` pvs || v `elemVarSet` gpvs
            then return VIParr
            else vectAvoidInfoTypeOf ce
    ; viTrace ce vi []
    ; when (vi == VIParr) $
        traceVt "  reason:" $ if v `elemVarSet` pvs  then text "local"  else
                              if v `elemVarSet` gpvs then text "global" else text "parallel type"

    ; return ((fvs, vi), AnnVar v)
    }

vectAvoidInfo _pvs ce@(fvs, AnnLit lit)
  = do 
    { vi <- vectAvoidInfoTypeOf ce  
    ; viTrace ce vi [] 
    ; return ((fvs, vi), AnnLit lit)
    }

vectAvoidInfo pvs ce@(fvs, AnnApp e1 e2)
  = do 
    { ceVI <- vectAvoidInfoTypeOf ce
    ; eVI1 <- vectAvoidInfo pvs e1  
    ; eVI2 <- vectAvoidInfo pvs e2
    ; let vi = ceVI `unlessVIParrExpr` eVI1 `unlessVIParrExpr` eVI2
    -- ; viTrace ce vi [eVI1, eVI2]                     
    ; return ((fvs, vi), AnnApp eVI1 eVI2)
    }

vectAvoidInfo pvs (fvs, AnnLam var body)
  = do 
    { bodyVI <- vectAvoidInfo pvs body 
    ; varVI  <- vectAvoidInfoType $ varType var
    ; let vi = vectAvoidInfoOf bodyVI `unlessVIParr` varVI
    -- ; viTrace ce vi [bodyVI]
    ; return ((fvs, vi), AnnLam var bodyVI)
    }

vectAvoidInfo pvs ce@(fvs, AnnLet (AnnNonRec var e) body)  
  = do 
    { ceVI       <- vectAvoidInfoTypeOf ce
    ; eVI        <- vectAvoidInfo pvs e
    ; isScalarTy <- isScalar $ varType var
    ; (bodyVI, vi) <- if isVIParr eVI && not isScalarTy
        then do -- binding is parallel
        { bodyVI <- vectAvoidInfo (pvs `extendVarSet` var) body
        ; return (bodyVI, VIParr)
        }
        else do -- binding doesn't affect parallelism
        { bodyVI <- vectAvoidInfo pvs body
        ; return (bodyVI, ceVI `unlessVIParrExpr` bodyVI)
        }
    -- ; viTrace ce vi [eVI, bodyVI]
    ; return ((fvs, vi), AnnLet (AnnNonRec var eVI) bodyVI)
    }

vectAvoidInfo pvs ce@(fvs, AnnLet (AnnRec bnds) body)  
  = do 
    { ceVI         <- vectAvoidInfoTypeOf ce
    ; bndsVI       <- mapM (vectAvoidInfoBnd pvs) bnds
    ; parrBndrs    <- map fst <$> filterM isVIParrBnd bndsVI
    ; if not . null $ parrBndrs
      then do         -- body may trigger parallelism via at least one binding
        { new_pvs <- filterM ((not <$>) . isScalar . varType) parrBndrs
        ; let extendedPvs = pvs `extendVarSetList` new_pvs
        ; bndsVI <- mapM (vectAvoidInfoBnd extendedPvs) bnds
        ; bodyVI <- vectAvoidInfo extendedPvs body
        -- ; viTrace ce VIParr (map snd bndsVI ++ [bodyVI])
        ; return ((fvs, VIParr), AnnLet (AnnRec bndsVI) bodyVI)
        }
      else do         -- demanded bindings cannot trigger parallelism
        { bodyVI <- vectAvoidInfo pvs body
        ; let vi = ceVI `unlessVIParrExpr` bodyVI
        -- ; viTrace ce vi (map snd bndsVI ++ [bodyVI])
        ; return ((fvs, vi), AnnLet (AnnRec bndsVI) bodyVI)          
        }
    }
  where
    vectAvoidInfoBnd pvs (var, e) = (var,) <$> vectAvoidInfo pvs e

    isVIParrBnd (var, eVI) 
      = do 
        { isScalarTy <- isScalar (varType var)
        ; return $ isVIParr eVI && not isScalarTy
        }

vectAvoidInfo pvs ce@(fvs, AnnCase e var ty alts) 
  = do 
    { ceVI           <- vectAvoidInfoTypeOf ce
    ; eVI            <- vectAvoidInfo pvs e
    ; altsVI         <- mapM (vectAvoidInfoAlt (isVIParr eVI)) alts
    ; let alteVIs = [eVI | (_, _, eVI) <- altsVI]
          vi      =  foldl unlessVIParrExpr ceVI (eVI:alteVIs)  -- NB: same effect as in the paper
    -- ; viTrace ce vi (eVI : alteVIs)
    ; return ((fvs, vi), AnnCase eVI var ty altsVI)
    }
  where
    vectAvoidInfoAlt scrutIsPar (con, bndrs, e) 
      = do
        { allScalar <- allScalarVarType bndrs
        ; let altPvs | scrutIsPar && not allScalar = pvs `extendVarSetList` bndrs
                     | otherwise                   = pvs
        ; (con, bndrs,) <$> vectAvoidInfo altPvs e
        }

vectAvoidInfo pvs (fvs, AnnCast e (fvs_ann, ann))
  = do 
    { eVI <- vectAvoidInfo pvs e
    ; return ((fvs, vectAvoidInfoOf eVI), AnnCast eVI ((fvs_ann, VISimple), ann))
    }

vectAvoidInfo pvs (fvs, AnnTick tick e)
  = do 
    { eVI <- vectAvoidInfo pvs e
    ; return ((fvs, vectAvoidInfoOf eVI), AnnTick tick eVI)
    }

vectAvoidInfo _pvs (fvs, AnnType ty)
  = return ((fvs, VISimple), AnnType ty)

vectAvoidInfo _pvs (fvs, AnnCoercion coe) 
  = return ((fvs, VISimple), AnnCoercion coe)

-- Compute vectorisation avoidance information for a type.
--
vectAvoidInfoType :: Type -> VM VectAvoidInfo   
vectAvoidInfoType ty
  | isPredTy ty
  = return VIDict
  | Just (arg, res) <- splitFunTy_maybe ty
  = do
    { argVI <- vectAvoidInfoType arg
    ; resVI <- vectAvoidInfoType res
    ; case (argVI, resVI) of
        (VISimple, VISimple) -> return VISimple   -- NB: diverts from the paper: scalar functions
        (_       , VIDict)   -> return VIDict
        _                    -> return $ VIComplex `unlessVIParr` argVI `unlessVIParr` resVI
    }
  | otherwise
  = do
    { parr <- maybeParrTy ty
    ; if parr
      then return VIParr
      else do 
    { scalar <- isScalar ty
    ; if scalar 
      then return VISimple
      else return VIComplex
    } }

-- Compute vectorisation avoidance information for the type of a Core expression (with FVs).
--
vectAvoidInfoTypeOf :: AnnExpr Var ann -> VM VectAvoidInfo
vectAvoidInfoTypeOf = vectAvoidInfoType . annExprType

-- Checks whether the type might be a parallel array type.
--
maybeParrTy :: Type -> VM Bool
maybeParrTy ty 
    -- looking through newtypes
  | Just ty'      <- coreView ty
  = (== VIParr) <$> vectAvoidInfoType ty'
    -- decompose constructor applications
  | Just (tc, ts) <- splitTyConApp_maybe ty 
  = do
    { isParallel <- (tyConName tc `elemNameSet`) <$> globalParallelTyCons
    ; if isParallel
      then return True 
      else or <$> mapM maybeParrTy ts
    }
maybeParrTy (ForAllTy _ ty) = maybeParrTy ty
maybeParrTy _               = return False

-- Are the types of all variables in the 'Scalar' class?
--
allScalarVarType :: [Var] -> VM Bool
allScalarVarType vs = and <$> mapM (isScalar . varType) vs

-- Are the types of all variables in the set in the 'Scalar' class?
--
allScalarVarTypeSet :: VarSet -> VM Bool
allScalarVarTypeSet = allScalarVarType . varSetElems

-- Debugging support
--
viTrace :: CoreExprWithFVs -> VectAvoidInfo -> [CoreExprWithVectInfo] -> VM ()
viTrace ce vi vTs
  = traceVt ("vect info: " ++ show vi ++ "[" ++ 
             (concat $ map ((++ " ") . show . vectAvoidInfoOf) vTs) ++ "]")
            (ppr $ deAnnotate ce)
