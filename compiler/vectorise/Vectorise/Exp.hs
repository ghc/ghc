{-# LANGUAGE TupleSections #-}

-- |Vectorisation of expressions.

module Vectorise.Exp
  (   -- * Vectorise polymorphic expressions with special cases for right-hand sides of particular 
      --   variable bindings
    vectPolyExpr
  , vectDictExpr
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
import PrelNames
import Var
import VarEnv
import VarSet
import Id
import BasicTypes( isStrongLoopBreaker )
import Literal
import TysWiredIn
import TysPrim
import Outputable
import FastString
import Control.Monad
import Control.Applicative
import Data.Maybe
import Data.List
import TcRnMonad (goptM)
import DynFlags
import Util


-- Main entry point to vectorise expressions -----------------------------------

-- |Vectorise a polymorphic expression.
--
-- If not yet available, precompute vectorisation avoidance information before vectorising.  If
-- the vectorisation avoidance optimisation is enabled, also use the vectorisation avoidance
-- information to encapsulated subexpression that do not need to be vectorised.
--
vectPolyExpr :: Bool -> [Var] -> CoreExprWithFVs -> Maybe VITree
             -> VM (Inline, Bool, VExpr)
  -- precompute vectorisation avoidance information (and possibly encapsulated subexpressions)
vectPolyExpr loop_breaker recFns expr Nothing
  = do
    { vectAvoidance <- liftDs $ goptM Opt_AvoidVect
    ; vi <- vectAvoidInfo expr  
    ; (expr', vi') <- 
        if vectAvoidance
        then do 
             { (expr', vi') <- encapsulateScalars vi expr
             ; traceVt "vectPolyExpr encapsulated:" (ppr $ deAnnotate expr')
             ; return (expr', vi')
             }
        else return (expr, vi)
    ; vectPolyExpr loop_breaker recFns expr' (Just vi')
    }

  -- traverse through ticks
vectPolyExpr loop_breaker recFns (_, AnnTick tickish expr) (Just (VITNode _ [vit])) 
  = do 
    { (inline, isScalarFn, expr') <- vectPolyExpr loop_breaker recFns expr (Just vit)
    ; return (inline, isScalarFn, vTick tickish expr')
    }

  -- collect and vectorise type abstractions; then, descent into the body
vectPolyExpr loop_breaker recFns expr (Just vit)
  = do 
    { let (tvs, mono) = collectAnnTypeBinders expr
          vit'        = stripLevels (length tvs) vit
    ; arity <- polyArity tvs
    ; polyAbstract tvs $ \args ->
        do 
        { (inline, isScalarFn, mono') <- vectFnExpr False loop_breaker recFns mono vit'
        ; return (addInlineArity inline arity, isScalarFn, mapVect (mkLams $ tvs ++ args) mono')
        }
    }
  where
    stripLevels 0 vit               = vit
    stripLevels n (VITNode _ [vit]) = stripLevels (n - 1) vit
    stripLevels _ vit               = pprPanic "vectPolyExpr: stripLevels:" (text (show vit))

-- Encapsulate every purely sequential subexpression of a (potentially) parallel expression into a
-- into a lambda abstraction over all its free variables followed by the corresponding application
-- to those variables.  We can, then, avoid the vectorisation of the ensapsulated subexpressions.
--
-- Preconditions:
--
-- * All free variables and the result type must be /simple/ types.
-- * The expression is sufficientlt complex (top warrant special treatment).  For now, that is
--   every expression that is not constant and contains at least one operation.
--  
encapsulateScalars :: VITree -> CoreExprWithFVs -> VM (CoreExprWithFVs, VITree)
encapsulateScalars  vit ce@(_, AnnType _ty) 
  = return (ce, vit)
      
encapsulateScalars  vit ce@(_, AnnVar _v)  
  = return (ce, vit)
  
encapsulateScalars vit ce@(_, AnnLit _)
  = return (ce, vit) 

encapsulateScalars (VITNode vi [vit]) (fvs, AnnTick tck expr)
  = do { (extExpr, vit') <- encapsulateScalars vit expr
       ; return ((fvs, AnnTick tck extExpr), VITNode vi [vit'])
       }

encapsulateScalars _ (_fvs, AnnTick _tck _expr)
  = panic "encapsulateScalar AnnTick doesn't match up"
  
encapsulateScalars (VITNode vi [vit]) ce@(fvs, AnnLam bndr expr) 
  = do { varsS <- varsSimple fvs 
       ; case (vi, varsS) of
           (VISimple, True) -> do { let (e', vit') = liftSimple vit ce
                                  ; return (e', vit') 
                                  }
           _                -> do { (extExpr, vit') <- encapsulateScalars vit expr
                                  ; return ((fvs, AnnLam bndr extExpr), VITNode vi [vit'])
                                  }
       }

encapsulateScalars _ (_fvs, AnnLam _bndr _expr) 
  = panic "encapsulateScalars AnnLam doesn't match up"

encapsulateScalars vt@(VITNode vi [vit1, vit2]) ce@(fvs, AnnApp ce1 ce2) 
  = do { varsS <- varsSimple fvs 
       ; case (vi, varsS) of
           (VISimple, True) -> do { let (e', vt') = liftSimple vt ce
                                  -- ; checkTreeAnnM vt' e'
                                  -- ; traceVt "Passed checkTree test!!" (ppr $ deAnnotate e')
                                  ; return (e', vt')
                                  }
           _                -> do { (etaCe1, vit1') <- encapsulateScalars vit1 ce1
                                  ; (etaCe2, vit2') <- encapsulateScalars vit2 ce2
                                  ; return ((fvs, AnnApp etaCe1 etaCe2), VITNode vi [vit1', vit2'])
                                  }
       }

encapsulateScalars _  (_fvs, AnnApp _ce1 _ce2)                           
  = panic "encapsulateScalars AnnApp doesn't match up"
  
encapsulateScalars vt@(VITNode vi (scrutVit : altVits)) ce@(fvs, AnnCase scrut bndr ty alts) 
  = do { varsS <- varsSimple fvs 
       ; case (vi, varsS) of
           (VISimple, True) -> return $ liftSimple vt ce
           _                -> do { (extScrut, scrutVit') <- encapsulateScalars scrutVit scrut
                                  ; extAltsVits  <- zipWithM expAlt altVits alts
                                  ; let (extAlts, altVits') = unzip extAltsVits
                                  ; return ((fvs, AnnCase extScrut bndr ty extAlts), VITNode vi (scrutVit': altVits'))
                                  }
       }
  where
    expAlt vt (con, bndrs, expr) 
      = do { (extExpr, vt') <- encapsulateScalars vt expr
           ; return ((con, bndrs, extExpr), vt')
           }
           
encapsulateScalars _ (_fvs, AnnCase _scrut _bndr _ty _alts) 
  = panic "encapsulateScalars AnnCase doesn't match up"
  
encapsulateScalars vt@(VITNode vi [vt1, vt2]) ce@(fvs, AnnLet (AnnNonRec bndr expr1) expr2) 
  = do { varsS <- varsSimple fvs 
       ; case (vi, varsS) of
           (VISimple, True) -> return $ liftSimple vt ce
           _                -> do { (extExpr1, vt1') <- encapsulateScalars vt1 expr1
                                  ; (extExpr2, vt2') <- encapsulateScalars vt2 expr2
                                  ; return ((fvs, AnnLet (AnnNonRec bndr extExpr1) extExpr2), VITNode vi [vt1', vt2'])
                                  }
       }

encapsulateScalars _ (_fvs, AnnLet (AnnNonRec _bndr _expr1) _expr2)       
  = panic "encapsulateScalars AnnLet nonrec doesn't match up"
         
encapsulateScalars vt@(VITNode vi (vtB : vtBnds)) ce@(fvs, AnnLet (AnnRec bndngs) expr) 
  = do { varsS <- varsSimple fvs 
       ; case (vi, varsS) of 
           (VISimple, True) -> return $ liftSimple vt ce
           _                -> do { extBndsVts <- zipWithM expBndg vtBnds bndngs
                                  ; let (extBnds, vtBnds') = unzip extBndsVts
                                  ; (extExpr, vtB') <- encapsulateScalars vtB expr
                                  ; let vt' = VITNode vi (vtB':vtBnds')
                                  ; return ((fvs, AnnLet (AnnRec extBnds) extExpr), vt')
                                  }
       }                            
    where
      expBndg vit (bndr, expr) 
        = do { (extExpr, vit') <- encapsulateScalars vit expr
             ; return  ((bndr, extExpr), vit')
             }
       
encapsulateScalars _ (_fvs, AnnLet (AnnRec _) _expr2)       
  = panic "encapsulateScalars AnnLet rec doesn't match up"

encapsulateScalars (VITNode vi [vit]) (fvs, AnnCast expr coercion)
  = do { (extExpr, vit') <- encapsulateScalars  vit expr
       ; return ((fvs, AnnCast extExpr coercion), VITNode vi [vit'])
       }
       
encapsulateScalars  _ (_fvs, AnnCast _expr _coercion) 
  = panic "encapsulateScalars AnnCast rec doesn't match up"
    
encapsulateScalars _ _  
  = panic "encapsulateScalars case not handled"

-- Lambda-lift the given expression and apply it to the abstracted free variables.
--
-- If the expression is a case expression scrutinising anything but a primitive type, then lift
-- each alternative individually.
--
liftSimple :: VITree -> CoreExprWithFVs -> (CoreExprWithFVs, VITree)
liftSimple (VITNode vi (scrutVit : altVits)) (fvs, AnnCase expr bndr t alts) 
  | Just (c,_) <- splitTyConApp_maybe (exprType $ deAnnotate $ expr),  
    (not $ elem c [boolTyCon, intTyCon, doubleTyCon, floatTyCon])   -- FIXME: shouldn't be hardcoded
     = ((fvs, AnnCase expr bndr t alts'), VITNode vi (scrutVit : altVits'))      
  where 
    (alts', altVits') = unzip $ map (\(ac,bndrs, (alt, avi)) -> ((ac,bndrs,alt), avi)) $ 
                        zipWith  (\(ac, bndrs, aex) -> \altVi -> (ac, bndrs, liftSimple altVi aex)) alts altVits
          
liftSimple viTree ae@(fvs, _annEx) 
  = (mkAnnApps (mkAnnLams ae vars) vars, viTree')
  where
    mkViTreeLams (VITNode _ vits) [] = VITNode VIEncaps vits
    mkViTreeLams vi (_:vs) = VITNode VIEncaps [mkViTreeLams vi vs]

    mkViTreeApps vi []      = vi
    mkViTreeApps vi (_:vs)  = VITNode VISimple [mkViTreeApps vi vs, VITNode VISimple []]
    
    vars    = varSetElems fvs
    viTree' = mkViTreeApps (mkViTreeLams viTree vars) vars
    
    mkAnnLam :: bndr -> AnnExpr bndr VarSet -> AnnExpr' bndr VarSet
    mkAnnLam bndr ce = AnnLam bndr ce         
      
    mkAnnLams:: CoreExprWithFVs -> [Var] -> CoreExprWithFVs
    mkAnnLams (fv, aex') []     = (fv, aex')  -- fv should be empty. check!
    mkAnnLams (fv, aex') (v:vs) = mkAnnLams (delVarSet fv v, (mkAnnLam v ((delVarSet fv v), aex'))) vs
      
    mkAnnApp :: (AnnExpr bndr VarSet) -> Var -> (AnnExpr' bndr VarSet)
    mkAnnApp aex v = AnnApp aex (unitVarSet v, (AnnVar v))
      
    mkAnnApps:: CoreExprWithFVs -> [Var] -> CoreExprWithFVs
    mkAnnApps (fv, aex') [] = (fv, aex')
    mkAnnApps ae (v:vs) = 
      let
        (fv, aex') = mkAnnApps ae vs
      in (extendVarSet fv v, mkAnnApp (fv, aex') v)

-- |Vectorise an expression.
--
vectExpr :: CoreExprWithFVs -> VITree -> VM VExpr
-- vectExpr e vi | not (checkTree vi (deAnnotate e))
--   = pprPanic "vectExpr" (ppr $ deAnnotate e)
 
vectExpr (_, AnnVar v)  _ 
  = vectVar v

vectExpr (_, AnnLit lit) _
  = vectConst $ Lit lit

vectExpr e@(_, AnnLam bndr _) vt
  | isId bndr = (\(_, _, ve) -> ve) <$> vectFnExpr True False [] e vt
  | otherwise = do dflags <- getDynFlags
                   cantVectorise dflags "Unexpected type lambda (vectExpr)" (ppr (deAnnotate e))

  -- SPECIAL CASE: Vectorise/lift 'patError @ ty err' by only vectorising/lifting the type 'ty';
  --   its only purpose is to abort the program, but we need to adjust the type to keep CoreLint
  --   happy.
-- FIXME: can't be do this with a VECTORISE pragma on 'pAT_ERROR_ID' now?
vectExpr (_, AnnApp (_, AnnApp (_, AnnVar v) (_, AnnType ty)) err)  _
  | v == pAT_ERROR_ID
  = do { (vty, lty) <- vectAndLiftType ty
       ; return (mkCoreApps (Var v) [Type vty, err'], mkCoreApps (Var v) [Type lty, err'])
       }
  where
    err' = deAnnotate err

  -- type application (handle multiple consecutive type applications simultaneously to ensure the
  -- PA dictionaries are put at the right places)
vectExpr e@(_, AnnApp _ arg) (VITNode _ [_, _])
  | isAnnTypeArg arg
  = vectPolyApp e
    
  -- 'Int', 'Float', or 'Double' literal
  -- FIXME: this needs to be generalised
vectExpr (_, AnnApp (_, AnnVar v) (_, AnnLit lit)) _
  | Just con <- isDataConId_maybe v
  , is_special_con con
  = do
      let vexpr = App (Var v) (Lit lit)
      lexpr <- liftPD vexpr
      return (vexpr, lexpr)
  where
    is_special_con con = con `elem` [intDataCon, floatDataCon, doubleDataCon]

  -- value application (dictionary or user value)
vectExpr e@(_, AnnApp fn arg) (VITNode _ [vit1, vit2]) 
  | isPredTy arg_ty   -- dictionary application (whose result is not a dictionary)
  = vectPolyApp e
  | otherwise         -- user value
  = do {   -- vectorise the types
       ; varg_ty <- vectType arg_ty 
       ; vres_ty <- vectType res_ty

           -- vectorise the function and argument expression
       ; vfn  <- vectExpr fn  vit1
       ; varg <- vectExpr arg vit2

           -- the vectorised function is a closure; apply it to the vectorised argument
       ; mkClosureApp varg_ty vres_ty vfn varg
       }
  where
    (arg_ty, res_ty) = splitFunTy . exprType $ deAnnotate fn

vectExpr (_, AnnCase scrut bndr ty alts)  vt
  | Just (tycon, ty_args) <- splitTyConApp_maybe scrut_ty
  , isAlgTyCon tycon
  = vectAlgCase tycon ty_args scrut bndr ty alts vt
  | otherwise = do dflags <- getDynFlags
                   cantVectorise dflags "Can't vectorise expression" (ppr scrut_ty)
  where
    scrut_ty = exprType (deAnnotate scrut)

vectExpr (_, AnnLet (AnnNonRec bndr rhs) body) (VITNode _ [vt1, vt2]) 
  = do
      vrhs <- localV . inBind bndr . liftM (\(_,_,z)->z) $ vectPolyExpr False [] rhs (Just vt1)
      (vbndr, vbody) <- vectBndrIn bndr (vectExpr body vt2)
      return $ vLet (vNonRec vbndr vrhs) vbody

vectExpr (_, AnnLet (AnnRec bs) body) (VITNode _ (vtB : vtBnds))
  = do
      (vbndrs, (vrhss, vbody)) <- vectBndrsIn bndrs
                                $ liftM2 (,)
                                  (zipWith3M vect_rhs bndrs rhss vtBnds)
                                  (vectExpr body vtB)
      return $ vLet (vRec vbndrs vrhss) vbody
  where
    (bndrs, rhss) = unzip bs

    vect_rhs bndr rhs vt = localV
                         . inBind bndr
                         . liftM (\(_,_,z)->z)
                         $ vectPolyExpr (isStrongLoopBreaker $ idOccInfo bndr) [] rhs (Just vt)
    zipWith3M f xs ys zs = zipWithM (\x -> \(y,z) -> (f x y z)) xs (zip ys zs)

vectExpr (_, AnnTick tickish expr)  (VITNode _ [vit])
  = liftM (vTick tickish) (vectExpr expr vit)

vectExpr (_, AnnType ty) _
  = liftM vType (vectType ty)

vectExpr e vit = do dflags <- getDynFlags
                    cantVectorise dflags "Can't vectorise expression (vectExpr)" (ppr (deAnnotate e) $$ text ("  " ++ show vit))

-- |Vectorise an expression that *may* have an outer lambda abstraction.
--
-- We do not handle type variables at this point, as they will already have been stripped off by
-- 'vectPolyExpr'.  We also only have to worry about one set of dictionary arguments as we (1) only
-- deal with Haskell 2011 and (2) class selectors are vectorised elsewhere.
--
vectFnExpr :: Bool             -- ^ If we process the RHS of a binding, whether that binding should
                               --   be inlined
           -> Bool             -- ^ Whether the binding is a loop breaker
           -> [Var]            -- ^ Names of function in same recursive binding group
           -> CoreExprWithFVs  -- ^ Expression to vectorise; must have an outer `AnnLam`
           -> VITree
           -> VM (Inline, Bool, VExpr)
-- vectFnExpr _ _ _ e vi | not (checkTree vi (deAnnotate e))
--   = pprPanic "vectFnExpr" (ppr $ deAnnotate e)
vectFnExpr inline loop_breaker recFns expr@(_fvs, AnnLam bndr body) vt@(VITNode _ [vt'])
      -- predicate abstraction: leave as a normal abstraction, but vectorise the predicate type
  | isId bndr
    && isPredTy (idType bndr)
  = do { vBndr <- vectBndr bndr
       ; (inline, isScalarFn, vbody) <- vectFnExpr inline loop_breaker recFns body vt'
       ; return (inline, isScalarFn, mapVect (mkLams [vectorised vBndr]) vbody)
       }
      -- non-predicate abstraction: vectorise (try to vectorise as a scalar computation)
  | isId bndr
  = mark DontInline True (vectScalarFunMaybe (deAnnotate expr) vt)
    `orElseV` 
    mark inlineMe False (vectLam inline loop_breaker expr vt)
vectFnExpr _ _ _  e vt
      -- not an abstraction: vectorise as a vanilla expression
  = mark DontInline False $ vectExpr e vt

mark :: Inline -> Bool -> VM a -> VM (Inline, Bool, a)
mark b isScalarFn p = do { x <- p; return (b, isScalarFn, x) }

-- |Vectorise type and dictionary applications.
--
-- These are always headed by a variable (as we don't support higher-rank polymorphism), but may
-- involve two sets of type variables and dictionaries.  Consider,
--
-- > class C a where
-- >   m :: D b => b -> a
--
-- The type of 'm' is 'm :: forall a. C a => forall b. D b => b -> a'.
--
vectPolyApp :: CoreExprWithFVs -> VM VExpr
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
vectScalarFunMaybe :: CoreExpr   -- ^ Expression to be vectorised
                   -> VITree     -- ^ Vectorisation information
                   -> VM VExpr
vectScalarFunMaybe expr  (VITNode VIEncaps _) = vectScalarFun expr
vectScalarFunMaybe _expr _                    = noV $ ptext (sLit "not a scalar function")

-- |Vectorise an expression of functional type by lifting it by an application of a member of the
-- zipWith family (i.e., 'map', 'zipWith', zipWith3', etc.)  This is only a valid strategy if the
-- function does not contain parallel subcomputations and has only 'Scalar' types in its result and
-- arguments — this is a predcondition for calling this function.
--
-- Dictionary functions are also scalar functions (as dictionaries themselves are not vectorised,
-- instead they become dictionaries of vectorised methods).  We treat them differently, though see
-- "Note [Scalar dfuns]" in 'Vectorise'.
--
vectScalarFun :: CoreExpr -> VM VExpr
vectScalarFun expr 
  = do 
    { traceVt "vectScalarFun" (ppr expr) 
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
    (tycon, tys) = splitTyConApp ty
    Just dataCon = isDataProductTyCon_maybe tycon
    Just cls     = tyConClass_maybe tycon
    methTys      = dataConInstArgTys dataCon tys
    selIds       = classAllSelIds cls

-- Vectorise an 'n'-ary lambda abstraction by building a set of 'n' explicit closures.
--
-- All non-dictionary free variables go into the closure's environment, whereas the dictionary
-- variables are passed explicit (as conventional arguments) into the body during closure
-- construction.
--
vectLam :: Bool             -- ^ When the RHS of a binding, whether that binding should be inlined.
        -> Bool             -- ^ Whether the binding is a loop breaker.
        -> CoreExprWithFVs  -- ^ Body of abstraction.
        -> VITree
        -> VM VExpr
vectLam inline loop_breaker expr@(fvs, AnnLam _ _)  vi
 = do { let (bndrs, body) = collectAnnValBinders expr

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
             ;  let viBody = stripLams expr vi
             -- ; checkTreeAnnM vi expr
             ; (vbndrs, vbody) <- vectBndrsIn (fvs_nondict ++ bndrs) (vectExpr body viBody)

             ; vbody' <- break_loop lc res_ty vbody
             ; return $ vLams lc vbndrs vbody'
             }
      }
  where
    stripLams  (_, AnnLam _ e)  (VITNode _ [vt]) = stripLams e vt
    stripLams _ vi = vi
    
    maybe_inline n | inline    = Inline n
                   | otherwise = DontInline

    -- If this is the body of a binding marked as a loop breaker, add a recursion termination test
    -- to the /lifted/ version of the function body.  The termination tests checks if the lifting
    -- context is empty.  If so, it returns an empty array of the (lifted) result type instead of
    -- executing the function body.  This is the test from the last line (defining \mathcal{L}')
    -- in Figure 6 of HtM.
    break_loop lc ty (ve, le)
      | loop_breaker
      = do { dflags <- getDynFlags
           ; empty <- emptyPD ty
           ; lty   <- mkPDataType ty
           ; return (ve, mkWildCase (Var lc) intPrimTy lty
                           [(DEFAULT, [], le),
                            (LitAlt (mkMachInt dflags 0), [], empty)])
           }
      | otherwise = return (ve, le)
vectLam _ _ _ _ = panic "vectLam"

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

-- FIXME: this is too lazy
vectAlgCase :: TyCon -> [Type] -> CoreExprWithFVs-> Var -> Type  
            -> [(AltCon, [Var], CoreExprWithFVs)]  -> VITree
            -> VM VExpr
vectAlgCase _tycon _ty_args scrut bndr ty [(DEFAULT, [], body)] (VITNode _ (scrutVit : [altVit]))
  = do
      vscrut         <- vectExpr scrut scrutVit
      (vty, lty)     <- vectAndLiftType ty
      (vbndr, vbody) <- vectBndrIn bndr (vectExpr body altVit)
      return $ vCaseDEFAULT vscrut vbndr vty lty vbody

vectAlgCase _tycon _ty_args scrut bndr ty [(DataAlt _, [], body)] (VITNode _ (scrutVit : [altVit]))
  = do
      vscrut         <- vectExpr scrut scrutVit
      (vty, lty)     <- vectAndLiftType ty
      (vbndr, vbody) <- vectBndrIn bndr (vectExpr body altVit)
      return $ vCaseDEFAULT vscrut vbndr vty lty vbody

vectAlgCase _tycon _ty_args scrut bndr ty [(DataAlt dc, bndrs, body)] (VITNode _ (scrutVit : [altVit]))
  = do
      (vty, lty) <- vectAndLiftType ty
      vexpr      <- vectExpr scrut scrutVit
      (vbndr, (vbndrs, (vect_body, lift_body)))
         <- vect_scrut_bndr
          . vectBndrsIn bndrs
          $ vectExpr body altVit
      let (vect_bndrs, lift_bndrs) = unzip vbndrs
      (vscrut, lscrut, pdata_dc) <- pdataUnwrapScrut (vVar vbndr)
      vect_dc <- maybeV dataConErr (lookupDataCon dc)

      let vcase = mk_wild_case vscrut vty vect_dc  vect_bndrs vect_body
          lcase = mk_wild_case lscrut lty pdata_dc lift_bndrs lift_body

      return $ vLet (vNonRec vbndr vexpr) (vcase, lcase)
  where
    vect_scrut_bndr | isDeadBinder bndr = vectBndrNewIn bndr (fsLit "scrut")
                    | otherwise         = vectBndrIn bndr

    mk_wild_case expr ty dc bndrs body
      = mkWildCase expr (exprType expr) ty [(DataAlt dc, bndrs, body)]
      
    dataConErr = (text "vectAlgCase: data constructor not vectorised" <+> ppr dc)

vectAlgCase tycon _ty_args scrut bndr ty alts (VITNode _ (scrutVit : altVits))
  = do
      vect_tc     <- maybeV tyConErr (lookupTyCon tycon)
      (vty, lty)  <- vectAndLiftType ty

      let arity = length (tyConDataCons vect_tc)
      sel_ty <- builtin (selTy arity)
      sel_bndr <- newLocalVar (fsLit "sel") sel_ty
      let sel = Var sel_bndr

      (vbndr, valts) <- vect_scrut_bndr
                      $ mapM (proc_alt arity sel vty lty) (zip alts' altVits)
      let (vect_dcs, vect_bndrss, lift_bndrss, vbodies) = unzip4 valts

      vexpr <- vectExpr scrut scrutVit
      (vect_scrut, lift_scrut, pdata_dc) <- pdataUnwrapScrut (vVar vbndr)

      let (vect_bodies, lift_bodies) = unzip vbodies

      vdummy <- newDummyVar (exprType vect_scrut)
      ldummy <- newDummyVar (exprType lift_scrut)
      let vect_case = Case vect_scrut vdummy vty
                           (zipWith3 mk_vect_alt vect_dcs vect_bndrss vect_bodies)

      lc <- builtin liftingContext
      lbody <- combinePD vty (Var lc) sel lift_bodies
      let lift_case = Case lift_scrut ldummy lty
                           [(DataAlt pdata_dc, sel_bndr : concat lift_bndrss,
                             lbody)]

      return . vLet (vNonRec vbndr vexpr)
             $ (vect_case, lift_case)
  where
    tyConErr = (text "vectAlgCase: type constructor not vectorised" <+> ppr tycon)

    vect_scrut_bndr | isDeadBinder bndr = vectBndrNewIn bndr (fsLit "scrut")
                    | otherwise         = vectBndrIn bndr

    alts' = sortBy (\(alt1, _, _) (alt2, _, _) -> cmp alt1 alt2) alts

    cmp (DataAlt dc1) (DataAlt dc2) = dataConTag dc1 `compare` dataConTag dc2
    cmp DEFAULT       DEFAULT       = EQ
    cmp DEFAULT       _             = LT
    cmp _             DEFAULT       = GT
    cmp _             _             = panic "vectAlgCase/cmp"

    proc_alt arity sel _ lty ((DataAlt dc, bndrs, body),  vi)
      = do
          dflags <- getDynFlags
          vect_dc <- maybeV dataConErr (lookupDataCon dc)
          let ntag = dataConTagZ vect_dc
              tag  = mkDataConTag dflags vect_dc
              fvs  = freeVarsOf body `delVarSetList` bndrs

          sel_tags  <- liftM (`App` sel) (builtin (selTags arity))
          lc        <- builtin liftingContext
          elems     <- builtin (selElements arity ntag)

          (vbndrs, vbody)
            <- vectBndrsIn bndrs
             . localV
             $ do
                 binds    <- mapM (pack_var (Var lc) sel_tags tag)
                           . filter isLocalId
                           $ varSetElems fvs
                 (ve, le) <- vectExpr body vi
                 return (ve, Case (elems `App` sel) lc lty
                             [(DEFAULT, [], (mkLets (concat binds) le))])
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

    pack_var len tags t v
      = do
          r <- lookupVar v
          case r of
            Local (vv, lv) ->
              do
                lv'  <- cloneVar lv
                expr <- packByTagPD (idType vv) (Var lv) len tags t
                updLEnv (\env -> env { local_vars = extendVarEnv
                                                (local_vars env) v (vv, lv') })
                return [(NonRec lv' expr)]

            _ -> return []
            
vectAlgCase tycon _ty_args _scrut _bndr _ty _alts (VITNode _ _)
  = pprPanic "vectAlgCase (mismatched node information)" (ppr tycon)


-- Support to compute information for vectorisation avoidance ------------------

-- Annotation for Core AST nodes that describes how they should be handled during vectorisation
-- and especially if vectorisation of the corresponding computation can be avoided.
--
data VectAvoidInfo = VIParr       -- tree contains parallel computations
                   | VISimple     -- result type is scalar & no parallel subcomputation
                   | VIComplex    -- any result type, no parallel subcomputation
                   | VIEncaps     -- tree encapsulated by 'liftSimple'
                   deriving (Eq, Show)

-- Instead of integrating the vectorisation avoidance information into Core expression, we keep
-- them in a separate tree (that structurally mirrors the Core expression that it annotates).
--
data VITree = VITNode VectAvoidInfo [VITree] 
            deriving (Show)

-- Is any of the tree nodes a 'VIPArr' node?
--
anyVIPArr :: [VITree] -> Bool
anyVIPArr = or . (map (\(VITNode vi _) -> vi == VIParr))

-- Compute Core annotations to determine for which subexpressions we can avoid vectorisation
--
-- FIXME: free scalar vars don't actually need to be passed through, since encapsulations makes sure,
--        that there are no free variables in encapsulated lambda expressions     
vectAvoidInfo :: CoreExprWithFVs -> VM VITree
vectAvoidInfo ce@(_, AnnVar v) 
  = do { vi <- vectAvoidInfoType $ exprType $ deAnnotate ce
       ; viTrace ce vi [] 
       ; traceVt "vectAvoidInfo AnnVar" ((ppr v) <+> (ppr $ exprType $ deAnnotate ce))
       ; return $ VITNode vi []
       }

vectAvoidInfo ce@(_, AnnLit _)     
  = do { vi <- vectAvoidInfoType $ exprType $ deAnnotate ce  
       ; viTrace ce vi [] 
       ; traceVt "vectAvoidInfo AnnLit" (ppr $ exprType $ deAnnotate ce)       
       ; return $ VITNode vi []
       }

vectAvoidInfo ce@(_, AnnApp e1 e2)  
  = do { vt1  <- vectAvoidInfo e1  
       ; vt2  <- vectAvoidInfo e2  
       ; vi <- if anyVIPArr [vt1, vt2] 
                    then return VIParr
                    else vectAvoidInfoType $ exprType $ deAnnotate ce
       ; viTrace ce vi [vt1, vt2]                     
       ; return $ VITNode vi [vt1, vt2]
       }

vectAvoidInfo ce@(_, AnnLam _var body) 
  = do { vt@(VITNode vi _) <- vectAvoidInfo body  
       ; viTrace ce vi [vt]
       ; let resultVI | vi == VIParr = VIParr
                      | otherwise    = VIComplex
       ; return $ VITNode resultVI [vt]
       }

vectAvoidInfo ce@(_, AnnLet (AnnNonRec _var expr) body)  
  = do { vtE <- vectAvoidInfo expr 
       ; vtB <- vectAvoidInfo body 
       ; vi <- if anyVIPArr [vtE, vtB] 
                 then return VIParr
                 else vectAvoidInfoType $ exprType $ deAnnotate ce
       ; viTrace ce vi [vtE, vtB]                                          
       ; return $ VITNode vi [vtE, vtB]
       }

vectAvoidInfo ce@(_, AnnLet (AnnRec bnds) body)  
  = do { let (_, exprs) = unzip bnds
       ; vtBnds <- mapM (\e -> vectAvoidInfo e) exprs
       ; if (anyVIPArr vtBnds)
            then do { vtBnds' <- mapM (\e -> vectAvoidInfo e) exprs    
                    ; vtB <- vectAvoidInfo body 
                    ; return (VITNode VIParr (vtB: vtBnds'))
                    }
            else do { vtB@(VITNode vib _)  <- vectAvoidInfo body 
                    ; ni <- if (vib == VIParr) 
                               then return VIParr
                               else vectAvoidInfoType $ exprType $ deAnnotate ce
                    ; viTrace ce ni (vtB : vtBnds)                                        
                    ; return $ VITNode ni (vtB : vtBnds)
                    }
       }

vectAvoidInfo ce@(_, AnnCase expr _var _ty alts) 
  = do { vtExpr <- vectAvoidInfo expr 
       ; vtAlts <- mapM (\(_, _, e) -> vectAvoidInfo e) alts
       ; ni <- if anyVIPArr (vtExpr : vtAlts)
                 then return VIParr
                 else vectAvoidInfoType $ exprType $ deAnnotate ce
       ; viTrace ce ni (vtExpr  : vtAlts)
       ; return $ VITNode ni (vtExpr: vtAlts)
       }

vectAvoidInfo (_, AnnCast expr _)       
  = do { vt@(VITNode vi _) <- vectAvoidInfo expr 
       ; return $ VITNode vi [vt]
       }

vectAvoidInfo (_, AnnTick _ expr)       
  = do { vt@(VITNode vi _) <- vectAvoidInfo expr 
       ; return $ VITNode vi [vt]
       }

vectAvoidInfo (_, AnnType {})  
  = return $ VITNode VISimple []

vectAvoidInfo (_, AnnCoercion {}) 
  = return $ VITNode VISimple []

-- Compute vectorisation avoidance information for a type.
--
vectAvoidInfoType :: Type -> VM VectAvoidInfo   
vectAvoidInfoType ty 
  | maybeParrTy ty = return VIParr
  | otherwise      
  = do { sType <- isSimpleType ty
       ; if sType 
           then return VISimple
           else return VIComplex
       }

-- Checks whether the type might be a parallel array type.  In particular, if the outermost
-- constructor is a type family, we conservatively assume that it may be a parallel array type.
--
maybeParrTy :: Type -> Bool
maybeParrTy ty 
    | Just ty'         <- coreView ty            = maybeParrTy ty'
    | Just (tyCon, ts) <- splitTyConApp_maybe ty = isPArrTyCon tyCon || isSynFamilyTyCon tyCon  
                                                 || or (map maybeParrTy ts)
maybeParrTy _  = False               

-- FIXME: This should not be hardcoded.
isSimpleType :: Type -> VM Bool
isSimpleType ty 
  | Just (c, _cs) <- splitTyConApp_maybe ty 
  = return $ (tyConName c) `elem` [boolTyConName, intTyConName, word8TyConName, doubleTyConName, floatTyConName]
{-
    = do { globals <- globalScalarTyCons
          ; traceVt ("isSimpleType " ++ (show (elemNameSet (tyConName c) globals ))) (ppr c)  
          ; return (elemNameSet (tyConName c) globals ) 
          } 
  -}
  | Nothing <- splitTyConApp_maybe ty
    = return False
isSimpleType ty 
  = pprPanic "Vectorise.Exp.isSimpleType not handled" (ppr ty)

varsSimple :: VarSet -> VM Bool
varsSimple vs 
  = do { varTypes <- mapM isSimpleType $ map varType $  varSetElems vs
       ; return $ and varTypes
       }

viTrace :: CoreExprWithFVs -> VectAvoidInfo -> [VITree] -> VM ()
viTrace ce vi vTs
  = traceVt ("vitrace " ++ (show vi) ++ "[" ++ (concat $ map (\(VITNode vi _) -> show vi ++ " ") vTs) ++"]") 
            (ppr $ deAnnotate ce)


{-
---- Sanity check  of the tree, for debugging only
checkTree :: VITree -> CoreExpr -> Bool
checkTree  (VITNode _ []) (Type _ty) 
  = True
      
checkTree  (VITNode _ [])  (Var _v)  
  = True
  
checkTree  (VITNode _ [])  (Lit _)
  = True
         
checkTree (VITNode _ [vit]) (Tick _ expr)
  = checkTree vit expr
  
checkTree (VITNode _ [vit]) (Lam _ expr) 
  = checkTree vit expr
  
checkTree (VITNode _ [vit1, vit2])  (App ce1 ce2) 
  = (checkTree vit1 ce1) && (checkTree vit2 ce2) 
        
checkTree (VITNode _ (scrutVit : altVits)) (Case scrut _ _ alts) 
  = (checkTree scrutVit scrut) && (and $ zipWith checkAlt altVits alts)
  where
    checkAlt vt (_, _, expr) = checkTree vt expr
    
checkTree (VITNode _ [vt1, vt2]) (Let (NonRec _ expr1) expr2) 
  = (checkTree vt1 expr1) && (checkTree vt2 expr2) 

checkTree (VITNode _ (vtB : vtBnds))  (Let (Rec bndngs) expr) 
  = (and $ zipWith checkBndr vtBnds bndngs) && 
    (checkTree vtB expr)
 where 
   checkBndr vt (_, e) = checkTree vt e
              
checkTree (VITNode _ [vit]) (Cast expr _)
  = checkTree vit expr

checkTree _ _ = False

checkTreeAnnM:: VITree -> CoreExprWithFVs -> VM ()
checkTreeAnnM vi e =  
  if not (checkTree vi $ deAnnotate e)
    then error ("checkTreeAnnM : \n " ++ show vi)
    else return ()
-}
