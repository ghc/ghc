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
import NameSet
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


-- |Vectorise a polymorphic expression.
--
vectPolyExpr :: Bool    -- ^ When vectorising the RHS of a binding: is that binding a loop breaker?
             -> [Var]                     
             -> CoreExprWithFVs
             -> VM (Inline, Bool, VExpr)
vectPolyExpr loop_breaker recFns (_, AnnTick tickish expr)
 = do { (inline, isScalarFn, expr') <- vectPolyExpr loop_breaker recFns expr
      ; return (inline, isScalarFn, vTick tickish expr')
      }
vectPolyExpr loop_breaker recFns expr
 = do { arity <- polyArity tvs
      ; polyAbstract tvs $ \args -> do
      { (inline, isScalarFn, mono') <- vectFnExpr False loop_breaker recFns mono
      ; return (addInlineArity inline arity, isScalarFn, mapVect (mkLams $ tvs ++ args) mono')
      } }
  where
    (tvs, mono) = collectAnnTypeBinders expr

-- |Vectorise an expression.
--
vectExpr :: CoreExprWithFVs -> VM VExpr

vectExpr (_, AnnVar v) 
  = vectVar v

vectExpr (_, AnnLit lit) 
  = vectConst $ Lit lit

vectExpr e@(_, AnnLam bndr _)
  | isId bndr = (\(_, _, ve) -> ve) <$> vectFnExpr True False [] e

  -- SPECIAL CASE: Vectorise/lift 'patError @ ty err' by only vectorising/lifting the type 'ty';
  --   its only purpose is to abort the program, but we need to adjust the type to keep CoreLint
  --   happy.
-- FIXME: can't be do this with a VECTORISE pragma on 'pAT_ERROR_ID' now?
vectExpr (_, AnnApp (_, AnnApp (_, AnnVar v) (_, AnnType ty)) err)
  | v == pAT_ERROR_ID
  = do { (vty, lty) <- vectAndLiftType ty
       ; return (mkCoreApps (Var v) [Type vty, err'], mkCoreApps (Var v) [Type lty, err'])
       }
  where
    err' = deAnnotate err

  -- type application (handle multiple consecutive type applications simultaneously to ensure the
  -- PA dictionaries are put at the right places)
vectExpr e@(_, AnnApp _ arg)
  | isAnnTypeArg arg
  = vectPolyApp e
    
  -- 'Int', 'Float', or 'Double' literal
  -- FIXME: this needs to be generalised
vectExpr (_, AnnApp (_, AnnVar v) (_, AnnLit lit))
  | Just con <- isDataConId_maybe v
  , is_special_con con
  = do
      let vexpr = App (Var v) (Lit lit)
      lexpr <- liftPD vexpr
      return (vexpr, lexpr)
  where
    is_special_con con = con `elem` [intDataCon, floatDataCon, doubleDataCon]

  -- value application (dictionary or user value)
vectExpr e@(_, AnnApp fn arg)
  | isPredTy arg_ty   -- dictionary application (whose result is not a dictionary)
  = vectPolyApp e
  | otherwise         -- user value
  = do {   -- vectorise the types
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
  | otherwise = cantVectorise "Can't vectorise expression" (ppr scrut_ty) 
  where
    scrut_ty = exprType (deAnnotate scrut)

vectExpr (_, AnnLet (AnnNonRec bndr rhs) body)
  = do
      vrhs <- localV . inBind bndr . liftM (\(_,_,z)->z) $ vectPolyExpr False [] rhs
      (vbndr, vbody) <- vectBndrIn bndr (vectExpr body)
      return $ vLet (vNonRec vbndr vrhs) vbody

vectExpr (_, AnnLet (AnnRec bs) body)
  = do
      (vbndrs, (vrhss, vbody)) <- vectBndrsIn bndrs
                                $ liftM2 (,)
                                  (zipWithM vect_rhs bndrs rhss)
                                  (vectExpr body)
      return $ vLet (vRec vbndrs vrhss) vbody
  where
    (bndrs, rhss) = unzip bs

    vect_rhs bndr rhs = localV
                      . inBind bndr
                      . liftM (\(_,_,z)->z)
                      $ vectPolyExpr (isStrongLoopBreaker $ idOccInfo bndr) [] rhs

vectExpr (_, AnnTick tickish expr)
  = liftM (vTick tickish) (vectExpr expr)

vectExpr (_, AnnType ty)
  = liftM vType (vectType ty)

vectExpr e = cantVectorise "Can't vectorise expression (vectExpr)" (ppr $ deAnnotate e)

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
           -> VM (Inline, Bool, VExpr)
vectFnExpr inline loop_breaker recFns expr@(_fvs, AnnLam bndr body)
      -- predicate abstraction: leave as a normal abstraction, but vectorise the predicate type
  | isId bndr
    && isPredTy (idType bndr)
  = do { vBndr <- vectBndr bndr
       ; (inline, isScalarFn, vbody) <- vectFnExpr inline loop_breaker recFns body
       ; return (inline, isScalarFn, mapVect (mkLams [vectorised vBndr]) vbody)
       }
      -- non-predicate abstraction: vectorise (try to vectorise as a scalar computation)
  | isId bndr
  = mark DontInline True (vectScalarFun False recFns (deAnnotate expr))
    `orElseV` 
    mark inlineMe False (vectLam inline loop_breaker expr)
vectFnExpr _ _ _  e 
      -- not an abstraction: vectorise as a vanilla expression
  = mark DontInline False $ vectExpr e

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
-- requires the full blown vectorisation transformation; instead, they can be lifted by application
-- of a member of the zipWith family (i.e., 'map', 'zipWith', zipWith3', etc.)
--
-- Dictionary functions are also scalar functions (as dictionaries themselves are not vectorised,
-- instead they become dictionaries of vectorised methods).  We treat them differently, though see
-- "Note [Scalar dfuns]" in 'Vectorise'.
--
vectScalarFun :: Bool       -- ^ Was the function marked as scalar by the user?
              -> [Var]      -- ^ Functions names in same recursive binding group
              -> CoreExpr   -- ^ Expression to be vectorised
              -> VM VExpr
vectScalarFun forceScalar recFns expr
 = do { gscalarVars  <- globalScalarVars
      ; scalarTyCons <- globalScalarTyCons
      ; let scalarVars = gscalarVars `extendVarSetList` recFns
            (arg_tys, res_ty) = splitFunTys (exprType expr)
      ; MASSERT( not $ null arg_tys )
      ; onlyIfV (ptext (sLit "not a scalar function"))
                (forceScalar                                 -- user asserts the functions is scalar
                 ||
                 all is_primitive_ty arg_tys                 -- check whether the function is scalar
                  && is_primitive_ty res_ty
                  && is_scalar scalarVars (is_scalar_ty scalarTyCons) expr
                  && uses scalarVars expr
                  && length arg_tys <= mAX_DPH_SCALAR_ARGS)
        $ mkScalarFun arg_tys res_ty expr
      }
  where
    -- !!!FIXME: We would like to allow scalar functions with arguments and results that can be
    --           any 'scalarTyCons', but can't at the moment, as those argument and result types
    --           need to be members of the 'Scalar' class (that in its current form would better
    --           be called 'Primitive'). *ALSO* the hardcoded list of types is ugly!
    is_primitive_ty ty
      | isPredTy ty               -- dictionaries never get into the environment
      = True
      | Just (tycon, _) <- splitTyConApp_maybe ty
      = tyConName tycon `elem` [boolTyConName, intTyConName, word8TyConName, doubleTyConName]
      | otherwise 
      = False

    is_scalar_ty scalarTyCons ty 
      | isPredTy ty               -- dictionaries never get into the environment
      = True
      | Just (tycon, _) <- splitTyConApp_maybe ty
      = tyConName tycon `elemNameSet` scalarTyCons
      | otherwise 
      = False

    -- Checks whether an expression contain a non-scalar subexpression. 
    --
    -- Precodition: The variables in the first argument are scalar.
    --
    -- In case of a recursive binding group, we /assume/ that all bindings are scalar (by adding
    -- them to the list of scalar variables) and then check them.  If one of them turns out not to
    -- be scalar, the entire group is regarded as not being scalar.
    --
    -- The second argument is a predicate that checks whether a type is scalar.
    --
    is_scalar :: VarSet -> (Type -> Bool) -> CoreExpr -> Bool
    is_scalar scalars  _isScalarTC (Var v)         = v `elemVarSet` scalars
    is_scalar _scalars _isScalarTC (Lit _)         = True
    is_scalar scalars  isScalarTC  e@(App e1 e2) 
      | maybe_parr_ty (exprType e)                  = False
      | otherwise                                   = is_scalar scalars isScalarTC e1 && 
                                                      is_scalar scalars isScalarTC e2
    is_scalar scalars  isScalarTC  (Lam var body)  
      | maybe_parr_ty (varType var)                 = False
      | otherwise                                   = is_scalar (scalars `extendVarSet` var)
                                                               isScalarTC body
    is_scalar scalars  isScalarTC  (Let bind body) = bindsAreScalar && 
                                                     is_scalar scalars' isScalarTC body
      where
        (bindsAreScalar, scalars') = is_scalar_bind scalars isScalarTC bind
    is_scalar scalars  isScalarTC  (Case e var ty alts)
      | isScalarTC ty                  = is_scalar scalars' isScalarTC e && 
                                         all (is_scalar_alt scalars' isScalarTC) alts
      | otherwise                      = False
      where
        scalars' = scalars `extendVarSet` var
    is_scalar scalars  isScalarTC  (Cast e _coe)   = is_scalar scalars isScalarTC e
    is_scalar scalars  isScalarTC  (Tick _ e   )   = is_scalar scalars isScalarTC e
    is_scalar _scalars _isScalarTC (Type {})       = True
    is_scalar _scalars _isScalarTC (Coercion {})   = True

    -- Result: (<is this binding group scalar>, scalars ++ variables bound in this group)
    is_scalar_bind scalars isScalarTCs (NonRec var e) = (is_scalar scalars isScalarTCs e, 
                                                         scalars `extendVarSet` var)
    is_scalar_bind scalars isScalarTCs (Rec bnds)     = (all (is_scalar scalars' isScalarTCs) es,
                                                         scalars')
      where
        (vars, es) = unzip bnds
        scalars'   = scalars `extendVarSetList` vars

    is_scalar_alt scalars isScalarTCs (_, vars, e) = is_scalar (scalars `extendVarSetList ` vars)
                                                               isScalarTCs e

    -- Checks whether the type might be a parallel array type.  In particular, if the outermost
    -- constructor is a type family, we conservatively assume that it may be a parallel array type.
    maybe_parr_ty :: Type -> Bool
    maybe_parr_ty ty 
      | Just ty'        <- coreView ty            = maybe_parr_ty ty'
      | Just (tyCon, _) <- splitTyConApp_maybe ty = isPArrTyCon tyCon || isSynFamilyTyCon tyCon 
    maybe_parr_ty _                               = False

    -- FIXME: I'm not convinced that this reasoning is (always) sound.  If the identify functions
    --        is called by some other function that is otherwise scalar, it would be very bad
    --        that just this call to the identity makes it not be scalar.
    -- A scalar function has to actually compute something. Without the check,
    -- we would treat (\(x :: Int) -> x) as a scalar function and lift it to
    -- (map (\x -> x)) which is very bad. Normal lifting transforms it to
    -- (\n# x -> x) which is what we want.
    uses funs (Var v)       = v `elemVarSet` funs 
    uses funs (App e1 e2)   = uses funs e1 || uses funs e2
    uses funs (Lam b body)  = uses (funs `extendVarSet` b) body
    uses funs (Let (NonRec _b letExpr) body) 
                            = uses funs letExpr || uses funs  body
    uses funs (Case e _eId _ty alts) 
                            = uses funs e || any (uses_alt funs) alts
    uses _ _                = False

    uses_alt funs (_, _bs, e) = uses funs e 

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
               -> [Var]      -- ^ Functions names in same recursive binding group
               -> VM CoreExpr
vectScalarDFun var recFns
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
       ; vScsOps <- mapM (\e -> vectorised <$> vectScalarFun True recFns e) scsOps

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

-- |Vectorise an 'n'-ary lambda abstraction by building a set of 'n' explicit closures.
--
-- All non-dictionary free variables go into the closure's environment, whereas the dictionary
-- variables are passed explicit (as conventional arguments) into the body during closure
-- construction.
--
vectLam :: Bool             -- ^ When the RHS of a binding, whether that binding should be inlined.
        -> Bool             -- ^ Whether the binding is a loop breaker.
        -> CoreExprWithFVs  -- ^ Body of abstraction.
        -> VM VExpr
vectLam inline loop_breaker expr@(fvs, AnnLam _ _)
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
             ; (vbndrs, vbody) <- vectBndrsIn (fvs_nondict ++ bndrs) (vectExpr body)

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
vectLam _ _ _ = panic "vectLam"

-- | Vectorise an algebraic case expression.
--   We convert
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
vectAlgCase :: TyCon -> [Type] -> CoreExprWithFVs -> Var -> Type
            -> [(AltCon, [Var], CoreExprWithFVs)]
            -> VM VExpr
vectAlgCase _tycon _ty_args scrut bndr ty [(DEFAULT, [], body)]
  = do
      vscrut         <- vectExpr scrut
      (vty, lty)     <- vectAndLiftType ty
      (vbndr, vbody) <- vectBndrIn bndr (vectExpr body)
      return $ vCaseDEFAULT vscrut vbndr vty lty vbody

vectAlgCase _tycon _ty_args scrut bndr ty [(DataAlt _, [], body)]
  = do
      vscrut         <- vectExpr scrut
      (vty, lty)     <- vectAndLiftType ty
      (vbndr, vbody) <- vectBndrIn bndr (vectExpr body)
      return $ vCaseDEFAULT vscrut vbndr vty lty vbody

vectAlgCase _tycon _ty_args scrut bndr ty [(DataAlt dc, bndrs, body)]
  = do
      (vty, lty) <- vectAndLiftType ty
      vexpr      <- vectExpr scrut
      (vbndr, (vbndrs, (vect_body, lift_body)))
         <- vect_scrut_bndr
          . vectBndrsIn bndrs
          $ vectExpr body
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

vectAlgCase tycon _ty_args scrut bndr ty alts
  = do
      vect_tc     <- maybeV tyConErr (lookupTyCon tycon)
      (vty, lty)  <- vectAndLiftType ty

      let arity = length (tyConDataCons vect_tc)
      sel_ty <- builtin (selTy arity)
      sel_bndr <- newLocalVar (fsLit "sel") sel_ty
      let sel = Var sel_bndr

      (vbndr, valts) <- vect_scrut_bndr
                      $ mapM (proc_alt arity sel vty lty) alts'
      let (vect_dcs, vect_bndrss, lift_bndrss, vbodies) = unzip4 valts

      vexpr <- vectExpr scrut
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

    proc_alt arity sel _ lty (DataAlt dc, bndrs, body)
      = do
          vect_dc <- maybeV dataConErr (lookupDataCon dc)
          let ntag = dataConTagZ vect_dc
              tag  = mkDataConTag vect_dc
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
                 (ve, le) <- vectExpr body
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

