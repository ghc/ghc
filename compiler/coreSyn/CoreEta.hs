{-
Worker/wrapper transformation for type directed etaExpansion.

To be done as part of tidying just before translation to STG.
-}

module CoreEta (arityWorkerWrapper) where

import GhcPrelude

import BasicTypes
import CoreSyn
import CoreSubst
import CoreArity
import Id
import TyCoRep
import UniqSupply
import Outputable

import qualified Data.Map as F

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
  = arityWorkerWrapper' name expr >>= \e_ww ->
      case e_ww of
        Left  (worker,wrapper) -> return (map (uncurry NonRec) [worker,wrapper])
        Right (n,e) -> return [NonRec n e]
arityWorkerWrapper (Rec binds) =
  do { out <- mapM (uncurry arityWorkerWrapper') binds
     ; let (recs,nonrecs) = collectRecs out
     ; return ([Rec recs] ++ map (uncurry NonRec) nonrecs) }
  where collectRecs = foldr (\x (recs,nonrecs) ->
                               case x of
                                 Left (worker,wrapper) ->
                                   (worker:recs,wrapper:nonrecs)
                                 Right cb -> (cb:recs,nonrecs)
                            )
                            ([],[])

-- ^ Change a function binding into a call to its wrapper and the production of
-- a wrapper. The worker/wrapper transformation *only* makes sense for Id's or
-- binders to code.
arityWorkerWrapper'
  :: CoreBndr
  -> CoreExpr
  -> UniqSM (Either ((CoreBndr,CoreExpr),(CoreBndr,CoreExpr))
                    (CoreBndr,CoreExpr))
  -- the first component are recursive binds and the second are non-recursive
  -- binds (the wrappers are non-recursive)
arityWorkerWrapper' name expr
  = let arity = manifestArity expr in
      case arity >= 1 && isId name of
        True ->
          let ty = exprArityType arity (idType name) in
            do { uniq <- getUniqueM
               ; let wname  = mkWorkerId uniq name ty
               ; let worker = mkArityWorker name wname expr
               ; let fm     = calledArityMap expr
               -- ; panic $ showSDocUnsafe (ppr (F.toList fm))
               -- ; panic $ showSDocUnsafe (ppr expr)
               ; wrapper <- mkArityWrapper fm name wname expr arity
               ; return (Left (worker,wrapper)) }
        False -> return (Right (name,expr))

{- | exprArityType creates the new type for an extensional function given the
arity. We also need to consider higher-order functions. The type of a
function argument can change based on the usage of the type in the body of
the function. For example, consider the zipWith function.

zipWith :: forall a b c. (a -> b -> c) -> [a] -> [b] -> [c]
zipWith =
  /\a -> /\b -> /\c -> \f -> \xs -> \ys ->
    case as of
      [] -> []
      (x:xs') ->
        case bs of
          [] -> []
          (y:ys') ->
            (f x y) : zipWith f xs' ys'

We know that zipWith has the type

forall a b c. (a ~> b ~> c) ~> [a] ~> [b] ~> [c]

because the function is only applied to two arguments in the body of the
function.
-}
exprArityType :: Arity -> Type -> Type
exprArityType n (ForAllTy tv body_ty)
  = ForAllTy tv (exprArityType n body_ty)
exprArityType 0 (FunTy arg res)
  = FunTy (extensionalize arg) (exprArityType 0 res)
exprArityType n (FunTy arg res)
  = FunTildeTy (extensionalize arg) (exprArityType (n-1) res)
exprArityType _ ty = ty

{-
Note [Extensionality and Higher-Order Functions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Consider the following program.

foo f =
  let a = f 1 2
      b = f 2
  in a + b 3

Should we give this program the type,

(Int ~> Int ~> Int) ~> Int
             -or-
(Int ~> Int -> Int) ~> Int

The problem is that we cannot decide what the arity of f from this function it
depends on the definition of the funciton passed in. For instance, (+) has the
type Int ~> Int ~> Int, but the following program has the type Int ~> Int ->
Int.

bar x =
  let x' = factorial x
  in \y -> y + x'

We can remedy this problem of choosing the correct type for higher-order
functions by always assuming the best (i.e. most extensional) type in the
worker, then handling the problems in the wrapper.

fooWorker :: (Int ~> Int ~> Int) ~> Int
fooWorker f =
  let a = f 1 2
      b = f 2
  in a + b 3


fooWrapper :: (Int -> Int -> Int) -> Int
fooWrapper f =
  let f' = \x1 x2 -> f x1 x2
  in fooWorker f'
w
The wrapper eta-expands all functions.
-}

-- ^ As described in Note [Extensionality and Higher-Order Functions],
-- extentionalize returns the most extensional version of a type. This only
-- effects function types

-- TODO Coercions need an extensionalize function
extensionalize :: Type -> Type
extensionalize (TyVarTy v) = TyVarTy v
extensionalize (AppTy a b) = AppTy (extensionalize a) (extensionalize b)
extensionalize (TyConApp tc args) = TyConApp tc (map extensionalize args)
extensionalize (ForAllTy tv a) = ForAllTy tv (extensionalize a)
extensionalize (FunTy a b) = FunTildeTy (extensionalize a) (extensionalize b)
extensionalize (FunTildeTy a b)
  = FunTildeTy (extensionalize a) (extensionalize b)
extensionalize (LitTy l) = LitTy l
extensionalize (CastTy a kc) = CastTy (extensionalize a) kc
extensionalize (CoercionTy c) = CoercionTy c

calledArityMap :: CoreExpr -> F.Map CoreBndr Arity
calledArityMap e =
  case e of
    Var x -> F.singleton x 0

    Lit _ -> F.empty

    expr@(App _ _) ->
      case collectArgs expr of
        (Var x,args) ->
          let fm = F.unionsWith retGreater (map calledArityMap args)
              a  = length args in
            F.unionWith retGreater (F.singleton x a) fm
        (_,args) -> F.unionsWith retGreater (map calledArityMap args)

    Lam _ expr -> calledArityMap expr

    Let bnds expr ->
      let fm = F.unionsWith retGreater (map calledArityMap (rhssOfBind bnds)) in
        F.unionWith retGreater fm (calledArityMap expr)

    Case expr _ _ alts ->
      let fm = F.unionsWith retGreater (map calledArityMap (rhssOfAlts alts)) in
        F.unionWith retGreater fm (calledArityMap expr)

    Cast expr _ -> calledArityMap expr

    Tick _ expr -> calledArityMap expr

    Type _ -> F.empty

    Coercion _ -> F.empty
  where retGreater x y =
          case x > y of
            True  -> x
            False -> y

-- ^ Given an expression and it's name, generate a new expression with a
-- tilde-lambda type. This is the exact same code, but we have encoded the arity
-- in the type.
mkArityWorker
  :: CoreBndr
  -> CoreBndr
  -> CoreExpr
  -> (CoreBndr,CoreExpr)
mkArityWorker name wname expr
  = ( wname , substExpr (text "eta-worker-subst") substitution expr )
  where substitution = extendIdSubst emptySubst name (Var wname)

-- ^ The wrapper does not change the type and will call the newly created worker
-- function.
mkArityWrapper
  :: F.Map CoreBndr Arity
  -> CoreBndr
  -> CoreBndr
  -> CoreExpr
  -> Arity
  -> UniqSM (CoreBndr,CoreExpr)
mkArityWrapper fm name wname expr arity
  = mkArityWrapper' fm expr arity wname [] >>= \expr' ->
     return ( name , expr' )

mkArityWrapper'
  :: F.Map CoreBndr Arity
  -> CoreExpr
  -> Arity
  -> CoreBndr
  -> [CoreExpr]
  -> UniqSM CoreExpr
mkArityWrapper' fm (Lam b e) a w l =
  case isId b of
    True  ->
      -- let expr = panic (showSDocUnsafe (ppr (idCallArity b))) in
      let expr = etaExpand (F.findWithDefault 0 b fm) (Var b) in
      -- let expr = etaExpand undefined (Var b) in
        Lam b <$> mkArityWrapper' fm e (a-1) w (expr : l)
    False ->
      Lam b <$> mkArityWrapper' fm e a w (Type (TyVarTy b) : l)
mkArityWrapper' _ _ _ w l = return $ mkApps (Var w) (reverse l)
