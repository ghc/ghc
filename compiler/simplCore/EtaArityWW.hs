module EtaArityWW (etaArityWW, shallowEtaType, deepEtaType) where

import GhcPrelude

import DynFlags
import BasicTypes
import CoreSyn
import CoreSubst
import CoreArity
import CoreFVs
import CoreUnfold
import Id
import IdInfo
import TyCoRep
import Type
import UniqSupply
import VarEnv
import Outputable
import MonadUtils

import qualified Data.Map as F

data WWInfo
  = NoWW
  | JustUnbox
  | JustEtaArity
  | BothUnboxAndEtaArity

{-
************************************************************************
*                                                                      *
                   Call Arity in the Types
*                                                                      *
************************************************************************

Goal:

Expose more arity information at code generation by tracking the arity of top
level (though let-bound terms should be included too) terms in the types.
-}

etaArityWW
  :: DynFlags -> UniqSupply -> CoreProgram -> CoreProgram
etaArityWW dflags us binds
  = initUs_ us $ concatMapM (etaArityWWBind dflags) binds

-- ^ Given a CoreBind, produce the WorkerWrapper transformed
-- version. This transformation may or may not produce new top level entities
-- depending on its arity.
etaArityWWBind :: DynFlags -> CoreBind -> UniqSM [CoreBind]
etaArityWWBind dflags (NonRec name expr)
  = map (uncurry NonRec)
  <$> (etaArityWWBind' dflags name =<< etaArityWWExpr dflags expr)
etaArityWWBind dflags (Rec binds)
  = (return . Rec)
  <$> concatMapM (\(id,expr) ->
                    etaArityWWBind' dflags id =<< etaArityWWExpr dflags expr)
                 binds

-- ^ Change a function binding into a call to its wrapper and the production of
-- a wrapper. The worker/wrapper transformation *only* makes sense for Id's or
-- binders to code.
etaArityWWBind'
  :: DynFlags
  -> Id
  -> CoreExpr
  -> UniqSM [(Id,CoreExpr)]
  -- the first component are recursive binds and the second are non-recursive
  -- binds (the wrappers are non-recursive)
etaArityWWBind' dflags fn_id rhs
  | arity >= 1                            -- * we only do etaArityWW on
                                          --   functions
    && isId fn_id                         -- * only work on terms
    && not (isJoinId fn_id)               -- * do not interfere with join points
    && not (isFunTildeTy (idType fn_id))  -- * do not worker/wrapper, things
                                          --   that are already tildefuns
  = let fm              = calledArityMap rhs
        work_ty         = deepEtaType (idType fn_id)
        fn_info         = idInfo fn_id
        fn_inl_prag     = inlinePragInfo fn_info
        fn_inline_spec  = inl_inline fn_inl_prag
        fn_act          = inl_act fn_inl_prag
        rule_match_info = inlinePragmaRuleMatchInfo fn_inl_prag
        -- see [Note Inlining etaWW]
        work_prag       = InlinePragma
                        { inl_src       = SourceText "{-# INLINE"
                        , inl_inline    = NoInline
                        , inl_sat       = Nothing
                        , inl_act       = work_act
                        , inl_rule      = FunLike }
        work_act        = case fn_act of
                            ActiveAfter {} -> fn_act
                            NeverActive    -> ActiveAfter NoSourceText 0
                            _              -> ActiveAfter NoSourceText 2
        wrap_prag       = alwaysInlinePragma
    in
    do { uniq <- getUniqueM
       ; let work_id = mkEtaWorkerId uniq fn_id work_ty
                         `setIdOccInfo` occInfo fn_info
                         `setIdArity` arity
                         `setInlinePragma` work_prag
             wrap_id = fn_id `setIdOccInfo` noOccInfo
                             `setInlinePragma` wrap_prag
             work_rhs = mkArityWorkerRhs fn_id work_id rhs
       ; wrap_rhs <- mkArityWrapperRhs fm work_id rhs arity
       ; return [(work_id `setIdUnfolding` mkSimpleUnfolding dflags work_rhs
                 ,work_rhs)
                ,(wrap_id `setIdUnfolding` mkSimpleUnfolding dflags wrap_rhs
                 ,wrap_rhs)] }

  | otherwise
  = return [(fn_id,rhs)]

  where arity = manifestArity rhs

{- [Note Inlining etaWW]

  Inlining the worker will nullify this worker/wrapper transformation. However,
  the opposite is not necessarily the best either.

  If I try to never inline the worker, then there is one case in which we get
  -83% allocations only one program. Most of the other programs show a minor
  increase in allocations. Also minimax loops forever and bernoulli has a
  linking error
-}

-- ^ Traverses the expression to do etaArityWWBind in let-expressions
etaArityWWExpr :: DynFlags -> CoreExpr -> UniqSM CoreExpr
etaArityWWExpr _ e@(Var _) = return e
etaArityWWExpr _ e@(Lit _) = return e
etaArityWWExpr dflags (App res arg)
  = App <$> etaArityWWExpr dflags res <*> etaArityWWExpr dflags arg
etaArityWWExpr dflags (Lam bndr expr) = Lam bndr <$> etaArityWWExpr dflags expr
etaArityWWExpr dflags (Let bind expr)
  = mkLets <$> etaArityWWBind dflags bind <*> etaArityWWExpr dflags expr
etaArityWWExpr dflags (Case expr bndr ty alts)
  = do { expr' <- etaArityWWExpr dflags expr
       ; alts' <- mapM goAlt alts
       ; return (Case expr' bndr ty alts') }
  where goAlt (con,bndrs,expr) =
          etaArityWWExpr dflags expr >>= \expr' -> return (con,bndrs,expr')
etaArityWWExpr dflags (Cast expr co)
  = Cast <$> etaArityWWExpr dflags expr <*> return co
etaArityWWExpr dflags (Tick tk expr) = Tick tk <$> etaArityWWExpr dflags expr
etaArityWWExpr _ e@(Type _) = return e
etaArityWWExpr _ e@(Coercion _) = return e

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

The wrapper eta-expands all functions so that the worker can assume that its
arguments are the most extensional functions types.
-}

{-
@calledArityMap@ takes a core expression (meant to be the RHS of a top level
binding) and returns a Map of binders to an arity. This map will be used for
determining how much to etaExpand the higher-order functions used in
@mkArityWrapper@.

foo :: (Int -> Int -> Int) -> Int
foo f =
  let x = f 1 2
      y = f 2 in
    x + y 3
-}
calledArityMap :: CoreExpr -> F.Map Id Arity
calledArityMap e =
  case e of
    Var x -> F.singleton x 0

    Lit _ -> F.empty

    expr@(App _ _) ->
      case collectArgs expr of
        (Var x,args) ->
          let fm = F.unionsWith max (map calledArityMap args)
              a  = length args in
            F.unionWith max (F.singleton x a) fm
        (_,args) -> F.unionsWith max (map calledArityMap args)

    Lam _ expr -> calledArityMap expr

    Let bnds expr ->
      let fm = F.unionsWith max (map calledArityMap (rhssOfBind bnds)) in
        F.unionWith max fm (calledArityMap expr)

    Case expr _ _ alts ->
      let fm = F.unionsWith max (map calledArityMap (rhssOfAlts alts)) in
        F.unionWith max fm (calledArityMap expr)

    Cast expr _ -> calledArityMap expr

    Tick _ expr -> calledArityMap expr

    Type _ -> F.empty

    Coercion _ -> F.empty

{-
@exprArityType@ creates the new type for an extensional function given the
arity. We also need to consider higher-order functions. The type of a function
argument can change based on the usage of the type in the body of the
function. For example, consider the zipWith function.

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
exprArityType :: Arity -> Type -> CoreExpr -> F.Map Id Arity -> Type
exprArityType n (ForAllTy tv body_ty) (Lam _ expr) fm
  = ForAllTy tv (exprArityType n body_ty expr fm)
exprArityType 0 (FunTy arg res) (Lam bndr expr) fm
  = FunTy (nArgsEtaType (F.findWithDefault 0 bndr fm) arg)
          (exprArityType 0 res expr fm)
exprArityType n (FunTy arg res) (Lam bndr expr) fm
  = FunTildeTy (nArgsEtaType (F.findWithDefault 0 bndr fm) arg)
               (exprArityType (n-1) res expr fm)
exprArityType _ ty _ _ = ty

nArgsEtaType :: Arity -> Type -> Type
nArgsEtaType n (ForAllTy tv body_ty) = ForAllTy tv (nArgsEtaType n body_ty)
nArgsEtaType 0 (FunTy arg res)       = FunTy arg (nArgsEtaType 0 res)
nArgsEtaType n (FunTy arg res)       = FunTildeTy arg (nArgsEtaType (n-1) res)
nArgsEtaType _ ty                    = ty


-- ^ As described in Note [Extensionality and Higher-Order Functions],
-- extentionalize returns the most extensional version of a type. This only
-- effects function types

-- TODO Coercions need an extensionalize function
shallowEtaType :: Type -> Type
shallowEtaType (ForAllTy tv body_ty) = ForAllTy tv (shallowEtaType body_ty)
shallowEtaType (FunTy arg res)       = FunTy arg (shallowEtaType res)
shallowEtaType (FunTy arg res)       = FunTildeTy arg (shallowEtaType res)
shallowEtaType ty                    = ty

deepEtaType :: Type -> Type
deepEtaType (ForAllTy tv body_ty) = ForAllTy tv (deepEtaType body_ty)
deepEtaType (FunTy arg res)       = FunTildeTy (deepEtaType arg) (deepEtaType res)
deepEtaType (FunTildeTy arg res)  = FunTildeTy (deepEtaType arg) (deepEtaType res)
deepEtaType ty                    = ty


-- ^ Given an expression and it's name, generate a new expression with a
-- tilde-lambda type. This is the exact same code, but we have encoded the arity
-- in the type.
mkArityWorkerRhs
  :: Id
  -> Id
  -> CoreExpr
  -> CoreExpr
mkArityWorkerRhs fn_id work_id rhs
  = substExprSC (text "eta-worker-subst") subst rhs
  where init_subst = mkEmptySubst . mkInScopeSet . exprFreeVars $ rhs
        subst = extendSubstWithVar init_subst fn_id work_id

-- ^ The wrapper does not change the type and will call the newly created worker
-- function.
mkArityWrapperRhs
  :: F.Map Id Arity
  -> Id
  -> CoreExpr
  -> Arity
  -> UniqSM CoreExpr
mkArityWrapperRhs fm work_id expr arity = go fm expr arity work_id []
  where go fm (Lam b e) a w l
          | isId b = let expr = etaExpand (F.findWithDefault 0 b fm) (Var b) in
                       Lam b <$> go fm e (a-1) w (expr : l)
          | otherwise = Lam b <$> go fm e a w (Type (TyVarTy b) : l)
        go _ _ _ w l = return $ mkApps (Var w) (reverse l)
