{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -fexpose-all-unfoldings #-} -- for specialisation
-- {-# OPTIONS_GHC -fdefer-type-errors #-}
-- {-# OPTIONS_GHC -Wwarn #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module GHC.Core.Semantics where

import GHC.Prelude

import GHC.Core
import GHC.Core.Coercion
import GHC.Core.DataCon

import qualified GHC.Data.Word64Map as WM

import GHC.Types.Literal
import GHC.Types.Id
import GHC.Types.Name
import GHC.Types.Var.Env
import GHC.Types.Unique.Set

import GHC.Utils.Misc
import GHC.Utils.Outputable

import Control.Monad
import Control.Monad.Trans.State
import Data.Word
import GHC.Core.Utils hiding (findAlt)
import GHC.Core.Type
import GHC.Builtin.PrimOps
import GHC.Builtin.Types
import GHC.Types.Var
import GHC.Core.TyCo.Rep
import GHC.Core.FVs
import GHC.Core.Class
import GHC.Types.Id.Info
import GHC.Types.Unique
import GHC.Builtin.Names

data Event = Look Id | LookArg CoreExpr | Update | App1 | App2 | Case1 | Case2 | Let1

class Trace d where
  step :: Event -> d -> d

-- A slight extension of the Domain type from the paper.
-- Note that the 'Id's bear no semantic significance: The `Domain (D τ)`
-- instance simply ignores them. They are needed for analyses and debugging, however.
class Domain d where
  stuck :: d
  erased :: d -- Think of it like coercionToken#
  lit :: Literal -> d
  global :: Id -> d
  classOp :: Id -> Class -> d
  primOp :: Id -> PrimOp -> d
  fun :: Var -> (d -> d) -> d
  con :: DataCon -> [d] -> d
  apply :: d -> (Bool, d) -> d
  select :: d -> CoreExpr -> Id -> [DAlt d] -> d
  keepAlive :: [d] -> d -> d
    -- ^ "keep alive" the list, but return the second argument.
    -- Used for coercion FVs, unfolding and RULE FVs. No simple semantic
    -- description for those; pretend that they may or may not be seq'd.
type DAlt d = (AltCon, [Id], d -> [d] -> d)

data BindHint = BindArg Id | BindLet CoreBind
class HasBind d where
  bind :: BindHint -> [[d] -> d] -> ([d] -> d) -> d
    -- NB: The `BindHint` bears no semantic sigificance:
    --     `HasBind (D (ByNeed T))` does not look at it.
    --     Still useful for analyses!

keepAliveVars :: Domain d => [Id] -> IdEnv d -> d -> d
-- Make sure that the given (local) Ids are all "alive", that is, in scope and
-- reachable through `keepAlive` (which itself is a rather abstract concept).
keepAliveVars xs env | Just ds <- traverse (lookupVarEnv env) xs = keepAlive ds
                     | otherwise                                 = const stuck

keepAliveCo :: Domain d => Coercion -> IdEnv d -> d -> d
-- Coercions are ultimately erased to `coercionToken#` because they are
-- irrelevant to runtime behavior (of a /well-typed/ program).
-- Nevertheless, they have a static semantics that requires its free variables
-- to be present; otherwise the coercion is considered stuck.
keepAliveCo co = keepAliveVars (nonDetEltsUniqSet $ coVarsOfCo co)

keepAliveUnfRules :: Domain d => Id -> IdEnv d -> d -> d
-- ^ `keepAlive` the free Ids of an Id's unfolding and RULE RHSs.
keepAliveUnfRules x = keepAliveVars (nonDetEltsUniqSet $ bndrRuleAndUnfoldingIds x)

feignBndr :: Name -> PiTyBinder -> Var
feignBndr n (Anon (Scaled mult ty) _) = mkLocalIdOrCoVar n mult ty
feignBndr n (Named (Bndr tcv _)) = tcv `setVarName` n

feignId :: Name -> Type -> Id
feignId n ty = mkLocalIdOrCoVar n ManyTy ty

mkPap :: (Trace d, Domain d) => [PiTyBinder] -> ([d] -> d) -> d
mkPap arg_bndrs app_head = go [] (zipWith feignBndr localNames arg_bndrs)
  where
    go ds []     = app_head (reverse ds)
    go ds (x:xs) = fun x (\d -> step App2 $ go (d:ds) xs) -- cf. the Lam case of eval

x1,x2 :: Name
localNames :: [Name]
localNames@(x1:x2:_) = [ mkSystemName (mkUniqueInt 'I' i) (mkVarOcc "local") | i <- [0..] ]

anfise :: (Trace d, Domain d, HasBind d) => [CoreExpr] -> IdEnv d -> ([d] -> d) -> d
anfise es env k = go (zip localNames es) []
  where
    go [] ds = k (reverse ds)
    go ((x,e):es) ds = anf_one x e env $ \d -> go es (d:ds)
    anf_one _ (Lit l) _ k = k (lit l)
    anf_one _ (Var x) env k = evalVar x env k
    anf_one _ (Coercion co) env k = keepAliveCo co env (k erased)
    anf_one _ (Type _ty) _ k = k erased
    anf_one x (Tick _t e) env k = anf_one x e env k
    anf_one x (Cast e co) env k = keepAliveCo co env (anf_one x e env k)
    anf_one x e env k = bind (BindArg (feignId x e_ty)) [\_ -> eval e env]
                       (\ds -> let d = step (LookArg e) (only ds) in
                               if isUnliftedType e_ty && not (exprOkForSpeculation e)
                                 then seq_ (d,e,e_ty) (k d)
                                 else k d)
      where
        e_ty = exprType e
        seq_ :: Domain d => (d,CoreExpr,Type) -> d -> d
        seq_ (a,e,ty) b = select a e wildCardId [(DEFAULT, [], \_a _ds -> b)]
          where
            wildCardId :: Id
            wildCardId = feignBndr wildCardName (Anon (Scaled ManyTy ty) FTF_T_T)

evalConApp :: (Trace d, Domain d, HasBind d) => DataCon -> [d] -> d
evalConApp dc args = case compareLength args rep_ty_bndrs of
  EQ -> con dc args
  GT -> stuck                                             -- oversaturated  => stuck
  LT -> mkPap rest_bndrs $ \etas -> con dc (args ++ etas) -- undersaturated => PAP
  where
    rep_ty_bndrs = fst $ splitPiTys (dataConRepType dc) -- TODO: Cache this in DataCon?
    rest_bndrs = dropList args rep_ty_bndrs

evalVar :: (Trace d, Domain d, HasBind d) => Var -> IdEnv d -> (d -> d) -> d
evalVar x env k = case idDetails x of
  _ | isTyVar x    -> k erased
  DataConWorkId dc -> k (evalConApp dc [])
  DataConWrapId _  -> -- pprTrace "unfolding wrapper" (ppr x $$ ppr (unfoldingTemplate (idUnfolding x))) $
                      k (eval (unfoldingTemplate (idUnfolding x)) emptyVarEnv)
  PrimOpId op _    -> k (primOp x op)
  ClassOpId cls _  -> k (classOp x cls)
  _ | isGlobalId x -> k (global x)
  _                -> maybe stuck k (lookupVarEnv env x)

eval :: (Trace d, Domain d, HasBind d) => CoreExpr -> IdEnv d -> d
eval (Coercion co) env = keepAliveCo co env erased
eval (Type _ty) _ = erased
eval (Lit l) _ = lit l
eval (Tick _t e) env = eval e env
eval (Cast e co) env = keepAliveCo co env (eval e env)
eval (Var x) env = evalVar x env id
eval (Lam x e) env = fun x (\d -> step App2 (eval e (extendVarEnv env x d)))
eval e@App{} env
  | Var v <- f, Just dc <- isDataConWorkId_maybe v
  = anfise as env (evalConApp dc)
  | otherwise
  = anfise (f:as) env $ \(df:das) -> -- NB: anfise is a no-op for Vars
      go df (zipWith (\d a -> (d, isTypeArg a)) das as)
  where
    (f, as) = collectArgs e
    go df [] = df
    go df ((d,is_ty):ds) = go (step App1 $ apply df (is_ty,d)) ds
eval (Let b@(NonRec x rhs) body) env =
  bind (BindLet b)
       -- See Note [Absence analysis for stable unfoldings and RULES]
       [\_  -> keepAliveUnfRules x env $
               eval rhs env]
       (\ds -> step Let1 (eval body (extendVarEnv env x (step (Look x) (only ds)))))
eval (Let b@(Rec binds) body) env =
  bind (BindLet b)
       [\ds -> keepAliveUnfRules x (new_env ds) $
               eval rhs  (new_env ds)  | (x,rhs) <- binds]
       (\ds -> step Let1 (eval body (new_env ds)))
  where
    xs = map fst binds
    new_env ds = extendVarEnvList env (zipWith (\x d -> (x, step (Look x) d)) xs ds)
eval (Case e b _ty alts) env = step Case1 $
  select (eval e env) e b
         [ (con, xs, cont xs rhs) | Alt con xs rhs <- alts ]
  where
    cont xs rhs scrut ds = step Case2 $
      eval rhs (extendVarEnvList env (zipEqual "eval Case{}" (b:xs) (scrut:ds)))
        -- TODO: I think we should ANFise the scrutinee so that the semantics of
        -- an expression like `case Just x of b -> b` actually reflects the heap
        -- allocation. Not now.

-- Haven't figured out yet how to do whole programs, because there is no notion
-- of "evaluation":
--evalProgram :: (Trace d, Domain d, HasBind d) => [CoreRule] -> CoreProgram -> [d]
--evalProgram rules binds
--  where
--    go [] =
--    keep_alive_roots :: AnalEnv -> [Id] -> DmdEnv
--    -- See Note [Absence analysis for stable unfoldings and RULES]
--    -- Here we keep alive "roots", e.g., exported ids and stuff mentioned in
--    -- orphan RULES
--    keep_alive_roots env ids = plusDmdEnvs (map (demandRoot env) (filter is_root ids))
--
--    is_root :: Id -> Bool
--    is_root id = isExportedId id || elemVarSet id rule_fvs
--
--    rule_fvs :: IdSet
--    rule_fvs = rulesRhsFreeIds rules

-- By-need semantics, from the paper

data T v = Step Event (T v) | Ret v
  deriving Functor
instance Applicative T where pure = Ret; (<*>) = ap
instance Monad T where Ret a >>= f = f a; Step ev t >>= f = Step ev (t >>= f)
instance Trace (T v) where step = Step

type D τ = τ (Value τ)
data Value τ
  = Stuck
  | Erased
  | Litt Literal
  | Fun (D τ -> D τ)
  | Con DataCon [D τ]

instance (Trace (D τ), Monad τ) => Domain (D τ) where
  stuck = return Stuck
  lit l = return (Litt l)
  fun _x f = return (Fun f)
  con k ds = return (Con k ds)
  apply d (_b,a) = d >>= \case Fun f -> f a; _ -> stuck
  select d _f _b fs = d >>= \v -> case v of
    Stuck                                                    -> stuck
    Con k ds | Just (_con, _xs, f) <- findAlt (DataAlt k) fs -> f (return v) ds
    Litt l   | Just (_con, _xs, f) <- findAlt (LitAlt l)  fs -> f (return v) []
    _        | Just (_con, _xs, f) <- findAlt DEFAULT     fs -> f (return v) []
    _                                                        -> stuck
  global _        = stuck -- For now; looking at the unfolding would need to call `eval`
  classOp _x _cls = stuck -- For now; looking at the unfolding would need to call `eval`
  primOp _x op = case op of
    IntAddOp -> intop (+)
    IntMulOp -> intop (*)
    IntRemOp -> intop rem
    _        -> stuck
    where
      intop op = binop int_ty int_ty $ \v1 v2 -> case (v1,v2) of
        (Litt (LitNumber LitNumInt i1), Litt (LitNumber LitNumInt i2))
          -> Litt (LitNumber LitNumInt (i1 `op` i2))
        _ -> Stuck
      binop ty1 ty2 f = mkPap [ty1,ty2] $ \[d1,d2] -> f <$> d1 <*> d2
      int_ty = Anon (Scaled ManyTy intTy) FTF_T_T
  erased = return Erased
  keepAlive _ d = d

-- The following function was copy and pasted from GHC.Core.Utils.findAlt:
findAlt :: AltCon -> [DAlt d] -> Maybe (DAlt d)
    -- A "Nothing" result *is* legitimate
    -- See Note [Unreachable code]
findAlt con alts
  = case alts of
        (deflt@(DEFAULT, _, _):alts) -> go alts (Just deflt)
        _                            -> go alts Nothing
  where
    go []                        deflt = deflt
    go (alt@(con1, _, _) : alts) deflt
      = case con `cmpAltCon` con1 of
          LT -> deflt   -- Missed it already; the alts are in increasing order
          EQ -> Just alt
          GT -> go alts deflt

-- By-need semantics, straight from the paper

type Addr = Word64
type Heap τ = WM.Word64Map (D τ)
newtype ByNeed τ v = ByNeed { runByNeed :: StateT (Heap (ByNeed τ)) τ v }
  deriving (Functor, Applicative, Monad)

instance (forall v. Trace (τ v)) => Trace (ByNeed τ v) where
  step ev (ByNeed (StateT m)) = ByNeed $ StateT $ step ev . m

fetch :: Monad τ => Addr -> D (ByNeed τ)
fetch a = ByNeed get >>= \μ -> μ WM.! a

memo :: forall τ. (Monad τ, forall v. Trace (τ v)) => Addr -> D (ByNeed τ) -> D (ByNeed τ)
memo a d = d >>= ByNeed . StateT . upd
  where upd Stuck μ = return (Stuck :: Value (ByNeed τ), μ)
        upd v     μ = step Update (return (v, WM.insert a (memo a (return v)) μ))

freeList :: Heap τ -> [Addr]
freeList μ = [a..]
  where a = case WM.lookupMax μ of Just (a,_) -> a+1; _ -> 0

instance (Monad τ, forall v. Trace (τ v)) => HasBind (D (ByNeed τ)) where
  bind _hint rhss body = do
    as <- take (length rhss) . freeList <$> ByNeed get
    let ds = map fetch as
    ByNeed $ modify (\μ -> foldr (\(a,rhs) -> WM.insert a (memo a (rhs ds))) μ (zip as rhss))
    body ds

evalByNeed :: CoreExpr -> T (Value (ByNeed T), Heap (ByNeed T))
evalByNeed e = runStateT (runByNeed (eval e emptyVarEnv)) WM.empty

-- Boilerplate
instance Outputable Event where
  ppr (Look n) = text "Look" <> parens (ppr n)
  ppr (LookArg e) = text "LookArg" <> parens (ppr e)
  ppr Update = text "Update"
  ppr App1 = text "App1"
  ppr App2 = text "App2"
  ppr Case1 = text "Case1"
  ppr Case2 = text "Case2"
  ppr Let1 = text "Let1"
instance Outputable v => Outputable (T v) where
  ppr (Step ev τ) = ppr ev <> arrow <> ppr τ
  ppr (Ret v) = char '<' <> ppr v <> char '>'
instance Outputable (Value τ) where
  ppr Stuck = text "stuck"
  ppr Erased = char '_'
  ppr (Litt l) = ppr l
  ppr (Fun _f) = text "Fun"
  ppr (Con dc _ds) = ppr dc
instance Outputable (Heap τ) where
  ppr μ = brackets (pprWithCommas (\(a,_) -> ppr a <> char '↦' <> underscore) (WM.toList μ))
