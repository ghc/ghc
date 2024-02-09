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
import GHC.Core.Utils hiding (findAlt, bindNonRec)
import GHC.Core.Type
import GHC.Builtin.PrimOps
import GHC.Builtin.Types
import GHC.Types.Var
import GHC.Core.TyCo.Rep
import GHC.Core.FVs
import GHC.Core.Class
import GHC.Types.Id.Info
import GHC.Types.Unique

data Event = Lookup Id | Update | App1 | App2 | Case1 | Case2 | Let1

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
  fun :: Id -> (d -> d) -> d
  con :: HasCallStack => DataCon -> [d] -> d
  apply :: d -> d -> d
  applyTy :: d -> d -- URGHHH otherwise we have no easy way to discern Type Apps
  select :: d -> CoreExpr -> Id -> [DAlt d] -> d
  keepAlive :: [d] -> d -- Used for coercion FVs, unfolding and RULE FVs. No simple semantic description for those; pretend that they may or may not be seq'd.
  seq_ :: d -> d -> d -- The primitive one. Just like `select a (const False) wildCardId [(DEFAULT, [], \_a _ds -> b)]`, but we don't have available the type of the LHS.
type DAlt d = (AltCon, [Id], d -> [d] -> d)

data BindHint = BindArg | BindNonRec | BindRec
class HasBind d where
  bind :: BindHint -> [(Id, CoreExpr, IdEnv d -> d)] -> (IdEnv d -> d) -> (IdEnv d -> d)
    -- NB: The passing of syntax bears no semantic sigificance:
    --     `HasBind (D (ByNeed T))` does not look at it.
    --     Still useful for analyses!
    -- TODO: since we do not intend to have multiple evaluation strategies, we
    --       could just move `bind` to `Domain`. Do that?

-- | To be used with `bind`
extend :: Trace d => Id -> d -> IdEnv d -> IdEnv d
extend x d env = extendVarEnv env x (step (Lookup x) d)

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

-- The following does not work, because we need the `idType` of `a` in `select`:
-- seq_ :: Domain d => d -> d -> d
-- seq_ a b = select a (const False) wildCardId [(DEFAULT, [], \_a _ds -> b)]
--
-- wildCardId :: Id
-- wildCardId = feignBndr wildCardName (Anon (Scaled ManyTy panicType) FTF_T_T)
--
-- panicType :: HasCallStack => Type
-- panicType = panic "The seq_ case binder should be dead, we don't need its type"

anfise :: (Trace d, Domain d, HasBind d) => Name -> CoreExpr -> IdEnv d -> (d -> d) -> d
anfise _ (Lit l) _ k = k (lit l)
anfise _ (Var x) env k = evalVar x env k
anfise _ (Coercion co) env k = keepAliveCo co env `seq_` k erased
anfise _ (Type _ty) _ k = k erased
anfise x (Tick _t e) env k = anfise x e env k
anfise x (Cast e co) env k = keepAliveCo co env `seq_` anfise x e env k
anfise x e env k = bind BindArg [(x_id, e, \_ -> step (Lookup x_id) (eval e env))]
                   (\env -> let d = evalVar x_id env id in
                            if isUnliftedType (idType x_id) -- && not (exprOkForSpeculation e) -- We evaluate even if ok-for-spec; can always discard those evals later
                              then d `seq_` k d
                              else k d)
                   env
                   where x_id = feignId x (exprType e)

anfiseMany :: (Trace d, Domain d, HasBind d) => [CoreExpr] -> IdEnv d -> ([d] -> d) -> d
anfiseMany es env k = go (zip localNames es) []
  where
    go [] ds = k (reverse ds)
    go ((x,e):es) ds = anfise x e env $ \d -> go es (d:ds)

keepAliveVars :: Domain d => [Id] -> IdEnv d -> d
-- Make sure that the given (local) Ids are all "alive", that is, in scope and
-- reachable through `keepAlive` (which itself is a rather abstract concept).
keepAliveVars xs env | Just ds <- traverse (lookupVarEnv env) xs = keepAlive ds
                     | otherwise                                 = stuck

keepAliveCo :: Domain d => Coercion -> IdEnv d -> d
-- Coercions are ultimately erased to `coercionToken#` because they are
-- irrelevant to runtime behavior (of a /well-typed/ program).
-- Nevertheless, they have a static semantics that requires its free variables
-- to be present; otherwise the coercion is considered stuck.
keepAliveCo co = keepAliveVars (nonDetEltsUniqSet $ coVarsOfCo co)

keepAliveUnfRules :: Domain d => Id -> IdEnv d -> d
-- ^ `keepAlive` the free Ids of an Id's unfolding and RULE RHSs.
keepAliveUnfRules x = keepAliveVars (nonDetEltsUniqSet $ bndrRuleAndUnfoldingIds x)

evalConApp :: (Trace d, Domain d, HasBind d) => DataCon -> [d] -> d
evalConApp dc args = case compareLength rep_ty_bndrs args of
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
eval (Coercion co) env = keepAliveCo co env
eval (Type _ty) _ = erased
eval (Lit l) _ = lit l
eval (Tick _t e) env = eval e env
eval (Cast e co) env = keepAliveCo co env `seq_` eval e env
eval (Var x) env = evalVar x env id
eval (Lam x e) env = fun x (\d -> step App2 (eval e (extendVarEnv env x d)))
eval e@App{} env
  | Var v <- f, Just dc <- isDataConWorkId_maybe v
  = anfiseMany as env (evalConApp dc)
  | otherwise
  = anfiseMany (f:as) env $ \(df:das) -> -- NB: anfise is a no-op for Vars
      go df (zipWith (\d a -> (d, isTypeArg a)) das as)
  where
    (f, as) = collectArgs e
    go df [] = df
    go df ((d,is_ty):ds) = go (step App1 $ app df is_ty d) ds
    app df {-isTypeArg=-}True  _da = applyTy df -- There must be a better way...
    app df               False da  = apply   df da
eval (Let (NonRec x rhs) body) env =
  bind BindNonRec
       -- See Note [Absence analysis for stable unfoldings and RULES]
       [(x,rhs,\_ -> step (Lookup x) (keepAliveUnfRules x env `seq_` eval rhs env))]
       (\env' -> step Let1 (eval body env'))
       env
eval (Let (Rec binds) body) env =
  bind BindRec
       [(x, rhs, \env' -> step (Lookup x) (keepAliveUnfRules x env' `seq_` eval rhs env'))  | (x,rhs) <- binds]
       (\env' -> step Let1 (eval body env'))
       env
eval (Case e b _ty alts) env = step Case1 $
  select (eval e env) e b
         [ (con, xs, cont xs rhs) | Alt con xs rhs <- alts ]
  where
    cont xs rhs scrut ds = step Case2 $
      eval rhs (extendVarEnvList env (zipEqual "eval Case{}" (b:xs) (scrut:ds)))
        -- TODO: Do we want (step (Lookup b) scrut)? I think not, because Case
        -- does not actually allocate itself. On the other hand, not all Values
        -- are currently heap-bound... e.g., case Just x of b -> b would not do
        -- a lookup transition at all, despite `Just x` living on the heap...
        -- Urgh, think about it later.
        -- Literature does not often handle case binders.
        -- Fast Curry and Frame-limited re-use do not, for example.
        -- But the former unconditionally let-binds values, thus absolving of
        -- the problem. Perhaps we should do the same. It's what CorePrep does,
        -- after all.

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
  keepAlive _ = return Erased
  stuck = return Stuck
  erased = return Erased
  lit l = return (Litt l)
  fun _x f = return (Fun f)
  con k ds = return (Con k ds)
  apply d a = d >>= \case Fun f -> f a; _ -> stuck
  applyTy d = d
  select d _f _b fs = d >>= \v -> case v of
    Stuck                                                    -> stuck
    Con k ds | Just (_con, _xs, f) <- findAlt (DataAlt k) fs -> f (return v) ds
    Litt l   | Just (_con, _xs, f) <- findAlt (LitAlt l)  fs -> f (return v) []
    _        | Just (_con, _xs, f) <- findAlt DEFAULT     fs -> f (return v) []
    _                                                        -> stuck
  seq_ a b = a >> b -- The caller may want to insert steps here... Not sure
  global _        = return Stuck -- For now; looking at the unfolding would need to call `eval`
  classOp _x _cls = return Stuck -- For now; looking at the unfolding would need to call `eval`
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
  bind _hint rhss body env = do
    as <- take (length rhss) . freeList <$> ByNeed get
    let env' = extendVarEnvList env (zip (map fstOf3 rhss) (map fetch as))
    ByNeed $ modify (\μ -> foldr (\(a,(_x,_e,eval)) -> WM.insert a (memo a (eval env'))) μ (zip as rhss))
    body env'

evalByNeed :: CoreExpr -> T (Value (ByNeed T), Heap (ByNeed T))
evalByNeed e = runStateT (runByNeed (eval e emptyVarEnv)) WM.empty

-- Boilerplate
instance Outputable Event where
  ppr (Lookup n) = text "Lookup" <> parens (ppr n)
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
