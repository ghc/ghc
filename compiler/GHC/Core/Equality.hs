{-# LANGUAGE MagicHash #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}

module GHC.Core.Equality where

import GHC.Exts (dataToTag#, tagToEnum#, (>#), (<#))
import GHC.Prelude

import GHC.Core
import GHC.Core.TyCo.Rep
import GHC.Core.Map.Type
import GHC.Core.Map.Expr
import GHC.Types.Var
import GHC.Types.Literal
import GHC.Types.Tickish
import Unsafe.Coerce (unsafeCoerce)

import Control.Monad.Trans.State.Strict (state)
import Data.Equality.Graph as EG
import Data.Equality.Analysis
import qualified Data.Equality.Graph.Monad as EGM
import Data.Equality.Utils (Fix(..))

import GHC.Utils.Misc (all2)
import GHC.Utils.Outputable
import GHC.Core.Coercion (coercionType)

-- Important to note the binders are also represented by $a$
-- This is because in the e-graph we will represent binders with the
-- equivalence class id of things equivalent to it.
--
-- Unfortunately type binders are still not correctly accounted for.
-- Perhaps it'd really be better to make DeBruijn work over these types

data AltF b a
    = AltF AltCon [b] a
    deriving (Functor, Foldable, Traversable)

data BindF b a
  = NonRecF b a
  | RecF [(b, a)]
  deriving (Functor, Foldable, Traversable)

data ExprF b a
  = VarF Id
  | LitF Literal
  | AppF a a
  | LamF b a
  | LetF (BindF b a) a
  | CaseF a b Type [AltF b a]

  | CastF a CoercionR
  | TickF CoreTickish a
  | TypeF Type
  | CoercionF Coercion
  deriving (Functor, Foldable, Traversable)

type CoreExprF
  = ExprF CoreBndr
type CoreAltF
  = AltF CoreBndr
type CoreBindF
  = BindF CoreBndr

newtype DeBruijnF f a = DF (DeBruijn (f a))
  deriving (Functor, Foldable, Traversable)

eqDeBruijnExprF :: forall a. Eq a => DeBruijn (CoreExprF a) -> DeBruijn (CoreExprF a) -> Bool
eqDeBruijnExprF (D env1 e1) (D env2 e2) = go e1 e2 where
    go :: CoreExprF a -> CoreExprF a -> Bool
    go (VarF v1) (VarF v2)             = eqDeBruijnVar (D env1 v1) (D env2 v2)
    go (LitF lit1)    (LitF lit2)      = lit1 == lit2
    go (TypeF t1)    (TypeF t2)        = eqDeBruijnType (D env1 t1) (D env2 t2)
    -- See Note [Alpha-equality for Coercion arguments]
    go (CoercionF {}) (CoercionF {}) = True
    go (CastF e1 co1) (CastF e2 co2) = D env1 co1 == D env2 co2 && e1 == e2
    go (AppF f1 a1)   (AppF f2 a2)   = f1 == f2 && a1 == a2
    go (TickF n1 e1) (TickF n2 e2)
      =  eqDeBruijnTickish (D env1 n1) (D env2 n2)
      && e1 == e2

    go (LamF b1 e1)  (LamF b2 e2)
      =  eqDeBruijnType (D env1 (varType b1)) (D env2 (varType b2))
      && D env1 (varMultMaybe b1) == D env2 (varMultMaybe b2)
      && e1 == e2

    go (LetF abs e1) (LetF bbs e2)
      = D env1 abs == D env2 bbs
      && e1 == e2

    go (CaseF e1 _b1 t1 a1) (CaseF e2 _b2 t2 a2)
      | null a1   -- See Note [Empty case alternatives]
      = null a2 && e1 == e2 && D env1 t1 == D env2 t2
      | otherwise
      = e1 == e2 && D env1 a1 == D env2 a2

    go _ _ = False

-- ROMES:TODO: This one can be derived automatically, but perhaps it's better
-- to be explicit here? We don't even really require the DeBruijn context here
eqDeBruijnAltF :: forall a. Eq a => DeBruijn (CoreAltF a) -> DeBruijn (CoreAltF a) -> Bool
eqDeBruijnAltF (D _env1 a1) (D _env2 a2) = go a1 a2 where
    go (AltF DEFAULT _ rhs1) (AltF DEFAULT _ rhs2)
        = rhs1 == rhs2
    go (AltF (LitAlt lit1) _ rhs1) (AltF (LitAlt lit2) _ rhs2)
        = lit1 == lit2 && rhs1 == rhs2
    go (AltF (DataAlt dc1) _bs1 rhs1) (AltF (DataAlt dc2) _bs2 rhs2)
        = dc1 == dc2 &&
          rhs1 == rhs2 -- the CM environments were extended on representation (see 'representDBAltExpr')
    go _ _ = False

-- | 'unsafeCoerce' mostly because I'm too lazy to write the boilerplate.
fromCoreExpr :: CoreExpr -> Fix CoreExprF
fromCoreExpr = unsafeCoerce

toCoreExpr :: CoreExpr -> Fix CoreExprF
toCoreExpr = unsafeCoerce

-- | Represents a DeBruijn CoreExpr being careful to correctly debruijnizie the expression as it is represented
--
-- Always represent Ids, at least for now. We're seemingly using inexistent ids
-- ROMES:TODO: do this all inside EGraphM instead
representDBCoreExpr :: Analysis a (DeBruijnF CoreExprF)
                    => DeBruijn CoreExpr
                    -> EGraph a (DeBruijnF CoreExprF)
                    -> (ClassId, EGraph a (DeBruijnF CoreExprF))
representDBCoreExpr (D cmenv expr) eg0 = case expr of
  Var v   -> add (Node $ DF (D cmenv (VarF v)))   eg0
  Lit lit -> add (Node $ DF (D cmenv (LitF lit))) eg0
  Type t  -> add (Node $ DF (D cmenv (TypeF t)))  eg0
  Coercion c -> add (Node $ DF (D cmenv (CoercionF c))) eg0
  Cast e co  -> let (eid,eg1) = representDBCoreExpr (D cmenv e) eg0
                 in add (Node $ DF (D cmenv (CastF eid co))) eg1
  App f a -> let (fid,eg1) = representDBCoreExpr (D cmenv f) eg0
                 (aid,eg2) = representDBCoreExpr (D cmenv a) eg1
              in add (Node $ DF (D cmenv (AppF fid aid))) eg2
  Tick n e -> let (eid,eg1) = representDBCoreExpr (D cmenv e) eg0
               in add (Node $ DF (D cmenv (TickF n eid))) eg1
  Lam b e  -> let (eid,eg1) = representDBCoreExpr (D (extendCME cmenv b) e) eg0
               in add (Node $ DF (D cmenv (LamF b eid))) eg1
  Let (NonRec v r) e -> let (rid,eg1) = representDBCoreExpr (D cmenv r) eg0
                            (eid,eg2) = representDBCoreExpr (D (extendCME cmenv v) e) eg1
                         in add (Node $ DF (D cmenv (LetF (NonRecF v rid) eid))) eg2
  Let (Rec (unzip -> (bs,rs))) e ->
    let cmenv' = extendCMEs cmenv bs
        (bsids, eg1) = EGM.runEGraphM eg0 $
                         traverse (state . representDBCoreExpr . D cmenv') rs
        (eid, eg2)  = representDBCoreExpr (D cmenv' e) eg1
     in add (Node $ DF (D cmenv (LetF (RecF (zip bs bsids)) eid))) eg2
  Case e b t as -> let (eid, eg1)  = representDBCoreExpr (D cmenv e) eg0
                       (as', eg2) = EGM.runEGraphM eg1 $
                         traverse (state . representDBAltExpr . D (extendCME cmenv b)) as
                    in add (Node $ DF (D cmenv (CaseF eid b t as'))) eg2

representDBAltExpr :: Analysis a (DeBruijnF CoreExprF)
                   => DeBruijn CoreAlt
                   -> EGraph a (DeBruijnF CoreExprF)
                   -> (CoreAltF ClassId, EGraph a (DeBruijnF CoreExprF))
representDBAltExpr (D cm (Alt cons bs a)) eg0 =
  let (ai, eg1) = representDBCoreExpr (D (extendCMEs cm bs) a) eg0
   in (AltF cons bs ai, eg1)

instance Eq a => Eq (DeBruijn (CoreAltF a)) where
  (==) = eqDeBruijnAltF

instance Eq a => Eq (DeBruijn (CoreExprF a)) where
  (==) = eqDeBruijnExprF

instance Eq a => Eq (DeBruijnF CoreExprF a) where
  (==) (DF a) (DF b) = eqDeBruijnExprF a b

instance Eq a => Eq (DeBruijnF CoreAltF a) where
  (==) (DF a) (DF b) = eqDeBruijnAltF a b

deriving instance Ord a => Ord (DeBruijnF CoreExprF a)

instance Ord a => Ord (DeBruijn (CoreExprF a)) where
  -- We must assume that if `a` is DeBruijn expression, it is already correctly "extended" because 'representDBCoreExpr' ensures that.
  -- RM:TODO: We don't yet compare the CmEnv at any point. Should we?
  -- RM: I don't think so, the CmEnv is used to determine whether bound variables are equal, but they don't otherwise influence the result.
  -- Or rather, if the subexpression with variables is equal, then the CmEnv is necessarily equal too?
  -- So I think that just works...
  -- Wait, in that sense, couldn't we find a way to derive ord? the important part being that to compare Types and Vars we must use the DeBruijn Env ...
  compare a b
    = case a of
        D cma (VarF va)
          -> case b of
               D cmb (VarF vb) -> cmpDeBruijnVar (D cma va) (D cmb vb)
               _ -> LT
        D _ (LitF la)
          -> case b of
               D _ VarF{} -> GT
               D _ (LitF lb) -> la `compare` lb
               _ -> LT
        D _ (AppF af aarg)
          -> case dataToTag# b of
               bt
                 -> if tagToEnum# (bt ># 2#) then
                        LT
                    else
                        case b of
                          D _ (AppF bf barg)
                            -> case compare af bf of
                                 LT -> LT
                                 EQ -> aarg `compare` barg -- e.g. here, if we had for children other expresssions debruijnized, they would have the *correct* environments, so we needn't worry.
                                                           -- the issue to automatically deriving is only really the 'Var' and 'Type' parameters ...
                                 GT -> GT
                          _ -> GT
        D _ (LamF _abind abody)
          -> case dataToTag# b of
               bt
                 -> if tagToEnum# (bt ># 3#) then
                        LT
                    else
                        case b of
                          D _ (LamF _bbind bbody) -- we can ignore the binder since the represented DB expression has the correct DB environments by construction (see 'representDBCoreExpr')
                            -> compare abody bbody
                          _ -> GT
        D cma (LetF as abody)
          -> case dataToTag# b of
               bt
                 -> if tagToEnum# (bt ># 4#) then
                        LT
                    else
                        case b of
                          D cmb (LetF bs bbody)
                            -> case compare (D cma as) (D cmb bs) of
                                 LT -> LT
                                 EQ -> compare abody bbody
                                 GT -> GT
                          _ -> GT
        D cma (CaseF cax _cabind catype caalt)
          -> case dataToTag# b of
               bt
                 -> if tagToEnum# (bt <# 5#) then
                        GT
                    else
                        case b of
                          D cmb (CaseF cbx _cbbind cbtype cbalt)
                            -> case compare cax cbx of
                                 LT -> LT
                                 -- ROMES:TODO: Consider changing order of comparisons to a more efficient one
                                 EQ -> case cmpDeBruijnType (D cma catype) (D cmb cbtype) of
                                        LT -> LT
                                        EQ -> D cma caalt `compare` D cmb cbalt
                                        GT -> GT
                                 GT -> GT
                          _ -> LT
        D cma (CastF cax caco)
          -> case dataToTag# b of
               bt
                 -> if tagToEnum# (bt <# 6#) then
                        GT
                    else
                        case b of
                          D cmb (CastF cbx cbco)
                            -> case compare cax cbx of
                                 LT -> LT
                                 EQ -> cmpDeBruijnCoercion (D cma caco) (D cmb cbco)
                                 GT -> GT
                          _ -> LT
        D cma (TickF tatickish tax)
          -> case dataToTag# b of
               bt
                 -> if tagToEnum# (bt <# 7#) then
                        GT
                    else
                        case b of
                          D cmb (TickF tbtickish tbx)
                            -> case cmpDeBruijnTickish (D cma tatickish) (D cmb tbtickish) of
                                 LT -> LT
                                 EQ -> tax `compare` tbx
                                 GT -> GT
                          _ -> LT
        D cma (TypeF at)
          -> case b of
               D _    CoercionF{} -> LT
               D cmb (TypeF bt) -> cmpDeBruijnType (D cma at) (D cmb bt)
               _ -> GT
        D cma (CoercionF aco)
          -> case b of
               D cmb (CoercionF bco) -> cmpDeBruijnCoercion (D cma aco) (D cmb bco)
               _ -> GT

instance Eq a => Eq (DeBruijn (CoreBindF a)) where
  D cma a == D cmb b = go a b where
    go (NonRecF _v1 r1) (NonRecF _v2 r2)
      = r1 == r2 -- See Note [Alpha-equality for let-bindings]

    go (RecF ps1) (RecF ps2)
      =
      -- See Note [Alpha-equality for let-bindings]
      all2 (\b1 b2 -> eqDeBruijnType (D cma (varType b1))
                                     (D cmb (varType b2)))
           bs1 bs2
      && rs1 == rs2
      where
        (bs1,rs1) = unzip ps1
        (bs2,rs2) = unzip ps2

    go _ _ = False


instance Ord a => Ord (DeBruijn (CoreBindF a)) where
  compare a b
    = case a of
        D _cma (NonRecF _ab ax)
          -> case b of
               D _cmb (NonRecF _bb bx) -- Again, we ignore the binders bc on representation they were accounted for correctly.
                 -> ax `compare` bx
               _ -> LT
        D _cma (RecF as)
          -> case b of
               D _cmb (RecF bs) -> compare (map snd as) (map snd bs)
               _ -> GT


instance Ord a => Ord (DeBruijn (CoreAltF a)) where
  compare a b
    = case a of
        D _cma (AltF ac _abs arhs)
          -> case b of
               D _cmb (AltF bc _bbs brhs)
                 -> case compare ac bc of
                      LT -> LT
                      EQ -> -- Again, we don't look at binders bc we assume on representation they were accounted for correctly.
                        arhs `compare` brhs
                      GT -> GT

cmpDeBruijnTickish :: DeBruijn CoreTickish -> DeBruijn CoreTickish -> Ordering
cmpDeBruijnTickish (D env1 t1) (D env2 t2) = go t1 t2 where
    go (Breakpoint lext lid lids) (Breakpoint rext rid rids)
        = case compare lid rid of
            LT -> LT
            EQ -> case compare (D env1 lids) (D env2 rids) of
                    LT -> LT
                    EQ -> compare lext rext
                    GT -> GT
            GT -> GT
    go l r = compare l r

cmpDeBruijnType :: DeBruijn Type -> DeBruijn Type -> Ordering
cmpDeBruijnType d1@(D _ t1) d2@(D _ t2)
  = if eqDeBruijnType d1 d2
       then EQ
       -- ROMES:TODO: Is this OK?
       else compare (showPprUnsafe t1) (showPprUnsafe t2)

cmpDeBruijnCoercion :: DeBruijn Coercion -> DeBruijn Coercion -> Ordering
cmpDeBruijnCoercion (D env1 co1) (D env2 co2)
  = cmpDeBruijnType (D env1 (coercionType co1)) (D env2 (coercionType co2))

-- instances for debugging purposes
instance Show a => Show (DeBruijnF CoreExprF a) where
  show (DF (D _ (VarF id) )) = showPprUnsafe $ text "VarF"  <+> ppr id
  show (DF (D _ (LitF lit))) = showPprUnsafe $ text "LitF" <+> ppr lit
  show (DF (D _ (AppF a b))) = "AppF " ++ show a ++ " " ++ show b
  show (DF (D _ (LamF b a))) = showPprUnsafe (text "LamF" <+> ppr b <+> text "") ++ show a
  show (DF (D _ (LetF b a))) = "LetF " ++ show b ++ " " ++ show a
  show (DF (D _ (CaseF a b t alts))) = "CaseF " ++ show a ++ showPprUnsafe (ppr b <+> ppr t) ++ show alts

  show (DF (D _ (CastF _a _cor)   )) = "CastF"
  show (DF (D _ (TickF _cotick _a))) = "Tick"
  show (DF (D _ (TypeF t)       )) = "TypeF " ++ showPprUnsafe (ppr t)
  show (DF (D _ (CoercionF co)  )) = "CoercionF " ++ showPprUnsafe co


instance Show a => Show (BindF CoreBndr a) where
  show (NonRecF b a) = "NonRecF " ++ showPprUnsafe b ++ show a
  show (RecF bs) = "RecF " ++ unwords (map (\(a,b) -> showPprUnsafe a ++ show b) bs)

instance Show a => Show (AltF CoreBndr a) where
  show (AltF alt bs a) = "AltF " ++ showPprUnsafe (ppr alt <+> ppr bs) ++ show a


