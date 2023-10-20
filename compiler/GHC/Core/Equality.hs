{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DerivingVia #-}

-- TODO: Rename to GHC.HsToCore.CoreEquality or something
module GHC.Core.Equality where

import GHC.Prelude

-- import GHC.Types.Name (Name)
import GHC.Core
import GHC.Core.DataCon
import GHC.Core.TyCo.Rep
import GHC.Core.Map.Type
import GHC.Types.Var
import GHC.Types.Literal

import Data.Equality.Graph as EG
import Data.Equality.Analysis.Monadic
import qualified Data.Equality.Graph.Monad as EGM
import GHC.Utils.Outputable
import GHC.Core.Coercion (coercionType)

import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader

-- Important to note the binders are also represented by $a$
-- This is because in the e-graph we will represent binders with the
-- equivalence class id of things equivalent to it.
--
-- Unfortunately type binders are still not correctly accounted for.
-- Perhaps it'd really be better to make DeBruijn work over these types

-- In the pattern match checker, expressions will always be kind of shallow.
-- In practice, no-one writes gigantic lambda expressions in guards and view patterns

data AltF a
    = AltF AltCon' [()] a -- [()] tells us the number of constructors..., bad representation TODO
    deriving (Functor, Foldable, Traversable, Eq, Ord)

data BindF a
  = NonRecF a
  | RecF [a]
  deriving (Functor, Foldable, Traversable, Eq, Ord, Show)

type BoundVar = Int
-- If we use this datatype specifically for representing HsToCore.Pmc, we may
-- be able to drop the coercion field, and add a specific one for constructor
-- application
data ExprF r
  = VarF BoundVar
  -- ROMES:TODO: what about using Names for comparison? Since this is only for equality purposes...
  -- It makes it possible to use the ConLikeName as the FreeVar Name, since there is conLikeName for PmAltConLike
  | FreeVarF Id
  | LitF Literal
  | AppF r r
  | LamF r
  | LetF (BindF r) r
  | CaseF r [AltF r] -- can we drop the case type for expr equality? we don't need them back, we just need to check equality. (perhaps this specialization makes this more suitable in the Pmc namespace)

  -- | CastF a (DeBruijn CoercionR) -- can we drop this
  -- | TickF CoreTickish a          -- this, when representing expressions for equality?
  -- but it'll be pretty hard to trigger any bug related to equality matching wrt coercions and casts on view and guard patterns solely
  | TypeF DBType
  | CoercionF DBCoercion
  deriving (Functor, Foldable, Traversable, Eq, Ord)

newtype DBType = DBT (DeBruijn Type) deriving Eq
instance Ord DBType where
  compare (DBT dt) (DBT dt') = cmpDeBruijnType dt dt'
newtype DBCoercion = DBC (DeBruijn Coercion) deriving Eq
instance Ord DBCoercion where
  compare (DBC dt) (DBC dt') = cmpDeBruijnCoercion dt dt'

newtype AltCon' = AC' AltCon deriving Eq
                             deriving Outputable via AltCon

instance Ord AltCon' where
  compare (AC' (DataAlt a)) (AC' (DataAlt b))
    = case compare (dataConName a) (dataConName b) of
        LT -> LT
        EQ -> compare (DataAlt a) (DataAlt b) -- AltCon's Ord instance only works for same datatypes
        GT -> GT
  compare (AC' a) (AC' b) = compare a b

-- this makes perfect sense, if we already have to represent this in the e-graph
-- we might as well make it a better suited representation for the e-graph...
-- keeping the on-fly debruijn is harder
representCoreExprEgr :: forall a m
                   . AnalysisM m a CoreExprF
                  => CoreExpr
                  -> EGraph a CoreExprF
                  -> m (ClassId, EGraph a CoreExprF)
representCoreExprEgr expr egr = EGM.runEGraphMT egr (runReaderT (go expr) emptyCME) where
  go :: CoreExpr -> ReaderT CmEnv (EGM.EGraphMT a CoreExprF m) ClassId
  go = \case
    Var v -> do
      env <- ask
      case lookupCME env v of
        -- Nothing -> addE (FreeVarF $ varName v)
        Nothing -> addE (FreeVarF v)
        Just i  -> addE (VarF i)
    Lit lit -> addE (LitF lit)
    Type t  -> addE (TypeF (DBT $ deBruijnize t))
    Coercion c -> addE (CoercionF (DBC $ deBruijnize c))
    Tick _ e -> go e -- bypass ticks!
    Cast e _ -> go e -- bypass casts! ouch? TODO
    App f a -> do
      f' <- go f
      a' <- go a
      addE (AppF f' a')
    Lam b e -> do
      e' <- local (`extendCME` b) $ go e
      addE (LamF e')
    Let (NonRec v r) e -> do
      r' <- go r
      e' <- local (`extendCME` v) $ go e
      addE (LetF (NonRecF r') e')
    Let (Rec (unzip -> (bs,rs))) e -> do
      rs' <- traverse (local (`extendCMEs` bs) . go) rs
      e'  <- local (`extendCMEs` bs) $ go e
      addE (LetF (RecF rs') e')
    Case e b _t as -> do
      e' <- go e
      as' <- traverse (local (`extendCME` b) . goAlt) as
      addE (CaseF e' as')

  goAlt :: CoreAlt -> ReaderT CmEnv (EGM.EGraphMT a CoreExprF m) (CoreAltF ClassId)
  goAlt (Alt c bs e) = do
    e' <- local (`extendCMEs` bs) $ go e
    return (AltF (AC' c) (map (const ()) bs) e')

  addE :: AnalysisM m a CoreExprF => CoreExprF ClassId -> ReaderT CmEnv (EGM.EGraphMT a CoreExprF m) ClassId
  addE e = lift $ EGM.addM $ Node e
{-# INLINEABLE representCoreExprEgr #-}

type CoreExprF = ExprF
type CoreAltF = AltF
type CoreBindF = BindF

instance Outputable (EG.ENode CoreExprF) where
  ppr (EG.Node n) = text (show n)

-- cmpDeBruijnTickish :: DeBruijn CoreTickish -> DeBruijn CoreTickish -> Ordering
-- cmpDeBruijnTickish (D env1 t1) (D env2 t2) = go t1 t2 where
--     go (Breakpoint lext lid lids _) (Breakpoint rext rid rids _)
--         = case compare lid rid of
--             LT -> LT
--             EQ -> case compare (D env1 lids) (D env2 rids) of
--                     LT -> LT
--                     EQ -> compare lext rext
--                     GT -> GT
--             GT -> GT
--     go l r = compare l r

cmpDeBruijnType :: DeBruijn Type -> DeBruijn Type -> Ordering
cmpDeBruijnType d1@(D _ t1) d2@(D _ t2)
  = if eqDeBruijnType d1 d2
       then EQ
       -- ROMES:TODO: This definitely does not look OK.
       -- ROMES:TODO: This hurts performance a lot (50% of regression is basically this)
       else compare (showPprUnsafe t1) (showPprUnsafe t2)

cmpDeBruijnCoercion :: DeBruijn Coercion -> DeBruijn Coercion -> Ordering
cmpDeBruijnCoercion (D env1 co1) (D env2 co2)
  = cmpDeBruijnType (D env1 (coercionType co1)) (D env2 (coercionType co2))

-- -- instances for debugging purposes
instance Show a => Show (CoreExprF a) where
  show (VarF id) = showPprUnsafe $ text "VarF"  <+> ppr id
  show (FreeVarF id) = showPprUnsafe $ ppr id
  show (LitF lit) = showPprUnsafe $ text "LitF" <+> ppr lit
  show (AppF a b) = "AppF " ++ show a ++ " " ++ show b
  show (LamF a) = "LamF " ++ show a
  show (LetF b a) = "LetF " ++ show b ++ " " ++ show a
  show (CaseF a alts) = "CaseF " ++ show a ++ show alts

  -- show (CastF _a _cor) = "CastF"
  -- show (TickF _cotick _a) = "Tick"
  show (TypeF (DBT (D _ t))) = "TypeF " ++ showPprUnsafe (ppr t)
  show (CoercionF (DBC (D _ co))) = "CoercionF " ++ showPprUnsafe co

instance Show a => Show (AltF a) where
  show (AltF alt bs a) = "AltF " ++ showPprUnsafe (ppr alt <+> ppr bs) ++ show a

