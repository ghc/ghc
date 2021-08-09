{-# LANGUAGE BangPatterns #-}

module GHC.Core.FreshenUniques ( freshenUniques ) where

import GHC.Prelude

import GHC.Core
import GHC.Core.Subst

import GHC.Types.Id
import GHC.Types.Var.Set
import GHC.Types.Var.Env

import GHC.Utils.Outputable
import GHC.Utils.Panic

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Strict

import qualified Data.List as List
import Data.Traversable (for)

type M a = ReaderT Subst (State InScopeSet) a

-- | Gives fresh uniques to all 'Var's ocurring in terms of the 'CoreProgram'.
-- It works by bringing all 'Var's into scope at once through calls to
-- 'substBndr'.
freshenUniques :: CoreProgram -> CoreProgram
freshenUniques prog = evalState (runReaderT (freshenTopBinds prog) emptySubst) emptyInScopeSet

freshenTopBinds :: [CoreBind] -> M [CoreBind]
freshenTopBinds binds = do
  -- The scoping semantics of top-level bindings are quite surprising;
  -- All bindings are brought into scope at the beginning. Hence they
  -- mustn't shadow each other.
  -- See also https://gitlab.haskell.org/ghc/ghc/-/issues/19529
  let bs = bindersOfBinds binds
  -- ... hence we bring them all into scope here, without substituting anything.
  let in_scope = mkInScopeSet $ mkVarSet bs
  lift $ put $! in_scope
  -- And we can be sure that no shadowing has happened so far, hence the assert:
  massertPpr (sizeVarSet (getInScopeVars in_scope) == length bs)
             (hang (text "Non-unique top-level Id(s)!") 2 $
               ppr (filter (\grp -> length grp > 1) (List.group bs)))
  local (`setInScope` in_scope) $
    traverse freshenTopBind binds

freshenTopBind :: CoreBind -> M CoreBind
-- Binders are already fresh; see freshenTopBinds above
freshenTopBind (NonRec b rhs) = NonRec b <$!> freshenExpr rhs
freshenTopBind (Rec binds) = fmap Rec $ for binds $ \(b, rhs) -> do
  !rhs' <- freshenExpr rhs
  pure (b, rhs')

-- | `wrapSubstFunM f ids k` wraps a `substBndrs`-like function `f` such that
--
--   1. The `InScopeSet` in the state of `M` is taken for the substitution of
--      the binders `ids`.
--   2. The extended `Subst` is available in the continuation `k`
--   3. (But after this function exits, the `Subst` is reset, reader-like, with
--      no trace of `ids`)
--   4. After this function exits, the `InScopeSet` is still extended with `ids`.
wrapSubstFunM :: (Subst -> ids -> (Subst, ids)) -> ids -> (ids -> M r) -> M r
wrapSubstFunM f ids k = ReaderT $ \subst -> do
  in_scope <- get
  let (!subst', !ids') = f (subst `setInScope` in_scope) ids
  put $! substInScope subst'
  runReaderT (k ids') subst'

withSubstBndrM :: Var -> (Var -> M r) -> M r
withSubstBndrM = wrapSubstFunM substBndr

withSubstBndrsM :: [Var] -> ([Var] -> M r) -> M r
withSubstBndrsM = wrapSubstFunM substBndrs

withSubstRecBndrsM :: [Id] -> ([Id] -> M r) -> M r
withSubstRecBndrsM = wrapSubstFunM substRecBndrs

-- | The binders of the `CoreBind` are \"in scope\" in the
-- continuation.
freshenLocalBind :: CoreBind -> (CoreBind -> M r) -> M r
freshenLocalBind (NonRec b rhs) k = do
  !rhs' <- freshenExpr rhs
  withSubstBndrM b $ \(!b') -> k $! NonRec b' rhs'
freshenLocalBind (Rec binds) k = do
  let (bs, rhss) = unzip binds
  withSubstRecBndrsM bs $ \(!bs') -> do
    !rhss' <- traverse freshenExpr rhss
    k $! Rec $! zip bs' rhss'

freshenExpr :: CoreExpr -> M CoreExpr
-- Quite like substExpr, but we freshen binders unconditionally.
-- So maybe this is more like substExpr, if we had that
freshenExpr (Coercion co) = Coercion <$!> (substCo <$> ask <*> pure co)
freshenExpr (Type t) = Type <$!> (substTy <$> ask <*> pure t)
freshenExpr e@Lit{} = pure e
freshenExpr (Var v) = lookupIdSubst <$> ask <*> pure v
freshenExpr (Tick t e) = do
  t <- substTickish <$> ask <*> pure t
  Tick t <$!> freshenExpr e
freshenExpr (Cast e co) = do
  co' <- substCo <$> ask <*> pure co
  flip Cast co' <$!> freshenExpr e
freshenExpr (App f a) = do
  !f' <- freshenExpr f
  !a' <- freshenExpr a
  pure $ App f' a'
freshenExpr (Lam b e) = withSubstBndrM b $ \(!b') -> do
  !e' <- freshenExpr e
  pure $ Lam b' e'
freshenExpr (Let b e) = do
  freshenLocalBind b $ \(!b') -> do
    !e' <- freshenExpr e
    pure $ Let b' e'
freshenExpr (Case e b ty alts) = do
  !e' <- freshenExpr e
  withSubstBndrM b $ \(!b') -> do
    !ty' <- substTy <$> ask <*> pure ty
    let do_alt (Alt con bs e) = withSubstBndrsM bs $ \(!bs') ->
          Alt con bs' <$!> freshenExpr e
    !alts' <- traverse do_alt alts
    pure $ Case e' b' ty' alts'
