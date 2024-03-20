{-# LANGUAGE DerivingStrategies #-}
module GHC.Tc.Gen.HsType.Monad
  ( -- * Monad
    TcTypeM,
    -- * Runners
    runTcTypeMWithPatBinds, runTcTypeM,
    -- * Lifting TcM
    liftTcM, liftHighOrderTcMWithMetaAndInfo,
    liftHighOrderTcMWithMeta, liftHighOrderTcM,
    -- * Interact with monad context
    isTypePatBinder, addTypeBinder,
    -- * Utils
    traceTcType
  ) where

import GHC.Prelude

import GHC.Types.Name.Env
import GHC.Types.Name.Set
import GHC.Types.Name (Name)
import GHC.Types.Var ( TcTyVar, Var(varName) )
import GHC.Tc.Types ( TcM )
import GHC.Tc.Utils.Env (tcExtendNameTyVarEnv)
import GHC.Tc.Utils.Monad ( traceTc )
import GHC.Utils.Outputable ( SDoc )

import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Control.Monad.IO.Class
import Data.Coerce (coerce)
import Data.Tuple ( swap )
import Data.Bitraversable ( Bitraversable(..) )

-- | Typecheck type monad
-- See Note [TcTypeM and Type Patterns]
-- See Note [CPS vs. StateT in TcTypeM]
newtype TcTypeM a = MkTcTypeM
  ( ReaderT NameSet (StateT (NameEnv TcTyVar) TcM) a
  ) deriving newtype (Functor, Applicative, Monad, MonadIO)

empty_state :: NameEnv TcTyVar
empty_state = mempty

-- | Run TcTypeM action with set of names that should be binded by
-- a type pattern, return paired list of names with their types
runTcTypeMWithPatBinds :: NameSet -> TcTypeM a -> TcM (a, [(Name, TcTyVar)])
runTcTypeMWithPatBinds ns action =
  fmap env_to_paired_tyvars <$>
    coerce action ns empty_state

-- | Run TcTypeM with empty environment, so it should behave as
-- simple TcM
runTcTypeM :: TcTypeM a -> TcM a
runTcTypeM action = coerce action emptyNameSet `evalStateT` empty_state

env_to_paired_tyvars :: NameEnv TcTyVar -> [(Name, TcTyVar)]
env_to_paired_tyvars = map (\tv -> (varName tv, tv)) . nonDetNameEnvElts -- is that safe?

tc_extend_ty_env :: NameEnv TcTyVar -> TcM r -> TcM r
tc_extend_ty_env ty_env thing_inside
  | isEmptyNameEnv ty_env = thing_inside -- Fast, most common path
  | otherwise = tcExtendNameTyVarEnv (env_to_paired_tyvars ty_env) thing_inside

liftTcM :: TcM a -> TcTypeM a
liftTcM action = do
  ty_env <- MkTcTypeM $ lift get
  MkTcTypeM $ lift $ lift $ (tc_extend_ty_env ty_env) $ action

-- (a,b,) is not a Traversable, so we need to use Bitraversable here
--
-- See https://github.com/haskell/core-libraries-committee/issues/206
liftHighOrderTcMWithMetaAndInfo :: forall f a meta info.
                                  Bitraversable f =>
                                   (forall a. (info -> TcM a) -> TcM (f meta a))
                               -> (info -> TcTypeM a)
                               -> TcTypeM (f meta a)
liftHighOrderTcMWithMetaAndInfo ho_tc_m thing_inside = coerce $ \(ns :: NameSet) ty_env -> do
    (res, inner_state) <- tc_extend_ty_env ty_env $
      fmap reassoc $ ho_tc_m $ \info ->
      coerce thing_inside info ns empty_state -- we added current state in scope,
                                              -- so there is no need to pass it inside
    pure (res, ty_env `plusNameEnv` inner_state)
  where
    reassoc :: (f meta (a, NameEnv TcTyVar)) -> (f meta a, (NameEnv TcTyVar))
    reassoc = swap . bitraverse pure id . fmap swap

liftHighOrderTcMWithMeta :: forall f a meta.
                                  Bitraversable f =>
                         (forall a. TcM a -> TcM (f meta a))
                         -> TcTypeM a
                         -> TcTypeM (f meta a)
liftHighOrderTcMWithMeta ho_tc_m thing_inside =
  liftHighOrderTcMWithMetaAndInfo (\f -> ho_tc_m (f ())) (\_ -> thing_inside)

liftHighOrderTcM :: (forall a. TcM a -> TcM a) -> TcTypeM a -> TcTypeM a
liftHighOrderTcM ho_tc_m = removeUnit . liftHighOrderTcMWithMeta (attachUnit . ho_tc_m)
  where
    attachUnit = fmap ((),)
    removeUnit = fmap snd

isTypePatBinder :: Name -> TcTypeM Bool
isTypePatBinder name = MkTcTypeM $ asks (elemNameSet name)

addTypeBinder :: TcTyVar -> TcTypeM ()
addTypeBinder ty_var =
  MkTcTypeM $ lift $ modify (\env -> extendNameEnv env (varName ty_var) ty_var)

traceTcType :: String -> SDoc -> TcTypeM ()
traceTcType herald doc = MkTcTypeM $ lift $ lift $ traceTc herald doc


{- Note [TcTypeM and Type Patterns]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider this example:

  type S :: (forall k. k -> Type) -> Type
  type S f = ...

  f (Proxy @(S f)) = Proxy @(S f)

It may work only if we assign `f` polymorphic kind
`forall k. k -> Type`. We know that type pattern adds `f`
to the scope because this information was collected during
renaming (see HsTyPatRn in GHC.Hs.Type) and we know kind
of `f` deeply recursive inside the type that we currently
kind check.

`TcTypeM` monad connects these pieces together. It consists of
  - ReaderT NameSet - a set of names, that type pattern
    brings into scope
  - StateT (NameEnv TcTyVar) - mapping from a Name to a TcTyVar,
    that were binded by the type pattern

The main work goes in `tcTyVar` function. Here is it, step-by-step:
  - Check whether a name is already in the scope using `tcLookup_maybe`
  - If it doesn't, examine it as a type pattern binder using `isTypePatBinder`
  - If it's a type pattern binder, make a TcTyVar with `newPatTyVar`
  - Add new type binder using `addTypeBinder`

`TcTypeM` monad also ensures that binded type pattern would
be in the scope for the rest of a type. This work happens in
`liftTcM` and all other lift-functions. Notice: it's essential
to have the same unique for `TcTyVar` as it is in the original
`Name`, so `liftTcM` will prevent double-binding of a type pattern.

Note [CPS vs. StateT in TcTypeM]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
`TcTypeM` monad looks very similar to the `TpRnM`, but it uses
`StateT` internally instead of `Cps` transformation for `TcM` and
writer for binders.

The reason is that `Cps` transformation makes state from the whole
reader context, not only from type pattern binders. That means that
it is not possible to write universal `liftHighOrderTcM` for Cps
transformed `TcM`: we should carefully unset environment changes
after each high-order function. That means that we should write `TcTypeM`
variant of each high-order function.

Moreover, typechecker uses exceptions internally to handle exeptions
in the `pushLevelAndCaptureConstraints` function. That means that
we have to write a variant of `try` function for `(a -> IO r) -> IO r`
that catches expection for action inside, but not for callback.

With `StateT` we have to play carefully with environment and
add new binds into it whenever it is possible to lookup them.
However, it is only two possible entry points: `liftTcM` and
`liftHighOrderTcMWithMetaAndInfo`.
-}
