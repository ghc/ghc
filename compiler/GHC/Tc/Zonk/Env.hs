{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE NoPolyKinds #-}

-- | The 'ZonkEnv' zonking environment, and the 'ZonkT' and 'ZonkBndrT'
-- monad transformers, for the final zonking to type in "GHC.Tc.Zonk.Type".
--
-- See Note [Module structure for zonking] in GHC.Tc.Zonk.Type.
module GHC.Tc.Zonk.Env
  ( -- * The 'ZonkEnv'
    ZonkEnv(..), getZonkEnv
  , ZonkFlexi(..)
  , initZonkEnv

    -- * The 'ZonkT' and 'ZonkBndrT' zonking monad transformers
  , ZonkT(ZonkT,runZonkT), ZonkBndrT(..)

    -- ** Going between 'ZonkT' and 'ZonkBndrT'
  , runZonkBndrT
  , noBinders, don'tBind

    -- ** Modifying and extending the 'ZonkEnv' in 'ZonkBndrT'
  , setZonkType
  , extendZonkEnv
  , extendIdZonkEnv, extendIdZonkEnvRec
  , extendTyZonkEnv

  )
  where

import GHC.Prelude

import GHC.Core.TyCo.Rep ( Type )
import GHC.Types.Var ( TyCoVar, Var, TyVar )

import GHC.Types.Var ( Id, isTyCoVar )
import GHC.Types.Var.Env

import GHC.Utils.Monad.Codensity
import GHC.Utils.Outputable

import Control.Monad.Fix         ( MonadFix(..) )
import Control.Monad.IO.Class    ( MonadIO(..) )
import Control.Monad.Trans.Class ( MonadTrans(..) )
import Data.Coerce               ( coerce )
import Data.IORef                ( IORef, newIORef )
import Data.List                 ( partition )

import GHC.Exts                  ( oneShot )

--------------------------------------------------------------------------------

-- | See Note [The ZonkEnv]
data ZonkEnv
  = ZonkEnv { ze_flexi       :: !ZonkFlexi
            , ze_tv_env      :: TyCoVarEnv TyCoVar
            , ze_id_env      :: IdEnv      Id
            , ze_meta_tv_env :: IORef (TyVarEnv Type) }

-- | How should we handle unfilled unification variables in the zonker?
--
-- See Note [Un-unified unification variables]
data ZonkFlexi
  = DefaultFlexi       -- ^ Default unbound unification variables to Any

  | SkolemiseFlexi     -- ^ Skolemise unbound unification variables
      (IORef [TyVar])  --   See Note [Zonking the LHS of a RULE]
                       --   Records the tyvars thus skolemised

  | RuntimeUnkFlexi -- ^ Used in the GHCi debugger

  | NoFlexi         -- ^ Panic on unfilled meta-variables
                    -- See Note [Error on unconstrained meta-variables]
                    -- in GHC.Tc.Utils.TcMType

instance Outputable ZonkEnv where
  ppr (ZonkEnv { ze_tv_env = tv_env
               , ze_id_env = id_env })
    = text "ZE" <+> braces (vcat
         [ text "ze_tv_env =" <+> ppr tv_env
         , text "ze_id_env =" <+> ppr id_env ])

{- Note [The ZonkEnv]
~~~~~~~~~~~~~~~~~~~~~
* ze_flexi :: ZonkFlexi says what to do with a
  unification variable that is still un-unified.
  See Note [Un-unified unification variables]

* ze_tv_env :: TyCoVarEnv TyCoVar promotes sharing. At a binding site
  of a tyvar or covar, we zonk the kind right away and add a mapping
  to the env. This prevents re-zonking the kind at every
  occurrence. But this is *just* an optimisation.

* ze_id_env : IdEnv Id promotes sharing among Ids, by making all
  occurrences of the Id point to a single zonked copy, built at the
  binding site.

  Unlike ze_tv_env, it is knot-tied: see extendIdZonkEnvRec.
  In a mutually recursive group
     rec { f = ...g...; g = ...f... }
  we want the occurrence of g to point to the one zonked Id for g,
  and the same for f.

  Because it is knot-tied, we must be careful to consult it lazily.

* ze_meta_tv_env: see Note [Sharing when zonking to Type]


Notes:
  * We must be careful never to put coercion variables (which are Ids,
    after all) in the knot-tied ze_id_env, because coercions can
    appear in types, and we sometimes inspect a zonked type in
    the GHC.Tc.Zonk.Type module.  [Question: where, precisely?]

  * An obvious suggestion would be to have one VarEnv Var to
    replace both ze_id_env and ze_tv_env, but that doesn't work
    because of the knot-tying stuff mentioned above.

Note [Un-unified unification variables]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
What should we do if we find a Flexi unification variable?
There are three possibilities:

* DefaultFlexi: this is the common case, in situations like
     length @alpha ([] @alpha)
  It really doesn't matter what type we choose for alpha.  But
  we must choose a type!  We can't leave mutable unification
  variables floating around: after typecheck is complete, every
  type variable occurrence must have a binding site.

  So we default it to 'Any' of the right kind.

  All this works for both type and kind variables (indeed
  the two are the same thing).

* SkolemiseFlexi: is a special case for the LHS of RULES.
  See Note [Zonking the LHS of a RULE]

* RuntimeUnkFlexi: is a special case for the GHCi debugger.
  It's a way to have a variable that is not a mutable
  unification variable, but doesn't have a binding site
  either.

* NoFlexi: See Note [Error on unconstrained meta-variables]
  in GHC.Tc.Utils.TcMType. This mode will panic on unfilled
  meta-variables.
-}

-- | A reader monad over 'ZonkEnv', for zonking computations which
-- don't modify the 'ZonkEnv' (e.g. don't bind any variables).
--
-- Use 'ZonkBndrT' when you need to modify the 'ZonkEnv' (e.g. to bind
-- a variable).
newtype ZonkT m a = ZonkT' { runZonkT :: ZonkEnv -> m a }

{- Note [Instances for ZonkT]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Below, we derive the following instances by hand:

  newtype ZonkT m a = ZonkT { runZonkT :: ZonkEnv -> m a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadFix)
      via ReaderT ZonkEnv m
    deriving MonadTrans
      via ReaderT ZonkEnv

Why? Two reasons:

  1. To use oneShot. See Note [The one-shot state monad trick] in GHC.Utils.Monad.
  2. To be strict in the ZonkEnv. This allows us to worker-wrapper functions,
     passing them individual fields of the ZonkEnv instead of the whole record.
     When this happens, we avoid allocating a ZonkEnv, which is a win.
-}

-- See Note [The one-shot state monad trick] in GHC.Utils.Monad
{-# COMPLETE ZonkT #-}
pattern ZonkT :: forall m a. (ZonkEnv -> m a) -> ZonkT m a
pattern ZonkT m <- ZonkT' m
  where
    ZonkT m = ZonkT' (oneShot m)

-- See Note [Instances for ZonkT]
instance Functor m => Functor (ZonkT m) where
  fmap f (ZonkT g) = ZonkT $ \ !env -> fmap f (g env)
  a <$ ZonkT g     = ZonkT $ \ !env -> a <$ g env
  {-# INLINE fmap #-}
  {-# INLINE (<$) #-}

-- See Note [Instances for ZonkT]
instance Applicative m => Applicative (ZonkT m) where
  pure a = ZonkT (\ !_ -> pure a)
  ZonkT f <*> ZonkT x = ZonkT (\ !env -> f env <*> x env )
  ZonkT m *> f = ZonkT (\ !env -> m env *> runZonkT f env)
  {-# INLINE pure #-}
  {-# INLINE (<*>) #-}
  {-# INLINE (*>) #-}

-- See Note [Instances for ZonkT]
instance Monad m => Monad (ZonkT m) where
  ZonkT m >>= f =
    ZonkT (\ !env -> do { r <- m env
                        ; runZonkT (f r) env })
  (>>)   = (*>)
  {-# INLINE (>>=) #-}
  {-# INLINE (>>) #-}

-- See Note [Instances for ZonkT]
instance MonadIO m => MonadIO (ZonkT m) where
  liftIO f = ZonkT (\ !_ -> liftIO f)
  {-# INLINE liftIO #-}

-- See Note [Instances for ZonkT]
instance MonadTrans ZonkT where
  lift ma = ZonkT $ \ !_ -> ma
  {-# INLINE lift #-}

-- See Note [Instances for ZonkT]
instance MonadFix m => MonadFix (ZonkT m) where
  mfix f = ZonkT $ \ !r -> mfix $ oneShot $ \ a -> runZonkT (f a) r
  {-# INLINE mfix #-}

-- | Zonk binders, bringing them into scope in the inner computation.
--
-- Can be thought of as a state monad transformer @StateT ZonkEnv m a@,
-- but written in continuation-passing style.
--
-- See Note [Continuation-passing style for zonking].
newtype ZonkBndrT m a = ZonkBndrT { runZonkBndrT' :: forall r. (a -> ZonkT m r) -> ZonkT m r }
  deriving (Functor, Applicative, Monad, MonadIO, MonadFix)
    via Codensity (ZonkT m)
    -- See GHC.Utils.Monad.Codensity for the instance definitions.
    -- See Note [Continuation-passing style for zonking] for why we use
    -- continuation-passing style instead of a direct state monad.

{- Note [Continuation-passing style for zonking]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
While zonking, we sometimes need to modify the ZonkEnv. For example, when
zonking a binder with zonkTyBndrX, we extend the type variable ZonkEnv.

We could use direct state passing:

  zonkTyBndrX :: ZonkEnv -> TcTyVar -> TcM (ZonkEnv, TyVar)
  zonkTyBndrX ze tv =
    do { tv' <- ... ze tv
       ; let ze' = extendTyZonkEnv ze tv'
       ; return (ze', tv') }

but we can avoid allocating pairs by using continuation-passing style instead,
for example:

  zonkTyBndrX :: (ZonkEnv -> TcTyVar -> TcM r) -> ZonkEnv -> TcM r
  zonkTyBndrX k ze =
    do { tv' <- ... ze tv
       ; let ze' = extendTyZonkEnv ze tv
       ; k ze' tv' }

We thus define:

  newtype ZonkBndrT m a =
    ZonkBndrT { runZonkBndrT :: forall r. (a -> ZonkT m r) -> ZonkT m r }

which is the type of continuation-passing computations over ZonkT m = ReaderT ZonkEnv m.
We thus have:

  zonkTyBndrX :: TcTyVar -> ZonkBndrT TcM TyVar

which expresses the fact that zonkTyBndrX takes in a TcTyVar, returns a TyVar,
modifying the ZonkEnv state in the process. We can build computations out of it
by using runZonkBndrT and nesting. For example, zonking a type synonym:

  zonkTySynRHS :: [TcTyConBinder] -> [TcTyVar] -> ZonkT TcM ([TyConBinder], [TyVar])
  zonkTySynRHS tc_bndrs rhs_tc_ty =
    runZonkBndrT (zonkTyVarBindersX tc_bndrs) $ \ bndrs ->
      do { rhs_ty <- zonkTcTypeToTypeX rhs_tc_ty
         ; return (bndrs, rhs_ty) }

This is known as the codensity transformation, where

  newtype Codensity m a = Codensity { forall r. (a -> m r) -> m r }

expresses continuation-passing computations in the monad m.

Codensity (ReaderT s m) naturally corresponds to StateT s (Codensity m), and
the instances for Codensity reflect that, e.g.

  traverse :: (a -> ZonkBndrT m b) -> t a -> ZonkBndrT m (t b)

naturally behaves like mapAccumLM, accumulating changes to the ZonkEnv as
we go.
-}

-- | Zonk some binders and run the continuation.
--
-- Example:
--
-- > zonk (ForAllTy (Bndr tv vis) body_ty)
-- >  = runZonkBndrT (zonkTyBndrX tv) $ \ tv' ->
-- >    do { body_ty' <- zonkTcTypeToTypeX body_ty
-- >       ; return (ForAllTy (Bndr tv' vis) body_ty') }
--
-- See Note [Continuation-passing style for zonking].
runZonkBndrT :: ZonkBndrT m a -> forall r. (a -> ZonkT m r) -> ZonkT m r
runZonkBndrT (ZonkBndrT k) f = k (oneShot f)
{-# INLINE runZonkBndrT #-}

-- | Embed a computation that doesn't modify the 'ZonkEnv' into 'ZonkBndrT'.
noBinders :: Monad m => ZonkT m a -> ZonkBndrT m a
noBinders z = coerce $ toCodensity z
{-# INLINE noBinders #-}

-- | Run a nested computation that modifies the 'ZonkEnv',
-- without affecting the outer environment.
don'tBind :: Monad m => ZonkBndrT m a -> ZonkT m a
don'tBind (ZonkBndrT k) = fromCodensity (Codensity k)
{-# INLINE don'tBind #-}

initZonkEnv :: MonadIO m => ZonkFlexi -> ZonkT m b -> m b
initZonkEnv flexi thing_inside
  = do { mtv_env_ref <- liftIO $ newIORef emptyVarEnv
       ; let ze = ZonkEnv { ze_flexi = flexi
                          , ze_tv_env = emptyVarEnv
                          , ze_id_env = emptyVarEnv
                          , ze_meta_tv_env = mtv_env_ref }

       ; runZonkT thing_inside ze }
{-# INLINEABLE initZonkEnv #-} -- so it can be specialised

nestZonkEnv :: (ZonkEnv -> ZonkEnv) -> ZonkBndrT m ()
nestZonkEnv f = ZonkBndrT $ \ k ->
  case k () of
    ZonkT g -> ZonkT (g . f)
{-# INLINE nestZonkEnv #-}

getZonkEnv :: Monad m => ZonkT m ZonkEnv
getZonkEnv = ZonkT return
{-# INLINE getZonkEnv #-}

-- | Extend the knot-tied environment.
extendIdZonkEnvRec :: [Var] -> ZonkBndrT m ()
extendIdZonkEnvRec ids =
  nestZonkEnv $
    \ ze@(ZonkEnv { ze_id_env = id_env }) ->
    -- NB: Don't look at the var to decide which env't to put it in. That
    -- would end up knot-tying all the env'ts.
      ze { ze_id_env = extendVarEnvList id_env [(id,id) | id <- ids] }
  -- Given coercion variables will actually end up here. That's OK though:
  -- coercion variables are never looked up in the knot-tied env't, so zonking
  -- them simply doesn't get optimised. No one gets hurt. An improvement (?)
  -- would be to do SCC analysis in zonkEvBinds and then only knot-tie the
  -- recursive groups. But perhaps the time it takes to do the analysis is
  -- more than the savings.

extendZonkEnv :: [Var] -> ZonkBndrT m ()
extendZonkEnv vars =
  nestZonkEnv $
    \ ze@(ZonkEnv { ze_tv_env = tyco_env, ze_id_env = id_env }) ->
      ze { ze_tv_env = extendVarEnvList tyco_env [(tv,tv) | tv <- tycovars]
         , ze_id_env = extendVarEnvList id_env   [(id,id) | id <- ids] }
  where
    (tycovars, ids) = partition isTyCoVar vars

extendIdZonkEnv :: Var -> ZonkBndrT m ()
extendIdZonkEnv id =
  nestZonkEnv $
    \ ze@(ZonkEnv { ze_id_env = id_env }) ->
      ze { ze_id_env = extendVarEnv id_env id id }

extendTyZonkEnv :: TyVar -> ZonkBndrT m ()
extendTyZonkEnv tv =
  nestZonkEnv $
    \ ze@(ZonkEnv { ze_tv_env = ty_env }) ->
      ze { ze_tv_env = extendVarEnv ty_env tv tv }

setZonkType :: ZonkFlexi -> ZonkT m a -> ZonkT m a
setZonkType flexi (ZonkT f) = ZonkT $ \ ze ->
  f $ ze { ze_flexi = flexi }
