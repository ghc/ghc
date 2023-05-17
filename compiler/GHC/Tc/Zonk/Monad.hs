{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TupleSections #-}

module GHC.Tc.Zonk.Monad where

import GHC.Prelude

import GHC.Core.Type

import GHC.Driver.Flags

import GHC.Types.SrcLoc ( SrcSpan )
import GHC.Types.Var ( TcTyVar, Id, isTyCoVar )
import GHC.Types.Var.Env

import GHC.Tc.Utils.TcType ( TcLevel )

import GHC.Utils.Logger
import GHC.Utils.Outputable

import Control.Monad ( when )
import Control.Monad.Fix
import Control.Monad.IO.Class ( MonadIO(..) )
import Control.Monad.Trans.Class
import Data.Coerce
import Data.IORef
import Data.List ( partition )
import qualified Data.Kind as Hs

import GHC.Exts( oneShot )

-- For MonadFix instance
import Control.Concurrent.MVar
import Control.Exception
import GHC.IO.Exception
import GHC.IO.Unsafe ( unsafeDupableInterleaveIO )

--------------------------------------------------------------------------------


-- | See Note [The ZonkEnv]
-- Confused by zonking? See Note [What is zonking?] in "GHC.Tc.Utils.TcMType".
data ZonkEnv  -- See Note [The ZonkEnv]
  = ZonkEnv { ze_flexi       :: !ZonkFlexi
            , ze_tv_env      :: TyCoVarEnv TyCoVar
            , ze_id_env      :: IdEnv      Id
            , ze_meta_tv_env :: IORef (TyVarEnv Type) }
newtype ZonkT m a = ZonkT' { runZonkT :: ZonkEnv -> m a }
--  deriving (Functor, Applicative, Monad, MonadIO, MonadFix)
--    via ReaderT ZonkEnv m
--  deriving MonadTrans
--    via ReaderT ZonkEnv


{-# COMPLETE ZonkT #-}
pattern ZonkT :: forall m a. (ZonkEnv -> m a) -> ZonkT m a
pattern ZonkT m <- ZonkT' m
  where
    ZonkT m = ZonkT' (oneShot m)

unZonkT :: ZonkT m a -> (ZonkEnv -> m a)
unZonkT (ZonkT m) = m
{-# INLINE unZonkT #-}

instance Functor m => Functor (ZonkT m) where
  fmap f (ZonkT g) = ZonkT $ \env -> fmap f (g env)
  a <$ ZonkT g     = ZonkT $ \env -> a <$ g env
  {-# INLINE fmap #-}
  {-# INLINE (<$) #-}
instance Applicative m => Applicative (ZonkT m) where
  pure a = ZonkT (\ _ -> pure a)
  ZonkT f <*> ZonkT x = ZonkT (\ env -> f env <*> x env )
  ZonkT m *> f = ZonkT (\ env -> m env *> unZonkT f env)
  {-# INLINE pure #-}
  {-# INLINE (<*>) #-}
  {-# INLINE (*>) #-}

instance Monad m => Monad (ZonkT m) where
  ZonkT m >>= f =
    ZonkT (\ env -> do { r <- m env
                       ; unZonkT (f r) env })
  (>>)   = (*>)
  {-# INLINE (>>=) #-}
  {-# INLINE (>>) #-}

instance MonadIO m => MonadIO (ZonkT m) where
  liftIO f = ZonkT (\_ -> liftIO f)
  {-# INLINE liftIO #-}

instance MonadTrans ZonkT where
  lift ma = ZonkT $ \ _ -> ma
  {-# INLINE lift #-}

instance MonadFix m => MonadFix (ZonkT m) where
  mfix f = ZonkT $ \ r -> mfix $ oneShot $ \ a -> runZonkT (f a) r
  {-# INLINE mfix #-}

-- | Zonk binders, bringing them into scope in the inner computation.
--
-- Can be thought of as a state monad transformer @StateT ZonkEnv m a@.
newtype ZonkBndrT m a = ZonkBndrT { zonkBndr' :: forall r. (a -> ZonkT m r) -> ZonkT m r }
  deriving (Functor, Applicative, Monad, MonadIO, MonadFix)
    via Codensity (ZonkT m)

zonkBndr :: ZonkBndrT m a -> forall r. (a -> ZonkT m r) -> ZonkT m r
zonkBndr (ZonkBndrT k) f = k (oneShot f)

runBndrT :: Monad m => ZonkBndrT m a -> ZonkT m (ZonkEnv, a)
runBndrT (ZonkBndrT k) = k go
  where go a = do { ze <- getZonkEnv; return (ze, a) }
{-# INLINE runBndrT #-}

runZonkBndrT :: Monad m => ZonkBndrT m a -> ZonkEnv -> m (ZonkEnv, a)
runZonkBndrT = runZonkT . runBndrT
{-# INLINE runZonkBndrT #-}

-- | Embed a computation that doesn't modify the 'ZonkEnv'.
noBinders :: Monad m => ZonkT m a -> ZonkBndrT m a
noBinders z = coerce $ toCodensity z
{-# INLINE noBinders #-}

-- | Run a nested computation that modifies the 'ZonkEnv',
-- without affecting the outer environment.
don'tBind :: Monad m => ZonkBndrT m a -> ZonkT m a
don'tBind (ZonkBndrT k) = fromCodensity (Codensity k)
{-# INLINE don'tBind #-}


--- Copied from kan-extensions for now...
type Codensity :: (Hs.Type -> Hs.Type) -> Hs.Type -> Hs.Type
newtype Codensity m a = Codensity { runCodensity :: forall r. (a -> m r) -> m r }
instance Functor (Codensity k) where
  fmap f (Codensity m) = Codensity $ oneShot (\k -> m $ oneShot (\x -> k $ f x))
  {-# INLINE fmap #-}
instance Applicative (Codensity f) where
  pure x = Codensity $ oneShot (\k -> k x)
  {-# INLINE pure #-}
  Codensity f <*> Codensity g = Codensity $ oneShot (\bfr -> f $ oneShot (\ab -> g $ oneShot (\x -> bfr (ab x))))
  {-# INLINE (<*>) #-}
instance Monad (Codensity f) where
  return = pure
  {-# INLINE return #-}
  m >>= k = Codensity $ oneShot (\c -> runCodensity m $ oneShot (\a -> runCodensity (k a) c))
  {-# INLINE (>>=) #-}
instance MonadTrans Codensity where
  lift m = Codensity $ oneShot (m >>=)
  {-# INLINE lift #-}
instance MonadIO m => MonadIO (Codensity m) where
  liftIO = lift . liftIO
  {-# INLINE liftIO #-}
instance MonadIO m => MonadFix (Codensity m) where
  mfix f = Codensity $ oneShot $ \ k -> do
    promise <- liftIO $ newEmptyMVar
    ans     <- liftIO $ unsafeDupableInterleaveIO
                      $ readMVar promise
                          `catch`
                        (\ BlockedIndefinitelyOnMVar -> throwIO FixIOException)
    runCodensity (f ans) $ oneShot $ \ a -> do
      liftIO $ putMVar promise a
      k a
  {-# INLINE mfix #-}

toCodensity :: Monad m => m a -> Codensity m a
toCodensity m = Codensity $ oneShot (m >>=)

fromCodensity :: Monad m => Codensity m a -> m a
fromCodensity c = runCodensity c return

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
  Specifically, zonkIdOcc is not monadic.

* ze_meta_tv_env: see Note [Sharing when zonking to Type]


Notes:
  * We must be careful never to put coercion variables (which are Ids,
    after all) in the knot-tied ze_id_env, because coercions can
    appear in types, and we sometimes inspect a zonked type in this
    module.  [Question: where, precisely?]

  * In zonkTyVarOcc we consult ze_tv_env in a monadic context,
    a second reason that ze_tv_env can't be monadic.

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

data ZonkFlexi   -- See Note [Un-unified unification variables]
  = DefaultFlexi    -- Default unbound unification variables to Any
  | SkolemiseFlexi  -- Skolemise unbound unification variables
                    -- See Note [Zonking the LHS of a RULE]
  | RuntimeUnkFlexi -- Used in the GHCi debugger
  | NoFlexi         -- Panic on unfilled meta-variables
                    -- See Note [Error on unconstrained meta-variables]
                    -- in GHC.Tc.Utils.TcMType

instance Outputable ZonkEnv where
  ppr (ZonkEnv { ze_tv_env = tv_env
               , ze_id_env = id_env })
    = text "ZE" <+> braces (vcat
         [ text "ze_tv_env =" <+> ppr tv_env
         , text "ze_id_env =" <+> ppr id_env ])


{-# INLINEABLE initZonkEnv #-} -- so it can be specialised
initZonkEnv :: MonadIO m => ZonkFlexi -> ZonkT m b -> m b
initZonkEnv flexi thing_inside
  = do { mtv_env_ref <- liftIO $ newIORef emptyVarEnv
       ; let ze = ZonkEnv { ze_flexi = flexi
                          , ze_tv_env = emptyVarEnv
                          , ze_id_env = emptyVarEnv
                          , ze_meta_tv_env = mtv_env_ref }

       ; runZonkT thing_inside ze }

nestZonkEnv :: (ZonkEnv -> ZonkEnv) -> ZonkBndrT m ()
nestZonkEnv f = ZonkBndrT $ \ k ->
  case k () of
    ZonkT g -> ZonkT (g . f)

getZonkEnv :: Monad m => ZonkT m ZonkEnv
getZonkEnv = ZonkT return

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

extendMetaEnv :: MonadIO m => TcTyVar -> Type -> ZonkT m ()
extendMetaEnv tv ty =
  ZonkT $ \ ( ZonkEnv { ze_meta_tv_env = mtv_env_ref } ) ->
    liftIO $ modifyIORef' mtv_env_ref (\env -> extendVarEnv env tv ty)

lookupInTyVarEnv :: Monad m => TcTyVar -> ZonkT m (Maybe TyVar)
lookupInTyVarEnv tv =
  ZonkT $ \ ( ZonkEnv { ze_tv_env = tv_env } ) ->
    return $ lookupVarEnv tv_env tv

lookupMetaTv :: MonadIO m => TcTyVar -> ZonkT m (Maybe Type)
lookupMetaTv tv =
  ZonkT $ \ ( ZonkEnv { ze_meta_tv_env = mtv_env_ref } ) ->
    do { mtv_env <- liftIO $ readIORef mtv_env_ref
       ; return $ lookupVarEnv mtv_env tv }

--------------------------------------------------------------------------------

-- | Information needed by the 'ZonkM' monad, which is a slimmed down version
-- of 'TcM' with just enough information for zonking.
data ZonkGblEnv
  = ZonkGblEnv
    { zge_logger       :: !Logger     -- needed for zonkTrace
    , zge_name_ppr_ctx :: !NamePprCtx --          ''
    , zge_src_span     :: !SrcSpan  -- needed for skolemiseUnboundMetaTyVar
    , zge_tc_level     :: !TcLevel  --               ''
    }

-- | A stripped down version of 'TcM' which is sufficient for zonking types.
newtype ZonkM a = ZonkM' { runZonkM :: ZonkGblEnv -> IO a }
--  deriving (Functor, Applicative, Monad, MonadIO)
--    via ReaderT ZonkGblEnv IO

{-# COMPLETE ZonkM #-}
pattern ZonkM :: forall a. (ZonkGblEnv -> IO a) -> ZonkM a
pattern ZonkM m <- ZonkM' m
  where
    ZonkM m = ZonkM' (oneShot m)

unZonkM :: ZonkM a -> (ZonkGblEnv -> IO a)
unZonkM (ZonkM m) = m

instance Functor ZonkM where
  fmap f (ZonkM g) = ZonkM $ \env -> fmap f (g env)
  a <$ ZonkM g     = ZonkM $ \env -> a <$ g env
  {-# INLINE fmap #-}
  {-# INLINE (<$) #-}
instance Applicative ZonkM where
  pure a = ZonkM (\ _ -> pure a)
  ZonkM f <*> ZonkM x = ZonkM (\ env -> f env <*> x env )
  ZonkM m *> f = ZonkM (\ env -> m env *> unZonkM f env)
  {-# INLINE pure #-}
  {-# INLINE (<*>) #-}
  {-# INLINE (*>) #-}

instance Monad ZonkM where
  ZonkM m >>= f =
    ZonkM (\ env -> do { r <- m env
                       ; unZonkM (f r) env })
  (>>)   = (*>)
  {-# INLINE (>>=) #-}
  {-# INLINE (>>) #-}

instance MonadIO ZonkM where
  liftIO f = ZonkM (\_ -> f)
  {-# INLINE liftIO #-}

getZonkGblEnv :: ZonkM ZonkGblEnv
getZonkGblEnv = ZonkM return
{-# INLINE getZonkGblEnv #-}

-- SLD TODO: replace with TcRef
readTcRefZ :: IORef a -> ZonkM a
readTcRefZ ref = liftIO $ readIORef ref
{-# INLINE readTcRefZ #-}

writeTcRefZ :: IORef a -> a -> ZonkM ()
writeTcRefZ ref a = liftIO $ writeIORef ref a
{-# INLINE writeTcRefZ #-}

-- | Same as 'traceTc', but for the 'ZonkM' monad.
traceZonk :: String -> SDoc -> ZonkM ()
traceZonk herald doc = ZonkM $
  \ ( ZonkGblEnv { zge_logger = logger, zge_name_ppr_ctx = ppr_ctx }) ->
    do { let sty   = mkDumpStyle ppr_ctx
             flag  = Opt_D_dump_tc_trace
             title = ""
             msg   = hang (text herald) 2 doc
       ; when (logHasDumpFlag logger flag) $
         logDumpFile logger sty flag title FormatText msg
       }
{-# INLINE traceZonk #-}
  -- see Note [INLINE conditional tracing utilities] in GHC.Tc.Utils.Monad
