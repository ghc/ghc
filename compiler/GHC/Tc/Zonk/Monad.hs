{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}

-- | The 'ZonkM' monad, a stripped down 'TcM', used when zonking within
-- the typechecker in "GHC.Tc.Zonk.TcType".
--
-- See Note [Module structure for zonking] in GHC.Tc.Zonk.Type.
module GHC.Tc.Zonk.Monad
  ( -- * The 'ZonkM' monad, a stripped down 'TcM' for zonking
    ZonkM(ZonkM,runZonkM)
  , ZonkGblEnv(..), getZonkGblEnv, getZonkTcLevel

   -- ** Logging within 'ZonkM'
  , traceZonk

  )
  where

import GHC.Prelude

import GHC.Driver.Flags ( DumpFlag(Opt_D_dump_tc_trace) )

import GHC.Types.SrcLoc ( SrcSpan )

import GHC.Tc.Types.BasicTypes ( TcBinderStack )
import GHC.Tc.Utils.TcType   ( TcLevel )

import GHC.Utils.Logger
import GHC.Utils.Outputable

import Control.Monad          ( when )
import Control.Monad.IO.Class ( MonadIO(..) )

import GHC.Exts               ( oneShot )

--------------------------------------------------------------------------------

-- | Information needed by the 'ZonkM' monad, which is a slimmed down version
-- of 'TcM' with just enough information for zonking.
data ZonkGblEnv
  = ZonkGblEnv
    { zge_logger       :: Logger     -- needed for traceZonk
    , zge_name_ppr_ctx :: NamePprCtx --          ''
    , zge_src_span     :: SrcSpan  -- needed for skolemiseUnboundMetaTyVar
    , zge_tc_level     :: TcLevel  --               ''
    , zge_binder_stack :: TcBinderStack -- needed for tcInitTidyEnv
    }

-- | A stripped down version of 'TcM' which is sufficient for zonking types.
newtype ZonkM a = ZonkM' { runZonkM :: ZonkGblEnv -> IO a }
{-
NB: we write the following instances by hand:

--  deriving (Functor, Applicative, Monad, MonadIO)
--    via ReaderT ZonkGblEnv IO

See Note [Instances for ZonkT] in GHC.Tc.Zonk.Env for the reasoning:

  - oneShot annotations,
  - strictness annotations to enable worker-wrapper.
-}

{-# COMPLETE ZonkM #-}
pattern ZonkM :: forall a. (ZonkGblEnv -> IO a) -> ZonkM a
pattern ZonkM m <- ZonkM' m
  where
    ZonkM m = ZonkM' (oneShot m)
-- See Note [The one-shot state monad trick] in GHC.Utils.Monad

instance Functor ZonkM where
  fmap f (ZonkM g) = ZonkM $ \ !env -> fmap f (g env)
  a <$ ZonkM g     = ZonkM $ \ !env -> a <$ g env
  {-# INLINE fmap #-}
  {-# INLINE (<$) #-}
instance Applicative ZonkM where
  pure a = ZonkM (\ !_ -> pure a)
  ZonkM f <*> ZonkM x = ZonkM (\ !env -> f env <*> x env )
  ZonkM m *> f = ZonkM (\ !env -> m env *> runZonkM f env)
  {-# INLINE pure #-}
  {-# INLINE (<*>) #-}
  {-# INLINE (*>) #-}

instance Monad ZonkM where
  ZonkM m >>= f =
    ZonkM (\ !env -> do { r <- m env
                        ; runZonkM (f r) env })
  (>>)   = (*>)
  {-# INLINE (>>=) #-}
  {-# INLINE (>>) #-}

instance MonadIO ZonkM where
  liftIO f = ZonkM (\ !_ -> f)
  {-# INLINE liftIO #-}

getZonkGblEnv :: ZonkM ZonkGblEnv
getZonkGblEnv = ZonkM return
{-# INLINE getZonkGblEnv #-}

getZonkTcLevel :: ZonkM TcLevel
getZonkTcLevel = ZonkM (\env -> return (zge_tc_level env))

-- | Same as 'traceTc', but for the 'ZonkM' monad.
traceZonk :: String -> SDoc -> ZonkM ()
traceZonk herald doc = ZonkM $
  \ ( ZonkGblEnv { zge_logger = !logger, zge_name_ppr_ctx = ppr_ctx }) ->
    do { let sty   = mkDumpStyle ppr_ctx
             flag  = Opt_D_dump_tc_trace
             title = ""
             msg   = hang (text herald) 2 doc
       ; when (logHasDumpFlag logger flag) $
         logDumpFile logger sty flag title FormatText msg
       }
{-# INLINE traceZonk #-}
  -- see Note [INLINE conditional tracing utilities] in GHC.Tc.Utils.Monad
