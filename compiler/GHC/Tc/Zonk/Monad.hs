{-# LANGUAGE PatternSynonyms #-}

-- | The 'ZonkM' monad, a stripped down 'TcM', used when zonking within
-- the typechecker in "GHC.Tc.Zonk.TcType".
--
-- See Note [Module structure for zonking] in GHC.Tc.Zonk.Type.
module GHC.Tc.Zonk.Monad
  ( -- * The 'ZonkM' monad, a stripped down 'TcM' for zonking
    ZonkM, pattern ZonkM, runZonkM
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

import GHC.Utils.Monad.StrictReader

import Control.Monad          ( when )

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
--
-- It is strict in the 'ZonkGblEnv', and uses the one-shot trick;
-- see Note [Instances for StrictReaderT] in GHC.Utils.Monad.StrictReader
type ZonkM = StrictReaderT ZonkGblEnv IO

{-# COMPLETE ZonkM #-}
pattern ZonkM :: forall a. (ZonkGblEnv -> IO a) -> ZonkM a
pattern ZonkM m = StrictReaderT m

runZonkM :: ZonkM a -> ZonkGblEnv -> IO a
runZonkM = runStrictReaderT
{-# INLINE runZonkM #-}

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
