{-# LANGUAGE PatternSynonyms #-}
{-
(c) The AQUA Project, Glasgow University, 1993-1998

\section[GHC.Core.Opt.Simplify.Monad]{The simplifier Monad}
-}

module GHC.Core.Opt.Simplify.Monad (
        -- The monad
        TopEnvConfig(..), SimplM,
        initSmpl, traceSmpl,
        getSimplRules,

        -- Unique supply
        MonadUnique(..), newId, newJoinId,

        -- Counting
        SimplCount, tick, freeTick, checkedTick,
        getSimplCount, zeroSimplCount, pprSimplCount,
        plusSimplCount, isZeroSimplCount
    ) where

import GHC.Prelude

import GHC.Types.Var       ( Var, isId, mkLocalVar )
import GHC.Types.Name      ( mkSystemVarName )
import GHC.Types.Id        ( Id, mkSysLocalOrCoVarM )
import GHC.Types.Id.Info   ( IdDetails(..), vanillaIdInfo, setArityInfo )
import GHC.Core.Type       ( Type, Mult )
import GHC.Core.Opt.Stats
import GHC.Core.Rules
import GHC.Core.Utils      ( mkLamTypes )
import GHC.Types.Unique.Supply
import GHC.Driver.Flags
import GHC.Utils.Outputable
import GHC.Data.FastString
import GHC.Utils.Monad
import GHC.Utils.Logger as Logger
import GHC.Utils.Misc      ( count )
import GHC.Utils.Panic     (throwGhcExceptionIO, GhcException (..))
import GHC.Types.Basic     ( IntWithInf, treatZeroAsInf, mkIntWithInf )
import Control.Monad       ( ap )
import GHC.Core.Multiplicity        ( pattern ManyTy )
import GHC.Exts( oneShot )

{-
************************************************************************
*                                                                      *
\subsection{Monad plumbing}
*                                                                      *
************************************************************************
-}

newtype SimplM result
  =  SM'  { unSM :: SimplTopEnv
                 -> SimplCount
                 -> IO (result, SimplCount)}
    -- We only need IO here for dump output, but since we already have it
    -- we might as well use it for uniques.

pattern SM :: (SimplTopEnv -> SimplCount
               -> IO (result, SimplCount))
          -> SimplM result
-- This pattern synonym makes the simplifier monad eta-expand,
-- which as a very beneficial effect on compiler performance
-- (worth a 1-2% reduction in bytes-allocated).  See #18202.
-- See Note [The one-shot state monad trick] in GHC.Utils.Monad
pattern SM m <- SM' m
  where
    SM m = SM' (oneShot $ \env -> oneShot $ \ct -> m env ct)

-- See Note [The environments of the Simplify pass]
data TopEnvConfig = TopEnvConfig
  { te_history_size :: !Int
  , te_tick_factor :: !Int
  }

data SimplTopEnv
  = STE { -- See Note [The environments of the Simplify pass]
          st_config :: !TopEnvConfig
        , st_logger    :: !Logger
        , st_max_ticks :: !IntWithInf  -- ^ Max #ticks in this simplifier run
        , st_read_ruleenv :: !(IO RuleEnv)
          -- ^ The action to retrieve an up-to-date EPS RuleEnv
          -- See Note [Overall plumbing for rules]
        }

initSmpl :: Logger
         -> IO RuleEnv
         -> TopEnvConfig
         -> Int -- ^ Size of the bindings, used to limit the number of ticks we allow
         -> SimplM a
         -> IO (a, SimplCount)

initSmpl logger read_ruleenv cfg size m
  = do -- No init count; set to 0
       let simplCount = zeroSimplCount $ logHasDumpFlag logger Opt_D_dump_simpl_stats
       unSM m env simplCount
  where
    env = STE { st_config = cfg
              , st_logger = logger
              , st_max_ticks = computeMaxTicks cfg size
              , st_read_ruleenv = read_ruleenv
              }

computeMaxTicks :: TopEnvConfig -> Int -> IntWithInf
-- Compute the max simplifier ticks as
--     (base-size + pgm-size) * magic-multiplier * tick-factor/100
-- where
--    magic-multiplier is a constant that gives reasonable results
--    base-size is a constant to deal with size-zero programs
computeMaxTicks cfg size
  = treatZeroAsInf $
    fromInteger ((toInteger (size + base_size)
                  * toInteger (tick_factor * magic_multiplier))
          `div` 100)
  where
    tick_factor      = te_tick_factor cfg
    base_size        = 100
    magic_multiplier = 40
        -- MAGIC NUMBER, multiplies the simplTickFactor
        -- We can afford to be generous; this is really
        -- just checking for loops, and shouldn't usually fire
        -- A figure of 20 was too small: see #5539.

{-# INLINE thenSmpl #-}
{-# INLINE thenSmpl_ #-}
{-# INLINE returnSmpl #-}
{-# INLINE mapSmpl #-}

instance Functor SimplM where
  fmap = mapSmpl

instance Applicative SimplM where
    pure  = returnSmpl
    (<*>) = ap
    (*>)  = thenSmpl_

instance Monad SimplM where
   (>>)   = (*>)
   (>>=)  = thenSmpl

mapSmpl :: (a -> b) -> SimplM a -> SimplM b
mapSmpl f m = thenSmpl m (returnSmpl . f)

returnSmpl :: a -> SimplM a
returnSmpl e = SM (\_st_env sc -> return (e, sc))

thenSmpl  :: SimplM a -> (a -> SimplM b) -> SimplM b
thenSmpl_ :: SimplM a -> SimplM b -> SimplM b

thenSmpl m k
  = SM $ \st_env sc0 -> do
      (m_result, sc1) <- unSM m st_env sc0
      unSM (k m_result) st_env sc1

thenSmpl_ m k
  = SM $ \st_env sc0 -> do
      (_, sc1) <- unSM m st_env sc0
      unSM k st_env sc1

-- TODO: this specializing is not allowed
-- {-# SPECIALIZE mapM         :: (a -> SimplM b) -> [a] -> SimplM [b] #-}
-- {-# SPECIALIZE mapAndUnzipM :: (a -> SimplM (b, c)) -> [a] -> SimplM ([b],[c]) #-}
-- {-# SPECIALIZE mapAccumLM   :: (acc -> b -> SimplM (acc,c)) -> acc -> [b] -> SimplM (acc, [c]) #-}

traceSmpl :: String -> SDoc -> SimplM ()
traceSmpl herald doc
  = do logger <- getLogger
       liftIO $ Logger.putDumpFileMaybe logger Opt_D_dump_simpl_trace "Simpl Trace"
         FormatText
         (hang (text herald) 2 doc)
{-# INLINE traceSmpl #-}  -- see Note [INLINE conditional tracing utilities]

{-
************************************************************************
*                                                                      *
\subsection{The unique supply}
*                                                                      *
************************************************************************
-}

-- See Note [Uniques for wired-in prelude things and known tags] in GHC.Builtin.Uniques
simplTag :: Char
simplTag = 's'

instance MonadUnique SimplM where
    getUniqueSupplyM = liftIO $ mkSplitUniqSupply simplTag
    getUniqueM = liftIO $ uniqFromTag simplTag

instance HasLogger SimplM where
    getLogger = gets st_logger

instance MonadIO SimplM where
    liftIO = liftIOWithEnv . const

getSimplRules :: SimplM RuleEnv
getSimplRules = liftIOWithEnv st_read_ruleenv

liftIOWithEnv :: (SimplTopEnv -> IO a) -> SimplM a
liftIOWithEnv m = SM (\st_env sc -> do
    x <- m st_env
    return (x, sc))

gets :: (SimplTopEnv -> a) -> SimplM a
gets f = liftIOWithEnv (return . f)

newId :: FastString -> Mult -> Type -> SimplM Id
newId fs w ty = mkSysLocalOrCoVarM fs w ty

-- | Make a join id with given type and arity but without call-by-value annotations.
newJoinId :: [Var] -> Type -> SimplM Id
newJoinId bndrs body_ty
  = do { uniq <- getUniqueM
       ; let name       = mkSystemVarName uniq (fsLit "$j")
             join_id_ty = mkLamTypes bndrs body_ty  -- Note [Funky mkLamTypes]
             arity      = count isId bndrs
             -- arity: See Note [Invariants on join points] invariant 2b, in GHC.Core
             join_arity = length bndrs
             details    = JoinId join_arity Nothing
             id_info    = vanillaIdInfo `setArityInfo` arity
--                                        `setOccInfo` strongLoopBreaker

       ; return (mkLocalVar details name ManyTy join_id_ty id_info) }

{-
************************************************************************
*                                                                      *
\subsection{Counting up what we've done}
*                                                                      *
************************************************************************
-}

getSimplCount :: SimplM SimplCount
getSimplCount = SM (\_st_env sc -> return (sc, sc))

tick :: Tick -> SimplM ()
tick t = SM (\st_env sc -> let
  history_size = te_history_size (st_config st_env)
  sc' = doSimplTick history_size t sc
  in sc' `seq` return ((), sc'))

checkedTick :: Tick -> SimplM ()
-- Try to take a tick, but fail if too many
checkedTick t
  = SM (\st_env sc ->
           if st_max_ticks st_env <= mkIntWithInf (simplCountN sc)
           then throwGhcExceptionIO $
                  PprProgramError "Simplifier ticks exhausted" (msg sc)
           else let
             history_size = te_history_size (st_config st_env)
             sc' = doSimplTick history_size t sc
             in sc' `seq` return ((), sc'))
  where
    msg sc = vcat
      [ text "When trying" <+> ppr t
      , text "To increase the limit, use -fsimpl-tick-factor=N (default 100)."
      , space
      , text "In addition try adjusting -funfolding-case-threshold=N and"
      , text "-funfolding-case-scaling=N for the module in question."
      , text "Using threshold=1 and scaling=5 should break most inlining loops."
      , space
      , text "If you need to increase the tick factor substantially, while also"
      , text "adjusting unfolding parameters please file a bug report and"
      , text "indicate the factor you needed."
      , space
      , text "If GHC was unable to complete compilation even"
               <+> text "with a very large factor"
      , text "(a thousand or more), please consult the"
                <+> doubleQuotes (text "Known bugs or infelicities")
      , text "section in the Users Guide before filing a report. There are a"
      , text "few situations unlikely to occur in practical programs for which"
      , text "simplifier non-termination has been judged acceptable."
      , space
      , pp_details sc
      , pprSimplCount sc ]
    pp_details sc
      | hasDetailedCounts sc = empty
      | otherwise = text "To see detailed counts use -ddump-simpl-stats"


freeTick :: Tick -> SimplM ()
-- Record a tick, but don't add to the total tick count, which is
-- used to decide when nothing further has happened
freeTick t
   = SM (\_st_env sc -> let sc' = doFreeSimplTick t sc
                           in sc' `seq` return ((), sc'))
