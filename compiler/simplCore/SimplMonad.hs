{-
(c) The AQUA Project, Glasgow University, 1993-1998

\section[SimplMonad]{The simplifier Monad}
-}

module SimplMonad (
        -- The monad
        SimplM,
        initSmpl, traceSmpl,
        getSimplRules, getFamEnvs,

        -- Unique supply
        MonadUnique(..), newId, newJoinId,

        -- Counting
        SimplCount, tick, freeTick, checkedTick,
        getSimplCount, zeroSimplCount, pprSimplCount,
        plusSimplCount, isZeroSimplCount
    ) where

import GhcPrelude

import Var              ( Var, isTyVar, mkLocalVar )
import Name             ( mkSystemVarName )
import Id               ( Id, mkSysLocalOrCoVar )
import IdInfo           ( IdDetails(..), vanillaIdInfo, setArityInfo )
import Type             ( Type, mkLamTypes )
import FamInstEnv       ( FamInstEnv )
import CoreSyn          ( RuleEnv(..) )
import UniqSupply
import DynFlags
import CoreMonad
import Outputable
import FastString
import MonadUtils
import ErrUtils as Err
import Panic (throwGhcExceptionIO, GhcException (..))
import BasicTypes          ( IntWithInf, treatZeroAsInf, mkIntWithInf )
import Control.Monad       ( liftM, ap )

{-
************************************************************************
*                                                                      *
\subsection{Monad plumbing}
*                                                                      *
************************************************************************

For the simplifier monad, we want to {\em thread} a unique supply and a counter.
(Command-line switches move around through the explicitly-passed SimplEnv.)
-}

newtype SimplM result
  =  SM  { unSM :: SimplTopEnv  -- Envt that does not change much
                -> UniqSupply   -- We thread the unique supply because
                                -- constantly splitting it is rather expensive
                -> SimplCount
                -> IO (result, UniqSupply, SimplCount)}
  -- we only need IO here for dump output

data SimplTopEnv
  = STE { st_flags     :: DynFlags
        , st_max_ticks :: IntWithInf  -- Max #ticks in this simplifier run
        , st_rules     :: RuleEnv
        , st_fams      :: (FamInstEnv, FamInstEnv) }

initSmpl :: DynFlags -> RuleEnv -> (FamInstEnv, FamInstEnv)
         -> UniqSupply          -- No init count; set to 0
         -> Int                 -- Size of the bindings, used to limit
                                -- the number of ticks we allow
         -> SimplM a
         -> IO (a, SimplCount)

initSmpl dflags rules fam_envs us size m
  = do (result, _, count) <- unSM m env us (zeroSimplCount dflags)
       return (result, count)
  where
    env = STE { st_flags = dflags, st_rules = rules
              , st_max_ticks = computeMaxTicks dflags size
              , st_fams = fam_envs }

computeMaxTicks :: DynFlags -> Int -> IntWithInf
-- Compute the max simplifier ticks as
--     (base-size + pgm-size) * magic-multiplier * tick-factor/100
-- where
--    magic-multiplier is a constant that gives reasonable results
--    base-size is a constant to deal with size-zero programs
computeMaxTicks dflags size
  = treatZeroAsInf $
    fromInteger ((toInteger (size + base_size)
                  * toInteger (tick_factor * magic_multiplier))
          `div` 100)
  where
    tick_factor      = simplTickFactor dflags
    base_size        = 100
    magic_multiplier = 40
        -- MAGIC NUMBER, multiplies the simplTickFactor
        -- We can afford to be generous; this is really
        -- just checking for loops, and shouldn't usually fire
        -- A figure of 20 was too small: see Trac #5539.

{-# INLINE thenSmpl #-}
{-# INLINE thenSmpl_ #-}
{-# INLINE returnSmpl #-}


instance Functor SimplM where
    fmap = liftM

instance Applicative SimplM where
    pure  = returnSmpl
    (<*>) = ap
    (*>)  = thenSmpl_

instance Monad SimplM where
   (>>)   = (*>)
   (>>=)  = thenSmpl

returnSmpl :: a -> SimplM a
returnSmpl e = SM (\_st_env us sc -> return (e, us, sc))

thenSmpl  :: SimplM a -> (a -> SimplM b) -> SimplM b
thenSmpl_ :: SimplM a -> SimplM b -> SimplM b

thenSmpl m k
  = SM $ \st_env us0 sc0 -> do
      (m_result, us1, sc1) <- unSM m st_env us0 sc0
      unSM (k m_result) st_env us1 sc1

thenSmpl_ m k
  = SM $ \st_env us0 sc0 -> do
      (_, us1, sc1) <- unSM m st_env us0 sc0
      unSM k st_env us1 sc1

-- TODO: this specializing is not allowed
-- {-# SPECIALIZE mapM         :: (a -> SimplM b) -> [a] -> SimplM [b] #-}
-- {-# SPECIALIZE mapAndUnzipM :: (a -> SimplM (b, c)) -> [a] -> SimplM ([b],[c]) #-}
-- {-# SPECIALIZE mapAccumLM   :: (acc -> b -> SimplM (acc,c)) -> acc -> [b] -> SimplM (acc, [c]) #-}

traceSmpl :: String -> SDoc -> SimplM ()
traceSmpl herald doc
  = do { dflags <- getDynFlags
       ; liftIO $ Err.dumpIfSet_dyn dflags Opt_D_dump_simpl_trace "Simpl Trace"
           (hang (text herald) 2 doc) }

{-
************************************************************************
*                                                                      *
\subsection{The unique supply}
*                                                                      *
************************************************************************
-}

instance MonadUnique SimplM where
    getUniqueSupplyM
       = SM (\_st_env us sc -> case splitUniqSupply us of
                                (us1, us2) -> return (us1, us2, sc))

    getUniqueM
       = SM (\_st_env us sc -> case takeUniqFromSupply us of
                                (u, us') -> return (u, us', sc))

    getUniquesM
        = SM (\_st_env us sc -> case splitUniqSupply us of
                                (us1, us2) -> return (uniqsFromSupply us1, us2, sc))

instance HasDynFlags SimplM where
    getDynFlags = SM (\st_env us sc -> return (st_flags st_env, us, sc))

instance MonadIO SimplM where
    liftIO m = SM $ \_ us sc -> do
      x <- m
      return (x, us, sc)

getSimplRules :: SimplM RuleEnv
getSimplRules = SM (\st_env us sc -> return (st_rules st_env, us, sc))

getFamEnvs :: SimplM (FamInstEnv, FamInstEnv)
getFamEnvs = SM (\st_env us sc -> return (st_fams st_env, us, sc))

newId :: FastString -> Type -> SimplM Id
newId fs ty = do uniq <- getUniqueM
                 return (mkSysLocalOrCoVar fs uniq ty)

newJoinId :: [Var] -> Type -> SimplM Id
newJoinId bndrs body_ty
  = do { uniq <- getUniqueM
       ; let name       = mkSystemVarName uniq (fsLit "$j")
             join_id_ty = mkLamTypes bndrs body_ty  -- Note [Funky mkLamTypes]
             arity      = length (filter (not . isTyVar) bndrs)
             join_arity = length bndrs
             details    = JoinId join_arity
             id_info    = vanillaIdInfo `setArityInfo` arity
--                                        `setOccInfo` strongLoopBreaker

       ; return (mkLocalVar details name join_id_ty id_info) }

{-
************************************************************************
*                                                                      *
\subsection{Counting up what we've done}
*                                                                      *
************************************************************************
-}

getSimplCount :: SimplM SimplCount
getSimplCount = SM (\_st_env us sc -> return (sc, us, sc))

tick :: Tick -> SimplM ()
tick t = SM (\st_env us sc -> let sc' = doSimplTick (st_flags st_env) t sc
                              in sc' `seq` return ((), us, sc'))

checkedTick :: Tick -> SimplM ()
-- Try to take a tick, but fail if too many
checkedTick t
  = SM (\st_env us sc ->
           if st_max_ticks st_env <= mkIntWithInf (simplCountN sc)
           then throwGhcExceptionIO $
                  PprProgramError "Simplifier ticks exhausted" (msg sc)
           else let sc' = doSimplTick (st_flags st_env) t sc
                in sc' `seq` return ((), us, sc'))
  where
    msg sc = vcat
      [ text "When trying" <+> ppr t
      , text "To increase the limit, use -fsimpl-tick-factor=N (default 100)."
      , space
      , text "If you need to increase the limit substantially, please file a"
      , text "bug report and indicate the factor you needed."
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
   = SM (\_st_env us sc -> let sc' = doFreeSimplTick t sc
                           in sc' `seq` return ((), us, sc'))
