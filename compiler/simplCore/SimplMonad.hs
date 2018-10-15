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
        plusSimplCount, isZeroSimplCount,
    ) where

import GhcPrelude

import Var              ( Var, isTyVar, mkLocalVar, VarMult(..) )
import Name             ( mkSystemVarName )
import Id               ( Id, mkSysLocalOrCoVar, idWeight )
import IdInfo           ( IdDetails(..), vanillaIdInfo, setArityInfo )
import Type             ( Type, mkLamTypes, Rig(..))
import Weight
import FamInstEnv       ( FamInstEnv )
import CoreSyn          ( RuleEnv(..) )
import UniqSupply
import DynFlags
import CoreMonad
import Outputable
import FastString
import MonadUtils
import ErrUtils
import Panic (throwGhcExceptionIO, GhcException (..))
import BasicTypes          ( IntWithInf, treatZeroAsInf, mkIntWithInf )
import Control.Monad       ( when, liftM, ap )

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
      (r, us2, sc2) <- unSM (k m_result) st_env us1 sc1
      return (r, us2, sc2)


thenSmpl_ m k
  = SM $ \st_env us0 sc0 -> do
      (_, us1, sc1) <- unSM m st_env us0 sc0
      (r, us2, sc2) <- unSM k st_env us1 sc1
      return (r, us2, sc2)

-- TODO: this specializing is not allowed
-- {-# SPECIALIZE mapM         :: (a -> SimplM b) -> [a] -> SimplM [b] #-}
-- {-# SPECIALIZE mapAndUnzipM :: (a -> SimplM (b, c)) -> [a] -> SimplM ([b],[c]) #-}
-- {-# SPECIALIZE mapAccumLM   :: (acc -> b -> SimplM (acc,c)) -> acc -> [b] -> SimplM (acc, [c]) #-}

traceSmpl :: String -> SDoc -> SimplM ()
traceSmpl herald doc
  = do { dflags <- getDynFlags
       ; when (dopt Opt_D_dump_simpl_trace dflags) $ liftIO $
         printOutputForUser dflags alwaysQualify $
         hang (text herald) 2 doc }

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

newId :: FastString -> Rig -> Type -> SimplM Id
newId fs w ty = do uniq <- getUniqueM
                   return (mkSysLocalOrCoVar fs uniq (Regular w) ty)

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

       ; -- pprTrace "newJoinId" (ppr name $$ ppr join_id_ty) $
          return (mkLocalVar details name (Regular Omega) join_id_ty id_info) } -- TODO: arnaud: I'm guessing this is used to create join points, in which case, the is really not the right multiplicity.

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
                              in sc' `seq` return ((),us, sc'))

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

{-
************************************************************************
*                                                                      *
Weights
*                                                                      *
************************************************************************
-}

{- Note [Scaling lets]
The simplifier creates let bindings under certain circumstances which
are then inserted later. These are returned in `SimplFloats`.

However, we have to be somewhat careful here when it comes to linearity
as if we create a floating binding x in the scrutinee position.

case_w (let x[1] = "Foo" in Qux x) of
  Qux _ -> "Bar"

then the let will end up outside the `case` if we perform KnownBranch or
the case of case optimisation.

let x[1] = "Foo"
in "Bar"

So we get a linearity failure as the one usage of x is eliminated.
However, because the ambient context is an Omega context, we know that
we will use the scrutinee Omega times and hence all bindings inside it
Omega times as well. The failure was that we created a [1] binding
whilst inside this context and it then escaped without being scaled.

We also have to be careful as if we have a [1] case

case_1 (let x[1] = "Foo" in Qux x) of
  Qux x -> x

then the binding maintains the correct linearity once it is floated rom
the case and KnownBranch is performed.

let x[1] = "Foo"
in x

The difficulty mainly comes from that we only discover this context
at a later point once we have rebuilt the contituation. So, whilst rebuilding
a continuation we keep track of how many case-of-case like opportunities
take place and hence how much we have to scale lets floated from the scrutinee.
This is achieved by adding a Writer like effect to the SimplM data type.
It seems to work in practice, at least for T12944 which originally highlighted
this problem.

Why do we do this scaling afterwards rather than when the binding is
created? It is possible the binding comes from a point deep inside the
expression. It wasn't clear to me that we know enough about the context
at the point we make the binding due to the SimplCont type. It might
be thread this information through to get it right at definition site.
For now, I leave warnings and this message to my future self.
-}

-- TODO: arnaud move note to a better place (Simplify, maybe?)
