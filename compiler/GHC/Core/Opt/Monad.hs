{-
(c) The AQUA Project, Glasgow University, 1993-1998

-}


{-# LANGUAGE DeriveFunctor #-}

module GHC.Core.Opt.Monad (
    -- * Types used in core-to-core passes
    FloatOutSwitches(..),

    -- * The monad
    CoreM, runCoreM,

    mapDynFlagsCoreM, dropSimplCount,

    -- ** Reading from the monad
    getHscEnv, getModule,
    initRuleEnv, getExternalRuleBase,
    getDynFlags, getPackageFamInstEnv,
    getInteractiveContext,
    getUniqTag,
    getNamePprCtx, getSrcSpanM,

    -- ** Writing to the monad
    addSimplCount,

    -- ** Lifting into the monad
    liftIO, liftIOWithCount,

    -- ** Dealing with annotations
    getAnnotations, getFirstAnnotations,

    -- ** Screen output
    putMsg, putMsgS, errorMsg, msg,
    fatalErrorMsg, fatalErrorMsgS,
    debugTraceMsg, debugTraceMsgS,
  ) where

import GHC.Prelude hiding ( read )

import GHC.Driver.DynFlags
import GHC.Driver.Env

import GHC.Core.Rules     ( RuleBase, RuleEnv, mkRuleEnv, extendRuleBaseList )
import GHC.Core.Opt.Stats ( SimplCount, zeroSimplCount, plusSimplCount )

import GHC.Types.Annotations
import GHC.Types.Unique.Supply
import GHC.Types.Name.Env
import GHC.Types.SrcLoc
import GHC.Types.Error

import GHC.Utils.Error ( errorDiagnostic )
import GHC.Utils.Outputable as Outputable
import GHC.Utils.Logger
import GHC.Utils.Monad

import GHC.Data.IOEnv hiding     ( liftIO, failM, failWithM )
import qualified GHC.Data.IOEnv  as IOEnv

import GHC.Runtime.Context ( InteractiveContext )

import GHC.Unit.Module
import GHC.Unit.Module.ModGuts
import GHC.Unit.External

import Data.Bifunctor ( bimap )
import Data.Dynamic
import Data.Maybe (listToMaybe)
import Data.Word
import Control.Monad
import Control.Applicative ( Alternative(..) )
import qualified GHC.Unit.Home.Graph as HUG

data FloatOutSwitches = FloatOutSwitches
  { floatOutLambdas   :: Maybe Int  -- ^ Just n <=> float lambdas to top level, if
                                    -- doing so will abstract over n or fewer
                                    -- value variables
                                    -- Nothing <=> float all lambdas to top level,
                                    --             regardless of how many free variables
                                    -- Just 0 is the vanilla case: float a lambda
                                    --    iff it has no free vars

  , floatOutConstants :: Bool       -- ^ True <=> float constants to top level,
                                    --            even if they do not escape a lambda

  , floatOutOverSatApps :: Bool     -- ^ True <=> float out over-saturated applications
                                    --            based on arity information.
                                    -- See Note [Floating over-saturated applications]
                                    -- in GHC.Core.Opt.SetLevels
  , floatToTopLevelOnly :: Bool     -- ^ Allow floating to the top level only.

  , floatJoinsToTop :: Bool         -- ^ Float join points to top level if possible
                                    -- See Note [Floating join point bindings]
                                    --     in GHC.Core.Opt.SetLevels
  }
instance Outputable FloatOutSwitches where
    ppr = pprFloatOutSwitches

pprFloatOutSwitches :: FloatOutSwitches -> SDoc
pprFloatOutSwitches sw
  = text "FOS" <+> (braces $
     sep $ punctuate comma $
     [ text "Lam ="    <+> ppr (floatOutLambdas sw)
     , text "Consts =" <+> ppr (floatOutConstants sw)
     , text "JoinsToTop =" <+> ppr (floatJoinsToTop sw)
     , text "OverSatApps ="   <+> ppr (floatOutOverSatApps sw) ])

{-
************************************************************************
*                                                                      *
             Monad and carried data structure definitions
*                                                                      *
************************************************************************
-}

data CoreReader = CoreReader {
        cr_hsc_env             :: HscEnv,
        cr_rule_base           :: RuleBase,  -- Home package table rules
        cr_module              :: Module,
        cr_name_ppr_ctx        :: NamePprCtx,
        cr_loc                 :: SrcSpan,   -- Use this for log/error messages so they
                                             -- are at least tagged with the right source file
        cr_uniq_tag            :: !Char      -- Tag for creating unique values
}

-- Note: CoreWriter used to be defined with data, rather than newtype.  If it
-- is defined that way again, the cw_simpl_count field, at least, must be
-- strict to avoid a space leak (#7702).
newtype CoreWriter = CoreWriter {
        cw_simpl_count :: SimplCount
}

emptyWriter :: Bool -- ^ -ddump-simpl-stats
            -> CoreWriter
emptyWriter dump_simpl_stats = CoreWriter {
        cw_simpl_count = zeroSimplCount dump_simpl_stats
    }

plusWriter :: CoreWriter -> CoreWriter -> CoreWriter
plusWriter w1 w2 = CoreWriter {
        cw_simpl_count = (cw_simpl_count w1) `plusSimplCount` (cw_simpl_count w2)
    }

type CoreIOEnv = IOEnv CoreReader

-- | The monad used by Core-to-Core passes to register simplification statistics.
--  Also used to have common state (in the form of UniqueSupply) for generating Uniques.
newtype CoreM a = CoreM { unCoreM :: CoreIOEnv (a, CoreWriter) }
    deriving (Functor)

instance Monad CoreM where
    mx >>= f = CoreM $ do
            (x, w1) <- unCoreM mx
            (y, w2) <- unCoreM (f x)
            let w = w1 `plusWriter` w2
            return $ seq w (y, w)
            -- forcing w before building the tuple avoids a space leak
            -- (#7702)

instance Applicative CoreM where
    pure x = CoreM $ nop x
    (<*>) = ap
    m *> k = m >>= \_ -> k

instance Alternative CoreM where
    empty   = CoreM Control.Applicative.empty
    m <|> n = CoreM (unCoreM m <|> unCoreM n)

instance MonadPlus CoreM

instance MonadUnique CoreM where
    getUniqueSupplyM = do
        tag <- read cr_uniq_tag
        liftIO $! mkSplitUniqSupply tag

    getUniqueM = do
        tag <- read cr_uniq_tag
        liftIO $! uniqFromTag tag

runCoreM :: HscEnv
         -> RuleBase
         -> Char -- ^ Mask
         -> Module
         -> NamePprCtx
         -> SrcSpan
         -> CoreM a
         -> IO (a, SimplCount)
runCoreM hsc_env rule_base tag mod name_ppr_ctx loc m
  = liftM extract $ runIOEnv reader $ unCoreM m
  where
    reader = CoreReader {
            cr_hsc_env = hsc_env,
            cr_rule_base = rule_base,
            cr_module = mod,
            cr_name_ppr_ctx = name_ppr_ctx,
            cr_loc = loc,
            cr_uniq_tag = tag
        }

    extract :: (a, CoreWriter) -> (a, SimplCount)
    extract (value, writer) = (value, cw_simpl_count writer)

{-
************************************************************************
*                                                                      *
             Core combinators, not exported
*                                                                      *
************************************************************************
-}

nop :: a -> CoreIOEnv (a, CoreWriter)
nop x = do
    logger <- hsc_logger . cr_hsc_env <$> getEnv
    return (x, emptyWriter $ logHasDumpFlag logger Opt_D_dump_simpl_stats)

read :: (CoreReader -> a) -> CoreM a
read f = CoreM $ getEnv >>= (\r -> nop (f r))

write :: CoreWriter -> CoreM ()
write w = CoreM $ return ((), w)

-- \subsection{Lifting IO into the monad}

-- | Lift an 'IOEnv' operation into 'CoreM'
liftIOEnv :: CoreIOEnv a -> CoreM a
liftIOEnv mx = CoreM (mx >>= (\x -> nop x))

instance MonadIO CoreM where
    liftIO = liftIOEnv . IOEnv.liftIO

-- | Lift an 'IO' operation into 'CoreM' while consuming its 'SimplCount'
liftIOWithCount :: IO (SimplCount, a) -> CoreM a
liftIOWithCount what = liftIO what >>= (\(count, x) -> addSimplCount count >> return x)

{-
************************************************************************
*                                                                      *
             Reader, writer and state accessors
*                                                                      *
************************************************************************
-}

getHscEnv :: CoreM HscEnv
getHscEnv = read cr_hsc_env

getHomeRuleBase :: CoreM RuleBase
getHomeRuleBase = read cr_rule_base

initRuleEnv :: ModGuts -> CoreM RuleEnv
initRuleEnv guts
  = do { hpt_rules <- getHomeRuleBase
       ; eps_rules <- getExternalRuleBase
       ; return (mkRuleEnv guts eps_rules hpt_rules) }

getExternalRuleBase :: CoreM RuleBase
getExternalRuleBase = do
  eps_rules <- eps_rule_base <$> get_eps
  hug <- hsc_HUG <$> getHscEnv

  dflags <- getDynFlags
  hpt_rules <- liftIO $ if (isOneShot (ghcMode dflags)) then HUG.allRules hug else return []
  let final = extendRuleBaseList eps_rules hpt_rules
  return final




getNamePprCtx :: CoreM NamePprCtx
getNamePprCtx = read cr_name_ppr_ctx

getSrcSpanM :: CoreM SrcSpan
getSrcSpanM = read cr_loc

addSimplCount :: SimplCount -> CoreM ()
addSimplCount count = write (CoreWriter { cw_simpl_count = count })

getUniqTag :: CoreM Char
getUniqTag = read cr_uniq_tag

-- Convenience accessors for useful fields of HscEnv

-- | Adjust the dyn flags passed to the argument action
mapDynFlagsCoreM :: (DynFlags -> DynFlags) -> CoreM a -> CoreM a
mapDynFlagsCoreM f m = CoreM $ do
  !e <- getEnv
  let !e' = e { cr_hsc_env = hscUpdateFlags f $ cr_hsc_env e }
  liftIO $ runIOEnv e' $! unCoreM m

-- | Drop the single count of the argument action so it doesn't effect
-- the total.
dropSimplCount :: CoreM a -> CoreM a
dropSimplCount m = CoreM $ do
  (a, _) <- unCoreM m
  unCoreM $ pure a

instance HasDynFlags CoreM where
    getDynFlags = fmap hsc_dflags getHscEnv

instance HasLogger CoreM where
    getLogger = fmap hsc_logger getHscEnv

instance HasModule CoreM where
    getModule = read cr_module

getInteractiveContext :: CoreM InteractiveContext
getInteractiveContext = hsc_IC <$> getHscEnv

getPackageFamInstEnv :: CoreM PackageFamInstEnv
getPackageFamInstEnv = eps_fam_inst_env <$> get_eps

get_eps :: CoreM ExternalPackageState
get_eps = do
    hsc_env <- getHscEnv
    liftIO $ hscEPS hsc_env

{-
************************************************************************
*                                                                      *
             Dealing with annotations
*                                                                      *
************************************************************************
-}

-- | Get all annotations of a given type. This happens lazily, that is
-- no deserialization will take place until the [a] is actually demanded and
-- the [a] can also be empty (the UniqFM is not filtered).
--
-- This should be done once at the start of a Core-to-Core pass that uses
-- annotations.
--
-- See Note [Annotations]
getAnnotations :: Typeable a => ([Word8] -> a) -> ModGuts -> CoreM (ModuleEnv [a], NameEnv [a])
getAnnotations deserialize guts = do
     hsc_env <- getHscEnv
     ann_env <- liftIO $ prepareAnnotations hsc_env (Just guts)
     return (deserializeAnns deserialize ann_env)

-- | Get at most one annotation of a given type per annotatable item.
getFirstAnnotations :: Typeable a => ([Word8] -> a) -> ModGuts -> CoreM (ModuleEnv a, NameEnv a)
getFirstAnnotations deserialize guts
  = bimap mod name <$> getAnnotations deserialize guts
  where
    mod = mapMaybeModuleEnv (const listToMaybe)
    name = mapMaybeNameEnv listToMaybe

{-
Note [Annotations]
~~~~~~~~~~~~~~~~~~
A Core-to-Core pass that wants to make use of annotations calls
getAnnotations or getFirstAnnotations at the beginning to obtain a UniqFM with
annotations of a specific type. This produces all annotations from interface
files read so far. However, annotations from interface files read during the
pass will not be visible until getAnnotations is called again. This is similar
to how rules work and probably isn't too bad.

The current implementation could be optimised a bit: when looking up
annotations for a thing from the HomePackageTable, we could search directly in
the module where the thing is defined rather than building one UniqFM which
contains all annotations we know of. This would work because annotations can
only be given to things defined in the same module. However, since we would
only want to deserialise every annotation once, we would have to build a cache
for every module in the HTP. In the end, it's probably not worth it as long as
we aren't using annotations heavily.

************************************************************************
*                                                                      *
                Direct screen output
*                                                                      *
************************************************************************
-}

msg :: MessageClass -> SDoc -> CoreM ()
msg msg_class doc = do
    logger <- getLogger
    loc    <- getSrcSpanM
    name_ppr_ctx <- getNamePprCtx
    let sty = case msg_class of
                MCDiagnostic _ _ _ -> err_sty
                MCDump             -> dump_sty
                _                  -> user_sty
        err_sty  = mkErrStyle name_ppr_ctx
        user_sty = mkUserStyle name_ppr_ctx AllTheWay
        dump_sty = mkDumpStyle name_ppr_ctx
    liftIO $ logMsg logger msg_class loc (withPprStyle sty doc)

-- | Output a String message to the screen
putMsgS :: String -> CoreM ()
putMsgS = putMsg . text

-- | Output a message to the screen
putMsg :: SDoc -> CoreM ()
putMsg = msg MCInfo

-- | Output an error to the screen. Does not cause the compiler to die.
errorMsg :: SDoc -> CoreM ()
errorMsg doc = msg errorDiagnostic doc

-- | Output a fatal error to the screen. Does not cause the compiler to die.
fatalErrorMsgS :: String -> CoreM ()
fatalErrorMsgS = fatalErrorMsg . text

-- | Output a fatal error to the screen. Does not cause the compiler to die.
fatalErrorMsg :: SDoc -> CoreM ()
fatalErrorMsg = msg MCFatal

-- | Output a string debugging message at verbosity level of @-v@ or higher
debugTraceMsgS :: String -> CoreM ()
debugTraceMsgS = debugTraceMsg . text

-- | Outputs a debugging message at verbosity level of @-v@ or higher
debugTraceMsg :: SDoc -> CoreM ()
debugTraceMsg = msg MCDump
