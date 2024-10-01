{-# LANGUAGE FlexibleInstances, DeriveFunctor, DerivingVia, CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-----------------------------------------------------------------------------
--
-- Monadery code used in InteractiveUI
--
-- (c) The GHC Team 2005-2006
--
-----------------------------------------------------------------------------

module GHCi.UI.Monad (
        GHCi(..), startGHCi,
        GHCiState(..), GhciMonad(..),
        GHCiOption(..), isOptionSet, setOption, unsetOption,
        Command(..), CommandResult(..), cmdSuccess,
        CmdExecOutcome(..),
        LocalConfigBehaviour(..),
        PromptFunction,
        BreakLocation(..),
        TickArray,
        extractDynFlags, getDynFlags,

        runStmt, runDecls, runDecls', resume, recordBreak, revertCAFs,
        ActionStats(..), runAndPrintStats, runWithStats, printStats,

        printForUserNeverQualify,
        printForUserGlobalRdrEnv,
        printForUser, printForUserPartWay, prettyLocations,

        compileGHCiExpr,
        initInterpBuffering,
        turnOffBuffering, turnOffBuffering_,
        flushInterpBuffers,
        runInternal,
        mkEvalWrapper
    ) where

import GHCi.UI.Info (ModInfo)
import qualified GHC
import GHC.Driver.Monad hiding (liftIO)
import GHC.Utils.Outputable
import qualified GHC.Driver.Ppr as Ppr
import GHC.Types.Name.Occurrence
import GHC.Types.Name.Reader
import GHC.Driver.Session
import GHC.Data.FastString
import GHC.Driver.Env
import GHC.Types.SrcLoc
import GHC.Types.SafeHaskell
import GHC.Driver.Make (ModIfaceCache(..))
import GHC.Unit
import GHC.Types.Name.Reader as RdrName (mkOrig)
import qualified GHC.Types.Name.Ppr as Ppr (mkNamePprCtx)
import GHC.Builtin.Names (gHC_INTERNAL_GHCI_HELPERS)
import GHC.Runtime.Interpreter
import GHC.Runtime.Context
import GHCi.RemoteTypes
import GHCi.UI.Exception (printGhciException)
import GHC.Hs (ImportDecl, GhcPs, GhciLStmt, LHsDecl)
import GHC.Hs.Utils
import GHC.Utils.Misc
import GHC.Utils.Logger

import GHC.Utils.Exception hiding (uninterruptibleMask, mask, catch)
import Numeric
import Data.Array
import Data.IORef
import Data.Time
import System.Environment
import System.IO
import Control.Monad
import Prelude hiding ((<>))

import System.Console.Haskeline (CompletionFunc, InputT)
import Control.Monad.Catch as MC
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import Data.Map.Strict (Map)
import qualified Data.IntMap.Strict as IntMap
import qualified GHC.Data.EnumSet as EnumSet
import qualified GHC.LanguageExtensions as LangExt

-----------------------------------------------------------------------------
-- GHCi monad

data GHCiState = GHCiState
     {
        progname       :: String,
        args           :: [String],
        evalWrapper    :: ForeignHValue, -- ^ of type @IO a -> IO a@
        prompt         :: PromptFunction,
        prompt_cont    :: PromptFunction,
        editor         :: String,
        stop           :: String,
        multiMode      :: Bool,
        localConfig    :: LocalConfigBehaviour,
        options        :: [GHCiOption],
        line_number    :: !Int,         -- ^ input line
        break_ctr      :: !Int,
        breaks         :: !(IntMap.IntMap BreakLocation),
        tickarrays     :: ModuleEnv TickArray,
            -- ^ 'tickarrays' caches the 'TickArray' for loaded modules,
            -- so that we don't rebuild it each time the user sets
            -- a breakpoint.
        ghci_commands  :: [Command],
            -- ^ available ghci commands
        ghci_macros    :: [Command],
            -- ^ user-defined macros
        last_command   :: Maybe Command,
            -- ^ @:@ at the GHCi prompt repeats the last command, so we
            -- remember it here
        cmd_wrapper    :: InputT GHCi CommandResult -> InputT GHCi (Maybe Bool),
            -- ^ The command wrapper is run for each command or statement.
            -- The 'Bool' value denotes whether the command is successful and
            -- 'Nothing' means to exit GHCi.
        cmdqueue       :: [String],

        remembered_ctx :: [InteractiveImport],
            -- ^ The imports that the user has asked for, via import
            -- declarations and :module commands.  This list is
            -- persistent over :reloads (but any imports for modules
            -- that are not loaded are temporarily ignored).  After a
            -- :load, all the home-package imports are stripped from
            -- this list.
            --
            -- See bugs #2049, #1873, #1360

        transient_ctx  :: [InteractiveImport],
            -- ^ An import added automatically after a :load, usually of
            -- the most recently compiled module.  May be empty if
            -- there are no modules loaded.  This list is replaced by
            -- :load, :reload, and :add.  In between it may be modified
            -- by :module.

        extra_imports  :: [ImportDecl GhcPs],
            -- ^ These are "always-on" imports, added to the
            -- context regardless of what other imports we have.
            -- This is useful for adding imports that are required
            -- by setGHCiMonad.  Be careful adding things here:
            -- you can create ambiguities if these imports overlap
            -- with other things in scope.
            --
            -- NB. although this is not currently used by GHCi itself,
            -- it was added to support other front-ends that are based
            -- on the GHCi code.  Potentially we could also expose
            -- this functionality via GHCi commands.

        prelude_imports :: [ImportDecl GhcPs],
            -- ^ These imports are added to the context when
            -- -XImplicitPrelude is on and we don't have a *-module
            -- in the context.  They can also be overridden by another
            -- import for the same module, e.g.
            -- "import Prelude hiding (map)"

        ghc_e :: Bool, -- ^ True if this is 'ghc -e' (or runghc)

        short_help :: String,
            -- ^ help text to display to a user
        long_help  :: String,
        lastErrorLocations :: IORef [(FastString, Int)],

        mod_infos  :: !(Map ModuleName ModInfo),

        flushStdHandles :: ForeignHValue,
            -- ^ @hFlush stdout; hFlush stderr@ in the interpreter
        noBuffering :: ForeignHValue,
            -- ^ @hSetBuffering NoBuffering@ for stdin/stdout/stderr
        ifaceCache :: ModIfaceCache
     }

type TickArray = Array Int [(GHC.BreakIndex,RealSrcSpan)]

-- | A GHCi command
data Command
   = Command
   { cmdName           :: String
     -- ^ Name of GHCi command (e.g. "exit")
   , cmdAction         :: String -> InputT GHCi CmdExecOutcome
     -- ^ The 'CmdExecOutcome' value denotes whether to exit GHCi cleanly or error out
   , cmdHidden         :: Bool
     -- ^ Commands which are excluded from default completion
     -- and @:help@ summary. This is usually set for commands not
     -- useful for interactive use but rather for IDEs.
   , cmdCompletionFunc :: CompletionFunc GHCi
     -- ^ 'CompletionFunc' for arguments
   }

-- | Used to denote GHCi command execution result. Specifically, used to
-- distinguish between two ghci execution modes - "REPL" and "Expression
-- evaluation mode (ghc -e)". When in "REPL" mode, we don't want to exit
-- GHCi session when error occurs, (which is when we use "CmdSuccess").
-- Otherwise, when in expression evaluation mode, all command failures
-- should lead to GHCi session termination (with ExitFailure 1) which is
-- when "CmdFailure" is used(this is useful when executing scripts).
-- "CleanExit" is used to signal end of GHCi session (for example, when
-- ":quit" command is called).
data CmdExecOutcome
  = CleanExit
  | CmdSuccess
  | CmdFailure

data CommandResult
   = CommandComplete
   { cmdInput :: String
   , cmdResult :: Either SomeException (Maybe Bool)
   , cmdStats :: ActionStats
   }
   | CommandIncomplete
     -- ^ Unterminated multiline command
   deriving Show

cmdSuccess :: MonadThrow m => CommandResult -> m (Maybe Bool)
cmdSuccess CommandComplete{ cmdResult = Left e } =
  {- Don't add a backtrace from ghci/ghc to the exception from the user program! -}
#if MIN_VERSION_base(4,21,0)
  throwM (NoBacktrace e)
#else
  -- NoBacktrace is not available in older compilers
  throwM e
#endif
cmdSuccess CommandComplete{ cmdResult = Right r } = return r
cmdSuccess CommandIncomplete = return $ Just True

type PromptFunction = [String]
                   -> Int
                   -> GHCi SDoc

data GHCiOption
        = ShowTiming            -- show time/allocs after evaluation
        | ShowType              -- show the type of expressions
        | RevertCAFs            -- revert CAFs after every evaluation
        | Multiline             -- use multiline commands
        | CollectInfo           -- collect and cache information about
                                -- modules after load
        deriving Eq

-- | Treatment of ./.ghci files.  For now we either load or
-- ignore.  But later we could implement a "safe mode" where
-- only safe operations are performed.
--
data LocalConfigBehaviour
  = SourceLocalConfig
  | IgnoreLocalConfig
  deriving (Eq)

data BreakLocation
   = BreakLocation
   { breakModule :: !GHC.Module
   , breakLoc    :: !SrcSpan
   , breakTick   :: {-# UNPACK #-} !Int
   , breakEnabled:: !Bool
   , onBreakCmd  :: String
   }

instance Eq BreakLocation where
  loc1 == loc2 = breakModule loc1 == breakModule loc2 &&
                 breakTick loc1   == breakTick loc2

prettyLocations :: IntMap.IntMap BreakLocation -> SDoc
prettyLocations  locs =
    case  IntMap.null locs of
      True  -> text "No active breakpoints."
      False -> vcat $ map (\(i, loc) -> brackets (int i) <+> ppr loc) $ IntMap.toAscList locs

instance Outputable BreakLocation where
   ppr loc = (ppr $ breakModule loc) <+> ppr (breakLoc loc) <+> pprEnaDisa <+>
                if null (onBreakCmd loc)
                   then empty
                   else doubleQuotes (text (onBreakCmd loc))
      where pprEnaDisa = case breakEnabled loc of
                True  -> text "enabled"
                False -> text "disabled"

recordBreak
  :: GhciMonad m => BreakLocation -> m (Bool{- was already present -}, Int)
recordBreak brkLoc = do
   st <- getGHCiState
   let oldmap = breaks st
       oldActiveBreaks = IntMap.assocs oldmap
   -- don't store the same break point twice
   case [ nm | (nm, loc) <- oldActiveBreaks, loc == brkLoc ] of
     (nm:_) -> return (True, nm)
     [] -> do
      let oldCounter = break_ctr st
          newCounter = oldCounter + 1
      setGHCiState $ st { break_ctr = newCounter,
                          breaks = IntMap.insert oldCounter brkLoc oldmap
                        }
      return (False, oldCounter)

newtype GHCi a = GHCi { unGHCi :: IORef GHCiState -> Ghc a }
    deriving (Functor)
    deriving (MonadThrow, MonadCatch, MonadMask) via (ReaderT (IORef GHCiState) Ghc)

reflectGHCi :: (Session, IORef GHCiState) -> GHCi a -> IO a
reflectGHCi (s, gs) m = unGhc (unGHCi m gs) s

startGHCi :: GHCi a -> GHCiState -> Ghc a
startGHCi g state = do ref <- liftIO $ newIORef state; unGHCi g ref

instance Applicative GHCi where
    pure a = GHCi $ \_ -> pure a
    (<*>) = ap

instance Monad GHCi where
  (GHCi m) >>= k  =  GHCi $ \s -> m s >>= \a -> unGHCi (k a) s

class GhcMonad m => GhciMonad m where
  getGHCiState    :: m GHCiState
  setGHCiState    :: GHCiState -> m ()
  modifyGHCiState :: (GHCiState -> GHCiState) -> m ()
  reifyGHCi       :: ((Session, IORef GHCiState) -> IO a) -> m a

instance GhciMonad GHCi where
  getGHCiState      = GHCi $ \r -> liftIO $ readIORef r
  setGHCiState s    = GHCi $ \r -> liftIO $ writeIORef r s
  modifyGHCiState f = GHCi $ \r -> liftIO $ modifyIORef' r f
  reifyGHCi f       = GHCi $ \r -> reifyGhc $ \s -> f (s, r)

instance GhciMonad (InputT GHCi) where
  getGHCiState    = lift getGHCiState
  setGHCiState    = lift . setGHCiState
  modifyGHCiState = lift . modifyGHCiState
  reifyGHCi       = lift . reifyGHCi

liftGhc :: Ghc a -> GHCi a
liftGhc m = GHCi $ \_ -> m

instance MonadIO GHCi where
  liftIO = liftGhc . liftIO

instance HasDynFlags GHCi where
  getDynFlags = getSessionDynFlags

instance HasLogger GHCi where
  getLogger = hsc_logger <$> getSession

instance GhcMonad GHCi where
  setSession s' = liftGhc $ setSession s'
  getSession    = liftGhc $ getSession


instance HasDynFlags (InputT GHCi) where
  getDynFlags = lift getDynFlags

instance HasLogger (InputT GHCi) where
  getLogger = lift getLogger

instance GhcMonad (InputT GHCi) where
  setSession = lift . setSession
  getSession = lift getSession

isOptionSet :: GhciMonad m => GHCiOption -> m Bool
isOptionSet opt
 = do st <- getGHCiState
      return $! (opt `elem` options st)

setOption :: GhciMonad m => GHCiOption -> m ()
setOption opt
 = do st <- getGHCiState
      setGHCiState (st{ options = opt : filter (/= opt) (options st) })

unsetOption :: GhciMonad m => GHCiOption -> m ()
unsetOption opt
 = do st <- getGHCiState
      setGHCiState (st{ options = filter (/= opt) (options st) })

printForUserNeverQualify :: GhcMonad m => SDoc -> m ()
printForUserNeverQualify doc = do
  dflags <- GHC.getInteractiveDynFlags
  liftIO $ Ppr.printForUser dflags stdout neverQualify AllTheWay doc

printForUserGlobalRdrEnv :: (GhcMonad m, Outputable info)
                         => Maybe (GlobalRdrEnvX info) -> SDoc -> m ()
printForUserGlobalRdrEnv mb_rdr_env doc = do
  dflags <- GHC.getInteractiveDynFlags
  name_ppr_ctx <- mkNamePprCtxFromGlobalRdrEnv dflags mb_rdr_env
  liftIO $ Ppr.printForUser dflags stdout name_ppr_ctx AllTheWay doc
    where
      mkNamePprCtxFromGlobalRdrEnv _ Nothing = GHC.getNamePprCtx
      mkNamePprCtxFromGlobalRdrEnv dflags (Just rdr_env) =
        withSession $ \ hsc_env ->
        let unit_env = hsc_unit_env hsc_env
            ptc = initPromotionTickContext dflags
        in  return $ Ppr.mkNamePprCtx ptc unit_env rdr_env

printForUser :: GhcMonad m => SDoc -> m ()
printForUser doc = do
  name_ppr_ctx <- GHC.getNamePprCtx
  dflags <- GHC.getInteractiveDynFlags
  liftIO $ Ppr.printForUser dflags stdout name_ppr_ctx AllTheWay doc

printForUserPartWay :: GhcMonad m => SDoc -> m ()
printForUserPartWay doc = do
  name_ppr_ctx <- GHC.getNamePprCtx
  dflags <- GHC.getInteractiveDynFlags
  liftIO $ Ppr.printForUser dflags stdout name_ppr_ctx DefaultDepth doc

-- | Run a single Haskell expression
runStmt
  :: GhciMonad m
  => GhciLStmt GhcPs -> String -> GHC.SingleStep -> m (Maybe GHC.ExecResult)
runStmt stmt stmt_text step = do
  st <- getGHCiState
  GHC.handleSourceError (\e -> do printGhciException e; return Nothing) $ do
    let opts = GHC.execOptions
                  { GHC.execSourceFile = progname st
                  , GHC.execLineNumber = line_number st
                  , GHC.execSingleStep = step
                  , GHC.execWrap = \fhv -> EvalApp (EvalThis (evalWrapper st))
                                                   (EvalThis fhv) }
    Just <$> GHC.execStmt' stmt stmt_text opts

runDecls :: GhciMonad m => String -> m (Maybe [GHC.Name])
runDecls decls = do
  st <- getGHCiState
  reifyGHCi $ \x ->
    withProgName (progname st) $
    withArgs (args st) $
      reflectGHCi x $ do
        GHC.handleSourceError (\e -> do printGhciException e
                                        return Nothing) $ do
          r <- GHC.runDeclsWithLocation (progname st) (line_number st) decls
          return (Just r)

runDecls' :: GhciMonad m => [LHsDecl GhcPs] -> m (Maybe [GHC.Name])
runDecls' decls = do
  st <- getGHCiState
  reifyGHCi $ \x ->
    withProgName (progname st) $
    withArgs (args st) $
    reflectGHCi x $
      GHC.handleSourceError
        (\e -> do printGhciException e
                  return Nothing)
        (Just <$> GHC.runParsedDecls decls)

resume :: GhciMonad m => (SrcSpan -> Bool) -> GHC.SingleStep -> Maybe Int -> m GHC.ExecResult
resume canLogSpan step mbIgnoreCnt = do
  st <- getGHCiState
  reifyGHCi $ \x ->
    withProgName (progname st) $
    withArgs (args st) $
      reflectGHCi x $ do
        GHC.resumeExec canLogSpan step mbIgnoreCnt

-- --------------------------------------------------------------------------
-- timing & statistics

data ActionStats = ActionStats
  { actionAllocs :: Maybe Integer
  , actionElapsedTime :: Double
  } deriving Show

runAndPrintStats
  :: GhciMonad m
  => (a -> Maybe Integer)
  -> m a
  -> m (ActionStats, Either SomeException a)
runAndPrintStats getAllocs action = do
  result <- runWithStats getAllocs action
  case result of
    (stats, Right{}) -> do
      showTiming <- isOptionSet ShowTiming
      when showTiming $ do
        dflags  <- getDynFlags
        liftIO $ printStats dflags stats
    _ -> return ()
  return result

runWithStats
  :: ExceptionMonad m
  => (a -> Maybe Integer) -> m a -> m (ActionStats, Either SomeException a)
runWithStats getAllocs action = do
  t0 <- liftIO getCurrentTime
  result <- MC.try action
  let allocs = either (const Nothing) getAllocs result
  t1 <- liftIO getCurrentTime
  let elapsedTime = realToFrac $ t1 `diffUTCTime` t0
  return (ActionStats allocs elapsedTime, result)

printStats :: DynFlags -> ActionStats -> IO ()
printStats dflags ActionStats{actionAllocs = mallocs, actionElapsedTime = secs}
   = do let secs_str = showFFloat (Just 2) secs
        putStrLn (Ppr.showSDoc dflags (
                 parens (text (secs_str "") <+> text "secs" <> comma <+>
                         case mallocs of
                           Nothing -> empty
                           Just allocs ->
                             text (separateThousands allocs) <+> text "bytes")))
  where
    separateThousands n = reverse . separate . reverse . show $ n
      where separate n'
              | n' `lengthAtMost` 3 = n'
              | otherwise           = take 3 n' ++ "," ++ separate (drop 3 n')

-----------------------------------------------------------------------------
-- reverting CAFs

revertCAFs :: GhciMonad m => m ()
revertCAFs = do
  interp <- hscInterp <$> GHC.getSession
  liftIO $ interpCmd interp RtsRevertCAFs
  s <- getGHCiState
  when (not (ghc_e s)) turnOffBuffering
     -- Have to turn off buffering again, because we just
     -- reverted stdout, stderr & stdin to their defaults.


-----------------------------------------------------------------------------
-- To flush buffers for the *interpreted* computation we need
-- to refer to *its* stdout/stderr handles

-- | Compile "hFlush stdout; hFlush stderr" once, so we can use it repeatedly
initInterpBuffering :: Ghc (ForeignHValue, ForeignHValue)
initInterpBuffering = do
  let mkHelperExpr :: OccName -> Ghc ForeignHValue
      mkHelperExpr occ =
        GHC.compileParsedExprRemote
        $ GHC.nlHsVar $ RdrName.mkOrig gHC_INTERNAL_GHCI_HELPERS occ
  nobuf <- mkHelperExpr $ mkVarOccFS (fsLit "disableBuffering")
  flush <- mkHelperExpr $ mkVarOccFS (fsLit "flushAll")
  return (nobuf, flush)

-- | Invoke "hFlush stdout; hFlush stderr" in the interpreter
flushInterpBuffers :: GhciMonad m => m ()
flushInterpBuffers = do
  st <- getGHCiState
  interp <- hscInterp <$> GHC.getSession
  liftIO $ evalIO interp (flushStdHandles st)

-- | Turn off buffering for stdin, stdout, and stderr in the interpreter
turnOffBuffering :: GhciMonad m => m ()
turnOffBuffering = do
  st <- getGHCiState
  turnOffBuffering_ (noBuffering st)

turnOffBuffering_ :: GhcMonad m => ForeignHValue -> m ()
turnOffBuffering_ fhv = do
  interp <- hscInterp <$> getSession
  liftIO $ evalIO interp fhv

mkEvalWrapper :: GhcMonad m => String -> [String] ->  m ForeignHValue
mkEvalWrapper progname' args' =
  runInternal $ GHC.compileParsedExprRemote
  $ evalWrapper' `GHC.mkHsApp` nlHsString progname'
                 `GHC.mkHsApp` nlList (map nlHsString args')
  where
    nlHsString = nlHsLit . mkHsString
    evalWrapper' =
      GHC.nlHsVar $ RdrName.mkOrig gHC_INTERNAL_GHCI_HELPERS (mkVarOccFS (fsLit "evalWrapper"))

-- | Run a 'GhcMonad' action to compile an expression for internal usage.
runInternal :: GhcMonad m => m a -> m a
runInternal =
    withTempSession mkTempSession
  where
    mkTempSession = hscUpdateFlags (\dflags -> dflags
      { -- Running GHCi's internal expression is incompatible with -XSafe.
          -- We temporarily disable any Safe Haskell settings while running
          -- GHCi internal expressions. (see #12509)
        safeHaskell = Sf_None,
          -- Disable dumping of any data during evaluation of GHCi's internal
          -- expressions. (#17500)
        dumpFlags = EnumSet.empty
      }
        -- RebindableSyntax can wreak havoc with GHCi in several ways
          -- (see #13385 and #14342 for examples), so we temporarily
          -- disable it too.
          `xopt_unset` LangExt.RebindableSyntax
          -- We heavily depend on -fimplicit-import-qualified to compile expr
          -- with fully qualified names without imports.
          `gopt_set` Opt_ImplicitImportQualified
      )

compileGHCiExpr :: GhcMonad m => String -> m ForeignHValue
compileGHCiExpr expr = runInternal $ GHC.compileExprRemote expr
