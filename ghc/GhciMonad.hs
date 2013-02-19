{-# OPTIONS_GHC -fno-cse -fno-warn-orphans #-}
-- -fno-cse is needed for GLOBAL_VAR's to behave properly

-----------------------------------------------------------------------------
--
-- Monadery code used in InteractiveUI
--
-- (c) The GHC Team 2005-2006
--
-----------------------------------------------------------------------------

module GhciMonad (
        GHCi(..), startGHCi,
        GHCiState(..), setGHCiState, getGHCiState, modifyGHCiState,
        GHCiOption(..), isOptionSet, setOption, unsetOption,
        Command,
        BreakLocation(..),
        TickArray,
        getDynFlags,

        runStmt, runDecls, resume, timeIt, recordBreak, revertCAFs,

        printForUser, printForUserPartWay, prettyLocations,
        initInterpBuffering, turnOffBuffering, flushInterpBuffers,
    ) where

#include "HsVersions.h"

import qualified GHC
import GhcMonad         hiding (liftIO)
import Outputable       hiding (printForUser, printForUserPartWay)
import qualified Outputable
import Util
import DynFlags
import HscTypes
import SrcLoc
import Module
import ObjLink
import Linker

import Exception
import Numeric
import Data.Array
import Data.Int         ( Int64 )
import Data.IORef
import System.CPUTime
import System.Environment
import System.IO
import Control.Monad
import GHC.Exts

import System.Console.Haskeline (CompletionFunc, InputT)
import qualified System.Console.Haskeline as Haskeline
import Control.Monad.Trans.Class
import Control.Monad.IO.Class

-----------------------------------------------------------------------------
-- GHCi monad

type Command = (String, String -> InputT GHCi Bool, CompletionFunc GHCi)

data GHCiState = GHCiState
     {
        progname       :: String,
        args           :: [String],
        prompt         :: String,
        def_prompt     :: String,
        editor         :: String,
        stop           :: String,
        options        :: [GHCiOption],
        line_number    :: !Int,         -- input line
        break_ctr      :: !Int,
        breaks         :: ![(Int, BreakLocation)],
        tickarrays     :: ModuleEnv TickArray,
                -- tickarrays caches the TickArray for loaded modules,
                -- so that we don't rebuild it each time the user sets
                -- a breakpoint.
        -- available ghci commands
        ghci_commands  :: [Command],
        -- ":" at the GHCi prompt repeats the last command, so we
        -- remember is here:
        last_command   :: Maybe Command,
        cmdqueue       :: [String],

        remembered_ctx :: [InteractiveImport],
             -- the imports that the user has asked for, via import
             -- declarations and :module commands.  This list is
             -- persistent over :reloads (but any imports for modules
             -- that are not loaded are temporarily ignored).  After a
             -- :load, all the home-package imports are stripped from
             -- this list.

             -- See bugs #2049, #1873, #1360

        transient_ctx  :: [InteractiveImport],
             -- An import added automatically after a :load, usually of
             -- the most recently compiled module.  May be empty if
             -- there are no modules loaded.  This list is replaced by
             -- :load, :reload, and :add.  In between it may be modified
             -- by :module.

        ghc_e :: Bool, -- True if this is 'ghc -e' (or runghc)

        -- help text to display to a user
        short_help :: String,
        long_help  :: String
     }

type TickArray = Array Int [(BreakIndex,SrcSpan)]

data GHCiOption
        = ShowTiming            -- show time/allocs after evaluation
        | ShowType              -- show the type of expressions
        | RevertCAFs            -- revert CAFs after every evaluation
        | Multiline             -- use multiline commands
        deriving Eq

data BreakLocation
   = BreakLocation
   { breakModule :: !GHC.Module
   , breakLoc    :: !SrcSpan
   , breakTick   :: {-# UNPACK #-} !Int
   , onBreakCmd  :: String
   }

instance Eq BreakLocation where
  loc1 == loc2 = breakModule loc1 == breakModule loc2 &&
                 breakTick loc1   == breakTick loc2

prettyLocations :: [(Int, BreakLocation)] -> SDoc
prettyLocations []   = text "No active breakpoints."
prettyLocations locs = vcat $ map (\(i, loc) -> brackets (int i) <+> ppr loc) $ reverse $ locs

instance Outputable BreakLocation where
   ppr loc = (ppr $ breakModule loc) <+> ppr (breakLoc loc) <+>
                if null (onBreakCmd loc)
                   then empty
                   else doubleQuotes (text (onBreakCmd loc))

recordBreak :: BreakLocation -> GHCi (Bool{- was already present -}, Int)
recordBreak brkLoc = do
   st <- getGHCiState
   let oldActiveBreaks = breaks st
   -- don't store the same break point twice
   case [ nm | (nm, loc) <- oldActiveBreaks, loc == brkLoc ] of
     (nm:_) -> return (True, nm)
     [] -> do
      let oldCounter = break_ctr st
          newCounter = oldCounter + 1
      setGHCiState $ st { break_ctr = newCounter,
                          breaks = (oldCounter, brkLoc) : oldActiveBreaks
                        }
      return (False, oldCounter)

newtype GHCi a = GHCi { unGHCi :: IORef GHCiState -> Ghc a }

reflectGHCi :: (Session, IORef GHCiState) -> GHCi a -> IO a
reflectGHCi (s, gs) m = unGhc (unGHCi m gs) s

reifyGHCi :: ((Session, IORef GHCiState) -> IO a) -> GHCi a
reifyGHCi f = GHCi f'
  where
    -- f' :: IORef GHCiState -> Ghc a
    f' gs = reifyGhc (f'' gs)
    -- f'' :: IORef GHCiState -> Session -> IO a
    f'' gs s = f (s, gs)

startGHCi :: GHCi a -> GHCiState -> Ghc a
startGHCi g state = do ref <- liftIO $ newIORef state; unGHCi g ref

instance Monad GHCi where
  (GHCi m) >>= k  =  GHCi $ \s -> m s >>= \a -> unGHCi (k a) s
  return a  = GHCi $ \_ -> return a

instance Functor GHCi where
    fmap f m = m >>= return . f

getGHCiState :: GHCi GHCiState
getGHCiState   = GHCi $ \r -> liftIO $ readIORef r
setGHCiState :: GHCiState -> GHCi ()
setGHCiState s = GHCi $ \r -> liftIO $ writeIORef r s
modifyGHCiState :: (GHCiState -> GHCiState) -> GHCi ()
modifyGHCiState f = GHCi $ \r -> liftIO $ readIORef r >>= writeIORef r . f

liftGhc :: Ghc a -> GHCi a
liftGhc m = GHCi $ \_ -> m

instance MonadIO GHCi where
  liftIO = liftGhc . liftIO

instance HasDynFlags GHCi where
  getDynFlags = getSessionDynFlags

instance GhcMonad GHCi where
  setSession s' = liftGhc $ setSession s'
  getSession    = liftGhc $ getSession

instance HasDynFlags (InputT GHCi) where
  getDynFlags = lift getDynFlags

instance GhcMonad (InputT GHCi) where
  setSession = lift . setSession
  getSession = lift getSession

instance ExceptionMonad GHCi where
  gcatch m h = GHCi $ \r -> unGHCi m r `gcatch` (\e -> unGHCi (h e) r)
  gmask f =
      GHCi $ \s -> gmask $ \io_restore ->
                             let
                                g_restore (GHCi m) = GHCi $ \s' -> io_restore (m s')
                             in
                                unGHCi (f g_restore) s

instance Haskeline.MonadException Ghc where
  controlIO f = Ghc $ \s -> Haskeline.controlIO $ \(Haskeline.RunIO run) -> let
                    run' = Haskeline.RunIO (fmap (Ghc . const) . run . flip unGhc s)
                    in fmap (flip unGhc s) $ f run'

instance Haskeline.MonadException GHCi where
  controlIO f = GHCi $ \s -> Haskeline.controlIO $ \(Haskeline.RunIO run) -> let
                    run' = Haskeline.RunIO (fmap (GHCi . const) . run . flip unGHCi s)
                    in fmap (flip unGHCi s) $ f run'

instance ExceptionMonad (InputT GHCi) where
  gcatch = Haskeline.catch
  gmask f = Haskeline.liftIOOp gmask (f . Haskeline.liftIOOp_)

isOptionSet :: GHCiOption -> GHCi Bool
isOptionSet opt
 = do st <- getGHCiState
      return (opt `elem` options st)

setOption :: GHCiOption -> GHCi ()
setOption opt
 = do st <- getGHCiState
      setGHCiState (st{ options = opt : filter (/= opt) (options st) })

unsetOption :: GHCiOption -> GHCi ()
unsetOption opt
 = do st <- getGHCiState
      setGHCiState (st{ options = filter (/= opt) (options st) })

printForUser :: GhcMonad m => SDoc -> m ()
printForUser doc = do
  unqual <- GHC.getPrintUnqual
  dflags <- getDynFlags
  liftIO $ Outputable.printForUser dflags stdout unqual doc

printForUserPartWay :: SDoc -> GHCi ()
printForUserPartWay doc = do
  unqual <- GHC.getPrintUnqual
  dflags <- getDynFlags
  liftIO $ Outputable.printForUserPartWay dflags stdout (pprUserLength dflags) unqual doc

-- | Run a single Haskell expression
runStmt :: String -> GHC.SingleStep -> GHCi (Maybe GHC.RunResult)
runStmt expr step = do
  st <- getGHCiState
  reifyGHCi $ \x ->
    withProgName (progname st) $
    withArgs (args st) $
      reflectGHCi x $ do
        GHC.handleSourceError (\e -> do GHC.printException e;
                                        return Nothing) $ do
          r <- GHC.runStmtWithLocation (progname st) (line_number st) expr step
          return (Just r)

runDecls :: String -> GHCi [GHC.Name]
runDecls decls = do
  st <- getGHCiState
  reifyGHCi $ \x ->
    withProgName (progname st) $
    withArgs (args st) $
      reflectGHCi x $ do
        GHC.handleSourceError (\e -> do GHC.printException e; return []) $ do
          GHC.runDeclsWithLocation (progname st) (line_number st) decls

resume :: (SrcSpan -> Bool) -> GHC.SingleStep -> GHCi GHC.RunResult
resume canLogSpan step = do
  st <- getGHCiState
  reifyGHCi $ \x ->
    withProgName (progname st) $
    withArgs (args st) $
      reflectGHCi x $ do
        GHC.resume canLogSpan step

-- --------------------------------------------------------------------------
-- timing & statistics

timeIt :: InputT GHCi a -> InputT GHCi a
timeIt action
  = do b <- lift $ isOptionSet ShowTiming
       if not b
          then action
          else do allocs1 <- liftIO $ getAllocations
                  time1   <- liftIO $ getCPUTime
                  a <- action
                  allocs2 <- liftIO $ getAllocations
                  time2   <- liftIO $ getCPUTime
                  dflags  <- getDynFlags
                  liftIO $ printTimes dflags (fromIntegral (allocs2 - allocs1))
                                  (time2 - time1)
                  return a

foreign import ccall unsafe "getAllocations" getAllocations :: IO Int64
        -- defined in ghc/rts/Stats.c

printTimes :: DynFlags -> Integer -> Integer -> IO ()
printTimes dflags allocs psecs
   = do let secs = (fromIntegral psecs / (10^(12::Integer))) :: Float
            secs_str = showFFloat (Just 2) secs
        putStrLn (showSDoc dflags (
                 parens (text (secs_str "") <+> text "secs" <> comma <+>
                         text (show allocs) <+> text "bytes")))

-----------------------------------------------------------------------------
-- reverting CAFs

revertCAFs :: GHCi ()
revertCAFs = do
  liftIO rts_revertCAFs
  s <- getGHCiState
  when (not (ghc_e s)) $ liftIO turnOffBuffering
        -- Have to turn off buffering again, because we just
        -- reverted stdout, stderr & stdin to their defaults.

foreign import ccall "revertCAFs" rts_revertCAFs  :: IO ()
        -- Make it "safe", just in case

-----------------------------------------------------------------------------
-- To flush buffers for the *interpreted* computation we need
-- to refer to *its* stdout/stderr handles

GLOBAL_VAR(stdin_ptr,  error "no stdin_ptr",  Ptr ())
GLOBAL_VAR(stdout_ptr, error "no stdout_ptr", Ptr ())
GLOBAL_VAR(stderr_ptr, error "no stderr_ptr", Ptr ())

-- After various attempts, I believe this is the least bad way to do
-- what we want.  We know look up the address of the static stdin,
-- stdout, and stderr closures in the loaded base package, and each
-- time we need to refer to them we cast the pointer to a Handle.
-- This avoids any problems with the CAF having been reverted, because
-- we'll always get the current value.
--
-- The previous attempt that didn't work was to compile an expression
-- like "hSetBuffering stdout NoBuffering" into an expression of type
-- IO () and run this expression each time we needed it, but the
-- problem is that evaluating the expression might cache the contents
-- of the Handle rather than referring to it from its static address
-- each time.  There's no safe workaround for this.

initInterpBuffering :: Ghc ()
initInterpBuffering = do -- make sure these are linked
    dflags <- GHC.getSessionDynFlags
    liftIO $ do
      initDynLinker dflags

        -- ToDo: we should really look up these names properly, but
        -- it's a fiddle and not all the bits are exposed via the GHC
        -- interface.
      mb_stdin_ptr  <- ObjLink.lookupSymbol "base_GHCziIOziHandleziFD_stdin_closure"
      mb_stdout_ptr <- ObjLink.lookupSymbol "base_GHCziIOziHandleziFD_stdout_closure"
      mb_stderr_ptr <- ObjLink.lookupSymbol "base_GHCziIOziHandleziFD_stderr_closure"

      let f ref (Just ptr) = writeIORef ref ptr
          f _   Nothing    = panic "interactiveUI:setBuffering2"
      zipWithM_ f [stdin_ptr,stdout_ptr,stderr_ptr]
                  [mb_stdin_ptr,mb_stdout_ptr,mb_stderr_ptr]

flushInterpBuffers :: GHCi ()
flushInterpBuffers
 = liftIO $ do getHandle stdout_ptr >>= hFlush
               getHandle stderr_ptr >>= hFlush

turnOffBuffering :: IO ()
turnOffBuffering
 = do hdls <- mapM getHandle [stdin_ptr,stdout_ptr,stderr_ptr]
      mapM_ (\h -> hSetBuffering h NoBuffering) hdls

getHandle :: IORef (Ptr ()) -> IO Handle
getHandle ref = do
  (Ptr addr) <- readIORef ref
  case addrToAny# addr of (# hval #) -> return (unsafeCoerce# hval)

