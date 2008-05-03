-----------------------------------------------------------------------------
--
-- Monadery code used in InteractiveUI
--
-- (c) The GHC Team 2005-2006
--
-----------------------------------------------------------------------------

module GhciMonad where

#include "HsVersions.h"

import qualified GHC
import Outputable       hiding (printForUser, printForUserPartWay)
import qualified Outputable
import Panic            hiding (showException)
import Util
import DynFlags
import HscTypes
import SrcLoc
import Module
import ObjLink
import StaticFlags

import Data.Maybe
import Numeric
import Control.Exception as Exception
import Data.Array
import Data.Char
import Data.Int         ( Int64 )
import Data.IORef
import Data.List
import Data.Typeable
import System.CPUTime
import System.Directory
import System.Environment
import System.IO
import Control.Monad as Monad
import GHC.Exts

-----------------------------------------------------------------------------
-- GHCi monad

type Command = (String, String -> GHCi Bool, Maybe String, String -> IO [String])

data GHCiState = GHCiState
     { 
	progname       :: String,
	args	       :: [String],
        prompt         :: String,
	editor         :: String,
        stop           :: String,
	session        :: GHC.Session,
	options        :: [GHCiOption],
        prelude        :: GHC.Module,
        break_ctr      :: !Int,
        breaks         :: ![(Int, BreakLocation)],
        tickarrays     :: ModuleEnv TickArray,
                -- tickarrays caches the TickArray for loaded modules,
                -- so that we don't rebuild it each time the user sets
                -- a breakpoint.
        -- ":" at the GHCi prompt repeats the last command, so we
        -- remember is here:
        last_command   :: Maybe Command,
        cmdqueue       :: [String],
        remembered_ctx :: [(CtxtCmd, [String], [String])],
             -- we remember the :module commands between :loads, so that
             -- on a :reload we can replay them.  See bugs #2049,
             -- #1873, #1360. Previously we tried to remember modules that
             -- were supposed to be in the context but currently had errors,
             -- but this was complicated.  Just replaying the :module commands
             -- seems to be the right thing.
        virtual_path   :: FilePath,
        ghc_e :: Bool -- True if this is 'ghc -e' (or runghc)
     }

data CtxtCmd
  = SetContext
  | AddModules
  | RemModules

type TickArray = Array Int [(BreakIndex,SrcSpan)]

data GHCiOption 
	= ShowTiming		-- show time/allocs after evaluation
	| ShowType		-- show the type of expressions
	| RevertCAFs		-- revert CAFs after every evaluation
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

newtype GHCi a = GHCi { unGHCi :: IORef GHCiState -> IO a }

startGHCi :: GHCi a -> GHCiState -> IO a
startGHCi g state = do ref <- newIORef state; unGHCi g ref

instance Monad GHCi where
  (GHCi m) >>= k  =  GHCi $ \s -> m s >>= \a -> unGHCi (k a) s
  return a  = GHCi $ \_ -> return a

instance Functor GHCi where
    fmap f m = m >>= return . f

ghciHandleDyn :: Typeable t => (t -> GHCi a) -> GHCi a -> GHCi a
ghciHandleDyn h (GHCi m) = GHCi $ \s -> 
   Exception.catchDyn (m s) (\e -> unGHCi (h e) s)

getGHCiState :: GHCi GHCiState
getGHCiState   = GHCi $ \r -> readIORef r
setGHCiState :: GHCiState -> GHCi ()
setGHCiState s = GHCi $ \r -> writeIORef r s

-- for convenience...
getSession :: GHCi Session
getSession = getGHCiState >>= return . session
getPrelude :: GHCi Module
getPrelude = getGHCiState >>= return . prelude

GLOBAL_VAR(saved_sess, no_saved_sess, GHC.Session)

no_saved_sess :: Session
no_saved_sess = error "no saved_ses"

saveSession :: GHCi ()
saveSession = getSession >>= io . writeIORef saved_sess

splatSavedSession :: GHCi ()
splatSavedSession = io (writeIORef saved_sess no_saved_sess)

restoreSession :: IO Session
restoreSession = readIORef saved_sess

getDynFlags :: GHCi DynFlags
getDynFlags = do
  s <- getSession
  io (GHC.getSessionDynFlags s)
setDynFlags :: DynFlags -> GHCi [PackageId]
setDynFlags dflags = do 
  s <- getSession 
  io (GHC.setSessionDynFlags s dflags)

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

io :: IO a -> GHCi a
io m = GHCi (\_ -> m)

printForUser :: SDoc -> GHCi ()
printForUser doc = do
  session <- getSession
  unqual <- io (GHC.getPrintUnqual session)
  io $ Outputable.printForUser stdout unqual doc

printForUserPartWay :: SDoc -> GHCi ()
printForUserPartWay doc = do
  session <- getSession
  unqual <- io (GHC.getPrintUnqual session)
  io $ Outputable.printForUserPartWay stdout opt_PprUserLength unqual doc

withVirtualPath :: GHCi a -> GHCi a
withVirtualPath m = do
  ghci_wd <- io getCurrentDirectory                -- Store the cwd of GHCi
  st  <- getGHCiState
  io$ setCurrentDirectory (virtual_path st)
  result <- m                                  -- Evaluate in the virtual wd..
  vwd <- io getCurrentDirectory
  setGHCiState (st{ virtual_path = vwd})       -- Update the virtual path
  io$ setCurrentDirectory ghci_wd                  -- ..and restore GHCi wd
  return result

runStmt :: String -> GHC.SingleStep -> GHCi GHC.RunResult
runStmt expr step = withVirtualPath$ do
  session <- getSession
  st      <- getGHCiState
  io$ withProgName (progname st) $ withArgs (args st) $
                    GHC.runStmt session expr step

resume :: GHC.SingleStep -> GHCi GHC.RunResult
resume step = withVirtualPath$ do
  session <- getSession
  io$ GHC.resume session step


-- --------------------------------------------------------------------------
-- timing & statistics

timeIt :: GHCi a -> GHCi a
timeIt action
  = do b <- isOptionSet ShowTiming
       if not b 
	  then action 
	  else do allocs1 <- io $ getAllocations
		  time1   <- io $ getCPUTime
		  a <- action
		  allocs2 <- io $ getAllocations
		  time2   <- io $ getCPUTime
		  io $ printTimes (fromIntegral (allocs2 - allocs1)) 
				  (time2 - time1)
		  return a

foreign import ccall unsafe "getAllocations" getAllocations :: IO Int64
	-- defined in ghc/rts/Stats.c

printTimes :: Integer -> Integer -> IO ()
printTimes allocs psecs
   = do let secs = (fromIntegral psecs / (10^12)) :: Float
	    secs_str = showFFloat (Just 2) secs
	putStrLn (showSDoc (
		 parens (text (secs_str "") <+> text "secs" <> comma <+> 
			 text (show allocs) <+> text "bytes")))

-----------------------------------------------------------------------------
-- reverting CAFs
	
revertCAFs :: GHCi ()
revertCAFs = do
  io $ rts_revertCAFs
  s <- getGHCiState
  when (not (ghc_e s)) $ io turnOffBuffering
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

initInterpBuffering :: GHC.Session -> IO ()
initInterpBuffering session
 = do -- make sure these are linked
      mb_hval1 <- GHC.compileExpr session "System.IO.stdout"
      mb_hval2 <- GHC.compileExpr session "System.IO.stderr"
      mb_hval3 <- GHC.compileExpr session "System.IO.stdin"
      when (any isNothing [mb_hval1,mb_hval2,mb_hval3]) $
        panic "interactiveUI:setBuffering"

        -- ToDo: we should really look up these names properly, but
        -- it's a fiddle and not all the bits are exposed via the GHC
        -- interface.
      mb_stdin_ptr  <- ObjLink.lookupSymbol "base_GHCziHandle_stdin_closure"
      mb_stdout_ptr <- ObjLink.lookupSymbol "base_GHCziHandle_stdout_closure"
      mb_stderr_ptr <- ObjLink.lookupSymbol "base_GHCziHandle_stderr_closure"

      let f ref (Just ptr) = writeIORef ref ptr
          f _   Nothing    = panic "interactiveUI:setBuffering2"
      zipWithM f [stdin_ptr,stdout_ptr,stderr_ptr]
                 [mb_stdin_ptr,mb_stdout_ptr,mb_stderr_ptr]
      return ()

flushInterpBuffers :: GHCi ()
flushInterpBuffers
 = io $ do getHandle stdout_ptr >>= hFlush
           getHandle stderr_ptr >>= hFlush

turnOffBuffering :: IO ()
turnOffBuffering
 = do hdls <- mapM getHandle [stdin_ptr,stdout_ptr,stderr_ptr]
      mapM_ (\h -> hSetBuffering h NoBuffering) hdls

getHandle :: IORef (Ptr ()) -> IO Handle
getHandle ref = do
  (Ptr addr) <- readIORef ref
  case addrToHValue# addr of (# hval #) -> return (unsafeCoerce# hval)
