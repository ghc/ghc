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
import {-#SOURCE#-} Debugger
import Breakpoints
import Outputable
import Panic hiding (showException)
import Util
import DynFlags

import Numeric
import Control.Exception as Exception
import Data.Char
import Data.Dynamic
import Data.Int         ( Int64 )
import Data.IORef
import Data.List
import Data.Typeable
import System.CPUTime
import System.IO
import Control.Monad as Monad
import GHC.Exts

-----------------------------------------------------------------------------
-- GHCi monad

data GHCiState = GHCiState
     { 
	progname       :: String,
	args	       :: [String],
        prompt         :: String,
	editor         :: String,
	session        :: GHC.Session,
	options        :: [GHCiOption],
        prelude        :: GHC.Module,
        bkptTable      :: IORef (BkptTable GHC.Module),
	topLevel       :: Bool
     }

data GHCiOption 
	= ShowTiming		-- show time/allocs after evaluation
	| ShowType		-- show the type of expressions
	| RevertCAFs		-- revert CAFs after every evaluation
	deriving Eq

newtype GHCi a = GHCi { unGHCi :: IORef GHCiState -> IO a }

startGHCi :: GHCi a -> GHCiState -> IO a
startGHCi g state = do ref <- newIORef state; unGHCi g ref

instance Monad GHCi where
  (GHCi m) >>= k  =  GHCi $ \s -> m s >>= \a -> unGHCi (k a) s
  return a  = GHCi $ \s -> return a

ghciHandleDyn :: Typeable t => (t -> GHCi a) -> GHCi a -> GHCi a
ghciHandleDyn h (GHCi m) = GHCi $ \s -> 
   Exception.catchDyn (m s) (\e -> unGHCi (h e) s)

getGHCiState   = GHCi $ \r -> readIORef r
setGHCiState s = GHCi $ \r -> writeIORef r s

-- for convenience...
getSession = getGHCiState >>= return . session
getPrelude = getGHCiState >>= return . prelude

GLOBAL_VAR(saved_sess, no_saved_sess, GHC.Session)
no_saved_sess = error "no saved_ses"
saveSession = getSession >>= io . writeIORef saved_sess
splatSavedSession = io (writeIORef saved_sess no_saved_sess)
restoreSession = readIORef saved_sess

getDynFlags = do
  s <- getSession
  io (GHC.getSessionDynFlags s)
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
io m = GHCi { unGHCi = \s -> m >>= return }

isTopLevel :: GHCi Bool
isTopLevel = getGHCiState >>= return . topLevel

getBkptTable :: GHCi (BkptTable GHC.Module)
getBkptTable = do table_ref <- getGHCiState >>= return . bkptTable
                  io$ readIORef table_ref

setBkptTable :: BkptTable GHC.Module -> GHCi ()
setBkptTable new_table = do 
    table_ref <- getGHCiState >>= return . bkptTable
    io$ writeIORef table_ref new_table
                  
modifyBkptTable :: (BkptTable GHC.Module -> BkptTable GHC.Module) -> GHCi ()
modifyBkptTable f = do 
    bt <- getBkptTable
    new_bt <- io . evaluate$ f bt 
    setBkptTable new_bt

showForUser :: SDoc -> GHCi String
showForUser doc = do
  session <- getSession
  unqual <- io (GHC.getPrintUnqual session)
  return $! showSDocForUser unqual doc

-- --------------------------------------------------------------------------
-- Inferior Sessions Exceptions (used by the debugger)

data InfSessionException = 
             StopChildSession -- A child session requests to be stopped
           | StopParentSession -- A child session requests to be stopped 
                               -- AND that the parent session quits after that
           | ChildSessionStopped String  -- A child session has stopped
  deriving Typeable


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
	
revertCAFs :: IO ()
revertCAFs = do
  rts_revertCAFs
  turnOffBuffering
	-- Have to turn off buffering again, because we just 
	-- reverted stdout, stderr & stdin to their defaults.

foreign import ccall "revertCAFs" rts_revertCAFs  :: IO ()  
	-- Make it "safe", just in case

-----------------------------------------------------------------------------
-- To flush buffers for the *interpreted* computation we need
-- to refer to *its* stdout/stderr handles

GLOBAL_VAR(flush_interp,       error "no flush_interp", IO ())
GLOBAL_VAR(turn_off_buffering, error "no flush_stdout", IO ())

command_sequence :: [String] -> String
command_sequence = unwords . intersperse "Prelude.>>"

no_buffer :: String -> String
no_buffer h = unwords ["System.IO.hSetBuffering",
                       "System.IO." ++ h,
                       "System.IO.NoBuffering"]

no_buf_cmd :: String
no_buf_cmd = command_sequence $ map no_buffer ["stdout", "stderr", "stdin"]

flush_buffer :: String -> String
flush_buffer h = unwords ["System.IO.hFlush", "System.IO." ++ h]

flush_cmd :: String
flush_cmd = command_sequence [flush_buffer "stdout", flush_buffer "stderr"]

initInterpBuffering :: GHC.Session -> IO ()
initInterpBuffering session
 = do -- we don't want to be fooled by any modules lying around in the current
      -- directory when we compile these code fragments, so set the import
      -- path to be empty while we compile them.
      dflags <- GHC.getSessionDynFlags session
      GHC.setSessionDynFlags session dflags{importPaths=[]}

      maybe_hval <- GHC.compileExpr session no_buf_cmd

      case maybe_hval of
	Just hval -> writeIORef turn_off_buffering (unsafeCoerce# hval :: IO ())
	other	  -> panic "interactiveUI:setBuffering"
	
      maybe_hval <- GHC.compileExpr session flush_cmd
      case maybe_hval of
	Just hval -> writeIORef flush_interp (unsafeCoerce# hval :: IO ())
	_         -> panic "interactiveUI:flush"

      GHC.setSessionDynFlags session dflags
      GHC.workingDirectoryChanged session
      return ()


flushInterpBuffers :: GHCi ()
flushInterpBuffers
 = io $ do Monad.join (readIORef flush_interp)
           return ()

turnOffBuffering :: IO ()
turnOffBuffering
 = do Monad.join (readIORef turn_off_buffering)
      return ()
