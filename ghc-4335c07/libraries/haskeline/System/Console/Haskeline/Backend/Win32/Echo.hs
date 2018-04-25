{-# LANGUAGE CPP #-}

module System.Console.Haskeline.Backend.Win32.Echo (hWithoutInputEcho) where

import Control.Exception (throw)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)

import System.Console.Haskeline.MonadException (MonadException, bracket)
import System.Exit (ExitCode(..))
import System.IO (Handle, hGetContents, hGetEcho, hSetEcho)
import System.Process (StdStream(..), createProcess, shell,
                       std_in, std_out, waitForProcess)

#if MIN_VERSION_Win32(2,5,0)
import Control.Concurrent.MVar (readMVar)

import Data.Typeable (cast)

import Foreign.C.Types
import Foreign.StablePtr (StablePtr, freeStablePtr, newStablePtr)

import GHC.IO.FD (FD(..))
import GHC.IO.Handle.Types (Handle(..), Handle__(..))

import System.Win32.Types (HANDLE)
import System.Win32.MinTTY (isMinTTYHandle)
#endif

-- | Return the handle's current input 'EchoState'.
hGetInputEchoState :: Handle -> IO EchoState
hGetInputEchoState input = do
  min_tty <- minTTY input
  if min_tty
     then fmap MinTTY (hGetInputEchoSTTY input)
     else fmap DefaultTTY $ hGetEcho input

-- | Return all of @stty@'s current settings in a non-human-readable format.
--
-- This function is not very useful on its own. Its greater purpose is to
-- provide a compact 'STTYSettings' that can be fed back into
-- 'hSetInputEchoState'.
hGetInputEchoSTTY :: Handle -> IO STTYSettings
hGetInputEchoSTTY input = hSttyRaw input "-g"

-- | Set the handle's input 'EchoState'.
hSetInputEchoState :: Handle -> EchoState -> IO ()
hSetInputEchoState input (MinTTY settings) = hSetInputEchoSTTY input settings
hSetInputEchoState input (DefaultTTY echo) = hSetEcho input echo

-- | Create an @stty@ process and wait for it to complete. This is useful for
-- changing @stty@'s settings, after which @stty@ does not output anything.
--
-- @
-- hSetInputEchoSTTY input = 'void' . 'hSttyRaw' input
-- @
hSetInputEchoSTTY :: Handle -> STTYSettings -> IO ()
hSetInputEchoSTTY input = void . hSttyRaw input

-- | Save the handle's current input 'EchoState', perform a computation,
-- restore the saved 'EchoState', and then return the result of the
-- computation.
--
-- @
-- bracketInputEcho input action =
--  'bracket' ('liftIO' $ 'hGetInputEchoState' input)
--            ('liftIO' . 'hSetInputEchoState' input)
--            (const action)
-- @
hBracketInputEcho :: MonadException m => Handle -> m a -> m a
hBracketInputEcho input action =
  bracket (liftIO $ hGetInputEchoState input)
          (liftIO . hSetInputEchoState input)
          (const action)

-- | Perform a computation with the handle's input echoing disabled. Before
-- running the computation, the handle's input 'EchoState' is saved, and the
-- saved 'EchoState' is restored after the computation finishes.
hWithoutInputEcho :: MonadException m => Handle -> m a -> m a
hWithoutInputEcho input action = do
  echo_off <- liftIO $ hEchoOff input
  hBracketInputEcho input
                    (liftIO (hSetInputEchoState input echo_off) >> action)

-- | Create an @stty@ process, wait for it to complete, and return its output.
hSttyRaw :: Handle -> String -> IO STTYSettings
hSttyRaw input arg = do
  let stty = (shell $ "stty " ++ arg) {
        std_in  = UseHandle input
      , std_out = CreatePipe
      }
  (_, mbStdout, _, rStty) <- createProcess stty
  exStty <- waitForProcess rStty
  case exStty of
    e@ExitFailure{} -> throw e
    ExitSuccess     -> maybe (return "") hGetContents mbStdout

-- | A representation of the handle input's current echoing state.
-- See, for instance, 'hEchoOff'.
data EchoState
  = MinTTY STTYSettings
    -- ^ The argument to (or value returned from) an invocation of the @stty@
    -- command-line utility. Most POSIX-like shells have @stty@, including
    -- MinTTY on Windows. Since neither 'hGetEcho' nor 'hSetEcho' work on
    -- MinTTY, when 'getInputEchoState' runs on MinTTY, it returns a value
    -- built with this constructor.
    --
    -- However, native Windows consoles like @cmd.exe@ or PowerShell do not
    -- have @stty@, so if you construct an 'EchoState' with this constructor
    -- manually, take care not to use it with a native Windows console.
  | DefaultTTY Bool
    -- ^ A simple on ('True') or off ('False') toggle. This is returned by
    -- 'hGetEcho' and given as an argument to 'hSetEcho', which work on most
    -- consoles, with the notable exception of MinTTY on Windows. If you
    -- construct an 'EchoState' with this constructor manually, take care not
    -- to use it with MinTTY.
  deriving (Eq, Ord, Show)

-- | Indicates that the handle's input echoing is (or should be) off.
hEchoOff :: Handle -> IO EchoState
hEchoOff input = do
  min_tty <- minTTY input
  return $ if min_tty
              then MinTTY "-echo"
              else DefaultTTY False

-- | Settings used to configure the @stty@ command-line utility.
type STTYSettings = String

-- | Is the current process attached to a MinTTY console (e.g., Cygwin or MSYS)?
minTTY :: Handle -> IO Bool
#if MIN_VERSION_Win32(2,5,0)
minTTY input = withHandleToHANDLE input isMinTTYHandle
#else
-- On older versions of Win32, we simply punt.
minTTY _     = return False
#endif

#if MIN_VERSION_Win32(2,5,0)
foreign import ccall unsafe "_get_osfhandle"
  c_get_osfhandle :: CInt -> IO HANDLE

-- | Extract a Windows 'HANDLE' from a Haskell 'Handle' and perform
-- an action on it.

-- Originally authored by Max Bolingbroke in the ansi-terminal library
withHandleToHANDLE :: Handle -> (HANDLE -> IO a) -> IO a
withHandleToHANDLE haskell_handle action =
    -- Create a stable pointer to the Handle. This prevents the garbage collector
    -- getting to it while we are doing horrible manipulations with it, and hence
    -- stops it being finalized (and closed).
    withStablePtr haskell_handle $ const $ do
        -- Grab the write handle variable from the Handle
        let write_handle_mvar = case haskell_handle of
                FileHandle _ handle_mvar     -> handle_mvar
                DuplexHandle _ _ handle_mvar -> handle_mvar
                  -- This is "write" MVar, we could also take the "read" one

        -- Get the FD from the algebraic data type
        Just fd <- fmap (\(Handle__ { haDevice = dev }) -> fmap fdFD (cast dev))
                 $ readMVar write_handle_mvar

        -- Finally, turn that (C-land) FD into a HANDLE using msvcrt
        windows_handle <- c_get_osfhandle fd

        -- Do what the user originally wanted
        action windows_handle

withStablePtr :: a -> (StablePtr a -> IO b) -> IO b
withStablePtr value = bracket (newStablePtr value) freeStablePtr
#endif
