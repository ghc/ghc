{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE CPP, NoImplicitPrelude, MagicHash, UnboxedTuples #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Internal.Event.Windows.ConsoleEvent
-- Copyright   :  (c) The University of Glasgow, 1994-2002
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  ghc-devs@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC extensions)
--
-- Windows I/O manager interfaces. Depending on which I/O Subsystem is used
-- requests will be routed to different places.
--
-----------------------------------------------------------------------------

module GHC.Internal.Event.Windows.ConsoleEvent (
  ConsoleEvent (..),
  start_console_handler,
  toWin32ConsoleEvent,
  win32ConsoleHandler
) where

import GHC.Internal.Base
import GHC.Internal.Conc.Sync
import GHC.Internal.Enum (Enum)
import GHC.Internal.IO (unsafePerformIO)
import GHC.Internal.MVar
import GHC.Internal.Num (Num(..))
import GHC.Internal.Read (Read)
import GHC.Internal.Word (Word32)
import GHC.Internal.Show (Show)

#include <windows.h>

data ConsoleEvent
  = ControlC
  | Break
  | Close
  -- these are sent to Services only.
  | Logoff
  | Shutdown
    deriving ( Eq   -- ^ @since base-4.3.0.0
             , Ord  -- ^ @since base-4.3.0.0
             , Enum -- ^ @since base-4.3.0.0
             , Show -- ^ @since base-4.3.0.0
             , Read -- ^ @since base-4.3.0.0
             )

start_console_handler :: Word32 -> IO ()
start_console_handler r =
  case toWin32ConsoleEvent r of
    Just x  -> withMVar win32ConsoleHandler $ \handler -> do
                 _ <- forkIO $ do
                     tid <- myThreadId
                     labelThread tid "console event handler"
                     handler x
                 return ()
    Nothing -> return ()

toWin32ConsoleEvent :: (Eq a, Num a) => a -> Maybe ConsoleEvent
toWin32ConsoleEvent ev =
  case ev of
    #{const CTRL_C_EVENT        } -> Just ControlC
    #{const CTRL_BREAK_EVENT    } -> Just Break
    #{const CTRL_CLOSE_EVENT    } -> Just Close
    #{const CTRL_LOGOFF_EVENT   } -> Just Logoff
    #{const CTRL_SHUTDOWN_EVENT } -> Just Shutdown
    _                             -> Nothing

win32ConsoleHandler :: MVar (ConsoleEvent -> IO ())
win32ConsoleHandler =
  unsafePerformIO (newMVar (errorWithoutStackTrace "win32ConsoleHandler"))
