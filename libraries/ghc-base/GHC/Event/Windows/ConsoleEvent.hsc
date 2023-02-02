{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE CPP, NoImplicitPrelude, MagicHash, UnboxedTuples #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Event.Windows.ConsoleEvent
-- Copyright   :  (c) The University of Glasgow, 1994-2002
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  cvs-ghc@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC extensions)
--
-- Windows I/O manager interfaces. Depending on which I/O Subsystem is used
-- requests will be routed to different places.
--
-----------------------------------------------------------------------------

module GHC.Event.Windows.ConsoleEvent (
  ConsoleEvent (..),
  start_console_handler,
  toWin32ConsoleEvent,
  win32ConsoleHandler
) where

import GHC.Base
import GHC.Conc.Sync
import GHC.Enum (Enum)
import GHC.IO (unsafePerformIO)
import GHC.MVar
import GHC.Num (Num(..))
import GHC.Read (Read)
import GHC.Word (Word32)
import GHC.Show (Show)

#include <windows.h>

data ConsoleEvent
  = ControlC
  | Break
  | Close
  -- these are sent to Services only.
  | Logoff
  | Shutdown
    deriving ( Eq   -- ^ @since 4.3.0.0
             , Ord  -- ^ @since 4.3.0.0
             , Enum -- ^ @since 4.3.0.0
             , Show -- ^ @since 4.3.0.0
             , Read -- ^ @since 4.3.0.0
             )

start_console_handler :: Word32 -> IO ()
start_console_handler r =
  case toWin32ConsoleEvent r of
    Just x  -> withMVar win32ConsoleHandler $ \handler -> do
                 _ <- forkIO (handler x)
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
