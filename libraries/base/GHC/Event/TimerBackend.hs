{-# LANGUAGE CPP #-}
{-# LANGUAGE NoImplicitPrelude #-}
module GHC.Event.TimerBackend
    ( BackendState(..)
    , new
    , poll
    , modifyFd
    , modifyFdOnce
    , delete
    ) where

import GHC.Base
import System.Posix.Types (Fd)
import qualified GHC.Event.Internal as E

#include "EventConfig.h"

#if defined(HAVE_POLL)
import GHC.Event.Poll
#else
data BackendState = BackendState
new :: IO BackendState
new = pure BackendState
delete :: BackendState -> IO ()
delete _ = fail "timer event manager: delete not implemented"
modifyFd :: BackendState -> Fd -> E.Event -> E.Event -> IO Bool
modifyFd _ _ _ _ = fail "timer event manager: modifyFd not implemented"
modifyFdOnce :: BackendState -> Fd -> E.Event -> IO Bool
modifyFdOnce _ _ _ = fail "timer event manager: modifyFdOnce not implemented"
poll :: BackendState -> Maybe E.Timeout -> (Fd -> E.Event -> IO ()) -> IO Int
poll _ _ _ = fail "timer event manager: poll not implemented"
#endif
