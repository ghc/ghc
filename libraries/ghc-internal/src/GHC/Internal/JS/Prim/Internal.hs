{- | Code used by the RTS

 -}

module GHC.Internal.JS.Prim.Internal ( blockedIndefinitelyOnMVar
                            , blockedIndefinitelyOnSTM
                            , wouldBlock
                            , ignoreException
                            , setCurrentThreadResultException
                            , setCurrentThreadResultValue
                            ) where

import           GHC.Internal.Control.Exception

import           GHC.Internal.JS.Prim
import           GHC.Internal.Base
import           GHC.Internal.Show

wouldBlock :: SomeException
wouldBlock = toException WouldBlockException

blockedIndefinitelyOnMVar :: SomeException
blockedIndefinitelyOnMVar = toException BlockedIndefinitelyOnMVar

blockedIndefinitelyOnSTM :: SomeException
blockedIndefinitelyOnSTM = toException BlockedIndefinitelyOnSTM

ignoreException :: SomeException -> IO ()
ignoreException _ = return ()

setCurrentThreadResultException :: SomeException -> IO ()
setCurrentThreadResultException e
  | Just WouldBlockException <- fromException e =
      js_setCurrentThreadResultWouldBlock
  | Just (JSException v _) <- fromException e =
      js_setCurrentThreadResultJSException v
  | otherwise =
      js_setCurrentThreadResultHaskellException (toJSString (show e))

setCurrentThreadResultValue :: IO JSVal -> IO ()
setCurrentThreadResultValue x = js_setCurrentThreadResultValue =<< x

foreign import javascript unsafe
  "(() => { return h$setCurrentThreadResultWouldBlock; })"
  js_setCurrentThreadResultWouldBlock :: IO ()

foreign import javascript unsafe
  "h$setCurrentThreadResultJSException"
  js_setCurrentThreadResultJSException :: JSVal -> IO ()

foreign import javascript unsafe
  "h$setCurrentThreadResultHaskellException"
  js_setCurrentThreadResultHaskellException :: JSVal -> IO ()

foreign import javascript unsafe
  "h$setCurrentThreadResultValue"
  js_setCurrentThreadResultValue :: JSVal -> IO ()

