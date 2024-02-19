module GHC.JS.Foreign.Callback
    ( Callback
    , OnBlocked(..)
    , releaseCallback
      -- * asynchronous callbacks
    , asyncCallback
    , asyncCallback1
    , asyncCallback2
    , asyncCallback3
      -- * synchronous callbacks
    , syncCallback
    , syncCallback1
    , syncCallback2
    , syncCallback3
      -- * synchronous callbacks that return a value
    , syncCallback'
    , syncCallback1'
    , syncCallback2'
    , syncCallback3'
    ) where

import GHC.Internal.JS.Foreign.Callback
