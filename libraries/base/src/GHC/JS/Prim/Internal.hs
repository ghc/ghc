module GHC.JS.Prim.Internal
    ( blockedIndefinitelyOnMVar
    , blockedIndefinitelyOnSTM
    , wouldBlock
    , ignoreException
    , setCurrentThreadResultException
    , setCurrentThreadResultValue
    ) where

import GHC.Internal.JS.Prim.Internal
