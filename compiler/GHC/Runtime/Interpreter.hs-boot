module GHC.Runtime.Interpreter where

import {-# SOURCE #-} GHC.Runtime.Interpreter.Types
import Data.Int (Int)
import GHC.Base (IO)
import GHCi.BreakArray (BreakArray)
import GHCi.RemoteTypes (ForeignRef)

newBreakArray :: Interp -> Int -> IO (ForeignRef BreakArray)

