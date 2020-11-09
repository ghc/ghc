module T18231 where

import Control.Monad (forever)
import Control.Monad.Trans.State.Strict

m :: State Int ()
m = forever $ modify' (+1)
